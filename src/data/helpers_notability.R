# How notable is a Wikipedia article? The number of language editions it exists in, which Wikidata
# records as "sitelinks". A well-known council or schism is written up in dozens of languages, a
# footnote of church history in two. Used to choose the major entries from long timeline articles
# (ds_religious_events.R). Needs helpers.R.

# Fetch many URLs at once (a few in flight at a time). Returns the response bodies as a character
# vector in the order of `urls`, NA where a request failed or did not return HTTP 200.
http_get_many <- function(urls, concurrency = 4) {
  pool <- curl::new_pool(total_con = concurrency, host_con = concurrency)
  bodies <- rep(NA_character_, length(urls))

  for (i in seq_along(urls)) {
    handle <- curl::new_handle(useragent = USER_AGENT, timeout = 60, connecttimeout = 15, followlocation = TRUE)
    local({
      index <- i
      curl::curl_fetch_multi(
        urls[[index]], handle = handle, pool = pool,
        done = \(response) {
          if (response$status_code == 200) {
            text <- rawToChar(response$content)
            Encoding(text) <- "UTF-8"
            bodies[index] <<- text
          }
        },
        fail = \(message) NULL
      )
    })
  }
  curl::multi_run(pool = pool)

  # One more try, on its own, for any that failed.
  for (i in which(is.na(bodies))) {
    bodies[i] <- tryCatch(http_get_text(urls[[i]], tries = 2), error = \(e) NA_character_)
  }
  bodies
}

# The Wikidata item (Q-number) behind each English Wikipedia article title, following redirects
# ("Council of Nicaea" -> "First Council of Nicaea"). Returns tibble(title, qid), qid NA where the
# article has no item or does not exist.
wikipedia_title_to_qid <- function(titles) {
  titles <- unique(titles[!is.na(titles)])
  batches <- split(titles, ceiling(seq_along(titles) / 50))
  urls <- map_chr(batches, \(batch) paste0(
    "https://en.wikipedia.org/w/api.php?action=query&prop=pageprops&ppprop=wikibase_item&redirects=1",
    "&format=json&formatversion=2&titles=", curl::curl_escape(paste(batch, collapse = "|"))
  ))

  map2(batches, http_get_many(urls), \(batch, body) {
    if (is.na(body)) return(tibble(title = batch, qid = NA_character_))
    query <- jsonlite::fromJSON(body)$query
    # The API may normalise a title (underscores, capitals) and then follow a redirect; undo both.
    # (Both are missing from the response when nothing was normalised or redirected.)
    lookup <- \(pairs) if (is.null(pairs)) character() else set_names(pairs$to, pairs$from)
    normalised <- lookup(query$normalized)
    redirected <- lookup(query$redirects)
    final <- map_chr(batch, \(t) {
      t <- if (t %in% names(normalised)) normalised[[t]] else t
      if (t %in% names(redirected)) redirected[[t]] else t
    })
    pages <- query$pages
    qid <- if ("pageprops" %in% names(pages)) pages$pageprops$wikibase_item else rep(NA_character_, nrow(pages))
    tibble(title = batch, qid = qid[match(final, pages$title)])
  }) |>
    list_rbind()
}

# What Wikidata knows about the English Wikipedia article behind each title:
# tibble(title, qid, sitelinks, classes). `sitelinks` is the number of language editions and
# `classes` what it is an instance of, as English labels joined by "; " ("ecumenical council",
# "human", "country"). Both are NA where the article could not be matched to a Wikidata item.
wikipedia_article_facts <- function(titles) {
  ids <- wikipedia_title_to_qid(titles)
  qids <- unique(na.omit(ids$qid))
  if (length(qids) == 0) return(mutate(ids, sitelinks = NA_integer_, classes = NA_character_))

  facts <- split(qids, ceiling(seq_along(qids) / 250)) |>
    map(\(batch) {
      wikidata_query(paste0(
        "SELECT ?item (MAX(?links) AS ?sitelinks) (GROUP_CONCAT(DISTINCT ?className; separator = \"; \") AS ?classes) WHERE { ",
        "VALUES ?item { ", paste0("wd:", batch, collapse = " "), " } ",
        "?item wikibase:sitelinks ?links . ",
        "OPTIONAL { ?item wdt:P31 ?class . ?class rdfs:label ?className . FILTER(LANG(?className) = \"en\") } ",
        "} GROUP BY ?item"
      ))
    }) |>
    list_rbind() |>
    transmute(qid = str_remove(item, "^.*/"), sitelinks = as.integer(sitelinks), classes = tolower(classes))

  left_join(ids, facts, by = "qid")
}
