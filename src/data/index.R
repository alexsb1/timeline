# Everything the data layer needs, in dependency order. The app and the command-line scripts in
# processing/ both load it with:  source("src/data/index.R", local = TRUE, encoding = "UTF-8")
# (run from the project root).

source("src/constants.R", local = TRUE, encoding = "UTF-8")
source("src/time_format.R", local = TRUE, encoding = "UTF-8")
source("src/data/helpers.R", local = TRUE, encoding = "UTF-8")
source("src/data/helpers_history.R", local = TRUE, encoding = "UTF-8")
source("src/data/helpers_wikidata.R", local = TRUE, encoding = "UTF-8")
source("src/data/registry.R", local = TRUE, encoding = "UTF-8")
source("src/data/load.R", local = TRUE, encoding = "UTF-8")
