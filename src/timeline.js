// Browser-side behaviour for the timeline plot. Passed to htmlwidgets::onRender(), so it
// runs once each time the figure is (re)drawn. Plain ES5 on purpose: no build step.
//
// 1. Whenever the visible time window changes, refit every panel's y axis to the data inside it.
//    Plotly only autoranges y over *all* the data, which is useless when the x axis can span
//    4.6 billion years or a few decades. The y axes are fixedrange, so this is the only thing
//    that moves them.
// 2. Show "No data in this time range" on panels whose data lies outside the window.
// 3. Position the labels of span panels (geological periods, reigns, ...) over the visible part
//    of each bar, shortening or hiding them to fit.
// 4. Relabel the x axis in "years ago" (12k, 66m, 4.5b) at round values, as the window changes.
//    The axis itself counts years elapsed since the Earth formed, so that concurrent events
//    share an x position.
// 5. Tell Shiny the visible window (input$x_range) so the sidebar can describe it and a redraw
//    can keep the same view.
//
// layout.meta.panels (written by build_timeline() in src/plot_timeline.R) has one entry per
// panel: { yId, yKey, fit, reversed, dataMin, dataMax, note }.
function (el, x) {
  var gd = el;
  var panels = (x.layout && x.layout.meta && x.layout.meta.panels) || [];
  if (!panels.length) return;

  var EARTH_AGE = x.layout.meta.earthAge;
  var lastKey = null;
  var reportTimer = null;
  var started = false;

  // ---- axis labels ----
  // The x axis counts years elapsed since the Earth formed, but is labelled in years ago. These
  // two functions mirror year_ticks() and format_ya() in src/time_format.R; keep them in step.

  // Abbreviate a number of years to three significant digits: 954, 12.3k, 66m, 4.54b.
  function formatYa(yearsAgo) {
    var divisor = 1;
    var suffix = '';
    if (yearsAgo >= 1e9) { divisor = 1e9; suffix = 'b'; }
    else if (yearsAgo >= 1e6) { divisor = 1e6; suffix = 'm'; }
    else if (yearsAgo >= 1e3) { divisor = 1e3; suffix = 'k'; }
    return String(parseFloat((yearsAgo / divisor).toPrecision(3))) + suffix;
  }

  // Round-number ticks for the window [lo, hi]: positions in elapsed years, text in years ago.
  function yearTicks(lo, hi) {
    var yaLo = Math.max(EARTH_AGE - hi, 0);
    var yaHi = Math.min(EARTH_AGE - lo, EARTH_AGE);
    var rawStep = Math.max(yaHi - yaLo, 1e-6) / 7;
    var magnitude = Math.pow(10, Math.floor(Math.log10(rawStep)));
    var ratio = rawStep / magnitude;
    var step = magnitude * (ratio < 1.5 ? 1 : ratio < 3.5 ? 2 : ratio < 7.5 ? 5 : 10);
    var values = [];
    var text = [];
    for (var k = Math.ceil(yaLo / step); k * step <= yaHi + step * 1e-9; k++) {
      var yearsAgo = k * step;
      values.push(EARTH_AGE - yearsAgo);
      text.push(yearsAgo <= 1e-4 ? 'now' : formatYa(yearsAgo));
    }
    return { values: values, text: text };
  }

  function visibleWindow() {
    var r = gd._fullLayout.xaxis.range;
    return [Math.min(r[0], r[1]), Math.max(r[0], r[1])];
  }

  // Min and max y of one panel's data inside [lo, hi], or null if there is none.
  function yExtent(panel, lo, hi, isLog) {
    var ymin = Infinity;
    var ymax = -Infinity;

    function take(y) {
      if (typeof y !== 'number' || !isFinite(y) || (isLog && y <= 0)) return;
      if (y < ymin) ymin = y;
      if (y > ymax) ymax = y;
    }

    gd.data.forEach(function (trace) {
      if ((trace.yaxis || 'y') !== panel.yId || trace.type === 'bar') return;
      var xs = trace.x;
      var ys = trace.y;
      if (!xs || !ys) return;

      // For a main line, also take the points just outside the window: the line is drawn
      // through them, so a window between two sparse points must still fit their values.
      var interpolated = trace.meta && trace.meta.role === 'series';
      var before = -1;
      var after = -1;

      for (var i = 0; i < xs.length; i++) {
        var xv = xs[i];
        if (xv === null) continue;
        if (xv < lo) {
          before = i;
        } else if (xv > hi) {
          if (after < 0) after = i;
          if (!interpolated) continue;
        } else {
          take(ys[i]);
        }
      }
      if (interpolated) {
        if (before >= 0) take(ys[before]);
        if (after >= 0) take(ys[after]);
      }
    });

    return ymin === Infinity ? null : [ymin, ymax];
  }

  function paddedRange(extent, isLog, reversed) {
    var low;
    var high;
    if (isLog) {
      var a = Math.log10(extent[0]);
      var b = Math.log10(extent[1]);
      var padLog = Math.max((b - a) * 0.12, 0.25);  // log axes are in decades
      low = a - padLog;
      high = b + padLog;
    } else {
      var span = extent[1] - extent[0];
      var pad = span > 0 ? span * 0.08 : (Math.abs(extent[1]) * 0.1 || 1);
      low = extent[0] - pad;
      high = extent[1] + pad;
    }
    return reversed ? [high, low] : [low, high];
  }

  // The layout changes for the visible window [w0, w1]: x tick labels, "no data" notes and each
  // panel's y range. Returned, not applied: see onTimeWindowChanged().
  function layoutUpdate(w) {
    var update = {};
    var ticks = yearTicks(w[0], w[1]);

    panels.forEach(function (panel) {
      update[panel.xKey + '.tickvals'] = ticks.values;
      update[panel.xKey + '.ticktext'] = ticks.text;

      var hasData = panel.dataMax >= w[0] && panel.dataMin <= w[1];
      update['annotations[' + panel.note + '].visible'] = !hasData;
      if (!panel.fit || !hasData) return;

      var axis = gd._fullLayout[panel.yKey];
      if (!axis) return;
      var isLog = axis.type === 'log';
      var extent = yExtent(panel, w[0], w[1], isLog);
      if (extent) update[panel.yKey + '.range'] = paddedRange(extent, isLog, panel.reversed);
    });

    return update;
  }

  // ---- bar labels ----
  // Span panels draw their bars and their labels as separate traces (see add_span_traces() in
  // src/plot_panels.R). Each label sits in the middle of the visible part of its bar and is
  // shortened, or hidden, to fit the room it has.
  var measure = document.createElement('canvas').getContext('2d');
  measure.font = '11px system-ui, -apple-system, "Segoe UI", Roboto, sans-serif';
  var ELLIPSIS = String.fromCharCode(8230);

  function asArray(value) {
    return Array.isArray(value) ? value : [value];
  }

  function fitLabel(label, widthPx) {
    var room = widthPx - 8;
    if (room < 14) return '';
    if (measure.measureText(label).width <= room) return label;
    for (var n = label.length - 1; n >= 3; n--) {
      var shorter = label.slice(0, n).replace(/\s+$/, '') + ELLIPSIS;
      if (measure.measureText(shorter).width <= room) return shorter;
    }
    return '';
  }

  // The data changes that put each label over the visible part of its bar: the traces to change
  // and their new x positions and texts. Returned, not applied: see onTimeWindowChanged().
  function labelUpdate(w) {
    var pxPerYear = gd._fullLayout.xaxis._length / (w[1] - w[0]);
    var indices = [];
    var allX = [];
    var allText = [];

    gd.data.forEach(function (trace, index) {
      // The names beside diamonds: shown where there is room before the next diamond or bar in the
      // same row (or the edge of the window); the diamonds themselves do not move.
      if (trace.meta && trace.meta.role === 'point-labels') {
        var px = trace.x;
        var py = trace.y;
        var names = asArray(trace.meta.labels);
        var order = names.map(function (_, i) { return i; }).sort(function (a, b) {
          return (py[a] - py[b]) || (px[a] - px[b]);
        });
        // The bars of this panel, as obstacles: where each starts, and in which row.
        var barLabelTrace = gd.data.filter(function (t) {
          return t.meta && t.meta.role === 'bar-labels' && (t.yaxis || 'y') === (trace.yaxis || 'y');
        })[0];
        var barStarts = barLabelTrace ? asArray(barLabelTrace.meta.x0) : [];
        var barRows = barLabelTrace ? asArray(barLabelTrace.y) : [];
        var shown = names.map(function () { return ''; });
        for (var k = 0; k < order.length; k++) {
          var point = order[k];
          if (px[point] < w[0] || px[point] > w[1]) continue;
          var sameRow = k + 1 < order.length && py[order[k + 1]] === py[point];
          var limit = Math.min(sameRow ? px[order[k + 1]] : Infinity, w[1]);
          for (var b = 0; b < barStarts.length; b++) {
            if (barRows[b] === py[point] && barStarts[b] > px[point] && barStarts[b] < limit) limit = barStarts[b];
          }
          // 14px for the diamond itself and the gap, then the room fitLabel() is given. The text
          // is padded with non-breaking spaces to stand clear of the diamond.
          var name = fitLabel(names[point], (limit - px[point]) * pxPerYear - 14);
          shown[point] = name ? '    ' + name : '';
        }
        indices.push(index);
        allX.push(px);
        allText.push(shown);
        return;
      }

      if (!(trace.meta && trace.meta.role === 'bar-labels')) return;

      var labels = asArray(trace.meta.labels);
      var starts = asArray(trace.meta.x0);
      var ends = asArray(trace.meta.x1);
      var xs = [];
      var texts = [];

      for (var i = 0; i < labels.length; i++) {
        var from = Math.max(starts[i], w[0]);
        var to = Math.min(ends[i], w[1]);
        if (to <= from) {  // the bar is entirely out of view
          xs.push((starts[i] + ends[i]) / 2);
          texts.push('');
        } else {
          xs.push((from + to) / 2);
          texts.push(fitLabel(labels[i], (to - from) * pxPerYear));
        }
      }
      indices.push(index);
      allX.push(xs);
      allText.push(texts);
    });

    return { indices: indices, data: { x: allX, text: allText } };
  }

  function reportToShiny() {
    if (!window.Shiny || !Shiny.setInputValue) return;
    clearTimeout(reportTimer);
    reportTimer = setTimeout(function () {
      Shiny.setInputValue('x_range', visibleWindow(), { priority: 'event' });
    }, 120);
  }

  // Everything that follows from the visible time window, applied in ONE Plotly.update() call. Two
  // separate calls (a relayout for the axes, a restyle for the labels) race each other once there
  // are many panels, and the labels are then left undrawn.
  function onTimeWindowChanged() {
    var w = visibleWindow();
    var layout = layoutUpdate(w);
    var labels = labelUpdate(w);

    // Skip the redraw when nothing changed (also stops this feeding back into itself).
    var key = JSON.stringify(layout) + '|' + w.join(',') + '|' + gd._fullLayout.xaxis._length;
    if (key !== lastKey) {
      lastKey = key;
      if (labels.indices.length) {
        Plotly.update(gd, labels.data, layout, labels.indices);
      } else {
        Plotly.relayout(gd, layout);
      }
    }
    reportToShiny();
  }

  function start() {
    if (started) return;
    started = true;
    onTimeWindowChanged();
  }

  // Any change to an x axis (drag-zoom, pan, double-click reset, or a Shiny-side relayout), or
  // to the plot's size, which changes how much room the labels have.
  // Our own y refits carry only y keys, so they never come back through here.
  //
  // This script runs again every time the figure is redrawn (e.g. a metric is ticked), on the
  // same element. Drop the previous run's listener first, or the runs pile up and fight over
  // the same traces.
  if (gd._timelineRelayoutHandler) gd.removeListener('plotly_relayout', gd._timelineRelayoutHandler);
  gd._timelineRelayoutHandler = function (event) {
    var affectsLabels = Object.keys(event).some(function (key) {
      return /^xaxis\d*\.(range|autorange)/.test(key) || key === 'autosize' || key === 'width';
    });
    if (affectsLabels) onTimeWindowChanged();
  };
  gd.on('plotly_relayout', gd._timelineRelayoutHandler);

  // Plotly's own `responsive` option only reacts to the *window* resizing. Collapsing the
  // sidebar changes the plot's width without that, so watch the plot's element directly. (Its
  // parent is `display: contents`, which has no box of its own to observe.)
  if (window.ResizeObserver) {
    if (gd._timelineResizeObserver) gd._timelineResizeObserver.disconnect();
    var resizeTimer = null;
    gd._timelineResizeObserver = new ResizeObserver(function () {
      clearTimeout(resizeTimer);
      resizeTimer = setTimeout(function () {
        // Plotly refuses to resize an element that is not displayed (mid-redraw, or hidden).
        var displayed = gd.isConnected && gd.clientWidth > 0;
        if (displayed && gd._fullLayout && Math.abs(gd.clientWidth - gd._fullLayout.width) > 1) {
          Plotly.Plots.resize(gd).catch(function () {});
        }
      }, 100);
    });
    gd._timelineResizeObserver.observe(gd);
  }

  // This script runs when the figure is handed to plotly, which draws a moment later. On a
  // redraw, plotly still holds the previous figure's layout until then (which can have fewer
  // panels), so wait until the layout it holds is this figure's and has been laid out.
  var token = x.layout.meta.token;
  function drawn() {
    var layout = gd._fullLayout;
    return !!(layout && layout.meta && layout.meta.token === token && gd.calcdata &&
      panels.every(function (panel) { return layout[panel.yKey] && layout[panel.yKey]._length > 0; }));
  }
  (function waitUntilDrawn(attempt) {
    if (drawn() || attempt >= 100) {
      start();
    } else {
      setTimeout(function () { waitUntilDrawn(attempt + 1); }, 50);
    }
  })(0);
}
