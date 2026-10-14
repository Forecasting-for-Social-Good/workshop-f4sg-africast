// Scene: from a time plot (autoplot) to a seasonal plot (gg_season).
//
// The series is split at the boundaries between seasonal periods (such as
// years), with the line between periods split half-way. The periods are then
// stacked onto the first one at a time, while the x range narrows to fit them.
// Within a period, the time plot's x-axis and the seasonal plot's x-axis
// differ only by an offset, so each move is a translation.
//
// Steps:
//   0. The time plot, exactly as autoplot() draws it.
//   1. Each period is coloured and bounded, and the colour legend is added.
//   2. The periods are stacked onto the first, one at a time, with the later
//      periods moving along to close the gap and the x range narrowing.
//   3. The observations fade, giving the gg_season() output.
PlotMorph.register("season", function (svg, data, h) {
  const { el, lerp, ease, phase, lerpBox } = h;
  const series = data.series;
  const nPeriods = data.periods.colour.length;
  const lineStyle = {
    fill: "none",
    "stroke-linecap": "butt",
    "stroke-linejoin": "round",
    "stroke-miterlimit": 10,
  };

  // Observation indices for each period, in time order
  const periodIdx = Array.from({ length: nPeriods }, () => []);
  series.period.forEach((k, i) => periodIdx[k - 1].push(i));
  const first = periodIdx.map((idx) => idx[0]);
  const last = periodIdx.map((idx) => idx[idx.length - 1]);

  // Offset from a position within a period to the matching time in the
  // first period, and the shift that stacks each period onto the first
  const offset = series.x[0] - series.sx[0];
  const shift = first.map((i) => series.x[i] - (series.sx[i] + offset));
  const residual = series.x.map(
    (x, i) => x - shift[series.period[i] - 1] - (series.sx[i] + offset)
  );

  // Boundaries half-way between periods, extended by a period at each end
  const bounds = [];
  for (let k = 1; k < nPeriods; k++) {
    bounds[k] = (series.x[last[k - 1]] + series.x[first[k]]) / 2;
  }
  const period = (k) => series.x[first[k]] - series.x[first[k - 1]];
  bounds[0] = nPeriods > 1 ? bounds[1] - period(1) : series.x[0];
  bounds[nPeriods] =
    nPeriods > 1 ? bounds[nPeriods - 1] + period(nPeriods - 1) : series.x[last[0]];
  // Value of the series at a boundary
  const boundY = (k) => (series.y[last[k - 1]] + series.y[first[k]]) / 2;

  // Layers, bottom to top
  const views = ["from", "mid", "stack", "to"];
  const size = { width: data.width, height: data.height };
  const chrome = views.map((v) => h.svg(data[v].chrome, size, svg));
  const common = { preserveAspectRatio: "none", class: "morph-grill" };
  const grills = views.map((v) => h.svg(data[v].grill, common, svg));

  const uid = svg.closest(".morph-figure").id;
  const defs = el("defs", {}, svg);
  const clip = el("rect", {}, el("clipPath", { id: `${uid}-panel` }, defs));
  const content = el("g", { "clip-path": `url(#${uid}-panel)` }, svg);

  const boundLines = bounds.map(() =>
    el(
      "line",
      { stroke: "#595959", "stroke-width": 0.75, "stroke-dasharray": "4 3" },
      content
    )
  );
  const baseLine = el(
    "polyline",
    { ...lineStyle, "stroke-width": series.stroke_width, stroke: series.colour },
    content
  );

  // Lines are drawn oldest first, as gg_season() does, with points on top
  const lines = periodIdx.map((_, k) =>
    el(
      "polyline",
      {
        ...lineStyle,
        "stroke-width": data.periods.stroke_width,
        stroke: data.periods.colour[k],
      },
      content
    )
  );
  const points = periodIdx.map((idx, k) =>
    idx.map(() => el("circle", { r: 1.6, fill: data.periods.colour[k] }, content))
  );

  // Scale data to a panel box, given the data ranges
  function scaler(box, xr, yr) {
    return {
      x: (v) => box.x + ((v - xr[0]) / (xr[1] - xr[0])) * box.w,
      y: (v) => box.y + ((yr[1] - v) / (yr[1] - yr[0])) * box.h,
    };
  }

  // Rounded like svglite's coordinates, so the end frames match exactly
  const round = (v) => (Math.round(v * 100) / 100).toFixed(2);
  function setBox(node, box) {
    node.setAttribute("x", round(box.x));
    node.setAttribute("y", round(box.y));
    node.setAttribute("width", round(box.w));
    node.setAttribute("height", round(box.h));
  }

  // Grills are drawn in place on the full plot, so view just their panel
  function setGrill(node, box, home) {
    setBox(node, box);
    node.setAttribute("viewBox", [home.x, home.y, home.w, home.h].map(round).join(" "));
    node.classList.toggle(
      "is-scaled",
      Math.abs(box.w - home.w) >= 0.005 || Math.abs(box.h - home.h) >= 0.005
    );
  }

  function show(node, opacity) {
    node.style.display = opacity > 0 ? "" : "none";
    node.setAttribute("opacity", opacity);
  }

  const polyline = (pts) => pts.map(([x, y]) => x.toFixed(2) + "," + y.toFixed(2)).join(" ");

  // Periods are stacked one at a time, each move overlapping the next. The
  // progress of stacking period k (with the first period stacked throughout)
  const stackTime = Math.min(1, 1.5 / Math.max(1, nPeriods - 1));
  function stacking(t, k) {
    if (k === 0) return ease(t);
    const start = nPeriods > 2 ? ((k - 1) / (nPeriods - 2)) * (1 - stackTime) : 0;
    return ease(phase(t, start, start + stackTime));
  }

  // Expansion of the x range beyond the data, as a fraction of its width
  const expansion = (range, xs) => {
    const lo = Math.min(...xs);
    const hi = Math.max(...xs);
    return [(lo - range[0]) / (hi - lo), (range[1] - hi) / (hi - lo)];
  };
  const expandFrom = expansion(data.from.panel.xrange, series.x);
  const expandTo = expansion(data.to.panel.xrange, series.sx);

  const lerpRange = (a, b, t) => [0, 1].map((i) => lerp(a[i], b[i], t));

  return function render(p) {
    const colour = ease(phase(p, 0, 1));
    const stack = phase(p, 1, 2);
    const fade = ease(phase(p, 2, 3));

    // Axes and panels: the legend is added, the time axis is hidden while the
    // x range narrows, and the seasonal axis is shown as the last period lands
    const fades = [1, phase(p, 0.2, 0.8), phase(p, 1, 1.1), phase(p, 1.96, 2)];
    const opacity = fades.map((o, i) => (i + 1 < fades.length && fades[i + 1] >= 1 ? 0 : o));
    chrome.forEach((node, i) => show(node, opacity[i]));

    const [from, mid, , to] = views.map((v) => data[v].panel);
    const box = p <= 1 ? lerpBox(from, mid, colour) : lerpBox(mid, to, ease(stack));
    grills.forEach((node, i) => {
      setGrill(node, box, data[views[i]].panel);
      show(node, opacity[i]);
    });
    setBox(clip, box);

    // Positions of the observations in time units, as the periods are
    // stacked (with small differences in their lengths removed as they land)
    const u = periodIdx.map((_, k) => stacking(stack, k));
    const disp = [0];
    for (let k = 1; k < nPeriods; k++) disp[k] = disp[k - 1] + (shift[k] - shift[k - 1]) * u[k];
    const obsX = series.x.map((x, i) => {
      const k = series.period[i] - 1;
      return x - disp[k] - residual[i] * u[k];
    });

    // The x range fits the observations, expanded as ggplot2 would
    const lo = Math.min(...obsX);
    const hi = Math.max(...obsX);
    const expand = lerpRange(expandFrom, expandTo, p <= 1 ? 0 : ease(stack));
    const xr = [lo - expand[0] * (hi - lo), hi + expand[1] * (hi - lo)];
    const yr = p <= 1
      ? lerpRange(from.yrange, mid.yrange, colour)
      : lerpRange(mid.yrange, to.yrange, ease(stack));
    const s = scaler(box, xr, yr);

    // The original series, covered by the coloured periods
    baseLine.setAttribute(
      "points",
      polyline(series.x.map((x, i) => [s.x(x), s.y(series.y[i])]))
    );
    show(baseLine, p < 1 ? 1 : 0);

    // The first period's boundaries stay as a reference while the x range
    // narrows, until they leave the panel as the last period lands. The
    // others move with the period they start (or end, for the last) and are
    // removed as it lands on the first.
    bounds.forEach((b, k) => {
      const owner = k < 2 ? 0 : Math.min(k, nPeriods - 1);
      const x = round(s.x(b - disp[owner]));
      const line = boundLines[k];
      line.setAttribute("x1", x);
      line.setAttribute("x2", x);
      line.setAttribute("y1", round(box.y));
      line.setAttribute("y2", round(box.y + box.h));
      const stacked = owner === 0 ? (stack < 1 ? 0 : 1) : u[owner];
      show(line, colour * (1 - stacked));
    });

    periodIdx.forEach((idx, k) => {
      const pts = idx.map((i) => [s.x(obsX[i]), s.y(series.y[i])]);
      // Half of the lines to the neighbouring periods
      if (k > 0) pts.unshift([s.x(bounds[k] - disp[k]), s.y(boundY(k))]);
      if (k < nPeriods - 1) pts.push([s.x(bounds[k + 1] - disp[k]), s.y(boundY(k + 1))]);
      lines[k].setAttribute("points", polyline(pts));
      show(lines[k], colour);

      const obs = k > 0 ? pts.slice(1) : pts;
      points[k].forEach((pt, j) => {
        pt.setAttribute("cx", obs[j][0]);
        pt.setAttribute("cy", obs[j][1]);
        show(pt, colour * (1 - fade));
      });
    });
  };
});
