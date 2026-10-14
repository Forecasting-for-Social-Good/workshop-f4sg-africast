// Scene: from a time plot (autoplot) to an ACF plot (autoplot of ACF()).
//
// The series is paired with a copy of itself lagged by one period, and each
// pair becomes a point of a lag plot. As the y value of each point is the
// same in the time plot and the lag plot, the points only move sideways. The
// lag plot sits beside the ACF plot being built, which gains a bar for each
// lag as its lag plot is shown.
//
// Steps:
//   0. The time plot, exactly as autoplot() draws it.
//   1. A copy of the series lagged by one period is added.
//   2. Each observation and the one before it become a point of a lag plot.
//   3. The autocorrelation of the lag plot is added as the first bar.
//   4. The lag plot is shown for each lag in turn, adding its bar.
//   5. The lag plot is removed and the ACF plot is widened, with its
//      significance bounds, giving the autoplot(ACF()) output.
PlotMorph.register("acf", function (svg, data, h) {
  const { el, clamp, lerp, ease, phase, lerpBox, lerpColour } = h;
  const series = data.series;
  const acf = data.acf;
  const n = series.y.length;
  const nLags = acf.lag.length;
  const lineStyle = {
    fill: "none",
    "stroke-linecap": "butt",
    "stroke-linejoin": "round",
    "stroke-miterlimit": 10,
  };

  // Layers, bottom to top
  const views = ["from", "lag", "bars", "to"];
  const chrome = views.map((v) =>
    h.svg(
      data[v].chrome,
      { x: data[v].offset, width: data[v].width, height: data.height },
      svg
    )
  );
  const common = { preserveAspectRatio: "none", class: "morph-grill" };
  const grills = views.map((v) => h.svg(data[v].grill, common, svg));

  // Panels of each view, placed on the full plot
  const place = (v) => ({ ...data[v].panel, x: data[v].panel.x + data[v].offset });
  const [fromBox, lagBox, barsBox, toBox] = views.map(place);

  const uid = svg.closest(".morph-figure").id;
  const defs = el("defs", {}, svg);
  const clip = el("rect", {}, el("clipPath", { id: `${uid}-series` }, defs));
  const content = el("g", { "clip-path": `url(#${uid}-series)` }, svg);

  // The time plot, the lagged copy, and the pairs between them
  const pairs = series.y.map(() =>
    el("line", { stroke: "#b3b3b3", "stroke-width": 0.75 }, content)
  );
  const baseLine = el(
    "polyline",
    { ...lineStyle, "stroke-width": series.stroke_width, stroke: series.colour },
    content
  );
  const lagLine = el(
    "polyline",
    { ...lineStyle, "stroke-width": series.stroke_width, stroke: data.lag_colour },
    content
  );
  const lagPoints = series.y.map(() => el("circle", { r: 1.8, fill: data.lag_colour }, content));
  const points = series.y.map(() => el("circle", { r: 1.8, fill: series.colour }, content));
  const label = el(
    "text",
    {
      "font-family": "Fira Sans, sans-serif",
      "font-size": 13.2,
      fill: "#1a1a1a",
    },
    svg
  );

  // The ACF plot
  const bars = el("g", {}, svg);
  const zero = el(
    "line",
    { "stroke-width": data.zero.stroke_width, stroke: data.zero.colour, "stroke-linecap": "butt" },
    bars
  );
  const barLines = acf.lag.map(() =>
    el("line", { "stroke-width": acf.stroke_width, "stroke-linecap": "butt" }, bars)
  );
  const ciLines = data.ci.y.map(() =>
    el(
      "line",
      {
        "stroke-width": data.ci.stroke_width,
        stroke: data.ci.colour,
        "stroke-dasharray": `${data.ci.dasharray},${data.ci.dasharray}`,
        "stroke-linecap": "butt",
      },
      bars
    )
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
  function setLine(node, x1, y1, x2, y2) {
    node.setAttribute("x1", round(x1));
    node.setAttribute("y1", round(y1));
    node.setAttribute("x2", round(x2));
    node.setAttribute("y2", round(y2));
  }

  // Grills are drawn in place on their own plot, so view just their panel
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

  // Time of the observation i periods on (extrapolated past the end)
  const step = series.x[n - 1] - series.x[n - 2];
  const timeAt = (i) => (i < n ? series.x[i] : series.x[n - 1] + (i - n + 1) * step);

  // Each lag after the first moves the points, then grows its bar
  const move = 0.6;
  function lagState(t) {
    const pos = t * (nLags - 1);
    const k = Math.min(Math.floor(pos), nLags - 2);
    const q = pos - k;
    return {
      lag: k + 1, // the lag being moved from
      move: ease(phase(q, 0, move)),
      grow: ease(phase(q, 1 - move, 1)),
    };
  }

  return function render(p) {
    const copy = ease(phase(p, 0, 1));
    const scatter = ease(phase(p, 1, 2));
    const first = ease(phase(p, 2, 3));
    const cycle = phase(p, 3, 4);
    const drop = phase(p, 4, 4.3);
    const widen = ease(phase(p, 4.3, 5));

    // Axes and panels
    // The ACF plot's axes are hidden while it widens, but not its panel
    const appear = phase(p, 1.4, 2);
    const opacity = [1 - phase(p, 1, 1.4), appear * (1 - drop), appear, phase(p, 4.8, 5)];
    chrome.forEach((node, i) => show(node, i === 2 ? appear * (1 - phase(p, 4.3, 4.45)) : opacity[i]));

    const box = lerpBox(fromBox, lagBox, scatter);
    const acfBox = lerpBox(barsBox, toBox, widen);
    setGrill(grills[0], box, data.from.panel);
    setGrill(grills[1], box, data.lag.panel);
    setGrill(grills[2], acfBox, data.bars.panel);
    setGrill(grills[3], acfBox, data.to.panel);
    show(grills[0], opacity[1] >= 1 ? 0 : opacity[0]);
    show(grills[1], opacity[1]);
    show(grills[2], opacity[3] >= 1 ? 0 : opacity[2]);
    show(grills[3], opacity[3]);
    setBox(clip, box);

    // Scales of the time plot and the lag plot, as the panel moves
    const sTime = scaler(box, data.from.panel.xrange, data.from.panel.yrange);
    const sLag = scaler(box, data.lag.panel.xrange, data.lag.panel.yrange);

    // The lag shown, and how far the points have moved to the next lag
    const state = p < 3 ? { lag: 1, move: 0, grow: 0 } : lagState(cycle);
    const lagX = (i) => {
      const a = i - state.lag;
      const b = a - 1;
      if (a < 0) return null;
      return b < 0 ? series.y[a] : lerp(series.y[a], series.y[b], state.move);
    };

    // The series and its lagged copy, sliding along by one period
    const timeY = series.y.map((v) => sTime.y(v));
    const lagTime = series.y.map((_, i) => lerp(timeAt(i), timeAt(i + 1), copy));
    baseLine.setAttribute("points", polyline(series.x.map((x, i) => [sTime.x(x), timeY[i]])));
    lagLine.setAttribute("points", polyline(lagTime.map((x, i) => [sTime.x(x), timeY[i]])));
    // The lines leave before they are squeezed into the lag plot
    const lines = 1 - phase(p, 1, 1.4);
    show(baseLine, lines);
    show(lagLine, copy * lines);

    series.y.forEach((v, i) => {
      // The pair of each observation: the lagged copy's value at its time
      const pairX = i > 0 ? sTime.x(series.x[i]) : null;
      const pairY = i > 0 ? timeY[i - 1] : null;
      if (i > 0) setLine(pairs[i], pairX, timeY[i], pairX, pairY);
      show(pairs[i], i > 0 ? copy * lines * 0.8 : 0);

      // Observations move sideways into the lag plot, and their pairs join them
      const lx = lagX(i);
      const x = lx === null ? sTime.x(series.x[i]) : lerp(sTime.x(series.x[i]), sLag.x(lx), scatter);
      const y = lerp(timeY[i], sLag.y(v), scatter);
      points[i].setAttribute("cx", x);
      points[i].setAttribute("cy", y);
      // Points without a pair at this lag fade away
      const paired = i >= state.lag + 1 ? 1 : i >= state.lag ? 1 - state.move : 0;
      show(points[i], copy * (i >= 1 ? paired : 1 - scatter) * (1 - drop));
    });

    // The lagged copy's points join the observation they are paired with
    lagPoints.forEach((pt, i) => {
      if (i + 1 < n) {
        const q = points[i + 1];
        pt.setAttribute("cx", lerp(sTime.x(lagTime[i]), Number(q.getAttribute("cx")), scatter));
        pt.setAttribute("cy", lerp(timeY[i], Number(q.getAttribute("cy")), scatter));
      }
      show(pt, i + 1 < n ? copy * (1 - scatter) : 0);
    });

    // The lag and autocorrelation of the lag plot
    const shown = state.lag + (state.move >= 0.5 ? 1 : 0);
    const r = acf.value[shown - 1];
    // In the lag plot's (empty) title
    label.setAttribute("x", round(lagBox.x));
    label.setAttribute("y", round(lagBox.y - 6));
    label.textContent =
      `lag ${shown}` + (p >= 2.5 || shown > 1 ? `, r = ${r.toFixed(2).replace("-", "−")}` : "");
    show(label, phase(p, 1.6, 2) * (1 - drop));

    // The ACF plot, with a bar for each lag shown
    const sAcf = scaler(
      acfBox,
      data.bars.panel.xrange.map((v, i) => lerp(v, data.to.panel.xrange[i], widen)),
      data.bars.panel.yrange.map((v, i) => lerp(v, data.to.panel.yrange[i], widen))
    );
    const y0 = sAcf.y(0);
    setLine(zero, acfBox.x, sAcf.y(data.zero.y), acfBox.x + acfBox.w, sAcf.y(data.zero.y));
    show(zero, phase(p, 1.6, 2));

    const current = p < 3 ? 1 : state.lag + (state.grow > 0 ? 1 : 0);
    barLines.forEach((line, j) => {
      const k = j + 1;
      const grow =
        k === 1 ? first : k < state.lag + 1 || cycle >= 1 ? 1 : k === state.lag + 1 ? state.grow : 0;
      const x = sAcf.x(acf.lag[j]);
      // Drawn from the value to zero, as geom_segment() does in autoplot()
      setLine(line, x, sAcf.y(acf.value[j] * grow), x, y0);
      const highlight = k === current ? 1 - ease(phase(p, 4, 4.5)) : 0;
      line.setAttribute("stroke", lerpColour(acf.colour, data.lag_colour, highlight));
      show(line, grow > 0 ? 1 : 0);
    });

    ciLines.forEach((line, j) => {
      const y = sAcf.y(data.ci.y[j]);
      setLine(line, acfBox.x, y, acfBox.x + acfBox.w, y);
      show(line, phase(p, 4.5, 5));
    });
  };
});
