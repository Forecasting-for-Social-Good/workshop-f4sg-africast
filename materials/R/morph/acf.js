// Scene: from a time plot (autoplot) to an ACF plot (autoplot of ACF()).
//
// The time plot shrinks into a strip along the top, where the series is
// paired with a copy of itself lagged by one period. Each pair becomes a point
// of a lag plot below, beside the ACF plot being built. As the lagged copy
// shifts along the time plot, the lag plot changes to match and the ACF plot
// gains a bar for each lag.
//
// Steps:
//   0. The time plot, exactly as autoplot() draws it.
//   1. The time plot shrinks to the top, and a copy of the series lagged by
//      one period is added.
//   2. Each observation and the one before it become a point of a lag plot.
//   3. The autocorrelation of the lag plot is added as the first bar.
//   4. The lagged copy shifts one period at a time up to the seasonal lag
//      (data.pause_lag), showing the lag plot and adding the bar of each lag.
//   5. The remaining lags are shown in the same way.
//   6. The time plot and lag plot are removed and the ACF plot is widened,
//      with its significance bounds, giving the autoplot(ACF()) output.
PlotMorph.register("acf", function (svg, data, h) {
  const { el, lerp, ease, phase, lerpBox, lerpColour } = h;
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
  const views = ["from", "time", "lag", "bars", "to"];
  const chrome = views.map((v) =>
    h.svg(
      data[v].chrome,
      { x: data[v].x, y: data[v].y, width: data[v].width, height: data[v].height },
      svg
    )
  );
  const common = { preserveAspectRatio: "none", class: "morph-grill" };
  const grills = views.map((v) => h.svg(data[v].grill, common, svg));

  // Panels of each view, placed on the full plot
  const place = (v) => ({ ...data[v].panel, x: data[v].panel.x + data[v].x, y: data[v].panel.y + data[v].y });
  const [fromBox, timeBox, lagBox, barsBox, toBox] = views.map(place);

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
  // The points move between the time plot and the lag plot, so are not clipped
  const lagPoints = series.y.map(() => el("circle", { r: 1.8, fill: data.lag_colour }, svg));
  const points = series.y.map(() => el("circle", { r: 1.8, fill: series.colour }, svg));
  const text = {
    "font-family": "Fira Sans, sans-serif",
    "font-size": 13.2,
    fill: "#1a1a1a",
  };
  const label = el("text", text, svg);
  // The series and its lagged copy, named in their colours
  const timeLabel = el("text", text, svg);
  el("tspan", { fill: series.colour }, timeLabel).textContent = series.label;
  const lagName = el("tspan", { fill: data.lag_colour, dx: 12 }, timeLabel);

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

  // Time of the observation u periods on, between observations and
  // extrapolated past the end
  const step = series.x[n - 1] - series.x[n - 2];
  const timeAt = (i) => (i < n ? series.x[i] : series.x[n - 1] + (i - n + 1) * step);
  const timeAtF = (u) => {
    const i = Math.floor(u);
    return lerp(timeAt(i), timeAt(i + 1), u - i);
  };

  // Each lag after the first moves the points, then grows its bar. pos is the
  // number of lags moved on from the first.
  const move = 0.6;
  const pause = data.pause_lag;
  function lagState(pos) {
    const k = Math.min(Math.floor(pos), nLags - 2);
    const q = pos - k;
    return {
      lag: k + 1, // the lag being moved from
      move: ease(phase(q, 0, move)),
      grow: ease(phase(q, 1 - move, 1)),
    };
  }

  return function render(p) {
    const shrink = ease(phase(p, 0, 0.5));
    const copy = ease(phase(p, 0.4, 1));
    const scatter = ease(phase(p, 1, 2));
    const first = ease(phase(p, 2, 3));
    // Lags up to the pause, then the rest
    const cycle = phase(p, 3, 4) * (pause - 1) + phase(p, 4, 5) * (nLags - pause);
    const drop = phase(p, 5, 5.3);
    const widen = ease(phase(p, 5.3, 6));

    // Axes and panels
    // The ACF plot's axes are hidden while it widens, but not its panel
    const strip = phase(p, 0.35, 0.5);
    const appear = phase(p, 1.4, 2);
    const opacity = [
      1 - phase(p, 0, 0.15),
      strip * (1 - drop),
      appear * (1 - drop),
      appear * (1 - phase(p, 5.3, 5.45)),
      phase(p, 5.8, 6),
    ];
    chrome.forEach((node, i) => show(node, opacity[i]));

    const box = lerpBox(fromBox, timeBox, shrink);
    const acfBox = lerpBox(barsBox, toBox, widen);
    setGrill(grills[0], box, data.from.panel);
    setGrill(grills[1], box, data.time.panel);
    setGrill(grills[2], lagBox, data.lag.panel);
    setGrill(grills[3], acfBox, data.bars.panel);
    setGrill(grills[4], acfBox, data.to.panel);
    show(grills[0], strip >= 1 ? 0 : 1);
    show(grills[1], opacity[1]);
    show(grills[2], opacity[2]);
    show(grills[3], opacity[4] >= 1 ? 0 : appear);
    show(grills[4], opacity[4]);
    setBox(clip, box);

    // Scales of the time plot and the lag plot
    const sTime = scaler(box, data.from.panel.xrange, data.from.panel.yrange);
    const sLag = scaler(lagBox, data.lag.panel.xrange, data.lag.panel.yrange);

    // The lag shown, and how far the points have moved to the next lag
    const state = p < 3 ? { lag: 1, move: 0, grow: 0 } : lagState(cycle);
    const shown = state.lag + (state.move >= 0.5 ? 1 : 0);
    const lagX = (i) => {
      const a = i - state.lag;
      const b = a - 1;
      if (a < 0) return null;
      return b < 0 ? series.y[a] : lerp(series.y[a], series.y[b], state.move);
    };

    // The series and its lagged copy, sliding along a period at a time
    const shift = p < 1 ? copy : state.lag + state.move;
    const timeY = series.y.map((v) => sTime.y(v));
    const lagTime = series.y.map((_, i) => timeAtF(i + shift));
    baseLine.setAttribute("points", polyline(series.x.map((x, i) => [sTime.x(x), timeY[i]])));
    lagLine.setAttribute("points", polyline(lagTime.map((x, i) => [sTime.x(x), timeY[i]])));
    show(baseLine, 1 - drop);
    show(lagLine, copy * (1 - drop));

    // Each observation and its pair, hidden while the lagged copy moves
    const pairOpacity = Math.abs(2 * state.move - 1);
    series.y.forEach((_, i) => {
      const j = i - shown;
      if (j >= 0) setLine(pairs[i], sTime.x(series.x[i]), timeY[i], sTime.x(series.x[i]), timeY[j]);
      show(pairs[i], j >= 0 ? copy * pairOpacity * 0.8 * (1 - drop) : 0);
    });

    series.y.forEach((v, i) => {
      // Observations move from the time plot into the lag plot, and their
      // pairs join them
      const lx = lagX(i);
      const tx = sTime.x(series.x[i]);
      const x = lx === null ? tx : lerp(tx, sLag.x(lx), scatter);
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

    // The lagged copy, in the time plot's (empty) title
    timeLabel.setAttribute("x", round(timeBox.x));
    timeLabel.setAttribute("y", round(timeBox.y - 6));
    lagName.textContent = `lagged by ${shown} ${acf.unit}${shown === 1 ? "" : "s"}`;
    lagName.setAttribute("opacity", copy);
    show(timeLabel, strip * (1 - drop));

    // The lag and autocorrelation of the lag plot
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
        k === 1 ? first : k < state.lag + 1 || p >= 5 ? 1 : k === state.lag + 1 ? state.grow : 0;
      const x = sAcf.x(acf.lag[j]);
      // Drawn from the value to zero, as geom_segment() does in autoplot()
      setLine(line, x, sAcf.y(acf.value[j] * grow), x, y0);
      const highlight = k === current ? 1 - ease(phase(p, 5, 5.5)) : 0;
      line.setAttribute("stroke", lerpColour(acf.colour, data.lag_colour, highlight));
      show(line, grow > 0 ? 1 : 0);
    });

    ciLines.forEach((line, j) => {
      const y = sAcf.y(data.ci.y[j]);
      setLine(line, acfBox.x, y, acfBox.x + acfBox.w, y);
      show(line, phase(p, 5.5, 6));
    });
  };
});
