// Scene: from a time plot (autoplot) to a horizon plot (ggHoriPlot).
//
// geom_horizon() splits a series into bands about an origin. Each band is
// drawn as a ribbon from 0 to the fraction v of the band that is filled, so
// in the time plot's units each band is an affine image of its ribbon:
//   y = origin + sign * (offset + step * v)
// where offset is the distance of the band's inner edge from the origin and
// step is its width. Mirroring flips the sign of the bands below the origin,
// and collapsing moves each band's inner edge onto the origin.
//
// Steps:
//   0. The time plot, exactly as autoplot() draws it.
//   1. The area between the series and the origin is filled with bands.
//   2. The bands below the origin are mirrored above it.
//   3. The bands are collapsed onto the origin, and the y range narrows to a
//      single band.
//   4. The panel moves into place among the horizon plots of other series,
//      giving the ggHoriPlot output.
PlotMorph.register("horizon", function (svg, data, h) {
  const { el, lerp, ease, phase, lerpBox } = h;
  const series = data.series;
  const origin = data.origin;
  const focus = data.focus;
  const bands = data.bands[focus];
  const target = data.to.panels[focus];
  // Height of a collapsed band, in the units of the series
  const unit = bands.find((b) => b.sign > 0 && b.offset === 0)?.step ?? bands[0].step;
  const lineStyle = {
    fill: "none",
    "stroke-linecap": "butt",
    "stroke-linejoin": "round",
    "stroke-miterlimit": 10,
  };

  // Layers, bottom to top
  const views = ["from", "mid", "stack"];
  const size = { width: data.width, height: data.height };
  const chrome = [...views.map((v) => data[v].chrome), data.to.chrome].map((c) =>
    h.svg(c, size, svg)
  );
  const common = { preserveAspectRatio: "none", class: "morph-grill" };
  const grills = [...views.map((v) => data[v].grill), data.to.grills[focus]].map((g) =>
    h.svg(g, common, svg)
  );
  const homes = [...views.map((v) => data[v].panel), target];

  // The other series, in their panels of the horizon plot
  const others = el("g", {}, svg);

  const uid = svg.closest(".morph-figure").id;
  const defs = el("defs", {}, svg);
  const clip = el("rect", {}, el("clipPath", { id: `${uid}-panel` }, defs));
  const content = el("g", { "clip-path": `url(#${uid}-panel)` }, svg);

  const ribbons = bands.map((b) => el("polygon", { fill: b.fill }, content));
  // The outer edge of each band, and the origin
  const cutLines = bands.map(() =>
    el(
      "line",
      { stroke: "#595959", "stroke-width": 0.75, "stroke-dasharray": "4 3" },
      content
    )
  );
  const originLine = el("line", { stroke: "#595959", "stroke-width": 0.75 }, content);
  const line = el(
    "polyline",
    { ...lineStyle, "stroke-width": series.stroke_width, stroke: series.colour },
    content
  );

  // Scale data to a panel box, given the data ranges
  function scaler(box, xr, yr) {
    return {
      x: (v) => box.x + ((v - xr[0]) / (xr[1] - xr[0])) * box.w,
      y: (v) => box.y + ((yr[1] - v) / (yr[1] - yr[0])) * box.h,
    };
  }

  // Points of a ribbon from a baseline, in SVG units
  function ribbon(xs, ys, base) {
    const upper = xs.map((x, i) => x.toFixed(2) + "," + ys[i].toFixed(2));
    const lower = xs.map((x) => x.toFixed(2) + "," + base.toFixed(2)).reverse();
    return upper.concat(lower).join(" ");
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

  function setLine(node, box, y) {
    node.setAttribute("x1", round(box.x));
    node.setAttribute("x2", round(box.x + box.w));
    node.setAttribute("y1", round(y));
    node.setAttribute("y2", round(y));
  }

  // The other series are drawn once, in place
  data.to.panels.forEach((panel, k) => {
    if (k === focus) return;
    const grill = h.svg(data.to.grills[k], common, others);
    setGrill(grill, panel, panel);
    const s = scaler(panel, panel.xrange, panel.yrange);
    data.bands[k].forEach((b) => {
      el(
        "polygon",
        { points: ribbon(b.x.map(s.x), b.v.map(s.y), s.y(0)), fill: b.fill },
        others
      );
    });
  });

  // geom_horizon() only evaluates each band at the observations, so a band
  // interpolated between two observations departs from the series wherever
  // it crosses a band edge. The series is evaluated where it crosses each
  // edge, and each band is given there both the fraction filled by the
  // series (exact) and the fraction geom_horizon() interpolates (gg).
  const edges = [origin];
  bands.forEach((b) => edges.push(origin + b.sign * b.offset, origin + b.sign * (b.offset + b.step)));
  const fine = { x: [series.x[0]], y: [series.y[0]] };
  for (let i = 1; i < series.x.length; i++) {
    const [x0, x1, y0, y1] = [series.x[i - 1], series.x[i], series.y[i - 1], series.y[i]];
    edges
      .filter((e) => (e - y0) * (e - y1) < 0)
      .map((e) => (e - y0) / (y1 - y0))
      .sort((a, b) => a - b)
      .forEach((t) => {
        fine.x.push(lerp(x0, x1, t));
        fine.y.push(lerp(y0, y1, t));
      });
    fine.x.push(x1);
    fine.y.push(y1);
  }
  function interp(xs, vs, x) {
    let j = 1;
    while (j < xs.length - 1 && xs[j] < x) j++;
    const t = (x - xs[j - 1]) / (xs[j] - xs[j - 1]);
    return lerp(vs[j - 1], vs[j], h.clamp(t));
  }
  const fineBands = bands.map((b) => ({
    exact: fine.y.map((y) => h.clamp((b.sign * (y - origin) - b.offset) / b.step)),
    gg: fine.x.map((x) => interp(b.x, b.v, x)),
  }));

  const lerpRange = (a, b, t) => [0, 1].map((i) => lerp(a[i], b[i], t));
  const [from, mid, stack] = views.map((v) => data[v].panel);
  const collapsedRange = target.yrange.map((v) => origin + unit * v);

  return function render(p) {
    const fill = ease(phase(p, 0, 1));
    const mirror = ease(phase(p, 1, 2));
    const collapse = ease(phase(p, 2, 3));
    const place = ease(phase(p, 3, 4));

    // Axes and panels: the legend is added, the y-axis is hidden as the
    // bands collapse, and the horizon plot is shown as the panel lands
    const fades = [1, phase(p, 0.2, 0.8), phase(p, 2, 2.1), phase(p, 3.5, 4)];
    const opacity = fades.map((o, i) => (i + 1 < fades.length && fades[i + 1] >= 1 ? 0 : o));
    chrome.forEach((node, i) => show(node, opacity[i]));

    let box;
    if (p <= 1) box = lerpBox(from, mid, fill);
    else if (p <= 3) box = lerpBox(mid, stack, collapse);
    else box = lerpBox(stack, target, place);
    grills.forEach((node, i) => {
      setGrill(node, box, homes[i]);
      show(node, opacity[i]);
    });
    setBox(clip, box);
    show(others, phase(p, 3.5, 4));

    const xr = p <= 1
      ? lerpRange(from.xrange, mid.xrange, fill)
      : lerpRange(mid.xrange, target.xrange, place);
    const yr = p <= 1
      ? lerpRange(from.yrange, mid.yrange, fill)
      : lerpRange(mid.yrange, collapsedRange, collapse);
    const s = scaler(box, xr, yr);

    // Position of a fraction v of each band, as the bands grow from the
    // origin, are mirrored and are collapsed
    function bandY(b, v, grow) {
      const sign = b.sign > 0 ? 1 : 2 * mirror - 1;
      const r = lerp(b.offset, 0, collapse) + lerp(b.step, unit, collapse) * v;
      return s.y(origin + grow * sign * r);
    }

    // Bands follow the series until they collapse into geom_horizon()'s bands
    bands.forEach((b, k) => {
      const f = fineBands[k];
      const xs = fine.x.map(s.x);
      const ys = f.exact.map((v, i) => bandY(b, lerp(v, f.gg[i], collapse), fill));
      ribbons[k].setAttribute("points", ribbon(xs, ys, bandY(b, 0, fill)));
      show(ribbons[k], fill);

      setLine(cutLines[k], box, bandY(b, 1, 1));
      show(cutLines[k], fill * (1 - phase(p, 2.6, 3)));
    });
    setLine(originLine, box, s.y(origin));
    show(originLine, fill * (1 - phase(p, 2.6, 3)));

    // The series is drawn over the bands until they are mirrored
    line.setAttribute(
      "points",
      series.x.map((x, i) => s.x(x).toFixed(2) + "," + s.y(series.y[i]).toFixed(2)).join(" ")
    );
    show(line, 1 - phase(p, 1, 1.3));
  };
});
