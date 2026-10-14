// Scene: from a time plot (autoplot) to a seasonal subseries plot (gg_subseries).
//
// Each season gets a "card": a copy of the time plot's panel containing only
// that season's observations. The x-axis of every gg_subseries() panel spans
// the full time range, so each card's journey to its facet is an affine map.
//
// Steps:
//   0. The time plot, exactly as autoplot() draws it.
//   1. Observations are coloured by season, and the line fades.
//   2. Observations from the same season are joined into subseries.
//   3. The cards squeeze side by side into the facets.
//   4. Season means are added, giving exactly the gg_subseries() output.
PlotMorph.register("subseries", function (svg, data, h) {
  const { el, clamp, lerp, ease, phase, lerpBox, lerpColour } = h;
  const from = data.from;
  const to = data.to;
  const series = data.series;
  const nSeasons = to.panels.length;
  const ghost = "#b3b3b3";
  const lineStyle = {
    fill: "none",
    "stroke-linecap": "butt",
    "stroke-linejoin": "round",
    "stroke-miterlimit": 10,
  };

  // Layers, bottom to top. The cards' panel backgrounds are drawn beneath
  // all of their contents, so no subseries is hidden while the cards overlap.
  const chromeFrom = h.svg(from.chrome, { width: data.width, height: data.height }, svg);
  const chromeTo = h.svg(to.chrome, { width: data.width, height: data.height }, svg);
  const defs = el("defs", {}, svg);
  const backgrounds = el("g", {}, svg);

  const uid = svg.closest(".morph-figure").id;
  function clipRect(name) {
    const cp = el("clipPath", { id: `${uid}-${name}` }, defs);
    return el("rect", {}, cp);
  }

  // Observation indices for each season, in time order
  const seasonIdx = Array.from({ length: nSeasons }, () => []);
  series.season.forEach((s, i) => seasonIdx[s - 1].push(i));

  let baseClip, baseLine;
  const cards = seasonIdx.map((idx, k) => {
    const layer = el("g", {}, svg);
    const clip = clipRect(`card-${k}`);
    const common = { preserveAspectRatio: "none", class: "morph-grill" };
    const grillFrom = h.svg(from.grill, common, backgrounds);
    const grillTo = h.svg(to.grills[k], common, backgrounds);

    // The original series sits on the first card, in the time plot's panel
    if (k === 0) {
      baseClip = clipRect("base");
      baseLine = el(
        "polyline",
        { ...lineStyle, "stroke-width": series.stroke_width, "clip-path": `url(#${uid}-base)` },
        layer
      );
    }

    const content = el("g", { "clip-path": `url(#${uid}-card-${k})` }, layer);
    const line = el("polyline", { ...lineStyle, "stroke-width": series.stroke_width }, content);
    const mean = el(
      "polyline",
      { ...lineStyle, "stroke-width": data.means.stroke_width, stroke: data.means.colour },
      content
    );
    const points = idx.map(() => el("circle", { r: 1.6, fill: data.palette[k] }, content));
    return { idx, clip, grillFrom, grillTo, line, mean, points };
  });

  // Scale data to a panel box, given the data ranges
  function scaler(box, xr, yr) {
    return {
      x: (v) => box.x + ((v - xr[0]) / (xr[1] - xr[0])) * box.w,
      y: (v) => box.y + ((yr[1] - v) / (yr[1] - yr[0])) * box.h,
    };
  }

  // Points of a polyline through observations, revealed from left to right
  function linePoints(idx, s, reveal, xAt = (i) => series.x[i]) {
    if (reveal <= 0 || idx.length === 0) return "";
    const xs = idx.map(xAt);
    const cut = lerp(xs[0], xs[xs.length - 1], reveal);
    let d = "";
    for (let j = 0; j < idx.length; j++) {
      const x = xs[j];
      const y = series.y[idx[j]];
      if (x <= cut) {
        d += s.x(x).toFixed(2) + "," + s.y(y).toFixed(2) + " ";
      } else {
        const x0 = xs[j - 1];
        const y0 = series.y[idx[j - 1]];
        const yc = lerp(y0, y, (cut - x0) / (x - x0));
        d += s.x(cut).toFixed(2) + "," + s.y(yc).toFixed(2) + " ";
        break;
      }
    }
    return d;
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
  }

  function sameSize(a, b) {
    return Math.abs(a.w - b.w) < 0.005 && Math.abs(a.h - b.h) < 0.005;
  }

  function show(node, opacity) {
    node.style.display = opacity > 0 ? "" : "none";
    node.setAttribute("opacity", opacity);
  }

  return function render(p) {
    const colour = phase(p, 0, 1);
    const join = phase(p, 1, 2);
    const squeeze = ease(phase(p, 2, 3));
    const means = phase(p, 3, 4);

    show(chromeFrom, 1 - phase(p, 2, 2.5));
    show(chromeTo, phase(p, 2.5, 3));

    // Original series
    const sFrom = scaler(from.panel, from.panel.xrange, from.panel.yrange);
    setBox(baseClip, from.panel);
    baseLine.setAttribute("points", linePoints(series.x.map((_, i) => i), sFrom, 1));
    baseLine.setAttribute("stroke", lerpColour(series.colour, ghost, ease(colour)));
    show(baseLine, 1 - ease(join));

    cards.forEach((card, k) => {
      const target = to.panels[k];
      const box = lerpBox(from.panel, target, squeeze);
      const xr = [0, 1].map((i) => lerp(from.panel.xrange[i], target.xrange[i], squeeze));
      const yr = [0, 1].map((i) => lerp(from.panel.yrange[i], target.yrange[i], squeeze));
      const s = scaler(box, xr, yr);

      setBox(card.clip, box);
      setGrill(card.grillFrom, box, from.panel);
      setGrill(card.grillTo, box, target);
      card.grillFrom.classList.toggle("is-scaled", !sameSize(box, from.panel));
      card.grillTo.classList.toggle("is-scaled", !sameSize(box, target));
      // Draw each panel background once, so overlapping copies stay exact
      const toOpacity = phase(p, 2.6, 3);
      // The other cards' backgrounds are shown once they move off the first
      const lift = k === 0 || p > 2 ? 1 : 0;
      show(card.grillFrom, toOpacity < 1 ? lift : 0);
      show(card.grillTo, toOpacity);

      // Subseries, drawn in turn, then coloured like the original series
      const reveal = ease(clamp((join - k * 0.12) / (1 - (nSeasons - 1) * 0.12)));
      // Observations move from their time to their position in the facet
      const xAt = (i) => lerp(series.x[i], series.sx[i], squeeze);
      card.line.setAttribute("points", linePoints(card.idx, s, reveal, xAt));
      card.line.setAttribute("stroke", lerpColour(data.palette[k], series.colour, ease(phase(p, 3, 3.5))));
      show(card.line, reveal > 0 ? 1 : 0);

      const pointOpacity = ease(colour) * (1 - ease(phase(p, 3, 3.5)));
      card.idx.forEach((i, j) => {
        card.points[j].setAttribute("cx", s.x(xAt(i)));
        card.points[j].setAttribute("cy", s.y(series.y[i]));
      });
      card.points.forEach((pt) => show(pt, pointOpacity));

      // Season means, drawn across the panel
      const r = ease(phase(means, 0.3, 1));
      const ym = s.y(data.means.y[k]).toFixed(2);
      card.mean.setAttribute(
        "points",
        `${box.x.toFixed(2)},${ym} ${(box.x + box.w * r).toFixed(2)},${ym}`
      );
      show(card.mean, r > 0 ? 1 : 0);
    });
  };
});
