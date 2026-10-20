// Scene: building the windows of a rolled tsibble (stretch_tsibble(),
// slide_tsibble() or tile_tsibble()), one row per window.
//
// The axes and panel never move, so the chrome and grill are drawn once in
// place. Each window is a row of observations, coloured if they are in the
// window and grey if not. A new window starts as a copy of the one before it,
// and moves down to its own row while its observations change colour.
//
// Steps:
//   0. The observations, in the first row.
//   1. The first window is coloured.
//   2. The second window is copied from the first.
//   3. The remaining windows are copied in turn, giving the static plot.
PlotMorph.register("roll", function (svg, data, h) {
  const { el, ease, phase, lerp, lerpColour } = h;
  const size = { width: data.width, height: data.height };
  h.svg(data.chrome, size, svg);
  h.svg(data.grill, size, svg);

  const box = data.panel;
  const sx = (v) => box.x + ((v - box.xrange[0]) / (box.xrange[1] - box.xrange[0])) * box.w;
  const sy = (v) => box.y + ((box.yrange[1] - v) / (box.yrange[1] - box.yrange[0])) * box.h;

  const rows = data.rows.map((row) => {
    const members = new Set(row.members);
    const g = el("g", {}, svg);
    const points = data.x.map((x) =>
      el("circle", { cx: sx(x).toFixed(2), r: data.radius }, g)
    );
    return { y: sy(row.y), inside: data.x.map((x) => members.has(x)), g, points };
  });
  const nRows = rows.length;

  // Progress of adding each window: the first row is present from the
  // start, the second is added in step 2, and the rest share step 3
  function progress(p, k) {
    if (k === 0) return 1;
    if (k === 1) return ease(phase(p, 1, 2));
    const from = 2 + (k - 2) / (nRows - 2);
    const to = 2 + (k - 1) / (nRows - 2);
    return ease(phase(p, from, to));
  }

  return function render(p) {
    const first = ease(phase(p, 0, 1));
    rows.forEach((row, k) => {
      const q = progress(p, k);
      row.g.style.display = q > 0 ? "" : "none";
      if (q <= 0) return;
      const prev = k > 0 ? rows[k - 1] : null;
      const y = prev ? lerp(prev.y, row.y, q) : row.y;
      row.points.forEach((point, i) => {
        // How coloured each observation is, moving from the previous window's
        // membership to this window's
        const was = prev ? (prev.inside[i] ? 1 : 0) : 0;
        const now = row.inside[i] ? 1 : 0;
        const c = k === 0 ? now * first : lerp(was, now, q);
        point.setAttribute("cy", y.toFixed(2));
        point.setAttribute("fill", lerpColour(data.grey, data.colour, c));
      });
    });
  };
});
