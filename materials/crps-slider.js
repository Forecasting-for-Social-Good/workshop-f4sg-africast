// Interactive CRPS explainer (.crps-slider): a Normal forecast distribution
// with sliders for its mean and standard deviation, the observation's step
// function at y, and the area between them. Uses the closed form of the CRPS
// of a Normal, and the deck's slider styles (.webr-slider-* in theme.scss).
(function () {
  var INK = "#434A5C", GRID = "#D9E2C8", MINOR = "#E8EEDD";
  var BLUE = "#2B6CB0", ORANGE = "#E07B39";
  var WIDTH = 1256, HEIGHT = 340, XLIM = 6;
  var Y = 0;

  function erf(x) {
    // Abramowitz-Stegun 7.1.26 is too coarse for 3 decimals; use a series/continued fraction
    var s = x < 0 ? -1 : 1, a = Math.abs(x);
    if (a > 6) return s;
    if (a < 2.5) {
      var sum = a, term = a, n = 0;
      while (Math.abs(term) > 1e-16 * Math.abs(sum)) {
        n++;
        term *= -a * a / n;
        sum += term / (2 * n + 1);
      }
      return s * 2 / Math.sqrt(Math.PI) * sum;
    }
    // Continued fraction for erfc
    var f = 0;
    for (var k = 60; k >= 1; k--) f = k / 2 / (a + f);
    return s * (1 - Math.exp(-a * a) / Math.sqrt(Math.PI) / (a + f));
  }
  function Phi(z) { return 0.5 * (1 + erf(z / Math.SQRT2)); }
  function phi(z) { return Math.exp(-z * z / 2) / Math.sqrt(2 * Math.PI); }
  function crps(mu, sigma, y) {
    var z = (y - mu) / sigma;
    return sigma * (z * (2 * Phi(z) - 1) + 2 * phi(z) - 1 / Math.sqrt(Math.PI));
  }
  window.crpsNormal = crps;

  function setup(el) {
    var spec = {
      mu: { label: "Forecast mean (μ)", value: 1, min: -3, max: 3, step: 0.1, ticks: [-3, -2, -1, 0, 1, 2, 3] },
      sigma: { label: "Forecast sd (σ)", value: 1, min: 0.1, max: 3, step: 0.1, ticks: [0.5, 1, 1.5, 2, 2.5, 3] }
    };
    var RES = 1000;
    var names = Object.keys(spec);
    var values = {}, inputs = {}, outputs = {};

    var readout = document.createElement("div");
    readout.className = "crps-readout";
    var plot = document.createElement("div");
    plot.className = "webr-slider-plot";
    var controls = document.createElement("div");
    controls.className = "webr-slider-controls";

    names.forEach(function (name) {
      var o = spec[name];
      var group = document.createElement("div");
      group.className = "webr-slider-input";
      group.innerHTML = '<label></label><div class="webr-slider-track"><input type="range">' +
        '<output></output><div class="webr-slider-ticks"></div></div>';
      group.querySelector("label").textContent = o.label;
      var input = inputs[name] = group.querySelector("input");
      outputs[name] = group.querySelector("output");
      input.min = 0; input.max = RES; input.step = 1;
      values[name] = o.value;
      input.value = Math.round((o.value - o.min) / (o.max - o.min) * RES);
      var ticks = group.querySelector(".webr-slider-ticks");
      o.ticks.forEach(function (t) {
        var tick = document.createElement("span");
        tick.textContent = String(t);
        tick.style.setProperty("--at", (t - o.min) / (o.max - o.min));
        ticks.append(tick);
      });
      controls.append(group);
    });
    el.append(readout, plot, controls);

    function draw() {
      var mu = values.mu, sigma = values.sigma;
      var score = crps(mu, sigma, Y);
      readout.innerHTML = 'CRPS = <strong>' + score.toFixed(2) + '</strong>';
      var xs = d3.range(-XLIM, XLIM + 1e-9, 0.02);
      var G = xs.map(function (x) { return { x: x, g: Phi((x - mu) / sigma), f: x < Y ? 0 : 1 }; });
      var ticksX = d3.range(-6, 7, 2), minorX = d3.range(-5, 6, 2);
      var ticksY = [0, 0.25, 0.5, 0.75, 1], minorY = [0.125, 0.375, 0.625, 0.875];
      var step = [{ x: -XLIM, f: 0 }, { x: Y, f: 0 }, { x: Y, f: 1 }, { x: XLIM, f: 1 }];
      var svg = Plot.plot({
        width: WIDTH, height: HEIGHT,
        marginLeft: 88, marginRight: 10, marginTop: 8, marginBottom: 70,
        style: { fontFamily: "'Fira Sans', sans-serif", fontSize: "17px", color: INK, background: "transparent", overflow: "visible" },
        x: { domain: [-XLIM, XLIM], ticks: ticksX, tickSize: 0, tickPadding: 6, label: "x", labelAnchor: "center", labelArrow: "none", labelOffset: 60 },
        y: { domain: [-0.03, 1.03], ticks: ticksY, tickSize: 0, tickPadding: 6, tickFormat: d3.format(".2f"), label: "Probability", labelAnchor: "center", labelArrow: "none", labelOffset: 78 },
        marks: [
          Plot.gridX(minorX, { stroke: MINOR, strokeOpacity: 1 }),
          Plot.gridY(minorY, { stroke: MINOR, strokeOpacity: 1 }),
          Plot.gridX(ticksX, { stroke: GRID, strokeOpacity: 1, strokeWidth: 1.5 }),
          Plot.gridY(ticksY, { stroke: GRID, strokeOpacity: 1, strokeWidth: 1.5 }),
          Plot.areaY(G, { x: "x", y1: "g", y2: "f", fill: BLUE, fillOpacity: 0.3 }),
          Plot.line(step, { x: "x", y: "f", stroke: ORANGE, strokeWidth: 2.5 }),
          Plot.line(G, { x: "x", y: "g", stroke: BLUE, strokeWidth: 2.5 }),
          Plot.text([{ x: Y, y: 0.5 }], { x: "x", y: "y", text: ["y"], dx: -12, fontSize: 20, fontWeight: 700, fill: ORANGE }),
          Plot.text([{ x: -XLIM + 0.15, y: 0.12 }], { x: "x", y: "y", text: ["F(x): observation"], textAnchor: "start", fontWeight: 600, fontSize: 18, fill: ORANGE }),
          Plot.text([{ x: XLIM - 0.15, y: 0.88 }], { x: "x", y: "y", text: ["G(x): forecast"], textAnchor: "end", fontWeight: 600, fontSize: 18, fill: BLUE })
        ]
      });
      plot.replaceChildren(svg);
    }
    function label() {
      names.forEach(function (name) {
        outputs[name].textContent = values[name].toFixed(1);
        inputs[name].parentNode.style.setProperty("--fill", inputs[name].value / RES);
      });
    }
    names.forEach(function (name) {
      var o = spec[name], input = inputs[name];
      input.addEventListener("input", function () {
        var v = o.min + Number(input.value) / RES * (o.max - o.min);
        v = Math.round((v - o.min) / o.step) * o.step + o.min;
        values[name] = Math.min(o.max, Math.max(o.min, +v.toFixed(10)));
        label();
        draw();
      });
      input.addEventListener("pointerup", function () { input.blur(); });
    });
    label();
    draw();
  }

  function init() { document.querySelectorAll(".crps-slider").forEach(setup); }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", init);
  else init();
})();
