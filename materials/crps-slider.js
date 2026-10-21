// Interactive CRPS explainers for a Normal forecast of an observation y,
// with sliders for the forecast mean and standard deviation:
//
//   <div class="crps-slider">                       CDF G, step F at y, and the area between
//   <div class="crps-slider" data-score="density">  density, expected error and spread
//
// Uses the closed form of the CRPS of a Normal, and the deck's slider styles
// (.webr-slider-* in theme.scss).
(function () {
  var INK = "#434A5C", NAVY = "#1B2A2C", GRID = "#D9E2C8", MINOR = "#E8EEDD";
  var BLUE = "#2B6CB0", ORANGE = "#E07B39";
  var WIDTH = 1256, HEIGHT = 340, XMIN = -2, XMAX = 14;
  var Y = 4;
  var SPEC = {
    mu: { label: "Forecast mean (μ)", value: 6, min: 0, max: 10, step: 0.1, ticks: [0, 2, 4, 6, 8, 10] },
    sigma: { label: "Forecast sd (σ)", value: 1.5, min: 0.1, max: 3, step: 0.1, ticks: [0.5, 1, 1.5, 2, 2.5, 3] }
  };

  function erf(x) {
    // Series for small |x|, continued fraction for erfc otherwise
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

  // Shared frame: gridlines, axes and labels around the given marks
  function frame(yLabel, marks) {
    var ticksX = d3.range(XMIN, XMAX + 1, 2), minorX = d3.range(XMIN + 1, XMAX, 2);
    var ticksY = [0, 0.25, 0.5, 0.75, 1], minorY = [0.125, 0.375, 0.625, 0.875];
    return Plot.plot({
      width: WIDTH, height: HEIGHT,
      marginLeft: 88, marginRight: 10, marginTop: 8, marginBottom: 70,
      style: { fontFamily: "'Fira Sans', sans-serif", fontSize: "17px", color: INK, background: "transparent", overflow: "visible" },
      x: { domain: [XMIN, XMAX], ticks: ticksX, tickSize: 0, tickPadding: 6, label: "x", labelAnchor: "center", labelArrow: "none", labelOffset: 60 },
      y: { domain: [-0.03, 1.03], ticks: ticksY, tickSize: 0, tickPadding: 6, tickFormat: d3.format(".2f"), label: yLabel, labelAnchor: "center", labelArrow: "none", labelOffset: 78 },
      marks: [
        Plot.gridX(minorX, { stroke: MINOR, strokeOpacity: 1 }),
        Plot.gridY(minorY, { stroke: MINOR, strokeOpacity: 1 }),
        Plot.gridX(ticksX, { stroke: GRID, strokeOpacity: 1, strokeWidth: 1.5 }),
        Plot.gridY(ticksY, { stroke: GRID, strokeOpacity: 1, strokeWidth: 1.5 })
      ].concat(marks)
    });
  }
  function label(x, y, text, color, options) {
    return Plot.text([{ x: x, y: y }], Object.assign({ x: "x", y: "y", text: [text], fontWeight: 600, fontSize: 18, fill: color, stroke: "white", strokeWidth: 5, paintOrder: "stroke" }, options));
  }

  var SCORES = {
    crps: function (mu, sigma) {
      var G = d3.range(XMIN, XMAX + 1e-9, 0.02).map(function (x) {
        return { x: x, g: Phi((x - mu) / sigma), f: x < Y ? 0 : 1 };
      });
      var step = [{ x: XMIN, f: 0 }, { x: Y, f: 0 }, { x: Y, f: 1 }, { x: XMAX, f: 1 }];
      return {
        score: "CRPS = <strong>" + crps(mu, sigma, Y).toFixed(2) + "</strong>",
        plot: frame("Probability", [
          Plot.areaY(G, { x: "x", y1: "g", y2: "f", fill: BLUE, fillOpacity: 0.3 }),
          Plot.line(step, { x: "x", y: "f", stroke: ORANGE, strokeWidth: 2.5 }),
          Plot.line(G, { x: "x", y: "g", stroke: BLUE, strokeWidth: 2.5 }),
          label(Y, 0.5, "y", ORANGE, { dx: -12, fontSize: 20, fontWeight: 700 }),
          label(XMIN + 0.15, 0.12, "F(x): observation", ORANGE, { textAnchor: "start" }),
          label(XMAX - 0.15, 0.88, "G(x): forecast", BLUE, { textAnchor: "end" })
        ])
      };
    },
    // CRPS = E|X - y| - E|X - X'| / 2, the forecast's expected error less half its spread
    density: function (mu, sigma) {
      var z = (Y - mu) / sigma, top = 1, bar1 = 0.88, bar2 = 0.68;
      var err = sigma * (z * (2 * Phi(z) - 1) + 2 * phi(z)), spread = sigma / Math.sqrt(Math.PI);
      var g = d3.range(XMIN, XMAX + 1e-9, 0.01).map(function (x) { return { x: x, d: phi((x - mu) / sigma) / sigma }; });
      var right = mu >= Y, end = right ? Y + err : Y - err;
      return {
        score: "CRPS = " + err.toFixed(2) + " − " + spread.toFixed(2) + " = <strong>" + (err - spread).toFixed(2) + "</strong>",
        plot: frame("Density", [
          Plot.areaY(g, { x: "x", y: "d", fill: BLUE, fillOpacity: 0.15, clip: true }),
          Plot.line(g, { x: "x", y: "d", stroke: BLUE, strokeWidth: 2.5, clip: true }),
          Plot.ruleX([mu], { y1: 0, y2: top, stroke: BLUE, strokeWidth: 2.5, strokeDasharray: "6,4" }),
          Plot.ruleX([Y], { y1: 0, y2: top, stroke: ORANGE, strokeWidth: 2.5 }),
          Plot.link([{ x1: Y, x2: end }], { x1: "x1", x2: "x2", y1: bar1, y2: bar1, stroke: NAVY, strokeWidth: 3, markerStart: "tick", markerEnd: "tick" }),
          label((Y + end) / 2, bar1, "E|X − y| = " + err.toFixed(2), NAVY, { dy: 20 }),
          Plot.link([{ x1: mu - spread / 2, x2: mu + spread / 2 }], { x1: "x1", x2: "x2", y1: bar2, y2: bar2, stroke: BLUE, strokeWidth: 3, markerStart: "tick", markerEnd: "tick" }),
          label(mu, bar2, "½E|X − X′| = " + spread.toFixed(2), BLUE, { dy: 20 }),
          label(Y, top, "y", ORANGE, { dx: right ? -12 : 12, dy: 10, fontSize: 20, fontWeight: 700 }),
          label(mu, top, "μ", BLUE, { dx: right ? 12 : -12, dy: 10, fontSize: 20, fontWeight: 700 })
        ])
      };
    }
  };

  function setup(el) {
    var score = SCORES[el.dataset.score || "crps"];
    var names = Object.keys(SPEC);
    var values = {}, inputs = {}, outputs = {};

    var readout = document.createElement("div");
    readout.className = "crps-readout";
    var plot = document.createElement("div");
    plot.className = "webr-slider-plot";
    var controls = document.createElement("div");
    controls.className = "webr-slider-controls";

    names.forEach(function (name) {
      var o = SPEC[name];
      var group = document.createElement("div");
      group.className = "webr-slider-input";
      group.innerHTML = '<label></label><div class="webr-slider-track"><input type="range">' +
        '<output></output><div class="webr-slider-ticks"></div></div>';
      group.querySelector("label").textContent = o.label;
      var input = inputs[name] = group.querySelector("input");
      outputs[name] = group.querySelector("output");
      input.min = o.min; input.max = o.max; input.step = o.step; input.value = o.value;
      values[name] = o.value;
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

    function update() {
      names.forEach(function (name) {
        var o = SPEC[name];
        outputs[name].textContent = values[name].toFixed(1);
        inputs[name].parentNode.style.setProperty("--fill", (values[name] - o.min) / (o.max - o.min));
      });
      var out = score(values.mu, values.sigma);
      readout.innerHTML = out.score;
      plot.replaceChildren(out.plot);
    }
    names.forEach(function (name) {
      var input = inputs[name];
      input.addEventListener("input", function () {
        values[name] = Number(input.value);
        update();
      });
      // Return the arrow keys to reveal.js once dragging ends
      input.addEventListener("pointerup", function () { input.blur(); });
    });
    update();
  }

  function init() { document.querySelectorAll(".crps-slider").forEach(setup); }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", init);
  else init();
})();
