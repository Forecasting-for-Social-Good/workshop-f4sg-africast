// Interactive slider plots made by webr_slider() in setup.R. Moving the slider
// recomputes the data with R running in the browser (webR), and the result is
// drawn with Observable Plot in the style of the deck's ggplot2 graphics.
(function () {
  var WEBR_URL = "https://webr.r-wasm.org/v0.6.0/webr.mjs";
  var INK = "#434A5C", GRID = "#D9E2C8", MINOR = "#E8EEDD";
  // The width of the deck's ggplot2 images (9in at 100% width), so text is the
  // same size: 8.8pt tick labels at ~140px per inch is ~17px
  var WIDTH = 1256, GAP = 10, STRIP = 38;

  // Start R once, shared by every slider in the deck
  var webR;
  function startR() {
    if (!webR) {
      webR = import(WEBR_URL).then(async function (mod) {
        var r = new mod.WebR();
        await r.init();
        return r;
      });
    }
    return webR;
  }

  function decimals(step) {
    var s = String(step);
    return s.includes(".") ? s.split(".")[1].length : 0;
  }

  // Map between a slider's value and the position of its <input>, which is
  // logarithmic for `log: true` sliders. Values snap to `min + k * step`.
  var RES = 1000;
  function scale(opts) {
    var lo = opts.log ? Math.log(opts.min) : opts.min;
    var hi = opts.log ? Math.log(opts.max) : opts.max;
    return {
      position: function (value) {
        return Math.round(((opts.log ? Math.log(value) : value) - lo) / (hi - lo) * RES);
      },
      value: function (position) {
        var v = lo + position / RES * (hi - lo);
        if (opts.log) v = Math.exp(v);
        v = opts.min + Math.round((v - opts.min) / opts.step) * opts.step;
        return Math.min(opts.max, Math.max(opts.min, v));
      }
    };
  }

  function fill(template, values) {
    if (template == null) return "";
    return template.replace(/\{(\w+)\}/g, function (m, name) {
      return name in values ? values[name] : m;
    });
  }

  // Major breaks and the minor breaks midway between them, as in ggplot2
  function breaks(scale, n) {
    var major = scale.ticks(n);
    var minor = [];
    var lo = scale.domain()[0], hi = scale.domain()[1];
    for (var i = -1; i < major.length; i++) {
      var a = i < 0 ? 2 * major[0] - major[1] : major[i];
      var b = i < 0 ? major[0] : (i + 1 < major.length ? major[i + 1] : 2 * major[i] - major[i - 1]);
      var mid = a instanceof Date ? new Date((+a + +b) / 2) : (a + b) / 2;
      if (mid >= lo && mid <= hi) minor.push(mid);
    }
    return { major: major, minor: minor };
  }

  // ggplot2's default 5% expansion of continuous scales
  function expand(extent) {
    var pad = (extent[1] - extent[0]) * 0.05 || 1;
    // Unary + so dates are numbers (Date + number would concatenate strings)
    return [+extent[0] - pad, +extent[1] + pad];
  }

  function panel(x, y, opts) {
    var xdom = expand(d3.extent(x)).map(function (d) { return new Date(d); });
    var ydom = expand(d3.extent(y));
    var xb = breaks(d3.scaleUtc().domain(xdom), 4);
    var yb = breaks(d3.scaleLinear().domain(ydom), opts.yticks);
    var ystep = d3.tickStep(ydom[0], ydom[1], opts.yticks);
    var points = y.map(function (v, i) { return [x[i], v]; });
    return Plot.plot({
      width: WIDTH,
      height: opts.height,
      marginLeft: 88,
      marginRight: opts.strip ? STRIP : 10,
      marginTop: 4,
      marginBottom: opts.xaxis ? 70 : 4 + GAP,
      style: {
        fontFamily: "'Fira Sans', sans-serif",
        fontSize: "17px",
        color: INK,
        background: "transparent",
        overflow: "visible"
      },
      x: {
        type: "utc", domain: xdom, ticks: xb.major, tickSize: 0, tickPadding: 6,
        tickFormat: d3.utcFormat("%Y %b"), axis: opts.xaxis ? "bottom" : null,
        label: opts.xaxis ? opts.xlab : null, labelAnchor: "center",
        labelArrow: "none", labelOffset: 60
      },
      y: {
        domain: ydom, ticks: yb.major, tickSize: 0, tickPadding: 6,
        tickFormat: d3.format("." + d3.precisionFixed(ystep) + "f"),
        label: opts.ylab, labelAnchor: "center", labelArrow: "none", labelOffset: 78
      },
      marks: [
        Plot.gridX(xb.minor, { stroke: MINOR, strokeOpacity: 1 }),
        Plot.gridY(yb.minor, { stroke: MINOR, strokeOpacity: 1 }),
        Plot.gridX(xb.major, { stroke: GRID, strokeOpacity: 1, strokeWidth: 1.5 }),
        Plot.gridY(yb.major, { stroke: GRID, strokeOpacity: 1, strokeWidth: 1.5 }),
        Plot.line(points, { stroke: "black", strokeWidth: 1.2 }),
        opts.strip ? Plot.text([opts.strip], {
          frameAnchor: "right", dx: STRIP / 2 + 2, rotate: 90,
          fontWeight: 700, fontSize: 15, fill: INK
        }) : null
      ]
    });
  }

  function setup(el) {
    var spec = JSON.parse(el.querySelector("script").textContent);
    var x = spec.x.map(function (d) { return new Date(d); });
    var names = Object.keys(spec.sliders);
    var fmt = {};
    names.forEach(function (name) {
      var digits = decimals(spec.sliders[name].step);
      fmt[name] = function (v) { return Number(v).toFixed(digits); };
    });

    var title = document.createElement("div");
    title.className = "webr-slider-title";
    var subtitle = document.createElement("div");
    subtitle.className = "webr-slider-subtitle";
    var plot = document.createElement("div");
    plot.className = "webr-slider-plot";
    var controls = document.createElement("div");
    controls.className = "webr-slider-controls";
    var inputs = {}, outputs = {}, values = {};
    names.forEach(function (name) {
      var opts = spec.sliders[name];
      var sc = scale(opts);
      var group = document.createElement("div");
      group.className = "webr-slider-input";
      group.innerHTML =
        '<label></label><div class="webr-slider-track"><input type="range">' +
        '<output></output><div class="webr-slider-ticks"></div></div>';
      group.querySelector("label").textContent = opts.label;
      var input = inputs[name] = group.querySelector("input");
      outputs[name] = group.querySelector("output");
      input.min = 0;
      input.max = RES;
      input.step = 1;
      values[name] = opts.value;
      input.value = sc.position(opts.value);
      input.webrValue = function () { return sc.value(Number(input.value)); };
      // Tick marks, centred under where the thumb sits at that value
      var ticks = group.querySelector(".webr-slider-ticks");
      (opts.ticks || []).forEach(function (t) {
        var tick = document.createElement("span");
        tick.textContent = String(+t.toFixed(10));
        tick.style.setProperty("--at", sc.position(t) / RES);
        ticks.append(tick);
      });
      controls.append(group);
    });
    var status = document.createElement("span");
    status.className = "webr-slider-status";
    controls.append(status);
    el.append(title, subtitle, plot, controls);

    // Current slider values, as numbers and formatted for display
    function current() {
      var labels = {};
      names.forEach(function (name) { labels[name] = fmt[name](values[name]); });
      return { values: Object.assign({}, values), labels: labels };
    }

    function draw(cols, labels) {
      title.textContent = fill(spec.title, labels);
      subtitle.textContent = fill(spec.subtitle, labels);
      subtitle.hidden = !spec.subtitle;
      var panels = Object.keys(cols);
      var multi = panels.length > 1;
      var inner = (spec.height - 66 - (panels.length - 1) * GAP) / panels.length - 8;
      plot.replaceChildren.apply(plot, panels.map(function (name, i) {
        var last = i === panels.length - 1;
        return panel(x, cols[name], {
          height: inner + 8 + (last ? 66 : GAP),
          xaxis: last,
          xlab: spec.xlab,
          ylab: multi ? null : name,
          strip: multi ? name : null,
          yticks: multi ? 3 : 5
        });
      }));
    }
    // Show each slider's value above its thumb, and fill the track up to it
    function label() {
      var labels = current().labels;
      names.forEach(function (name) {
        outputs[name].textContent = labels[name];
        inputs[name].parentNode.style.setProperty("--fill", inputs[name].value / RES);
      });
      return labels;
    }
    draw(spec.init, label());

    // Recompute in R, dropping intermediate values while R is busy
    var id = ".webr_slider_" + Math.random().toString(36).slice(2);
    var ready = startR().then(function (r) {
      return r.evalRVoid("assign(id, " + spec.fn + ", envir = globalenv())", { env: { id: id } })
        .then(function () { return r; });
    });
    var pending = null, running = false;
    status.textContent = "Starting R…";
    ready.then(function () { status.textContent = ""; }, function (e) {
      status.textContent = "R failed to start";
      console.error(e);
    });

    async function run() {
      if (running) return;
      running = true;
      var r = await ready;
      while (pending !== null) {
        var now = pending;
        pending = null;
        var res = await r.evalR(
          "do.call(get(id), c(list(as.data.frame(data)), setNames(as.list(values), names)))",
          { env: { id: id, data: spec.data, names: names, values: names.map(function (name) {
            return now.values[name];
          }) } }
        );
        var js = await res.toJs();
        r.destroy(res);
        var cols = {};
        js.names.forEach(function (name, i) { cols[name] = js.values[i].values; });
        draw(cols, now.labels);
      }
      running = false;
    }

    names.forEach(function (name) {
      var input = inputs[name];
      input.addEventListener("input", function () {
        var value = input.webrValue();
        var changed = value !== values[name];
        values[name] = value;
        label();
        if (!changed) return;
        pending = current();
        run().catch(function (e) {
          running = false;
          status.textContent = "R error";
          console.error(e);
        });
      });
      // Return the arrow keys to reveal.js for slide navigation after dragging
      input.addEventListener("pointerup", function () { input.blur(); });
    });
  }

  function init() {
    var sliders = document.querySelectorAll(".webr-slider");
    if (!sliders.length) return;
    sliders.forEach(setup);
    startR();
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", init);
  else init();
})();
