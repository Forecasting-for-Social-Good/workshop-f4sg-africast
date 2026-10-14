// Engine for staged plot animations created by morph_widget() in morph.R.
//
// A scene is registered with PlotMorph.register(name, factory). The factory
// receives the <svg> element, the data exported from R, and helpers, and
// returns a render(progress) function. Progress is a number from 0 to
// (steps - 1), where whole numbers are the storyboard steps. The engine moves
// progress towards the active step, which is set by the revealjs fragments (.fragment.morph-step) on the slide.
(function () {
  if (window.PlotMorph) return;

  const SVGNS = "http://www.w3.org/2000/svg";
  const scenes = {};
  const reduceMotion = window.matchMedia("(prefers-reduced-motion: reduce)");

  const helpers = {
    SVGNS,
    el(name, attrs = {}, parent = null) {
      const node = document.createElementNS(SVGNS, name);
      for (const [k, v] of Object.entries(attrs)) node.setAttribute(k, v);
      if (parent) parent.appendChild(node);
      return node;
    },
    // Parse an SVG string from R into a nested <svg> element
    svg(markup, attrs = {}, parent = null) {
      const doc = new DOMParser().parseFromString(markup, "image/svg+xml");
      const node = document.importNode(doc.documentElement, true);
      for (const [k, v] of Object.entries(attrs)) node.setAttribute(k, v);
      if (parent) parent.appendChild(node);
      return node;
    },
    clamp: (x, lo = 0, hi = 1) => Math.min(hi, Math.max(lo, x)),
    lerp: (a, b, t) => a + (b - a) * t,
    ease: (t) => (t < 0.5 ? 4 * t * t * t : 1 - Math.pow(-2 * t + 2, 3) / 2),
    // Local progress of a phase spanning [from, to] of the overall progress
    phase(p, from, to) {
      return helpers.clamp((p - from) / (to - from));
    },
    lerpBox(a, b, t) {
      return {
        x: helpers.lerp(a.x, b.x, t),
        y: helpers.lerp(a.y, b.y, t),
        w: helpers.lerp(a.w, b.w, t),
        h: helpers.lerp(a.h, b.h, t),
      };
    },
    lerpColour(a, b, t) {
      const pa = helpers.parseHex(a);
      const pb = helpers.parseHex(b);
      const c = pa.map((v, i) => Math.round(helpers.lerp(v, pb[i], t)));
      return "#" + c.map((v) => v.toString(16).padStart(2, "0")).join("");
    },
    parseHex(hex) {
      const h = hex.replace("#", "");
      return [0, 2, 4].map((i) => parseInt(h.slice(i, i + 2), 16));
    },
  };

  function init(figure) {
    if (figure.dataset.morphReady) return;
    const factory = scenes[figure.dataset.scene];
    if (!factory) return;
    figure.dataset.morphReady = "true";
    figure.tabIndex = 0;

    const data = JSON.parse(
      document.getElementById(figure.id + "-data").textContent
    );
    const nSteps = Number(figure.dataset.steps);
    // The slide holding this figure; its fragments are the steps
    const slide = figure.closest("section");
    const stepEls = slide ? Array.from(slide.querySelectorAll(".morph-step")) : [];

    const svg = helpers.el("svg", {
      viewBox: `0 0 ${data.width} ${data.height}`,
      class: "morph-svg",
    });
    figure.appendChild(svg);
    const render = factory(svg, data, helpers);

    let progress = 0;
    let target = 0;
    let last = null;
    let frame = null;
    const stepDuration = 1100; // default ms per step
    const durations = (figure.dataset.durations || "")
      .split(",")
      .filter(Boolean)
      .map(Number);
    // Duration of the step being animated through, in the given direction
    function duration(direction) {
      const seg = direction > 0 ? Math.floor(progress) : Math.ceil(progress) - 1;
      return durations[helpers.clamp(seg, 0, nSteps - 2)] || stepDuration;
    }

    function tick(now) {
      const dt = last === null ? 16 : now - last;
      last = now;
      const delta = target - progress;
      const stepSize = reduceMotion.matches ? Infinity : dt / duration(Math.sign(delta));
      progress =
        Math.abs(delta) <= stepSize ? target : progress + Math.sign(delta) * stepSize;
      render(progress);
      if (progress !== target) {
        frame = requestAnimationFrame(tick);
      } else {
        frame = null;
        last = null;
      }
    }

    function goTo(step, { jump = false } = {}) {
      target = helpers.clamp(step, 0, nSteps - 1);
      stepEls.forEach((el, i) => el.classList.toggle("is-active", i === target));
      if (jump || reduceMotion.matches) {
        progress = target;
        render(progress);
        return;
      }
      if (frame === null) frame = requestAnimationFrame(tick);
    }

    // The current step is the number of this slide's step fragments shown
    function sync(jump) {
      const shown = slide
        ? slide.querySelectorAll(".fragment.morph-step.visible").length
        : 0;
      goTo(shown, { jump });
    }
    figure.morphSync = sync;

    // Handle for scripting and testing the animation
    figure.plotMorph = { goTo, render };

    render(0);
    sync(true);
  }

  // Step the figures on the current slide as fragments are shown and hidden
  function figuresOf(event) {
    const slide = event && event.fragment ? event.fragment.closest("section") : null;
    return slide ? slide.querySelectorAll(".morph-figure") : [];
  }
  function wireReveal() {
    if (typeof Reveal === "undefined" || window.PlotMorphWired) return;
    window.PlotMorphWired = true;
    const step = (e) => figuresOf(e).forEach((f) => f.morphSync && f.morphSync(false));
    Reveal.on("fragmentshown", step);
    Reveal.on("fragmenthidden", step);
    // Arriving on a slide (forwards or back) shows its last-seen step at once
    Reveal.on("slidechanged", (e) =>
      e.currentSlide.querySelectorAll(".morph-figure").forEach((f) => f.morphSync && f.morphSync(true))
    );
    const all = () => document.querySelectorAll(".morph-figure").forEach((f) => f.morphSync && f.morphSync(true));
    Reveal.on("ready", all);
    if (Reveal.isReady && Reveal.isReady()) all();
  }
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", wireReveal);
  } else {
    wireReveal();
  }

  window.PlotMorph = {
    helpers,
    register(name, factory) {
      scenes[name] = factory;
      // Wait for the steps, which follow the inline scripts in the page
      const initAll = () => document.querySelectorAll(".morph-figure").forEach(init);
      if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", initAll);
      } else {
        initAll();
      }
    },
  };
})();
