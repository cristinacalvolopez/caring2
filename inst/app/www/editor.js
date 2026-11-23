// CARING 2 · editor de informes y diapositivas
// Lienzo libre (arrastrar y redimensionar) o por bloques (cuadrícula de 12 columnas).
// El documento vive aquí y se envía a R (guardado) con cada cambio.
(function () {
  "use strict";

  var SWATCHES = ["#000000", "#FFFFFF", "#2F7BEE", "#135DCD", "#CBDEFB", "#6B6B6B", "#F5F5F5", "#B3261E", "#1E6B34", "#FF7F0E"];
  var TEMPLATES_SLIDES = [["portada", "Portada"], ["contenido", "Título y contenido"], ["dos_columnas", "Dos columnas"], ["seccion", "Sección"], ["blanca", "En blanco"]];
  var TEMPLATES_REPORT = [["portada", "Portada"], ["apartado", "Apartado"], ["blanca", "En blanco"]];

  var E = null; // estado del editor activo

  function uid() { return Math.random().toString(16).slice(2, 12); }
  function clone(o) { return JSON.parse(JSON.stringify(o)); }
  function h(tag, attrs, children) {
    var n = document.createElement(tag);
    if (attrs) Object.keys(attrs).forEach(function (k) {
      if (k === "style") n.style.cssText = attrs[k];
      else if (k.slice(0, 2) === "on") n.addEventListener(k.slice(2), attrs[k]);
      else if (k === "html") n.innerHTML = attrs[k];
      else if (k === "text") n.textContent = attrs[k];
      else n.setAttribute(k, attrs[k]);
    });
    (children || []).forEach(function (c) { if (c) n.appendChild(typeof c === "string" ? document.createTextNode(c) : c); });
    return n;
  }

  function size() { return E.doc.type === "report" ? { w: 595, h: 842 } : { w: 960, h: 540 }; }
  function page() { return E.doc.pages[E.page]; }
  function selected() {
    if (!E.sel) return null;
    var els = page().elements;
    for (var i = 0; i < els.length; i++) if (els[i].id === E.sel) return els[i];
    return null;
  }

  // Guardado e historial --------------------------------------------------------------
  function commit(skipHistory) {
    if (!skipHistory) {
      E.undo.push(E.last);
      if (E.undo.length > 60) E.undo.shift();
      E.redo = [];
    }
    E.last = JSON.stringify(E.doc);
    clearTimeout(E.saveTimer);
    setStatus("Guardando…");
    E.saveTimer = setTimeout(function () {
      Shiny.setInputValue(E.inputId, E.last, { priority: "event" });
      setStatus("Guardado");
    }, 500);
  }
  function undo() {
    if (!E.undo.length) return;
    E.redo.push(JSON.stringify(E.doc));
    E.doc = JSON.parse(E.undo.pop());
    E.page = Math.min(E.page, E.doc.pages.length - 1);
    E.sel = null;
    commit(true); render();
  }
  function redo() {
    if (!E.redo.length) return;
    E.undo.push(JSON.stringify(E.doc));
    E.doc = JSON.parse(E.redo.pop());
    E.sel = null;
    commit(true); render();
  }
  function setStatus(t) { var s = E.root.querySelector(".ed-status"); if (s) s.textContent = t; }

  // Modo por bloques --------------------------------------------------------------------
  function grid() {
    var s = size(), margin = E.doc.type === "report" ? 50 : 40, gutter = 16;
    var col = (s.w - 2 * margin - 11 * gutter) / 12;
    return { margin: margin, gutter: gutter, col: col, row: 10 };
  }
  function contentTop(p) {
    var top = grid().margin, s = size();
    p.elements.forEach(function (e) {
      if (e.kind !== "shape") return;
      var decor = (e.role === "band" || e.role === "rule") && e.y < s.h * 0.3;
      var legacy = e.y <= 0 && e.w >= s.w - 1 && e.h < s.h * 0.5;
      if (decor || legacy) top = Math.max(top, e.y + e.h + 20);
    });
    return top;
  }
  // Las formas y lo que está encima de una forma (p. ej. el título sobre la franja) no se recolocan.
  function insideShape(e) {
    return page().elements.some(function (s) {
      return s.kind === "shape" && s !== e && e.x >= s.x - 1 && e.y >= s.y - 1 &&
        e.x + e.w <= s.x + s.w + 1 && e.y + e.h <= s.y + s.h + 1;
    });
  }
  function isLayoutEl(e) { return e.kind !== "shape" && !e.pinned && !insideShape(e); }
  function snapEl(e) {
    var g = grid();
    var span = Math.max(1, Math.min(12, Math.round((e.w + g.gutter) / (g.col + g.gutter))));
    var c0 = Math.max(0, Math.min(12 - span, Math.round((e.x - g.margin) / (g.col + g.gutter))));
    e.x = Math.round(g.margin + c0 * (g.col + g.gutter));
    e.w = Math.round(span * g.col + (span - 1) * g.gutter);
    e.h = Math.max(g.row * 3, Math.round(e.h / g.row) * g.row);
    e.y = Math.round(e.y / g.row) * g.row;
  }
  function overlap(a, b) { return a.x < b.x + b.w - 1 && b.x < a.x + a.w - 1 && a.y < b.y + b.h - 1 && b.y < a.y + a.h - 1; }
  // Coloca los bloques sin solaparse y los sube todo lo posible (gravedad hacia arriba).
  function pack(p, moved) {
    var top = contentTop(p), g = grid();
    var els = p.elements.filter(isLayoutEl);
    els.forEach(snapEl);
    els.sort(function (a, b) {
      if (moved && a.id === moved) return a.y - b.y || -1;
      if (moved && b.id === moved) return a.y - b.y || 1;
      return a.y - b.y || a.x - b.x;
    });
    var place = function () {
      var placed = [];
      els.forEach(function (e) {
        e.y = top;
        var guard = 0;
        while (placed.some(function (o) { return overlap(e, o); }) && guard++ < 500) e.y += g.row;
        placed.push(e);
      });
      return placed.reduce(function (m, e) { return Math.max(m, e.y + e.h); }, top);
    };
    // Si no cabe en la página, se reducen las alturas para que todo quede dentro.
    var limit = size().h - g.margin, bottom = place(), tries = 0;
    while (bottom > limit && tries++ < 8) {
      var f = Math.max(0.3, (limit - top) / (bottom - top));
      els.forEach(function (e) { e.h = Math.max(g.row * 3, Math.floor(e.h * f / g.row) * g.row); });
      bottom = place();
    }
  }
  function packAll() {
    var cur = E.page;
    E.doc.pages.forEach(function (p, i) { E.page = i; pack(p); });
    E.page = cur;
  }
  function freeSlot(w, h) {
    var p = page();
    if (E.doc.mode === "blocks") {
      var bottom = contentTop(p);
      p.elements.filter(isLayoutEl).forEach(function (e) { bottom = Math.max(bottom, e.y + e.h + grid().gutter); });
      var room = size().h - grid().margin - bottom;
      return { x: grid().margin, y: bottom, w: size().w - 2 * grid().margin, h: Math.max(grid().row * 4, Math.min(h, room)) };
    }
    // Busca un hueco libre debajo del contenido; si no cabe, reduce el tamaño y prueba otra vez.
    var s = size(), m = 30, top = contentTop(p);
    var busy = p.elements.filter(isLayoutEl);
    for (var scale = 1; scale >= 0.4; scale -= 0.15) {
      var ww = Math.round(w * scale), hh = Math.round(h * scale);
      for (var y = top; y + hh <= s.h - m; y += 10) {
        for (var x = m; x + ww <= s.w - m; x += 10) {
          var c = { x: x, y: y, w: ww, h: hh };
          if (!busy.some(function (o) { return overlap(c, o); })) return c;
        }
      }
    }
    var off = (p.elements.length % 6) * 14;
    return { x: Math.round((s.w - w) / 2) + off, y: Math.round((s.h - h) / 2) + off, w: w, h: h };
  }

  // Añadir elementos -----------------------------------------------------------------------
  function addElement(e) {
    page().elements.push(e);
    if (E.doc.mode === "blocks") pack(page(), e.id);
    E.sel = e.id;
    commit(); render();
  }
  function addText(kind) {
    var title = kind === "title", t = E.theme;
    var slot = freeSlot(title ? 700 : 420, title ? 60 : 120);
    addElement({ id: uid(), kind: "text", role: title ? "heading" : "body", x: slot.x, y: slot.y, w: slot.w, h: slot.h,
      text: title ? "Título" : "Escribe aquí tu texto",
      style: { size: title ? (E.doc.type === "report" ? 20 : 32) : (E.doc.type === "report" ? 12 : 18), bold: title,
               color: title ? t.heading : t.text, align: "left" } });
  }
  // Formas: [clave, nombre]; las poligonales usan clip-path (los mismos vértices que en R)
  var SHAPES = [["rect", "Rectángulo"], ["elipse", "Círculo o elipse"], ["triangulo", "Triángulo"], ["rombo", "Rombo"],
                ["hexagono", "Hexágono"], ["estrella", "Estrella"], ["flecha", "Flecha"]];
  var SHAPE_POINTS = {
    triangulo: [0.5, 0, 1, 1, 0, 1], rombo: [0.5, 0, 1, 0.5, 0.5, 1, 0, 0.5],
    hexagono: [0.25, 0, 0.75, 0, 1, 0.5, 0.75, 1, 0.25, 1, 0, 0.5],
    estrella: [0.5, 0, 0.61, 0.35, 0.98, 0.35, 0.68, 0.57, 0.79, 0.91, 0.5, 0.7, 0.21, 0.91, 0.32, 0.57, 0.02, 0.35, 0.39, 0.35],
    flecha: [0, 0.3, 0.65, 0.3, 0.65, 0, 1, 0.5, 0.65, 1, 0.65, 0.7, 0, 0.7]
  };
  function shapeCss(shape, radius) {
    shape = shape || "rect";
    if (shape === "elipse") return "border-radius:50%";
    var p = SHAPE_POINTS[shape];
    if (!p) return "border-radius:" + (radius || 0) + "px";
    var pts = [];
    for (var i = 0; i < p.length; i += 2) pts.push((p[i] * 100) + "% " + (p[i + 1] * 100) + "%");
    return "clip-path:polygon(" + pts.join(",") + ")";
  }
  function addShape(shape) {
    var cuadrada = shape && shape !== "rect" && shape !== "flecha";
    var slot = freeSlot(cuadrada ? 160 : 240, cuadrada ? 160 : 140);
    var st = { bg: E.theme.accent || "#2F7BEE", radius: Math.min(E.theme.radius || 0, 20) };
    if (shape && shape !== "rect") st.shape = shape;
    addElement({ id: uid(), kind: "shape", x: slot.x, y: slot.y, w: slot.w, h: slot.h, style: st });
  }
  function addResult(id) {
    var r = E.assets.results[id];
    if (!r) return;
    var s = size(), w = r.kind === "tabla" ? s.w * 0.55 : s.w * 0.62, h = r.kind === "tabla" ? s.h * 0.4 : s.h * 0.58;
    var slot = freeSlot(Math.round(w), Math.round(h));
    var st = E.theme.resultBg ? { bg: E.theme.resultBg, radius: E.theme.radius || 0 } : undefined;
    addElement({ id: uid(), kind: "result", role: "result", ref: id, x: slot.x, y: slot.y, w: slot.w, h: slot.h, style: st });
  }
  function addImage(src) {
    var slot = freeSlot(320, 220);
    addElement({ id: uid(), kind: "image", src: src, x: slot.x, y: slot.y, w: slot.w, h: slot.h });
  }

  // Páginas ---------------------------------------------------------------------------------
  function addPage(tpl) {
    var t = E.templates[tpl] || { elements: [] };
    var p = clone(t);
    p.id = uid();
    p.elements.forEach(function (e) { e.id = uid(); });
    E.doc.pages.splice(E.page + 1, 0, p);
    E.page += 1; E.sel = null;
    if (E.doc.mode === "blocks") pack(p);
    commit(); render();
  }
  function deletePage(i) {
    if (E.doc.pages.length <= 1) return;
    E.doc.pages.splice(i, 1);
    E.page = Math.min(E.page, E.doc.pages.length - 1); E.sel = null;
    commit(); render();
  }
  function movePage(i, d) {
    var j = i + d;
    if (j < 0 || j >= E.doc.pages.length) return;
    var p = E.doc.pages.splice(i, 1)[0];
    E.doc.pages.splice(j, 0, p);
    E.page = j; commit(); render();
  }
  function duplicatePage(i) {
    var p = clone(E.doc.pages[i]);
    p.id = uid(); p.elements.forEach(function (e) { e.id = uid(); });
    E.doc.pages.splice(i + 1, 0, p);
    E.page = i + 1; commit(); render();
  }

  // Dibujo ------------------------------------------------------------------------------------
  function elementNode(e, interactive) {
    var n = h("div", { "class": "ed-el ed-" + e.kind + (interactive && e.id === E.sel ? " sel" : ""),
      style: "left:" + e.x + "px;top:" + e.y + "px;width:" + e.w + "px;height:" + e.h + "px;" });
    n.dataset.id = e.id;
    if (e.kind === "text") {
      var s = e.style || {};
      var t = h("div", { "class": "ed-text", text: e.runs ? "" : (e.text || ""),
        style: "font-size:" + (s.size || 18) + "px;font-weight:" + (s.bold ? 700 : 400) + ";font-style:" + (s.italic ? "italic" : "normal") +
          ";color:" + (s.color || "#000") + ";text-align:" + (s.align || "left") + ";" + (s.bg ? "background:" + s.bg + ";" : "") });
      fillRuns(t, e);
      n.appendChild(t);
    } else if (e.kind === "shape") {
      n.style.background = (e.style && e.style.bg) || "#2F7BEE";
      n.style.cssText += ";" + shapeCss(e.style && e.style.shape, e.style && e.style.radius);
    } else if (e.kind === "image") {
      var src = E.assets.images[e.src];
      n.appendChild(src ? h("img", { src: src, alt: "", draggable: "false" }) : h("div", { "class": "ed-missing", text: "Imagen no disponible" }));
    } else if (e.kind === "result") {
      if (e.style && e.style.bg) {
        n.style.background = e.style.bg; n.style.borderRadius = (e.style.radius || 0) + "px"; n.style.padding = "8px";
      }
      var r = E.assets.results[e.ref];
      if (!r) n.appendChild(h("div", { "class": "ed-missing", text: "Resultado borrado" }));
      else if (r.kind === "tabla" && r.html) {
        var tp = E.palettes && E.palettes[(e.style && e.style.palette) || "netcoin"];
        n.appendChild(h("div", { "class": "ed-table", html: r.html,
          style: tp ? "--th:" + tp.head + ";--thc:" + tp.head_text : "" }));
      }
      else if (interactive && r.fig && window.Plotly) {
        // Gráfico interactivo (en las descargas se usa la imagen)
        var pd = h("div", { "class": "ed-plotly", role: "img", "aria-label": r.title });
        n.appendChild(pd);
        var pal = e.style && e.style.palette;
        E.plots.push({ node: pd, fig: (pal && r.figs && r.figs[pal]) || r.fig, w: e.w - (e.style && e.style.bg ? 16 : 0), h: e.h - (e.style && e.style.bg ? 16 : 0) });
      }
      else if (r.png) {
        var pv = e.style && e.style.palette;
        n.appendChild(h("img", { src: (pv && r.pngs && r.pngs[pv]) || r.png, alt: r.title, draggable: "false" }));
      }
      else n.appendChild(h("div", { "class": "ed-missing", text: r.title }));
      if (interactive && r && r.interactive) n.appendChild(h("span", { "class": "ed-badge", text: "Red interactiva" }));
    }
    if (interactive && e.id === E.sel) n.appendChild(h("span", { "class": "ed-handle", "aria-hidden": "true" }));
    return n;
  }

  // Subrayador: el texto se guarda como trozos { t: texto, m: color de fondo o null } en e.runs
  // (y también como texto plano en e.text, para todo lo demás).
  var MARKS = [["#FFF176", "Amarillo"], ["#A5D6A7", "Verde"], ["#90CAF9", "Azul"], ["#F48FB1", "Rosa"], ["#FFCC80", "Naranja"]];
  function fillRuns(t, e) {
    if (!e.runs || !e.runs.length) return;
    e.runs.forEach(function (r) {
      var sp = document.createElement("span");
      sp.textContent = r.t;
      if (r.m) { sp.style.backgroundColor = r.m; sp.className = "ed-mark"; }
      t.appendChild(sp);
    });
  }
  function toHex(c) {
    var m = /rgba?\((\d+),\s*(\d+),\s*(\d+)(?:,\s*([\d.]+))?\)/.exec(c || "");
    if (!m) return /^#[0-9a-f]{6}$/i.test(c || "") ? c.toUpperCase() : null;
    if (m[4] !== undefined && +m[4] === 0) return null;
    return "#" + [m[1], m[2], m[3]].map(function (x) { return ("0" + (+x).toString(16)).slice(-2); }).join("").toUpperCase();
  }
  function readRuns(t) {
    var out = [];
    function push(txt, m) {
      if (!txt) return;
      var last = out[out.length - 1];
      if (last && last.m === m) last.t += txt; else out.push({ t: txt, m: m });
    }
    function walk(node, mark) {
      node.childNodes.forEach(function (c) {
        if (c.nodeType === 3) { push(c.nodeValue, mark); return; }
        if (c.nodeType !== 1) return;
        if (c.tagName === "BR") { push("\n", mark); return; }
        var m = mark, bg = c.style && c.style.backgroundColor;
        if (bg) m = toHex(bg);
        var block = /^(DIV|P)$/.test(c.tagName);
        if (block && out.length && !/\n$/.test(out[out.length - 1].t)) push("\n", mark);
        walk(c, m);
      });
    }
    walk(t, null);
    if (out.length) out[out.length - 1].t = out[out.length - 1].t.replace(/\n$/, "");
    return out;
  }
  function markBar(t) {
    var bar = h("div", { "class": "ed-markbar", role: "toolbar", "aria-label": "Subrayador" }, [h("span", { text: "Subrayar:" })]);
    MARKS.forEach(function (mk) {
      bar.appendChild(h("button", { type: "button", "class": "ed-mk", title: mk[1], "aria-label": "Subrayar en " + mk[1].toLowerCase(), style: "background:" + mk[0],
        onmousedown: function (ev) { ev.preventDefault(); }, onclick: function () { highlight(mk[0]); } }));
    });
    bar.appendChild(h("button", { type: "button", "class": "ed-mk none", title: "Quitar subrayado", "aria-label": "Quitar subrayado", text: "✕",
      onmousedown: function (ev) { ev.preventDefault(); }, onclick: function () { highlight("transparent"); } }));
    bar.appendChild(h("span", { "class": "ed-mk-hint", text: "Selecciona palabras y elige un color" }));
    return bar;
  }
  function highlight(color) {
    try { document.execCommand("styleWithCSS", false, true); document.execCommand("hiliteColor", false, color); } catch (err) {}
  }

  function isLightColor(c) {
    var m = /^#?([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})/i.exec(c || "#FFFFFF");
    if (!m) return true;
    return (0.299 * parseInt(m[1], 16) + 0.587 * parseInt(m[2], 16) + 0.114 * parseInt(m[3], 16)) / 255 > 0.6;
  }
  function pageNode(p, interactive) {
    var s = size();
    var n = h("div", { "class": "ed-page", style: "width:" + s.w + "px;height:" + s.h + "px;background:" + (p.bg || "#FFFFFF") });
    if (interactive && E.doc.mode === "blocks") n.appendChild(gridOverlay());
    p.elements.forEach(function (e) { n.appendChild(elementNode(e, interactive)); });
    // Número de página (opción del documento; no en la portada ni en la primera página)
    var idx = E.doc.pages.indexOf(p);
    if (E.doc.page_numbers && idx > 0) {
      n.appendChild(h("div", { "class": "ed-pnum " + (E.doc.type === "report" ? "center" : "right"),
        style: "color:" + (isLightColor(p.bg) ? "#555555" : "#FFFFFF"), text: String(idx + 1) }));
    }
    // «Elaborado con CARING» en la primera diapositiva: no es un elemento, no se puede borrar
    if (E.doc.type !== "report" && p === E.doc.pages[0]) {
      var claro = isLightColor(p.bg);
      n.appendChild(h("div", { "class": "ed-brand", style: "color:" + (claro ? "#555555" : "#FFFFFF"), title: "Marca de CARING (fija)" }, [
        h("span", { text: "Elaborado con" }),
        h("img", { src: "caring/img/" + (claro ? "caring-logo-azul.png" : "caring-logo-blanco.png"), alt: "CARING", draggable: "false" })]));
    }
    return n;
  }

  function gridOverlay() {
    var g = grid(), s = size();
    var o = h("div", { "class": "ed-grid", style: "left:" + g.margin + "px;right:" + g.margin + "px;column-gap:" + g.gutter + "px" });
    for (var i = 0; i < 12; i++) o.appendChild(h("span"));
    return o;
  }

  function render() {
    if (!E) return;
    var s = size();
    // Miniaturas
    var pages = E.root.querySelector(".ed-pages-list");
    pages.innerHTML = "";
    E.doc.pages.forEach(function (p, i) {
      var sc = 150 / s.w;
      var wrap = h("div", { "class": "ed-thumb" + (i === E.page ? " on" : ""), style: "height:" + Math.round(s.h * sc + 4) + "px",
        onclick: function () { E.page = i; E.sel = null; render(); } });
      var pn = pageNode(p, false);
      pn.style.transform = "scale(" + sc + ")";
      wrap.appendChild(h("span", { "class": "ed-num", text: String(i + 1) }));
      wrap.appendChild(pn);
      var tools = h("div", { "class": "ed-thumb-tools" }, [
        h("button", { type: "button", title: "Subir", "aria-label": "Subir página", text: "↑", onclick: function (ev) { ev.stopPropagation(); movePage(i, -1); } }),
        h("button", { type: "button", title: "Bajar", "aria-label": "Bajar página", text: "↓", onclick: function (ev) { ev.stopPropagation(); movePage(i, 1); } }),
        h("button", { type: "button", title: "Duplicar", "aria-label": "Duplicar página", text: "⧉", onclick: function (ev) { ev.stopPropagation(); duplicatePage(i); } }),
        h("button", { type: "button", title: "Borrar", "aria-label": "Borrar página", text: "✕", onclick: function (ev) { ev.stopPropagation(); deletePage(i); } })
      ]);
      wrap.appendChild(tools);
      pages.appendChild(wrap);
    });
    // Página activa
    var stage = E.root.querySelector(".ed-stage");
    stage.innerHTML = "";
    var avail = Math.max(200, stage.clientWidth - 48);
    E.scale = Math.min(1.2, avail / s.w);
    var holder = h("div", { "class": "ed-holder", style: "width:" + Math.round(s.w * E.scale) + "px;height:" + Math.round(s.h * E.scale) + "px" });
    E.plots = [];
    var pn = pageNode(page(), true);
    pn.style.transform = "scale(" + E.scale + ")";
    pn.classList.add("ed-active");
    holder.appendChild(pn);
    stage.appendChild(holder);
    stage.appendChild(h("div", { "class": "ed-pageinfo", text: (E.doc.type === "report" ? "Página " : "Diapositiva ") + (E.page + 1) + " de " + E.doc.pages.length }));
    bindPage(pn);
    drawPlots();
    // Modo
    E.root.querySelectorAll(".ed-mode button").forEach(function (b) { b.classList.toggle("on", b.dataset.mode === E.doc.mode); });
    var pb = E.root.querySelector(".ed-pnum-btn");
    if (pb) { pb.classList.toggle("on", !!E.doc.page_numbers); pb.setAttribute("aria-pressed", E.doc.page_numbers ? "true" : "false"); }
    renderSide();
  }

  function renderSide() {
    var side = E.root.querySelector(".ed-props");
    side.innerHTML = "";
    var e = selected();
    if (!e) {
      side.appendChild(h("p", { "class": "ed-hint", text: "Selecciona un elemento para cambiar su aspecto. Doble clic en un texto para escribir." }));
      side.appendChild(h("p", { "class": "ed-hint ed-mode-help", text: E.doc.mode === "blocks"
        ? "Modo «Por bloques»: los elementos se colocan solos en una cuadrícula, uno debajo de otro y alineados. Al arrastrar uno, los demás se recolocan; al cambiar su ancho, se ajusta a las columnas. Cómodo para informes ordenados sin preocuparse de alinear."
        : "Modo «Lienzo libre»: colocas y cambias de tamaño cada elemento donde quieras, como en PowerPoint o Canva. Las guías te ayudan a centrarlo (mantén Alt para moverlo sin guías)." }));
      var p = page();
      side.appendChild(h("div", { "class": "ed-label", text: "Fondo de la página" }));
      side.appendChild(swatches(p.bg || "#FFFFFF", function (c) { p.bg = c; commit(); render(); }));
      return;
    }
    if (e.kind === "text") {
      var s = e.style = e.style || {};
      side.appendChild(h("div", { "class": "ed-label", text: "Texto" }));
      var sizeIn = h("input", { type: "number", min: "6", max: "160", value: s.size || 18, "aria-label": "Tamaño de letra", "class": "form-control form-control-sm", style: "width:80px" });
      sizeIn.addEventListener("change", function () { s.size = Math.max(6, Math.min(160, +sizeIn.value || 18)); commit(); render(); });
      side.appendChild(h("div", { "class": "ed-row" }, [h("span", { text: "Tamaño" }), sizeIn,
        toggle("N", "Negrita", s.bold, function () { s.bold = !s.bold; }),
        toggle("K", "Cursiva", s.italic, function () { s.italic = !s.italic; })]));
      var al = h("div", { "class": "ed-row" });
      [["left", "Izq."], ["center", "Centro"], ["right", "Der."], ["justify", "Just."]].forEach(function (a) {
        al.appendChild(toggle(a[1], "Alinear " + a[1], (s.align || "left") === a[0], function () { s.align = a[0]; }));
      });
      side.appendChild(al);
      side.appendChild(h("div", { "class": "ed-label", text: "Color del texto" }));
      side.appendChild(swatches(s.color || "#000000", function (c) { s.color = c; commit(); render(); }));
      side.appendChild(h("div", { "class": "ed-label", text: "Fondo" }));
      side.appendChild(swatches(s.bg || null, function (c) { s.bg = c; commit(); render(); }, true));
    } else if (e.kind === "shape") {
      e.style = e.style || {};
      side.appendChild(h("div", { "class": "ed-label", text: "Forma" }));
      var fs = h("div", { "class": "ed-shapes" });
      SHAPES.forEach(function (sh) {
        fs.appendChild(h("button", { type: "button", "class": "ed-shape-btn" + ((e.style.shape || "rect") === sh[0] ? " on" : ""), title: sh[1], "aria-label": sh[1],
          onclick: function () { e.style.shape = sh[0] === "rect" ? undefined : sh[0]; commit(); render(); } },
          [h("span", { "class": "ed-shape-ico", style: shapeCss(sh[0], 0) })]));
      });
      side.appendChild(fs);
      side.appendChild(h("div", { "class": "ed-label", text: "Color" }));
      side.appendChild(swatches(e.style.bg || "#2F7BEE", function (c) { e.style.bg = c || "#2F7BEE"; commit(); render(); }));
      if ((e.style.shape || "rect") === "rect") {
      var rad = h("input", { type: "range", min: "0", max: "60", value: e.style.radius || 0, "aria-label": "Redondeo" });
      rad.addEventListener("change", function () { e.style.radius = +rad.value; commit(); render(); });
      side.appendChild(h("div", { "class": "ed-row" }, [h("span", { text: "Redondeo" }), rad]));
      }
    } else if (e.kind === "result") {
      var r = E.assets.results[e.ref];
      side.appendChild(h("div", { "class": "ed-label", title: r ? r.title : "", text: r ? shortTitle(r.title, 80) : "Resultado" }));
      if (r && (r.kind === "grafico" || r.kind === "tabla") && E.palettes) {
        // Colores del gráfico o de la tabla
        e.style = e.style || {};
        var cur = e.style.palette || "netcoin", esTabla = r.kind === "tabla";
        side.appendChild(h("div", { "class": "ed-label", text: esTabla ? "Colores de la tabla" : "Colores del gráfico" }));
        var pl = h("div", { "class": "ed-palettes", role: "radiogroup", "aria-label": "Colores del gráfico" });
        Object.keys(E.palettes).forEach(function (k) {
          var P = E.palettes[k];
          pl.appendChild(h("button", { type: "button", "class": "ed-pal" + (cur === k ? " on" : ""), role: "radio", "aria-checked": cur === k ? "true" : "false",
            title: P.nombre, onclick: function () { e.style.palette = k === "netcoin" ? undefined : k; commit(); render(); } },
            [h("span", { "class": "ed-pal-sw" }, P.colors.slice(0, 5).map(function (c) { return h("i", { style: "background:" + c }); })),
             h("span", { text: P.nombre })]));
        });
        side.appendChild(pl);
        side.appendChild(h("button", { type: "button", "class": "btn btn-light-c btn-sm mb-2",
          text: esTabla ? "Usar estos colores en todas las tablas" : "Usar estos colores en todos los gráficos",
          onclick: function () {
            E.doc.pages.forEach(function (pg) { pg.elements.forEach(function (x) {
              var rr = x.kind === "result" && E.assets.results[x.ref];
              if (rr && rr.kind === r.kind) { x.style = x.style || {}; x.style.palette = e.style.palette; }
            }); });
            commit(); render();
          } }));
      }
      if (r && r.interactive) side.appendChild(h("p", { "class": "ed-hint", text: "En HTML la red se exporta interactiva; en PowerPoint, Word y PDF, como imagen." }));
      if (r && r.fig) side.appendChild(h("p", { "class": "ed-hint", text: "Aquí el gráfico es interactivo (pasa el ratón por encima). Al descargar el informe se incluye como imagen." }));
    }
    side.appendChild(h("div", { "class": "ed-label", text: "Posición y tamaño" }));
    var pos = h("div", { "class": "ed-pos" });
    ["x", "y", "w", "h"].forEach(function (k) {
      var inp = h("input", { type: "number", value: Math.round(e[k]), "aria-label": k, "class": "form-control form-control-sm" });
      inp.addEventListener("change", function () {
        e[k] = Math.max(k === "w" || k === "h" ? 10 : -2000, +inp.value || 0);
        if (E.doc.mode === "blocks") pack(page(), e.id);
        commit(); render();
      });
      pos.appendChild(h("label", null, [h("span", { text: { x: "X", y: "Y", w: "Ancho", h: "Alto" }[k] }), inp]));
    });
    side.appendChild(pos);
    side.appendChild(h("div", { "class": "ed-actions" }, [
      h("button", { type: "button", "class": "btn btn-light-c btn-sm", text: "Al frente", onclick: function () { reorder(e, 1); } }),
      h("button", { type: "button", "class": "btn btn-light-c btn-sm", text: "Al fondo", onclick: function () { reorder(e, -1); } }),
      h("button", { type: "button", "class": "btn btn-light-c btn-sm", text: "Duplicar", onclick: function () {
        var c = clone(e); c.id = uid(); c.x += 16; c.y += 16; addElement(c); } }),
      h("button", { type: "button", "class": "btn btn-danger-c btn-sm", text: "Eliminar", onclick: removeSelected })
    ]));
  }

  function toggle(label, title, on, fn) {
    return h("button", { type: "button", "class": "ed-tg" + (on ? " on" : ""), title: title, "aria-pressed": on ? "true" : "false", text: label,
      onclick: function () { fn(); commit(); render(); } });
  }
  function swatches(current, fn, allowNone) {
    var w = h("div", { "class": "ed-swatches" });
    if (allowNone) w.appendChild(h("button", { type: "button", "class": "ed-sw none" + (!current ? " on" : ""), title: "Sin color", "aria-label": "Sin color", onclick: function () { fn(null); } }));
    SWATCHES.forEach(function (c) {
      w.appendChild(h("button", { type: "button", "class": "ed-sw" + (current && current.toUpperCase() === c ? " on" : ""), title: c, "aria-label": "Color " + c,
        style: "background:" + c, onclick: function () { fn(c); } }));
    });
    var custom = h("input", { type: "color", value: current || "#000000", title: "Otro color", "aria-label": "Otro color" });
    custom.addEventListener("change", function () { fn(custom.value.toUpperCase()); });
    w.appendChild(custom);
    return w;
  }
  function reorder(e, dir) {
    var els = page().elements, i = els.indexOf(e);
    els.splice(i, 1);
    if (dir > 0) els.push(e); else els.unshift(e);
    commit(); render();
  }
  function removeSelected() {
    var p = page();
    p.elements = p.elements.filter(function (x) { return x.id !== E.sel; });
    E.sel = null;
    if (E.doc.mode === "blocks") pack(p);
    commit(); render();
  }

  // Gráficos interactivos de la página activa (plotly ya está cargado por Explorar)
  function drawPlots() {
    (E.plots || []).forEach(function (p) {
      var fig;
      try { fig = typeof p.fig === "string" ? JSON.parse(p.fig) : p.fig; } catch (err) { return; }
      var lay = Object.assign({}, fig.layout || {}, { width: Math.max(80, p.w), height: Math.max(60, p.h), autosize: false, dragmode: false });
      try {
        window.Plotly.newPlot(p.node, fig.data || [], lay,
          { displayModeBar: false, responsive: false, scrollZoom: false, doubleClick: false, locale: "es", showTips: false });
      } catch (err) { /* si falla, el elemento queda vacío pero el informe sigue funcionando */ }
    });
  }

  // Interacción con el ratón ----------------------------------------------------------------
  function bindPage(pn) {
    pn.addEventListener("pointerdown", function (ev) {
      if (E.editing) return;
      var node = ev.target.closest(".ed-el");
      if (!node) { if (E.sel) { E.sel = null; render(); } return; }
      var e = page().elements.filter(function (x) { return x.id === node.dataset.id; })[0];
      if (!e) return;
      ev.preventDefault();
      var resizing = ev.target.classList.contains("ed-handle");
      if (E.sel !== e.id) { E.sel = e.id; render(); node = E.root.querySelector('.ed-active [data-id="' + e.id + '"]'); }
      var start = { x: ev.clientX, y: ev.clientY, ex: e.x, ey: e.y, ew: e.w, eh: e.h }, moved = false;
      function onMove(m) {
        var dx = (m.clientX - start.x) / E.scale, dy = (m.clientY - start.y) / E.scale;
        if (!moved && Math.abs(dx) + Math.abs(dy) < 2) return;
        moved = true;
        if (resizing) {
          e.w = Math.max(20, Math.round(start.ew + dx));
          e.h = Math.max(16, Math.round(start.eh + dy));
          if (m.shiftKey && e.kind !== "text") e.h = Math.round(e.w * start.eh / start.ew);
        } else {
          e.x = Math.round(start.ex + dx);
          e.y = Math.round(start.ey + dy);
          if (E.doc.mode === "free" && !m.altKey) snapCenter(e);
        }
        node.style.left = e.x + "px"; node.style.top = e.y + "px";
        node.style.width = e.w + "px"; node.style.height = e.h + "px";
      }
      function onUp() {
        document.removeEventListener("pointermove", onMove);
        document.removeEventListener("pointerup", onUp);
        if (moved) {
          if (E.doc.mode === "blocks" && isLayoutEl(e)) pack(page(), e.id);
          commit(); render();
        }
      }
      document.addEventListener("pointermove", onMove);
      document.addEventListener("pointerup", onUp);
    });
    pn.addEventListener("dblclick", function (ev) {
      var node = ev.target.closest(".ed-el.ed-text");
      if (!node) return;
      var e = selected();
      if (!e || e.kind !== "text") return;
      var t = node.querySelector(".ed-text");
      E.editing = true;
      t.contentEditable = "true";
      t.focus();
      document.execCommand && document.execCommand("selectAll", false, null);
      var bar = markBar(t);
      E.root.querySelector(".ed-stage").appendChild(bar);
      t.addEventListener("blur", function () {
        t.contentEditable = "false";
        E.editing = false;
        if (bar.parentNode) bar.parentNode.removeChild(bar);
        var runs = readRuns(t);
        var txt = runs.map(function (r) { return r.t; }).join("");
        var marcado = runs.some(function (r) { return r.m; });
        var cambio = txt !== e.text || JSON.stringify(marcado ? runs : null) !== JSON.stringify(e.runs || null);
        e.text = txt;
        if (marcado) e.runs = runs; else delete e.runs;
        if (cambio) commit();
        render();
      }, { once: true });
    });
  }
  // Imán a los centros de la página (mantén Alt para moverlo sin imán).
  function snapCenter(e) {
    var s = size(), th = 6;
    if (Math.abs(e.x + e.w / 2 - s.w / 2) < th) e.x = Math.round(s.w / 2 - e.w / 2);
    if (Math.abs(e.y + e.h / 2 - s.h / 2) < th) e.y = Math.round(s.h / 2 - e.h / 2);
    if (Math.abs(e.x) < th) e.x = 0;
    if (Math.abs(e.y) < th) e.y = 0;
  }

  document.addEventListener("keydown", function (ev) {
    if (!E || !document.body.contains(E.root) || E.editing) return;
    var tag = (ev.target.tagName || "").toLowerCase();
    if (tag === "input" || tag === "textarea" || ev.target.isContentEditable) return;
    if ((ev.ctrlKey || ev.metaKey) && ev.key.toLowerCase() === "z") { ev.preventDefault(); if (ev.shiftKey) redo(); else undo(); return; }
    var e = selected();
    if (!e) return;
    if (ev.key === "Delete" || ev.key === "Backspace") { ev.preventDefault(); removeSelected(); }
    var step = ev.shiftKey ? 10 : 1, mv = { ArrowLeft: [-step, 0], ArrowRight: [step, 0], ArrowUp: [0, -step], ArrowDown: [0, step] }[ev.key];
    if (mv && E.doc.mode === "free") { ev.preventDefault(); e.x += mv[0]; e.y += mv[1]; commit(); render(); }
  });

  // Panel de resultados -----------------------------------------------------------------------
  function renderResults() {
    var list = E.root.querySelector(".ed-results");
    list.innerHTML = "";
    var ids = Object.keys(E.assets.results || {});
    if (!ids.length) {
      list.appendChild(h("p", { "class": "ed-hint", text: "Aún no hay resultados. Usa «+ Añadir al informe» en Explorar o en Redes." }));
      return;
    }
    ids.forEach(function (id) {
      var r = E.assets.results[id];
      var kind = { red: "RED", grafico: "GRÁF", tabla: "TABLA" }[r.kind] || r.kind;
      list.appendChild(h("div", { "class": "ed-res-item" }, [
        r.png ? h("img", { src: r.png, alt: "" }) : h("span", { "class": "ed-kind", text: kind }),
        h("div", { "class": "ed-rtext", title: r.title }, [h("strong", { text: shortTitle(r.title, 60) }), h("span", { text: kind + " · " + (r.source || "") })]),
        h("button", { type: "button", "class": "btn btn-primary btn-sm", text: "Añadir", "aria-label": "Añadir " + r.title, onclick: function () { addResult(id); } }),
        h("button", { type: "button", "class": "btn-link-c ed-del", text: "✕", title: "Borrar resultado", "aria-label": "Borrar " + r.title,
          onclick: function () { Shiny.setInputValue(E.deleteResultId, { id: id, t: Date.now() }, { priority: "event" }); } })
      ]));
    });
  }

  // Títulos largos: se cortan (el título completo sale al pasar el ratón)
  function shortTitle(t, max) {
    t = String(t || "");
    return t.length > max ? t.slice(0, max - 1).replace(/\s+\S*$/, "") + "…" : t;
  }

  // Montaje -------------------------------------------------------------------------------
  function mount(m) {
    var root = document.getElementById(m.el);
    if (!root) { if (E && E.root) E.root.innerHTML = ""; E = null; return; }
    E = { root: root, doc: m.doc, assets: m.assets || { results: {}, images: {} }, templates: m.templates || {},
          theme: m.theme || { text: "#000000", heading: "#135DCD", accent: "#2F7BEE", resultBg: null, radius: 0 },
          palettes: m.palettes || null,
          inputId: m.inputId, deleteResultId: m.deleteResultId, imageInput: m.imageInput,
          page: 0, sel: null, undo: [], redo: [], last: JSON.stringify(m.doc), scale: 1 };
    var tpls = E.doc.type === "report" ? TEMPLATES_REPORT : TEMPLATES_SLIDES;
    root.innerHTML = "";
    var addMenu = h("ul", { "class": "dropdown-menu" }, tpls.map(function (t) {
      return h("li", null, [h("a", { "class": "dropdown-item", href: "#", text: t[1], onclick: function (ev) { ev.preventDefault(); addPage(t[0]); } })]);
    }));
    var toolbar = h("div", { "class": "ed-toolbar" }, [
      h("div", { "class": "ed-mode", role: "radiogroup", "aria-label": "Modo de edición" }, [
        h("button", { type: "button", "data-mode": "free", title: "Colocas cada elemento donde quieras, como en PowerPoint o Canva", text: "Lienzo libre",
          onclick: function () { E.doc.mode = "free"; commit(); render(); } }),
        h("button", { type: "button", "data-mode": "blocks", title: "Los elementos se colocan y alinean solos en una cuadrícula, uno debajo de otro", text: "Por bloques",
          onclick: function () { E.doc.mode = "blocks"; packAll(); commit(); render(); } })
      ]),
      h("span", { "class": "ed-sep" }),
      h("button", { type: "button", "class": "btn btn-light-c btn-sm", text: "T  Texto", onclick: function () { addText("text"); } }),
      h("button", { type: "button", "class": "btn btn-light-c btn-sm", text: "Título", onclick: function () { addText("title"); } }),
      h("div", { "class": "dropdown d-inline-block" }, [
        h("button", { type: "button", "class": "btn btn-light-c btn-sm dropdown-toggle", "data-bs-toggle": "dropdown", text: "Forma" }),
        h("ul", { "class": "dropdown-menu ed-shape-menu" }, SHAPES.map(function (sh) {
          return h("li", null, [h("a", { "class": "dropdown-item", href: "#", onclick: function (ev) { ev.preventDefault(); addShape(sh[0]); } },
            [h("span", { "class": "ed-shape-ico", style: shapeCss(sh[0], 0) }), h("span", { text: sh[1] })])]);
        }))]),
      h("button", { type: "button", "class": "btn btn-light-c btn-sm", text: "Imagen", onclick: function () {
        var f = document.getElementById(E.imageInput); if (f) f.click(); } }),
      h("button", { type: "button", "class": "btn btn-light-c btn-sm ed-pnum-btn", text: "Nº de página", title: "Numerar las páginas automáticamente (no en la portada)",
        "aria-pressed": "false", onclick: function () { E.doc.page_numbers = !E.doc.page_numbers; commit(); render(); } }),
      h("span", { "class": "ed-sep" }),
      h("button", { type: "button", "class": "btn btn-light-c btn-sm", text: "↶", title: "Deshacer (Ctrl+Z)", "aria-label": "Deshacer", onclick: undo }),
      h("button", { type: "button", "class": "btn btn-light-c btn-sm", text: "↷", title: "Rehacer (Ctrl+Mayús+Z)", "aria-label": "Rehacer", onclick: redo }),
      h("span", { "class": "ed-status small-2", text: "Guardado" })
    ]);
    var body = h("div", { "class": "ed-body" }, [
      h("aside", { "class": "ed-pages", "aria-label": "Páginas" }, [
        h("div", { "class": "ed-pages-list" }),
        h("div", { "class": "dropdown" }, [
          h("button", { type: "button", "class": "btn btn-outline-primary btn-sm w-100", "data-bs-toggle": "dropdown",
            text: E.doc.type === "report" ? "+ Página" : "+ Diapositiva" }),
          addMenu])
      ]),
      h("main", { "class": "ed-stage" }),
      h("aside", { "class": "ed-side" }, [
        h("section", { "class": "c-card c-card-pad mb-3" }, [h("h2", { text: "Propiedades" }), h("div", { "class": "ed-props" })]),
        h("section", { "class": "c-card c-card-pad" }, [h("h2", { text: "Resultados del proyecto" }), h("div", { "class": "ed-results" })])
      ])
    ]);
    root.appendChild(toolbar);
    root.appendChild(body);
    renderResults();
    render();
  }

  if (window.Shiny) {
    Shiny.addCustomMessageHandler("caring-editor-load", function (m) { mount(m); });
    Shiny.addCustomMessageHandler("caring-editor-assets", function (m) {
      if (!E) return;
      E.assets = m.assets; renderResults(); render();
    });
    Shiny.addCustomMessageHandler("caring-editor-add-image", function (m) {
      if (!E) return;
      E.assets.images[m.src] = m.uri; addImage(m.src);
    });
    Shiny.addCustomMessageHandler("caring-editor-flush", function (m) {
      if (!E) return;
      clearTimeout(E.saveTimer);
      Shiny.setInputValue(E.inputId, JSON.stringify(E.doc), { priority: "event" });
    });
  }
  // Al mostrar el paso Informes, recalcula la escala (antes estaba oculto).
  document.addEventListener("shown.bs.tab", function () { if (E && document.body.contains(E.root)) setTimeout(render, 30); });
  var rt;
  window.addEventListener("resize", function () { clearTimeout(rt); rt = setTimeout(function () { if (E && document.body.contains(E.root)) render(); }, 150); });
})();
