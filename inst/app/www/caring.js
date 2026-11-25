// CARING 2 · utilidades del navegador
(function () {
  var COOKIE = "caring_session";

  // Algunos visores (paneles de editores, iframes) no dejan usar cookies:
  // en ese caso CARING funciona igual, solo que sin "Recordarme".
  function getCookie(name) {
    try {
      var m = document.cookie.match(new RegExp("(?:^|; )" + name + "=([^;]*)"));
      return m ? decodeURIComponent(m[1]) : "";
    } catch (e) { return ""; }
  }
  function setCookie(value) {
    try { document.cookie = value; } catch (e) { /* sin cookies */ }
  }

  // Al conectar, el servidor recibe el token de sesión guardado (si existe).
  var sent = false;
  function sendAuth() {
    if (sent || !window.Shiny || !Shiny.setInputValue) return;
    sent = true;
    Shiny.setInputValue("auth_token", getCookie(COOKIE) || "", { priority: "event" });
  }
  $(document).on("shiny:connected", sendAuth);
  $(function () {
    if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected && Shiny.shinyapp.isConnected()) sendAuth();
  });

  // La cookie se limita a la ruta de la app (p. ej. /caring/) para no mezclarse con otras webs del dominio.
  function cookiePath() {
    var p = window.location.pathname || "/";
    return p.substring(0, p.lastIndexOf("/") + 1) || "/";
  }

  Shiny.addCustomMessageHandler("caring-set-cookie", function (m) {
    var c = COOKIE + "=" + encodeURIComponent(m.token) + "; path=" + cookiePath() + "; SameSite=Lax";
    if (m.days) c += "; max-age=" + Math.round(m.days * 86400);
    if (m.secure) c += "; Secure";
    setCookie(c);
  });

  Shiny.addCustomMessageHandler("caring-clear-cookie", function (m) {
    setCookie(COOKIE + "=; path=" + cookiePath() + "; max-age=0; SameSite=Lax");
    setCookie(COOKIE + "=; path=/; max-age=0; SameSite=Lax");
  });

  // Quita ?verificar= / ?recuperar= de la barra de direcciones una vez usados.
  Shiny.addCustomMessageHandler("caring-clean-url", function (m) {
    if (window.history && window.history.replaceState) {
      window.history.replaceState({}, document.title, window.location.pathname);
    }
  });

  // Abre un HTML (red o presentación) en una pestaña nueva sin pasar por el servidor.
  Shiny.addCustomMessageHandler("caring-open-html", function (m) {
    var blob = new Blob([m.html], { type: "text/html" });
    var url = URL.createObjectURL(blob);
    var w = window.open(url, "_blank");
    if (!w) alert("El navegador ha bloqueado la ventana emergente. Permite las ventanas emergentes para CARING.");
    setTimeout(function () { URL.revokeObjectURL(url); }, 60000);
  });

  // Acciones con identificador (tarjetas de proyecto, historial, etc.)
  window.caringAction = function (input, id, action, ev) {
    if (ev) { ev.stopPropagation(); ev.preventDefault(); }
    Shiny.setInputValue(input, { id: id, action: action, t: Date.now() }, { priority: "event" });
  };

  // Comprueba el tamaño del archivo antes de subirlo (el límite lo fija Administración).
  document.addEventListener("change", function (e) {
    var t = e.target;
    if (!t || t.type !== "file" || !t.closest) return;
    var box = t.closest("[data-max-mb]");
    if (!box) return;
    var container = t.closest(".shiny-input-container") || box;
    var msg = container.querySelector(".c-size-error");
    if (msg) msg.remove();
    var max = parseFloat(box.getAttribute("data-max-mb")) * 1024 * 1024;
    var f = t.files && t.files[0];
    if (f && max > 0 && f.size > max) {
      e.stopImmediatePropagation();
      e.stopPropagation();
      t.value = "";
      var mb = function (b) { return (b / 1024 / 1024).toLocaleString("es-ES", { maximumFractionDigits: 1 }); };
      msg = document.createElement("div");
      msg.className = "c-msg-error c-size-error";
      msg.style.marginTop = "6px";
      msg.textContent = "«" + f.name + "» pesa " + mb(f.size) + " MB y el máximo permitido es " +
        mb(max) + " MB. Pide al equipo de administración que amplíe el límite.";
      container.appendChild(msg);
    }
  }, true);

  // Pulsar Enter en un formulario de acceso equivale a pulsar su botón.
  $(document).on("keydown", "[data-enter-click] input", function (e) {
    if (e.key === "Enter") {
      var target = $(this).closest("[data-enter-click]").attr("data-enter-click");
      setTimeout(function () { $("#" + target).trigger("click"); }, 50);
    }
  });
})();

// Errores que llegan a una salida sin pasar por error_es() (fallos imprevistos): se
// muestran en español; el texto original queda como detalle técnico.
(function () {
  var conocidos = [
    [/subscript out of bounds|subíndice fuera de\s+los límites/i, "Se ha pedido un elemento que no existe (índice fuera de los límites)."],
    [/object '([^']+)' not found/i, "No existe «$1»."],
    [/undefined columns selected/i, "Alguna de las variables elegidas no existe."],
    [/non-numeric argument/i, "Se ha intentado hacer una operación numérica con una variable que no es numérica."],
    [/missing value where TRUE\/FALSE needed|valor ausente donde TRUE\/FALSE es necesario/i, "Hay valores perdidos (o categorías sin etiqueta) donde se necesitaba un sí/no."],
    [/argument is of length zero/i, "Falta un valor necesario."],
    [/An error has occurred/i, "Ha ocurrido un error inesperado. Si se repite, avisa a la administración de CARING."]
  ];
  var ingles = /\b(the|of|is|are|not|must|be|argument|object|cannot|could|found|invalid|undefined|missing|length|unused|specify|please|error|should|need|failed|unable|with|to)\b/i;
  var espanol = /[áéíóúñ¿¡]|\b(el|la|los|las|de|del|que|una?|para|con|hay)\b/;
  function traducir(msg) {
    if (!msg) return msg;
    for (var i = 0; i < conocidos.length; i++) {
      if (conocidos[i][0].test(msg)) return msg.replace(/^[\s\S]*$/, function (m) { var r = m.match(conocidos[i][0]); return conocidos[i][1].replace("$1", r && r[1] ? r[1] : ""); });
    }
    if (ingles.test(msg) && !espanol.test(msg)) return "No se ha podido mostrar este apartado (detalle técnico: " + msg + ").";
    return msg;
  }
  window.caringTraducirError = traducir;
  $(document).on("shiny:error", function (ev) {
    if (ev.error && typeof ev.error.message === "string") ev.error.message = traducir(ev.error.message);
  });
})();

// Textos de la barra de subida de archivos de Shiny en español.
(function () {
  var es = { "Upload complete": "Archivo subido", "Finishing upload": "Terminando la subida…" };
  new MutationObserver(function (muts) {
    muts.forEach(function (m) {
      var el = m.target.nodeType === 3 ? m.target.parentNode : m.target;
      if (!el || !el.classList || !el.classList.contains("progress-bar")) return;
      var t = (el.textContent || "").trim();
      if (es[t]) el.textContent = es[t];
    });
  }).observe(document.documentElement, { subtree: true, childList: true, characterData: true });
})();

// Visita guiada: resalta un elemento y explica para qué sirve. Se muestra una vez (se recuerda en
// este navegador) y se puede volver a ver desde Ayuda. Los pasos sin elemento visible se saltan.
// Mientras dura, la página no se desplaza: la visita lleva cada elemento a la vista (sin animación).
(function () {
  var T = null;
  function visible(el) { if (!el) return false; var r = el.getBoundingClientRect(); return r.width > 0 && r.height > 0; }
  function done(key) { try { return localStorage.getItem("caring_tour_" + key) === "1"; } catch (e) { return false; } }
  function mark(key) { try { localStorage.setItem("caring_tour_" + key, "1"); } catch (e) {} }
  function lockScroll(on) {
    var de = document.documentElement, b = document.body;
    if (on) {
      T.saved = { de: de.style.overflow, b: b.style.overflow, sb: de.style.scrollBehavior };
      de.style.scrollBehavior = "auto";            // sin desplazamiento animado (Bootstrap lo activa)
      de.style.overflow = "hidden"; b.style.overflow = "hidden";
    } else if (T && T.saved) {
      de.style.overflow = T.saved.de; b.style.overflow = T.saved.b; de.style.scrollBehavior = T.saved.sb;
    }
  }
  function stop(e) { e.preventDefault(); }
  function close() {
    if (!T) return;
    mark(T.key);
    lockScroll(false);
    [T.hl, T.pop].forEach(function (n) { if (n && n.parentNode) n.parentNode.removeChild(n); });
    document.removeEventListener("keydown", T.onkey, true);
    window.removeEventListener("resize", T.reflow);
    window.removeEventListener("scroll", T.reflow, true);
    document.removeEventListener("wheel", stop, { passive: false });
    document.removeEventListener("touchmove", stop, { passive: false });
    var f = T.prevFocus;
    T = null;
    if (f && f.focus) try { f.focus(); } catch (e) {}
  }
  // Lleva el elemento a la vista sin animación y devuelve su posición
  function bringIntoView(el) {
    var r = el.getBoundingClientRect(), vh = innerHeight;
    if (r.top < 70 || r.bottom > vh - 20) {
      var y = window.scrollY + r.top - Math.max(80, (vh - Math.min(r.height, vh * 0.6)) / 2);
      document.documentElement.style.overflow = "";             // permitir el salto un instante
      window.scrollTo({ top: Math.max(0, y), left: 0, behavior: "instant" });
      document.documentElement.style.overflow = "hidden";
    }
  }
  function place() {
    if (!T || !T.el) return;
    var r = T.el.getBoundingClientRect(), pad = 6, vw = innerWidth, vh = innerHeight;
    // solo la parte visible (los elementos muy altos, como la lista de variables)
    var top = Math.max(r.top, 4), bottom = Math.min(r.bottom, vh - 4);
    if (bottom < top) bottom = top + 20;
    T.hl.style.left = (r.left - pad) + "px"; T.hl.style.top = (top - pad) + "px";
    T.hl.style.width = (r.width + 2 * pad) + "px"; T.hl.style.height = (bottom - top + 2 * pad) + "px";
    var pw = T.pop.offsetWidth, ph = T.pop.offsetHeight;
    var y = bottom + 14 + ph <= vh - 10 ? bottom + 14 : (top - ph - 14 >= 10 ? top - ph - 14 : vh - ph - 16);
    var x = Math.min(Math.max(10, r.left), vw - pw - 10);
    T.pop.style.top = Math.min(Math.max(10, y), Math.max(10, vh - ph - 10)) + "px";   // siempre dentro de la pantalla
    T.pop.style.left = Math.max(10, x) + "px";
  }
  function show(i) {
    var steps = T.steps, dir = i >= T.i ? 1 : -1;
    while (i >= 0 && i < steps.length && !visible(document.querySelector(steps[i].sel))) i += dir;
    if (i < 0 || i >= steps.length) return close();
    T.i = i;
    T.el = document.querySelector(steps[i].sel);
    bringIntoView(T.el);
    var vis = steps.filter(function (s) { return visible(document.querySelector(s.sel)); });
    var n = vis.length, pos = vis.indexOf(steps[i]) + 1;
    T.pop.innerHTML = "";
    var h = document.createElement("h2"); h.id = "c-tour-title"; h.textContent = steps[i].title;
    var p = document.createElement("p"); p.textContent = steps[i].text;
    var foot = document.createElement("div"); foot.className = "c-tour-foot";
    var cnt = document.createElement("span"); cnt.className = "small-2"; cnt.textContent = pos + " de " + n;
    var bx = document.createElement("div"); bx.className = "d-flex gap-2";
    function btn(txt, cls, fn) { var b = document.createElement("button"); b.type = "button"; b.className = "btn btn-sm " + cls; b.textContent = txt; b.onclick = fn; return b; }
    if (pos > 1) bx.appendChild(btn("Anterior", "btn-light-c", function () { show(T.i - 1); }));
    var last = pos === n;
    var next = btn(last ? "Terminar" : "Siguiente", "btn-primary", function () { last ? close() : show(T.i + 1); });
    bx.appendChild(next);
    foot.appendChild(cnt); foot.appendChild(btn("Saltar la visita", "btn-link-c", close)); foot.appendChild(bx);
    T.pop.appendChild(h); T.pop.appendChild(p); T.pop.appendChild(foot);
    place();
    requestAnimationFrame(function () { place(); if (T) next.focus({ preventScroll: true }); });
  }
  window.caringTour = function (steps, key, force) {
    if (!steps || !steps.length || (!force && done(key))) return;
    if (T) close();
    var hl = document.createElement("div"); hl.className = "c-tour-hl"; hl.setAttribute("aria-hidden", "true");
    var pop = document.createElement("div"); pop.className = "c-tour-pop"; pop.setAttribute("role", "dialog");
    pop.setAttribute("aria-modal", "true"); pop.setAttribute("aria-labelledby", "c-tour-title");
    document.body.appendChild(hl); document.body.appendChild(pop);
    T = { steps: steps, key: key, i: 0, hl: hl, pop: pop, prevFocus: document.activeElement };
    lockScroll(true);
    T.reflow = function () { place(); };
    T.onkey = function (e) {
      if (["Escape", "ArrowRight", "ArrowLeft"].indexOf(e.key) >= 0) e.stopPropagation();   // no llegan a la página de debajo
      if (e.key === "Escape") { e.preventDefault(); close(); }
      else if (e.key === "Tab") {                       // el foco no sale del recuadro de la visita
        var bs = T.pop.querySelectorAll("button");
        if (!bs.length) return;
        var i = Array.prototype.indexOf.call(bs, document.activeElement);
        e.preventDefault();
        bs[(i + (e.shiftKey ? -1 : 1) + bs.length) % bs.length].focus({ preventScroll: true });
      }
      else if (e.key === "ArrowRight") { e.preventDefault(); show(T.i + 1); }
      else if (e.key === "ArrowLeft") { e.preventDefault(); show(T.i - 1); }
      else if (["PageDown", "PageUp", "Home", "End", "ArrowDown", "ArrowUp", " "].indexOf(e.key) >= 0 && !e.target.closest(".c-tour-pop")) e.preventDefault();
    };
    document.addEventListener("keydown", T.onkey, true);
    window.addEventListener("resize", T.reflow);
    window.addEventListener("scroll", T.reflow, true);
    document.addEventListener("wheel", stop, { passive: false });          // la página no se mueve durante la visita
    document.addEventListener("touchmove", stop, { passive: false });
    show(0);
  };
  if (window.Shiny) Shiny.addCustomMessageHandler("caring-tour", function (m) {
    setTimeout(function () { window.caringTour(m.steps, m.key, m.force); }, m.delay || 0);
  });
})();

// Accesibilidad ------------------------------------------------------------------------------
// Ajustes que Shiny y sus componentes no traen de serie: nombre de las ventanas, etiquetas de los
// desplegables, avisos que se leen en voz alta y filas de tabla que se pueden usar con el teclado.
(function () {
  var uid = 0;
  function fix(root) {
    if (!root || !root.querySelectorAll) return;
    // Ventanas (modales): se anuncian con su título, que pasa a ser un encabezado de nivel 2
    var modals = Array.prototype.slice.call(root.querySelectorAll(".modal"));
    if (root.classList && root.classList.contains("modal")) modals.push(root);
    modals.forEach(function (m) {
      var t = m.querySelector(".modal-title");
      if (!t) return;
      if (!t.id) t.id = "c-modal-title-" + (++uid);
      m.setAttribute("aria-labelledby", t.id);
      if (/^H[3-6]$/.test(t.tagName)) { t.setAttribute("role", "heading"); t.setAttribute("aria-level", "2"); }
    });
    // Desplegables con buscador (selectize): toman el texto de su etiqueta, aunque esté oculta
    root.querySelectorAll("input[id$='-selectized']").forEach(function (inp) {
      if (inp.getAttribute("aria-label")) return;
      var base = inp.id.replace(/-selectized$/, "");
      var lab = document.querySelector("label[for='" + base + "'], label[for='" + inp.id + "'], #" + CSS.escape(base) + "-label");
      var sel = document.getElementById(base);
      var txt = (lab && lab.textContent.trim()) || (sel && sel.getAttribute("aria-label")) || inp.getAttribute("placeholder") || "";
      if (txt) inp.setAttribute("aria-label", txt);
    });
    // Pestañas ocultas sin texto (p. ej. «recuperar contraseña»): fuera del recorrido con Tab
    root.querySelectorAll(".nav-link").forEach(function (a) {
      if (!a.textContent.trim() && !a.getAttribute("aria-label")) { a.setAttribute("tabindex", "-1"); a.setAttribute("aria-hidden", "true"); }
    });
    // Filas de tabla que se abren al pulsarlas: también con Tab + Intro
    root.querySelectorAll(".c-rows-kbd .rt-tbody .rt-tr").forEach(function (tr) {
      if (tr.hasAttribute("tabindex") || tr.classList.contains("-padRow")) return;
      tr.setAttribute("tabindex", "0");
      var first = tr.querySelector(".rt-td");
      if (first) tr.setAttribute("aria-label", "Abrir " + first.textContent.trim());
    });
    // Mensajes de los formularios (contraseña incorrecta, enlace enviado…): se leen al aparecer
    root.querySelectorAll(".c-msg-error:not([aria-live]), .c-msg-ok:not([aria-live])").forEach(function (m) {
      m.setAttribute("aria-live", m.classList.contains("c-msg-error") ? "assertive" : "polite");
    });
    // Ventanas de red y de informes: un título para el lector de pantalla
    root.querySelectorAll("iframe:not([title])").forEach(function (f) { f.setAttribute("title", "Contenido incrustado"); });
  }
  $(document).on("keydown", ".c-rows-kbd .rt-tbody .rt-tr", function (e) {
    if ((e.key === "Enter" || e.key === " ") && e.target === this) {
      e.preventDefault();
      var td = this.querySelector(".rt-td");
      if (td) td.click();
    }
  });
  // Los avisos (notificaciones) se leen en voz alta; los de error, con prioridad
  function liveRegion() {
    var p = document.getElementById("shiny-notification-panel");
    if (p && !p.getAttribute("aria-live")) { p.setAttribute("aria-live", "polite"); p.setAttribute("role", "status"); }
    if (p) p.querySelectorAll(".shiny-notification-error:not([role])").forEach(function (n) { n.setAttribute("role", "alert"); });
  }
  var pending = false;
  new MutationObserver(function () {
    if (pending) return;
    pending = true;
    requestAnimationFrame(function () { pending = false; fix(document); liveRegion(); });
  }).observe(document.documentElement, { subtree: true, childList: true });
  $(function () { fix(document); });
  // Al abrir una ventana, el foco va dentro; al cerrarla, vuelve a donde estaba
  var lastFocus = null;
  $(document).on("show.bs.modal", function (e) { lastFocus = document.activeElement; fix(e.target); });
  $(document).on("shown.bs.modal", function (e) { fix(e.target); });
  $(document).on("hidden.bs.modal", function () {
    if (lastFocus && document.body.contains(lastFocus) && lastFocus.focus) try { lastFocus.focus(); } catch (e) {}
    lastFocus = null;
  });
})();
