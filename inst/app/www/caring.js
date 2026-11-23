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
