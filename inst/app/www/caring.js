// CARING 2 · utilidades del navegador
(function () {
  var COOKIE = "caring_session";

  function getCookie(name) {
    var m = document.cookie.match(new RegExp("(?:^|; )" + name + "=([^;]*)"));
    return m ? decodeURIComponent(m[1]) : "";
  }

  // Al conectar, el servidor recibe el token de sesión guardado (si existe).
  $(document).on("shiny:connected", function () {
    Shiny.setInputValue("auth_token", getCookie(COOKIE) || "", { priority: "event" });
  });

  Shiny.addCustomMessageHandler("caring-set-cookie", function (m) {
    var c = COOKIE + "=" + encodeURIComponent(m.token) + "; path=/; SameSite=Lax";
    if (m.days) c += "; max-age=" + Math.round(m.days * 86400);
    if (m.secure) c += "; Secure";
    document.cookie = c;
  });

  Shiny.addCustomMessageHandler("caring-clear-cookie", function (m) {
    document.cookie = COOKIE + "=; path=/; max-age=0; SameSite=Lax";
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

  // Pulsar Enter en un formulario de acceso equivale a pulsar su botón.
  $(document).on("keydown", "[data-enter-click] input", function (e) {
    if (e.key === "Enter") {
      var target = $(this).closest("[data-enter-click]").attr("data-enter-click");
      setTimeout(function () { $("#" + target).trigger("click"); }, 50);
    }
  });
})();
