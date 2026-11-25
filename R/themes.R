# Estilos de informes y diapositivas ---------------------------------------------------------
# Todos siguen la identidad de CARING (azules #236CDB / #1257C2 / #CBDEFB, Roboto).
# Cada elemento de una página lleva un "role" que dice qué papel cumple:
#   decorado: bg (fondo de página completo), band (franja del título), rule (línea), deco (adorno)
#   texto:    title, subtitle, foot, heading (títulos dentro del contenido), body
#   result:   gráficos, tablas y redes
# Así un estilo puede rehacer el decorado y recolorear los textos sin tocar el contenido.

caring_themes <- list(
  clasico = list(
    nombre = "Clásico", desc = "El de siempre: franja azul CARING y fondo blanco.",
    page = "#FFFFFF", text = "#000000", heading = "#1257C2", accent = "#236CDB",
    band = "#236CDB", title = "#FFFFFF",
    cover = "#236CDB", cover_text = "#FFFFFF", cover_sub = "#FFFFFF",
    section = "#1257C2", section_text = "#FFFFFF",
    result_bg = NULL, radius = 0,
    rep_page = "#FFFFFF", rep_cover = "#236CDB", rep_cover_text = "#FFFFFF", rep_heading = "#1257C2", rep_text = "#000000"),
  claro = list(
    nombre = "Claro", desc = "Fondos azul muy pálido y títulos en azul: luminoso y tranquilo.",
    page = "#F7FAFF", text = "#1D2B3F", heading = "#1257C2", accent = "#236CDB",
    band = "#EAF2FE", title = "#1257C2",
    cover = "#EAF2FE", cover_text = "#1257C2", cover_sub = "#1D2B3F",
    section = "#CBDEFB", section_text = "#1257C2",
    result_bg = "#FFFFFF", radius = 10,
    rep_page = "#FFFFFF", rep_cover = "#EAF2FE", rep_cover_text = "#1257C2", rep_heading = "#1257C2", rep_text = "#1D2B3F"),
  profesional = list(
    nombre = "Profesional", desc = "Azul marino oscuro con acentos azules: sobrio y elegante.",
    page = "#0F1E36", text = "#DCE6F5", heading = "#FFFFFF", accent = "#236CDB",
    band = NULL, title = "#FFFFFF",
    cover = "#0B1730", cover_text = "#FFFFFF", cover_sub = "#9DB7DC",
    section = "#236CDB", section_text = "#FFFFFF",
    result_bg = "#FFFFFF", radius = 4,
    rep_page = "#FFFFFF", rep_cover = "#0F1E36", rep_cover_text = "#FFFFFF", rep_heading = "#0F1E36", rep_text = "#1A1A1A"),
  divertido = list(
    nombre = "Divertido", desc = "Formas redondeadas y círculos de colores, como los nodos de una red.",
    page = "#FFFFFF", text = "#1A1A1A", heading = "#236CDB", accent = "#FF7F0E",
    band = "#236CDB", title = "#FFFFFF",
    cover = "#236CDB", cover_text = "#FFFFFF", cover_sub = "#FFFFFF",
    section = "#FF7F0E", section_text = "#FFFFFF",
    result_bg = "#F2F7FF", radius = 18,
    rep_page = "#FFFFFF", rep_cover = "#236CDB", rep_cover_text = "#FFFFFF", rep_heading = "#236CDB", rep_text = "#1A1A1A"),
  academico = list(
    nombre = "Académico", desc = "Blanco y sobrio, con una línea fina bajo el título: para informes y artículos.",
    page = "#FFFFFF", text = "#000000", heading = "#1257C2", accent = "#236CDB",
    band = NULL, title = "#1257C2",
    cover = "#FFFFFF", cover_text = "#0D1B2A", cover_sub = "#4A4A4A",
    section = "#F5F5F5", section_text = "#1257C2",
    result_bg = NULL, radius = 0,
    rep_page = "#FFFFFF", rep_cover = "#FFFFFF", rep_cover_text = "#0D1B2A", rep_heading = "#1257C2", rep_text = "#000000")
)

theme_get <- function(id) caring_themes[[id %||% "clasico"]] %||% caring_themes$clasico

decor_roles <- c("bg", "band", "rule", "deco")

shape <- function(x, y, w, h, bg, role, radius = 0) {
  el("shape", x, y, w, h, role = role, style = list(bg = bg, radius = radius))
}

# Decorado de una página según estilo, tipo de documento y clase de página.
theme_decor <- function(theme, kind, type = "slides") {
  th <- theme_get(theme)
  sz <- page_size(type)
  W <- sz[["w"]]; H <- sz[["h"]]
  if (type == "report") {
    if (kind == "portada") {
      d <- list(shape(0, 0, W, 260, th$rep_cover, "band"))
      if (theme == "academico") d <- list(shape(32, 105, 6, 130, "#236CDB", "rule"))
      if (theme == "claro") d <- c(d, list(shape(0, 256, W, 4, "#236CDB", "rule")))
      if (theme == "profesional") d <- c(d, list(shape(50, 236, 80, 4, "#236CDB", "rule")))
      if (theme == "divertido") d <- c(d, list(
        shape(W - 170, -60, 220, 220, "#FF7F0E", "deco", 110),
        shape(W - 90, 150, 90, 90, "#2CA02C", "deco", 45)))
      return(d)
    }
    if (kind == "apartado") {
      return(switch(theme,
        claro = list(shape(50, 92, 495, 3, "#CBDEFB", "rule")),
        profesional = list(shape(50, 92, 60, 4, "#236CDB", "rule")),
        divertido = list(shape(50, 92, 40, 6, "#FF7F0E", "rule", 3)),
        academico = list(shape(50, 92, 495, 1, "#236CDB", "rule")),
        list()))
    }
    return(list())
  }
  # Diapositivas
  if (kind == "portada") {
    d <- list(shape(0, 0, W, H, th$cover, "bg"))
    d <- c(d, switch(theme,
      claro = list(shape(0, 0, 18, H, "#236CDB", "deco"), shape(70, 290, 120, 5, "#236CDB", "rule")),
      profesional = list(shape(0, H - 14, W, 14, "#236CDB", "deco"), shape(70, 290, 120, 4, "#236CDB", "rule")),
      divertido = list(
        shape(W - 300, -110, 400, 400, "#FF7F0E", "deco", 200),
        shape(W - 150, 330, 190, 190, "#2CA02C", "deco", 95),
        shape(-70, 400, 210, 210, "#CBDEFB", "deco", 105),
        shape(W - 360, 360, 60, 60, "#9467BD", "deco", 30)),
      academico = list(shape(46, 160, 8, 210, "#236CDB", "rule"), shape(70, 450, W - 140, 1, "#C6C6C6", "rule")),
      list()))
    return(d)
  }
  if (kind == "seccion") {
    d <- list(shape(0, 0, W, H, th$section, "bg"))
    d <- c(d, switch(theme,
      divertido = list(shape(W - 220, -60, 280, 280, "#236CDB", "deco", 140),
                       shape(60, 380, 90, 90, "#FFFFFF", "deco", 45)),
      academico = list(shape(46, 215, 8, 100, "#236CDB", "rule")),
      profesional = list(shape(70, 330, 120, 4, "#FFFFFF", "rule")),
      list()))
    return(d)
  }
  if (kind == "blanca") return(list())
  # contenido y dos columnas: franja o línea de título
  switch(theme,
    clasico = list(shape(0, 0, W, 90, th$band, "band")),
    claro = list(shape(0, 0, W, 90, th$band, "band"), shape(0, 86, W, 4, "#236CDB", "rule")),
    profesional = list(shape(48, 80, 90, 4, "#236CDB", "rule")),
    divertido = list(shape(24, 14, W - 48, 72, th$band, "band", 36),
                     shape(W - 46, 480, 26, 26, "#FF7F0E", "deco", 13),
                     shape(W - 80, 494, 16, 16, "#2CA02C", "deco", 8)),
    academico = list(shape(48, 84, W - 96, 2, "#236CDB", "rule")),
    list())
}

# Color de un texto según su papel, la clase de página y el estilo.
theme_text_color <- function(theme, kind, role, type = "slides") {
  th <- theme_get(theme)
  if (type == "report") {
    if (kind == "portada" && role %in% c("title", "subtitle")) {
      if (theme == "academico") return(if (role == "title") th$rep_cover_text else th$cover_sub)
      return(th$rep_cover_text)
    }
    if (role %in% c("title", "heading")) return(th$rep_heading)
    return(th$rep_text)
  }
  if (kind == "portada") return(switch(role, title = th$cover_text, heading = th$cover_text, th$cover_sub))
  if (kind == "seccion") return(th$section_text)
  if (role == "title") return(th$title)
  if (role == "heading") return(th$heading)
  th$text
}

theme_page_bg <- function(theme, kind, type = "slides") {
  th <- theme_get(theme)
  if (type == "report") return(th$rep_page)
  switch(kind, portada = th$cover, seccion = th$section, th$page)
}

txt <- function(x, y, w, h, text, role, size, bold = FALSE, align = "left") {
  el("text", x, y, w, h, text = text, role = role, style = txt_style(size, bold, "#000000", align))
}

# Plantillas de página (contenido); el aspecto lo pone el estilo.
page_template <- function(template, type = "slides", title = "Título", theme = "clasico") {
  if (type == "report") {
    kind <- if (template %in% c("portada", "blanca")) template else "apartado"
    els <- switch(kind,
      portada = list(
        txt(50, 110, 495, 70, title, "title", 30, TRUE),
        txt(50, 190, 495, 40, "Subtítulo o autoría", "subtitle", 15),
        txt(50, 290, 495, 60, "Escribe aquí la introducción del informe.", "body", 12)),
      blanca = list(),
      list(txt(50, 50, 495, 40, "Apartado", "heading", 20, TRUE),
           txt(50, 105, 495, 60, "Escribe aquí el texto del apartado.", "body", 12)))
  } else {
    kind <- if (template %in% c("portada", "dos_columnas", "seccion", "blanca")) template else "contenido"
    ty <- if (theme == "divertido") 22 else 18
    els <- switch(kind,
      portada = list(
        txt(70, 170, 820, 110, title, "title", 48, TRUE),
        txt(70, 305, 820, 50, "Subtítulo · autoría · fecha", "subtitle", 22)),       # la marca de CARING va aparte (fija)
      dos_columnas = list(
        txt(48, ty, 864, 56, "Título de la diapositiva", "title", 30, TRUE),
        txt(48, 120, 420, 370, "Texto de la columna izquierda.", "body", 18),
        txt(492, 120, 420, 370, "Texto de la columna derecha.", "body", 18)),
      blanca = list(),
      seccion = list(txt(70, 220, 820, 90, "Sección", "title", 44, TRUE)),
      list(
        txt(48, ty, 864, 56, "Título de la diapositiva", "title", 30, TRUE),
        txt(48, 110, 864, 50, "Escribe aquí el contenido.", "body", 20)))
  }
  page <- list(id = substr(gsub("-", "", new_id()), 1, 10), kind = kind, elements = els)
  theme_page(page, theme, type)
}

# Aplica un estilo a una página: rehace el decorado y recolorea textos y resultados.
theme_page <- function(page, theme, type = "slides") {
  th <- theme_get(theme)
  kind <- page$kind %||% guess_kind(page, type)
  page$kind <- kind
  sz <- page_size(type)
  keep <- Filter(function(e) !(identical(e$kind, "shape") && (isTRUE(e$role %in% decor_roles) ||
                                                              (is.null(e$role) && is_legacy_decor(e, sz)))),
                 page$elements)
  keep <- lapply(keep, function(e) {
    if (identical(e$kind, "text")) {
      role <- e$role %||% guess_text_role(e, kind)
      e$role <- role
      e$style$color <- theme_text_color(theme, kind, role, type)
      if (role == "title" && type == "slides" && kind %in% c("contenido", "dos_columnas") && e$y < 40) {
        e$y <- if (theme == "divertido") 22 else 18
      }
    } else if (identical(e$kind, "result")) {
      e$style <- compact(list(bg = th$result_bg, radius = if (th$radius > 0) th$radius))
      if (!length(e$style)) e$style <- NULL
    }
    e
  })
  page$elements <- c(theme_decor(theme, kind, type), keep)
  page$bg <- theme_page_bg(theme, kind, type)
  page
}

apply_theme <- function(doc, theme) {
  if (is.null(caring_themes[[theme]])) stop("Estilo desconocido: ", theme)
  doc$theme <- theme
  doc$pages <- lapply(doc$pages, theme_page, theme = theme, type = doc$type)
  doc
}

# Documentos creados antes de los estilos: se deduce la clase de página y el papel de los textos.
is_legacy_decor <- function(e, sz) {
  (e$x <= 0 && e$y <= 0 && e$w >= sz[["w"]] && e$h >= sz[["h"]]) ||
    (e$x <= 0 && e$y <= 0 && e$w >= sz[["w"]] && e$h <= 300)
}

guess_kind <- function(page, type) {
  sz <- page_size(type)
  full <- Filter(function(e) identical(e$kind, "shape") && e$w >= sz[["w"]] && e$h >= sz[["h"]], page$elements)
  band <- Filter(function(e) identical(e$kind, "shape") && e$y <= 0 && e$w >= sz[["w"]] && e$h < sz[["h"]], page$elements)
  if (type == "report") return(if (length(band)) "portada" else "apartado")
  if (length(full)) {
    big <- Filter(function(e) identical(e$kind, "text") && isTRUE(as.numeric(e$style$size %||% 0) >= 44), page$elements)
    n_txt <- length(Filter(function(e) identical(e$kind, "text"), page$elements))
    return(if (n_txt <= 1 && length(big)) "seccion" else "portada")
  }
  if (length(band)) return("contenido")
  "blanca"
}

guess_text_role <- function(e, kind) {
  size <- as.numeric(e$style$size %||% 18)
  if (kind == "portada") return(if (size >= 30) "title" else if (e$y > 400) "foot" else "subtitle")
  if (kind == "seccion") return("title")
  if (e$y < 90 && size >= 24) return("title")
  if (isTRUE(e$style$bold) && size >= 20) return("heading")
  "body"
}

# Lo que necesita el editor (JavaScript) para crear elementos nuevos con el estilo.
theme_for_editor <- function(theme, type = "slides") {
  th <- theme_get(theme)
  list(id = theme, text = if (type == "report") th$rep_text else th$text,
       heading = if (type == "report") th$rep_heading else th$heading,
       accent = th$accent, resultBg = th$result_bg, radius = th$radius)
}

# Vista previa en miniatura (portada + página de contenido) para la galería de estilos.
theme_preview_html <- function(theme, type = "slides") {
  th <- theme_get(theme)
  sz <- page_size(type)
  pages <- if (type == "report") {
    list(page_template("portada", "report", "Informe de ejemplo", theme), page_template("apartado", "report", theme = theme))
  } else {
    list(page_template("portada", "slides", "Encuesta 2026", theme), page_template("contenido", "slides", theme = theme))
  }
  # Un gráfico de ejemplo en la página de contenido, con el estilo de los resultados
  bx <- if (type == "report") c(50, 180, 495, 300) else c(300, 170, 600, 320)
  chart <- list(el("shape", bx[1], bx[2], bx[3], bx[4], style = list(bg = th$result_bg %||% "transparent", radius = th$radius)))
  hs <- c(.55, .8, .4, .95, .65)
  bw <- bx[3] / 11
  for (i in seq_along(hs)) {
    h <- (bx[4] - 40) * hs[i]
    chart <- c(chart, list(el("shape", bx[1] + bw * (2 * i - 1), bx[2] + bx[4] - 20 - h, bw, h,
                              style = list(bg = if (i %% 2) "#236CDB" else "#FF7F0E", radius = min(th$radius, 6)))))
  }
  pages[[2]]$elements <- c(pages[[2]]$elements, chart)
  scale <- if (type == "report") 0.2 else 0.18
  one <- function(p) {
    els <- vapply(p$elements, function(e) {
      st <- sprintf("position:absolute;left:%spx;top:%spx;width:%spx;height:%spx;", e$x, e$y, e$w, e$h)
      if (identical(e$kind, "shape")) {
        sprintf('<div style="%sbackground:%s;border-radius:%spx"></div>', st, css_color(e$style$bg, "transparent"), e$style$radius %||% 0)
      } else if (identical(e$kind, "text")) {
        s <- e$style
        sprintf('<div style="%sfont-size:%spx;font-weight:%s;color:%s;line-height:1.25;overflow:hidden">%s</div>',
                st, s$size, if (isTRUE(s$bold)) 700 else 400, css_color(s$color), htmltools::htmlEscape(e$text %||% ""))
      } else ""
    }, "")
    sprintf('<div style="position:relative;width:%dpx;height:%dpx;overflow:hidden;flex:none;box-shadow:0 1px 4px rgba(0,0,0,.2)"><div style="position:absolute;left:0;top:0;width:%dpx;height:%dpx;background:%s;transform:scale(%s);transform-origin:0 0;font-family:Roboto,Arial,sans-serif">%s</div></div>',
            round(sz[["w"]] * scale), round(sz[["h"]] * scale), sz[["w"]], sz[["h"]], css_color(p$bg, "#FFFFFF"), scale,
            paste(els, collapse = ""))
  }
  paste0('<div style="display:flex;gap:8px;justify-content:center">', paste(vapply(pages, one, ""), collapse = ""), "</div>")
}
