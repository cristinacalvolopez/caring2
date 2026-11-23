# Gráficos interactivos ------------------------------------------------------------------------
# Versión interactiva (plotly) de los gráficos de Explorar, con la misma estética que las
# redes de netCoin: fuente Roboto, fondo blanco, paleta d3 y textos en español.

plotly_caring <- function(w, filename = "grafico") {
  w <- suppressWarnings(plotly::plotly_build(w))
  w$x$layout <- set_font_family(w$x$layout)
  w$x$data <- lapply(w$x$data, set_font_family)
  w <- plotly::layout(w,
    font = list(family = caring_font, size = 13, color = "#333333"),
    paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF",
    hoverlabel = list(bgcolor = "#FFFFFF", bordercolor = caring_blue,
                      font = list(family = caring_font, size = 13, color = "#1A1A1A")),
    margin = list(t = 40, r = 20, b = 60, l = 60))
  # ejes con texto algo más pequeño para que no se monten las categorías
  for (k in grep("^[xy]axis[0-9]*$", names(w$x$layout), value = TRUE)) {
    w$x$layout[[k]]$tickfont <- utils::modifyList(w$x$layout[[k]]$tickfont %||% list(), list(size = 11, family = caring_font))
  }
  plotly::config(w, locale = "es", displaylogo = FALSE, responsive = TRUE,
    modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "toggleSpikelines",
                               "hoverCompareCartesian", "hoverClosestCartesian"),
    toImageButtonOptions = list(format = "png", filename = filename, scale = 2))
}

# r: resultado de explore_*(). Devuelve un htmlwidget o NULL si no hay gráfico.
to_plotly <- function(r) {
  if (is.null(r$plot)) return(NULL)
  if (isTRUE(r$chart %in% c("sectores", "anillo")) && !is.null(r$pd)) {
    pd <- r$pd
    w <- plotly::plot_ly(pd, labels = ~cat, values = ~pct, type = "pie", sort = FALSE, direction = "clockwise",
                         hole = if (r$chart == "anillo") 0.5 else 0,
                         marker = list(colors = rep(caring_palette, length.out = nrow(pd)), line = list(color = "#FFFFFF", width = 2)),
                         text = fmt_pct(pd$pct / sum(pd$pct), 0), customdata = fmt_pct(pd$pct / sum(pd$pct)),
                         textinfo = "text", texttemplate = "%{text}",
                         hovertemplate = "%{label}<br><b>%{customdata}</b><extra></extra>")
    return(plotly_caring(w))
  }
  # Barras de una variable: se dibujan directamente en plotly (ggplotly gira las horizontales)
  if (isTRUE(r$chart %in% c("barras_h", "barras_v")) && !is.null(r$pd)) {
    pd <- r$pd
    h <- r$chart == "barras_h"
    hover <- paste0(pd$cat, "<br><b>", fmt_pct(pd$pct), "</b>", if (!is.null(pd$n)) paste0(" · n = ", fmt_int(round(pd$n))) else "")
    marcas <- pretty(c(0, max(pd$pct, na.rm = TRUE)))
    eje_pct <- list(title = "% de respuestas válidas", tickvals = marcas, ticktext = fmt_pct(marcas, 0),
                    range = c(0, max(pd$pct, na.rm = TRUE) * 1.12), zeroline = FALSE, gridcolor = "#E3E3E3")
    eje_cat <- list(title = "", categoryorder = "array", categoryarray = if (h) rev(levels(pd$cat)) else levels(pd$cat), automargin = TRUE)
    w <- plotly::plot_ly(pd, type = "bar", orientation = if (h) "h" else "v",
                         x = if (h) ~pct else ~cat, y = if (h) ~cat else ~pct,
                         marker = list(color = caring_blue), text = fmt_pct(pd$pct), textposition = "outside",
                         cliponaxis = FALSE, textfont = list(size = 12, color = "#333333"),
                         hovertext = hover, hoverinfo = "text")
    w <- plotly::layout(w, xaxis = if (h) eje_pct else eje_cat, yaxis = if (h) eje_cat else eje_pct, bargap = 0.3)
    return(plotly_caring(w))
  }
  p <- r$plot
  tiene_hover <- any(vapply(p$layers, function(l) "text" %in% names(l$mapping), logical(1)))
  # Las etiquetas de valor (geom_text) no se colocan bien en plotly: el valor sale al pasar el ratón.
  if (tiene_hover) p$layers <- Filter(function(l) !inherits(l$geom, "GeomText"), p$layers)
  w <- suppressWarnings(suppressMessages(plotly::ggplotly(p, tooltip = if (tiene_hover) "text" else "all")))
  if (tiene_hover) w <- bar_value_labels(w, apiladas = identical(r$chart, "apiladas"))
  else w <- clean_hover(w, p$labels$x, p$labels$y)
  plotly_caring(w)
}

# Textos de ayuda automáticos de ggplotly en español y sin variables internas.
clean_hover <- function(w, xlab = NULL, ylab = NULL) {
  w <- suppressWarnings(plotly::plotly_build(w))
  nombres <- c(count = "Casos", density = "Densidad", x = xlab %||% "x", y = ylab %||% "y", g = "Grupo",
               fill = "Grupo", colour = "Grupo", level = "Nivel", m = "Media", lo = "IC inferior", hi = "IC superior",
               ymin = "Mínimo", ymax = "Máximo", lower = "Percentil 25", upper = "Percentil 75", middle = "Mediana",
               ncount = "Proporción", ndensity = "Densidad", value = "Valor")
  arregla <- function(t) {
    if (is.null(t) || !is.character(t)) return(t)
    vapply(t, function(s) {
      lineas <- strsplit(s, "<br />|<br>")[[1]]
      lineas <- lineas[!grepl("^\\s*(w|weight|group|PANEL|\\.data\\$w|text)\\s*:", lineas)]
      lineas <- vapply(lineas, function(l) {
        m <- regmatches(l, regexec("^\\s*([^:]+):\\s*(.*)$", l))[[1]]
        if (length(m) < 3) return(l)
        k <- trimws(m[2]); v <- trimws(m[3])
        k <- if (k %in% names(nombres)) nombres[[k]] else sub("^\\.data\\$", "", k)
        num <- suppressWarnings(as.numeric(v))
        if (!is.na(num) && is.finite(num)) v <- if (abs(num - round(num)) < 1e-9) fmt_int(num) else fmt_num(num, 2)
        paste0(k, ": ", v)
      }, "")
      paste(lineas, collapse = "<br>")
    }, "", USE.NAMES = FALSE)
  }
  w$x$data <- lapply(w$x$data, function(tr) { tr$text <- arregla(tr$text); tr$hovertext <- arregla(tr$hovertext); tr })
  w
}

# Escribe el porcentaje en cada barra (dentro de plotly, para que quede en su sitio).
bar_value_labels <- function(w, apiladas = FALSE) {
  w <- suppressWarnings(plotly::plotly_build(w))
  w$x$data <- lapply(w$x$data, function(tr) {
    if (!identical(tr$type, "bar") || is.null(tr$text) || !any(grepl("%", tr$text))) return(tr)
    vals <- if (identical(tr$orientation, "h")) tr$x else tr$y
    if (!is.numeric(vals) || any(vals > 1.0001, na.rm = TRUE) || any(abs(as.numeric(tr$base %||% 0)) > 1.0001)) return(tr)
    tr$hovertext <- tr$text
    tr$hoverinfo <- "text"
    tr$text <- ifelse(vals >= 0.04 | !apiladas, fmt_pct(vals, 0), "")
    tr$textposition <- if (apiladas) "inside" else "outside"
    tr$insidetextanchor <- "middle"
    tr$cliponaxis <- FALSE
    tr$textfont <- list(family = caring_font, size = 12, color = if (apiladas) "#FFFFFF" else "#333333")
    tr
  })
  w
}

caring_font <- "Roboto, Helvetica, Arial, sans-serif"

# ggplotly copia la fuente de cada texto del gráfico («sans», que algunos navegadores
# muestran con serifa): se cambia en todos los elementos por la de CARING.
set_font_family <- function(x) {
  if (!is.list(x)) return(x)
  for (k in seq_along(x)) {
    nm <- names(x)[k]
    if (!is.null(nm) && identical(nm, "family")) x[[k]] <- caring_font
    else if (is.list(x[[k]])) x[[k]] <- set_font_family(x[[k]])
  }
  x
}

# Guarda el gráfico interactivo como un único archivo HTML (sin carpetas aparte).
save_plotly_html <- function(w, file, title = "Gráfico") {
  tmp <- tempfile("pl-"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  w <- htmlwidgets::prependContent(w, htmltools::tags$style("body{margin:0;font-family:Roboto,Helvetica,Arial,sans-serif}"))
  suppressMessages(htmlwidgets::saveWidget(w, file.path(tmp, "index.html"), selfcontained = FALSE, libdir = "lib", title = title))
  writeLines(inline_html(tmp), file, useBytes = TRUE)
  invisible(file)
}
