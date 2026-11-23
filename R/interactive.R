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
  p <- r$plot
  tiene_hover <- any(vapply(p$layers, function(l) "text" %in% names(l$mapping), logical(1)))
  # Las etiquetas de valor (geom_text) no se colocan bien en plotly: el valor sale al pasar el ratón.
  if (tiene_hover) p$layers <- Filter(function(l) !inherits(l$geom, "GeomText"), p$layers)
  w <- suppressWarnings(suppressMessages(plotly::ggplotly(p, tooltip = if (tiene_hover) "text" else "all")))
  plotly_caring(w)
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
