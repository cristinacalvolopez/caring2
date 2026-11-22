# Tipos de gráfico de Explorar ----------------------------------------------------------------
# Cada combinación de variables tiene su propia lista de gráficos (chart_choices) y una
# función que dibuja el elegido. El primero de cada lista es el que se usa por defecto.

chart_choices <- function(kind) {
  switch(kind,
    cat = c("Barras horizontales" = "barras_h", "Barras verticales" = "barras_v",
            "Sectores" = "sectores", "Anillo" = "anillo", "Puntos (lollipop)" = "puntos"),
    num = c("Histograma" = "histograma", "Histograma con densidad" = "hist_dens", "Densidad" = "densidad",
            "Caja y bigotes" = "caja", "Violín" = "violin"),
    catcat = c("Barras agrupadas" = "agrupadas", "Barras apiladas" = "apiladas", "Mapa de calor" = "calor",
               "Burbujas" = "burbujas", "Mosaico" = "mosaico"),
    numcat = c("Caja y bigotes" = "caja", "Violín" = "violin", "Medias con intervalo de confianza" = "medias",
               "Densidades superpuestas" = "densidades", "Puntos" = "puntos"),
    numnum = c("Dispersión con recta" = "recta", "Dispersión con curva suavizada" = "curva",
               "Densidad 2D (cuadrícula)" = "densidad2d", "Medias por tramos" = "tramos"),
    mr = c("Barras horizontales" = "barras_h", "Barras verticales" = "barras_v", "Puntos (lollipop)" = "puntos"),
    mrcat = c("Barras agrupadas" = "agrupadas", "Mapa de calor" = "calor", "Burbujas" = "burbujas"),
    character())
}

# Qué combinación forman una o dos variables.
chart_kind <- function(d, v1, v2 = NULL) {
  es_mr <- function(v) !is.null(v) && startsWith(v, "mr:")
  if (es_mr(v1) || es_mr(v2)) {
    if (is.null(v2)) return("mr")
    otra <- if (es_mr(v1)) v2 else v1
    if (es_mr(otra)) return("texto")
    return(if (is.factor(d[[otra]])) "mrcat" else "texto")
  }
  x <- d[[v1]]
  if (is.null(v2)) return(if (is.factor(x)) "cat" else if (is.numeric(x)) "num" else "texto")
  y <- d[[v2]]
  if (is.factor(x) && is.factor(y)) "catcat"
  else if (xor(is.factor(x), is.factor(y)) && (is.numeric(x) || is.numeric(y))) "numcat"
  else if (is.numeric(x) && is.numeric(y)) "numnum"
  else "texto"
}

pick_chart <- function(chart, kind) {
  ch <- chart_choices(kind)
  if (!is.null(chart) && chart %in% ch) chart else unname(ch[1])
}

grid_y <- function() ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "#E3E3E3"))
grid_x <- function() ggplot2::theme(panel.grid.major.x = ggplot2::element_line(colour = "#E3E3E3"),
                                    panel.grid.major.y = ggplot2::element_blank())

# Una variable categórica: pd con columnas cat (factor en el orden original), pct, n
plot_cat <- function(pd, chart) {
  pal <- rep(caring_palette, length.out = nrow(pd))
  switch(chart,
    barras_v = ggplot2::ggplot(pd, ggplot2::aes(x = .data$cat, y = .data$pct)) +
      ggplot2::geom_col(fill = caring_blue, width = 0.7) +
      ggplot2::geom_text(ggplot2::aes(label = fmt_pct(.data$pct)), vjust = -0.4, size = 3.6, colour = "#333333") +
      ggplot2::scale_y_continuous(labels = function(z) fmt_pct(z, 0), expand = ggplot2::expansion(c(0, 0.12))) +
      ggplot2::scale_x_discrete(labels = function(z) wrap_labels(z, 14)) +
      ggplot2::labs(x = NULL, y = "% de respuestas válidas") + theme_caring() + grid_y(),
    sectores = , anillo = {
      hole <- chart == "anillo"
      ggplot2::ggplot(pd, ggplot2::aes(x = if (hole) 2 else 1, y = .data$pct, fill = .data$cat)) +
        ggplot2::geom_col(width = 1, colour = "white") +
        ggplot2::geom_text(ggplot2::aes(label = ifelse(.data$pct >= 0.04, fmt_pct(.data$pct, 0), "")),
                           position = ggplot2::position_stack(vjust = 0.5), colour = "white", size = 3.8, fontface = "bold") +
        ggplot2::coord_polar(theta = "y", direction = -1) +
        (if (hole) ggplot2::xlim(c(0.4, 2.5))) +
        ggplot2::scale_fill_manual(values = pal, labels = function(z) wrap_labels(z, 24)) +
        ggplot2::theme_void(base_size = 13) +
        ggplot2::theme(legend.position = "right", legend.title = ggplot2::element_blank())
    },
    puntos = ggplot2::ggplot(pd, ggplot2::aes(x = .data$pct, y = forcats_rev(.data$cat))) +
      ggplot2::geom_segment(ggplot2::aes(x = 0, xend = .data$pct, yend = forcats_rev(.data$cat)), colour = "#CBDEFB", linewidth = 1.6) +
      ggplot2::geom_point(colour = caring_blue, size = 5) +
      ggplot2::geom_text(ggplot2::aes(label = fmt_pct(.data$pct)), hjust = -0.4, size = 3.6, colour = "#333333") +
      ggplot2::scale_x_continuous(labels = function(z) fmt_pct(z, 0), expand = ggplot2::expansion(c(0, 0.15)), limits = c(0, NA)) +
      ggplot2::scale_y_discrete(labels = function(z) wrap_labels(z, 28)) +
      ggplot2::labs(x = "% de respuestas válidas", y = NULL) + theme_caring() + grid_x(),
    # barras horizontales
    ggplot2::ggplot(pd, ggplot2::aes(x = .data$pct, y = forcats_rev(.data$cat))) +
      ggplot2::geom_col(fill = caring_blue, width = 0.7) +
      ggplot2::geom_text(ggplot2::aes(label = fmt_pct(.data$pct)), hjust = -0.1, size = 3.6, colour = "#333333") +
      ggplot2::scale_x_continuous(labels = function(z) fmt_pct(z, 0), expand = ggplot2::expansion(c(0, 0.15))) +
      ggplot2::scale_y_discrete(labels = function(z) wrap_labels(z, 28)) +
      ggplot2::labs(x = "% de respuestas válidas", y = NULL) + theme_caring() + grid_x()
  )
}

forcats_rev <- function(f) factor(f, levels = rev(levels(f)))

# Una variable numérica: pd con x, w; m = media; lab = etiqueta
plot_num <- function(pd, chart, m, lab) {
  bins <- min(30, max(10, ceiling(log2(nrow(pd)) + 1) * 2))
  media <- list(
    ggplot2::geom_vline(xintercept = m, colour = "#135DCD", linetype = "dashed"),
    ggplot2::annotate("text", x = m, y = Inf, label = paste("media", fmt_num(m, 1)),
                      vjust = 1.5, hjust = -0.05, colour = "#135DCD", size = 3.6))
  base <- ggplot2::ggplot(pd, ggplot2::aes(x = .data$x, weight = .data$w))
  switch(chart,
    hist_dens = base +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = bins, fill = "#CBDEFB", colour = "white") +
      ggplot2::geom_density(colour = caring_blue, linewidth = 1.1) + media +
      ggplot2::labs(x = lab, y = "Densidad") + theme_caring() + grid_y(),
    densidad = base +
      ggplot2::geom_density(fill = caring_blue, alpha = 0.35, colour = caring_blue, linewidth = 1) + media +
      ggplot2::labs(x = lab, y = "Densidad") + theme_caring() + grid_y(),
    caja = ggplot2::ggplot(pd, ggplot2::aes(x = .data$x, y = "", weight = .data$w)) +
      ggplot2::geom_boxplot(fill = "#CBDEFB", colour = "#135DCD", width = 0.4, outlier.colour = "#999999") +
      ggplot2::stat_summary(ggplot2::aes(group = 1), fun = mean, geom = "point", shape = 23, size = 3.5,
                            fill = "white", colour = "#000000", orientation = "y") +
      ggplot2::labs(x = lab, y = NULL, caption = "El rombo es la media") + theme_caring() + grid_x(),
    violin = ggplot2::ggplot(pd, ggplot2::aes(x = .data$x, y = "")) +
      ggplot2::geom_violin(fill = "#CBDEFB", colour = "#135DCD", orientation = "y") +
      ggplot2::geom_boxplot(width = 0.12, fill = "white", outlier.shape = NA) +
      ggplot2::labs(x = lab, y = NULL, caption = if (length(unique(pd$w)) > 1) "La forma del violín no tiene en cuenta la ponderación") +
      theme_caring() + grid_x(),
    # histograma
    base + ggplot2::geom_histogram(bins = bins, fill = caring_blue, colour = "white") + media +
      ggplot2::labs(x = lab, y = "Casos") + theme_caring() + grid_y()
  )
}

# Dos categóricas: pd con grupo, serie, pct; ct = tabla de frecuencias (filas a, columnas b)
plot_catcat <- function(pd, ct, chart, pct, xl, fl) {
  pal <- rep(caring_palette, length.out = nlevels(pd$serie))
  ylab <- switch(pct, col = paste("% dentro de", tolower(xl)), row = paste("% dentro de", tolower(xl)), "% del total")
  switch(chart,
    calor = {
      mx <- max(pd$pct)
      ggplot2::ggplot(pd, ggplot2::aes(x = .data$grupo, y = forcats_rev(.data$serie), fill = .data$pct)) +
        ggplot2::geom_tile(colour = "white", linewidth = 1) +
        ggplot2::geom_text(ggplot2::aes(label = fmt_pct(.data$pct, 0), colour = .data$pct > mx * 0.55), size = 3.8, show.legend = FALSE) +
        ggplot2::scale_colour_manual(values = c(`TRUE` = "#FFFFFF", `FALSE` = "#1A1A1A")) +
        ggplot2::scale_fill_gradient(low = "#EAF2FE", high = "#135DCD", labels = function(z) fmt_pct(z, 0), name = ylab) +
        ggplot2::scale_x_discrete(labels = function(z) wrap_labels(z, 14)) +
        ggplot2::scale_y_discrete(labels = function(z) wrap_labels(z, 22)) +
        ggplot2::labs(x = xl, y = fl) + theme_caring() +
        ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "right", legend.title = ggplot2::element_text(size = 10))
    },
    burbujas = ggplot2::ggplot(pd, ggplot2::aes(x = .data$grupo, y = forcats_rev(.data$serie), size = .data$pct, colour = .data$serie)) +
      ggplot2::geom_point(alpha = 0.85) +
      ggplot2::geom_text(ggplot2::aes(label = fmt_pct(.data$pct, 0)), size = 3.2, colour = "#1A1A1A", vjust = -1.6) +
      ggplot2::scale_size_area(max_size = 18, guide = "none") +
      ggplot2::scale_colour_manual(values = pal, guide = "none") +
      ggplot2::scale_x_discrete(labels = function(z) wrap_labels(z, 14)) +
      ggplot2::scale_y_discrete(labels = function(z) wrap_labels(z, 22), expand = ggplot2::expansion(add = 0.8)) +
      ggplot2::labs(x = xl, y = fl) + theme_caring() +
      ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "#F0F0F0")),
    mosaico = {
      # Ancho de cada columna = peso de su grupo; alto de cada bloque = % dentro del grupo
      m <- if (pct == "row") ct else t(ct)            # filas = grupos del eje X
      tot <- rowSums(m)
      x1 <- cumsum(tot) / sum(tot); x0 <- c(0, utils::head(x1, -1))
      rects <- do.call(rbind, lapply(seq_len(nrow(m)), function(i) {
        p <- unname(m[i, ] / tot[i]); y1 <- cumsum(p); y0 <- c(0, utils::head(y1, -1))
        data.frame(grupo = rownames(m)[i], serie = factor(colnames(m), levels = colnames(m)),
                   x0 = unname(x0[i]), x1 = unname(x1[i]), y0 = y0, y1 = y1, p = p, row.names = NULL)
      }))
      mid <- (x0 + x1) / 2
      ggplot2::ggplot(rects) +
        ggplot2::geom_rect(ggplot2::aes(xmin = .data$x0, xmax = .data$x1, ymin = .data$y0, ymax = .data$y1, fill = .data$serie),
                           colour = "white", linewidth = 1) +
        ggplot2::geom_text(ggplot2::aes(x = (.data$x0 + .data$x1) / 2, y = (.data$y0 + .data$y1) / 2,
                                        label = ifelse(.data$p >= 0.06 & (.data$x1 - .data$x0) >= 0.06, fmt_pct(.data$p, 0), "")),
                           colour = "white", size = 3.4, fontface = "bold") +
        ggplot2::scale_fill_manual(values = pal) +
        ggplot2::scale_x_continuous(breaks = mid, labels = wrap_labels(rownames(m), 14), expand = c(0, 0)) +
        ggplot2::scale_y_continuous(labels = function(z) fmt_pct(z, 0), expand = c(0, 0)) +
        ggplot2::labs(x = paste(xl, "(ancho = peso de cada grupo)"), y = paste("% dentro de", tolower(xl)), fill = fl) +
        theme_caring() + ggplot2::theme(panel.grid = ggplot2::element_blank())
    },
    {
      apil <- chart == "apiladas"
      ggplot2::ggplot(pd, ggplot2::aes(x = .data$grupo, y = .data$pct, fill = .data$serie)) +
        ggplot2::geom_col(position = if (apil) "stack" else ggplot2::position_dodge(width = 0.8),
                          width = if (apil) 0.7 else 0.75) +
        (if (apil || nlevels(pd$serie) <= 4)
          ggplot2::geom_text(ggplot2::aes(label = ifelse(.data$pct >= 0.04, fmt_pct(.data$pct, 0), "")),
            position = if (apil) ggplot2::position_stack(vjust = 0.5) else ggplot2::position_dodge(width = 0.8),
            vjust = if (apil) 0.5 else -0.4, size = 3.3, colour = if (apil) "#FFFFFF" else "#333333")) +
        ggplot2::scale_fill_manual(values = pal, labels = function(z) wrap_labels(z, 34)) +
        ggplot2::scale_y_continuous(labels = function(z) fmt_pct(z, 0), expand = ggplot2::expansion(c(0, 0.05))) +
        ggplot2::scale_x_discrete(labels = function(z) wrap_labels(z, 14)) +
        ggplot2::labs(x = xl, y = ylab, fill = fl) + theme_caring() + grid_y() +
        # leyenda en varias columnas cuando las etiquetas son largas, para que no se corte
        (if (sum(nchar(levels(pd$serie))) > 60)
          list(ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2, byrow = TRUE)),
               ggplot2::theme(legend.position = "bottom")))
    })
}

# Numérica según categórica: pd con x (numérica), g (grupo), w
plot_numcat <- function(pd, chart, xlab_g, ylab_n) {
  pal <- rep(caring_palette, length.out = nlevels(pd$g))
  base <- ggplot2::ggplot(pd, ggplot2::aes(x = .data$g, y = .data$x, fill = .data$g))
  ejes <- list(ggplot2::scale_fill_manual(values = pal), ggplot2::scale_colour_manual(values = pal),
               ggplot2::scale_x_discrete(labels = function(z) wrap_labels(z, 14)),
               ggplot2::labs(x = xlab_g, y = ylab_n), theme_caring(), grid_y())
  media <- ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", colour = "#000000")
  switch(chart,
    violin = base + ggplot2::geom_violin(alpha = 0.8, show.legend = FALSE, colour = NA) +
      ggplot2::geom_boxplot(width = 0.12, fill = "white", outlier.shape = NA) + media + ejes,
    medias = {
      s <- do.call(rbind, lapply(levels(pd$g), function(l) {
        x <- pd$x[pd$g == l]; w <- pd$w[pd$g == l]
        m <- sum(w * x) / sum(w); se <- wsd(x, w) / sqrt(length(x))
        data.frame(g = factor(l, levels = levels(pd$g)), m = m, lo = m - 1.96 * se, hi = m + 1.96 * se)
      }))
      ggplot2::ggplot(s, ggplot2::aes(x = .data$g, y = .data$m, colour = .data$g)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lo, ymax = .data$hi), width = 0.2, linewidth = 1, show.legend = FALSE) +
        ggplot2::geom_point(size = 4, show.legend = FALSE) +
        ggplot2::geom_text(ggplot2::aes(label = fmt_num(.data$m, 1)), hjust = -0.5, size = 3.6, colour = "#333333") +
        ejes + ggplot2::labs(y = paste("Media de", tolower(ylab_n)), caption = "Barras: intervalo de confianza del 95 %")
    },
    densidades = ggplot2::ggplot(pd, ggplot2::aes(x = .data$x, weight = .data$w, fill = .data$g, colour = .data$g)) +
      ggplot2::geom_density(alpha = 0.25, linewidth = 1) +
      ggplot2::scale_fill_manual(values = pal) + ggplot2::scale_colour_manual(values = pal) +
      ggplot2::labs(x = ylab_n, y = "Densidad", fill = xlab_g, colour = xlab_g) + theme_caring() + grid_y(),
    puntos = {
      if (nrow(pd) > 4000) pd <- pd[sample(nrow(pd), 4000), ]
      ggplot2::ggplot(pd, ggplot2::aes(x = .data$g, y = .data$x, colour = .data$g)) +
        ggplot2::geom_jitter(width = 0.25, height = 0, alpha = 0.45, size = 1.6, show.legend = FALSE) +
        ggplot2::stat_summary(fun = mean, geom = "crossbar", width = 0.5, colour = "#000000", linewidth = 0.4) +
        ejes + ggplot2::labs(caption = "La barra negra es la media")
    },
    base + ggplot2::geom_boxplot(width = 0.55, alpha = 0.85, outlier.colour = "#999999", show.legend = FALSE) +
      media + ejes + ggplot2::labs(caption = "El rombo es la media")
  )
}

# Dos numéricas: pd con x, y
plot_numnum <- function(pd, chart, xl, yl) {
  if (nrow(pd) > 3000 && chart != "densidad2d") pd <- pd[sample(nrow(pd), 3000), ]
  puntos <- ggplot2::geom_point(colour = caring_blue, alpha = 0.4, size = 1.8)
  fin <- list(ggplot2::labs(x = xl, y = yl), theme_caring(),
              ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "#E3E3E3")))
  switch(chart,
    curva = ggplot2::ggplot(pd, ggplot2::aes(x = .data$x, y = .data$y)) + puntos +
      ggplot2::geom_smooth(method = "loess", formula = y ~ x, colour = "#FF7F0E", fill = "#FFE3C7", se = TRUE) + fin,
    densidad2d = ggplot2::ggplot(pd, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_bin2d(bins = 30) +
      ggplot2::scale_fill_gradient(low = "#EAF2FE", high = "#135DCD", name = "Casos") + fin +
      ggplot2::theme(legend.position = "right"),
    tramos = {
      br <- unique(stats::quantile(pd$x, probs = seq(0, 1, 0.1), na.rm = TRUE))
      pd$t <- cut(pd$x, br, include.lowest = TRUE)
      s <- do.call(rbind, lapply(split(pd, pd$t), function(z) {
        if (!nrow(z)) return(NULL)
        se <- stats::sd(z$y) / sqrt(nrow(z))
        data.frame(x = stats::median(z$x), m = mean(z$y), lo = mean(z$y) - 1.96 * se, hi = mean(z$y) + 1.96 * se)
      }))
      ggplot2::ggplot(s, ggplot2::aes(x = .data$x, y = .data$m)) +
        ggplot2::geom_line(colour = "#135DCD", linewidth = 1) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lo, ymax = .data$hi), width = 0, colour = "#2F7BEE") +
        ggplot2::geom_point(colour = caring_blue, size = 3.5) + fin +
        ggplot2::labs(y = paste("Media de", tolower(yl)), caption = "Puntos: media en cada décimo de la variable horizontal (IC 95 %)")
    },
    ggplot2::ggplot(pd, ggplot2::aes(x = .data$x, y = .data$y)) + puntos +
      ggplot2::geom_smooth(method = "lm", formula = y ~ x, colour = "#135DCD", se = TRUE, fill = "#CBDEFB") + fin
  )
}
