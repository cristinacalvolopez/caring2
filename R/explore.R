# Análisis exploratorio --------------------------------------------------------------
# Cada función devuelve una lista con:
#   title, subtitle, table (data.frame ya formateado), stats (texto), plot (ggplot)

caring_blue <- "#2F7BEE"
caring_palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
                    "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")

theme_caring <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(colour = "#333333"),
      axis.text = ggplot2::element_text(colour = "#333333"),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.title.position = "plot"
    )
}

weights_of <- function(d, weight) {
  if (is.null(weight) || !nzchar(weight)) return(rep(1, nrow(d)))
  w <- d[[weight]]
  w[is.na(w)] <- 0
  w
}

wq <- function(x, w, probs) {
  ok <- !is.na(x) & w > 0
  x <- x[ok]; w <- w[ok]
  if (!length(x)) return(rep(NA_real_, length(probs)))
  o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w)
  vapply(probs, function(p) x[which(cw >= p)[1]], numeric(1))
}

wsd <- function(x, w) {
  ok <- !is.na(x) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) < 2) return(NA_real_)
  m <- sum(w * x) / sum(w)
  sqrt(sum(w * (x - m)^2) / (sum(w) - sum(w^2) / sum(w)))
}

fmt_p <- function(p) {
  if (is.na(p)) return("—")
  if (p < 0.001) "< 0,001" else fmt_num(p, 3)
}

# "p = 0,032" o "p < 0,001"
p_txt <- function(p) {
  f <- fmt_p(p)
  if (startsWith(f, "<")) paste("p", f) else paste("p =", f)
}

strength <- function(v) {
  v <- abs(v)
  if (is.na(v)) "" else if (v < 0.1) "muy débil" else if (v < 0.3) "débil" else if (v < 0.5) "moderada" else "fuerte"
}

signif_text <- function(p) {
  if (is.na(p)) "" else if (p < 0.05) "estadísticamente significativa (p < 0,05)" else "no significativa (p ≥ 0,05)"
}

wrap_labels <- function(x, width = 18) {
  vapply(x, function(s) paste(strwrap(s, width), collapse = "\n"), character(1))
}

# Una variable ----------------------------------------------------------------------

explore_one <- function(d, v, weight = NULL) {
  need_var(d, v)
  x <- d[[v]]
  w <- weights_of(d, weight)
  lab <- var_label(d, v)
  wtxt <- if (!is.null(weight) && nzchar(weight)) " · ponderado" else ""
  if (is.factor(x)) {
    lv <- levels(x)
    n <- vapply(lv, function(l) sum(w[!is.na(x) & x == l]), numeric(1))
    nas <- sum(w[is.na(x)])
    tot <- sum(n) + nas
    tab <- data.frame(Categoría = lv, n = round(n, 1), `%` = fmt_pct(n / tot),
                      `% válido` = fmt_pct(n / sum(n)), check.names = FALSE)
    if (nas > 0) tab <- rbind(tab, data.frame(Categoría = "Perdidos", n = round(nas, 1), `%` = fmt_pct(nas / tot),
                                              `% válido` = "", check.names = FALSE))
    tab <- rbind(tab, data.frame(Categoría = "Total", n = round(tot, 1), `%` = "100,0 %",
                                 `% válido` = "", check.names = FALSE))
    pd <- data.frame(cat = factor(lv, levels = rev(lv)), pct = n / sum(n), n = n)
    p <- ggplot2::ggplot(pd, ggplot2::aes(x = .data$pct, y = .data$cat)) +
      ggplot2::geom_col(fill = caring_blue, width = 0.7) +
      ggplot2::geom_text(ggplot2::aes(label = fmt_pct(.data$pct)), hjust = -0.1, size = 3.6, colour = "#333333") +
      ggplot2::scale_x_continuous(labels = function(z) fmt_pct(z, 0), expand = ggplot2::expansion(c(0, 0.15))) +
      ggplot2::scale_y_discrete(labels = function(z) wrap_labels(z, 28)) +
      ggplot2::labs(x = "% de respuestas válidas", y = NULL) +
      theme_caring() + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                                      panel.grid.major.x = ggplot2::element_line(colour = "#E3E3E3"))
    list(title = lab, subtitle = paste0("Frecuencias · n = ", fmt_int(round(sum(n))), wtxt),
         table = tab, stats = paste0("Categoría más frecuente: ", lv[which.max(n)], " (", fmt_pct(max(n) / sum(n)), ")."),
         plot = p)
  } else if (is.numeric(x)) {
    ok <- !is.na(x)
    m <- sum(w[ok] * x[ok]) / sum(w[ok])
    qs <- wq(x, w, c(0.25, 0.5, 0.75))
    tab <- data.frame(
      Estadístico = c("Casos válidos", "Perdidos", "Media", "Mediana", "Desviación típica",
                      "Mínimo", "Percentil 25", "Percentil 75", "Máximo"),
      Valor = c(fmt_int(sum(ok)), fmt_int(sum(!ok)), fmt_num(m), fmt_num(qs[2]), fmt_num(wsd(x, w)),
                fmt_num(min(x, na.rm = TRUE)), fmt_num(qs[1]), fmt_num(qs[3]), fmt_num(max(x, na.rm = TRUE))))
    pd <- data.frame(x = x[ok], w = w[ok])
    p <- ggplot2::ggplot(pd, ggplot2::aes(x = .data$x, weight = .data$w)) +
      ggplot2::geom_histogram(bins = min(30, max(10, ceiling(log2(nrow(pd)) + 1) * 2)),
                              fill = caring_blue, colour = "white") +
      ggplot2::geom_vline(xintercept = m, colour = "#135DCD", linetype = "dashed") +
      ggplot2::annotate("text", x = m, y = Inf, label = paste("media", fmt_num(m, 1)),
                        vjust = 1.5, hjust = -0.05, colour = "#135DCD", size = 3.6) +
      ggplot2::labs(x = lab, y = "Casos") + theme_caring() +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "#E3E3E3"))
    list(title = lab, subtitle = paste0("Distribución · n = ", fmt_int(sum(ok)), wtxt),
         table = tab, stats = paste0("Media ", fmt_num(m), "; la mitad de los casos está entre ",
                                     fmt_num(qs[1]), " y ", fmt_num(qs[3]), "."),
         plot = p)
  } else {
    tb <- sort(table(x), decreasing = TRUE)
    tab <- data.frame(Valor = names(tb), n = as.integer(tb), check.names = FALSE)
    tab <- utils::head(tab, 30)
    list(title = lab, subtitle = paste0("Variable de texto · ", fmt_int(length(tb)), " valores distintos"),
         table = tab, stats = "Las variables de texto no tienen gráfico. Si son categorías, cambia su tipo a categórica en el paso Datos.",
         plot = NULL)
  }
}

# Dos variables -------------------------------------------------------------------------

explore_two <- function(d, v1, v2, weight = NULL, pct = "col", chart = "agrupadas") {
  need_var(d, v1); need_var(d, v2)
  x <- d[[v1]]; y <- d[[v2]]
  if (is.factor(x) && is.factor(y)) return(explore_catcat(d, v1, v2, weight, pct, chart))
  if (is.numeric(x) && is.factor(y)) return(explore_numcat(d, v1, v2, weight))
  if (is.factor(x) && is.numeric(y)) return(explore_numcat(d, v2, v1, weight))
  if (is.numeric(x) && is.numeric(y)) return(explore_numnum(d, v1, v2, weight))
  stop("No se puede cruzar una variable de texto. Cambia su tipo en el paso Datos.")
}

explore_catcat <- function(d, v1, v2, weight, pct, chart) {
  w <- weights_of(d, weight)
  ok <- !is.na(d[[v1]]) & !is.na(d[[v2]])
  dd <- data.frame(a = droplevels(d[[v1]][ok]), b = droplevels(d[[v2]][ok]), w = w[ok])
  ct <- stats::xtabs(w ~ a + b, data = dd)
  n <- sum(ct)
  prop <- switch(pct,
    col = sweep(ct, 2, colSums(ct), "/"),
    row = sweep(ct, 1, rowSums(ct), "/"),
    ct / n)
  tab <- as.data.frame.matrix(matrix(fmt_pct(prop), nrow = nrow(ct), dimnames = dimnames(ct)))
  tab <- cbind(stats::setNames(data.frame(rownames(ct)), var_label(d, v1)), tab)
  marg <- if (pct == "col") colSums(ct) else NULL
  if (pct == "col") {
    tot <- rowSums(ct) / n
    tab$Total <- fmt_pct(tot)
    tab <- rbind(tab, c("n", fmt_int(round(marg)), fmt_int(round(n))))
  } else if (pct == "row") {
    tab$n <- fmt_int(round(rowSums(ct)))
  }
  rownames(tab) <- NULL
  test <- suppressWarnings(stats::chisq.test(ct, correct = FALSE))
  k <- min(dim(ct)) - 1
  cv <- if (k > 0) sqrt(unname(test$statistic) / (n * k)) else NA
  stats_txt <- paste0("χ²(", test$parameter, ") = ", fmt_num(unname(test$statistic)), " · ", p_txt(test$p.value),
                      " · V de Cramér = ", fmt_num(cv), ". Asociación ", strength(cv), " y ", signif_text(test$p.value), ".")
  if (any(test$expected < 5)) stats_txt <- paste(stats_txt, "Atención: hay casillas con menos de 5 casos esperados.")

  pd <- as.data.frame(as.table(prop))
  names(pd) <- c("a", "b", "pct")
  # El gráfico muestra la variable 1 dentro de cada categoría de la variable 2 (o al revés con % por fila).
  if (pct == "row") {
    pd$grupo <- pd$a; pd$serie <- pd$b; xl <- var_label(d, v1); fl <- var_label(d, v2)
  } else {
    pd$grupo <- pd$b; pd$serie <- pd$a; xl <- var_label(d, v2); fl <- var_label(d, v1)
  }
  pal <- rep(caring_palette, length.out = nlevels(pd$serie))
  p <- ggplot2::ggplot(pd, ggplot2::aes(x = .data$grupo, y = .data$pct, fill = .data$serie)) +
    ggplot2::geom_col(
      position = if (chart == "apiladas") "stack" else ggplot2::position_dodge(width = 0.8),
      width = if (chart == "apiladas") 0.7 else 0.75) +
    (if (chart == "apiladas" || nlevels(pd$serie) <= 4)
      ggplot2::geom_text(ggplot2::aes(label = ifelse(.data$pct >= 0.04, fmt_pct(.data$pct, 0), "")),
        position = if (chart == "apiladas") ggplot2::position_stack(vjust = 0.5) else ggplot2::position_dodge(width = 0.8),
        vjust = if (chart == "apiladas") 0.5 else -0.4, size = 3.3,
        colour = if (chart == "apiladas") "#FFFFFF" else "#333333")) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::scale_y_continuous(labels = function(z) fmt_pct(z, 0), expand = ggplot2::expansion(c(0, 0.05))) +
    ggplot2::scale_x_discrete(labels = function(z) wrap_labels(z, 14)) +
    ggplot2::labs(x = xl, y = switch(pct, col = paste("% dentro de", tolower(xl)),
                                     row = paste("% dentro de", tolower(xl)), "% del total"), fill = fl) +
    theme_caring() + ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "#E3E3E3"))
  list(title = paste(var_label(d, v1), "según", tolower(var_label(d, v2))),
       subtitle = paste0(switch(pct, col = "% por columna", row = "% por fila", "% sobre el total"),
                         " · n = ", fmt_int(round(n)), if (!is.null(weight) && nzchar(weight)) " · ponderado" else ""),
       table = tab, stats = stats_txt, plot = p)
}

explore_numcat <- function(d, vn, vc, weight) {
  w <- weights_of(d, weight)
  ok <- !is.na(d[[vn]]) & !is.na(d[[vc]])
  x <- d[[vn]][ok]; g <- droplevels(d[[vc]][ok]); ww <- w[ok]
  tab <- do.call(rbind, lapply(levels(g), function(l) {
    s <- g == l
    data.frame(Grupo = l, n = fmt_int(sum(s)),
               Media = fmt_num(sum(ww[s] * x[s]) / sum(ww[s])),
               Mediana = fmt_num(wq(x[s], ww[s], 0.5)),
               `Desv. típica` = fmt_num(wsd(x[s], ww[s])), check.names = FALSE)
  }))
  names(tab)[1] <- var_label(d, vc)
  fit <- tryCatch(stats::lm(x ~ g, weights = ww), error = function(e) NULL)
  stats_txt <- "No se ha podido calcular la prueba."
  if (!is.null(fit) && nlevels(g) > 1) {
    a <- stats::anova(fit)
    eta2 <- a[1, "Sum Sq"] / sum(a[, "Sum Sq"])
    stats_txt <- paste0("ANOVA: F(", a[1, "Df"], ", ", a[2, "Df"], ") = ", fmt_num(a[1, "F value"]),
                        " · ", p_txt(a[1, "Pr(>F)"]), " · η² = ", fmt_num(eta2, 3),
                        ". Diferencia de medias ", signif_text(a[1, "Pr(>F)"]), ".")
  }
  pd <- data.frame(x = x, g = g)
  pal <- rep(caring_palette, length.out = nlevels(g))
  p <- ggplot2::ggplot(pd, ggplot2::aes(x = .data$g, y = .data$x, fill = .data$g)) +
    ggplot2::geom_boxplot(width = 0.55, alpha = 0.85, outlier.colour = "#999999", show.legend = FALSE) +
    ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", colour = "#000000") +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::scale_x_discrete(labels = function(z) wrap_labels(z, 14)) +
    ggplot2::labs(x = var_label(d, vc), y = var_label(d, vn)) +
    theme_caring() + ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "#E3E3E3"))
  list(title = paste(var_label(d, vn), "según", tolower(var_label(d, vc))),
       subtitle = paste0("Medias por grupo (el rombo es la media) · n = ", fmt_int(length(x))),
       table = tab, stats = stats_txt, plot = p)
}

explore_numnum <- function(d, v1, v2, weight) {
  ok <- !is.na(d[[v1]]) & !is.na(d[[v2]])
  x <- d[[v1]][ok]; y <- d[[v2]][ok]
  pe <- suppressWarnings(stats::cor.test(x, y, method = "pearson"))
  sp <- suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = FALSE))
  tab <- data.frame(
    Medida = c("Correlación de Pearson (r)", "Correlación de Spearman (ρ)", "Casos"),
    Valor = c(fmt_num(unname(pe$estimate), 3), fmt_num(unname(sp$estimate), 3), fmt_int(length(x))),
    p = c(fmt_p(pe$p.value), fmt_p(sp$p.value), ""))
  r <- unname(pe$estimate)
  stats_txt <- paste0("Relación ", strength(r), if (r >= 0) " y positiva" else " y negativa",
                      ", ", signif_text(pe$p.value), ".")
  pd <- data.frame(x = x, y = y)
  if (nrow(pd) > 3000) pd <- pd[sample(nrow(pd), 3000), ]
  p <- ggplot2::ggplot(pd, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_point(colour = caring_blue, alpha = 0.45, size = 1.8) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, colour = "#135DCD", se = TRUE, fill = "#CBDEFB") +
    ggplot2::labs(x = var_label(d, v1), y = var_label(d, v2)) +
    theme_caring() + ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "#E3E3E3"))
  list(title = paste(var_label(d, v1), "y", tolower(var_label(d, v2))),
       subtitle = paste0("Dispersión con recta de regresión · n = ", fmt_int(length(x))),
       table = tab, stats = stats_txt, plot = p)
}

# Exporta un resultado de Explorar como imagen.
save_plot_png <- function(p, file, width = 9, height = 5) {
  ggplot2::ggsave(file, p, width = width, height = height, dpi = 150, bg = "white")
  file
}

# Imagen de una tabla (para vista previa en el editor de informes).
save_table_png <- function(tab, title, file) {
  ft <- flextable_caring(tab)
  tryCatch(flextable::save_as_image(ft, path = file), error = function(e) NULL)
}

flextable_caring <- function(tab) {
  ft <- flextable::flextable(tab)
  ft <- flextable::font(ft, fontname = "Arial", part = "all")
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::color(ft, color = "#FFFFFF", part = "header")
  ft <- flextable::bg(ft, bg = "#2F7BEE", part = "header")
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline(ft, border = officer::fp_border(color = "#E3E3E3"), part = "body")
  ft <- flextable::autofit(ft)
  ft
}
