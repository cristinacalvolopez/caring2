# Análisis exploratorio --------------------------------------------------------------
# Cada función devuelve una lista con:
#   title, subtitle, table (data.frame ya formateado), stats (texto), plot (ggplot)

caring_blue <- "#2F7BEE"
caring_palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
                    "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF")

# Paletas de los gráficos ------------------------------------------------------------------------
# main: color de los gráficos de una sola serie; light/dark: tonos de apoyo; low: inicio de
# los degradados; colors: series. La activa se elige con with_palette() (por defecto, netCoin).
chart_palettes <- list(
  netcoin = list(nombre = "netCoin (por defecto)", main = "#2F7BEE", light = "#CBDEFB", dark = "#135DCD", low = "#EAF2FE",
                 colors = caring_palette),
  azules = list(nombre = "Azules", main = "#1F5FC4", light = "#C9DAF5", dark = "#0B3D91", low = "#EEF4FE",
                colors = c("#0B3D91", "#1F5FC4", "#3D85E0", "#73A8EE", "#A9C8F5", "#5B6C8F", "#26457A", "#8FB4E8")),
  calidos = list(nombre = "Cálidos", main = "#E4572E", light = "#F9D3C5", dark = "#A3361A", low = "#FDF0EA",
                 colors = c("#E4572E", "#F3A712", "#A3361A", "#F7C873", "#C2185B", "#FF8A65", "#8D4004", "#FFD54F")),
  verdes = list(nombre = "Verdes", main = "#2E8B57", light = "#CBE8D6", dark = "#1B5E3A", low = "#EEF7F1",
                colors = c("#1B5E3A", "#2E8B57", "#66BB6A", "#A5D6A7", "#00897B", "#8BC34A", "#33691E", "#4DB6AC")),
  pastel = list(nombre = "Pastel", main = "#7BA7D9", light = "#DCE8F6", dark = "#4F7CB0", low = "#F3F7FC",
                colors = c("#7BA7D9", "#F4A9A8", "#9ED8B4", "#F9D58B", "#C3A6E0", "#8FD3DA", "#F7B98B", "#C9C9C9")),
  grises = list(nombre = "Escala de grises", main = "#555555", light = "#D9D9D9", dark = "#222222", low = "#F2F2F2",
                colors = c("#222222", "#555555", "#888888", "#AAAAAA", "#CCCCCC", "#3D3D3D", "#707070", "#B5B5B5")),
  vivos = list(nombre = "Vivos", main = "#7B2FF7", light = "#E2D1FD", dark = "#4B0FB0", low = "#F5EEFE",
               colors = c("#7B2FF7", "#F72585", "#4CC9F0", "#FFB703", "#06D6A0", "#FB5607", "#3A0CA3", "#8338EC"))
)

active_palette <- function() {
  k <- getOption("caring.palette", "netcoin")
  chart_palettes[[if (k %in% names(chart_palettes)) k else "netcoin"]]
}
pal_main <- function() active_palette()$main
pal_light <- function() active_palette()$light
pal_dark <- function() active_palette()$dark
pal_low <- function() active_palette()$low
pal_colors <- function(n = NULL) { x <- active_palette()$colors; if (is.null(n)) x else rep(x, length.out = n) }

with_palette <- function(pal, expr) {
  old <- options(caring.palette = pal %||% "netcoin")
  on.exit(options(old))
  force(expr)
}

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

# Prueba χ² de independencia. Sin ponderación: χ² de Pearson. Con ponderación: corrección
# de Rao-Scott (paquete survey), que no depende de la escala de los pesos; el χ² que se
# muestra y la V de Cramér se calculan con los pesos reescalados al número real de casos.
chisq_w <- function(a, b, w = NULL) {
  ok <- !is.na(a) & !is.na(b)
  if (!is.null(w)) ok <- ok & !is.na(w) & w > 0
  a <- droplevels(as.factor(a[ok])); b <- droplevels(as.factor(b[ok]))
  ponderado <- !is.null(w) && length(unique(w[ok])) > 1
  w <- if (is.null(w)) rep(1, length(a)) else w[ok] * sum(ok) / sum(w[ok])
  ct <- stats::xtabs(w ~ a + b)
  if (nlevels(a) < 2 || nlevels(b) < 2) return(list(statistic = NA_real_, df = NA_integer_, p = NA_real_, v = NA_real_, expected = ct, weighted = ponderado))
  pe <- suppressWarnings(stats::chisq.test(ct, correct = FALSE))
  p <- unname(pe$p.value); f <- NULL
  if (ponderado) {
    rs <- tryCatch({
      dd <- data.frame(a = a, b = b, w = w)
      ds <- survey::svydesign(ids = ~1, weights = ~w, data = dd)
      suppressWarnings(survey::svychisq(~a + b, ds, statistic = "F"))
    }, error = function(e) NULL)
    if (!is.null(rs)) { p <- unname(rs$p.value); f <- list(F = unname(rs$statistic), df = unname(rs$parameter)) }
  }
  k <- min(dim(ct)) - 1
  list(statistic = unname(pe$statistic), df = unname(pe$parameter), p = p, f = f,
       v = if (k > 0) sqrt(unname(pe$statistic) / (sum(ct) * k)) else NA_real_,
       expected = pe$expected, weighted = ponderado)
}

chisq_txt <- function(t) {
  base <- paste0("χ²(", t$df, ") = ", fmt_num(t$statistic))
  if (isTRUE(t$weighted) && !is.null(t$f)) {
    gl <- function(x) if (abs(x - round(x)) < 0.005) fmt_int(round(x)) else fmt_num(x, 2)
    base <- paste0(base, " · Rao-Scott F(", gl(t$f$df[1]), "; ", gl(t$f$df[2]), ") = ", fmt_num(t$f$F))
  }
  base
}

# Una variable ----------------------------------------------------------------------

explore_one <- function(d, v, weight = NULL, chart = NULL) {
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
    pd <- data.frame(cat = factor(lv, levels = lv), pct = n / sum(n), n = n)
    ch <- pick_chart(chart, "cat")
    p <- plot_cat(pd, ch)
    list(pd = pd, chart = ch, title = lab, subtitle = paste0("Frecuencias · n = ", fmt_int(round(sum(n))), wtxt),
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
    p <- plot_num(pd, pick_chart(chart, "num"), m, lab)
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

explore_two <- function(d, v1, v2, weight = NULL, pct = "col", chart = NULL, counts = FALSE) {
  need_var(d, v1); need_var(d, v2)
  x <- d[[v1]]; y <- d[[v2]]
  if (is.factor(x) && is.factor(y)) return(explore_catcat(d, v1, v2, weight, pct, chart, counts))
  if (is.numeric(x) && is.factor(y)) return(explore_numcat(d, v1, v2, weight, chart))
  if (is.factor(x) && is.numeric(y)) return(explore_numcat(d, v2, v1, weight, chart))
  if (is.numeric(x) && is.numeric(y)) return(explore_numnum(d, v1, v2, weight, chart))
  stop("No se puede cruzar una variable de texto. Cambia su tipo en el paso Datos.")
}

# Tabla cruzada: v1 en filas, v2 en columnas.
#   pct = "col": porcentajes verticales (cada columna suma 100 %)
#   pct = "row": porcentajes horizontales (cada fila suma 100 %)
#   pct = "total": sobre el total de casos
explore_catcat <- function(d, v1, v2, weight, pct, chart, counts = FALSE) {
  w <- weights_of(d, weight)
  ok <- !is.na(d[[v1]]) & !is.na(d[[v2]])
  sin_na <- function(f) factor(as.character(f), levels = levels(f)[!is.na(levels(f))], ordered = is.ordered(f))
  dd <- data.frame(a = droplevels(sin_na(d[[v1]][ok])), b = droplevels(sin_na(d[[v2]][ok])), w = w[ok])
  dd <- dd[!is.na(dd$a) & !is.na(dd$b), , drop = FALSE]
  ct <- stats::xtabs(w ~ a + b, data = dd)
  n <- sum(ct)
  prop <- switch(pct,
    col = sweep(ct, 2, colSums(ct), "/"),
    row = sweep(ct, 1, rowSums(ct), "/"),
    ct / n)
  cell <- function(pr, n) if (counts) paste0(fmt_pct(pr), " (", fmt_int(round(n)), ")") else fmt_pct(pr)
  m <- matrix(cell(prop, ct), nrow = nrow(ct), dimnames = dimnames(ct))
  rs <- rowSums(ct); cs <- colSums(ct)
  if (pct == "col") {
    m <- cbind(m, Total = cell(rs / n, rs))
    m <- rbind(m, Total = cell(rep(1, ncol(m)), c(cs, n)))          # cada columna suma 100 %
    if (!counts) m <- rbind(m, n = fmt_int(round(c(cs, n))))
  } else if (pct == "row") {
    m <- cbind(m, Total = cell(rep(1, nrow(ct)), rs))                          # cada fila suma 100 %
    m <- rbind(m, Total = c(cell(cs / n, cs), cell(1, n)))
    if (!counts) m <- cbind(m, n = fmt_int(round(c(rs, n))))
  } else {
    m <- cbind(m, Total = cell(rs / n, rs))
    m <- rbind(m, Total = c(cell(cs / n, cs), cell(1, n)))
  }
  tab <- cbind(stats::setNames(data.frame(rownames(m), check.names = FALSE), var_label(d, v1)),
               as.data.frame(m, check.names = FALSE, stringsAsFactors = FALSE))
  rownames(tab) <- NULL
  test <- chisq_w(dd$a, dd$b, if (!is.null(weight) && nzchar(weight)) dd$w)
  cv <- test$v
  stats_txt <- paste0(chisq_txt(test), " · ", p_txt(test$p),
                      " · V de Cramér = ", fmt_num(cv), ". Asociación ", strength(cv), " y ", signif_text(test$p), ".")
  if (isTRUE(test$weighted)) stats_txt <- paste(stats_txt, "Con datos ponderados, la p sale de la corrección de Rao-Scott.")
  if (any(test$expected < 5)) stats_txt <- paste(stats_txt, "Atención: hay casillas con menos de 5 casos esperados.")

  pd <- as.data.frame(as.table(prop))
  names(pd) <- c("a", "b", "pct")
  # El gráfico muestra la variable 1 dentro de cada categoría de la variable 2 (o al revés con % por fila).
  if (pct == "row") {
    pd$grupo <- pd$a; pd$serie <- pd$b; xl <- var_label(d, v1); fl <- var_label(d, v2)
  } else {
    pd$grupo <- pd$b; pd$serie <- pd$a; xl <- var_label(d, v2); fl <- var_label(d, v1)
  }
  p <- plot_catcat(pd, ct, pick_chart(chart, "catcat"), pct, xl, fl)
  list(test = test, pd = pd, xl = xl, fl = fl, title = paste(var_label(d, v1), "según", tolower(var_label(d, v2))),
       subtitle = paste0(var_label(d, v1), " en filas · ", var_label(d, v2), " en columnas · ",
                         switch(pct, col = "% verticales (por columna)", row = "% horizontales (por fila)", "% sobre el total"),
                         " · n = ", fmt_int(round(n)), if (!is.null(weight) && nzchar(weight)) " · ponderado" else ""),
       table = tab, stats = stats_txt, plot = p)
}

explore_numcat <- function(d, vn, vc, weight, chart = NULL) {
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
  a <- NULL
  if (!is.null(fit) && nlevels(g) > 1) {
    a <- stats::anova(fit)
    eta2 <- a[1, "Sum Sq"] / sum(a[, "Sum Sq"])
    stats_txt <- paste0("ANOVA: F(", a[1, "Df"], ", ", a[2, "Df"], ") = ", fmt_num(a[1, "F value"]),
                        " · ", p_txt(a[1, "Pr(>F)"]), " · η² = ", fmt_num(eta2, 3),
                        ". Diferencia de medias ", signif_text(a[1, "Pr(>F)"]), ".")
  }
  pd <- data.frame(x = x, g = g, w = ww)
  p <- plot_numcat(pd, pick_chart(chart, "numcat"), var_label(d, vc), var_label(d, vn))
  list(anova = a, pd = pd, title = paste(var_label(d, vn), "según", tolower(var_label(d, vc))),
       subtitle = paste0("Comparación de grupos · n = ", fmt_int(length(x))),
       table = tab, stats = stats_txt, plot = p)
}

explore_numnum <- function(d, v1, v2, weight, chart = NULL) {
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
  p <- plot_numnum(data.frame(x = x, y = y), pick_chart(chart, "numnum"), var_label(d, v1), var_label(d, v2))
  list(title = paste(var_label(d, v1), "y", tolower(var_label(d, v2))),
       subtitle = paste0("Relación entre dos variables numéricas · n = ", fmt_int(length(x))),
       table = tab, stats = stats_txt, plot = p)
}

# Vuelve a calcular un resultado de Explorar a partir de su receta (la que se guarda
# con cada resultado): modo, variables, ponderación y opciones.
explore_from_spec <- function(d, spec) {
  v1 <- spec$v1; v2 <- spec$v2; w <- spec$weight
  if (identical(spec$mode, "three")) {
    return(explore_three(d, v1, v2, spec$v3, weight = w, pct = spec$pct %||% "col", chart = spec$chart,
                         counts = isTRUE(spec$counts)))
  }
  if (identical(spec$mode, "two")) {
    if (identical(v1, v2)) stop("Elige dos variables distintas.")
    if (startsWith(v1, "mr:") || startsWith(v2, "mr:")) {
      if (startsWith(v1, "mr:") && startsWith(v2, "mr:")) stop("Cruza el conjunto con una variable categórica, no con otro conjunto.")
      set <- if (startsWith(v1, "mr:")) v1 else v2
      otra <- if (startsWith(v1, "mr:")) v2 else v1
      return(explore_mr_cat(d, set, otra, weight = w, chart = spec$chart, counts = isTRUE(spec$counts)))
    }
    return(explore_two(d, v1, v2, weight = w, pct = spec$pct %||% "col", chart = spec$chart, counts = isTRUE(spec$counts)))
  }
  if (startsWith(v1, "mr:")) explore_mr(d, v1, weight = w, chart = spec$chart)
  else explore_one(d, v1, weight = w, chart = spec$chart)
}

# Exporta un resultado de Explorar como imagen.
save_plot_png <- function(p, file, width = 9, height = 5) {
  ggplot2::ggsave(file, p, width = width, height = height, dpi = 150, bg = "white")
  file
}

# Imagen de una tabla (para vista previa en el editor de informes).
save_table_png <- function(tab, title, file) {
  ft <- flextable_caring(tab)
  tryCatch(quiet_ft(flextable::save_as_image(ft, path = file)), error = function(e) NULL)
}

# flextable (0.9.x) llama a una función de gdtools que ya está obsoleta y avisa en la
# consola («'m_str_extents' is deprecated»). No afecta al resultado: se silencia.
quiet_ft <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (inherits(w, "deprecatedWarning") || grepl("m_str_extents|strings_sizes", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  })
}

flextable_caring <- function(tab) quiet_ft({
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
})

# Respuesta múltiple ----------------------------------------------------------------------

mr_find <- function(d, id) {
  name <- sub("^mr:", "", id)
  s <- mrsets(d)[[name]]
  if (is.null(s)) stop("No existe el conjunto de respuesta múltiple «", name, "».")
  s
}

# Frecuencias de un conjunto: % de casos (suma más de 100 %) y % de respuestas.
explore_mr <- function(d, id, weight = NULL, chart = NULL) {
  s <- mr_find(d, id)
  w <- weights_of(d, weight)
  f <- mr_freq(d, s, w)
  casos <- attr(f, "casos"); resp <- attr(f, "respuestas")
  tab <- data.frame(Opción = f$opcion, n = fmt_int(round(f$n)), `% de casos` = fmt_pct(f$pct_casos),
                    `% de respuestas` = fmt_pct(f$pct_resp), check.names = FALSE)
  tab <- rbind(tab, data.frame(Opción = "Total", n = fmt_int(round(resp)), `% de casos` = fmt_pct(resp / casos),
                               `% de respuestas` = "100,0 %", check.names = FALSE))
  pd <- data.frame(cat = factor(f$opcion, levels = f$opcion), pct = f$pct_casos, n = f$n)
  ch <- pick_chart(chart, "mr")
  p <- plot_cat(pd, ch) + ggplot2::labs(x = if (ch == "barras_v") NULL else "% de casos",
                                        y = if (ch == "barras_v") "% de casos" else NULL)
  list(pd = pd, chart = ch, title = mr_label(s),
       subtitle = paste0("Respuesta múltiple · ", fmt_int(round(casos)), " casos con respuesta",
                         if (!is.null(weight) && nzchar(weight)) " · ponderado" else ""),
       table = tab, plot = p,
       stats = paste0("Cada caso marca de media ", fmt_num(resp / casos), " opciones. El % de casos suma más de 100 % ",
                      "porque se puede marcar más de una opción",
                      if (attr(f, "sin_respuesta") > 0) paste0("; ", fmt_int(round(attr(f, "sin_respuesta"))), " casos no responden al conjunto") else "",
                      "."))
}

# Conjunto (en filas) según una variable categórica (en columnas): % de casos de cada grupo.
explore_mr_cat <- function(d, id, vc, weight = NULL, chart = NULL, counts = FALSE) {
  s <- mr_find(d, id)
  need_var(d, vc)
  if (!is.factor(d[[vc]])) stop("Un conjunto de respuesta múltiple solo se puede cruzar con una variable categórica.")
  w <- weights_of(d, weight)
  m <- mr_incidences(d, s)
  ok <- stats::complete.cases(m) & !is.na(d[[vc]])
  m <- m[ok, , drop = FALSE]; g <- droplevels(d[[vc]][ok]); ww <- w[ok]
  casos_g <- tapply(ww, g, sum)
  n_og <- sapply(levels(g), function(l) colSums(m[g == l, , drop = FALSE] * ww[g == l]))
  if (is.null(dim(n_og))) n_og <- matrix(n_og, nrow = ncol(m), dimnames = list(names(m), levels(g)))
  pct <- sweep(n_og, 2, casos_g, "/")
  tot <- colSums(m * ww) / sum(ww)
  # prueba χ² de cada opción (marcada / no marcada) frente a los grupos
  p_opt <- vapply(seq_len(ncol(m)), function(j) {
    if (length(unique(m[[j]])) < 2) return(NA_real_)
    chisq_w(factor(m[[j]], levels = 0:1), g, if (!is.null(weight) && nzchar(weight)) ww)$p
  }, numeric(1))
  marca <- ifelse(is.na(p_opt), "", ifelse(p_opt < 0.001, " ***", ifelse(p_opt < 0.01, " **", ifelse(p_opt < 0.05, " *", ""))))
  cell <- function(pr, n) if (counts) paste0(fmt_pct(pr), " (", fmt_int(round(n)), ")") else fmt_pct(pr)
  tabm <- matrix(cell(pct, n_og), nrow = nrow(pct), dimnames = list(paste0(rownames(pct), marca), colnames(pct)))
  tabm <- cbind(tabm, Total = cell(tot, colSums(m * ww)))
  tabm <- rbind(tabm, `Casos (n)` = fmt_int(round(c(casos_g, sum(ww)))))
  tab <- cbind(stats::setNames(data.frame(rownames(tabm), check.names = FALSE), mr_label(s)),
               as.data.frame(tabm, check.names = FALSE, stringsAsFactors = FALSE))
  rownames(tab) <- NULL
  pd <- as.data.frame(as.table(pct)); names(pd) <- c("serie", "grupo", "pct")
  pd$serie <- factor(pd$serie, levels = rownames(pct))
  ch <- pick_chart(chart, "mrcat")
  p <- plot_catcat(pd, NULL, ch, "col", var_label(d, vc), mr_label(s)) +
    ggplot2::labs(y = paste("% de casos dentro de", tolower(var_label(d, vc))))
  sig <- sum(p_opt < 0.05, na.rm = TRUE)
  list(title = paste(mr_label(s), "según", tolower(var_label(d, vc))),
       subtitle = paste0(mr_label(s), " en filas · ", var_label(d, vc), " en columnas · % de casos de cada grupo · n = ",
                         fmt_int(round(sum(ww))), if (!is.null(weight) && nzchar(weight)) " · ponderado" else ""),
       table = tab, plot = p,
       stats = paste0(sig, " de ", length(p_opt), " opciones difieren entre grupos de forma significativa ",
                      "(χ² de cada opción; * p < 0,05, ** p < 0,01, *** p < 0,001). ",
                      "Cada columna suma más de 100 % porque se puede marcar más de una opción."))
}

# Tres variables: la relación entre dos variables dentro de cada categoría de una
# tercera (variable de control). Una tabla y una prueba por cada grupo de control.
explore_three <- function(d, v1, v2, v3, weight = NULL, pct = "col", chart = NULL, counts = FALSE) {
  for (v in c(v1, v2, v3)) need_var(d, v)
  if (any(startsWith(c(v1, v2, v3), "mr:"))) stop("Los conjuntos de respuesta múltiple no se pueden usar con tres variables.")
  if (anyDuplicated(c(v1, v2, v3))) stop("Elige tres variables distintas.")
  if (!is.factor(d[[v3]])) stop("La variable de control tiene que ser categórica. Si es numérica, agrúpala en tramos en el paso Datos.")
  k <- chart_kind(d, v1, v2)
  if (!k %in% c("catcat", "numcat")) {
    stop("Con variable de control se pueden cruzar dos categóricas, o una numérica y una categórica.")
  }
  ctrl <- droplevels(d[[v3]])
  capas <- levels(ctrl)
  if (length(capas) > 12) stop("La variable de control tiene demasiadas categorías (", length(capas), "). Agrúpalas antes en el paso Datos.")
  kind3 <- paste0(k, "3")
  ch <- pick_chart(chart, kind3)
  lab3 <- var_label(d, v3)
  partes <- list(); tabs <- list(); pds <- list(); lineas <- character(); sig <- 0; validas <- 0
  for (L in capas) {
    sub <- subset_rows(d, !is.na(ctrl) & ctrl == L)
    r <- tryCatch(if (k == "catcat") explore_catcat(sub, v1, v2, weight, pct, NULL, counts)
                  else if (is.numeric(d[[v1]])) explore_numcat(sub, v1, v2, weight)
                  else explore_numcat(sub, v2, v1, weight),
                  error = function(e) NULL)
    if (is.null(r)) { lineas <- c(lineas, paste0(L, ": sin casos suficientes")); next }
    tb <- r$table
    tb <- cbind(stats::setNames(data.frame(c(L, rep("", nrow(tb) - 1)), check.names = FALSE), lab3), tb)
    tabs[[L]] <- tb
    pd <- r$pd; pd$capa <- L; pds[[L]] <- pd
    if (k == "catcat") {
      t <- r$test
      if (!is.na(t$p)) { validas <- validas + 1; if (t$p < 0.05) sig <- sig + 1 }
      lineas <- c(lineas, paste0(L, ": ", chisq_txt(t), " · ", p_txt(t$p), " · V = ", fmt_num(t$v)))
    } else if (!is.null(r$anova)) {
      a <- r$anova; pv <- a[1, "Pr(>F)"]
      validas <- validas + 1; if (isTRUE(pv < 0.05)) sig <- sig + 1
      lineas <- c(lineas, paste0(L, ": F(", a[1, "Df"], ", ", a[2, "Df"], ") = ", fmt_num(a[1, "F value"]), " · ", p_txt(pv)))
    }
  }
  if (!length(tabs)) stop("Ningún grupo de la variable de control tiene casos suficientes.")
  tab <- rbind_fill_chr(tabs)
  pd <- do.call(rbind, pds)
  pd$capa <- factor(pd$capa, levels = capas)
  faceta <- ggplot2::facet_wrap(ggplot2::vars(.data$capa), labeller = ggplot2::label_wrap_gen(28),
                                ncol = min(3, length(pds)))
  p <- if (k == "catcat") {
    plot_catcat(pd, NULL, ch, pct, if (pct == "row") var_label(d, v1) else var_label(d, v2),
                if (pct == "row") var_label(d, v2) else var_label(d, v1)) + faceta
  } else {
    vn <- if (is.numeric(d[[v1]])) v1 else v2; vc <- setdiff(c(v1, v2), vn)
    plot_numcat(pd, ch, var_label(d, vc), var_label(d, vn)) + faceta
  }
  p <- p + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = 11, hjust = 0),
                          strip.background = ggplot2::element_rect(fill = "#EEF4FE", colour = NA))
  prueba <- if (k == "catcat") "χ²" else "ANOVA"
  list(title = paste0(var_label(d, v1), " según ", tolower(var_label(d, v2)), ", por ", tolower(lab3)),
       subtitle = paste0("Control: ", lab3, " · ", length(tabs), " grupos",
                         if (k == "catcat") paste0(" · ", switch(pct, col = "% verticales", row = "% horizontales", "% sobre el total de cada grupo")),
                         if (!is.null(weight) && nzchar(weight)) " · ponderado" else ""),
       table = tab, plot = p, kind = kind3,
       stats = paste0("La relación es significativa (p < 0,05) en ", sig, " de ", validas, " grupos de ", tolower(lab3),
                      " (", prueba, " en cada grupo). ", paste(lineas, collapse = " | "), "."))
}

# Une tablas de texto con columnas distintas (las que faltan quedan con «—»).
rbind_fill_chr <- function(tabs) {
  cols <- unique(unlist(lapply(tabs, names)))
  out <- do.call(rbind, unname(lapply(tabs, function(t) {
    for (c in setdiff(cols, names(t))) t[[c]] <- "—"
    t[cols]
  })))
  rownames(out) <- NULL
  out
}
