test_that("explorar una variable", {
  d <- read_any(demo_sav())
  r <- explore_one(d, "educacion")
  expect_s3_class(r$plot, "ggplot")
  expect_true("Total" %in% r$table$Categoría)
  r <- explore_one(d, "ingresos", weight = "peso")
  expect_match(r$subtitle, "ponderado")
  expect_equal(nrow(r$table), 9)
  r <- explore_one(d, "aficiones")
  expect_null(r$plot)
})

test_that("explorar dos variables", {
  d <- read_any(demo_sav())
  r <- explore_two(d, "internet", "educacion")
  expect_match(r$stats, "Cramér")
  expect_s3_class(r$plot, "ggplot")
  r <- explore_two(d, "internet", "educacion", pct = "row", chart = "apiladas")
  expect_true("n" %in% names(r$table))
  r <- explore_two(d, "edad", "internet")
  expect_match(r$stats, "ANOVA")
  r <- explore_two(d, "genero", "ingresos", weight = "peso")
  expect_match(r$stats, "ANOVA")
  r <- explore_two(d, "edad", "ingresos")
  expect_match(r$table$Medida[1], "Pearson")
  expect_error(explore_two(d, "aficiones", "edad"), "texto")
})

test_that("todos los tipos de red se construyen", {
  d <- read_any(demo_sav())
  specs <- list(
    list(type = "coincidencias", vars = c("genero", "educacion", "internet", "prensa"), community = "louvain"),
    list(type = "coincidencias", vars = c("genero", "educacion", "internet"), weight = "peso", signif = FALSE),
    list(type = "correlaciones", vars = c("edad", "ingresos", "ideologia", "confianza_ciencia")),
    list(type = "factorial", vars = c("genero", "educacion", "internet", "prensa"), nclusters = 3),
    list(type = "loglineal", vars = c("genero", "educacion", "internet")),
    list(type = "regresion", dep = "internet", vars = c("edad", "educacion"), weight = "peso"),
    list(type = "regresion", dep = "ingresos", vars = c("edad", "genero")),
    list(type = "multiple", vars = "aficiones", sep = ";")
  )
  for (s in specs) {
    net <- net_build(d, s)
    expect_true(inherits(net, "netCoin"), info = s$type)
  }
  expect_error(net_build(d, list(type = "coincidencias", vars = c("edad", "genero"))), "no son categóricas")
  expect_error(net_build(d, list(type = "regresion", vars = "edad")), "explicar")
})

test_that("la red se guarda como HTML autocontenido", {
  d <- read_any(demo_sav())
  net <- net_build(d, list(type = "coincidencias", vars = c("genero", "internet", "prensa")))
  f <- tempfile(fileext = ".html")
  net_render_html(net, f)
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_false(grepl('src="scripts/', html, fixed = TRUE))
  expect_false(grepl('href="styles/', html, fixed = TRUE))
  expect_match(html, "font/woff2")
})

test_that("todos los tipos de gráfico de Explorar se dibujan", {
  d <- read_any(demo_sav())
  casos <- list(cat = list("educacion"), num = list("ingresos"),
                catcat = list("internet", "educacion"), numcat = list("edad", "educacion"),
                numnum = list("edad", "ingresos"))
  for (k in names(casos)) {
    v <- casos[[k]]
    expect_equal(chart_kind(d, v[[1]], if (length(v) > 1) v[[2]]), k)
    for (ch in chart_choices(k)) {
      for (pct in if (k == "catcat") c("col", "row", "total") else "col") {
        r <- if (length(v) == 1) explore_one(d, v[[1]], weight = "peso", chart = ch)
             else explore_two(d, v[[1]], v[[2]], weight = "peso", pct = pct, chart = ch)
        f <- tempfile(fileext = ".png")
        expect_silent(suppressMessages(save_plot_png(r$plot, f)))
        expect_gt(file.size(f), 5000)
      }
    }
  }
  expect_equal(pick_chart("inventado", "cat"), "barras_h")
  expect_equal(chart_kind(d, "aficiones"), "texto")
})

test_that("cada tipo de red tiene sus propias opciones y las usa", {
  expect_true("order" %in% net_options("loglineal"))
  for (t in setdiff(names(net_types), "loglineal")) expect_false("order" %in% net_options(t), info = t)
  expect_false("signif" %in% net_options("factorial"))
  expect_true(all(c("nclusters", "clusters") %in% net_options("factorial")))
  expect_true("method" %in% net_options("correlaciones"))
  d <- read_any(demo_sav())
  for (m in c("pearson", "spearman", "kendall")) {
    expect_true(inherits(net_build(d, list(type = "correlaciones", vars = c("edad", "ingresos", "ideologia"), method = m)), "netCoin"))
  }
  for (cl in c("kmeans", "pam", "hclust")) {
    expect_true(inherits(suppressWarnings(net_build(d, list(type = "factorial", vars = c("genero", "educacion", "internet"),
                                                     nclusters = 3, clusters = cl))), "netCoin"))
  }
  expect_true(inherits(suppressWarnings(net_build(d, list(type = "loglineal", vars = c("genero", "internet", "prensa"), order = 3))), "netCoin"))
})

test_that("tabla cruzada: porcentajes verticales, horizontales y totales, en ambos sentidos", {
  d <- read_any(demo_sav())
  num <- function(x) as.numeric(gsub(",", ".", gsub(" %.*| \\(.*", "", x)))
  r <- explore_two(d, "internet", "educacion", pct = "col")
  expect_equal(r$table[[1]], c("Sí", "No", "Total", "n"))
  cols <- setdiff(names(r$table), names(r$table)[1])
  expect_true(all(r$table[r$table[[1]] == "Total", cols] == "100,0 %"))   # cada columna suma 100
  suma <- colSums(sapply(r$table[1:2, cols], num))
  expect_true(all(abs(suma - 100) < 0.2))
  expect_match(r$subtitle, "Usa internet en filas · Nivel educativo en columnas")

  v <- explore_two(d, "educacion", "internet", pct = "col")              # dado la vuelta
  expect_equal(names(v$table)[1], "Nivel educativo")
  expect_true(all(c("Sí", "No", "Total") %in% names(v$table)))
  expect_equal(nrow(v$table), nlevels(d$educacion) + 2)

  h <- explore_two(d, "internet", "educacion", pct = "row")
  expect_true(all(h$table$Total[1:2] == "100,0 %"))                     # cada fila suma 100
  t <- explore_two(d, "internet", "educacion", pct = "total")
  expect_equal(t$table$Total[t$table[[1]] == "Total"], "100,0 %")

  rc <- explore_two(d, "internet", "educacion", pct = "col", counts = TRUE)
  expect_match(rc$table[1, 2], "^[0-9]+,[0-9] % \\([0-9]+\\)$")
  expect_false("n" %in% rc$table[[1]])
})
