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
