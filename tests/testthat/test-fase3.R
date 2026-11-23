d <- read_any(demo_sav())

test_that("tablas de tres variables (categóricas)", {
  r <- explore_three(d, "internet", "educacion", "genero")
  expect_equal(r$kind, "catcat3")
  expect_equal(names(r$table)[1], var_label(d, "genero"))
  expect_setequal(setdiff(r$table[[1]], ""), levels(d$genero))
  # cada bloque coincide con la tabla de dos variables de ese grupo
  muj <- subset_rows(d, !is.na(d$genero) & d$genero == "Mujer")
  dos <- explore_two(muj, "internet", "educacion")$table
  bloque <- r$table[seq_len(nrow(dos)), -1]
  expect_equal(unname(as.matrix(bloque)), unname(as.matrix(dos)))
  expect_match(r$stats, "2 grupos|de 2 grupos")
  expect_s3_class(r$plot, "ggplot")
  expect_true(inherits(r$plot$facet, "FacetWrap"))
})

test_that("tres variables con una numérica y con pesos", {
  r <- explore_three(d, "edad", "educacion", "genero", chart = "violin")
  expect_equal(r$kind, "numcat3")
  expect_match(r$stats, "F\\(")
  set.seed(1); d$pw <- runif(nrow(d), 0.5, 2)
  a <- explore_three(d, "internet", "educacion", "genero", weight = "pw")
  d$pw <- d$pw * 10
  b <- explore_three(d, "internet", "educacion", "genero", weight = "pw")
  expect_identical(a$stats, b$stats)
})

test_that("tres variables: errores claros", {
  expect_error(explore_three(d, "internet", "educacion", "edad"), "control tiene que ser categórica")
  expect_error(explore_three(d, "edad", "ideologia", "genero"), "dos categóricas")
  expect_error(explore_three(d, "internet", "internet", "genero"), "distintas")
  r <- explore_from_spec(d, list(mode = "three", v1 = "internet", v2 = "educacion", v3 = "genero", pct = "row"))
  expect_match(r$subtitle, "horizontales")
})

test_that("gráficos interactivos para todos los tipos", {
  skip_if_not_installed("plotly", "4.11.0")
  specs <- list(list(mode = "one", v1 = "educacion"), list(mode = "one", v1 = "educacion", chart = "anillo"),
                list(mode = "one", v1 = "edad", chart = "caja"), list(mode = "two", v1 = "internet", v2 = "educacion", chart = "mosaico"),
                list(mode = "two", v1 = "edad", v2 = "genero"), list(mode = "two", v1 = "edad", v2 = "ideologia"),
                list(mode = "three", v1 = "internet", v2 = "educacion", v3 = "genero"))
  for (sp in specs) {
    w <- to_plotly(explore_from_spec(d, sp))
    expect_s3_class(w, "plotly")
    b <- suppressWarnings(plotly::plotly_build(w))
    expect_equal(b$x$config$locale, "es")
  }
  # las barras llevan el texto de ayuda en español
  b <- suppressWarnings(plotly::plotly_build(to_plotly(explore_one(d, "educacion"))))
  expect_true(any(grepl("%", unlist(lapply(b$x$data, `[[`, "text")))))
  f <- withr::local_tempfile(fileext = ".html")
  save_plotly_html(to_plotly(explore_one(d, "genero")), f, "Prueba")
  html <- readLines(f, warn = FALSE)
  expect_false(any(grepl("<script src=", html)))          # todo dentro del archivo
  expect_true(any(grepl("plotly", html)))
})
