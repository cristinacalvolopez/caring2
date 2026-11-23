d0 <- read_any(demo_sav())

test_that("los resultados guardados se rehacen cuando cambian los datos", {
  dir <- withr::local_tempdir()
  rec0 <- list()
  sp <- list(mode = "two", v1 = "internet", v2 = "educacion", pct = "col")
  x <- explore_from_spec(d0, sp)
  png <- withr::local_tempfile(fileext = ".png"); save_plot_png(x$plot, png)
  id_g <- save_result(dir, "grafico", x$title, list(table = x$table, spec = sp, fig = plotly_figure_json(to_plotly(x))),
                      png = png, data_hash = recipe_hash(rec0))
  id_t <- save_result(dir, "tabla", x$title, list(table = x$table, spec = sp), data_hash = recipe_hash(rec0))
  nsp <- list(type = "coincidencias", vars = c("genero", "internet"), title = default_net_title(d0, list(type = "coincidencias", vars = c("genero", "internet"))), auto_title = TRUE)
  id_r <- save_result(dir, "red", nsp$title, list(spec = nsp), data_hash = recipe_hash(rec0))
  id_viejo <- save_result(dir, "tabla", "Sin receta", list(table = x$table))
  # sin cambios en los datos: no se toca nada
  expect_equal(refresh_results(dir, d0, recipe_hash(rec0)), 0)

  rec1 <- list(list(op = "label", var = "internet", label = "Conexión a internet"),
               list(op = "catlabels", var = "educacion", map = list(Postgrado = "Máster o doctorado")))
  d1 <- apply_recipe(d0, rec1)
  n <- suppressWarnings(refresh_results(dir, d1, recipe_hash(rec1)))
  expect_equal(n, 3)
  g <- load_result(dir, id_g)
  expect_match(g$title, "Conexión a internet")
  expect_true("Máster o doctorado" %in% names(g$object$table))
  expect_match(g$object$fig, "Máster o doctorado")
  expect_equal(g$data_hash, recipe_hash(rec1))
  expect_match(load_result(dir, id_t)$object$table[[1]][1], "Sí")
  r <- load_result(dir, id_r)
  expect_match(r$title, "Conexión a internet")
  expect_true(file.exists(file.path(dir, "resultados", paste0(id_r, ".html"))))
  expect_equal(load_result(dir, id_viejo)$title, "Sin receta")          # sin receta: se deja igual
  # si ya no se puede rehacer (variable borrada), se conserva el resultado anterior
  rec2 <- c(rec1, list(list(op = "drop", vars = "internet")))
  suppressWarnings(refresh_results(dir, apply_recipe(d0, rec2), recipe_hash(rec2)))
  expect_match(load_result(dir, id_g)$title, "Conexión a internet")
  expect_equal(load_result(dir, id_g)$data_hash, recipe_hash(rec2))
})

test_that("la presentación lleva los gráficos interactivos y las descargas no", {
  dir <- withr::local_tempdir()
  x <- explore_from_spec(d0, list(mode = "one", v1 = "educacion"))
  png <- withr::local_tempfile(fileext = ".png"); save_plot_png(x$plot, png)
  id <- save_result(dir, "grafico", x$title, list(table = x$table, fig = plotly_figure_json(to_plotly(x))), png = png)
  doc <- new_document("Prueba", "slides")
  doc$pages[[1]]$elements <- list(list(id = "e1", kind = "result", ref = id, x = 10, y = 10, w = 500, h = 300))
  f1 <- withr::local_tempfile(fileext = ".html"); export_html(dir, doc, f1, present = TRUE)
  f2 <- withr::local_tempfile(fileext = ".html"); export_html(dir, doc, f2)
  p <- paste(readLines(f1, warn = FALSE), collapse = "\n"); h <- paste(readLines(f2, warn = FALSE), collapse = "\n")
  expect_match(p, 'class="plt"'); expect_match(p, "Plotly.newPlot")
  expect_false(grepl("Plotly.newPlot", h)); expect_match(h, "<img")
})
