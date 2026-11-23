d <- read_any(ext_file("demo_encuesta.sav"))

test_that("el χ² ponderado no depende de la escala de los pesos (Rao-Scott)", {
  set.seed(4)
  w <- runif(nrow(d), 0.5, 2)
  d$pw <- w
  r1 <- explore_two(d, "genero", "educacion", weight = "pw")
  d$pw <- w * 10
  r2 <- explore_two(d, "genero", "educacion", weight = "pw")
  expect_identical(r1$stats, r2$stats)
  expect_match(r1$stats, "Rao-Scott")
  sin <- explore_two(d, "genero", "educacion")
  expect_false(grepl("Rao-Scott", sin$stats))
  t1 <- chisq_w(d$genero, d$educacion, w)
  t2 <- chisq_w(d$genero, d$educacion, w * 1000)
  expect_equal(t1$p, t2$p)
  expect_equal(t1$v, t2$v)
})

test_that("las pruebas por opción de respuesta múltiple tampoco dependen de los pesos", {
  set.seed(5)
  dd <- d
  for (k in 1:3) dd[[paste0("m_", k)]] <- factor(sample(c("Sí", "No"), nrow(d), TRUE))
  s <- list(op = "mrset", name = "medios", type = "dicotomias", vars = paste0("m_", 1:3), value = "Sí")
  dd <- apply_step(dd, s)
  w <- runif(nrow(dd), 0.5, 2)
  dd$pw <- w
  a <- explore_mr_cat(dd, paste0("mr:", s$name), "genero", weight = "pw")
  dd$pw <- w * 50
  b <- explore_mr_cat(dd, paste0("mr:", s$name), "genero", weight = "pw")
  expect_identical(a$table[-nrow(a$table), ], b$table[-nrow(b$table), ])   # % y asteriscos iguales
  expect_identical(a$stats, b$stats)
})

test_that("un resultado se rehace desde su receta y se guarda ligero", {
  spec <- list(mode = "two", v1 = "genero", v2 = "educacion", pct = "row", chart = NULL, counts = TRUE)
  r <- explore_from_spec(d, spec)
  expect_identical(r$table, explore_two(d, "genero", "educacion", pct = "row", counts = TRUE)$table)
  expect_identical(explore_from_spec(d, list(mode = "one", v1 = "edad"))$title, var_label(d, "edad"))
  dir <- withr::local_tempdir()
  id <- save_result(dir, "grafico", r$title, list(table = r$table, spec = spec))
  expect_lt(file.size(file.path(dir, "resultados", paste0(id, ".rds"))), 20000)
  # resultados antiguos con el ggplot dentro se aligeran al listarlos
  old <- save_result(dir, "grafico", r$title, list(plot = r$plot, table = r$table))
  f <- file.path(dir, "resultados", paste0(old, ".rds"))
  antes <- file.size(f)
  list_results(dir)
  expect_lt(file.size(f), antes)
  expect_null(load_result(dir, old)$object$plot)
})

test_that("se proponen etiquetas legibles para variables sin etiqueta", {
  expect_equal(humanize_name("confianza_ciencia"), "Confianza ciencia")
  expect_equal(humanize_name("nivelEstudios"), "Nivel estudios")
  expect_equal(humanize_name("PIB_pc"), "PIB pc")
  expect_equal(humanize_name("P12"), "P12")
  expect_equal(humanize_name("v3_1"), "v3_1")
  x <- data.frame(mi_var = 1:3, otra = 4:6)
  attr(x$otra, "label") <- "Otra cosa"
  expect_equal(unlabelled_vars(x), "mi_var")
  expect_equal(var_label(x, "mi_var"), "Mi var")
  y <- apply_step(x, list(op = "labels", labels = list(mi_var = "Mi variable")))
  expect_equal(var_label(y, "mi_var"), "Mi variable")
  expect_length(unlabelled_vars(y), 0)
  expect_match(step_label(list(op = "labels", labels = list(a = "A"))), "1 variable")
})

test_that("se cambian las etiquetas de las categorías", {
  y <- apply_step(d, list(op = "catlabels", var = "genero", map = list(Mujer = "Mujeres", Hombre = "Hombres")))
  expect_setequal(levels(y$genero), c("Mujeres", "Hombres"))
  expect_equal(sum(y$genero == "Mujeres", na.rm = TRUE), sum(d$genero == "Mujer", na.rm = TRUE))
  expect_equal(attr(y$genero, "label"), attr(d$genero, "label"))
  cod <- attr(y$genero, "codes")
  if (length(cod)) expect_true(all(names(cod) %in% c("Mujeres", "Hombres")))
  # dos categorías con la misma etiqueta se unen
  lv <- levels(d$educacion)
  z <- apply_step(d, list(op = "catlabels", var = "educacion", map = stats::setNames(list("Superiores", "Superiores"), lv[3:4])))
  expect_equal(nlevels(z$educacion), length(lv) - 1)
  expect_error(apply_step(d, list(op = "catlabels", var = "genero", map = list(Otro = "X"))), "no existe")
  expect_error(apply_step(d, list(op = "catlabels", var = "edad", map = list(a = "b"))), "categóricas")
  expect_match(paste(step_code(list(op = "catlabels", var = "genero", map = list(Mujer = "Mujeres"))), collapse = ""), "levels")
})

test_that("los nodos de las redes llevan nombres legibles", {
  net <- suppressWarnings(suppressMessages(net_build(d, list(type = "coincidencias", vars = c("genero", "internet")))))
  expect_equal(net$options$nodeLabel, "etiqueta")
  expect_true(paste0("Mujer (", var_label(d, "genero"), ")") %in% net$nodes$etiqueta)
  # los identificadores no cambian: los enlaces siguen apuntando a nodos existentes
  expect_true(all(c(net$links$Source, net$links$Target) %in% net$nodes[[net$options$nodeName]]))
  solo <- suppressWarnings(suppressMessages(net_build(d, list(type = "coincidencias", vars = c("genero", "internet"), labels = "categoria"))))
  expect_true("Mujer" %in% solo$nodes$etiqueta)
  orig <- suppressWarnings(suppressMessages(net_build(d, list(type = "coincidencias", vars = c("genero", "internet"), labels = "original"))))
  expect_null(orig$nodes$etiqueta)
})

test_that("la imagen de la red se recorta sin márgenes", {
  skip_if_not_installed("magick")
  f <- withr::local_tempfile(fileext = ".png")
  img <- magick::image_blank(1200 * 2, 750 * 2, "white")
  img <- magick::image_draw(img); graphics::rect(1000, 600, 1400, 900, col = "red", border = NA); grDevices::dev.off()
  magick::image_write(img, f)
  crop_net_png(f)
  info <- magick::image_info(magick::image_read(f))
  expect_lt(info$width, 700); expect_lt(info$height, 600)
})

test_that("una variable calculada no hereda la etiqueta de la de origen", {
  y <- apply_step(d, list(op = "compute", to = "edad10", expr = "round(edad / 10)"))
  expect_null(attr(y$edad10, "label"))
  expect_true("edad10" %in% unlabelled_vars(y))
  net <- suppressWarnings(suppressMessages(net_build(d, list(type = "coincidencias", vars = c("genero", "internet")))))
  expect_true(var_label(d, "genero") %in% net$nodes$variable)
})

test_that("los mensajes de error se muestran en español", {
  expect_match(error_es(simpleError("subscript out of bounds")), "no existe")
  expect_match(error_es(simpleError("object 'edad2' not found")), "No existe «edad2»")
  expect_match(error_es("Some unknown failure with the thing"), "^No se ha podido completar la operación")
  expect_equal(error_es(simpleError("Elige dos variables distintas.")), "Elige dos variables distintas.")
})

test_that("el editor de variables admite categorías raras", {
  ns <- shiny::NS("x")
  x <- addNA(factor(c("a", NA, "b")))
  expect_s3_class(catlabels_ui(ns, x), "shiny.tag.list")
  expect_s3_class(catlabels_ui(ns, factor(c("", "a"))), "shiny.tag.list")
  expect_no_error(missing_ui(ns, factor(rep(NA, 3))))
  sp <- data.frame(p = haven::labelled(c(1, 2, 1), c(" " = 1, "Bien" = 2)))
  f <- withr::local_tempfile(fileext = ".sav"); haven::write_sav(sp, f)
  expect_equal(levels(read_any(f)$p), c("1", "Bien"))
})

test_that("una categoría sin etiqueta (NA) no rompe las tablas", {
  dd <- d
  dd$x <- addNA(factor(ifelse(seq_len(nrow(d)) %% 9 == 0, NA, as.character(d$region))))
  r <- explore_two(dd, "educacion", "x")
  expect_false(anyNA(names(r$table)))
  expect_s3_class(html_table(r$table), "shiny.tag")
  t <- r$table; names(t)[2] <- NA
  expect_s3_class(html_table(t), "shiny.tag")
  f <- withr::local_tempfile(fileext = ".rds"); saveRDS(dd, f)
  x <- read_any(f)$x
  expect_false(anyNA(levels(x)))
  expect_equal(sum(is.na(x)), sum(seq_len(nrow(d)) %% 9 == 0))
  y <- read_any({ g <- withr::local_tempfile(fileext = ".rds"); saveRDS(data.frame(z = factor(c("", "a", "a"))), g); g })$z
  expect_true("(sin etiqueta)" %in% levels(y))
  expect_match(error_es(simpleError("valor ausente donde TRUE/FALSE es necesario")), "valores perdidos")
})
