d0 <- read_any(demo_sav())

doc_con_grafico <- function(dir, pal = NULL, type = "slides") {
  x <- explore_from_spec(d0, list(mode = "two", v1 = "internet", v2 = "educacion"))
  png <- withr::local_tempfile(fileext = ".png", .local_envir = parent.frame()); save_plot_png(x$plot, png)
  id <- save_result(dir, "grafico", x$title, list(table = x$table, spec = list(mode = "two", v1 = "internet", v2 = "educacion"),
                                                  fig = plotly_figure_json(to_plotly(x))), png = png)
  doc <- new_document("Prueba", type)
  doc$pages[[1]]$bg <- "#FFFFFF"
  doc$pages[[1]]$elements <- list(
    list(id = "g1", kind = "result", ref = id, x = 20, y = 20, w = 500, h = 300, style = if (!is.null(pal)) list(palette = pal)),
    list(id = "s1", kind = "shape", x = 600, y = 40, w = 120, h = 120, style = list(bg = "#FF0000", shape = "estrella")))
  doc$pages[[2]] <- list(id = "p2", bg = "#FFFFFF", elements = list())
  save_document(dir, doc)
  list(doc = doc, id = id)
}

test_that("los gráficos de una presentación pueden llevar otra paleta", {
  dir <- withr::local_tempdir()
  x <- doc_con_grafico(dir, "calidos")
  expect_equal(ensure_palette_variants(dir, d0), 1)
  expect_equal(ensure_palette_variants(dir, d0), 0)                  # ya existe
  expect_true(file.exists(file.path(dir, "resultados", paste0(x$id, "__calidos.png"))))
  r <- load_result(dir, x$id)
  expect_match(r$object$figs$calidos, "#E4572E", ignore.case = TRUE)
  expect_false(grepl("#E4572E", r$object$fig, ignore.case = TRUE))
  expect_match(basename(palette_png(dir, x$id, "calidos")), "__calidos")
  expect_equal(basename(palette_png(dir, x$id, NULL)), paste0(x$id, ".png"))
  expect_equal(basename(palette_png(dir, x$id, "../../x")), paste0(x$id, ".png"))
  a <- editor_assets(dir, list_results(dir))
  expect_true("calidos" %in% names(a$results[[x$id]]$figs))
  # al cambiar los datos, la versión de color se rehace
  rec <- list(list(op = "label", var = "internet", label = "Conexión"))
  suppressWarnings(refresh_results(dir, apply_recipe(d0, rec), recipe_hash(rec)))
  expect_false(file.exists(file.path(dir, "resultados", paste0(x$id, "__calidos.png"))))
  expect_equal(ensure_palette_variants(dir, apply_recipe(d0, rec)), 1)
})

test_that("todas las paletas dibujan todos los tipos de gráfico", {
  for (k in names(chart_palettes)) with_palette(k, {
    for (sp in list(list(mode = "one", v1 = "educacion"), list(mode = "one", v1 = "edad"), list(mode = "two", v1 = "edad", v2 = "genero"),
                    list(mode = "two", v1 = "internet", v2 = "educacion", chart = "calor"))) {
      x <- explore_from_spec(d0, sp)
      expect_s3_class(ggplot2::ggplot_build(x$plot), "ggplot_built")
    }
    expect_equal(pal_main(), chart_palettes[[k]]$main)
  })
  expect_equal(pal_main(), chart_palettes$netcoin$main)                 # se restaura
})

test_that("formas y marca de CARING en las exportaciones", {
  dir <- withr::local_tempdir()
  x <- doc_con_grafico(dir, "verdes")
  ensure_palette_variants(dir, d0)
  f <- withr::local_tempfile(fileext = ".html"); export_html(dir, x$doc, f)
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_equal(lengths(regmatches(h, gregexpr('class="brand"', h))), 1)     # solo en la primera diapositiva
  expect_match(h, "Elaborado con")
  expect_match(h, "clip-path:polygon")
  for (k in c("elipse", names(shape_points))) expect_gt(file.size(shape_png(100, 80, k, "#00AA00")), 500)
  pp <- withr::local_tempfile(fileext = ".pptx"); export_pptx(dir, x$doc, pp)
  s1 <- officer::pptx_summary(officer::read_pptx(pp))
  expect_true(any(grepl("Elaborado con", s1$text[s1$slide_id == 1])))
  expect_false(any(grepl("Elaborado con", s1$text[s1$slide_id == 2])))
  # en los informes también, solo en la primera página (y en Word, en el pie de la primera página)
  dir2 <- withr::local_tempdir()
  y <- doc_con_grafico(dir2, NULL, type = "report")
  f2 <- withr::local_tempfile(fileext = ".html"); export_html(dir2, y$doc, f2)
  h2 <- paste(readLines(f2, warn = FALSE), collapse = "")
  expect_equal(lengths(regmatches(h2, gregexpr('class="brand"', h2))), 1)
  w <- withr::local_tempfile(fileext = ".docx"); export_docx(dir2, y$doc, w)
  pies <- grep("footer", unzip(w, list = TRUE)$Name, value = TRUE)
  txt <- paste(unlist(lapply(pies, function(n) readLines(unz(w, n), warn = FALSE))), collapse = "")
  expect_match(txt, "Elaborado con")
  expect_match(paste(readLines(unz(w, "word/document.xml"), warn = FALSE), collapse = ""), "titlePg")
  # fondo oscuro: logo blanco
  expect_match(brand_html("#1257C2"), "#FFFFFF")
  expect_match(brand_html("#FFFFFF"), "#555555")
})

test_that("las tablas de los documentos también cambian de colores", {
  dir <- withr::local_tempdir()
  y <- explore_from_spec(d0, list(mode = "one", v1 = "educacion"))
  id <- save_result(dir, "tabla", y$title, list(table = y$table))
  doc <- new_document("Tablas", "slides")
  doc$pages[[2]]$elements <- list(list(id = "t1", kind = "result", ref = id, x = 20, y = 20, w = 500, h = 250,
                                       style = list(palette = "verdes")))
  verde <- chart_palettes$verdes$main
  f <- withr::local_tempfile(fileext = ".html"); export_html(dir, doc, f)
  expect_match(paste(readLines(f, warn = FALSE), collapse = ""), paste0("--th:", verde))
  pp <- withr::local_tempfile(fileext = ".pptx"); export_pptx(dir, doc, pp)
  expect_match(paste(readLines(unz(pp, "ppt/slides/slide2.xml"), warn = FALSE), collapse = ""), sub("#", "", verde), ignore.case = TRUE)
  dx <- withr::local_tempfile(fileext = ".docx"); export_docx(dir, doc, dx)
  expect_match(paste(readLines(unz(dx, "word/document.xml"), warn = FALSE), collapse = ""), sub("#", "", verde), ignore.case = TRUE)
  x <- withr::local_tempfile(fileext = ".xlsx"); export_tables_xlsx(dir, doc, x)
  st <- paste(utils::capture.output(print(openxlsx::getStyles(openxlsx::loadWorkbook(x)))), collapse = " ")
  expect_match(st, verde, ignore.case = TRUE)
  # contraste: cabecera clara con texto oscuro
  expect_equal(table_colors("pastel")$head_text, "#1A1A1A")
  expect_equal(table_colors(NULL)$head, "#236CDB")
})
