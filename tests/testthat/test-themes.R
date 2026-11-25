contenido <- function(doc) {
  # Lo que el usuario ha puesto (sin el decorado del estilo), para comparar entre estilos
  unlist(lapply(doc$pages, function(p) vapply(Filter(function(e) !isTRUE(e$role %in% decor_roles), p$elements),
    function(e) paste(e$kind, e$text %||% e$ref %||% "", sep = ":"), "")))
}

test_that("los cinco estilos crean documentos coherentes", {
  expect_length(caring_themes, 5)
  for (type in c("slides", "report")) for (th in names(caring_themes)) {
    d <- new_document("Prueba", type, theme = th)
    expect_equal(d$theme, th)
    expect_true(all(vapply(d$pages, function(p) !is.null(p$kind) && !is.null(p$bg), logical(1))), info = paste(type, th))
    p1 <- d$pages[[1]]
    titulo <- Filter(function(e) identical(e$role, "title"), p1$elements)[[1]]
    # color real bajo el título: la forma de decorado que lo contiene o, si no hay, el fondo de página
    debajo <- p1$bg
    for (e in p1$elements) if (identical(e$kind, "shape") && isTRUE(e$role %in% c("bg", "band")) &&
                               titulo$x >= e$x && titulo$y >= e$y && titulo$y + titulo$h <= e$y + e$h) debajo <- e$style$bg
    expect_false(is_light(titulo$style$color) == is_light(debajo), info = paste("título visible", type, th))
    expect_match(theme_preview_html(th, type), "transform:scale")
  }
})

test_that("cambiar de estilo conserva el contenido y no duplica el decorado", {
  d <- new_document("Encuesta", "slides", "clasico")
  d$pages <- c(d$pages, list(page_template("seccion", "slides", theme = "clasico"),
                             page_template("dos_columnas", "slides", theme = "clasico")))
  d$pages[[2]]$elements <- c(d$pages[[2]]$elements, list(el("result", 100, 200, 300, 200, ref = "abc123", role = "result")))
  orig <- contenido(d)
  n_decor <- function(x) sum(vapply(x$pages, function(p) sum(vapply(p$elements, function(e) isTRUE(e$role %in% decor_roles), logical(1))), 0))
  for (th in c("profesional", "divertido", "claro", "academico", "clasico", "divertido")) {
    d <- apply_theme(d, th)
    expect_equal(contenido(d), orig, info = th)
  }
  d2 <- apply_theme(d, "divertido")
  expect_equal(n_decor(d2), n_decor(d))                     # aplicar dos veces no acumula adornos
  res <- Filter(function(e) identical(e$kind, "result"), d2$pages[[2]]$elements)[[1]]
  expect_equal(res$style$bg, caring_themes$divertido$result_bg)
  d3 <- apply_theme(d2, "clasico")
  res <- Filter(function(e) identical(e$kind, "result"), d3$pages[[2]]$elements)[[1]]
  expect_null(res$style$bg)
  pro <- apply_theme(d, "profesional")
  cuerpo <- Filter(function(e) identical(e$role, "body"), pro$pages[[2]]$elements)[[1]]
  expect_true(is_light(cuerpo$style$color))                 # texto claro sobre fondo oscuro
  expect_error(apply_theme(d, "inventado"), "desconocido")
})

test_that("los documentos creados antes de los estilos se convierten", {
  viejo <- list(id = "abc", title = "Antiguo", type = "slides", mode = "free", pages = list(
    list(id = "p1", bg = "#FFFFFF", elements = list(
      list(id = "a", kind = "shape", x = 0, y = 0, w = 960, h = 540, style = list(bg = "#236CDB")),
      list(id = "b", kind = "text", x = 70, y = 170, w = 820, h = 110, text = "Mi título", style = list(size = 48, bold = TRUE, color = "#FFFFFF")),
      list(id = "c", kind = "text", x = 70, y = 300, w = 820, h = 50, text = "Sub", style = list(size = 22, color = "#FFFFFF")))),
    list(id = "p2", bg = "#FFFFFF", elements = list(
      list(id = "d", kind = "shape", x = 0, y = 0, w = 960, h = 90, style = list(bg = "#236CDB")),
      list(id = "e", kind = "text", x = 48, y = 18, w = 864, h = 56, text = "Título", style = list(size = 30, bold = TRUE, color = "#FFFFFF")),
      list(id = "f", kind = "text", x = 48, y = 120, w = 864, h = 300, text = "Texto", style = list(size = 20, color = "#000000"))))))
  n <- apply_theme(viejo, "academico")
  expect_equal(n$pages[[1]]$kind, "portada")
  expect_equal(n$pages[[2]]$kind, "contenido")
  textos <- unlist(lapply(n$pages, function(p) vapply(Filter(function(e) e$kind == "text", p$elements), `[[`, "", "text")))
  expect_equal(textos, c("Mi título", "Sub", "Título", "Texto"))
  # el decorado antiguo se ha sustituido por el del estilo académico (sin fondos azules a página completa)
  azules <- Filter(function(e) e$kind == "shape" && identical(e$style$bg, "#236CDB") && e$w >= 960, n$pages[[1]]$elements)
  expect_length(azules, 0)
  t2 <- Filter(function(e) identical(e$text, "Título"), n$pages[[2]]$elements)[[1]]
  expect_equal(t2$style$color, caring_themes$academico$title)
})

test_that("cada estilo se exporta a PowerPoint, Word y HTML", {
  dir <- withr::local_tempdir()
  for (sub in c("resultados", "informes")) dir.create(file.path(dir, sub))
  d <- read_any(demo_sav())
  r <- explore_one(d, "educacion")
  png <- tempfile(fileext = ".png"); save_plot_png(r$plot, png)
  rid <- save_result(dir, "grafico", r$title, list(plot = r$plot), png = png)
  for (th in names(caring_themes)) for (type in c("slides", "report")) {
    doc <- new_document(paste("Prueba", th), type, theme = th)
    last <- length(doc$pages)
    doc$pages[[last]]$elements <- c(doc$pages[[last]]$elements,
      list(el("result", 60, 200, 400, 250, ref = rid, role = "result",
              style = compact(list(bg = caring_themes[[th]]$result_bg, radius = caring_themes[[th]]$radius)))))
    f <- tempfile(fileext = ".pptx"); export_pptx(dir, doc, f)
    expect_equal(length(officer::read_pptx(f)), length(doc$pages), info = paste(th, type))
    f <- tempfile(fileext = ".docx"); export_docx(dir, doc, f)
    expect_gt(file.size(f), 5000)
    f <- tempfile(fileext = ".html"); export_html(dir, doc, f)
    expect_match(paste(readLines(f, warn = FALSE), collapse = ""), "Prueba")
  }
})
