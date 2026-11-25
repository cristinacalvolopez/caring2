# Accesibilidad: contraste de los colores de CARING, estructura de la página y manejo con teclado.

contraste <- function(a, b) {
  lum <- function(h) {
    v <- grDevices::col2rgb(h)[, 1] / 255
    v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
    sum(c(0.2126, 0.7152, 0.0722) * v)
  }
  l <- sort(c(lum(a), lum(b)), decreasing = TRUE)
  (l[1] + 0.05) / (l[2] + 0.05)
}

test_that("los colores de CARING cumplen el contraste mínimo (WCAG AA, 4,5:1)", {
  css <- readLines(app_file("www", "caring.css"), warn = FALSE)
  token <- function(n) sub(".*:\\s*(#[0-9A-Fa-f]{6}).*", "\\1", grep(paste0("--c-", n, ":"), css, value = TRUE)[1])
  azul <- token("blue"); oscuro <- token("blue-dark")
  expect_gte(contraste(azul, "#FFFFFF"), 4.5)          # botones y barra superior
  expect_gte(contraste(azul, token("bg")), 4.5)        # enlaces sobre el fondo gris
  expect_gte(contraste(oscuro, token("blue-light")), 4.5)  # etiquetas y chips
  expect_gte(contraste(oscuro, token("blue-soft")), 4.5)   # avisos y estadísticos
  expect_gte(contraste(token("text-2"), token("bg")), 4.5) # texto secundario
  expect_equal(caring_blue, azul)
  expect_gte(contraste(theme_get("clasico")$cover, theme_get("clasico")$cover_text), 4.5)  # portadas
})

test_that("las opciones segmentadas siguen siendo alcanzables con el teclado", {
  css <- paste(readLines(app_file("www", "caring.css"), warn = FALSE), collapse = "\n")
  expect_false(grepl("\\.c-seg [^{]*input[^{]*\\{\\s*display:\\s*none", css))
  expect_match(css, "focus-visible")
  expect_match(css, "prefers-reduced-motion")
})

test_that("la página tiene idioma, zona principal y enlace para saltar al contenido", {
  expect_equal(attr(app_ui(NULL), "lang"), "es")
  user <- list(nombre = "Ana Pérez", email = "a@b.es", rol = "usuario")
  expect_match(as.character(skip_link()), 'href="#main"')
  expect_match(as.character(topbar(user)), 'aria-label="Mi cuenta"')
  expect_match(as.character(mod_login_ui("login")), "<main")
})

test_that("los desplegables sin etiqueta visible tienen una etiqueta para el lector de pantalla", {
  expect_match(as.character(mod_redes_ui("r")), "Tipo de análisis")
  expect_match(as.character(mod_explorar_ui("e")), "Tipo de gráfico")
  expect_match(as.character(mod_explorar_ui("e")), 'role="figure"')
})
