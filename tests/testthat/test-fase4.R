d <- read_any(demo_sav())

periodos <- function() {
  e <- new.env(); utils::data("sociologists", package = "netCoin", envir = e)
  s <- e$sociologists
  attr(s$school, "label") <- "Escuela"
  s
}

test_that("evolución temporal (timeCoin)", {
  s <- periodos()
  net <- net_build(s, list(type = "evolucion", name = "name", start = "birth", end = "death", group = "school"))
  expect_s3_class(net, "timeCoin")
  expect_equal(net_summary(net)$nodes, nrow(s))
  f <- withr::local_tempfile(fileext = ".html")
  net_render_html(net, f)
  expect_gt(file.size(f), 10000)
  # sin fin: hasta el último año; fechas; errores claros
  s$death[1] <- NA
  p <- time_periods(s, list(name = "name", start = "birth", end = "death"))
  expect_equal(p$fin[1], max(c(s$birth, s$death), na.rm = TRUE))
  fechas <- data.frame(n = c("a", "b"), i = as.Date(c("2020-01-01", "2021-05-01")), f = as.Date(c("2020-06-01", NA)))
  p2 <- time_periods(fechas, list(name = "n", start = "i", end = "f"))
  expect_equal(p2$inicio, c("2020-01-01", "2021-05-01"))
  mal <- data.frame(n = "a", i = 2000, f = 1990)
  expect_error(time_periods(mal, list(name = "n", start = "i", end = "f")), "antes de empezar")
  expect_error(time_periods(s, list(name = "name", start = "birth")), "fin")
  expect_match(default_net_title(s, list(type = "evolucion", name = "name")), "Evolución temporal")
})

test_that("el modelo de lavaan solo admite relaciones, nunca código", {
  expect_length(sem_parse("y ~ x1 + 0.5*x2\nlat =~ a + b + c # comentario\na ~~ b"), 3)
  for (m in c("x := a*b", "y ~ log(x)", "y ~ x; z == 2", "y ~ system('ls')", "y ~ x\nz < 1", "", "y ~", "`y` ~ x"))
    expect_error(sem_parse(m), "modelo")
})

test_that("ecuaciones estructurales con variables latentes (lavaan + netCoin)", {
  m <- "conf =~ confianza_ciencia + satisfaccion + ideologia\nconf ~ edad + ingresos\nedad ~~ ingresos"
  net <- suppressWarnings(net_build(d, list(type = "trayectorias", model = m)))
  expect_setequal(net$nodes$nombre, c("conf", "confianza_ciencia", "satisfaccion", "ideologia", "edad", "ingresos"))
  expect_equal(net$nodes$tipo[net$nodes$nombre == "conf"], "Latente")
  expect_setequal(unique(net$links$relacion), c("Carga factorial", "Regresión", "Correlación"))
  expect_true(all(c(net$links$Source, net$links$Target) %in% net$nodes$nombre))
  expect_match(net$options$note, "CFI")
  expect_equal(net$nodes$etiqueta[net$nodes$nombre == "edad"], var_label(d, "edad"))
  # coincide con lavaan directamente
  dd <- d; dd$satisfaccion <- as.numeric(dd$satisfaccion)
  fit <- lavaan::sem(m, data = dd[c("confianza_ciencia", "satisfaccion", "ideologia", "edad", "ingresos")], missing = "ml")
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)
  b <- pe$std.all[pe$op == "~" & pe$rhs == "edad"]
  expect_equal(net$links$beta[net$links$relacion == "Regresión" & net$links$Source == "edad"], round(b, 3))
  # disposición: las explicativas a la izquierda de lo que explican
  lay <- sem_layout(net$nodes$nombre, net$links, "conf")
  rownames(lay) <- net$nodes$nombre
  expect_lt(lay["edad", 1], lay["conf", 1]); expect_lt(lay["conf", 1], lay["ideologia", 1])
  # con pesos
  w <- suppressWarnings(net_build(d, list(type = "trayectorias", model = "confianza_ciencia ~ edad + ideologia", weight = "peso")))
  expect_s3_class(w, "netCoin")
})

test_that("errores claros en ecuaciones estructurales", {
  expect_error(net_build(d, list(type = "trayectorias", model = "y ~ x")), "no existen")
  expect_error(net_build(d, list(type = "trayectorias", model = "edad =~ ideologia + ingresos")), "no puede coincidir")
  expect_error(net_build(d, list(type = "trayectorias", model = "confianza_ciencia ~ aficiones")), "no es numérica")
  expect_match(net_code(list(type = "trayectorias", model = "y ~ x")), "sem\\(modelo")
})
