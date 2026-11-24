test_that("datos de ejemplo de evolución temporal", {
  f <- system.file("extdata", "sociologos_clasicos.sav", package = "caring2")
  if (!nzchar(f)) f <- testthat::test_path("..", "..", "inst", "extdata", "sociologos_clasicos.sav")
  d <- read_any(f)
  expect_equal(nrow(d), 16)
  expect_equal(var_label(d, "nacimiento"), "Año de nacimiento")
  expect_true(is.factor(d$escuela) && "Positivismo" %in% levels(d$escuela))
  expect_false(any(c("USA", "EEUU", "France") %in% levels(d$pais_nacimiento)))
  net <- net_build(d, list(type = "evolucion", name = "nombre", start = "nacimiento", end = "muerte", group = "escuela"))
  expect_s3_class(net, "timeCoin")
})

test_that("ayuda: cada análisis tiene su «¿Cómo se lee?» y la visita guiada sus pasos", {
  for (t in names(net_types)) expect_false(is.null(net_help_ui(t)), info = t)
  for (w in c("inicio", "proyecto")) {
    st <- tour_steps(w)
    expect_gt(length(st), 3)
    expect_true(all(vapply(st, function(x) nzchar(x$sel) && nzchar(x$title) && nzchar(x$text), logical(1))))
  }
})

test_that("«Sobre tus datos» explica cada resultado de Explorar con sus valores", {
  d <- read_any(demo_sav())
  d$pw <- runif(nrow(d), 0.5, 2)
  s <- list(op = "mrset", name = "medios", type = "dicotomias", vars = c("internet", "prensa", "tv"), value = "Sí")
  d <- apply_step(d, s)
  casos <- list(
    list(spec = list(mode = "one", v1 = "educacion"), clave = "% válido"),
    list(spec = list(mode = "one", v1 = "edad", chart = "caja"), clave = "percentil 25"),
    list(spec = list(mode = "two", v1 = "internet", v2 = "educacion", pct = "row"), clave = "horizontales"),
    list(spec = list(mode = "two", v1 = "internet", v2 = "educacion", weight = "pw"), clave = "Rao-Scott"),
    list(spec = list(mode = "two", v1 = "edad", v2 = "genero"), clave = "η²"),
    list(spec = list(mode = "two", v1 = "edad", v2 = "ingresos"), clave = "Spearman"),
    list(spec = list(mode = "one", v1 = "mr:medios"), clave = "% de respuestas"),
    list(spec = list(mode = "two", v1 = "mr:medios", v2 = "genero"), clave = "asteriscos"),
    list(spec = list(mode = "three", v1 = "internet", v2 = "educacion", v3 = "genero"), clave = "variable de control"))
  for (k in casos) {
    r <- explore_from_spec(d, k$spec)
    h <- as.character(explain_explore(d, k$spec, r))
    expect_match(h, k$clave, fixed = TRUE, info = paste(unlist(k$spec), collapse = " "))
    expect_match(h, "Qué se analiza", info = paste(unlist(k$spec), collapse = " "))
  }
  # los valores son los del resultado
  r <- explore_from_spec(d, list(mode = "two", v1 = "internet", v2 = "educacion"))
  expect_match(as.character(explain_explore(d, list(mode = "two", v1 = "internet", v2 = "educacion"), r)), fmt_num(r$test$v), fixed = TRUE)
})

test_that("«Sobre tus datos» explica cada tipo de red: papeles de las variables, nodos y enlaces", {
  d <- read_any(demo_sav())
  redes <- list(
    list(spec = list(type = "coincidencias", vars = c("genero", "educacion", "internet"), community = "louvain"), clave = c("tamaño", "más de lo esperado", "comunidades")),
    list(spec = list(type = "correlaciones", vars = c("edad", "ingresos", "ideologia")), clave = c("correlacionadas", "grosor")),
    list(spec = list(type = "factorial", ftype = "mca", vars = c("genero", "educacion", "internet"), passive = "edad", nclusters = "3"), clave = c("activas", "pasivas", "conglomerado")),
    list(spec = list(type = "loglineal", vars = c("genero", "educacion", "internet")), clave = c("log-lineal", "estimación")),
    list(spec = list(type = "regresion", dep = "internet", vars = c("genero", "edad")), clave = c("dependiente", "independientes")),
    list(spec = list(type = "trayectorias", model = "conf =~ confianza_ciencia + satisfaccion + ideologia\nconf ~ edad"), clave = c("latentes", "CFI", "dependientes")))
  for (k in redes) {
    net <- suppressWarnings(suppressMessages(net_build(d, k$spec)))
    h <- as.character(explain_network(d, k$spec, net))
    for (cl in k$clave) expect_match(h, cl, fixed = TRUE, info = paste(k$spec$type, cl))
  }
  e <- new.env(); utils::data("sociologists", package = "netCoin", envir = e)
  sp <- list(type = "evolucion", name = "name", start = "birth", end = "death", group = "school")
  expect_match(as.character(explain_network(e$sociologists, sp, net_build(e$sociologists, sp))), "periodo")
})
