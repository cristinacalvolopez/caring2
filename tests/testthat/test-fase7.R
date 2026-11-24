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
  for (k in c("cat", "num", "catcat", "numcat", "numnum", "mr", "mrcat", "catcat3", "numcat3")) expect_false(is.null(explore_help(k)), info = k)
  expect_null(explore_help("texto"))
  for (w in c("inicio", "proyecto")) {
    st <- tour_steps(w)
    expect_gt(length(st), 3)
    expect_true(all(vapply(st, function(x) nzchar(x$sel) && nzchar(x$title) && nzchar(x$text), logical(1))))
  }
})
