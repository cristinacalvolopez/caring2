test_that("operaciones de la receta", {
  d <- read_any(demo_sav())
  r <- list(
    list(op = "rename", var = "genero", to = "sexo"),
    list(op = "label", var = "sexo", label = "Sexo"),
    list(op = "recode", var = "educacion", to = "educ3", others = "na",
         groups = list(list(name = "Básicos", from = c("Sin estudios", "Secundaria")),
                       list(name = "Medios", from = "Bachillerato o FP"),
                       list(name = "Superiores", from = c("Universitarios", "Postgrado")))),
    list(op = "cut", var = "edad", to = "edad3", breaks = c(35, 55)),
    list(op = "missing", var = "satisfaccion", values = "Neutral"),
    list(op = "compute", to = "ingresos_log", expr = "log(ingresos)", label = "Log ingresos"),
    list(op = "compute", to = "joven", expr = "edad < 35"),
    list(op = "type", var = "region", type = "texto"),
    list(op = "filter", expr = "edad >= 18 & !is.na(ingresos)"),
    list(op = "drop", vars = "id")
  )
  out <- apply_recipe(d, r)
  expect_true("sexo" %in% names(out))
  expect_equal(attr(out$sexo, "label"), "Sexo")
  expect_equal(levels(out$educ3), c("Básicos", "Medios", "Superiores"))
  expect_equal(nlevels(out$edad3), 3)
  expect_true(is.ordered(out$edad3))
  expect_false("Neutral" %in% levels(out$satisfaccion))
  expect_true(is.numeric(out$ingresos_log))
  expect_equal(levels(out$joven), c("No", "Sí"))
  expect_true(is.character(out$region))
  expect_false(any(is.na(out$ingresos)))
  expect_false("id" %in% names(out))

  code <- recipe_to_code(r, "demo.sav")
  expect_match(code, "read_any")
  expect_match(code, "educ3")
})

test_that("el código generado reproduce el resultado", {
  d <- read_any(demo_sav())
  r <- list(
    list(op = "recode", var = "educacion", to = "educ2", others = "na",
         groups = list(list(name = "Sin título", from = c("Sin estudios", "Secundaria", "Bachillerato o FP")),
                       list(name = "Con título", from = c("Universitarios", "Postgrado")))),
    list(op = "compute", to = "x2", expr = "edad * 2"),
    list(op = "rename", var = "x2", to = "doble_edad")
  )
  esperado <- apply_recipe(d, r)
  code <- recipe_to_code(r)
  code <- sub("datos <- caring2::read_any\\([^)]*\\)", "", code)
  env <- new.env()
  env$datos <- d
  eval(parse(text = code), env)
  expect_equal(as.character(env$datos$educ2), as.character(esperado$educ2))
  expect_equal(env$datos$doble_edad, esperado$doble_edad)
})

test_that("las fórmulas no permiten código arbitrario", {
  d <- data.frame(a = 1:3)
  expect_error(eval_safe("system('ls')", d), "no permitida")
  expect_error(eval_safe("a + b", d), "No existe")
  expect_error(eval_safe("get('a')", d), "no permitida")
  expect_equal(eval_safe("a * 2", d), c(2, 4, 6))
  expect_equal(eval_safe("mean(a)", d), 2)
})

test_that("errores claros", {
  d <- data.frame(a = 1:3, b = 4:6)
  expect_error(apply_step(d, list(op = "rename", var = "a", to = "b")), "Ya existe")
  expect_error(apply_step(d, list(op = "rename", var = "a", to = "1x")), "no es válido")
  expect_error(apply_step(d, list(op = "rename", var = "z", to = "y")), "no existe")
})
