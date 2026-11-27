datos_vg <- function() {
  d <- data.frame(P3A = factor(c("Sí", "No", "Sí")), P3B = factor(c("No", "No", "Sí")), P3C = factor(c("Sí", "Sí", "No")),
                  edad = c(20, 40, 60), sexo = factor(c("Hombre", "Mujer", "Mujer")), ingresos = c(1, 2, 3))
  attr(d$edad, "label") <- "Edad"
  d
}

test_that("grupos de variables: una variable puede estar en varios y viajan por la receta", {
  d <- datos_vg()
  g <- list(op = "vgroups", groups = list(list(name = "Socio", vars = c("edad", "sexo")),
                                          list(name = "Edad y P3", vars = c("edad", "P3A", "P3B", "P3C"))))
  x <- apply_recipe(d, list(g, list(op = "filter", expr = "edad > 25"), list(op = "compute", to = "e2", expr = "edad * 2")))
  expect_equal(names(vgroups(x)), c("Socio", "Edad y P3"))
  expect_equal(vgroup_vars(x, c("Socio", "Edad y P3")), c("P3A", "P3B", "P3C", "edad", "sexo"))
  expect_equal(nrow(x), 2)
  # renombrar y eliminar actualizan los grupos
  x <- apply_recipe(d, list(g, list(op = "rename", var = "edad", to = "anios"), list(op = "drop", vars = "P3C")))
  expect_equal(vgroups(x)$Socio$vars, c("anios", "sexo"))
  expect_equal(vgroups(x)[["Edad y P3"]]$vars, c("anios", "P3A", "P3B"))
  # errores
  expect_error(apply_step(d, list(op = "vgroups", groups = list(list(name = "A", vars = "nada")))), "no existe")
  expect_error(apply_step(d, list(op = "vgroups", groups = list(list(name = "A", vars = "edad"), list(name = "A", vars = "sexo")))), "dos grupos")
  # un paso sin grupos los quita
  expect_length(vgroups(apply_recipe(d, list(g, list(op = "vgroups", groups = list())))), 0)
})

test_that("variables ocultas: siguen en los datos y no se ofrecen", {
  d <- datos_vg()
  h <- list(op = "hidden", vars = c("ingresos", "P3C"))
  x <- apply_recipe(d, list(h, list(op = "filter", expr = "edad > 25")))
  expect_true(all(c("ingresos", "P3C") %in% names(x)))
  expect_equal(hidden_vars(x), c("ingresos", "P3C"))
  expect_false(any(c("ingresos", "P3C") %in% shown_vars(x)))
  x <- apply_recipe(d, list(h, list(op = "rename", var = "ingresos", to = "renta"), list(op = "drop", vars = "P3C")))
  expect_equal(hidden_vars(x), "renta")
})

test_that("renombrar con expresión regular", {
  d <- datos_vg()
  m <- regex_rename_preview(names(d), "^P3(.)$", "p3_\\L$1")
  expect_equal(unname(m[1:3]), c("p3_a", "p3_b", "p3_c"))
  expect_equal(unname(regex_rename_preview("abc", "^(.+)$", "\\U$1")), "ABC")
  expect_null(regex_rename_preview(names(d), "([", ""))
  map <- m[m != names(m)]
  x <- apply_recipe(d, list(list(op = "vgroups", groups = list(list(name = "P3", vars = c("P3A", "P3B")))),
                            list(op = "hidden", vars = "P3C"),
                            list(op = "rename_many", map = as.list(map), pattern = "^P3(.)$", replacement = "p3_\\L$1")))
  expect_equal(names(x)[1:3], c("p3_a", "p3_b", "p3_c"))
  expect_equal(vgroups(x)$P3$vars, c("p3_a", "p3_b"))
  expect_equal(hidden_vars(x), "p3_c")
  # repetidos y no válidos
  expect_equal(unname(rename_problems(names(d), c(P3A = "edad"))), "se repite")
  expect_equal(unname(rename_problems(names(d), c(P3A = "1x"))), "no es un nombre válido")
  expect_error(apply_step(d, list(op = "rename_many", map = list(P3A = "sexo"))), "se repite")
})

test_that("ordenar variables", {
  d <- datos_vg()
  x <- apply_step(d, list(op = "order", vars = order_by(d, "az")))
  expect_equal(names(x), c("edad", "ingresos", "P3A", "P3B", "P3C", "sexo"))
  expect_equal(order_by(d, "regex", patterns = "^s, ^e"), c("sexo", "edad", "P3A", "P3B", "P3C", "ingresos"))
  expect_equal(order_by(d, "regex", patterns = "^P3", top = FALSE), c("edad", "sexo", "ingresos", "P3A", "P3B", "P3C"))
  expect_error(order_by(d, "regex", patterns = "^zz"), "Ninguna")
  # las que se crean después siguen detrás
  x <- apply_recipe(d, list(list(op = "order", vars = c("sexo", "edad")), list(op = "compute", to = "nueva", expr = "edad")))
  expect_equal(names(x)[c(1, 2, 7)], c("sexo", "edad", "nueva"))
  # conserva las etiquetas y los grupos
  x <- apply_recipe(d, list(list(op = "vgroups", groups = list(list(name = "G", vars = "edad"))), list(op = "order", vars = "sexo")))
  expect_equal(attr(x$edad, "label"), "Edad")
  expect_equal(vgroups(x)$G$vars, "edad")
})

test_that("grupos propuestos por prefijo", {
  sug <- vgroup_suggest(datos_vg())
  expect_equal(sug[[1]]$name, "P3")
  expect_equal(sug[[1]]$vars, c("P3A", "P3B", "P3C"))
})

test_that("los pasos nuevos se describen y dan un script que funciona", {
  d <- datos_vg()
  rec <- list(list(op = "vgroups", groups = list(list(name = "Socio", vars = c("edad", "sexo")))),
              list(op = "hidden", vars = "ingresos"),
              list(op = "rename_many", map = list(P3A = "p3a", P3B = "p3b"), pattern = "^P3", replacement = "p3"),
              list(op = "order", vars = c("sexo", "edad"), how = "a mano"))
  for (s in rec) expect_true(nzchar(step_label(s)))
  code <- recipe_to_code(rec, "x.sav")
  env <- new.env(); env$datos <- d
  for (l in strsplit(code, "\n")[[1]]) if (grepl("^datos <- caring2::read_any", l)) next
  expr <- parse(text = sub("datos <- caring2::read_any\\([^)]*\\)", "", code))
  eval(expr, env)
  expect_equal(names(env$datos)[1:2], c("sexo", "edad"))
  expect_true(all(c("p3a", "p3b") %in% names(env$datos)))
  expect_equal(env$grupos$Socio, c("edad", "sexo"))
  expect_equal(env$ocultas, "ingresos")
})
