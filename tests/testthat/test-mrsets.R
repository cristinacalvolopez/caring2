# Conjuntos de respuesta múltiple

cuestionario <- function(n = 400) {
  set.seed(3)
  opciones <- c("Televisión", "Prensa", "Radio", "Redes sociales", "Podcast")
  edad <- round(runif(n, 18, 80))
  joven <- edad < 35
  marca <- sapply(opciones, function(o) {
    p <- if (o %in% c("Redes sociales", "Podcast")) ifelse(joven, .7, .2) else ifelse(joven, .3, .6)
    runif(n) < p
  })
  d <- data.frame(id = seq_len(n), edad = edad,
                  sexo = factor(sample(c("Mujer", "Hombre"), n, TRUE)),
                  grupo_edad = factor(ifelse(joven, "18-34", "35+"), levels = c("18-34", "35+")))
  # p5_*: dicotomías Sí/No (factor) · p6_*: dicotomías 1/0 (numéricas)
  for (j in seq_along(opciones)) {
    d[[paste0("p5_", j)]] <- factor(ifelse(marca[, j], "Sí", "No"), levels = c("Sí", "No"))
    attr(d[[paste0("p5_", j)]], "label") <- opciones[j]
    d[[paste0("p6_", j)]] <- as.numeric(marca[, j])
  }
  # p7_1..3: hasta tres menciones con categorías
  menc <- t(apply(marca, 1, function(r) c(opciones[r], NA, NA, NA)[1:3]))
  for (k in 1:3) d[[paste0("p7_", k)]] <- factor(menc[, k], levels = opciones)
  d$p5_1[1:5] <- NA; for (j in 2:5) d[[paste0("p5_", j)]][1:5] <- NA     # 5 casos sin respuesta
  d$texto <- apply(marca, 1, function(r) paste(opciones[r], collapse = "; "))
  d
}

test_that("los tres tipos de conjunto dan la misma información", {
  d <- cuestionario()
  r <- list(
    list(op = "mrset", name = "medios", label = "Medios", type = "dicotomias", vars = paste0("p5_", 1:5), value = "Sí"),
    list(op = "mrset", name = "medios01", type = "dicotomias", vars = paste0("p6_", 1:5), value = "1"),
    list(op = "mrset", name = "menciones", type = "categorias", vars = paste0("p7_", 1:3)),
    list(op = "mrset", name = "texto", type = "texto", vars = "texto", sep = ";"))
  x <- apply_recipe(d, r)
  expect_equal(names(mrsets(x)), c("medios", "medios01", "menciones", "texto"))
  m1 <- mr_incidences(x, mrsets(x)$medios)
  expect_equal(names(m1), c("Televisión", "Prensa", "Radio", "Redes sociales", "Podcast"))  # etiquetas de las variables
  expect_true(all(is.na(m1[1:5, ])))                                                     # sin respuesta = NA
  m2 <- mr_incidences(x, mrsets(x)$medios01)
  expect_equal(unname(as.matrix(m1[-(1:5), ])), unname(as.matrix(m2[-(1:5), ])))
  m4 <- mr_incidences(x, mrsets(x)$texto)
  expect_setequal(names(m4), names(m1))
  f <- mr_freq(x, mrsets(x)$medios)
  expect_equal(attr(f, "casos"), 395)
  expect_equal(sum(f$pct_resp), 1)
  expect_gt(sum(f$pct_casos), 1)                                                        # % de casos suma más de 100
})

test_that("errores claros al definir conjuntos", {
  d <- cuestionario()
  expect_error(apply_step(d, list(op = "mrset", name = "1x", type = "dicotomias", vars = c("p5_1", "p5_2"), value = "Sí")), "no es válido")
  expect_error(apply_step(d, list(op = "mrset", name = "a", type = "dicotomias", vars = "p5_1", value = "Sí")), "al menos dos")
  expect_error(apply_step(d, list(op = "mrset", name = "a", type = "dicotomias", vars = c("p5_1", "p5_2"), value = "Quizá")), "Ninguna")
  expect_error(apply_step(d, list(op = "mrset", name = "a", type = "dicotomias", vars = c("p5_1", "nada"))), "no existe")
})

test_that("sugerencias automáticas por prefijo", {
  d <- cuestionario()
  s <- mr_suggest(d)
  nombres <- vapply(s, `[[`, "", "name")
  expect_true(all(c("p5", "p6", "p7") %in% nombres))
  expect_equal(s[[which(nombres == "p5")]]$value, "Sí")
  expect_equal(s[[which(nombres == "p6")]]$type, "dicotomias")
  expect_equal(s[[which(nombres == "p7")]]$type, "categorias")
  x <- apply_step(d, list(op = "mrset", name = "medios", type = "dicotomias", vars = paste0("p5_", 1:5), value = "Sí"))
  expect_false("p5" %in% vapply(mr_suggest(x), `[[`, "", "name"))                         # ya usado: no se sugiere
})

test_that("los conjuntos sobreviven a renombrar, filtrar y eliminar variables", {
  d <- cuestionario()
  r <- list(
    list(op = "mrset", name = "medios", type = "dicotomias", vars = paste0("p5_", 1:5), value = "Sí"),
    list(op = "rename", var = "p5_1", to = "tv"),
    list(op = "filter", expr = "edad >= 20"),
    list(op = "compute", to = "doble", expr = "edad * 2"),
    list(op = "drop", vars = "p5_5"))
  x <- apply_recipe(d, r)
  s <- mrsets(x)$medios
  expect_equal(s$vars, c("tv", "p5_2", "p5_3", "p5_4"))
  expect_equal(nrow(mr_incidences(x, s)), nrow(x))
  y <- apply_recipe(x, list(list(op = "drop", vars = c("tv", "p5_2", "p5_3"))))
  expect_null(mrsets(y)$medios)                                                         # queda con 1 variable: desaparece
  z <- apply_recipe(d, c(r[1], list(list(op = "mrset_drop", name = "medios"))))
  expect_length(mrsets(z), 0)
})

test_that("Explorar: frecuencias y cruce con una categórica", {
  d <- apply_recipe(cuestionario(), list(list(op = "mrset", name = "medios", label = "Medios", type = "dicotomias",
                                              vars = paste0("p5_", 1:5), value = "Sí")))
  r <- explore_mr(d, "mr:medios")
  expect_equal(r$title, "Medios")
  expect_true(all(c("Opción", "n", "% de casos", "% de respuestas") %in% names(r$table)))
  expect_equal(tail(r$table$`% de respuestas`, 1), "100,0 %")
  for (ch in chart_choices("mr")) expect_s3_class(explore_mr(d, "mr:medios", chart = ch)$plot, "ggplot")
  c2 <- explore_mr_cat(d, "mr:medios", "grupo_edad", weight = NULL)
  expect_true(all(c("18-34", "35+", "Total") %in% names(c2$table)))
  expect_equal(tail(c2$table[[1]], 1), "Casos (n)")
  expect_match(c2$table[[1]][4], "\\*")                                                  # Redes sociales difiere por edad
  expect_match(c2$stats, "difieren")
  for (ch in chart_choices("mrcat")) {
    f <- tempfile(fileext = ".png")
    suppressMessages(save_plot_png(explore_mr_cat(d, "mr:medios", "sexo", chart = ch)$plot, f))
    expect_gt(file.size(f), 5000)
  }
  expect_error(explore_mr_cat(d, "mr:medios", "edad"), "categórica")
  expect_equal(chart_kind(d, "mr:medios"), "mr")
  expect_equal(chart_kind(d, "sexo", "mr:medios"), "mrcat")
})

test_that("Redes: uno o varios conjuntos, cruzados con otras variables", {
  d <- apply_recipe(cuestionario(), list(
    list(op = "mrset", name = "medios", label = "Medios", type = "dicotomias", vars = paste0("p5_", 1:5), value = "Sí"),
    list(op = "mrset", name = "menc", label = "Menciones", type = "categorias", vars = paste0("p7_", 1:3))))
  n1 <- net_build(d, list(type = "multiple", vars = "mr:medios"))
  expect_true(inherits(n1, "netCoin"))
  n2 <- net_build(d, list(type = "multiple", vars = "mr:medios", cross = c("grupo_edad", "sexo"), weight = NULL))
  grupos <- unique(n2$nodes$grupo)
  expect_true(all(c("Medios", "grupo_edad", "sexo") %in% grupos) || length(grupos) == 3)
  enl <- paste(n2$links[[1]], n2$links[[2]])
  expect_true(any(grepl("Redes sociales", enl) & grepl("18-34", enl)))                  # la asociación que hemos simulado
  n3 <- net_build(d, list(type = "multiple", vars = c("mr:medios", "mr:menc"), signif = FALSE))
  expect_true(any(grepl("^Medios: ", n3$nodes[[1]])))
  n4 <- net_build(d, list(type = "multiple", vars = "texto", sep = ";"))                  # variable de texto (como antes)
  expect_true(inherits(n4, "netCoin"))
  d$peso <- runif(nrow(d), .5, 1.5)
  expect_true(inherits(net_build(d, list(type = "multiple", vars = "mr:medios", weight = "peso", cross = "sexo")), "netCoin"))
  expect_error(net_build(d, list(type = "multiple", vars = character())), "al menos un conjunto")
  expect_error(net_build(d, list(type = "multiple", vars = "mr:medios", cross = "edad")), "categórica")
})

test_that("el script de R reproduce los conjuntos", {
  d <- cuestionario()
  r <- list(list(op = "mrset", name = "medios", type = "dicotomias", vars = paste0("p5_", 1:5), value = "Sí"),
            list(op = "mrset", name = "menc", type = "categorias", vars = paste0("p7_", 1:3)))
  code <- sub("datos <- caring2::read_any\\([^)]*\\)", "", recipe_to_code(r))
  env <- new.env(); env$datos <- d
  eval(parse(text = code, encoding = "UTF-8"), env)
  x <- apply_recipe(d, r)
  esperado <- mr_incidences(x, mrsets(x)$medios)
  obtenido <- env$conjuntos$medios
  # la misma tabla que usa CARING, con los mismos nombres de opción y los mismos NA
  expect_equal(obtenido, esperado)
  expect_equal(env$conjuntos$menc, mr_incidences(x, mrsets(x)$menc))
})

test_that("SPSS con codificaciones mezcladas: el valor marcado cuenta por etiqueta y por código", {
  set.seed(2); n <- 300
  raw <- data.frame(
    p5_1 = haven::labelled(ifelse(runif(n) < .4, 1, 2), c("Sí" = 1, "No" = 2), label = "Televisión"),
    p5_2 = haven::labelled(c(ifelse(runif(n - 3) < .4, 1, 2), 9, 9, 9), c("Sí" = 1, "No" = 2), label = "Prensa"),
    p5_3 = haven::labelled(ifelse(runif(n) < .3, 1, NA), c("Sí" = 1), label = "Radio"))
  f <- tempfile(fileext = ".sav"); haven::write_sav(raw, f)
  d <- read_any(f)
  expect_true(is.factor(d$p5_1)); expect_true(is.numeric(d$p5_2))      # la mezcla que provocaba el error
  x <- apply_recipe(d, list(list(op = "mrset", name = "m", type = "dicotomias", vars = c("p5_1", "p5_2", "p5_3"), value = "Sí")))
  inc <- mr_incidences(x, mrsets(x)$m)
  expect_true(all(colSums(inc, na.rm = TRUE) > 0))
  expect_equal(sum(inc$Prensa, na.rm = TRUE), sum(raw$p5_2 == 1))
  expect_true(inherits(net_build(x, list(type = "multiple", vars = "mr:m")), "netCoin"))
  # los códigos sobreviven a un filtro
  y <- apply_recipe(d, list(list(op = "filter", expr = "!is.na(p5_1)"),
                            list(op = "mrset", name = "m", type = "dicotomias", vars = c("p5_1", "p5_2"), value = "Sí")))
  expect_true(all(colSums(mr_incidences(y, mrsets(y)$m), na.rm = TRUE) > 0))
  # y el mensaje dice qué opciones están vacías cuando de verdad lo están
  z <- apply_recipe(d, list(list(op = "mrset", name = "m", type = "dicotomias", vars = c("p5_1", "p5_2"), value = "No")))
  expect_length(mr_empty_options(z, mrsets(z)$m)$opciones, 0)
})

test_that("si todas las variables comparten etiqueta, las opciones se nombran por variable", {
  set.seed(1); n <- 200
  raw <- as.data.frame(setNames(lapply(1:4, function(j) haven::labelled(ifelse(runif(n) < .4, 1, NA), c("Sí" = 1),
                                                                        label = "¿Qué medios usa?")), paste0("p5_", 1:4)))
  f <- tempfile(fileext = ".sav"); haven::write_sav(raw, f)
  x <- apply_recipe(read_any(f), list(list(op = "mrset", name = "m", type = "dicotomias", vars = paste0("p5_", 1:4), value = "Sí")))
  expect_equal(names(mr_incidences(x, mrsets(x)$m)), paste0("p5_", 1:4))
  d2 <- data.frame(a = factor(c("Sí", "No")), b = factor(c("No", "Sí")), c = factor(c("Sí", "Sí")))
  attr(d2$a, "label") <- "Medios"; attr(d2$b, "label") <- "Medios"; attr(d2$c, "label") <- "Radio"
  expect_equal(mr_option_names(d2, c("a", "b", "c")), c("Medios (a)", "Medios (b)", "Radio"))
})

test_that("mensaje claro si la red se queda sin opciones", {
  d <- data.frame(a = factor(c("Sí", "No", "No")), b = factor(c("No", "No", "No"), levels = c("Sí", "No")))
  attr(d$a, "label") <- "Uno"; attr(d$b, "label") <- "Dos"
  x <- apply_recipe(d, list(list(op = "mrset", name = "m", type = "dicotomias", vars = c("a", "b"), value = "Sí")))
  expect_error(net_build(x, list(type = "multiple", vars = "mr:m")), "Sin ninguna respuesta: Dos")
  expect_equal(mr_empty_options(x, mrsets(x)$m)$opciones, "Dos")
})

test_that("juntar opciones de un conjunto en una nueva (con nombre nuevo o el de una de ellas)", {
  d <- read_any(demo_sav())
  base <- list(op = "mrset", name = "af", type = "texto", vars = "aficiones", sep = ";")
  x <- apply_recipe(d, list(base))
  m0 <- mr_incidences(x, mrsets(x)$af)
  # cine + música -> «Cultura»; un caso cuenta si marcó cualquiera de las dos
  j <- list(op = "mrset_merge", name = "af", merge = list(list(name = "Cultura", from = c("cine", "música")),
                                                         list(name = "", from = c("viajes", "deporte"))))
  y <- apply_recipe(d, list(base, j))
  m <- mr_incidences(y, mrsets(y)$af)
  expect_equal(names(m), c("Cultura", "viajes", "lectura", "videojuegos"))    # va en el sitio de la primera; sin nombre, el de la primera
  expect_equal(sum(m$Cultura, na.rm = TRUE), sum(pmax(m0$cine, m0[["música"]]), na.rm = TRUE))
  expect_equal(sum(is.na(m$Cultura)), sum(is.na(m0$cine)))
  f <- mr_freq(y, mrsets(y)$af)
  expect_true("Cultura" %in% f$opcion)
  # con el nombre de una de ellas
  z <- apply_recipe(d, list(base, list(op = "mrset_merge", name = "af", merge = list(list(name = "cine", from = c("cine", "videojuegos"))))))
  expect_equal(names(mr_incidences(z, mrsets(z)$af))[1], "cine")
  # una sola opción: cambiarle el nombre
  r <- apply_recipe(d, list(base, list(op = "mrset_merge", name = "af", merge = list(list(name = "Leer", from = "lectura")))))
  expect_true("Leer" %in% names(mr_incidences(r, mrsets(r)$af)))
  # errores
  expect_error(apply_recipe(d, list(base, list(op = "mrset_merge", name = "af", merge = list(list(name = "X", from = "nada"))))), "no tiene la opción")
  expect_error(apply_recipe(d, list(base, list(op = "mrset_merge", name = "af",
    merge = list(list(name = "A", from = "cine"), list(name = "B", from = "cine"))))), "dos grupos")
  expect_error(apply_recipe(d, list(base, list(op = "mrset_merge", name = "af", merge = list(list(name = "viajes", from = "cine"))))), "Ya hay una opción")
  expect_error(apply_recipe(d, list(base, list(op = "mrset_merge", name = "otro", merge = list()))), "No existe")
  # sin uniones vuelve a las opciones originales
  w <- apply_recipe(d, list(base, j, list(op = "mrset_merge", name = "af", merge = list())))
  expect_equal(names(mr_incidences(w, mrsets(w)$af)), names(m0))
  # descripción y script: el script da las mismas columnas, también con dos pasos seguidos
  expect_match(step_label(j), "Juntar opciones del conjunto «af»")
  rec <- list(base, j, list(op = "compute", to = "k", expr = "1"),
              list(op = "mrset_merge", name = "af", merge = list(list(name = "Cultura", from = c("cine", "música", "lectura")))))
  code <- recipe_to_code(rec, "x.sav")
  env <- new.env(); env$datos <- d
  eval(parse(text = sub("datos <- caring2::read_any\\([^)]*\\)", "", code)), env)
  final <- apply_recipe(d, rec)
  expect_equal(colnames(env$conjuntos$af), names(mr_incidences(final, mrsets(final)$af)))
})
