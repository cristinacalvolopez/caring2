# Genera inst/extdata/demo_encuesta.sav: una encuesta ficticia con etiquetas SPSS,
# perdidos codificados (99) y una estructura de asociaciones para que las redes
# muestren algo interpretable. Ejecutar desde la raíz del repo:
#   Rscript data-raw/demo_encuesta.R

set.seed(2024)
n <- 1200

genero <- sample(1:2, n, TRUE, prob = c(.52, .48))
edad <- round(pmin(pmax(rnorm(n, 45, 16), 18), 85))
grupo_edad_lat <- cut(edad, c(0, 34, 54, Inf), labels = FALSE)

# educación depende de la edad
p_sup <- c(.45, .35, .18)[grupo_edad_lat]
educacion <- vapply(p_sup, function(p) {
  sample(1:5, 1, prob = c((1 - p) * .35, (1 - p) * .45, (1 - p) * .2, p * .7, p * .3))
}, numeric(1))

ingresos <- round(exp(rnorm(n, 3.2 + 0.18 * educacion, 0.45)))
ingresos[sample(n, 40)] <- 99999  # código "No contesta"

internet <- ifelse(runif(n) < plogis(3 - 0.06 * (edad - 45) + 0.4 * (educacion - 3)), 1, 2)
redes_sociales <- ifelse(internet == 1 & runif(n) < plogis(1.5 - 0.07 * (edad - 40)), 1, 2)
prensa <- ifelse(runif(n) < plogis(-0.8 + 0.035 * (edad - 45) + 0.35 * (educacion - 3)), 1, 2)
tv <- ifelse(runif(n) < plogis(0.5 + 0.04 * (edad - 45)), 1, 2)

satisfaccion <- pmin(pmax(round(3 + 0.25 * (educacion - 3) + rnorm(n, 0, 1.1)), 1), 5)
ideologia <- pmin(pmax(round(5 + rnorm(n, 0, 2) + 0.02 * (edad - 45)), 1), 10)
ideologia[sample(n, 60)] <- 98    # "No sabe"
confianza_ciencia <- pmin(pmax(round(6 + 0.4 * (educacion - 3) + rnorm(n, 0, 2)), 0), 10)

region <- sample(1:4, n, TRUE, prob = c(.3, .25, .25, .2))
habitat <- sample(1:3, n, TRUE)
peso <- round(runif(n, 0.6, 1.5), 3)

# respuesta múltiple: aficiones separadas por ";"
aficiones_pool <- c("deporte", "lectura", "música", "cine", "viajes", "videojuegos")
aficiones <- vapply(seq_len(n), function(i) {
  pr <- c(.4, .3 + .05 * (educacion[i] - 3), .5, .4, .25 + .05 * (educacion[i] - 3),
          ifelse(edad[i] < 35, .5, .1))
  sel <- aficiones_pool[runif(6) < pr]
  paste(sel, collapse = ";")
}, character(1))

lab <- function(x, labels, label, na = NULL) {
  haven::labelled_spss(x, labels = labels, label = label, na_values = na)
}

d <- data.frame(id = seq_len(n))
d$genero <- lab(genero, c(Mujer = 1, Hombre = 2), "Género")
d$edad <- edad; attr(d$edad, "label") <- "Edad"
d$educacion <- lab(educacion, c("Sin estudios" = 1, "Secundaria" = 2, "Bachillerato o FP" = 3,
                                "Universitarios" = 4, "Postgrado" = 5), "Nivel educativo")
d$ingresos <- lab(ingresos, c("No contesta" = 99999), "Ingresos mensuales (cientos de euros)", na = 99999)
d$internet <- lab(internet, c("Sí" = 1, "No" = 2), "Usa internet")
d$redes_sociales <- lab(redes_sociales, c("Sí" = 1, "No" = 2), "Usa redes sociales")
d$prensa <- lab(prensa, c("Sí" = 1, "No" = 2), "Lee prensa")
d$tv <- lab(tv, c("Sí" = 1, "No" = 2), "Ve informativos en TV")
d$satisfaccion <- lab(satisfaccion, c("Muy insatisfecho" = 1, "Insatisfecho" = 2, "Neutral" = 3,
                                      "Satisfecho" = 4, "Muy satisfecho" = 5), "Satisfacción con la vida")
d$ideologia <- lab(ideologia, c("Izquierda" = 1, "Derecha" = 10, "No sabe" = 98),
                   "Autoubicación ideológica (1-10)", na = 98)
d$confianza_ciencia <- lab(confianza_ciencia, c("Ninguna" = 0, "Total" = 10), "Confianza en la ciencia (0-10)")
d$region <- lab(region, c(Norte = 1, Sur = 2, Este = 3, Oeste = 4), "Región")
d$habitat <- lab(habitat, c(Rural = 1, Intermedio = 2, Urbano = 3), "Tamaño de hábitat")
d$aficiones <- aficiones
attr(d$aficiones, "label") <- "Aficiones (respuesta múltiple)"
d$peso <- peso
attr(d$peso, "label") <- "Factor de ponderación"

dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
haven::write_sav(d, "inst/extdata/demo_encuesta.sav")
message("Escrito inst/extdata/demo_encuesta.sav (", n, " casos, ", ncol(d), " variables)")
