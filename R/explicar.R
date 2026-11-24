# «Sobre tus datos» --------------------------------------------------------------------------------
# Explicación del resultado que se está viendo (en Explorar y en Redes): qué variables entran y con
# qué papel, cómo leer la tabla y el gráfico o la red, y qué significa cada estadístico calculado,
# con sus valores. Se abre desde el botón «Sobre tus datos» de cada resultado.

ex_sec <- function(titulo, ...) {
  cont <- Filter(Negate(is.null), list(...))
  if (!length(cont)) return(NULL)
  tags$section(class = "c-about-sec", tags$h3(titulo), cont)
}
ex_p <- function(...) p(HTML(paste0(...)))
ex_var <- function(d, v) {
  if (is.null(v) || !nzchar(v)) return("")
  if (startsWith(v, "mr:")) {
    s <- mrsets(d)[[sub("^mr:", "", v)]]
    return(paste0("<strong>", htmltools::htmlEscape(if (is.null(s)) v else mr_label(s)), "</strong> (respuesta múltiple)"))
  }
  l <- var_label(d, v)
  paste0("<strong>", htmltools::htmlEscape(l), "</strong>", if (!identical(l, v)) paste0(" <code>", htmltools::htmlEscape(v), "</code>") else "")
}
ex_vars <- function(d, vs) paste(vapply(vs, function(v) ex_var(d, v), ""), collapse = ", ")
fuerza_v <- function(v) if (is.na(v)) "" else paste0(" (relación ", strength(v), ")")

# Qué muestra cada tipo de gráfico de Explorar
chart_text <- c(
  barras_h = "Cada barra es una categoría y su longitud, el porcentaje de casos.",
  barras_v = "Cada barra es una categoría y su altura, el porcentaje de casos.",
  sectores = "Cada sector es una categoría; su tamaño es su parte del total (todo el círculo es el 100 %).",
  anillo = "Cada trozo del anillo es una categoría; su tamaño es su parte del total (todo el anillo es el 100 %).",
  puntos = "Cada punto marca el porcentaje de una categoría; la línea lo une con el cero.",
  histograma = "Cada barra agrupa los casos con valores en un intervalo; su altura es el número de casos. La línea discontinua es la media.",
  hist_dens = "Las barras agrupan los valores en intervalos y la curva suaviza su forma (densidad). La línea discontinua es la media.",
  densidad = "La curva muestra dónde se concentran los valores: más alta donde hay más casos. La línea discontinua es la media.",
  caja = "La caja va del percentil 25 al 75 (la mitad central de los casos), la raya de dentro es la mediana y el rombo, la media. Los bigotes llegan a los valores normales y los puntos sueltos son valores alejados.",
  violin = "La anchura del violín indica cuántos casos tienen cada valor; dentro hay una caja con la mediana y la mitad central de los casos.",
  agrupadas = "Las barras de cada grupo se ponen una al lado de otra para comparar porcentajes entre grupos.",
  apiladas = "Cada barra es un grupo y sus trozos, cómo se reparte entre las categorías (cada barra suma 100 %).",
  calor = "Cada casilla es una combinación de categorías; cuanto más oscura, mayor el porcentaje.",
  burbujas = "Cada burbuja es una combinación de categorías; su tamaño es el porcentaje.",
  mosaico = "El ancho de cada columna es el peso de su grupo y el alto de cada bloque, el porcentaje dentro del grupo: el área es proporcional al número de casos.",
  medias = "Cada punto es la media de un grupo y las barras, su intervalo de confianza del 95 %: si dos intervalos no se solapan, las medias probablemente difieren.",
  densidades = "Una curva por grupo: permite comparar dónde se concentran los valores en cada grupo.",
  recta = "Cada punto es un caso; la recta resume la tendencia lineal (con su margen de error sombreado).",
  curva = "Cada punto es un caso; la curva resume la tendencia sin suponer que sea recta.",
  densidad2d = "La cuadrícula cuenta los casos de cada zona: más oscura donde hay más casos.",
  tramos = "Cada punto es la media de la variable vertical en un décimo de la horizontal, con su intervalo de confianza del 95 %.")

explain_explore <- function(d, spec, r) {
  if (is.null(r) || !is.null(r$error)) return(p("No hay ningún resultado que explicar."))
  modo <- spec$mode %||% "one"
  pond <- if (length(spec$weight) && nzchar(spec$weight)) paste0(" Los resultados están ponderados por ", ex_var(d, spec$weight),
    ": cada caso cuenta según su peso, para que la muestra se parezca a la población.") else ""
  k <- r$kind %||% (if (modo == "one") chart_kind(d, spec$v1) else chart_kind(d, spec$v1, spec$v2))
  if (modo == "three") k <- r$kind
  que <- switch(modo,
    one = ex_p("Se describe una variable: ", ex_var(d, spec$v1), ".", pond),
    two = ex_p("Se cruzan dos variables: ", ex_var(d, spec$v1), " (en filas) y ", ex_var(d, spec$v2), " (en columnas).", pond),
    ex_p("Se cruzan ", ex_var(d, spec$v1), " (filas) y ", ex_var(d, spec$v2), " (columnas) por separado dentro de cada categoría de ",
         ex_var(d, spec$v3), ", la <em>variable de control</em>.", pond))
  pct <- spec$pct %||% "col"
  tabla <- switch(k,
    cat = ex_p("Una fila por categoría: <strong>n</strong> es el número de casos, <strong>%</strong> el porcentaje sobre todos los casos ",
               "y <strong>% válido</strong> el porcentaje sin contar los perdidos (no respuesta)."),
    num = ex_p("Resumen de la variable: la <strong>media</strong> (promedio), la <strong>mediana</strong> (deja la mitad de los casos por debajo), ",
               "la <strong>desviación típica</strong> (cuánto se alejan los casos de la media, en las mismas unidades), los percentiles 25 y 75 ",
               "(entre ellos está la mitad central de los casos) y los valores mínimo y máximo."),
    catcat = , catcat3 = ex_p(switch(pct,
      col = "Porcentajes <strong>verticales</strong>: cada columna suma 100 %. Para ver si hay relación, compara los porcentajes de una misma fila entre columnas.",
      row = "Porcentajes <strong>horizontales</strong>: cada fila suma 100 %. Para ver si hay relación, compara los porcentajes de una misma columna entre filas.",
      "Porcentajes <strong>sobre el total</strong>: todas las casillas juntas suman 100 %."),
      " La fila o columna <strong>Total</strong> es la distribución de todos los casos juntos, y <strong>n</strong> el número de casos de cada grupo.",
      if (k == "catcat3") " Hay un bloque por cada categoría de la variable de control."),
    numcat = , numcat3 = ex_p("Para cada grupo, el número de casos (<strong>n</strong>), la <strong>media</strong>, la <strong>mediana</strong> y la ",
                              "<strong>desviación típica</strong> de la variable numérica. Compara las medias entre grupos.",
                              if (k == "numcat3") " Hay un bloque por cada categoría de la variable de control."),
    numnum = ex_p("Las correlaciones entre las dos variables, entre −1 y 1: cerca de 0, sin relación; positiva, aumentan juntas; ",
                  "negativa, cuando una sube la otra baja. <strong>Pearson (r)</strong> mide la relación en línea recta y <strong>Spearman (ρ)</strong> ",
                  "cualquier relación que siempre suba o siempre baje."),
    mr = ex_p("En una pregunta de respuesta múltiple cada caso puede marcar varias opciones. <strong>% de casos</strong> es la parte de los casos que ",
              "marca cada opción (suma más de 100 %); <strong>% de respuestas</strong> reparte el total de marcas (suma 100 %)."),
    mrcat = ex_p("Para cada grupo, el porcentaje de casos que marca cada opción; las columnas suman más de 100 % porque se pueden marcar varias. ",
                 "Los asteriscos señalan las opciones cuyo porcentaje cambia entre grupos más de lo que cabría esperar por azar."),
    NULL)
  graf <- if (!is.null(r$plot) && !is.null(spec$chart %||% NULL) && spec$chart %in% names(chart_text)) ex_p(chart_text[[spec$chart]])
          else if (!is.null(r$plot)) {
            ch <- unname(chart_choices(k)[1])
            if (length(ch) && ch %in% names(chart_text)) ex_p(chart_text[[ch]])
          }
  est <- switch(k,
    catcat = {
      t <- r$test
      if (is.null(t)) NULL else tagList(
        ex_p("<strong>χ² (ji cuadrado) = ", fmt_num(t$statistic), "</strong> con ", t$df, " grados de libertad. Compara la tabla con la que habría si las variables ",
             "no tuvieran nada que ver: cuanto mayor, más se aleja de esa independencia.",
             if (isTRUE(t$weighted)) " Como los datos están ponderados, la p se calcula con la corrección de Rao-Scott, que tiene en cuenta los pesos." else ""),
        ex_p("<strong>", p_txt(t$p), "</strong>: probabilidad de encontrar una relación así de grande por azar si no existiera. ",
             if (isTRUE(t$p < 0.05)) "Como es menor que 0,05, la relación es <strong>estadísticamente significativa</strong>."
             else "Como no es menor que 0,05, no hay pruebas suficientes de relación."),
        ex_p("<strong>V de Cramér = ", fmt_num(t$v), "</strong>", fuerza_v(t$v), ": mide la fuerza de la relación de 0 (ninguna) a 1 (total). ",
             "Hasta 0,10 es muy débil, hasta 0,30 débil, hasta 0,50 moderada y por encima, fuerte."),
        if (any(t$expected < 5)) ex_p("Hay casillas con menos de 5 casos esperados: con tan pocos casos el χ² es menos fiable; conviene agrupar categorías."))
    },
    numcat = {
      a <- r$anova
      if (is.null(a)) NULL else {
        eta <- a[1, "Sum Sq"] / sum(a[, "Sum Sq"])
        tagList(
          ex_p("<strong>ANOVA: F = ", fmt_num(a[1, "F value"]), "</strong> (", a[1, "Df"], " y ", a[2, "Df"], " grados de libertad). Compara la diferencia ",
               "entre las medias de los grupos con la variación dentro de cada grupo."),
          ex_p("<strong>", p_txt(a[1, "Pr(>F)"]), "</strong>: ", if (isTRUE(a[1, "Pr(>F)"] < 0.05)) "al menos un grupo tiene una media distinta de los demás (diferencia estadísticamente significativa)."
               else "no hay pruebas suficientes de que las medias sean distintas."),
          ex_p("<strong>η² = ", fmt_num(eta, 3), "</strong>: parte de la variación de la variable que se explica por los grupos ",
               "(0,01 pequeña, 0,06 media, 0,14 o más grande)."))
      }
    },
    numnum = ex_p("La columna <strong>p</strong> indica si cada correlación es estadísticamente significativa (menor que 0,05). ",
                  "Una correlación no implica causa: solo que las variables varían juntas."),
    catcat3 = , numcat3 = ex_p("Se hace la prueba (", if (k == "catcat3") "χ² y V de Cramér" else "ANOVA", ") en cada grupo de la variable de control. ",
                               "Si la relación aparece en todos los grupos, no se debe a la variable de control; si cambia o desaparece en alguno, esa variable la modifica."),
    mrcat = ex_p("Para cada opción se compara, con una prueba χ², el porcentaje que la marca en cada grupo: * p < 0,05, ** p < 0,01, *** p < 0,001."),
    NULL)
  tagList(
    ex_sec("Qué se analiza", que),
    ex_sec("Cómo leer la tabla", tabla),
    ex_sec("Cómo leer el gráfico", graf),
    ex_sec("Los estadísticos", est,
           if (nzchar(r$stats %||% "")) div(class = "c-about-res", tags$strong("Resultado: "), r$stats)))
}

# Redes --------------------------------------------------------------------------------------------
explain_network <- function(d, spec, net) {
  if (is.null(net) || is.null(spec)) return(p("Crea primero una red."))
  t <- spec$type
  sm <- net_summary(net)
  pond <- if (length(spec$weight) && nzchar(spec$weight)) paste0(" Los cálculos están ponderados por ", ex_var(d, spec$weight), ".") else ""
  vars <- spec$vars %||% character()
  crit <- if (isFALSE(spec$signif)) "; se dibujan con un criterio amplio (p < 0,5), para ver también las relaciones débiles"
          else "; solo se dibujan las <strong>estadísticamente significativas</strong> (p < 0,05)"
  if (isTRUE(spec$bonferroni)) crit <- paste0(crit, ", con la <strong>corrección de Bonferroni</strong> (más exigente cuando hay muchas comparaciones)")
  comunidades <- if (nzchar(spec$community %||% "")) ex_p("Las <strong>comunidades</strong> (método ", spec$community, ") agrupan los nodos más conectados entre sí; ",
                                                         "se pueden ver como zonas de color en la red.")
  filtro_grado <- if (isTRUE(as.numeric(spec$degree %||% 0) > 0)) ex_p("Se ocultan los nodos con menos de ", spec$degree, " enlaces.")
  minimo <- if (isTRUE(as.numeric(spec$min_freq %||% 0) > 0)) ex_p("Solo entran las categorías con al menos ", spec$min_freq, " casos.")
  recuento <- ex_p("La red tiene <strong>", fmt_int(sm$nodes), "</strong> nodos y <strong>", fmt_int(sm$links), "</strong> enlaces.")
  pasa <- ex_p("Pasa el ratón o pulsa sobre un nodo o un enlace para ver sus valores; en el panel de la red puedes buscar, filtrar y cambiar el tamaño y el color.")
  switch(t,
    factorial = {
      tipo <- if (identical(spec$ftype, "pca")) "un <strong>análisis factorial (componentes principales)</strong>, porque las variables activas son cuantitativas"
              else "un <strong>análisis de correspondencias múltiples</strong>, porque las variables activas son cualitativas"
      nc <- tryCatch(parse_nclusters(spec$nclusters), error = function(e) NULL)
      tagList(
        ex_sec("Qué se analiza", ex_p("Se hace ", tipo, ". Variables <strong>activas</strong> (construyen los ejes): ", ex_vars(d, vars), ".", pond),
               if (length(spec$passive)) ex_p("Variables <strong>pasivas</strong> (no influyen en los ejes; se describen sobre ellos): ", ex_vars(d, spec$passive), ".")),
        ex_sec("Cómo leer la nube", ex_p("Cada <strong>punto es un caso</strong>. Los casos que responden de forma parecida quedan cerca; los que responden distinto, lejos. ",
                                         "Los dos ejes resumen las principales diferencias entre los casos."),
               if (length(nc) && !identical(nc, 0L)) ex_p("El <strong>color</strong> indica el <strong>conglomerado</strong>: grupos de casos parecidos entre sí",
                                                         if (is.character(nc)) " (el número de grupos lo elige el propio análisis)" else "", "."),
               pasa),
        ex_sec("Los números", recuento))
    },
    coincidencias = , multiple = {
      que <- if (t == "multiple") ex_p("Se analizan las opciones de ", ex_vars(d, vars), if (length(spec$cross)) paste0(", cruzadas con ", ex_vars(d, spec$cross)) else "", ".", pond)
             else ex_p("Variables: ", ex_vars(d, vars), ". No hay variable dependiente: se busca qué categorías aparecen juntas.", pond)
      tagList(
        ex_sec("Qué se analiza", que, minimo),
        ex_sec("Los nodos", ex_p("Cada <strong>nodo es ", if (t == "multiple") "una opción" else "una categoría", "</strong>",
                                 if (t == "coincidencias") " (por ejemplo, «Mujer (Género)»)" else "", ". Su <strong>tamaño</strong> es el porcentaje de casos que ",
                                 if (t == "multiple") "la marcan" else "la tienen", " y su <strong>color</strong>, ",
                                 if (t == "multiple") "la pregunta o variable a la que pertenece." else "la variable a la que pertenece."),
               comunidades, filtro_grado),
        ex_sec("Los enlaces", ex_p("Una <strong>línea une dos ", if (t == "multiple") "opciones" else "categorías", "</strong> que aparecen juntas en los mismos casos ",
                                   "<strong>más de lo esperado</strong> si fueran independientes", crit, ". La medida es ",
                                   names(similarity_measures)[similarity_measures == (spec$procedure %||% "Haberman")][1] %||% spec$procedure, "."),
               ex_p("Que no haya línea no significa que nunca coincidan, sino que no lo hacen más de lo esperado por azar.")),
        ex_sec("Los números", recuento, pasa))
    },
    correlaciones = tagList(
      ex_sec("Qué se analiza", ex_p("Variables numéricas: ", ex_vars(d, vars), ". Correlación de ",
                                    c(pearson = "Pearson (relación en línea recta)", spearman = "Spearman (por rangos)", kendall = "Kendall (por rangos)")[[spec$method %||% "pearson"]], ".", pond)),
      ex_sec("Los nodos", ex_p("Cada <strong>nodo es una variable</strong>."), comunidades, filtro_grado),
      ex_sec("Los enlaces", ex_p("Una <strong>línea une dos variables correlacionadas</strong>", crit, ". Su <strong>grosor</strong> y su <strong>color</strong> dependen del valor ",
                                 "de la correlación (entre −1 y 1): positiva si aumentan juntas, negativa si una sube cuando la otra baja."),
             ex_p("Una correlación no implica causa.")),
      ex_sec("Los números", recuento, pasa)),
    loglineal = tagList(
      ex_sec("Qué se analiza", ex_p("Variables: ", ex_vars(d, vars), ". Se ajusta un <strong>modelo log-lineal</strong> de orden ", spec$order %||% 2,
                                    ": estima cómo se asocian las categorías teniendo en cuenta todas las variables a la vez. No hay variable dependiente.", pond)),
      ex_sec("Los nodos", ex_p("Cada <strong>nodo es una categoría</strong>; su <strong>tamaño</strong> es su estimación en el modelo (cuánto pesa esa categoría) ",
                               "y su <strong>color</strong>, la variable a la que pertenece."), comunidades, filtro_grado),
      ex_sec("Los enlaces", ex_p("Una <strong>línea une dos categorías asociadas</strong>", crit, ". Su <strong>grosor</strong> es la estimación de la asociación: ",
                                 "más gruesa cuanto más fuerte."),
             if (identical(as.integer(spec$order %||% 2), 3L)) ex_p("Con orden 3 también se estiman asociaciones entre tres categorías a la vez.")),
      ex_sec("Los números", recuento, pasa)),
    regresion = tagList(
      ex_sec("Qué se analiza", ex_p("Variable <strong>dependiente</strong> (la que se quiere explicar): ", ex_var(d, spec$dep), "."),
             ex_p("Variables <strong>independientes</strong> (explicativas): ", ex_vars(d, setdiff(vars, spec$dep)), ". Cada efecto se mide manteniendo constantes las demás.", pond)),
      ex_sec("Los nodos", ex_p("Los nodos son la variable dependiente y las categorías o variables explicativas; el <strong>color</strong> indica la variable a la que pertenece cada uno.")),
      ex_sec("Los enlaces", ex_p("Una <strong>línea</strong> indica un <strong>efecto sobre la variable dependiente</strong>", crit, ". ",
                                 "Su <strong>color</strong> depende del coeficiente (positivo: la aumenta; negativo: la reduce) y su <strong>grosor</strong>, de la evidencia ",
                                 "del efecto (cuanto menor la p, más gruesa).")),
      ex_sec("Los números", recuento, pasa)),
    evolucion = tagList(
      ex_sec("Qué se analiza", ex_p("Cada caso (", ex_var(d, spec$name), ") es un periodo que va de ", ex_var(d, spec$start), " a ", ex_var(d, spec$end), "."),
             if (length(spec$group) && nzchar(spec$group)) ex_p("Los casos se agrupan y colorean según ", ex_var(d, spec$group), ".")),
      ex_sec("Cómo leer la línea de tiempo", ex_p("Cada <strong>barra es un caso</strong>, desde su inicio hasta su fin; las barras que se solapan coinciden en el tiempo. ",
                                                  "La banda de arriba resume todo el periodo y debajo se ve cada grupo. Si un caso no tiene fin, llega hasta el último año de los datos."),
             pasa),
      ex_sec("Los números", ex_p("Hay <strong>", fmt_int(sm$nodes), "</strong> periodos."))),
    trayectorias = {
      ps <- tryCatch(sem_parse(spec$model), error = function(e) list())
      dep <- unique(unlist(lapply(Filter(function(p) p$op == "~", ps), `[[`, "lhs")))
      ind <- setdiff(unique(unlist(lapply(Filter(function(p) p$op == "~", ps), `[[`, "rhs"))), dep)
      lat <- unique(unlist(lapply(Filter(function(p) p$op == "=~", ps), `[[`, "lhs")))
      nombre <- function(v) if (v %in% names(d)) ex_var(d, v) else paste0("<strong>", htmltools::htmlEscape(v), "</strong> (latente)")
      lista <- function(vs) paste(vapply(vs, nombre, ""), collapse = ", ")
      fit <- attr(net, "fit_text") %||% ""
      tagList(
        ex_sec("Qué se analiza", ex_p("Un <strong>modelo de ecuaciones estructurales</strong> estimado con lavaan.", pond),
               if (length(dep)) ex_p("Variables <strong>dependientes</strong> (reciben flechas): ", lista(dep), "."),
               if (length(ind)) ex_p("Variables <strong>independientes</strong> (solo salen flechas de ellas): ", lista(ind), "."),
               if (length(lat)) ex_p("Variables <strong>latentes</strong> (no se observan directamente; se miden con sus indicadores): ", lista(lat), ".")),
        ex_sec("Los nodos", ex_p("El <strong>color y la forma</strong> distinguen las variables observadas de las latentes. Pulsa un nodo para ver su <strong>R²</strong>: ",
                                 "la parte de esa variable que explica el modelo (de 0 a 1).")),
        ex_sec("Las flechas", ex_p("Van de la causa supuesta al efecto (o de la variable latente a sus indicadores). El número es el <strong>coeficiente estandarizado (β)</strong>, ",
                                   "entre −1 y 1: más lejos de 0 cuanto más fuerte. El <strong>grosor</strong> también es |β| y el <strong>color</strong>, su signo ",
                                   "(verde, positivo; rojo, negativo)."),
               ex_p("Al pulsar una flecha ves también el coeficiente sin estandarizar (b), su error típico y su p.")),
        ex_sec("El ajuste del modelo", ex_p("Indica si el modelo reproduce bien los datos: el <strong>χ²</strong> debería no ser significativo (p ≥ 0,05), ",
                                            "el <strong>CFI</strong> 0,90 o más (mejor 0,95) y el <strong>RMSEA</strong> 0,08 o menos (mejor 0,05)."),
               if (nzchar(fit)) div(class = "c-about-res", tags$strong("Resultado: "), fit)))
    },
    p("No hay explicación para este tipo de red."))
}

about_modal <- function(contenido) {
  showModal(modalDialog(title = "Sobre tus datos", size = "l", easyClose = TRUE,
                        div(class = "c-about", contenido), footer = modalButton("Cerrar")))
}
