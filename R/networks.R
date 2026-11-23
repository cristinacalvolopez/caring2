# Redes con netCoin -----------------------------------------------------------------

net_types <- list(
  factorial = list(
    titulo = "Nube de puntos (factorial)",
    desc = "Casos en un plano: correspondencias (cualitativas) o factorial (cuantitativas), con conglomerados y variables pasivas (surScat)",
    vars = "categóricas o numéricas", min = 2),
  coincidencias = list(
    titulo = "Coincidencias",
    desc = "Qué categorías aparecen juntas más de lo esperado (surCoin)",
    vars = "categóricas", min = 2),
  correlaciones = list(
    titulo = "Correlaciones",
    desc = "Red entre variables numéricas (netCorr)",
    vars = "numéricas", min = 2),
  loglineal = list(
    titulo = "Log-lineal",
    desc = "Asociaciones entre categorías controlando las demás (logCoin)",
    vars = "categóricas", min = 2),
  regresion = list(
    titulo = "Regresión en red",
    desc = "Qué variables explican otra (glmCoin)",
    vars = "una dependiente y varias independientes", min = 1),
  multiple = list(
    titulo = "Respuesta múltiple",
    desc = "Qué opciones de una o varias preguntas de respuesta múltiple se marcan juntas (allNet)",
    vars = "uno o varios conjuntos de respuesta múltiple", min = 1),
  evolucion = list(
    titulo = "Evolución temporal",
    desc = "Cada caso como un periodo (inicio y fin) en una línea de tiempo (timeCoin)",
    vars = "nombre, inicio y fin", min = 0),
  trayectorias = list(
    titulo = "Ecuaciones estructurales",
    desc = "Modelo de rutas, con variables latentes si hace falta (pathCoin con lavaan)",
    vars = "un modelo", min = 0)
)

# Opciones que tienen sentido en cada tipo de análisis (las demás no se muestran).
net_options <- function(type) {
  switch(type,
    coincidencias = c("procedure", "signif", "community", "bonferroni", "min_freq", "degree", "layout", "labels"),
    correlaciones = c("method", "signif", "community", "bonferroni", "degree", "layout", "labels"),
    factorial = c("nclusters", "clusters"),
    loglineal = c("order", "signif", "community", "degree", "layout", "labels"),
    regresion = c("signif", "layout", "labels"),
    multiple = c("procedure", "signif", "community", "min_freq", "degree", "layout"),
    trayectorias = c("layout", "labels"),
    character())
}

similarity_measures <- c(
  "Haberman (residuos ajustados)" = "Haberman", "Coincidencias" = "coincidences",
  "Esperadas" = "expected", "Relativas" = "relative", "Jaccard" = "Jaccard",
  "Dice" = "dice", "Ochiai" = "Ochiai", "Pearson (phi)" = "Pearson",
  "Yule" = "Yule", "Odds ratio" = "odds", "Tetracórica" = "tetrachoric",
  "Probabilidad condicional" = "c.conditional"
)

community_methods <- c(
  "Ninguno" = "", "Louvain" = "louvain", "Fast greedy" = "fast_greedy",
  "Walktrap" = "walktrap", "Propagación de etiquetas" = "label_prop",
  "Leiden" = "leiden_eigen", "Edge betweenness" = "edge_betweenness"
)

node_label_styles <- c(
  "Categoría (Variable) · Mujer (Género)" = "legible",
  "Solo la categoría · Mujer" = "categoria",
  "Como en los datos · genero:Mujer" = "original"
)

layout_methods <- c(
  "Automática (fuerzas)" = "", "Fruchterman-Reingold" = "fruchterman.reingold",
  "Kamada-Kawai" = "kamada.kawai", "Círculo" = "circle", "Estrella" = "star",
  "Cuadrícula" = "grid", "MDS" = "mds", "DrL" = "drl", "Force Atlas 2" = "Force-Atlas-2"
)

# spec: list(type, vars, dep, weight, signif, procedure, community, layout,
#            title, note, sep, bonferroni, min_freq, degree, nclusters, order, cex)
net_build <- function(d, spec) {
  type <- spec$type
  vars <- spec$vars %||% character()
  if (type != "multiple") for (v in c(vars, spec$dep)) need_var(d, v)
  w <- if (length(spec$weight) && nzchar(spec$weight)) spec$weight else NULL
  if (!is.null(w)) need_var(d, w)
  common <- compact(list(
    language = "es",
    main = nz(spec$title),
    note = nz(spec$note),
    community = nz(spec$community),
    layout = nz(spec$layout),
    degreeFilter = if (isTRUE(spec$degree > 0)) spec$degree,
    cex = if (!is.null(spec$cex) && spec$cex != 1) spec$cex
  ))
  maxL <- if (isFALSE(spec$signif)) 0.5 else 0.05
  min_need <- net_types[[type]]$min

  net <- switch(type,
    coincidencias = {
      check_types(d, vars, "cat", min_need)
      do.call(netCoin::surCoin, c(compact(list(
        data = d[c(vars, w)], variables = vars, weight = w,
        procedures = spec$procedure %||% "Haberman", criteria = "Z", maxL = maxL,
        Bonferroni = isTRUE(spec$bonferroni),
        minimum = if (isTRUE(spec$min_freq > 0)) spec$min_freq,
        color = "variable")),
        common))
    },
    correlaciones = {
      check_types(d, vars, "num", min_need)
      do.call(netCoin::netCorr, c(compact(list(
        variables = d[vars], weight = if (!is.null(w)) d[[w]],
        method = spec$method %||% "pearson",
        criteria = "p", maxL = if (isFALSE(spec$signif)) Inf else 0.05,
        Bonferroni = isTRUE(spec$bonferroni))), common))
    },
    factorial = {
      # ftype: "mca" (correspondencias, activas cualitativas) o "pca" (factorial, activas cuantitativas);
      # sin él (informes antiguos) surScat lo decide según las variables. Las pasivas no construyen los ejes.
      ftype <- spec$ftype
      if (identical(ftype, "mca")) check_types(d, vars, "cat", min_need)
      else if (identical(ftype, "pca")) check_types(d, vars, "num", min_need)
      if (length(vars) < min_need) stop("Elige al menos ", min_need, " variables activas.")
      passive <- setdiff(spec$passive %||% character(), vars)
      for (v in passive) need_var(d, v)
      dd <- d[c(vars, passive, w)]
      args <- c(compact(list(data = dd, variables = c(vars, passive), active = vars, weight = w, type = ftype)), common)
      args$layout <- NULL
      args$degreeFilter <- NULL
      args$community <- NULL
      nc <- parse_nclusters(spec$nclusters)
      if (!is.null(nc)) {
        args$nclusters <- nc
        # El método adicional de conglomerados solo tiene sentido con números de grupos concretos
        if (!is.character(nc) && any(nc >= 2)) args$clusters <- spec$clusters %||% "kmeans"
      }
      do.call(netCoin::surScat, args)
    },
    loglineal = {
      check_types(d, vars, "cat", min_need)
      do.call(netCoin::logCoin, c(compact(list(
        data = d[c(vars, w)], variables = vars, weight = w,
        order = spec$order %||% 2, pmax = maxL, color = "variable")), common))
    },
    regresion = {
      dep <- spec$dep
      if (!length(dep) || !nzchar(dep)) stop("Elige la variable que quieres explicar.")
      indep <- setdiff(vars, dep)
      if (!length(indep)) stop("Elige al menos una variable explicativa.")
      prep <- glm_prepare(d, dep, indep, w)
      args <- c(list(formulas = prep$formula, data = prep$data, pmax = maxL), common)
      args$community <- NULL
      if (!is.null(w)) args$weights <- prep$weights
      do.call(netCoin::glmCoin, args)
    },
    multiple = {
      inc <- mr_network_incidences(d, spec)
      w_all <- if (!is.null(w)) d[[w]] else rep(1, nrow(d))
      ok <- stats::complete.cases(inc$m) & !is.na(w_all)
      m <- inc$m[ok, , drop = FALSE]
      keep <- colSums(m) > 0
      if (sum(keep) < 2) {
        vacias <- names(m)[!keep]
        stop("Hacen falta al menos dos opciones con respuestas para dibujar la red",
             if (length(vacias)) paste0(". Sin ninguna respuesta: ", paste(utils::head(vacias, 8), collapse = ", "),
                                        ". Revisa en Datos qué valor cuenta como «marcado» en el conjunto."), ".")
      }
      m <- m[, keep, drop = FALSE]
      nodes <- data.frame(nombre = names(m), grupo = inc$grupo[keep], check.names = FALSE)
      do.call(netCoin::allNet, c(compact(list(
        incidences = m, weight = if (!is.null(w)) w_all[ok],
        nodes = nodes, color = "grupo",
        procedures = spec$procedure %||% "Haberman", criteria = "Z", maxL = maxL,
        minimum = if (isTRUE(spec$min_freq > 0)) spec$min_freq)), common))
    },
    evolucion = {
      p <- time_periods(d, spec)
      # los controles de timeCoin muestran los nombres de columna: se usan las etiquetas
      g_lab <- if ("grupo" %in% names(p)) var_label(d, spec$group)
      if (identical(g_lab, "Nombre") || identical(g_lab, "Inicio") || identical(g_lab, "Fin")) g_lab <- paste(g_lab, "(grupo)")
      names(p)[names(p) == "nombre"] <- "Nombre"; names(p)[names(p) == "inicio"] <- "Inicio"; names(p)[names(p) == "fin"] <- "Fin"
      if (!is.null(g_lab)) names(p)[names(p) == "grupo"] <- g_lab
      if ("info" %in% names(p)) names(p)[names(p) == "info"] <- "Información"
      do.call(netCoin::timeCoin, compact(list(periods = p, name = "Nombre", start = "Inicio", end = "Fin",
                                              group = g_lab,
                                              info = if ("Información" %in% names(p)) "Información",
                                              main = nz(spec$title), note = nz(spec$note), language = "es",
                                              cex = if (!is.null(spec$cex) && spec$cex != 1) spec$cex)))
    },
    trayectorias = {
      fit <- sem_fit(d, spec$model, w)
      g <- sem_graph(fit, sem_parse(spec$model))
      if (!nzchar(spec$layout %||% "")) common$layout <- g$layout
      args <- c(list(nodes = g$nodes, links = g$links, name = "nombre", color = "tipo", shape = "tipo",
                     lwidth = "|beta|", lcolor = "beta", linkBipolar = TRUE, showArrows = TRUE,
                     ltext = "texto"), common)
      args$community <- NULL; args$degreeFilter <- NULL; args$note <- NULL
      net <- do.call(netCoin::netCoin, args)
      net$options$note <- paste0("<p>Ajuste del modelo: ", sem_fit_text(fit), "</p>",
                                 if (nzchar(spec$note %||% "")) paste0("<p>", htmltools::htmlEscape(spec$note), "</p>"))
      attr(net, "fit_text") <- sem_fit_text(fit)
      net
    },
    stop("Tipo de red desconocido")
  )
  net_readable(net, d, spec$labels %||% "legible")
}

# Nombres de nodo legibles: en vez de "genero:Mujer", "Mujer (Género)" usando las
# etiquetas de variable. Solo cambia el texto que se ve (columna etiqueta y nodeLabel);
# los identificadores de los nodos y los enlaces no se tocan.
net_readable <- function(net, d, style = "legible") {
  nodes <- net$nodes
  if (identical(style, "original") || is.null(nodes) || !nrow(nodes) || is.null(net$options)) return(net)
  name_col <- net$options$nodeName %||% "nombre"
  ids <- as.character(nodes[[name_col]] %||% rownames(nodes))
  var_col <- Filter(function(k) is.character(nodes[[k]]) || is.factor(nodes[[k]]), intersect(c("variable", "var"), names(nodes)))[1]
  if (is.null(var_col)) var_col <- NA
  lab <- ids
  if (!is.na(var_col)) {
    vv <- as.character(nodes[[var_col]])
    cat <- if ("cat" %in% names(nodes)) as.character(nodes$cat) else rep(NA_character_, nrow(nodes))
    for (i in seq_along(ids)) {
      v <- vv[i]
      if (is.na(v) || !v %in% names(d)) next
      ci <- cat[i]
      if (is.na(ci) || !nzchar(ci)) {
        ci <- if (startsWith(ids[i], paste0(v, ":"))) substring(ids[i], nchar(v) + 2)
              else if (startsWith(ids[i], v) && ids[i] != v) substring(ids[i], nchar(v) + 1)
              else NA_character_
      }
      lab[i] <- if (is.na(ci)) var_label(d, v)
                else if (style == "categoria") ci
                else paste0(ci, " (", var_label(d, v), ")")
    }
  } else {
    # redes de variables (correlaciones): el nodo es la propia variable
    es_var <- ids %in% names(d)
    lab[es_var] <- vapply(ids[es_var], function(v) var_label(d, v), "")
  }
  if (identical(lab, ids)) return(net)
  # la leyenda de colores muestra también la etiqueta de la variable
  if (!is.na(var_col)) {
    vv <- as.character(nodes[[var_col]])
    hit <- !is.na(vv) & vv %in% names(d)
    vv[hit] <- vapply(vv[hit], function(v) var_label(d, v), "")
    nodes[[var_col]] <- vv
  }
  nodes$etiqueta <- lab
  net$nodes <- nodes
  net$options$nodeLabel <- "etiqueta"
  net
}

# Tabla de incidencias para la red de respuesta múltiple: uno o varios conjuntos
# (spec$vars: "mr:nombre" o una variable de texto) y, opcionalmente, otras variables
# categóricas (spec$cross) cuyas categorías se añaden como nodos de otro grupo.
mr_network_incidences <- function(d, spec) {
  ids <- unlist(spec$vars)
  if (!length(ids)) stop("Elige al menos un conjunto de respuesta múltiple.")
  sets <- mrsets(d)
  partes <- list(); grupo <- character()
  for (id in ids) {
    s <- if (startsWith(id, "mr:")) sets[[sub("^mr:", "", id)]]
         else if (id %in% names(d)) list(name = id, label = var_label(d, id), type = "texto", vars = id, sep = spec$sep %||% ";")
    if (is.null(s)) stop("No existe el conjunto «", sub("^mr:", "", id), "».")
    m <- mr_incidences(d, s)
    if (ncol(m) < 1) stop("El conjunto «", mr_label(s), "» no tiene opciones con respuestas.")
    if (length(ids) > 1) names(m) <- paste0(mr_label(s), ": ", names(m))
    partes[[length(partes) + 1]] <- m
    grupo <- c(grupo, rep(mr_label(s), ncol(m)))
  }
  for (v in unlist(spec$cross)) {
    need_var(d, v)
    f <- d[[v]]
    if (!is.factor(f)) stop("Para cruzar, «", var_label(d, v), "» debe ser categórica (puedes agruparla en tramos en Datos).")
    f <- droplevels(f)
    dm <- vapply(levels(f), function(l) ifelse(is.na(f), NA_integer_, as.integer(f == l)), integer(length(f)))
    dm <- as.data.frame(matrix(dm, nrow = length(f)), check.names = FALSE)
    names(dm) <- paste0(var_label(d, v), ": ", levels(f))
    partes[[length(partes) + 1]] <- dm
    grupo <- c(grupo, rep(var_label(d, v), ncol(dm)))
  }
  m <- do.call(cbind, partes)
  names(m) <- make.unique(names(m), sep = " ")
  list(m = m, grupo = grupo)
}

nz <- function(x) if (is.null(x) || length(x) == 0 || identical(x, "") || all(is.na(x))) NULL else x

compact <- function(x) x[!vapply(x, is.null, logical(1))]

# Número de conglomerados de la nube de puntos. Acepta "3", "2:4", "3, 5, 3", "auto" o "auto, 2:6"
# (el último es la solución principal). "0" = sin conglomerados; vacío = el valor de surScat (2).
# Devuelve NULL, un vector de enteros, o un vector de texto si incluye "auto".
parse_nclusters <- function(x) {
  if (is.null(x) || !length(x) || all(is.na(x))) return(NULL)
  # Informes antiguos guardaban un número (0 = sin indicar)
  if (is.numeric(x) && length(x) == 1 && x < 2) return(NULL)
  txt <- tolower(gsub("^\\s*c\\(|\\)\\s*$", "", paste(x, collapse = ",")))
  tok <- strsplit(txt, "[,;[:space:]]+")[[1]]
  tok <- tok[nzchar(tok)]
  if (!length(tok)) return(NULL)
  out <- list()
  for (t in tok) {
    if (t == "auto") out <- c(out, "auto")
    else if (grepl("^[0-9]+$", t)) out <- c(out, as.integer(t))
    else if (grepl("^[0-9]+:[0-9]+$", t)) {
      ab <- as.integer(strsplit(t, ":")[[1]])
      out <- c(out, as.list(ab[1]:ab[2]))
    } else stop("No entiendo «", t, "» en el número de conglomerados. Usa, por ejemplo, 3 · 2:4 · 3, 5 · auto · auto, 2:6.")
  }
  v <- unlist(out)
  if (is.character(v)) v else as.integer(v)
}

check_types <- function(d, vars, kind, min_need) {
  if (length(vars) < min_need) stop("Elige al menos ", min_need, " variables.")
  bad <- if (kind == "cat") vars[!vapply(d[vars], is.factor, logical(1))]
         else vars[!vapply(d[vars], is.numeric, logical(1))]
  if (length(bad)) {
    stop(if (kind == "cat") "Estas variables no son categóricas: " else "Estas variables no son numéricas: ",
         paste(bad, collapse = ", "),
         if (kind == "cat") ". Puedes agruparlas en tramos en el paso Datos." else ".")
  }
}

# Prepara datos y fórmula para glmCoin a partir de una dependiente.
glm_prepare <- function(d, dep, indep, w = NULL) {
  dd <- d[c(dep, indep)]
  wt <- if (!is.null(w)) d[[w]]
  y <- dd[[dep]]
  if (is.numeric(y)) {
    fam <- "gaussian"
    yname <- dep
  } else {
    y <- factor(y)
    if (nlevels(y) == 2) {
      fam <- "binomial"
      yname <- make.names(paste0(dep, "_", levels(y)[1]))
      dd[[yname]] <- as.numeric(y == levels(y)[1])
      dd[[dep]] <- NULL
    } else {
      fam <- "multinomial"
      yname <- dep
    }
  }
  for (v in indep) if (is.factor(dd[[v]])) dd[[v]] <- factor(dd[[v]], ordered = FALSE)
  ok <- stats::complete.cases(dd) & (if (is.null(wt)) TRUE else !is.na(wt))
  list(data = dd[ok, , drop = FALSE], weights = if (!is.null(wt)) wt[ok],
       formula = paste(yname, "~", paste(indep, collapse = " + "), ",", fam))
}

# Evolución temporal ----------------------------------------------------------------------------
# Una fila por caso: nombre, inicio y fin (años, números o fechas), y grupo opcional.
time_periods <- function(d, spec) {
  for (k in c("name", "start", "end")) {
    if (!length(spec[[k]]) || !nzchar(spec[[k]])) {
      stop(switch(k, name = "Elige la variable con el nombre de cada caso.", start = "Elige la variable de inicio.", "Elige la variable de fin."))
    }
    need_var(d, spec[[k]])
  }
  tiempo <- function(v) {
    x <- d[[v]]
    if (inherits(x, c("Date", "POSIXct"))) return(as.Date(x))
    if (is.numeric(x)) return(x)
    y <- suppressWarnings(as.numeric(as.character(x)))
    if (sum(!is.na(y)) < 0.8 * sum(!is.na(x))) stop("La variable ", v, " no contiene años, números ni fechas.")
    y
  }
  ini <- tiempo(spec$start); fin <- tiempo(spec$end)
  if (inherits(ini, "Date") != inherits(fin, "Date")) stop("El inicio y el fin tienen que ser del mismo tipo (los dos fechas o los dos números).")
  nom <- as.character(d[[spec$name]])
  ok <- !is.na(ini) & !is.na(nom) & nzchar(nom)
  if (!any(ok)) stop("Ningún caso tiene nombre e inicio.")
  hoy <- if (inherits(ini, "Date")) Sys.Date() else max(c(ini, fin), na.rm = TRUE)
  fin[is.na(fin)] <- hoy                                  # sin fin: sigue hasta hoy (o el último año)
  if (sum(ok) > 3000) stop("Hay demasiados casos para una línea de tiempo (", fmt_int(sum(ok)), "; máximo 3.000). Filtra antes los casos en el paso Datos.")
  if (any(fin[ok] < ini[ok])) stop("Hay casos que terminan antes de empezar: revisa las variables de inicio y fin.")
  p <- data.frame(nombre = make.unique(nom[ok], sep = " "), inicio = ini[ok], fin = fin[ok], stringsAsFactors = FALSE)
  if (length(spec$group) && nzchar(spec$group)) {
    need_var(d, spec$group)
    g <- as.character(d[[spec$group]])[ok]; g[is.na(g)] <- "(sin grupo)"
    p$grupo <- g
  }
  if (length(spec$info) && nzchar(spec$info)) {
    need_var(d, spec$info)
    p$info <- htmltools::htmlEscape(as.character(d[[spec$info]])[ok])
  }
  if (inherits(p$inicio, "Date")) { p$inicio <- format(p$inicio, "%Y-%m-%d"); p$fin <- format(p$fin, "%Y-%m-%d") }
  p
}

# Ecuaciones estructurales ---------------------------------------------------------------------
# Solo se aceptan las partes del lenguaje de lavaan que no ejecutan código:
#   latente =~ indicadores · dependiente ~ explicativas · a ~~ b
# con multiplicadores numéricos o etiquetas (0.5*x, a*x). No se permiten «:=», restricciones
# (==, <, >) ni funciones: lavaan las evaluaría como código de R.
sem_parse <- function(model) {
  txt <- gsub("#[^\n]*", "", model %||% "")
  partes <- trimws(unlist(strsplit(txt, "[\n;]")))
  partes <- partes[nzchar(partes)]
  if (!length(partes)) stop("Escribe el modelo: al menos una relación (por ejemplo, satisfaccion ~ edad + ingresos).")
  nombre <- "[A-Za-z][A-Za-z0-9_.]*"
  termino <- paste0("(([0-9]+(\\.[0-9]+)?|NA|", nombre, ")\\s*\\*\\s*)?(", nombre, "|1)")
  patron <- paste0("^(", nombre, ")\\s*(=~|~~|~)\\s*(", termino, "(\\s*\\+\\s*", termino, ")*)$")
  out <- lapply(partes, function(l) {
    if (!grepl(patron, l, perl = TRUE)) {
      stop("No se entiende esta línea del modelo: «", l, "». Usa solo =~ (variable latente), ~ (regresión) y ~~ (correlación).")
    }
    m <- regmatches(l, regexec(patron, l, perl = TRUE))[[1]]
    rhs <- trimws(strsplit(m[4], "+", fixed = TRUE)[[1]])
    rhs <- sub(".*\\*\\s*", "", rhs)
    list(lhs = m[2], op = m[3], rhs = rhs[rhs != "1"], line = l)
  })
  out
}

sem_fit <- function(d, model, weight = NULL) {
  if (!requireNamespace("lavaan", quietly = TRUE)) stop("Falta el paquete lavaan para los modelos de ecuaciones estructurales.")
  ps <- sem_parse(model)
  latentes <- unique(vapply(Filter(function(p) p$op == "=~", ps), `[[`, "", "lhs"))
  usadas <- unique(c(unlist(lapply(ps, function(p) c(p$lhs, p$rhs)))))
  observadas <- setdiff(usadas, latentes)
  falta <- setdiff(observadas, names(d))
  if (length(falta)) stop("Estas variables no existen: ", paste(falta, collapse = ", "), ". Las variables latentes se definen con =~.")
  if (length(intersect(latentes, names(d)))) stop("El nombre de una variable latente no puede coincidir con el de una variable de los datos: ",
                                                  paste(intersect(latentes, names(d)), collapse = ", "), ".")
  dd <- d[observadas]
  for (v in observadas) {
    x <- dd[[v]]
    if (is.factor(x)) {
      if (nlevels(x) == 2 || is.ordered(x) || nlevels(x) <= 11) dd[[v]] <- as.numeric(as.integer(x))
      else stop("La variable ", v, " es categórica con muchas categorías: no puede entrar en el modelo.")
    } else if (!is.numeric(x)) stop("La variable ", v, " no es numérica.")
  }
  args <- list(model = paste(vapply(ps, `[[`, "", "line"), collapse = "
"), data = dd, missing = "ml")
  if (!is.null(weight)) {
    dd$.peso <- d[[weight]]
    args$data <- dd[!is.na(dd$.peso) & dd$.peso > 0, , drop = FALSE]
    args$sampling.weights <- ".peso"
    args$estimator <- "MLR"
  }
  avisos <- character()
  fit <- withCallingHandlers(tryCatch(do.call(lavaan::sem, args), error = function(e) {
    stop("lavaan no ha podido estimar el modelo: ", conditionMessage(e))
  }), warning = function(w) { avisos <<- c(avisos, conditionMessage(w)); invokeRestart("muffleWarning") })
  if (!isTRUE(lavaan::lavInspect(fit, "converged"))) stop("El modelo no converge. Prueba con menos variables o revisa las relaciones.")
  attr(fit, "avisos") <- avisos
  fit
}

# Red del modelo a partir de las estimaciones de lavaan (como pathCoin, pero también con
# las variables latentes, sus indicadores y las correlaciones que se piden en el modelo).
sem_graph <- function(fit, ps) {
  pe <- lavaan::parameterEstimates(fit, standardized = TRUE)
  pedidas <- unlist(lapply(Filter(function(p) p$op == "~~", ps), function(p) paste(p$lhs, p$rhs)))
  keep <- pe$op %in% c("~", "=~") | (pe$op == "~~" & pe$lhs != pe$rhs &
                                       (paste(pe$lhs, pe$rhs) %in% pedidas | paste(pe$rhs, pe$lhs) %in% pedidas))
  pe <- pe[keep, , drop = FALSE]
  if (!nrow(pe)) stop("El modelo no tiene relaciones que dibujar.")
  links <- data.frame(
    Source = ifelse(pe$op == "~", pe$rhs, pe$lhs),
    Target = ifelse(pe$op == "~", pe$lhs, pe$rhs),
    relacion = c("~" = "Regresión", "=~" = "Carga factorial", "~~" = "Correlación")[pe$op],
    b = round(pe$est, 3), se = round(pe$se, 3), z = round(pe$z, 2), pvalue = round(pe$pvalue, 4),
    beta = round(pe$std.all, 3), stringsAsFactors = FALSE)
  links$`|beta|` <- abs(links$beta)
  links$texto <- fmt_num(links$beta, 2)
  latentes <- unique(pe$lhs[pe$op == "=~"])
  nombres <- unique(c(links$Source, links$Target))
  nodes <- data.frame(nombre = nombres, tipo = ifelse(nombres %in% latentes, "Latente", "Observada"), stringsAsFactors = FALSE)
  r2 <- tryCatch(lavaan::lavInspect(fit, "r2"), error = function(e) NULL)
  nodes$R2 <- if (length(r2)) round(unname(r2[nodes$nombre]), 3) else NA
  list(nodes = nodes, links = links, layout = sem_layout(nodes$nombre, links, latentes))
}

# Disposición de izquierda a derecha: las explicativas a la izquierda, después lo que
# explican; los indicadores de cada variable latente, en la columna siguiente a ella.
sem_layout <- function(nombres, links, latentes) {
  reg <- links[links$relacion == "Regresión", , drop = FALSE]
  carga <- links[links$relacion == "Carga factorial", , drop = FALSE]
  indicadores <- setdiff(unique(carga$Target), c(reg$Target, reg$Source, latentes))
  estructurales <- setdiff(nombres, indicadores)
  nivel <- stats::setNames(rep(0, length(estructurales)), estructurales)
  for (it in seq_along(estructurales)) {               # camino más largo (sin ciclos infinitos)
    for (k in seq_len(nrow(reg))) {
      a <- reg$Source[k]; b <- reg$Target[k]
      if (a %in% estructurales && b %in% estructurales) nivel[b] <- max(nivel[b], nivel[a] + 1)
    }
  }
  x <- stats::setNames(numeric(length(nombres)), nombres)
  y <- x
  x[estructurales] <- 2 * nivel[estructurales]
  for (i in indicadores) {
    padre <- carga$Source[carga$Target == i][1]
    x[i] <- 2 * nivel[padre] + 1
  }
  # dentro de cada columna, repartidos de arriba abajo; los indicadores, junto a su latente
  orden <- function(v) {
    padre <- carga$Source[match(v, carga$Target)]
    ifelse(is.na(padre), v, paste(padre, v))
  }
  for (cx in unique(x)) {
    en <- names(x)[x == cx]
    en <- en[order(orden(en))]
    y[en] <- if (length(en) == 1) 0 else seq(1, -1, length.out = length(en)) * min(1, length(en) / 4)
  }
  mx <- max(x)
  cbind(if (mx > 0) x / mx * 2 - 1 else x, y)
}

sem_fit_text <- function(fit) {
  f <- tryCatch(lavaan::fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "rmsea")), error = function(e) NULL)
  if (is.null(f)) return("")
  paste0("χ²(", fmt_int(f[["df"]]), ") = ", fmt_num(f[["chisq"]]), " · ", p_txt(f[["pvalue"]]),
         " · CFI = ", fmt_num(f[["cfi"]], 3), " · RMSEA = ", fmt_num(f[["rmsea"]], 3),
         " · n = ", fmt_int(lavaan::lavInspect(fit, "nobs")))
}

net_summary <- function(net) {
  if (inherits(net, "timeCoin")) return(list(nodes = nrow(net$periods), links = 0))
  n <- if (!is.null(net$nodes)) nrow(net$nodes) else NA
  l <- if (!is.null(net$links)) nrow(net$links) else 0
  list(nodes = n, links = l)
}

# Renderizado -------------------------------------------------------------------------

# Genera la red como un único HTML autocontenido (sin archivos externos).
net_render_html <- function(net, out_file) {
  tmp <- tempfile("red-")
  # netCoin abre la página en el navegador cuando R es interactivo: aquí solo se quiere el
  # archivo (la red se ve dentro de CARING; pantalla completa y descarga usan este HTML).
  old <- options(browser = function(url, ...) invisible(NULL))
  on.exit({ options(old); unlink(tmp, recursive = TRUE) })
  suppressMessages(utils::capture.output(plot(net, dir = tmp)))
  html <- inline_html(tmp)
  writeLines(html, out_file, useBytes = TRUE)
  invisible(out_file)
}

data_uri <- function(path, mime) {
  paste0("data:", mime, ";base64,", base64enc::base64encode(path))
}

inline_html <- function(dir) {
  read <- function(p) paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  html <- read(file.path(dir, "index.html"))
  # CSS (con las fuentes incrustadas)
  for (m in regmatches(html, gregexpr('<link[^>]*href="([^"]+\\.css)"[^>]*>(</link>)?', html))[[1]]) {
    href <- sub('.*href="([^"]+)".*', "\\1", m)
    css <- read(file.path(dir, href))
    cssdir <- dirname(file.path(dir, href))
    for (u in unique(regmatches(css, gregexpr("url\\(([^)]+\\.woff2)\\)", css))[[1]])) {
      f <- sub("url\\(([^)]+)\\)", "\\1", u)
      css <- gsub(u, paste0("url(", data_uri(file.path(cssdir, f), "font/woff2"), ")"), css, fixed = TRUE)
    }
    html <- sub(m, paste0("<style>\n", css, "\n</style>"), html, fixed = TRUE)
  }
  # JavaScript
  for (m in regmatches(html, gregexpr('<script src="([^"]+)"></script>', html))[[1]]) {
    src <- sub('.*src="([^"]+)".*', "\\1", m)
    js <- read(file.path(dir, src))
    icons <- file.path(dir, "stats-icons")
    if (dir.exists(icons)) {
      for (f in list.files(icons)) {
        js <- gsub(paste0("stats-icons/", f), data_uri(file.path(icons, f), "image/png"), js, fixed = TRUE)
      }
    }
    js <- gsub("</script", "<\\/script", js, fixed = TRUE)
    html <- sub(m, paste0("<script>\n", js, "\n</script>"), html, fixed = TRUE)
  }
  html
}

# Genera el HTML interactivo y una imagen PNG sin controles. Devuelve las rutas.
net_render <- function(net, html_file, png_file = NULL) {
  net_render_html(net, html_file)
  png <- NULL
  if (!is.null(png_file)) {
    clean <- net
    if (!is.null(clean$options)) clean$options$controls <- 0
    tmp <- tempfile(fileext = ".html")
    on.exit(unlink(tmp))
    ok <- tryCatch({ net_render_html(clean, tmp); TRUE }, error = function(e) FALSE)
    if (ok) png <- net_render_png(tmp, png_file)
  }
  list(html = html_file, png = png)
}

# Captura una imagen PNG de la red (para miniaturas, PowerPoint y Word).
# Necesita Chrome/Chromium; si no está disponible devuelve NULL.
net_render_png <- function(html_file, png_file, width = 1200, height = 750) {
  ok <- tryCatch({
    suppressMessages(webshot2::webshot(paste0("file://", normalizePath(html_file)), png_file,
                                       vwidth = width, vheight = height, delay = 1.5, zoom = 2, quiet = TRUE))
    crop_net_png(png_file)
    file.exists(png_file)
  }, error = function(e) {
    message("No se pudo capturar la imagen de la red: ", conditionMessage(e))
    FALSE
  })
  if (ok) png_file else NULL
}

# Recorta la captura: quita la barra del título y el sello inferior de la página de
# netCoin y el espacio en blanco alrededor de la red, dejando un margen pequeño.
crop_net_png <- function(png_file, header = 37, footer = 32, margin = 24, zoom = 2) {
  if (!requireNamespace("magick", quietly = TRUE)) return(invisible(png_file))
  tryCatch({
    img <- magick::image_read(png_file)
    info <- magick::image_info(img)
    h <- info$height - (header + footer) * zoom
    if (h < 50) return(invisible(png_file))
    img <- magick::image_crop(img, magick::geometry_area(info$width, h, 0, header * zoom))
    img <- magick::image_trim(img, fuzz = 4)
    img <- magick::image_border(img, "white", paste0(margin * zoom, "x", margin * zoom))
    magick::image_write(img, png_file, format = "png")
  }, error = function(e) NULL)
  invisible(png_file)
}
