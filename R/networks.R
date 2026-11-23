# Redes con netCoin -----------------------------------------------------------------

net_types <- list(
  coincidencias = list(
    titulo = "Coincidencias",
    desc = "Qué categorías aparecen juntas más de lo esperado (surCoin)",
    vars = "categóricas", min = 2),
  correlaciones = list(
    titulo = "Correlaciones",
    desc = "Red entre variables numéricas (netCorr)",
    vars = "numéricas", min = 2),
  factorial = list(
    titulo = "Mapa factorial",
    desc = "Casos y categorías en un plano, con conglomerados (surScat)",
    vars = "categóricas o numéricas", min = 2),
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
    vars = "uno o varios conjuntos de respuesta múltiple", min = 1)
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
      if (length(vars) < min_need) stop("Elige al menos ", min_need, " variables.")
      dd <- d[c(vars, w)]
      args <- c(compact(list(data = dd, variables = vars, weight = w)), common)
      args$layout <- NULL
      args$degreeFilter <- NULL
      args$community <- NULL
      if (isTRUE(spec$nclusters >= 2)) {
        args$nclusters <- spec$nclusters
        args$clusters <- spec$clusters %||% "kmeans"
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
  var_col <- intersect(c("variable", "var"), names(nodes))[1]
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

net_summary <- function(net) {
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
