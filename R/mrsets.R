# Conjuntos de respuesta múltiple ----------------------------------------------------------
# Un conjunto agrupa varias variables que juntas responden a una pregunta de respuesta
# múltiple. Se define como un paso más del historial (op = "mrset") y se guarda en
# attr(datos, "mrsets"). Tres tipos:
#   dicotomias: una variable por opción; cuenta como marcada si vale `value` (Sí, 1...)
#   categorias: una variable por mención (1.ª, 2.ª...); las opciones son sus categorías
#   texto:      una variable de texto con las opciones separadas por `sep`

mr_types <- c("Dicotomías (una variable por opción)" = "dicotomias",
              "Categorías (una variable por mención)" = "categorias",
              "Texto con separador (una sola variable)" = "texto")

mrsets <- function(d) attr(d, "mrsets", exact = TRUE) %||% list()

# Valores que significan lo mismo que el elegido: "Sí" y su código original (1), en
# cualquiera de las variables. Así cuenta igual una variable leída como categórica
# (Sí/No) y otra que quedó numérica (1/2) por tener algún valor sin etiqueta.
mr_equivalent_values <- function(d, vars, value) {
  out <- value
  for (v in vars) {
    codes <- attr(d[[v]], "codes", exact = TRUE)
    if (length(codes)) {
      out <- c(out, codes[names(codes) == value], names(codes)[codes == value])
    }
  }
  num <- suppressWarnings(as.numeric(out))
  out <- c(out, as.character(num[!is.na(num)]))      # "1" y "1.0" son lo mismo
  unique(out[!is.na(out) & nzchar(out)])
}

# Nombres de las opciones de un conjunto de dicotomías: la etiqueta de cada variable;
# si varias comparten etiqueta (suele ser el texto de la pregunta), el nombre de la variable.
mr_option_names <- function(d, vars) {
  labs <- vapply(vars, function(v) var_label(d, v), "")
  dup <- duplicated(labs) | duplicated(labs, fromLast = TRUE)
  if (all(dup)) return(unname(vars))
  labs[dup] <- paste0(labs[dup], " (", vars[dup], ")")
  unname(labs)
}

# Variables de un conjunto que no tienen ninguna respuesta marcada (para avisar).
mr_empty_options <- function(d, set) {
  set$merge <- NULL                       # se avisa por cada variable, antes de juntar opciones
  m <- mr_incidences(d, set)
  vacias <- names(m)[colSums(m, na.rm = TRUE) == 0]
  if (!length(vacias) || set$type != "dicotomias") return(list(opciones = vacias, detalle = character()))
  idx <- match(vacias, names(m))
  detalle <- vapply(set$vars[idx], function(v) {
    vals <- if (is.factor(d[[v]])) levels(droplevels(d[[v]])) else as.character(sort(unique(stats::na.omit(d[[v]]))))
    paste0(v, " (valores: ", paste(utils::head(vals, 6), collapse = ", "), ")")
  }, "")
  list(opciones = vacias, detalle = unname(detalle))
}

mr_label <- function(set) if (nzchar(set$label %||% "")) set$label else set$name

# Valida y guarda un conjunto en los datos (lo usa apply_step).
mr_add <- function(d, s) {
  if (is.null(s$name) || !grepl("^[[:alpha:]][[:alnum:]_.]*$", s$name)) {
    stop("El nombre del conjunto no es válido: usa letras, números, _ o . y empieza por una letra.")
  }
  if (!s$type %in% mr_types) stop("Tipo de conjunto desconocido.")
  vars <- unlist(s$vars)
  if (!length(vars)) stop("Elige las variables del conjunto.")
  for (v in vars) need_var(d, v)
  if (s$type %in% c("dicotomias", "categorias") && length(vars) < 2) stop("Un conjunto necesita al menos dos variables.")
  if (s$type == "texto" && length(vars) != 1) stop("En un conjunto de texto se elige una sola variable.")
  values <- NULL
  if (s$type == "dicotomias") {
    if (is.null(s$value) || !nzchar(as.character(s$value))) stop("Indica qué valor cuenta como «marcado» (por ejemplo, Sí o 1).")
    values <- mr_equivalent_values(d, vars, as.character(s$value))
    hit <- vapply(vars, function(v) any(as.character(d[[v]]) %in% values, na.rm = TRUE), logical(1))
    if (!any(hit)) stop("Ninguna de las variables tiene el valor «", s$value, "».")
  }
  sets <- mrsets(d)
  sets[[s$name]] <- list(name = s$name, label = s$label %||% "", type = s$type, vars = vars,
                         value = if (s$type == "dicotomias") as.character(s$value), values = values,
                         sep = if (s$type == "texto") (s$sep %||% ";"))
  attr(d, "mrsets") <- sets
  d
}

# Mantiene los conjuntos al cambiar nombres o eliminar variables.
mr_rename_var <- function(d, from, to) {
  sets <- mrsets(d)
  for (k in names(sets)) sets[[k]]$vars[sets[[k]]$vars == from] <- to
  attr(d, "mrsets") <- if (length(sets)) sets
  d
}

mr_drop_vars <- function(d, vars) {
  sets <- mrsets(d)
  for (k in names(sets)) {
    sets[[k]]$vars <- setdiff(sets[[k]]$vars, vars)
    min_v <- if (sets[[k]]$type == "texto") 1 else 2
    if (length(sets[[k]]$vars) < min_v) sets[[k]] <- NULL
  }
  attr(d, "mrsets") <- if (length(sets)) sets
  d
}

# Tabla de incidencias (0/1): una columna por opción, una fila por caso.
# Los casos sin ninguna respuesta válida en el conjunto quedan como NA.
mr_incidences <- function(d, set) {
  vars <- set$vars
  if (set$type == "dicotomias") {
    marcado <- set$values %||% mr_equivalent_values(d, vars, set$value)
    m <- vapply(vars, function(v) {
      x <- as.character(d[[v]])
      ifelse(is.na(x), NA_integer_, as.integer(x %in% marcado))
    }, integer(nrow(d)))
    m <- matrix(m, nrow = nrow(d))
    colnames(m) <- mr_option_names(d, vars)
    all_na <- rowSums(!is.na(m)) == 0
    m[is.na(m)] <- 0L
    m[all_na, ] <- NA
  } else if (set$type == "categorias") {
    vals <- lapply(vars, function(v) as.character(d[[v]]))
    opts <- unique(unlist(lapply(vars, function(v) if (is.factor(d[[v]])) levels(d[[v]]) else sort(unique(stats::na.omit(as.character(d[[v]])))))))
    m <- vapply(opts, function(o) as.integer(Reduce(`|`, lapply(vals, function(x) !is.na(x) & x == o))), integer(nrow(d)))
    m <- matrix(m, nrow = nrow(d)); colnames(m) <- opts
    all_na <- Reduce(`&`, lapply(vals, is.na))
    m[all_na, ] <- NA
  } else {
    x <- as.character(d[[vars[1]]])
    parts <- lapply(strsplit(ifelse(is.na(x), "", x), set$sep %||% ";", fixed = TRUE), function(p) unique(trimws(p[nzchar(trimws(p))])))
    opts <- sort(unique(unlist(parts)))
    m <- t(vapply(parts, function(p) as.integer(opts %in% p), integer(length(opts))))
    if (length(opts) == 1) m <- t(m)
    colnames(m) <- opts
    m[is.na(x), ] <- NA
  }
  mr_apply_merge(as.data.frame(m, check.names = FALSE), set$merge)
}

#' Tabla 0/1 de un conjunto de respuesta múltiple, como la calcula CARING.
#'
#' Una columna por opción y una fila por caso (NA si el caso no contestó a ninguna).
#' La usa el script de R que genera CARING.
#' @param datos data.frame leído con read_any()
#' @param tipo "dicotomias", "categorias" o "texto"
#' @param variables variables del conjunto
#' @param valor en las dicotomías, el valor que cuenta como marcado
#' @param sep en las de texto, el separador de las opciones
#' @export
conjunto_respuesta_multiple <- function(datos, tipo, variables, valor = NULL, sep = ";") {
  mr_incidences(datos, list(type = tipo, vars = variables, value = valor, sep = sep))
}

# Opciones juntadas ------------------------------------------------------------------------------
# set$merge: lista de list(name, from). Cada grupo sustituye a las opciones de `from` por
# una sola, `name`, que vale 1 si el caso marcó cualquiera de ellas. Va en el sitio de
# la primera que junta. `name` puede ser el de una de ellas o uno nuevo, y con una sola
# opción en `from` sirve para cambiarle el nombre. Las que ya no existen se ignoran.
mr_apply_merge <- function(m, merge) {
  for (g in merge %||% list()) {
    src <- intersect(unlist(g$from), names(m))
    if (!length(src)) next
    nueva <- do.call(pmax, c(unname(as.list(m[src])), na.rm = TRUE))
    pos <- min(match(src, names(m)))
    resto <- m[setdiff(names(m), src)]
    antes <- seq_len(pos - 1)
    m <- cbind(resto[antes], stats::setNames(data.frame(nueva), g$name), resto[setdiff(seq_along(resto), antes)])
  }
  m
}

# Opciones de un conjunto antes de juntar nada
mr_raw_options <- function(d, set) { set$merge <- NULL; names(mr_incidences(d, set)) }

# Comprueba y normaliza las uniones de un conjunto (lo usa el paso mrset_merge).
mr_merge_check <- function(d, set, merge) {
  opts <- mr_raw_options(d, set)
  out <- list()
  usadas <- character()
  for (g in merge %||% list()) {
    from <- unique(as.character(unlist(g$from)))
    if (!length(from)) next
    nm <- trimws(g$name %||% "")
    if (!nzchar(nm)) nm <- from[1]
    falta <- setdiff(from, opts)
    if (length(falta)) stop("El conjunto «", set$name, "» no tiene la opción «", falta[1], "».")
    rep <- intersect(from, usadas)
    if (length(rep)) stop("La opción «", rep[1], "» está en dos grupos.")
    usadas <- c(usadas, from)
    out[[length(out) + 1]] <- list(name = nm, from = from)
  }
  nombres <- vapply(out, `[[`, "", "name")
  if (anyDuplicated(nombres)) stop("Dos opciones nuevas se llaman igual: «", nombres[duplicated(nombres)][1], "».")
  choca <- intersect(nombres, setdiff(opts, usadas))
  if (length(choca)) stop("Ya hay una opción «", choca[1], "» que no estás juntando: ponle otro nombre o júntala también.")
  out
}

mr_set_merge <- function(d, s) {
  sets <- mrsets(d)
  if (is.null(sets[[s$name]])) stop("No existe el conjunto «", s$name, "».")
  m <- mr_merge_check(d, sets[[s$name]], s$merge)
  sets[[s$name]]$merge <- if (length(m)) m
  attr(d, "mrsets") <- sets
  d
}

# Sugerencias: grupos de variables con el mismo prefijo y las mismas categorías.
mr_suggest <- function(d) {
  usadas <- unlist(lapply(mrsets(d), `[[`, "vars"))
  cand <- setdiff(names(d)[vapply(d, function(x) is.factor(x) || is.numeric(x), logical(1))], usadas)
  pref <- sub("([_.]?[0-9]+|_[[:alpha:]][[:alnum:]]*)$", "", cand)
  pref[pref == cand] <- NA
  out <- list()
  for (p in unique(stats::na.omit(pref))) {
    vs <- cand[!is.na(pref) & pref == p]
    if (length(vs) < 2) next
    niveles <- lapply(vs, function(v) {
      x <- d[[v]]
      if (is.factor(x)) levels(x) else sort(unique(stats::na.omit(x)))
    })
    if (!all(vapply(niveles, identical, logical(1), niveles[[1]]))) next
    lv <- as.character(niveles[[1]])
    if (length(lv) == 2) {
      marcado <- c("Sí", "Si", "sí", "si", "1", "Mencionado", "Marcado", "Seleccionado", "Yes", "TRUE", "Mencionada")
      value <- lv[lv %in% marcado][1]
      if (is.na(value)) value <- lv[1]
      out[[length(out) + 1]] <- list(name = make.names(p), type = "dicotomias", vars = vs, value = value)
    } else if (length(lv) > 2 && length(lv) <= 60 && all(vapply(vs, function(v) is.factor(d[[v]]), logical(1)))) {
      out[[length(out) + 1]] <- list(name = make.names(p), type = "categorias", vars = vs)
    }
  }
  out
}

# Frecuencias de un conjunto: % de casos y % de respuestas.
mr_freq <- function(d, set, w = rep(1, nrow(d))) {
  m <- mr_incidences(d, set)
  ok <- stats::complete.cases(m)
  m <- m[ok, , drop = FALSE]; ww <- w[ok]
  n_resp <- colSums(m * ww)
  casos <- sum(ww)
  data.frame(opcion = names(m), n = n_resp, pct_casos = n_resp / casos, pct_resp = n_resp / sum(n_resp),
             stringsAsFactors = FALSE, check.names = FALSE) |>
    structure(casos = casos, respuestas = sum(n_resp), sin_respuesta = sum(w[!ok]))
}
