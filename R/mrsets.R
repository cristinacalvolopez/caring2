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
  if (s$type == "dicotomias") {
    if (is.null(s$value) || !nzchar(as.character(s$value))) stop("Indica qué valor cuenta como «marcado» (por ejemplo, Sí o 1).")
    hit <- vapply(vars, function(v) any(as.character(d[[v]]) == as.character(s$value), na.rm = TRUE), logical(1))
    if (!any(hit)) stop("Ninguna de las variables tiene el valor «", s$value, "».")
  }
  sets <- mrsets(d)
  sets[[s$name]] <- list(name = s$name, label = s$label %||% "", type = s$type, vars = vars,
                         value = if (s$type == "dicotomias") as.character(s$value),
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
    m <- vapply(vars, function(v) {
      x <- as.character(d[[v]])
      ifelse(is.na(x), NA_integer_, as.integer(x == set$value))
    }, integer(nrow(d)))
    m <- matrix(m, nrow = nrow(d))
    colnames(m) <- vapply(vars, function(v) var_label(d, v), "")
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
  as.data.frame(m, check.names = FALSE)
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
