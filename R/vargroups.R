# Organizar las variables --------------------------------------------------------------
# Grupos de variables, variables ocultas, renombrar en bloque y ordenar.
#
# Los grupos y las ocultas no cambian los datos: viajan con ellos como atributos
# (attr(datos, "vgroups") y attr(datos, "hidden")), igual que los conjuntos de
# respuesta múltiple. Cada uno es un paso del historial que guarda el estado
# completo (op = "vgroups" / "hidden"); si el último paso ya es de ese tipo, se
# actualiza en su sitio para no llenar el historial (ctx$upsert_step).
#
#   - Una variable puede estar en varios grupos.
#   - Una variable oculta sigue en los datos, pero no se ofrece en Explorar, Redes
#     ni en las herramientas.
#   - Al renombrar o eliminar variables, grupos y ocultas se actualizan solos.

vgroups <- function(d) attr(d, "vgroups", exact = TRUE) %||% list()
hidden_vars <- function(d) intersect(attr(d, "hidden", exact = TRUE) %||% character(), names(d))

# Variables que se ofrecen en los análisis: todas menos las ocultas.
shown_vars <- function(d, vars = names(d)) setdiff(vars, hidden_vars(d))

# Variables de unos grupos (unión), en el orden de los datos.
vgroup_vars <- function(d, groups) {
  g <- vgroups(d)
  intersect(names(d), unique(unlist(lapply(g[intersect(groups, names(g))], `[[`, "vars"))))
}

valid_group_name <- function(nm) {
  if (is.null(nm) || !nzchar(trimws(nm))) stop("Falta el nombre del grupo.")
  if (nchar(nm) > 60) stop("El nombre del grupo es demasiado largo (máximo 60 caracteres).")
}

# Paso vgroups: s$groups es una lista de list(name, vars) con todos los grupos.
vg_set <- function(d, s) {
  gs <- s$groups %||% list()
  out <- list()
  for (g in gs) {
    nm <- trimws(g$name %||% "")
    valid_group_name(nm)
    if (!is.null(out[[nm]])) stop("Hay dos grupos con el nombre «", nm, "».")
    vars <- unique(as.character(unlist(g$vars)))
    for (v in vars) need_var(d, v)
    out[[nm]] <- list(name = nm, vars = vars)
  }
  attr(d, "vgroups") <- if (length(out)) out
  d
}

# Paso hidden: s$vars son todas las variables ocultas.
hidden_set <- function(d, s) {
  vars <- unique(as.character(unlist(s$vars)))
  for (v in vars) need_var(d, v)
  attr(d, "hidden") <- if (length(vars)) vars
  d
}

# Grupos y ocultas al renombrar (map: nombre viejo -> nuevo) y al eliminar.
meta_rename <- function(d, map) {
  if (!length(map)) return(d)
  ren <- function(x) { hit <- x %in% names(map); x[hit] <- unname(map[x[hit]]); x }
  g <- vgroups(d)
  for (k in names(g)) g[[k]]$vars <- ren(g[[k]]$vars)
  attr(d, "vgroups") <- if (length(g)) g
  h <- attr(d, "hidden", exact = TRUE)
  attr(d, "hidden") <- if (length(h)) ren(h)
  for (from in names(map)) d <- mr_rename_var(d, from, map[[from]])
  d
}

meta_drop <- function(d, vars) {
  g <- vgroups(d)
  for (k in names(g)) g[[k]]$vars <- setdiff(g[[k]]$vars, vars)
  attr(d, "vgroups") <- if (length(g)) g
  h <- setdiff(attr(d, "hidden", exact = TRUE) %||% character(), vars)
  attr(d, "hidden") <- if (length(h)) h
  mr_drop_vars(d, vars)
}

# Renombrar en bloque ----------------------------------------------------------------------
# La sustitución se escribe como en otras herramientas: $1, $2 (o \1) para los grupos
# capturados y \U / \L delante para pasar a mayúsculas o minúsculas.
regex_replacement <- function(x) gsub("\\$([0-9])", "\\\\\\1", x %||% "")

# Nombres nuevos que daría un patrón; NULL si el patrón no es válido.
regex_rename_preview <- function(vars, pattern, replacement, all = TRUE, ignore_case = FALSE) {
  if (!nzchar(pattern %||% "")) return(stats::setNames(vars, vars))
  f <- if (isTRUE(all)) gsub else sub
  r <- regex_replacement(replacement)
  out <- tryCatch(f(pattern, r, vars, perl = TRUE, ignore.case = isTRUE(ignore_case)),
                  error = function(e) NULL, warning = function(w) NULL)
  if (is.null(out)) return(NULL)
  stats::setNames(out, vars)
}

# Problemas de un renombrado: nombres no válidos o repetidos. Vector con un texto por
# variable ("" si no hay problema).
rename_problems <- function(all_names, map) {
  nuevo <- all_names
  nuevo[match(names(map), all_names)] <- unname(map)
  dup <- nuevo[duplicated(nuevo)]
  vapply(names(map), function(v) {
    n <- map[[v]]
    if (!nzchar(n)) "queda vacío"
    else if (!grepl("^[[:alpha:]][[:alnum:]_.]*$", n)) "no es un nombre válido"
    else if (n %in% dup) "se repite"
    else ""
  }, character(1))
}

rename_many <- function(d, map) {
  map <- unlist(map)
  if (!length(map)) stop("No hay variables que renombrar.")
  for (v in names(map)) need_var(d, v)
  p <- rename_problems(names(d), map)
  if (any(nzchar(p))) {
    i <- which(nzchar(p))[1]
    stop("El nombre «", map[[i]], "» (", names(map)[i], ") ", p[i], ".")
  }
  names(d)[match(names(map), names(d))] <- unname(map)
  meta_rename(d, map)
}

# Ordenar ------------------------------------------------------------------------------------
# s$vars: las variables en el orden pedido; las que no están (p. ej. creadas
# después) siguen detrás, en su orden.
order_vars <- function(d, vars) {
  vars <- unique(as.character(unlist(vars)))
  for (v in vars) need_var(d, v)
  keep <- lapply(c("mrsets", "vgroups", "hidden"), function(a) attr(d, a, exact = TRUE))
  out <- d[c(vars, setdiff(names(d), vars))]
  for (i in seq_along(keep)) attr(out, c("mrsets", "vgroups", "hidden")[i]) <- keep[[i]]
  out
}

# Órdenes rápidos
order_by <- function(d, how, groups = NULL, patterns = NULL, top = TRUE, original = NULL, v = names(d)) {
  v <- intersect(v, names(d))
  switch(how,
    az = v[order(tolower(v))],
    za = v[order(tolower(v), decreasing = TRUE)],
    original = c(intersect(original %||% character(), v), setdiff(v, original %||% character())),
    groups = {
      g <- vgroups(d)
      c(intersect(unique(unlist(lapply(g, `[[`, "vars"))), v), setdiff(v, unlist(lapply(g, `[[`, "vars"))))
    },
    regex = {
      pats <- trimws(unlist(strsplit(patterns %||% "", ",")))
      pats <- pats[nzchar(pats)]
      if (!length(pats)) stop("Escribe al menos una expresión.")
      hit <- character()
      for (p in pats) {
        m <- tryCatch(grepl(p, v, perl = TRUE), error = function(e) stop("La expresión «", p, "» no es válida."))
        hit <- c(hit, setdiff(v[m], hit))
      }
      if (!length(hit)) stop("Ninguna variable coincide con esas expresiones.")
      if (isTRUE(top)) c(hit, setdiff(v, hit)) else c(setdiff(v, hit), hit)
    },
    stop("Orden desconocido."))
}

# Grupos propuestos ----------------------------------------------------------------------------
# Por prefijo común (P3A, P3B, P3C...; valoralideres_1, _2...) con 3 variables o más, y,
# en los estudios del CIS armonizados, las armonizadas y las escalas y Likert convertidas.
vgroup_suggest <- function(d, recipe = list(), dir = NULL) {
  ya <- vapply(vgroups(d), function(g) paste(sort(g$vars), collapse = "\r"), "")
  out <- list()
  add <- function(name, vars) {
    vars <- intersect(names(d), vars)
    if (length(vars) < 2 || paste(sort(vars), collapse = "\r") %in% ya || name %in% names(vgroups(d))) return()
    out[[length(out) + 1]] <<- list(name = name, vars = vars)
  }
  paso <- Filter(function(s) identical(s$op, "cis"), recipe)
  if (length(paso) && !is.null(dir)) {
    harm <- c("genero", "generacion", "edad", "ideologia", "religion", "estudios", "laboral", "profesional", "clase")
    add("Sociodemográficas armonizadas (CIS)", harm)
    conv <- tryCatch(names(cis_convertido(project_original(dir), paso[[1]]$estudio)$datos), error = function(e) NULL)
    add("Escalas y Likert convertidas (CIS)", setdiff(conv, c(harm, "orden", "numentr")))
  }
  # prefijo: sin el último «_algo» (valoralideres_1), sin la letra final tras un
  # número (P3A → P3) o sin el número final (item12 → item)
  v <- names(d)
  pref <- sub("[_.][[:alnum:]]+$", "", v)
  sin <- pref == v
  pref[sin] <- sub("(?<=[0-9])[[:alpha:]]$", "", v[sin], perl = TRUE)
  sin <- pref == v
  pref[sin] <- sub("[0-9]+$", "", v[sin])
  pref[pref == v | nchar(pref) < 2] <- NA
  for (p in unique(stats::na.omit(pref))) {
    vs <- v[!is.na(pref) & pref == p]
    if (length(vs) >= 3) add(p, vs)
  }
  out
}

# Selector «Añadir las variables de un grupo» para los selectores de varias variables.
group_adder_ui <- function(id, d) {
  g <- vgroups(d)
  if (!length(g)) return(NULL)
  div(class = "c-group-adder",
    selectInput(id, NULL, width = "100%",
      choices = c(stats::setNames("", "＋ Añadir las variables de un grupo…"),
                  stats::setNames(names(g), paste0(names(g), " (", vapply(g, function(x) length(x$vars), 1L), ")")))))
}

# Al elegir un grupo, añade sus variables (las que están entre las opciones) a la selección.
group_adder_server <- function(input, session, adder, target, data, choices) {
  observeEvent(input[[adder]], {
    g <- input[[adder]]
    req(nzchar(g))
    vs <- intersect(vgroup_vars(data(), g), unname(unlist(choices())))
    updateSelectizeInput(session, target, selected = union(input[[target]], vs))
    updateSelectInput(session, adder, selected = "")
    if (!length(vs)) showNotification("Ninguna variable de ese grupo sirve aquí.", type = "warning")
  }, ignoreInit = TRUE)
}
