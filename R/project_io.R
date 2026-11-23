# Exportar e importar un proyecto completo ---------------------------------------------------
# Archivo .caring (un zip) con: datos originales, archivos unidos, historial de cambios,
# resultados, informes e imágenes. Al importar se trata como un archivo de fuera:
#   - solo se aceptan nombres de archivo esperados, sin rutas, y con tamaño limitado;
#   - nunca se usa readRDS(): el historial y los resultados van en JSON y solo se
#     aceptan si contienen datos (texto, números, listas), nunca código;
#   - las páginas HTML de las redes no se importan: se vuelven a generar con netCoin;
#   - las imágenes solo pueden ser PNG, JPG, GIF o WebP (no SVG), y del texto de los
#     informes se quitan scripts y atributos de eventos.

project_format <- "caring2-proyecto"

# JSON con tipos de R exactos (jsonlite::serializeJSON) y comprobación de que solo hay datos.
to_json_data <- function(x) jsonlite::serializeJSON(x, digits = NA)

from_json_data <- function(txt) {
  tipos_ok <- c("list", "character", "double", "integer", "logical", "NULL", "complex")
  check <- function(node) {
    if (is.list(node)) {
      if (!is.null(node$type) && is.character(node$type) && !node$type %in% tipos_ok) {
        stop("El archivo contiene un tipo de dato no permitido (", node$type, ").")
      }
      for (k in node) check(k)
    }
  }
  raw <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  check(raw)
  jsonlite::unserializeJSON(txt)
}

export_project <- function(con, project_id, file) {
  dir <- project_path(con, project_id)
  p <- get_project(con, project_id)
  tmp <- tempfile("exp-"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  writeLines(jsonlite::toJSON(list(formato = project_format, version = 1, nombre = p$nombre, archivo = p$archivo,
                                   exportado = now_utc()), auto_unbox = TRUE, pretty = TRUE),
             file.path(tmp, "proyecto.json"), useBytes = TRUE)
  orig <- project_original(dir)
  file.copy(orig, file.path(tmp, basename(orig)))
  writeLines(to_json_data(read_recipe(dir)), file.path(tmp, "receta.json"), useBytes = TRUE)
  if (dir.exists(annex_dir(dir))) {
    dir.create(file.path(tmp, "anexos"))
    file.copy(list.files(annex_dir(dir), full.names = TRUE), file.path(tmp, "anexos"))
  }
  dir.create(file.path(tmp, "resultados"))
  for (f in list.files(file.path(dir, "resultados"), pattern = "\\.rds$", full.names = TRUE)) {
    r <- readRDS(f)                              # archivo propio del servidor: es de confianza
    r$object$plot <- NULL
    id <- tools::file_path_sans_ext(basename(f))
    writeLines(to_json_data(r), file.path(tmp, "resultados", paste0(id, ".json")), useBytes = TRUE)
    png <- file.path(dir, "resultados", paste0(id, ".png"))
    if (file.exists(png)) file.copy(png, file.path(tmp, "resultados", basename(png)))
  }
  inf <- file.path(dir, "informes")
  if (dir.exists(inf)) {
    dir.create(file.path(tmp, "informes", "img"), recursive = TRUE)
    file.copy(list.files(inf, pattern = "\\.json$", full.names = TRUE), file.path(tmp, "informes"))
    imgs <- list.files(file.path(inf, "img"), full.names = TRUE)
    if (length(imgs)) file.copy(imgs, file.path(tmp, "informes", "img"))
  }
  if (file.exists(file)) unlink(file)
  zip::zip(file, files = list.files(tmp, recursive = TRUE), root = tmp, mode = "mirror")
  invisible(file)
}

# Nombres de archivo que se aceptan dentro de un .caring
import_patterns <- c(
  "^proyecto\\.json$",
  paste0("^original\\.(", paste(names(formatos_admitidos), collapse = "|"), ")$"),
  "^receta\\.json$",
  "^anexos/[a-f0-9]{8,40}\\.[a-z0-9]{1,6}$",
  "^resultados/[a-f0-9]{6,40}\\.(json|png)$",
  "^informes/[a-f0-9]{1,64}\\.json$",
  "^informes/img/[A-Za-z0-9_.-]{1,100}\\.(png|jpe?g|gif|webp)$"
)

sanitize_html <- function(x) {
  if (!is.character(x)) return(x)
  x <- gsub("<\\s*/?\\s*(script|iframe|object|embed|link|meta|style|base|form)[^>]*>", "", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\s+on[a-z]+\\s*=\\s*(\"[^\"]*\"|'[^']*'|[^\\s>]+)", "", x, ignore.case = TRUE, perl = TRUE)
  gsub("(href|src)\\s*=\\s*([\"'])\\s*javascript:[^\"']*\\2", "\\1=\\2#\\2", x, ignore.case = TRUE, perl = TRUE)
}

sanitize_tree <- function(x) {
  if (is.list(x)) { for (k in seq_along(x)) if (!is.null(x[[k]])) x[[k]] <- sanitize_tree(x[[k]]); x }
  else sanitize_html(x)
}

import_project <- function(con, user, zip_path, nombre = NULL, storage = caring_config()$storage) {
  fail <- function(msg) list(ok = FALSE, error = msg)
  lst <- tryCatch(zip::zip_list(zip_path), error = function(e) NULL)
  if (is.null(lst) || !nrow(lst)) return(fail("El archivo no es un proyecto de CARING (.caring)."))
  lst <- data.frame(Name = lst$filename, Length = lst$uncompressed_size, stringsAsFactors = FALSE)
  lst <- lst[!grepl("/$", lst$Name), , drop = FALSE]                   # carpetas
  bad <- lst$Name[!vapply(lst$Name, function(n) any(vapply(import_patterns, grepl, logical(1), n, perl = TRUE)), logical(1))]
  if (length(bad)) return(fail(paste0("El proyecto contiene archivos no admitidos: ", paste(utils::head(bad, 3), collapse = ", "), ".")))
  if (!"proyecto.json" %in% lst$Name || !any(grepl("^original\\.", lst$Name))) return(fail("Al proyecto le faltan los datos originales."))
  total <- sum(lst$Length)
  max_total <- as.numeric(get_setting(con, "max_mb")) * 1024^2 * 4
  if (total > max_total) return(fail(paste0("El proyecto ocupa ", fmt_bytes(total), " descomprimido: es demasiado grande.")))
  if (user_bytes(con, user$id) + total > user_quota_mb(con, user) * 1024^2) {
    return(fail("No queda espacio en tu cuenta. Borra algún proyecto o pide más cuota."))
  }
  tmp <- tempfile("imp-"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  zip::unzip(zip_path, files = lst$Name, exdir = tmp)
  meta <- tryCatch(jsonlite::fromJSON(file.path(tmp, "proyecto.json")), error = function(e) NULL)
  if (!identical(meta$formato, project_format)) return(fail("El archivo no es un proyecto de CARING 2."))
  orig <- list.files(tmp, pattern = "^original\\.", full.names = TRUE)[1]
  d <- tryCatch(read_any(orig), error = function(e) e)
  if (inherits(d, "error")) return(fail(paste("No se han podido leer los datos del proyecto:", error_es(d))))
  recipe <- list()
  if (file.exists(file.path(tmp, "receta.json"))) {
    recipe <- tryCatch(from_json_data(paste(readLines(file.path(tmp, "receta.json"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")),
                       error = function(e) e)
    if (inherits(recipe, "error") || !is.list(recipe)) return(fail("El historial de cambios del proyecto no es válido."))
  }

  id <- new_id()
  dir <- file.path(user_dir(user$id, storage), "projects", id)
  for (sub in c("resultados", "redes", "informes/img")) dir.create(file.path(dir, sub), recursive = TRUE)
  ok <- FALSE
  on.exit(if (!ok) unlink(dir, recursive = TRUE), add = TRUE)
  file.copy(orig, file.path(dir, basename(orig)))
  if (dir.exists(file.path(tmp, "anexos"))) {
    dir.create(annex_dir(dir))
    file.copy(list.files(file.path(tmp, "anexos"), full.names = TRUE), annex_dir(dir))
  }
  # La receta tiene que poder aplicarse a los datos
  chk <- tryCatch(apply_recipe(d, recipe, dir), error = function(e) e)
  if (inherits(chk, "error")) return(fail(paste("El historial de cambios no se puede aplicar a los datos:", error_es(chk))))
  write_recipe(dir, recipe)

  # Resultados (las redes se vuelven a dibujar)
  for (f in list.files(file.path(tmp, "resultados"), pattern = "\\.json$", full.names = TRUE)) {
    r <- tryCatch(from_json_data(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")), error = function(e) NULL)
    rid <- tools::file_path_sans_ext(basename(f))
    if (!is.list(r) || !identical(r$id, rid)) next
    r$title <- sanitize_html(as.character(r$title %||% ""))
    r$html <- NULL
    png <- file.path(tmp, "resultados", paste0(rid, ".png"))
    if (identical(r$kind, "red") && !is.null(r$object$spec)) {
      h <- file.path(dir, "resultados", paste0(rid, ".html"))
      net <- tryCatch(suppressWarnings(suppressMessages(net_build(chk, r$object$spec))), error = function(e) NULL)
      if (!is.null(net) && isTRUE(tryCatch({ net_render_html(net, h); TRUE }, error = function(e) FALSE))) r$html <- basename(h)
    }
    if (file.exists(png)) file.copy(png, file.path(dir, "resultados", basename(png)))
    saveRDS(r, file.path(dir, "resultados", paste0(rid, ".rds")))
  }
  # Informes e imágenes
  for (f in list.files(file.path(tmp, "informes"), pattern = "\\.json$", full.names = TRUE)) {
    doc <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(doc)) next
    writeLines(jsonlite::toJSON(sanitize_tree(doc), auto_unbox = TRUE, null = "null", digits = NA),
               file.path(dir, "informes", basename(f)), useBytes = TRUE)
  }
  imgs <- list.files(file.path(tmp, "informes", "img"), full.names = TRUE)
  if (length(imgs)) file.copy(imgs, file.path(dir, "informes", "img"))

  nombre <- trimws(nombre %||% meta$nombre %||% tools::file_path_sans_ext(basename(zip_path)))
  if (!nzchar(nombre)) nombre <- "Proyecto importado"
  db_run(con, "INSERT INTO projects (id, owner_id, nombre, archivo, n_casos, n_vars, bytes, creado, modificado)
               VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
         list(id, user$id, nombre, as.character(meta$archivo %||% basename(orig)), nrow(chk), ncol(chk),
              dir_bytes(dir), now_utc(), now_utc()))
  audit(con, user$id, "proyecto_importado", id)
  ok <- TRUE
  list(ok = TRUE, id = id)
}
