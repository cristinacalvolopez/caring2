# Informes y diapositivas -------------------------------------------------------------------
# Un documento es una lista (se guarda como JSON):
#   list(id, title, type = "slides" | "report", mode = "free" | "blocks",
#        theme = "clasico" | "claro" | "profesional" | "divertido" | "academico" (ver themes.R),
#        pages = list(list(id, kind, bg, elements = list(...))))
# Cada elemento: list(id, kind = "text" | "shape" | "image" | "result",
#   x, y, w, h (en puntos: la diapositiva mide 960 × 540 y la página A4 595 × 842),
#   text, style = list(size, bold, italic, color, align, bg), ref (id del resultado),
#   src (imagen subida))

page_size <- function(type) if (identical(type, "report")) c(w = 595, h = 842) else c(w = 960, h = 540)

el <- function(kind, x, y, w, h, ...) {
  c(list(id = substr(gsub("-", "", new_id()), 1, 10), kind = kind, x = x, y = y, w = w, h = h), list(...))
}

txt_style <- function(size = 18, bold = FALSE, color = "#000000", align = "left", bg = NULL, italic = FALSE) {
  compact(list(size = size, bold = bold, italic = italic, color = color, align = align, bg = bg))
}

new_document <- function(title, type = "slides", theme = "clasico") {
  pages <- if (type == "report") list(page_template("portada", "report", title, theme))
           else list(page_template("portada", "slides", title, theme), page_template("contenido", "slides", theme = theme))
  list(id = substr(gsub("-", "", new_id()), 1, 12), title = title, type = type, mode = "free", theme = theme,
       created = now_utc(), modified = now_utc(), pages = pages)
}

doc_path <- function(dir, id) {
  if (!grepl("^[a-f0-9]+$", id)) stop("Documento no válido")
  file.path(dir, "informes", paste0(id, ".json"))
}

save_document <- function(dir, doc) {
  dir.create(file.path(dir, "informes"), showWarnings = FALSE)
  doc$modified <- now_utc()
  jsonlite::write_json(doc, doc_path(dir, doc$id), auto_unbox = TRUE, null = "null", digits = NA, pretty = FALSE)
  invisible(doc$id)
}

load_document <- function(dir, id) {
  jsonlite::read_json(doc_path(dir, id), simplifyVector = FALSE)
}

list_documents <- function(dir) {
  f <- list.files(file.path(dir, "informes"), pattern = "\\.json$", full.names = TRUE)
  if (!length(f)) return(data.frame(id = character(), title = character(), type = character(), modified = character()))
  r <- lapply(f, function(x) {
    d <- jsonlite::read_json(x, simplifyVector = FALSE)
    data.frame(id = d$id, title = d$title, type = d$type, modified = d$modified %||% "")
  })
  r <- do.call(rbind, r)
  r[order(r$modified, decreasing = TRUE), ]
}

delete_document <- function(dir, id) unlink(doc_path(dir, id))

# Imágenes subidas al editor
image_dir <- function(dir) file.path(dir, "informes", "img")

save_image <- function(dir, path, name) {
  ext <- tolower(tools::file_ext(name))
  if (!ext %in% c("png", "jpg", "jpeg", "gif", "svg", "webp")) stop("Formato de imagen no admitido.")
  dir.create(image_dir(dir), showWarnings = FALSE, recursive = TRUE)
  id <- paste0(substr(gsub("-", "", new_id()), 1, 12), ".", ext)
  file.copy(path, file.path(image_dir(dir), id))
  id
}

image_file <- function(dir, src) {
  if (is.null(src) || !grepl("^[a-f0-9]+\\.(png|jpg|jpeg|gif|svg|webp)$", src)) return(NULL)
  f <- file.path(image_dir(dir), src)
  if (file.exists(f)) f else NULL
}

mime_of <- function(f) {
  switch(tolower(tools::file_ext(f)), png = "image/png", jpg = , jpeg = "image/jpeg", gif = "image/gif",
         svg = "image/svg+xml", webp = "image/webp", "application/octet-stream")
}

# Recursos que necesita el editor para pintar resultados e imágenes (data URIs).
editor_assets <- function(dir, results) {
  res <- lapply(results, function(r) {
    png <- file.path(dir, "resultados", paste0(r$id, ".png"))
    full <- load_result(dir, r$id)
    tab_html <- if (identical(r$kind, "tabla") && !is.null(full$object$table))
      as.character(html_table(full$object$table)) else NULL
    list(id = r$id, kind = r$kind, title = r$title, source = r$source,
         png = if (file.exists(png)) data_uri(png, "image/png"), html = tab_html,
         interactive = identical(r$kind, "red"))
  })
  names(res) <- vapply(results, `[[`, "", "id")
  imgs <- list.files(image_dir(dir), full.names = TRUE)
  img <- lapply(imgs, function(f) data_uri(f, mime_of(f)))
  names(img) <- basename(imgs)
  list(results = res, images = img)
}

# Exportación ---------------------------------------------------------------------------

css_color <- function(x, default = "#000000") if (is.null(x) || !nzchar(x)) default else x

# HTML autocontenido. Las redes se incluyen interactivas.
export_html <- function(dir, doc, file, present = FALSE, static = FALSE) {
  sz <- page_size(doc$type)
  nets <- list()
  slides <- doc$type == "slides"
  page_html <- function(p, i) {
    els <- lapply(p$elements, function(e) {
      st <- sprintf("left:%spx;top:%spx;width:%spx;height:%spx;", e$x, e$y, e$w, e$h)
      inner <- switch(e$kind,
        text = {
          s <- e$style %||% list()
          sprintf('<div class="t" style="font-size:%spx;font-weight:%s;font-style:%s;color:%s;text-align:%s;%s">%s</div>',
                  s$size %||% 18, if (isTRUE(s$bold)) 700 else 400, if (isTRUE(s$italic)) "italic" else "normal",
                  css_color(s$color), s$align %||% "left",
                  if (!is.null(s$bg)) paste0("background:", s$bg, ";") else "",
                  gsub("\n", "<br>", htmltools::htmlEscape(e$text %||% "")))
        },
        shape = sprintf('<div style="width:100%%;height:100%%;background:%s;border-radius:%spx"></div>',
                        css_color(e$style$bg, "#2F7BEE"), e$style$radius %||% 0),
        image = {
          f <- image_file(dir, e$src)
          if (is.null(f)) "" else sprintf('<img src="%s" style="width:100%%;height:100%%;object-fit:contain">', data_uri(f, mime_of(f)))
        },
        result = {
          r <- load_result(dir, e$ref %||% "")
          if (is.null(r)) "" else if (identical(r$kind, "red")) {
            h <- file.path(dir, "resultados", paste0(r$id, ".html"))
            png <- file.path(dir, "resultados", paste0(r$id, ".png"))
            img <- if (file.exists(png)) sprintf('<img src="%s" style="width:100%%;height:100%%;object-fit:contain">', data_uri(png, "image/png")) else ""
            if (!static && file.exists(h)) {
              nets[[r$id]] <<- paste(readLines(h, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
              paste0(img, sprintf('<button class="net-open" onclick="openNet(\'%s\', event)">Ver red interactiva</button>', r$id))
            } else img
          } else if (identical(r$kind, "tabla")) {
            paste0('<div class="tbl">', as.character(html_table(r$object$table)), "</div>")
          } else {
            png <- file.path(dir, "resultados", paste0(r$id, ".png"))
            if (file.exists(png)) sprintf('<img src="%s" style="width:100%%;height:100%%;object-fit:contain">', data_uri(png, "image/png")) else ""
          }
        }, "")
      if (identical(e$kind, "result") && !is.null(e$style$bg)) {
        st <- paste0(st, sprintf("background:%s;border-radius:%spx;padding:8px;box-sizing:border-box;",
                                 e$style$bg, e$style$radius %||% 0))
      }
      sprintf('<div class="el" style="%s">%s</div>', st, inner)
    })
    sprintf('<section class="page" id="p%d" style="background:%s">%s</section>', i, css_color(p$bg, "#FFFFFF"),
            paste(unlist(els), collapse = "\n"))
  }
  pages <- vapply(seq_along(doc$pages), function(i) page_html(doc$pages[[i]], i), "")
  fonts <- paste(readLines(app_file("www", "fonts", "roboto.css"), warn = FALSE), collapse = "\n")
  for (f in list.files(app_file("www", "fonts"), pattern = "woff2$")) {
    fonts <- gsub(paste0("url('", f, "')"), paste0("url('", data_uri(app_file("www", "fonts", f), "font/woff2"), "')"), fonts, fixed = TRUE)
  }
  css <- sprintf('
%s
body{margin:0;background:%s;font-family:Roboto,Arial,sans-serif}
.page{position:relative;width:%dpx;height:%dpx;margin:24px auto;box-shadow:0 2px 10px rgba(0,0,0,.15);overflow:hidden;transform-origin:top left}
.el{position:absolute;overflow:hidden}
.t{width:100%%;height:100%%;line-height:1.3;white-space:normal;box-sizing:border-box;padding:2px}
.tbl{width:100%%;height:100%%;overflow:auto;font-size:12px}
.c-table{width:100%%;border-collapse:collapse}.c-table th{background:#2F7BEE;color:#fff;padding:4px 8px;text-align:left}
.c-table td{padding:4px 8px;border-top:1px solid #E3E3E3}.c-table .num{text-align:right}
.net-open{position:absolute;right:6px;bottom:6px;background:#2F7BEE;color:#fff;border:0;border-radius:5px;padding:6px 10px;font:inherit;font-size:13px;cursor:pointer}
.net-open:hover{background:#135DCD}
#netbox{position:fixed;inset:0;background:rgba(0,0,0,.6);display:none;z-index:99;padding:3vh 3vw}
#netbox.on{display:block}#netbox iframe{width:100%%;height:100%%;border:0;background:#fff;border-radius:5px}
#netbox button{position:absolute;top:calc(3vh - 14px);right:calc(3vw - 14px);width:32px;height:32px;border-radius:50%%;border:0;background:#fff;font-size:18px;cursor:pointer}
@media print{.net-open{display:none}}
@media print{body{background:#fff}.page{margin:0;box-shadow:none;page-break-after:always}}
@page{size:%dpx %dpx;margin:0}
%s',
    fonts, if (present) "#000" else "#E3E3E3", sz["w"], sz["h"], sz["w"], sz["h"],
    if (present) ".page{display:none;margin:0;position:absolute;top:0;left:0}.page.on{display:block}" else "")
  js <- if (present) sprintf('
<script>
var ps=document.querySelectorAll(".page"),i=0,W=%d,H=%d;
function fit(){var s=Math.min(innerWidth/W,innerHeight/H);ps.forEach(function(p){p.style.transform="translate("+((innerWidth-W*s)/2)+"px,"+((innerHeight-H*s)/2)+"px) scale("+s+")"})}
function show(n){i=Math.max(0,Math.min(ps.length-1,n));ps.forEach(function(p,k){p.classList.toggle("on",k===i)})}
addEventListener("keydown",function(e){if(["ArrowRight","PageDown"," "].indexOf(e.key)>=0)show(i+1);if(["ArrowLeft","PageUp"].indexOf(e.key)>=0)show(i-1)});
addEventListener("click",function(e){if(!e.target.closest("#netbox")&&!e.target.closest(".net-open"))show(i+1)});
addEventListener("resize",fit);fit();show(0);
</script>', sz["w"], sz["h"]) else ""
  net_js <- if (length(nets)) paste0(
    '<div id="netbox"><button onclick="closeNet(event)" aria-label="Cerrar">✕</button><iframe title="Red interactiva"></iframe></div>',
    '<script>var NETS=', jsonlite::toJSON(nets, auto_unbox = TRUE), ';',
    'function openNet(id,e){e.stopPropagation();var b=document.getElementById("netbox");b.querySelector("iframe").srcdoc=NETS[id];b.classList.add("on")}',
    'function closeNet(e){e.stopPropagation();var b=document.getElementById("netbox");b.classList.remove("on");b.querySelector("iframe").srcdoc=""}',
    '</script>') else ""
  html <- paste0('<!DOCTYPE html><html lang="es"><head><meta charset="utf-8"><title>',
                 htmltools::htmlEscape(doc$title), '</title><style>', css, '</style></head><body>',
                 paste(pages, collapse = "\n"), net_js, js, '</body></html>')
  writeLines(html, file, useBytes = TRUE)
  invisible(file)
}

export_pdf <- function(dir, doc, file) {
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp))
  export_html(dir, doc, tmp, static = TRUE)
  sz <- page_size(doc$type)
  ok <- tryCatch({
    b <- chromote::ChromoteSession$new()
    on.exit(b$close(), add = TRUE)
    loaded <- b$Page$loadEventFired(wait_ = FALSE, timeout_ = 60)
    b$Page$navigate(paste0("file://", normalizePath(tmp)), wait_ = FALSE)
    b$wait_for(loaded)
    Sys.sleep(2)
    r <- b$Page$printToPDF(printBackground = TRUE, preferCSSPageSize = TRUE,
                           paperWidth = sz[["w"]] / 72, paperHeight = sz[["h"]] / 72,
                           marginTop = 0, marginBottom = 0, marginLeft = 0, marginRight = 0)
    writeBin(base64enc::base64decode(r$data), file)
    TRUE
  }, error = function(e) { message("PDF: ", conditionMessage(e)); FALSE })
  if (!ok) stop("No se ha podido generar el PDF (hace falta Google Chrome o Chromium).")
  invisible(file)
}

# PowerPoint 16:9. Las unidades del editor son puntos: 960 pt = 13,333 pulgadas.
export_pptx <- function(dir, doc, file) {
  tpl <- app_file("templates", "caring_16x9.pptx")
  pp <- officer::read_pptx(tpl)
  inch <- function(v) as.numeric(v) / 72
  for (p in doc$pages) {
    pp <- officer::add_slide(pp, layout = "Blank", master = "Office Theme")
    if (!is.null(p$bg) && !identical(toupper(p$bg), "#FFFFFF")) {
      pp <- officer::ph_with(pp, officer::block_list(officer::fpar("")),
                             location = officer::ph_location(left = 0, top = 0, width = 13.333, height = 7.5, bg = p$bg))
    }
    for (e in p$elements) {
      loc_args <- list(left = inch(e$x), top = inch(e$y), width = inch(e$w), height = inch(e$h))
      if (e$kind == "shape") {
        if (isTRUE(as.numeric(e$style$radius %||% 0) > 0)) {
          # PowerPoint (vía officer) no tiene formas redondeadas: se dibujan como imagen.
          img <- rounded_png(e$w, e$h, as.numeric(e$style$radius), css_color(e$style$bg, "#2F7BEE"))
          pp <- officer::ph_with(pp, officer::external_img(img, width = inch(e$w), height = inch(e$h)),
                                 location = do.call(officer::ph_location, loc_args), use_loc_size = TRUE)
        } else {
          loc <- do.call(officer::ph_location, c(loc_args, list(bg = css_color(e$style$bg, "#2F7BEE"))))
          pp <- officer::ph_with(pp, officer::block_list(officer::fpar("")), location = loc)
        }
      } else if (e$kind == "text") {
        loc <- do.call(officer::ph_location, c(loc_args, if (!is.null(e$style$bg)) list(bg = e$style$bg)))
        pp <- officer::ph_with(pp, office_text(e), location = loc)
      } else if (e$kind == "image") {
        f <- image_file(dir, e$src)
        if (!is.null(f) && tolower(tools::file_ext(f)) %in% c("png", "jpg", "jpeg", "gif")) {
          pp <- officer::ph_with(pp, officer::external_img(f, width = inch(e$w), height = inch(e$h)),
                                 location = do.call(officer::ph_location, loc_args), use_loc_size = TRUE)
        }
      } else if (e$kind == "result") {
        r <- load_result(dir, e$ref %||% "")
        if (is.null(r)) next
        if (!is.null(e$style$bg)) {
          card <- rounded_png(e$w, e$h, as.numeric(e$style$radius %||% 0), e$style$bg)
          pp <- officer::ph_with(pp, officer::external_img(card, width = inch(e$w), height = inch(e$h)),
                                 location = do.call(officer::ph_location, loc_args), use_loc_size = TRUE)
          e$x <- e$x + 8; e$y <- e$y + 8; e$w <- e$w - 16; e$h <- e$h - 16
          loc_args <- list(left = inch(e$x), top = inch(e$y), width = inch(e$w), height = inch(e$h))
        }
        if (identical(r$kind, "tabla") && !is.null(r$object$table)) {
          ft <- flextable::fit_to_width(flextable_caring(r$object$table), max_width = inch(e$w))
          pp <- officer::ph_with(pp, ft, location = do.call(officer::ph_location, loc_args))
        } else {
          png <- file.path(dir, "resultados", paste0(r$id, ".png"))
          if (file.exists(png)) {
            fit <- fit_image(png, e$w, e$h)
            pp <- officer::ph_with(pp, officer::external_img(png, width = inch(fit$w), height = inch(fit$h)),
                                   location = officer::ph_location(left = inch(e$x + fit$dx), top = inch(e$y + fit$dy),
                                                                   width = inch(fit$w), height = inch(fit$h)),
                                   use_loc_size = TRUE)
          }
        }
      }
    }
  }
  print(pp, target = file)
  invisible(file)
}

# Forma (rectángulo redondeado o círculo) como imagen PNG transparente.
rounded_png <- function(w, h, radius, color) {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = max(2, round(w * 2)), height = max(2, round(h * 2)), bg = "transparent")
  grid::grid.newpage()
  r <- min(radius, w / 2, h / 2) / min(w, h)
  grid::grid.roundrect(width = grid::unit(1, "npc"), height = grid::unit(1, "npc"),
                       r = grid::unit(r, "snpc"), gp = grid::gpar(fill = color, col = NA))
  grDevices::dev.off()
  f
}

# ¿Color claro? (para pasar a negro los textos claros al exportar a Word, que siempre es blanco)
is_light <- function(col) {
  if (is.null(col) || !nzchar(col)) return(FALSE)
  rgb <- tryCatch(grDevices::col2rgb(col)[, 1] / 255, error = function(e) c(0, 0, 0))
  sum(c(0.299, 0.587, 0.114) * rgb) > 0.6
}

office_text <- function(e) {
  s <- e$style %||% list()
  props <- officer::fp_text(font.size = as.numeric(s$size %||% 18), bold = isTRUE(s$bold), italic = isTRUE(s$italic),
                            color = css_color(s$color), font.family = "Roboto")
  par <- officer::fp_par(text.align = switch(s$align %||% "left", center = "center", right = "right", justify = "justify", "left"))
  lines <- strsplit(e$text %||% "", "\n", fixed = TRUE)[[1]]
  if (!length(lines)) lines <- ""
  do.call(officer::block_list, lapply(lines, function(l) officer::fpar(officer::ftext(l, props), fp_p = par)))
}

# Ajusta una imagen a una caja manteniendo proporciones.
fit_image <- function(png, w, h) {
  dims <- tryCatch({
    con <- file(png, "rb"); on.exit(close(con))
    hdr <- readBin(con, "raw", 24)
    c(sum(as.integer(hdr[17:20]) * 256^(3:0)), sum(as.integer(hdr[21:24]) * 256^(3:0)))
  }, error = function(e) c(w, h))
  ratio <- dims[1] / dims[2]
  if (w / h > ratio) { nh <- h; nw <- h * ratio } else { nw <- w; nh <- w / ratio }
  list(w = nw, h = nh, dx = (w - nw) / 2, dy = (h - nh) / 2)
}

# Word: los elementos se colocan en orden de lectura (página, de arriba abajo).
export_docx <- function(dir, doc, file) {
  dx <- officer::read_docx()
  first <- TRUE
  for (p in doc$pages) {
    if (!first) dx <- officer::body_add_break(dx)
    first <- FALSE
    els <- p$elements
    if (!length(els)) next
    ord <- order(vapply(els, function(e) as.numeric(e$y), 0), vapply(els, function(e) as.numeric(e$x), 0))
    for (e in els[ord]) {
      if (e$kind == "text" && nzchar(trimws(e$text %||% ""))) {
        size <- as.numeric(e$style$size %||% 12)
        if (size >= 26) {
          dx <- officer::body_add_par(dx, e$text, style = "heading 1")
        } else if (size >= 18 && isTRUE(e$style$bold)) {
          dx <- officer::body_add_par(dx, e$text, style = "heading 2")
        } else {
          e2 <- e; e2$style$color <- if (is_light(e$style$color)) "#000000" else e$style$color
          e2$style$size <- min(size, 12)
          for (fp in office_text(e2)) dx <- officer::body_add_fpar(dx, fp)
        }
      } else if (e$kind == "image") {
        f <- image_file(dir, e$src)
        if (!is.null(f) && tolower(tools::file_ext(f)) %in% c("png", "jpg", "jpeg", "gif")) {
          fit <- fit_image(f, 6.3 * 72, 6 * 72)
          dx <- officer::body_add_img(dx, f, width = fit$w / 72, height = fit$h / 72)
        }
      } else if (e$kind == "result") {
        r <- load_result(dir, e$ref %||% "")
        if (is.null(r)) next
        if (identical(r$kind, "tabla") && !is.null(r$object$table)) {
          dx <- officer::body_add_par(dx, r$title, style = "table title")
          dx <- flextable::body_add_flextable(dx, flextable::fit_to_width(flextable_caring(r$object$table), 6.3))
        } else {
          png <- file.path(dir, "resultados", paste0(r$id, ".png"))
          if (file.exists(png)) {
            fit <- fit_image(png, 6.3 * 72, 6 * 72)
            dx <- officer::body_add_img(dx, png, width = fit$w / 72, height = fit$h / 72)
            dx <- officer::body_add_par(dx, r$title, style = "Image Caption")
          }
        }
      }
    }
  }
  print(dx, target = file)
  invisible(file)
}
