# ==============================================================================
# Chart data: every chart saved with ggsave() also gets <name>_data.csv beside
# it, holding the data the chart plots. The dashboard links each chart to its
# CSV ("Download data") and reads the latest period from it ("Data to").
#
# Sourced near the top of the 1b-1j scripts. It replaces ggsave() with a
# version that saves the chart as usual and then writes the CSV, so no chart
# code needs to change. Patents writes its own CSVs, so it sets
# CHART_DATA_CSV <- FALSE after sourcing this file and keeps only the hover
# tooltips (below).
#   * US$ twins (<name>_usd.png) get no CSV: the A$ chart's CSV covers both.
#   * Charts made of several panels (patchwork) get one CSV with a "panel"
#     column naming each panel.
#   * The Trade in goods/services product charts (hundreds of them) are skipped.
# ==============================================================================

CHART_DATA_SKIP <- "^(goods_(exports|imports)|credits|debits)_[0-9]"
CHART_DATA_CSV  <- TRUE

# Data frames behind one ggplot: the plot's own data plus any layer that brings
# its own (e.g. labels or reference lines). Columns are kept as text so that
# layers with different columns can be stacked.
chart_frames <- function(p) {
  as_text <- function(d) {
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    d[] <- lapply(d, function(x) if (inherits(x, "Date")) format(x, "%Y-%m-%d") else as.character(x))
    d
  }
  out <- list()
  if (is.data.frame(p$data) && nrow(p$data) > 0) out[[1]] <- as_text(p$data)
  for (l in p$layers) {
    if (is.data.frame(l$data) && nrow(l$data) > 0 && !identical(l$data, p$data))
      out[[length(out) + 1]] <- cbind(as_text(l$data), layer = class(l$geom)[1])
  }
  if (!length(out)) return(NULL)
  dplyr::bind_rows(out)
}

chart_data <- function(plot) {
  if (inherits(plot, "patchwork")) {
    plots <- c(plot$patches$plots, list(plot))
    parts <- lapply(seq_along(plots), function(i) {
      d <- chart_frames(plots[[i]])
      if (is.null(d)) return(NULL)
      ttl <- plots[[i]]$labels$title
      cbind(panel = if (is.null(ttl)) paste("Panel", i) else ttl, d)
    })
    return(dplyr::bind_rows(parts))
  }
  if (inherits(plot, "ggplot")) return(chart_frames(plot))
  NULL
}

write_chart_data <- function(plot, filename) {
  file <- basename(filename)
  if (!grepl("\\.png$", file) || grepl("_usd\\.png$", file) || grepl(CHART_DATA_SKIP, file)) return(invisible(NULL))
  d <- tryCatch(chart_data(plot), error = function(e) NULL)
  if (is.null(d) || !nrow(d)) return(invisible(NULL))
  utils::write.csv(d, sub("\\.png$", "_data.csv", filename), row.names = FALSE, na = "")
  invisible(NULL)
}

ggsave <- function(filename, plot = ggplot2::last_plot(), ...) {
  res <- ggplot2::ggsave(filename, plot, ...)
  if (isTRUE(CHART_DATA_CSV))
    tryCatch(write_chart_data(plot, filename),
             error = function(e) message("Chart data not written for ", filename, ": ", conditionMessage(e)))
  tryCatch(write_hover(plot, filename, spots = time_series_spots, ...),
           error = function(e) message("Hover tooltips not written for ", filename, ": ", conditionMessage(e)))
  invisible(res)
}




# ==============================================================================
# Hover tooltips: write_hover() saves <name>_hover.json beside a chart, listing
# spots on the image (as fractions of its width and height) and the text to
# show when the pointer is over each. theme/chart-hover.lua attaches the file
# to the chart and theme/chart-footer.html shows the text. A chart's US$ twin
# (<name>_usd.png) gets its own <name>_usd_hover.json.
#
# ggsave() above calls it for every chart with time_series_spots(), so every
# chart with time along the x axis gets tooltips with no chart code changing:
# one per data point on lines and points, one per bar or band on columns and
# stacked areas. A chart can also call write_hover() itself with its own spots
# (see the investment-by-type chart in '1i. Investment.R').
#
#   plot    the ggplot (or patchwork) saved as the chart
#   file    the chart's .png path
#   spots   function(built) returning one row per hover spot: PANEL, xmin,
#           xmax, ymin, ymax (in the plot's data units; discrete axes count
#           1, 2, 3 ...) and text (may hold <b> and <br>). built is
#           ggplot_build() of one plot. Limits outside the panel are cut to
#           its edge. For a data point give xmin = xmax and ymin = ymax: the
#           text then shows only when the pointer is within a few pixels.
#   ...     as given to ggsave() (width, height, dpi, units ...)
# ==============================================================================

# The single plots inside a chart: the chart itself, or each plot of a patchwork
hover_leaves <- function(p) {
  if (!inherits(p, "patchwork")) return(list(p))
  unlist(lapply(seq_along(p), function(i) hover_leaves(p[[i]])), recursive = FALSE)
}

# The same chart with each plot's panels outlined in its own marker colour
hover_marked <- function(p, counter) {
  if (inherits(p, "patchwork")) {
    for (i in seq_along(p)) p[[i]] <- hover_marked(p[[i]], counter)
    return(p)
  }
  counter$k <- counter$k + 1
  p + ggplot2::theme(panel.border = ggplot2::element_rect(
    colour = hover_marker(counter$k), fill = NA, linewidth = 1.2))
}
hover_marker <- function(k) sprintf("#FF%02XFE", 3 * k)

# Where each panel sits on the saved image. The chart is saved again with the
# same settings and every panel outlined in a marker colour, and the outlines
# are found in the pixels; this works for facets and patchworks alike.
# Returns, per plot, a list of panel boxes (fractions of the image) by PANEL.
hover_boxes <- function(plot, leaves, ...) {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  counter <- new.env()
  counter$k <- 0
  suppressMessages(ggplot2::ggsave(tmp, hover_marked(plot, counter), ...))
  img <- png::readPNG(tmp)
  H <- dim(img)[1]; W <- dim(img)[2]
  R <- round(img[, , 1] * 255); G <- round(img[, , 2] * 255); B <- round(img[, , 3] * 255)
  runs <- function(idx) {                  # contiguous runs of pixel indices
    if (!length(idx)) return(list())
    brk <- c(0, which(diff(idx) > 3), length(idx))
    lapply(seq_len(length(brk) - 1), function(i) idx[(brk[i] + 1):brk[i + 1]])
  }
  lapply(seq_along(leaves), function(k) {
    m <- R == 255 & G == 3 * k & B == 254
    lay <- tryCatch(ggplot2::ggplot_build(leaves[[k]])$layout$layout, error = function(e) NULL)
    if (is.null(lay)) return(NULL)
    cols <- runs(which(colSums(m) > 0)); rows <- runs(which(rowSums(m) > 0))
    if (length(cols) < max(lay$COL) || length(rows) < max(lay$ROW)) return(NULL)
    boxes <- lapply(seq_len(nrow(lay)), function(i) {
      xr <- range(cols[[lay$COL[i]]]); yr <- range(rows[[lay$ROW[i]]])
      # Panel edges run along the middle of the outline
      t <- sum(m[yr[1]:min(yr[2], yr[1] + 20), round(mean(xr))]) / 2
      c(x0 = (xr[1] - 1 + t) / W, x1 = (xr[2] - t) / W,
        y0 = (yr[1] - 1 + t) / H, y1 = (yr[2] - t) / H)
    })
    names(boxes) <- as.character(lay$PANEL)
    boxes
  })
}

write_hover <- function(plot, file, spots, ...) {
  file <- as.character(file)
  if (!grepl("\\.png$", file) || !requireNamespace("png", quietly = TRUE)) return(invisible(NULL))
  # Every plot is kept (spacers too) so that plot k matches marker colour k
  leaves <- hover_leaves(plot)
  builts <- lapply(leaves, function(p) tryCatch(ggplot2::ggplot_build(p), error = function(e) NULL))
  found <- lapply(builts, function(b) if (is.null(b)) NULL else tryCatch(spots(b), error = function(e) NULL))
  out_json <- sub("\\.png$", "_hover.json", file)
  if (all(vapply(found, function(s) is.null(s) || !nrow(s), logical(1)))) {
    unlink(out_json)                       # no longer a time series: drop an old map
    return(invisible(NULL))
  }
  boxes <- hover_boxes(plot, leaves, ...)
  frac <- function(v, r) pmin(pmax((v - r[1]) / diff(r), 0), 1)
  out <- do.call(rbind, lapply(seq_along(leaves), function(k) {
    s <- found[[k]]
    if (is.null(s) || !nrow(s) || is.null(boxes[[k]])) return(NULL)
    pp <- builts[[k]]$layout$panel_params
    do.call(rbind, lapply(seq_len(nrow(s)), function(j) {
      i <- as.integer(s$PANEL[j])
      b <- boxes[[k]][[as.character(s$PANEL[j])]]
      if (is.null(b)) return(NULL)
      xs <- frac(c(s$xmin[j], s$xmax[j]), pp[[i]]$x.range)
      ys <- frac(c(s$ymin[j], s$ymax[j]), pp[[i]]$y.range)
      data.frame(x0 = b[["x0"]] + min(xs) * (b[["x1"]] - b[["x0"]]),
                 x1 = b[["x0"]] + max(xs) * (b[["x1"]] - b[["x0"]]),
                 y0 = b[["y1"]] - max(ys) * (b[["y1"]] - b[["y0"]]),
                 y1 = b[["y1"]] - min(ys) * (b[["y1"]] - b[["y0"]]),
                 text = s$text[j])
    }))
  }))
  if (is.null(out) || !nrow(out)) return(invisible(NULL))
  out[1:4] <- lapply(out[1:4], round, 5)
  out <- out[!duplicated(out), ]
  jsonlite::write_json(out, out_json, dataframe = "rows")
  invisible(out)
}

# ---- Spots for any chart with time along the x axis ---------------------------

HOVER_POINT_GEOMS <- c("GeomLine", "GeomPath", "GeomPoint", "GeomStep")
HOVER_BAR_GEOMS   <- c("GeomCol", "GeomBar")
HOVER_AREA_GEOMS  <- c("GeomArea", "GeomRibbon")
HOVER_PERIOD_RE   <- "^(\\d{4}|\\d{4}-\\d{2}|\\d{4}-\\d{2}-\\d{2}|[A-Z][a-z]{2,8}\\.? \\d{4}|(Q[1-4]|[A-Z][a-z]{2,8} (qtr|quarter)) \\d{4}|\\d{4} ?Q[1-4])$"

# Rows of a layer that sit on real periods. Stacked areas add points just
# either side of each period (e.g. 2009.986 and 2010.014 around 2010); keep the
# middle of each such cluster.
hover_rows <- function(d, geom) {
  if (!geom %in% HOVER_AREA_GEOMS || !"x" %in% names(d) || nrow(d) < 2) return(d)
  xs <- sort(unique(d$x[is.finite(d$x)]))
  if (length(xs) < 2) return(d)
  cl <- cumsum(c(1, diff(xs) > diff(range(xs)) * 0.003))
  mids <- vapply(split(xs, cl), function(v) v[ceiling(length(v) / 2)], numeric(1))
  d[d$x %in% mids, , drop = FALSE]
}

# Is x time: a date axis, years (whole numbers 1900-2100), or period labels?
hover_time_axis <- function(built, layers) {
  sx <- built$layout$panel_scales_x[[1]]
  if (inherits(sx, c("ScaleContinuousDate", "ScaleContinuousDatetime"))) return("date")
  if (sx$is_discrete()) {
    lim <- as.character(sx$get_limits())
    return(if (length(lim) >= 3 && all(grepl(HOVER_PERIOD_RE, trimws(lim)))) "discrete" else NA)
  }
  x <- unlist(lapply(layers, function(d) d$x))
  x <- x[is.finite(x)]
  if (length(unique(x)) >= 3 && all(abs(x - round(x)) < 1e-6) && all(x >= 1900 & x <= 2100)) "year" else NA
}

# Value as the y axis shows it, with one more significant figure or so
hover_value <- function(built, v, whole) {
  sy <- built$layout$panel_scales_y[[1]]
  digits <- if (whole) 0 else max(1, min(3, 1 - floor(log10(max(abs(v), 1e-9)))))
  plain <- function(v) formatC(round(v, digits), format = "f", digits = digits, big.mark = ",")
  f <- sy$labels
  if (!is.function(f)) return(plain(v))
  if (inherits(f, "ggproto_method")) f <- environment(f)$f   # the labels function a scale was given
  env <- environment(f)
  if (!is.null(env) && exists("accuracy", envir = env, inherits = FALSE)) {
    sc <- if (exists("scale", envir = env, inherits = FALSE) && is.numeric(env$scale)) env$scale else 1
    acc_digits <- if (whole && sc == 1) 0 else max(1, min(3, 1 - floor(log10(max(abs(v * sc), 1e-9)))))
    env2 <- new.env(parent = env)          # finer accuracy; everything else as the axis has it
    env2$accuracy <- 10^-acc_digits
    environment(f) <- env2
    out <- tryCatch(f(v), error = function(e) NULL)
  } else {
    out <- tryCatch(f(round(v, digits)), error = function(e) NULL)
  }
  if (is.null(out) || !length(out) || is.na(out[1]) || !nzchar(out[1])) plain(v) else as.character(out[1])
}

# Series name for each row of a layer: the label of its colour or fill
hover_series <- function(built, d) {
  for (aes in c("colour", "fill")) {
    sc <- built$plot$scales$get_scales(aes)
    if (is.null(sc) || !aes %in% names(d) || !sc$is_discrete()) next
    lim <- sc$get_limits()
    pal <- sc$map(lim)
    lab <- sc$get_labels()
    if (is.null(lab) || length(lab) != length(lim)) lab <- lim
    hit <- match(toupper(d[[aes]]), toupper(pal))
    if (any(!is.na(hit))) return(ifelse(is.na(hit), NA_character_, as.character(lab[hit])))
  }
  rep(NA_character_, nrow(d))
}

time_series_spots <- function(built) {
  coord <- built$layout$coord
  if (!inherits(coord, "CoordCartesian") || inherits(coord, "CoordFlip")) return(NULL)
  if (built$layout$panel_scales_y[[1]]$is_discrete()) return(NULL)
  geoms <- vapply(built$plot$layers, function(l) class(l$geom)[1], "")
  keep <- which(geoms %in% c(HOVER_POINT_GEOMS, HOVER_BAR_GEOMS, HOVER_AREA_GEOMS))
  if (!length(keep)) return(NULL)
  layers <- lapply(keep, function(li) hover_rows(built$data[[li]], geoms[li]))
  names(layers) <- keep
  kind <- hover_time_axis(built, layers)
  if (is.na(kind)) return(NULL)

  sx <- built$layout$panel_scales_x[[1]]
  period <- function(x) {
    if (kind == "discrete") return(as.character(sx$get_limits())[round(x)])
    if (kind == "year") return(as.character(round(x)))
    d <- as.Date(if (inherits(sx, "ScaleContinuousDatetime")) as.POSIXct(x, origin = "1970-01-01") else x,
                 origin = "1970-01-01")
    all_d <- as.Date(unlist(lapply(layers, function(l) l$x)), origin = "1970-01-01")
    if (length(unique(format(all_d, "%d"))) > 1) format(d, "%d %b %Y")
    else if (length(unique(format(all_d, "%m"))) > 1) format(d, "%b %Y")
    else format(d, "%Y")
  }
  # Facet name for panels, when there are several
  lay <- built$layout$layout
  fcols <- setdiff(names(lay), c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y", "AXIS_X", "AXIS_Y", "COORD"))
  facet_of <- function(panel) {
    if (nrow(lay) < 2 || !length(fcols)) return(NA_character_)
    # Without any note in brackets: "Australia (financial years ...)" -> "Australia"
    sub(" \\(.*$", "", paste(vapply(fcols, function(f) as.character(lay[[f]][lay$PANEL == panel]), ""), collapse = ", "))
  }

  spots <- lapply(keep, function(li) {
    d <- layers[[as.character(li)]]
    if (!nrow(d) || !"x" %in% names(d)) return(NULL)
    d <- d[is.finite(d$x), , drop = FALSE]
    g <- geoms[li]
    if (g %in% HOVER_POINT_GEOMS) {
      d <- d[is.finite(d$y), , drop = FALSE]
      val <- d$y
      box <- data.frame(xmin = d$x, xmax = d$x, ymin = d$y, ymax = d$y)
    } else {
      lo <- if ("ymin" %in% names(d)) d$ymin else 0
      hi <- if ("ymax" %in% names(d)) d$ymax else d$y
      ok <- is.finite(lo) & is.finite(hi)
      d <- d[ok, , drop = FALSE]; lo <- lo[ok]; hi <- hi[ok]
      val <- ifelse(abs(lo) < 1e-12, hi, ifelse(abs(hi) < 1e-12, lo, hi - lo))
      if (g %in% HOVER_BAR_GEOMS) {
        xmin <- if ("xmin" %in% names(d)) d$xmin else d$x - 0.45
        xmax <- if ("xmax" %in% names(d)) d$xmax else d$x + 0.45
      } else {                             # area: a band half way to each neighbouring period
        xs <- sort(unique(d$x))
        step <- if (length(xs) > 1) min(diff(xs)) / 2 else 0.5
        xmin <- d$x - step; xmax <- d$x + step
      }
      box <- data.frame(xmin = xmin, xmax = xmax, ymin = pmin(lo, hi), ymax = pmax(lo, hi))
    }
    if (!nrow(d)) return(NULL)
    whole <- all(abs(val - round(val)) < 1e-9)
    series <- hover_series(built, d)
    facet <- vapply(as.character(d$PANEL), facet_of, "")
    name <- ifelse(is.na(series), facet,
                   ifelse(is.na(facet) | facet == series, series, paste0(series, " (", facet, ")")))
    when <- period(d$x)
    vals <- vapply(val, function(v) hover_value(built, v, whole), "")
    text <- ifelse(is.na(name), paste0("<b>", when, "</b><br>", vals),
                   paste0("<b>", htmltools::htmlEscape(name), "</b>, ", when, "<br>", vals))
    cbind(PANEL = d$PANEL, box, text = text, stringsAsFactors = FALSE)
  })
  do.call(rbind, spots)
}
