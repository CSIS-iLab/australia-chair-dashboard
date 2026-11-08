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

# ---- House style --------------------------------------------------------------
# Every chart saved through ggsave() is finished in the dashboard's house style:
# Source Sans 3 throughout (text on the chart too), a navy bold title, grey
# subtitle and caption, and the caption left-aligned under the whole chart.
# Only type and colour are set here, so each script's own layout stays.
HOUSE_NAVY <- "#00205B"
HOUSE_GREY <- "#565A5C"
if (!"source_sans" %in% sysfonts::font_families())
  sysfonts::font_add_google("Source Sans 3", "source_sans")
for (g in c("text", "label")) ggplot2::update_geom_defaults(g, list(family = "source_sans"))
if (requireNamespace("ggrepel", quietly = TRUE))                 # repelled labels too
  for (g in list(ggrepel::GeomTextRepel, ggrepel::GeomLabelRepel))
    ggplot2::update_geom_defaults(g, list(family = "source_sans"))

# CSIS headline style (Chicago title case) for chart titles and legends:
# major words capitalised; articles, coordinating conjunctions and
# prepositions (of any length, as Chicago has it) lower case unless first or
# last, or after a colon. Acronyms and mixed-case words (U.S., GDP, NSW, kWh)
# and units (bn, tn) are left alone.
TITLE_SMALL <- c("a", "an", "the", "and", "but", "or", "nor", "for", "so", "yet",
                 "about", "above", "across", "after", "against", "along", "among", "around", "as", "at",
                 "before", "behind", "below", "beneath", "beside", "between", "beyond", "by", "despite",
                 "during", "except", "from", "in", "into", "of", "on", "onto", "over", "per", "since",
                 "through", "throughout", "to", "toward", "towards", "under", "until", "upon", "via", "vs",
                 "with", "within", "without")
TITLE_KEEP  <- c("bn", "tn", "m", "k", "km", "kt", "mt", "pj", "gwh", "twh", "mwh", "e.g.", "i.e.", "etc.", "n.e.c.", "n.i.e", "n.i.e.")
title_case <- function(x) {
  if (!is.character(x)) return(x)
  vapply(x, function(s) {
    if (is.na(s) || !nzchar(s)) return(s)
    lines <- strsplit(s, "\n", fixed = TRUE)[[1]]
    out <- vapply(lines, function(l) {
      w <- strsplit(l, " ", fixed = TRUE)[[1]]
      n <- length(w); real <- which(grepl("[A-Za-z0-9]", w))
      for (i in seq_len(n)) {
        wi <- w[i]
        core <- sub("^[^A-Za-z0-9]*", "", wi); core_l <- tolower(sub("[^A-Za-z.]+$", "", core))
        first <- length(real) && i == real[1]; last <- length(real) && i == real[length(real)]
        after_colon <- i > 1 && grepl("[:—]$", w[i - 1])
        minor <- !first && !last && !after_colon && core_l %in% TITLE_SMALL
        if (minor && grepl("^[A-Z][a-z]+[^A-Za-z]*$", core)) {  # "From", "Over" mid-title: lower case
          w[i] <- sub("([A-Z])", "\\L\\1", wi, perl = TRUE); next
        }
        if (grepl("^[A-Z]", core)) {                           # capitalised already: just the parts after a hyphen
          w[i] <- gsub("-([a-z])(?=[a-z]{2})", "-\\U\\1", wi, perl = TRUE); next
        }
        if (!grepl("^[a-z]", core)) next                       # a number or a symbol
        if (core_l %in% TITLE_KEEP) next
        if (minor) next
        # capitalise each part of a hyphenated word (small-scale -> Small-Scale)
        w[i] <- gsub("(^|[-/(\"'‘“])([a-z])", "\\1\\U\\2", wi, perl = TRUE)
      }
      paste(w, collapse = " ")
    }, character(1))
    paste(out, collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
}
# Applies title case to one ggplot's title, legend titles and legend entries
title_case_plot <- function(p) {
  if (!inherits(p, "ggplot")) return(p)
  labs_now <- tryCatch(ggplot2::get_labs(p), error = function(e) p$labels)
  leg_aes <- c("fill", "colour", "color", "linetype", "shape", "size", "alpha")
  new <- list()
  for (a in intersect(c("title", leg_aes), names(labs_now))) {
    v <- labs_now[[a]]
    if (is.character(v) && length(v) == 1) new[[a]] <- title_case(v)
  }
  if (length(new)) p <- p + do.call(ggplot2::labs, new)
  # legends drawn from ggplot's default scales: add that scale so its entries can be relabelled
  built <- tryCatch(ggplot2::ggplot_build(p), error = function(e) NULL)
  if (!is.null(built)) for (a in c("fill", "colour", "linetype", "shape")) {
    if (!is.null(p$scales$get_scales(a))) next
    sc <- built$plot$scales$get_scales(a)
    if (!is.null(sc) && inherits(sc, "ScaleDiscrete")) p <- p + sc$clone()
  }
  for (i in seq_along(p$scales$scales)) {
    sc <- p$scales$scales[[i]]
    if (!inherits(sc, "ScaleDiscrete") || !length(intersect(sc$aesthetics, leg_aes))) next
    sc <- sc$clone(); lab <- sc$labels
    sc$labels <- if (inherits(lab, "waiver")) function(x) title_case(as.character(x))
                 else if (is.function(lab)) { f <- lab; function(x) title_case(f(x)) }
                 else if (is.character(lab)) stats::setNames(title_case(lab), names(lab))
                 else lab
    if (is.character(sc$name) && length(sc$name) == 1) sc$name <- title_case(sc$name)
    p$scales$scales[[i]] <- sc
  }
  p
}

# Chart text 20% larger than each script sets it: the chart is drawn on a
# canvas 1/1.2 the size, at 1.2 times the dpi, so the PNG keeps its pixel size
# and only the text (and points and lines) grow. Applied to every chart saved
# through ggsave() below, and to its hover map in the same way.
TEXT_SCALE <- 1.2
scale_dims <- function(dots) {
  if (is.null(dots$dpi)) dots$dpi <- 300
  if (!is.null(dots$width))  dots$width  <- dots$width  / TEXT_SCALE
  if (!is.null(dots$height)) dots$height <- dots$height / TEXT_SCALE
  dots$dpi <- dots$dpi * TEXT_SCALE
  dots
}

# Long titles, subtitles and source lines are wrapped to the chart's width so
# that nothing runs off the edge (existing line breaks are kept)
wrap_text_to <- function(x, width_in, pt, char_em = 0.45) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) return(x)
  n <- max(30, floor(0.95 * width_in * 72 / (char_em * pt)))   # characters that fit on a line
  lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
  paste(unlist(lapply(lines, function(l) if (nchar(l) <= n) l else strwrap(l, width = n))), collapse = "\n")
}
wrap_labels <- function(p, width_in) {
  if (is.null(width_in) || !inherits(p, "ggplot")) return(p)
  th <- tryCatch(ggplot2:::plot_theme(p), error = function(e) ggplot2::theme_get())
  pt <- function(el, d) {
    v <- tryCatch(ggplot2::calc_element(el, th)$size, error = function(e) NULL)
    if (is.numeric(v) && length(v) == 1) v else d
  }
  labs_now <- tryCatch(ggplot2::get_labs(p), error = function(e) p$labels)
  new <- list()
  for (el in c("title", "subtitle", "caption")) {
    v <- labs_now[[el]]
    if (is.character(v) && length(v) == 1) {
      size <- c(title = HOUSE_SIZES[["title"]] * 1.15, subtitle = HOUSE_SIZES[["subtitle"]], caption = HOUSE_SIZES[["caption"]] * 1.05)[[el]]
      w <- wrap_text_to(v, width_in, size, char_em = if (el == "caption") CAPTION_EM else 0.45)
      if (!identical(w, v)) new[[el]] <- w
    }
  }
  if (length(new)) p <- p + do.call(ggplot2::labs, new)
  p
}

# ---- One look for every chart (dashboard consistency pass, Nov 2026) ----------
# Fixed text sizes (in points, before the 20% TEXT_SCALE): every chart's title,
# subtitle, axis, legend and footnote text is the same size whatever base size
# its script uses.
HOUSE_SIZES <- c(title = 15, subtitle = 11.5, axis = 10.5, legend = 10.5, strip = 11, caption = 9)
HOUSE_LINE  <- 1.0    # line width for data lines
HOUSE_POINT <- 1.6    # point size on lines (dot-plot points, 2.4 and up, are left alone)
HOUSE_GRID  <- "#E3E6EB"

# Footnote wording: "Source:" for one source, "Sources:" for several (sources
# separated by ";"), and every sentence ends with a full stop.
# Footnotes are kept compact: the sources form one paragraph and all notes run
# together as a second paragraph (forced line breaks in a chart script are
# dropped), and both are then wrapped to the chart's full width.
CAPTION_EM <- 0.41   # average character width of the footnote font, in ems (for wrapping)
tidy_caption <- function(x) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) return(x)
  lines <- trimws(strsplit(x, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return(x)
  # 1. Rejoin sentences split across lines (line ends in , ; : or the next starts in lower case)
  joined <- lines[1]
  for (l in lines[-1]) {
    k <- length(joined)
    if (grepl("[,;:(]$", joined[k]) || (grepl("^[a-z(]", l) && !grepl("[.!?]$", joined[k]))) joined[k] <- paste(joined[k], l) else joined <- c(joined, l)
  }
  # 2. Group: the source line plus any country-by-country source lines after it
  #    ("U.S.: EIA ..."), then every other note run together
  is_src  <- grepl("^Sources?:", joined)
  country <- grepl("^(Australia|U\\.S\\.|United States|US|OECD|World)[A-Za-z .]{0,25}:\\s", joined)
  grp <- integer(length(joined)); g <- 0L; prev_src <- FALSE
  for (i in seq_along(joined)) {
    if (is_src[i] || (country[i] && prev_src)) {
      if (!prev_src) g <- g + 1L
      prev_src <- TRUE
    } else {
      if (prev_src || i == 1) g <- g + 1L
      prev_src <- FALSE
    }
    grp[i] <- g
  }
  lines <- vapply(split(joined, grp), function(v) {
    v <- ifelse(grepl("[].!?)]$|[;,:]$", v), v, paste0(v, "."))     # end each piece cleanly before joining
    paste(v, collapse = " ")
  }, "", USE.NAMES = FALSE)
  lines <- vapply(lines, function(l) {
    l <- sub("\\s+$", "", l)
    if (!nzchar(l)) return(l)
    first <- sub("^(Sources?):\\s*", "", l)
    if (grepl("^Sources?:", l)) {
      several <- grepl(";", first) || grepl("^Sources:", l)   # never turn "Sources:" into "Source:"
      l <- paste0(if (several) "Sources: " else "Source: ", first)
    }
    if (!grepl("[].!?)]$", l) && !grepl("[;,:]$", l)) l <- paste0(l, ".")
    l
  }, "", USE.NAMES = FALSE)
  paste(lines, collapse = "\n")
}

# Lines and points the same weight in every chart: data lines at HOUSE_LINE and
# their points at HOUSE_POINT, where a script set a fixed size (sizes mapped to
# data, e.g. thin gray comparator lines, are left as they are)
house_marks <- function(p) {
  if (!inherits(p, "ggplot")) return(p)
  for (i in seq_along(p$layers)) {
    l <- p$layers[[i]]
    g <- class(l$geom)[1]
    if (g %in% c("GeomLine", "GeomPath", "GeomStep") && is.numeric(l$aes_params$linewidth) &&
        l$aes_params$linewidth >= 0.5 && l$aes_params$linewidth <= 1.3) {
      l$aes_params$linewidth <- HOUSE_LINE
    }
    if (g == "GeomPoint" && is.numeric(l$aes_params$size) && l$aes_params$size < 2.4) {
      l$aes_params$size <- HOUSE_POINT
    }
  }
  p
}

# Gridlines only along the value axis: vertical lines for horizontal bar and
# dot charts (categories down the side), horizontal lines for charts over time;
# never minor gridlines. Charts that are neither keep their own gridlines.
house_grid <- function(p) {
  if (!inherits(p, "ggplot")) return(NULL)
  b <- tryCatch(ggplot2::ggplot_build(p), error = function(e) NULL)
  if (is.null(b) || !inherits(b$layout$coord, "CoordCartesian") || inherits(b$layout$coord, "CoordFlip")) return(NULL)
  line <- ggplot2::element_line(colour = HOUSE_GRID, linewidth = 0.4)
  none <- ggplot2::element_blank()
  sy <- b$layout$panel_scales_y[[1]]; sx <- b$layout$panel_scales_x[[1]]
  if (sy$is_discrete() && !sx$is_discrete())
    return(ggplot2::theme(panel.grid.major.x = line, panel.grid.major.y = none, panel.grid.minor = none))
  layers <- lapply(b$data, function(d) d)
  if (!sy$is_discrete() && (sx$is_discrete() || !is.na(tryCatch(hover_time_axis(b, layers), error = function(e) NA))))
    return(ggplot2::theme(panel.grid.major.y = line, panel.grid.major.x = none, panel.grid.minor = none))
  ggplot2::theme(panel.grid.minor = none)
}

house_style <- function(p, width_in = NULL) {
  if (inherits(p, "patchwork") && !is.null(width_in)) {
    for (k in seq_along(p$patches$plots)) p$patches$plots[[k]] <- wrap_labels(p$patches$plots[[k]], width_in)
    for (el in c("subtitle", "caption"))
      if (is.character(p$patches$annotation[[el]]))
        p$patches$annotation[[el]] <- wrap_text_to(p$patches$annotation[[el]], width_in, c(subtitle = 11, caption = 9)[[el]],
                                                   char_em = if (el == "caption") CAPTION_EM else 0.45)
  }
  if (inherits(p, "patchwork")) {
    for (k in seq_along(p$patches$plots)) p$patches$plots[[k]] <- title_case_plot(p$patches$plots[[k]])
    if (is.character(p$patches$annotation$title)) p$patches$annotation$title <- title_case(p$patches$annotation$title)
  }
  one_plot <- function(q) {
    if (!inherits(q, "ggplot")) return(q)
    cap <- tryCatch(ggplot2::get_labs(q)$caption, error = function(e) NULL)
    if (is.character(cap) && length(cap) == 1) q <- q + ggplot2::labs(caption = tidy_caption(cap))
    q <- house_marks(q)
    g <- house_grid(q)
    if (!is.null(g)) q <- q + g
    # Legend at the top left, above the chart (charts that hide their legend,
    # or place it inside the panel, keep that)
    lp <- tryCatch(ggplot2:::plot_theme(q)$legend.position, error = function(e) "right")
    if (is.character(lp) && lp %in% c("right", "left", "top", "bottom"))
      q <- q + ggplot2::theme(legend.position = "top", legend.justification = "left", legend.location = "plot")
    q
  }
  if (inherits(p, "patchwork")) {
    for (k in seq_along(p$patches$plots)) p$patches$plots[[k]] <- one_plot(p$patches$plots[[k]])
    if (is.character(p$patches$annotation$caption))
      p$patches$annotation$caption <- tidy_caption(p$patches$annotation$caption)
  }
  p <- one_plot(p)
  p <- wrap_labels(title_case_plot(p), width_in)
  th <- ggplot2::theme(
    text          = ggplot2::element_text(family = "source_sans"),
    plot.title    = ggplot2::element_text(family = "source_sans", face = "bold", colour = HOUSE_NAVY,
                                          size = HOUSE_SIZES[["title"]]),
    plot.subtitle = ggplot2::element_text(family = "source_sans", colour = HOUSE_GREY,
                                          size = HOUSE_SIZES[["subtitle"]]),
    plot.caption  = ggplot2::element_text(family = "source_sans", colour = HOUSE_GREY, hjust = 0,
                                          size = HOUSE_SIZES[["caption"]], lineheight = 1.1),
    axis.text     = ggplot2::element_text(family = "source_sans", size = HOUSE_SIZES[["axis"]]),
    axis.title    = ggplot2::element_text(family = "source_sans", size = HOUSE_SIZES[["axis"]]),
    legend.text   = ggplot2::element_text(family = "source_sans", size = HOUSE_SIZES[["legend"]]),
    strip.text    = ggplot2::element_text(family = "source_sans", face = "bold", size = HOUSE_SIZES[["strip"]]),
    legend.title  = ggplot2::element_blank(),
    plot.title.position = "plot", plot.caption.position = "plot",
    # Transparent background, so charts sit on the dashboard's cards (and on
    # any page they are pasted into) without a white box
    plot.background   = ggplot2::element_rect(fill = "transparent", colour = NA),
    panel.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.box.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.key        = ggplot2::element_rect(fill = "transparent", colour = NA))
  if (inherits(p, "patchwork")) p & th else if (inherits(p, "ggplot")) p + th else p
}

# ---- Latest period named in a chart's title or subtitle -------------------------
# Written to <chart>_period.txt; the dashboard uses it for the "Data to" line
# only when the chart's CSV has no date column it recognises. Financial years
# ("2024-25") and years ("2025") count; years after this one (targets) do not.
chart_period <- function(p) {
  plots <- if (inherits(p, "patchwork")) hover_leaves(p) else list(p)
  txt <- unlist(lapply(plots, function(q) tryCatch(c(q$labels$title, q$labels$subtitle), error = function(e) NULL)))
  txt <- paste(txt[vapply(txt, is.character, logical(1))], collapse = " ")
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  fy <- regmatches(txt, gregexpr("\\b(19|20)\\d{2}[-–]\\d{2}\\b", txt))[[1]]
  fy_end <- as.integer(substr(fy, 1, 4)) + 1L
  yr <- as.integer(regmatches(txt, gregexpr("\\b(19|20)\\d{2}\\b", txt))[[1]])
  cand <- data.frame(end = c(fy_end, yr), label = c(fy, as.character(yr)), stringsAsFactors = FALSE)
  cand <- cand[cand$end <= this_year + (cand$label %in% fy), ]   # a financial year may end next year
  if (!nrow(cand)) return(NULL)
  cand <- cand[order(-cand$end, -nchar(cand$label)), ]          # prefer "2024-25" over "2025" at a tie
  cand$label[1]
}

ggsave <- function(filename, plot = ggplot2::last_plot(), ...) {
  dots <- list(...)
  showtext::showtext_auto(TRUE)
  dots_s <- scale_dims(dots)                                 # text 20% larger (see TEXT_SCALE)
  showtext::showtext_opts(dpi = dots_s$dpi)                  # must match the saved dpi
  plot <- house_style(plot, dots_s$width)
  dots_s$bg <- "transparent"                                 # whatever the script asked for
  res <- do.call(ggplot2::ggsave, c(list(filename, plot), dots_s))
  per <- tryCatch(chart_period(plot), error = function(e) NULL)
  if (!is.null(per) && grepl("\\.png$", filename) && !grepl("_usd\\.png$", filename))
    writeLines(per, sub("\\.png$", "_period.txt", filename))
  if (isTRUE(CHART_DATA_CSV))
    tryCatch(write_chart_data(plot, filename),
             error = function(e) message("Chart data not written for ", filename, ": ", conditionMessage(e)))
  tryCatch(write_hover(plot, filename, spots = chart_spots, ...),
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
  # Drawn exactly as the saved chart: same house style, same 20% larger text
  dots <- scale_dims(list(...))
  showtext::showtext_auto(TRUE); showtext::showtext_opts(dpi = dots$dpi)
  plot <- house_style(plot, dots$width)
  # Every plot is kept (spacers too) so that plot k matches marker colour k
  leaves <- hover_leaves(plot)
  builts <- lapply(leaves, function(p) tryCatch(ggplot2::ggplot_build(p), error = function(e) NULL))
  found <- lapply(builts, function(b) if (is.null(b)) NULL else tryCatch(spots(b), error = function(e) NULL))
  out_json <- sub("\\.png$", "_hover.json", file)
  if (all(vapply(found, function(s) is.null(s) || !nrow(s), logical(1)))) {
    unlink(out_json)                       # no longer a time series: drop an old map
    return(invisible(NULL))
  }
  boxes <- do.call(hover_boxes, c(list(plot, leaves), dots))
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

# Hover pop-ups for horizontal bar charts (categories down the y axis), used
# where a chart's printed value labels are replaced by tooltips. text_fun gets
# the category, the series (fill legend entry, or NA) and the bar's value, and
# returns the tooltip's HTML. Pass the result as write_hover(spots = ...).
hbar_spots <- function(text_fun) function(built) {
  sy <- built$layout$panel_scales_y[[1]]
  if (!sy$is_discrete()) return(NULL)
  cats <- as.character(sy$get_limits())
  fs <- built$plot$scales$get_scales("fill")
  fill_lab <- if (!is.null(fs)) {
    br <- fs$get_breaks(); setNames(as.character(fs$get_labels(br)), fs$map(br))
  } else character()
  geoms <- vapply(built$plot$layers, function(l) class(l$geom)[1], "")
  out <- lapply(which(geoms %in% c("GeomCol", "GeomBar", "GeomRect")), function(li) {
    d <- built$data[[li]]
    if (!all(c("xmin", "xmax", "ymin", "ymax") %in% names(d))) return(NULL)
    cat_i  <- cats[pmin(pmax(round((d$ymin + d$ymax) / 2), 1), length(cats))]
    series <- if ("fill" %in% names(d)) unname(fill_lab[d$fill]) else rep(NA_character_, nrow(d))
    value  <- ifelse(d$xmin == 0, d$xmax, d$xmax - d$xmin)
    data.frame(PANEL = d$PANEL, xmin = d$xmin, xmax = d$xmax, ymin = d$ymin, ymax = d$ymax,
               text = mapply(text_fun, cat_i, series, value, USE.NAMES = FALSE))
  })
  do.call(rbind, out)
}

# Hover pop-ups for charts with categories on one axis (bar charts, dot plots),
# used automatically for every chart that is not a time series. Heading: the
# category (and panel); then "Series: value", the value formatted as its axis.
cat_spots <- function(built) {
  coord <- built$layout$coord
  if (!inherits(coord, "CoordCartesian") || inherits(coord, "CoordFlip")) return(NULL)
  lay <- built$layout$layout
  sy1 <- built$layout$panel_scales_y[[1]]; sx1 <- built$layout$panel_scales_x[[1]]
  horiz <- sy1$is_discrete() && !sx1$is_discrete()
  vert  <- sx1$is_discrete() && !sy1$is_discrete()
  if (!horiz && !vert) return(NULL)
  geoms <- vapply(built$plot$layers, function(l) class(l$geom)[1], "")
  fcols <- setdiff(names(lay), c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y", "AXIS_X", "AXIS_Y", "COORD"))
  facet_of <- function(panel) {
    if (nrow(lay) < 2 || !length(fcols)) return(NA_character_)
    sub(" \\(.*$", "", paste(vapply(fcols, function(f) as.character(lay[[f]][lay$PANEL == panel]), ""), collapse = ", "))
  }
  fmt <- function(v, panel, whole) {
    out <- tryCatch(hover_value(built, v, whole, if (horiz) "x" else "y"), error = function(e) NULL)
    if (is.null(out) || length(out) != 1 || is.na(out) || !nzchar(out)) format(signif(v, 3), big.mark = ",") else out
  }
  cat_of <- function(pos, panel) {
    k <- if (horiz) lay$SCALE_Y[lay$PANEL == panel][1] else lay$SCALE_X[lay$PANEL == panel][1]
    sc <- if (horiz) built$layout$panel_scales_y[[k]] else built$layout$panel_scales_x[[k]]
    lim <- tryCatch(as.character(sc$get_labels()), error = function(e) NULL)   # as the axis shows them
    if (is.null(lim) || !length(lim)) lim <- as.character(sc$get_limits())
    lim[pmin(pmax(round(pos), 1), length(lim))]
  }
  out <- lapply(seq_along(geoms), function(li) {
    g <- geoms[li]; d <- built$data[[li]]
    if (!nrow(d)) return(NULL)
    if (g %in% c("GeomCol", "GeomBar", "GeomRect") && all(c("xmin", "xmax", "ymin", "ymax") %in% names(d))) {
      lo <- if (horiz) d$xmin else d$ymin; hi <- if (horiz) d$xmax else d$ymax
      val <- ifelse(abs(lo) < 1e-12, hi, ifelse(abs(hi) < 1e-12, lo, hi - lo))
      pos <- if (horiz) (d$ymin + d$ymax) / 2 else (d$xmin + d$xmax) / 2
      box <- data.frame(xmin = d$xmin, xmax = d$xmax, ymin = d$ymin, ymax = d$ymax)
    } else if (g == "GeomPoint" && all(c("x", "y") %in% names(d))) {
      val <- if (horiz) d$x else d$y
      pos <- if (horiz) d$y else d$x
      box <- data.frame(xmin = d$x, xmax = d$x, ymin = d$y, ymax = d$y)
    } else return(NULL)
    ok <- is.finite(val) & is.finite(pos)
    if (!any(ok)) return(NULL)
    d <- d[ok, , drop = FALSE]; val <- val[ok]; pos <- pos[ok]; box <- box[ok, , drop = FALSE]
    series <- hover_series(built, d)
    panel <- as.character(d$PANEL)
    head <- mapply(function(p, ps) { f <- facet_of(p); c0 <- cat_of(ps, p); if (is.na(f)) c0 else paste0(c0, " (", f, ")") },
                   panel, pos, USE.NAMES = FALSE)
    whole <- all(abs(val - round(val)) < 1e-9)
    vals <- mapply(fmt, val, panel, MoreArgs = list(whole = whole), USE.NAMES = FALSE)
    text <- ifelse(is.na(series), paste0("<b>", htmltools::htmlEscape(head), "</b><br>", vals),
                   paste0("<b>", htmltools::htmlEscape(head), "</b><br>", htmltools::htmlEscape(series), ": ", vals))
    cbind(PANEL = d$PANEL, box, text = text, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  if (is.null(out) || !nrow(out)) NULL else out
}
# The hover map written for every chart: over time, or by category
chart_spots <- function(built) {
  s <- tryCatch(time_series_spots(built), error = function(e) NULL)
  if (!is.null(s) && nrow(s)) return(s)
  cat_spots(built)
}

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
hover_value <- function(built, v, whole, axis = "y") {
  sy <- if (axis == "x") built$layout$panel_scales_x[[1]] else built$layout$panel_scales_y[[1]]
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
    lim <- sc$get_breaks()
    if (is.null(lim) || !length(lim) || all(is.na(lim))) lim <- sc$get_limits()
    lim <- lim[!is.na(lim)]
    pal <- sc$map(lim)
    lab <- tryCatch(sc$get_labels(lim), error = function(e) NULL)
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
    m <- as.integer(format(all_d, "%m"))
    steps <- diff(sort(unique(as.numeric(all_d))))
    quarterly <- length(unique(m %% 3)) == 1 && length(steps) && stats::median(steps) > 80 && stats::median(steps) < 100
    if (quarterly) paste0(format(d, "%Y"), " Q", (as.integer(format(d, "%m")) - 1) %/% 3 + 1)
    else if (length(unique(format(all_d, "%d"))) > 1) format(d, "%d %b %Y")
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
    # Heading: the period; then "Series: value" (the value as the axis shows it)
    text <- ifelse(is.na(name), paste0("<b>", when, "</b><br>", vals),
                   paste0("<b>", when, "</b><br>", htmltools::htmlEscape(name), ": ", vals))
    cbind(PANEL = d$PANEL, box, text = text, stringsAsFactors = FALSE)
  })
  do.call(rbind, spots)
}
