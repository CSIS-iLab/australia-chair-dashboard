# Renders the dashboard and copies the published files into docs/, the folder
# GitHub Pages serves. Run this instead of clicking Render in RStudio.
#
#   1. Set DASHBOARD_DIR (section 1) if the folder has moved.
#   2. Run the whole script (Ctrl+Shift+Enter, or Source).
#   3. Commit and push, including docs/.

# ---- 1. Folder path: the only line to change on a new computer ---------------
# Same as in '1. Refresh all data.R'.

DASHBOARD_DIR <- "C:/Users/Angelina/Documents/CSIS/Dashboard"

if (!dir.exists(DASHBOARD_DIR))
  stop("DASHBOARD_DIR doesn't exist: ", DASHBOARD_DIR,
       "\nEdit the path in section 1 of this script.")

# Stored as an environment variable so it survives scripts that begin with
# rm(list = ls()).
Sys.setenv(DASHBOARD_DIR = normalizePath(DASHBOARD_DIR, winslash = "/"))


# ---- 2. What to publish: the one list to edit ---------------------------------
# Each entry is a file or folder, relative to the dashboard folder. It is
# copied to the same place inside docs/ (a folder is copied with everything in
# it), overwriting the old copy. To publish more, add a line here, e.g.
#   "Trade/goods_charts",      # goods chart images
#   "editions",                # earlier quarterly editions

PUBLISH <- c(
  "2. Australia Chair Tool.html",   # the dashboard (rendered below)
  "index.html",                     # short-link page that opens the dashboard
  ".nojekyll"                       # tells GitHub Pages to serve files as they are
)

# Folders kept in step with docs/: every file in `from` whose name matches
# `pattern` is copied to the same folder inside docs/. Files in that docs/
# folder that match `prune` but no longer exist in `from` are deleted (so a
# chart dropped from Trade/ disappears from the site too). Nothing else in
# docs/ is ever deleted. Add a line per set of files, as below. (Old files are
# judged against everything in `from`, so two sets in one folder never delete
# each other's files.)
MIRROR <- list(
  list(from = "Trade", pattern = "^goods_.*\\.png$", prune = "\\.png$"),                     # goods charts (Trade in Goods page)
  list(from = "Trade", pattern = "^(credits|debits)_[0-9]{2}_.*\\.png$", prune = "\\.png$")  # services charts (Trade in Services page)
)

# Chart compression, done in place just before rendering, so both the HTML and
# docs/ get the smaller files. pngquant cuts each chart to a 256-colour palette
# (about 65% smaller, no visible change on these charts; a chart it can't
# shrink without visible loss is left as it is), then oxipng squeezes the file
# losslessly (a further few %). Only charts changed since the last run are
# done. If a tool can't be found, that step is skipped with a note and the
# dashboard is still published. Set COMPRESS to FALSE to skip both.
COMPRESS   <- TRUE
PNGQUANT   <- "C:/Program Files/pngquant/pngquant.exe"   # from pngquant.org
OXIPNG     <- "oxipng"     # from winget (install: winget install Shssoichiro.Oxipng); found on PATH
                           # or in winget's own folder, or give the full path to oxipng.exe here
CHART_DIRS <- c("Overview", "Economy", "Trade", "Data centres", "Energy", "Tech",
                "Migration", "Patents", "Investment", "Critical minerals")   # PNGs directly in these folders

QMD         <- "2. Australia Chair Tool.qmd"   # the file rendered
HTML        <- "2. Australia Chair Tool.html"  # what rendering makes
DOCS        <- "docs"                          # folder GitHub Pages serves
MAX_HTML_MB <- 95                              # GitHub refuses files over 100 MB


# ---- 3. Render, check size, copy (no need to edit below this line) ----------
setwd(Sys.getenv("DASHBOARD_DIR"))

# Quarto: the quarto R package if installed, otherwise the quarto command line
# (on PATH, or the copy that comes with RStudio). Nothing extra is needed when
# RStudio is installed.
find_quarto <- function() {
  candidates <- c(Sys.getenv("QUARTO_PATH"), Sys.which("quarto"),
                  "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe",
                  "C:/Program Files/Quarto/bin/quarto.exe",
                  file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Quarto/bin/quarto.exe"))
  hit <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(hit))
    stop("Can't find Quarto. Install RStudio (it includes Quarto) or Quarto from https://quarto.org,\n",
         "or set QUARTO_PATH to quarto.exe, then run this script again.")
  hit[1]
}

# ---- Compress charts -------------------------------------------------------
# A record of each chart as left after compression (path, size, time) is kept
# on this computer (not in the dashboard folder), so a chart is compressed
# again only once a chart script has rewritten it.
find_tool <- function(tool, winget_glob) {
  if (file.exists(tool)) return(normalizePath(tool, winslash = "/"))
  hit <- Sys.which(tool)
  if (nzchar(hit)) return(unname(hit))
  w <- Sys.glob(file.path(Sys.getenv("LOCALAPPDATA"), "Microsoft/WinGet/Packages", winget_glob))
  if (length(w)) return(w[1])
  ""
}
run_batches <- function(exe, args, files, size = 50) {   # Windows limits command-line length
  for (b in split(files, ceiling(seq_along(files) / size)))
    system2(exe, c(args, shQuote(b)), stdout = FALSE, stderr = FALSE)
}
comp <- list(files = 0L, before = 0, after = 0, notes = character())
if (isTRUE(COMPRESS)) {
  pq <- find_tool(PNGQUANT, "*/pngquant*/pngquant.exe")
  ox <- find_tool(OXIPNG, "Shssoichiro.Oxipng*/*/oxipng.exe")
  if (!nzchar(pq)) comp$notes <- c(comp$notes, paste0("pngquant not found at '", PNGQUANT, "': palette step skipped."))
  if (!nzchar(ox)) comp$notes <- c(comp$notes, "oxipng not found (winget install Shssoichiro.Oxipng): lossless step skipped.")
  if (nzchar(pq) || nzchar(ox)) {
    log_file <- file.path(tools::R_user_dir("csis-dashboard", "cache"), "compressed_charts.csv")
    dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
    done <- if (file.exists(log_file)) utils::read.csv(log_file, stringsAsFactors = FALSE) else
      data.frame(file = character(), size = numeric(), mtime = numeric())
    pngs <- unlist(lapply(CHART_DIRS[dir.exists(CHART_DIRS)], function(d)
      list.files(d, pattern = "\\.png$", full.names = TRUE)))
    info <- file.info(pngs)
    key  <- paste(pngs, info$size, round(as.numeric(info$mtime)))
    todo <- pngs[!key %in% paste(done$file, done$size, done$mtime)]
    if (length(todo)) {
      message("Compressing ", length(todo), " chart", if (length(todo) > 1) "s", " (only new or changed ones) ...")
      comp$files  <- length(todo)
      comp$before <- sum(file.size(todo))
      # pngquant: overwrite in place; keeps the original if the result would be larger or too lossy
      if (nzchar(pq)) run_batches(pq, c("--quality=70-95", "--speed", "1", "--strip", "--skip-if-larger",
                                        "--force", "--ext", ".png"), todo)
      if (nzchar(ox)) run_batches(ox, c("-q", "-o", "4", "--strip", "safe"), todo)
      comp$after <- sum(file.size(todo))
    }
    info <- file.info(pngs)
    utils::write.csv(data.frame(file = pngs, size = info$size, mtime = round(as.numeric(info$mtime))),
                     log_file, row.names = FALSE)
  }
}

message("Rendering ", QMD, " ...")
t0 <- Sys.time()
if (requireNamespace("quarto", quietly = TRUE)) {
  quarto::quarto_render(QMD)
} else {
  qbin <- find_quarto()
  status <- system2(qbin, c("render", shQuote(QMD)))
  if (!identical(status, 0L)) stop("Quarto render failed (exit code ", status, "). See the messages above.")
}
if (!file.exists(HTML) || file.mtime(HTML) < t0)
  stop("Rendering didn't produce a new '", HTML, "'. See the messages above.")

html_mb <- file.size(HTML) / 1024^2
if (html_mb > MAX_HTML_MB)
  stop(sprintf("'%s' is %.1f MB, over the %d MB limit (GitHub refuses files over 100 MB).\n", HTML, html_mb, MAX_HTML_MB),
       "Nothing was copied to ", DOCS, "/. Reduce the size (e.g. fewer or smaller embedded charts) and run again.")

missing <- c(PUBLISH[!file.exists(PUBLISH)],
             Filter(function(f) !dir.exists(f), vapply(MIRROR, `[[`, "", "from")))
if (length(missing))
  stop("Not found, so nothing was copied: ", paste0("'", missing, "'", collapse = ", "))
for (m in MIRROR) if (!length(list.files(m$from, pattern = m$pattern)))
  stop("No files in '", m$from, "' match ", m$pattern, ", so nothing was copied (have the chart scripts run?).")

dir.create(DOCS, showWarnings = FALSE)
copied <- data.frame(file = character(), size_mb = numeric())
for (p in PUBLISH) {
  if (dir.exists(p)) {                                   # a folder: copy every file in it
    files <- list.files(p, recursive = TRUE, all.files = TRUE, no.. = TRUE)
    src   <- file.path(p, files)
  } else src <- p
  for (s in src) {
    dest <- file.path(DOCS, s)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(s, dest, overwrite = TRUE, copy.date = TRUE)) stop("Couldn't copy '", s, "' to '", dest, "'.")
    copied[nrow(copied) + 1, ] <- list(s, file.size(dest) / 1024^2)
  }
}

# Mirrored folders: copy matching files, then delete stale ones in docs/
mirrored <- data.frame(folder = character(), files = integer(), size_mb = numeric(), deleted = integer())
for (m in MIRROR) {
  files <- list.files(m$from, pattern = m$pattern)
  dest_dir <- file.path(DOCS, m$from)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(file.path(m$from, files), file.path(dest_dir, files), overwrite = TRUE, copy.date = TRUE)
  if (!all(ok)) stop("Couldn't copy to '", dest_dir, "': ", paste(files[!ok], collapse = ", "))
  stale <- setdiff(list.files(dest_dir, pattern = m$prune), list.files(m$from, pattern = m$prune))
  if (length(stale)) file.remove(file.path(dest_dir, stale))
  mirrored[nrow(mirrored) + 1, ] <- list(paste0(m$from, "/ (", m$pattern, ")"), length(files),
                                         sum(file.size(file.path(dest_dir, files))) / 1024^2, length(stale))
}


# ---- 4. Summary ---------------------------------------------------------------
fmt_size <- function(mb) ifelse(mb >= 1, sprintf("%8.1f MB", mb), sprintf("%8.1f KB", mb * 1024))
if (comp$files) cat(sprintf("\nCompressed %d chart%s: %s -> %s (%.0f%% smaller)\n", comp$files,
                            if (comp$files > 1) "s" else "", trimws(fmt_size(comp$before / 1024^2)),
                            trimws(fmt_size(comp$after / 1024^2)), 100 * (1 - comp$after / comp$before)))
if (isTRUE(COMPRESS) && !comp$files && !length(comp$notes)) cat("\nCharts: all already compressed.\n")
if (length(comp$notes)) cat("\nNote: ", paste(comp$notes, collapse = "\nNote: "), "\n", sep = "")
cat("\nCopied to ", DOCS, "/:\n", sep = "")
cat(sprintf("  %-50s %s\n", copied$file, fmt_size(copied$size_mb)), sep = "")
if (nrow(mirrored))
  cat(sprintf("  %-50s %s  (%d files; %d old files deleted)\n", mirrored$folder, fmt_size(mirrored$size_mb),
              mirrored$files, mirrored$deleted), sep = "")
cat(sprintf("  %-50s %s  (%d files)\n", "Total", fmt_size(sum(copied$size_mb, mirrored$size_mb)),
            nrow(copied) + sum(mirrored$files)))
cat("\nNext: commit and push, including the docs/ folder.\n")
