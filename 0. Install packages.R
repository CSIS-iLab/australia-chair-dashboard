# Installs every R package the dashboard scripts use. Run once on a new
# computer (or after updating R), then again any time a script says a
# package is missing. It's safe to re-run: only missing packages are installed.
#
# It finds packages by scanning the numbered scripts and the dashboard .qmd for
# library(), require(), requireNamespace() and pkg:: calls, so new packages are
# picked up automatically.

dir <- Filter(dir.exists, c(
  "C:/Users/Angelina/Documents/CSIS/Dashboard",   # Windows PC
  "/Users/angelina/Documents/CSIS/Dashboard"      # MacBook Air
))[1]
if (is.na(dir)) stop("Can't find the Dashboard folder; edit the paths above.")

files <- c(list.files(dir, pattern = "^[0-9].*\\.R$", full.names = TRUE),
           file.path(dir, "2. Australia Chair Tool.qmd"))
code  <- unlist(lapply(files, readLines, warn = FALSE))
code  <- sub("#.*$", "", code)                       # ignore comments

pat1 <- "(library|require|requireNamespace)\\(\\s*[\"']?([A-Za-z][A-Za-z0-9.]*)"
pat2 <- "([A-Za-z][A-Za-z0-9.]*):::?"
pkgs <- c(sub(pat1, "\\2", regmatches(code, gregexpr(pat1, code)) |> unlist()),
          sub(pat2, "\\1", regmatches(code, gregexpr(pat2, code)) |> unlist()))

base <- rownames(installed.packages(priority = "base"))
pkgs <- setdiff(unique(c(pkgs, "quarto", "rnaturalearthdata")), base)
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing)) {
  message("Installing ", length(missing), " package(s): ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
} else message("All ", length(pkgs), " packages are already installed.")

still <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
if (length(still)) warning("Couldn't install: ", paste(still, collapse = ", "),
                           ". sf may need GDAL first on a Mac: brew install gdal")

# Other tools (not R packages):
#  - Quarto (renders the dashboard): comes with RStudio, or https://quarto.org
#  - pngquant and oxipng (optional chart compression):
#      Mac:     brew install pngquant oxipng
#      Windows: pngquant.org, and winget install Shssoichiro.Oxipng
message("Done.")
