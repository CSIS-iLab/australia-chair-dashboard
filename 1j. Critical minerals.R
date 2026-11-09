rm(list = ls())
# The dashboard folder is set once, in section 1 of '1. Refresh all data.R'.
# To run this script on its own, run that section first.
if (Sys.getenv("DASHBOARD_DIR") == "")
  stop("Dashboard folder not set. Run section 1 of '1. Refresh all data.R' first.")
MINERALS_DIR <- file.path(Sys.getenv("DASHBOARD_DIR"), "Critical minerals")
dir.create(file.path(MINERALS_DIR, "raw"), recursive = TRUE, showWarnings = FALSE)
setwd(MINERALS_DIR)   # charts saved without a folder land in Critical minerals/

# ==============================================================================
# Australia's place in world critical minerals, seen from the US
#
# Two charts:
#   1. For each critical mineral Australia mines in quantity: Australia's
#      share of world production (and world rank), how much of its use the US
#      imports, and how much of those imports come from Australia.
#   2. Australia's share of world reserves against its share of production:
#      where it has room to supply more.
#
# Data: USGS Mineral Commodity Summaries (MCS) 2026 data release, ScienceBase
#   DOI 10.5066/P1WKQ63T, published February 2026. Values are for 2025 and are
#   USGS estimates. "Critical" means on the 2025 US critical minerals list.
#   - MCS2026_Commodities_Data.csv   : world production and reserves by
#     country, US net import reliance, US import sources 2021-24
#   - MCS2026_Fig3_Major_Import_Sources.csv : import shares for minerals
#     whose chapters do not list them by country (titanium)
#
# USGS publishes a new edition each February under new ScienceBase items:
# when it does, update MCS_YEAR and the two item IDs below (from the data
# release page, https://www.usgs.gov/data/mineral-commodity-summaries-2026-data-release
# with the year changed), and check the measure names in MINERALS still match.
# ==============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
source(file.path(Sys.getenv("DASHBOARD_DIR"), "0. Chart data.R"))   # <chart>_data.csv beside each chart
library(scales)
library(showtext)
library(sysfonts)

MCS_YEAR      <- 2026
DATA_YEAR     <- MCS_YEAR - 1
SB_COMMODITY  <- "69837e43b66b01367d7ec7c7"   # Commodity Salient U.S. and World Statistics
SB_TRENDS     <- "69837ec8b66b01367d7ec7d9"   # Mineral Industry Trends and Salient Statistics


# ------------------------------------------------------------------------------
# 1. Download (kept in Critical minerals/raw/ and reused after that)
# ------------------------------------------------------------------------------
# ScienceBase file links change with each upload, so each file is found by
# name in the item's file list.

BROWSER_UA <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
                    "(KHTML, like Gecko) Chrome/124.0 Safari/537.36")

fetch_sciencebase <- function(item_id, file_name) {
  dest <- file.path("raw", file_name)
  if (file.exists(dest) && file.size(dest) > 1000) return(dest)
  message("Downloading ", file_name)
  item <- httr2::request(paste0("https://www.sciencebase.gov/catalog/item/", item_id)) |>
    httr2::req_url_query(format = "json") |>
    httr2::req_user_agent(BROWSER_UA) |>
    httr2::req_retry(max_tries = 4) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  hit <- Filter(function(f) identical(f$name, file_name), item$files)
  if (length(hit) == 0) stop(file_name, " not found in ScienceBase item ", item_id)
  httr2::request(hit[[1]]$url) |>
    httr2::req_user_agent(BROWSER_UA) |>
    httr2::req_timeout(300) |>
    httr2::req_retry(max_tries = 4) |>
    httr2::req_perform(path = dest)
  dest
}

mcs <- read_csv(fetch_sciencebase(SB_COMMODITY, sprintf("MCS%d_Commodities_Data.csv", MCS_YEAR)),
                locale = locale(encoding = "latin1"), show_col_types = FALSE,
                col_types = cols(.default = col_character()))
fig3 <- read_csv(fetch_sciencebase(SB_TRENDS, sprintf("MCS%d_Fig3_Major_Import_Sources.csv", MCS_YEAR)),
                 show_col_types = FALSE, col_types = cols(.default = col_character()))


# ------------------------------------------------------------------------------
# 2. Minerals to show
# ------------------------------------------------------------------------------
# Critical minerals where Australia mines at least ~1% of world output, plus
# alumina (refined from Australian bauxite, a large export to the US).
# Columns:
#   commodity - USGS commodity name
#   measure   - the world production line, lower case, without ", rounded"
#   reserves  - the world reserves line (NA where USGS gives none)
#   nir       - pattern matching the US net import reliance line to use
#   imports   - pattern matching the US import-source line for Australia
#               (NA = Australia not among the listed sources)

MINERALS <- tribble(
  ~label,                 ~commodity,                       ~measure,                                     ~reserves,                     ~nir,                              ~imports,
  "Lithium",              "Lithium",                        "mine production",                            "reserves",                    "apparent consumption$",           NA,
  "Zirconium",            "Zirconium",                      "zirconium mineral concentrates, mine production", "zirconium reserves",          "Zirconium ores", "Zirconium ores and concentrates",
  "Titanium (rutile)",    "Titanium Mineral Concentrates",  "mine production: rutile",                    "reserves: rutile",            "apparent consumption$",           "fig3:Titanium",
  "Titanium (ilmenite)",  "Titanium Mineral Concentrates",  "mine production: ilmenite",                  "reserves: ilmenite",          "apparent consumption$",           "fig3:Titanium",
  "Bauxite",              "Bauxite",                        "bauxite, mine production",                   "bauxite reserves",            "apparent consumption$",           "^Bauxite$",
  "Alumina (refined)",    "Alumina",                        "alumina, refinery production",               NA,                            "apparent consumption$",           "^Alumina$",
  "Lead",                 "Lead",                           "mine production",                            "reserves",                    "refined metal",                   "Refined metal",
  "Zinc",                 "Zinc",                           "mine production",                            "reserves",                    "Refined zinc",                    NA,
  "Manganese",            "Manganese",                      "mine production",                            "reserves",                    "manganese content",               "Manganese contained",
  "Rare earths",          "Rare Earths",                    "mine production",                            "reserves",                    "Compounds and metals",            NA,
  "Tin",                  "Tin",                            "mine production",                            "reserves",                    "refined tin",                     NA,
  "Silver",               "Silver",                         "mine production",                            "reserves",                    "apparent consumption$",           NA,
  "Copper",               "Copper",                         "mine production",                            "reserves",                    "apparent consumption$",           NA,
  "Tantalum",             "Tantalum",                       "mine production",                            "reserves",                    "apparent consumption$",           "^Total$",
  "Cobalt",               "Cobalt",                         "mine production",                            "reserves",                    "apparent consumption$",           NA,
  "Antimony",             "Antimony",                       "mine production",                            "reserves",                    "apparent consumption$",           NA,
  "Nickel",               "Nickel",                         "mine production",                            "reserves",                    "apparent consumption$",           "Primary nickel",
  "Tungsten",             "Tungsten",                       "mine production",                            "reserves",                    "apparent consumption$",           NA
)


# ------------------------------------------------------------------------------
# 3. Shares, ranks, import reliance
# ------------------------------------------------------------------------------
# USGS codes: "W" withheld to protect company data, "—" (em dash) zero,
# "s" less than half the unit, ">"/"<" bounds, "E" net exporter.

num <- function(v) suppressWarnings(as.numeric(gsub("[,<>]", "", v)))
norm_measure <- function(x) x |> str_to_lower() |> str_remove("[:,] rounded$") |>
  str_remove(" ?\\(.*\\)$") |> str_squish()

world <- mcs |>
  filter(str_detect(Section, "^World"), Year == as.character(DATA_YEAR),
         Statistics %in% c("Production", "Reserves")) |>
  mutate(measure = norm_measure(Statistics_detail), val = num(Value))

# One row per mineral and line (production or reserves)
share_of_world <- function(line_col) {
  MINERALS |>
    select(label, commodity, measure = all_of(line_col)) |>
    filter(!is.na(measure)) |>
    pmap(function(label, commodity, measure) {
      rows <- world |> filter(Commodity == commodity, .data$measure == !!measure)
      tot  <- rows |> filter(Country == "World total")
      ctry <- rows |> filter(!Country %in% c("World total", "Other countries"))
      au   <- ctry |> filter(Country == "Australia")
      if (nrow(tot) != 1 || nrow(au) != 1)
        stop("Can't find '", measure, "' for ", commodity, " in the USGS world table")
      ranked <- ctry |> filter(!is.na(val), val > 0) |> arrange(desc(val))
      tibble(label,
             au_value    = au$val,
             world_value = tot$val,
             world_note  = tot$Notes %||% "",
             share       = au$val / tot$val,
             rank        = match("Australia", ranked$Country),
             withheld    = paste(ctry$Country[ctry$Value == "W"], collapse = ", "),
             world_lower_bound = str_detect(tot$Value, "^>"))
    }) |>
    list_rbind()
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a
pmap <- purrr::pmap; list_rbind <- purrr::list_rbind

prod <- share_of_world("measure")
resv <- share_of_world("reserves")

# US net import reliance, 2025
nir_rows <- mcs |>
  filter(Statistics == "Net import reliance", Year == as.character(DATA_YEAR))

nir <- MINERALS |>
  select(label, commodity, nir) |>
  pmap(function(label, commodity, nir) {
    r <- nir_rows |> filter(Commodity == commodity, str_detect(Statistics_detail, nir))
    if (nrow(r) != 1) stop("Net import reliance line not unique for ", label)
    tibble(label, nir_raw = r$Value,
           nir = if_else(r$Value == "E", 0, num(r$Value) / 100))
  }) |>
  list_rbind()

# Australia's share of US imports, 2021-24
imp_rows <- mcs |> filter(Statistics == "Import sources 2021-2024", Country == "Australia")

imports <- MINERALS |>
  select(label, commodity, imports) |>
  pmap(function(label, commodity, imports) {
    if (is.na(imports)) return(tibble(label, au_import_share = NA_real_))
    if (str_starts(imports, "fig3:")) {
      r <- fig3 |> filter(str_detect(Commodity, str_remove(imports, "^fig3:")),
                          Import_Source_2021_24 == "Australia")
      v <- num(r$Percent)
    } else {
      r <- imp_rows |> filter(Commodity == commodity, str_detect(Statistics_detail, imports))
      v <- num(r$Value)
    }
    if (length(v) != 1) stop("Import share for Australia not unique for ", label)
    tibble(label, au_import_share = v / 100)
  }) |>
  list_rbind()

minerals <- prod |>
  left_join(nir, by = "label") |>
  left_join(imports, by = "label") |>
  left_join(resv |> select(label, reserves_share = share, reserves_rank = rank,
                           reserves_lower_bound = world_lower_bound), by = "label") |>
  arrange(desc(share))

stopifnot(nrow(minerals) == nrow(MINERALS), all(minerals$share > 0 & minerals$share < 1))

# Footnote material for the captions
excl_us <- minerals$label[str_detect(minerals$world_note, "Excludes U\\.S\\.")]
w_other <- minerals |> filter(withheld != "", !label %in% excl_us) |> pull(label)


# ------------------------------------------------------------------------------
# 4. CSIS style (as in 1f. Trade.R)
# ------------------------------------------------------------------------------

font_add_google("Source Sans 3", "source_sans")
showtext_auto()
showtext_opts(dpi = 150)   # match ggsave() dpi or text renders too small

csis_colours <- c(
  navy   = "#00205B",
  red    = "#D2232A",
  teal   = "#0098A8",
  gold   = "#F2A900",
  grey   = "#565A5C",
  ltgrey = "#B1B3B3"
)

theme_csis <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "source_sans") %+replace%
    theme(
      plot.title    = element_text(face = "bold", size = rel(1.25), hjust = 0,
                                   colour = csis_colours[["navy"]],
                                   margin = margin(b = 4)),
      plot.subtitle = element_text(hjust = 0, colour = csis_colours[["grey"]],
                                   margin = margin(b = 10)),
      plot.caption  = element_text(size = rel(0.75), hjust = 0,
                                   colour = csis_colours[["grey"]],
                                   margin = margin(t = 10), lineheight = 1.1),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = "#E6E7E8", linewidth = 0.4),
      axis.line.x  = element_line(colour = csis_colours[["grey"]], linewidth = 0.4),
      axis.ticks.x = element_line(colour = csis_colours[["grey"]], linewidth = 0.4),
      axis.title   = element_text(colour = csis_colours[["grey"]], size = rel(0.85)),
      axis.text    = element_text(colour = csis_colours[["grey"]]),
      legend.position      = "top",
      legend.justification = "left",
      legend.title         = element_blank(),
      complete = TRUE
    )
}

save_csis <- function(plot, file, width = 8, height = 4.8) {
  ggsave(file, plot, width = width, height = height, dpi = 150, bg = "white")
}

MCS_SOURCE <- sprintf(paste0("Source: USGS, Mineral Commodity Summaries %d data release ",
                             "(doi:10.5066/P1WKQ63T). %d values are USGS estimates."),
                      MCS_YEAR, DATA_YEAR)


# ------------------------------------------------------------------------------
# 5. CHART 1 - Australia's share of world production next to US import needs
# ------------------------------------------------------------------------------

panels <- c(share           = "Australia's share of\nworld production and world ranking",
            nir             = "U.S. net import reliance\n(share of U.S. use imported)",
            au_import_share = "Australia's share of\nU.S. imports, 2021-24")

chart1 <- minerals |>
  select(label, rank, nir_raw, share, nir, au_import_share) |>
  pivot_longer(c(share, nir, au_import_share), names_to = "panel", values_to = "value") |>
  mutate(
    text = case_when(
      panel == "share"                 ~ sprintf("%s%%  #%d", label_number(0.1)(value * 100), rank),
      panel == "nir" & nir_raw == "E"  ~ "Net exporter",
      panel == "nir"                   ~ paste0(nir_raw, "%"),
      is.na(value)                     ~ "Not a major source",
      TRUE                             ~ paste0(round(value * 100), "%")),
    value = replace_na(value, 0),
    panel = factor(panel, levels = names(panels), labels = panels),
    label = factor(label, levels = rev(minerals$label))
  )

p_share <- ggplot(chart1, aes(value, label, fill = panel)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = text,
                colour = if_else(text %in% c("Not a major source", "Net exporter"), "muted", "dark")),
            hjust = -0.08, size = 3.3, family = "source_sans") +
  facet_wrap(~ panel, nrow = 1) +
  scale_fill_manual(values = setNames(unname(csis_colours[c("navy", "red", "teal")]), panels),
                    guide = "none") +
  scale_colour_manual(values = c(dark = csis_colours[["navy"]], muted = csis_colours[["ltgrey"]]),
                      guide = "none") +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.45),
                     breaks = seq(0, 1, 0.5), expand = expansion(c(0, 0))) +
  labs(
    title    = sprintf("Australia's share of world production and U.S. import reliance, critical minerals, %d", DATA_YEAR),
    x = NULL, y = NULL,
    caption = paste0(
      MCS_SOURCE,
      "\nAlumina is refinery output; all others are mine output. Titanium import share covers all ",
      "titanium mineral concentrates. Import reliance for lead, zinc and tin is for refined metal, ",
      "for rare earths\ncompounds and metals, and for zirconium ores and concentrates; '>' and '<' are ",
      "USGS bounds. 'Not a major source': USGS does not list Australia among the main U.S. suppliers.",
      if (length(excl_us) > 0)
        paste0("\nWorld totals for ", paste(str_to_lower(excl_us), collapse = " and "),
               " exclude U.S. output, which USGS withholds.") else "",
      if (length(w_other) > 0)
        paste0("\nSome countries' output is withheld for ", paste(str_to_lower(w_other), collapse = ", "),
               "; ranks count only published figures.") else ""
    )
  ) +
  theme_csis() +
  theme(strip.text  = element_text(face = "bold", colour = csis_colours[["navy"]],
                                   size = rel(0.9), hjust = 0, lineheight = 1),
        panel.grid.major.y = element_blank(),
        panel.spacing.x = unit(1.2, "lines"),
        axis.text.y = element_text(colour = csis_colours[["navy"]], size = rel(0.95)))

save_csis(p_share, "minerals_au_world_share_us_reliance.png", width = 11, height = 7)


# ------------------------------------------------------------------------------
# 6. CHART 2 - share of reserves vs share of production
# ------------------------------------------------------------------------------
# Where the reserves dot sits right of the production dot, Australia holds
# more of the world's known deposits than it currently mines: room to grow.
# USGS reserves for Australia are Geoscience Australia's economic
# demonstrated resources, which run larger than JORC-compliant reserves.

headroom <- minerals |>
  filter(!is.na(reserves_share)) |>
  transmute(label, Production = share, Reserves = reserves_share,
            gap = reserves_share - share) |>
  arrange(gap) |>
  mutate(label = factor(label, levels = label))

headroom_long <- headroom |>
  pivot_longer(c(Production, Reserves), names_to = "measure", values_to = "value")

lb <- minerals$label[minerals$reserves_lower_bound %in% TRUE]

p_headroom <- ggplot(headroom, aes(y = label)) +
  geom_segment(aes(x = Production, xend = Reserves, yend = label),
               colour = csis_colours[["ltgrey"]], linewidth = 1.6) +
  geom_point(data = headroom_long, aes(x = value, colour = measure), size = 3.2) +
  # values are in the hover pop-ups (printed labels sat on top of the dots)
  scale_colour_manual(values = c(Production = csis_colours[["navy"]],
                                 Reserves   = csis_colours[["teal"]]),
                      labels = c(Production = sprintf("Share of world production, %d", DATA_YEAR),
                                 Reserves   = "Share of world reserves")) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA),
                     expand = expansion(c(0, 0.05))) +
  labs(
    title    = "Australia's share of world reserves and world production, critical minerals",
    subtitle = "Australia's share of world reserves and of world mine production, sorted by the gap. Hover over a dot for its value.",
    x = NULL, y = NULL,
    caption = paste0(
      MCS_SOURCE,
      "\nAustralian reserves are Geoscience Australia's economic demonstrated resources, which are ",
      "larger than JORC-compliant reserves.",
      if (length(lb) > 0)
        paste0("\n", "USGS gives world reserves only as a lower bound for ",
               paste(str_to_lower(lb), collapse = ", "),
               ", so Australia's share of reserves there is an upper bound.") else ""   # wrapped when saved
    )
  ) +
  theme_csis() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#E6E7E8", linewidth = 0.4),
        axis.text.y = element_text(colour = csis_colours[["navy"]]))

save_csis(p_headroom, "minerals_au_reserves_vs_production.png", width = 9, height = 7)


# ------------------------------------------------------------------------------
# 7. Tidy exports
# ------------------------------------------------------------------------------

write_csv(minerals |>
            transmute(mineral = label,
                      au_share_world_production_pct = round(share * 100, 1),
                      au_world_rank = rank,
                      au_production = au_value, world_production = world_value,
                      us_net_import_reliance = nir_raw,
                      au_share_us_imports_2021_24_pct = round(au_import_share * 100),
                      au_share_world_reserves_pct = round(reserves_share * 100, 1),
                      au_reserves_rank = reserves_rank,
                      countries_withheld = withheld,
                      world_total_note = world_note),
          "minerals_au_us_tidy.csv")

# Takeaways under each chart
source(file.path(Sys.getenv("DASHBOARD_DIR"), "0. Takeaways.R"))
local({
  m <- read.csv("minerals_au_us_tidy.csv", stringsAsFactors = FALSE)
  first <- tolower(m$mineral[m$au_world_rank %in% 1])
  takeaway("Critical minerals/minerals_au_world_share_us_reliance.png", sprintf(
    "Australia is the world's largest producer of %s, and among the top three for %d of the %d minerals shown.",
    paste(if (length(first) > 2) c(paste(first[-length(first)], collapse = ", "), first[length(first)]) else first,
          collapse = " and "), sum(m$au_world_rank <= 3, na.rm = TRUE), nrow(m)))
  r <- m[!is.na(m$au_share_world_reserves_pct), ]
  r <- r[order(-(r$au_share_world_reserves_pct - r$au_share_world_production_pct)), ][1, ]
  takeaway("Critical minerals/minerals_au_reserves_vs_production.png", sprintf(
    "Australia holds %.0f%% of known world reserves of %s but produces %.0f%% of world output.",
    r$au_share_world_reserves_pct, tolower(r$mineral), r$au_share_world_production_pct))
})

# ---- The minerals table as a picture (minerals_au_us_table.png) ------------------
# The dashboard shows this table as a sortable HTML table (built in
# '2. Australia Chair Tool.qmd'). This draws the same table, sorted by
# Australia's share of world production, as a standalone page and photographs
# it with headless Chrome (2x resolution), for slides and reports. Skipped with
# a note if Chrome isn't found.
local({
  chrome <- c(Sys.getenv("CHROME_PATH"), "C:/Program Files/Google/Chrome/Application/chrome.exe",
              "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
              "C:/Program Files/Microsoft/Edge/Application/msedge.exe")
  chrome <- chrome[nzchar(chrome) & file.exists(chrome)]
  if (!length(chrome) || !requireNamespace("png", quietly = TRUE)) {
    message("Minerals table PNG skipped: Chrome (or the png package) not found.")
    return(invisible(NULL))
  }
  m <- read.csv("minerals_au_us_tidy.csv", stringsAsFactors = FALSE, check.names = FALSE)
  m <- m[order(-m$au_share_world_production_pct), ]
  esc <- function(x) htmltools::htmlEscape(as.character(x))
  nir <- suppressWarnings(as.numeric(gsub("[<>]", "", m$us_net_import_reliance)))
  bar <- function(v, txt, col) {
    if (is.na(v)) return(sprintf('<td class="na">%s</td>', txt))
    sprintf('<td><div class="cell"><span class="bar" style="width:%.1f%%;background:%s"></span><span>%s</span></div></td>',
            max(0.8, 0.7 * v), col, txt)
  }
  rows <- paste(vapply(seq_len(nrow(m)), function(i) paste0(
    '<tr><th>', esc(m$mineral[i]), '</th>',
    bar(m$au_share_world_production_pct[i], sprintf("%.1f%%", m$au_share_world_production_pct[i]), "#00205B"),
    '<td class="rank">#', m$au_world_rank[i], '</td>',
    bar(nir[i], paste0(esc(m$us_net_import_reliance[i]), "%"), "#D2232A"),
    bar(m$au_share_us_imports_2021_24_pct[i],
        if (is.na(m$au_share_us_imports_2021_24_pct[i])) "Not a major source" else sprintf("%.0f%%", m$au_share_us_imports_2021_24_pct[i]),
        "#0098A8"),
    '</tr>'), ""), collapse = "")
  page <- paste0(
    '<!doctype html><html><head><meta charset="utf-8">',
    '<link href="https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@400;600;700&display=swap" rel="stylesheet">',
    '<style>body{margin:0;background:#fff;font-family:"Source Sans 3","Source Sans Pro",Arial,sans-serif;color:#1F2A3A;}',
    '.wrap{width:1040px;padding:18px 22px 16px;box-sizing:border-box;}',
    'h1{font-size:24px;color:#00205B;margin:0 0 4px;font-weight:700;}',
    '.sub{font-size:17px;color:#5E6B78;margin:0 0 12px;}',
    'table{width:100%;border-collapse:collapse;font-size:15px;}',
    'thead th{text-align:left;color:#00205B;font-weight:700;border-bottom:2px solid #00205B;padding:6px 9px;vertical-align:bottom;line-height:1.25;}',
    'tbody th,td{padding:5px 9px;border-bottom:1px solid #E3E6EB;text-align:left;}',
    'tbody tr:nth-child(even){background:#F3F5F8;}',
    'tbody th{font-weight:600;white-space:nowrap;}td{width:27%;}td.rank,th.rank{width:7%;text-align:center;}',
    '.cell{display:flex;align-items:center;gap:6px;}.bar{display:inline-block;height:12px;border-radius:2px;}',
    '.na{color:#8A94A6;font-style:italic;}',
    '.note{font-size:13px;color:#5E6B78;margin:10px 0 0;line-height:1.4;}</style></head><body><div class="wrap">',
    '<h1>Australia&rsquo;s Share of World Production and U.S. Import Reliance</h1>',
    '<div class="sub">Critical minerals, 2025. Bars show each value out of 100%.</div>',
    '<table><thead><tr><th>Mineral</th><th>Australia&rsquo;s share of world production</th><th class="rank">World rank</th>',
    '<th>U.S. net import reliance (share of U.S. use imported)</th><th>Australia&rsquo;s share of U.S. imports, 2021&ndash;24</th></tr></thead>',
    '<tbody>', rows, '</tbody></table>',
    '<p class="note">Source: USGS, Mineral Commodity Summaries 2026 data release (doi:10.5066/P1WKQ63T). ',
    'Notes: 2025 values are USGS estimates. Alumina is refinery output; all others are mine output. ',
    'Titanium import share covers all titanium mineral concentrates. Import reliance for lead, zinc and tin is for refined metal, ',
    'for rare earths compounds and metals, and for zirconium ores and concentrates; &lsquo;&gt;&rsquo; and &lsquo;&lt;&rsquo; are USGS bounds. ',
    '&lsquo;Not a major source&rsquo;: USGS does not list Australia among the main U.S. suppliers. ',
    'World totals for lithium and bauxite exclude U.S. output, which USGS withholds. ',
    'Some countries&rsquo; output is withheld for antimony; ranks count only published figures.</p>',
    '</div></body></html>')
  html <- normalizePath(file.path(tempdir(), "minerals_au_us_table.html"), winslash = "/", mustWork = FALSE)
  writeLines(page, html, useBytes = TRUE)
  out <- normalizePath("minerals_au_us_table.png", winslash = "/", mustWork = FALSE)
  system2(chrome[1], c("--headless=new", "--disable-gpu", "--hide-scrollbars", "--force-device-scale-factor=2",
                       "--window-size=1040,1400", "--virtual-time-budget=4000",
                       paste0("--screenshot=", shQuote(out)), shQuote(paste0("file:///", html))),
          stdout = FALSE, stderr = FALSE)
  if (!file.exists(out)) { message("Minerals table PNG: Chrome did not write the file."); return(invisible(NULL)) }
  # Trim the empty space below the table
  im <- png::readPNG(out)
  ink <- which(apply(im[, , 1:3, drop = FALSE] < 0.98, 1, any))
  im <- im[1:min(dim(im)[1], max(ink) + 30), , , drop = FALSE]
  png::writePNG(im, out)
  message("Saved ", out)
})

message("Done - two charts, the table picture and one tidy CSV written to ", getwd())
