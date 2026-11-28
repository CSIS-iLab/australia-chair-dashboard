rm(list = ls())
# The dashboard folder is set once, in section 1 of '1. Refresh all data.R'.
# To run this script on its own, run that section first.
if (Sys.getenv("DASHBOARD_DIR") == "")
  stop("Dashboard folder not set. Run section 1 of '1. Refresh all data.R' first.")
setwd(file.path(Sys.getenv("DASHBOARD_DIR"), "Energy"))
source(file.path(Sys.getenv("DASHBOARD_DIR"), "0. Exchange rates.R"))   # A$/US$ toggle
source(file.path(Sys.getenv("DASHBOARD_DIR"), "0. Takeaways.R"))       # line under each chart

# ==============================================================================
# Australian energy: five charts for a US policy audience
#
# Data: DCCEEW (2026), Australian Energy Statistics, Table O
#   Electricity generation by fuel type, 2024-25 and calendar 2025
#   https://www.energy.gov.au/energy-data/australian-energy-statistics
#
# Workbook structure (verified against the file):
#   - Sheets "AUS FY", "NSW FY", ... "NT FY"   : 1989-90 to 2024-25
#   - Sheets "AUS CY", "NSW CY", ... "NT CY"   : 2015 to 2025 (est.)
#   - Sheet  "State summary 2025"              : fuels x states, calendar 2025
#   - Header is row 5, so skip = 4. Units are GWh throughout.
#   - Fuel names differ between FY and CY sheets: FY splits bioenergy into
#     "Bagasse, wood" + "Biogas" and carries an "Other [note a]" row; CY
#     collapses these to "Biomass". Handled in recode_fuel() below.
# ==============================================================================

library(data.table)   # for export_chart_csv(); loaded first so dplyr keeps first/last/between
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
source(file.path(Sys.getenv("DASHBOARD_DIR"), "0. Chart data.R"))   # <chart>_data.csv beside each chart
library(scales)
library(forcats)
library(readr)

# Charts here use default fonts. If an earlier script in this R session turned
# on showtext (set for a different dpi), switch it off or text renders at the
# wrong size and titles get cut off.
if ("showtext" %in% loadedNamespaces()) showtext::showtext_auto(FALSE)

# Table O is downloaded the first time and reused after that. DCCEEW publishes
# a new edition each June at a new address: when it does, paste the new .xlsx
# link (from the Table O page linked above) into TABLE_O_URL, and update the
# years in the chart titles and the validation figures below.
TABLE_O_URL <- paste0("https://www.energy.gov.au/sites/default/files/2026-06/",
                      "table-o-electricity-generation-by-fuel%20type-2024-25-and-2025.xlsx")
path <- utils::URLdecode(basename(TABLE_O_URL))

if (!file.exists(path)) {
  message("Downloading ", path)
  try(httr2::request(TABLE_O_URL) |>
        httr2::req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)") |>   # site blocks requests without one
        httr2::req_timeout(120) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_perform(path = path))
}
if (!file.exists(path))
  stop("Table O is missing. energy.gov.au usually blocks scripted downloads: open\n  ", TABLE_O_URL,
       "\nin your web browser and save the file into the Energy folder as\n  ", path,
       "\n(see ReadME.txt).", call. = FALSE)

# Table C (energy consumption by state and fuel, financial years) for the
# energy mix charts. Saved by hand from energy.gov.au each September (see
# ReadME.txt); put the new edition's file name here.
TABLE_C_URL  <- "https://www.energy.gov.au/sites/default/files/Australian%20Energy%20Statistics%202026%20Table%20C.xlsx"
TABLE_C_FILE <- "Australian Energy Statistics 2026 Table C.xlsx"
if (!file.exists(TABLE_C_FILE))
  stop("Table C is missing. energy.gov.au blocks scripted downloads: open\n  ", TABLE_C_URL,
       "\nin your web browser and save the file into the Energy folder as\n  ", TABLE_C_FILE,
       "\n(see ReadME.txt).", call. = FALSE)

# Table J (energy supply and trade by fuel, financial years) for the energy
# security chart. Saved by hand each September, like Table C.
TABLE_J_URL  <- "https://www.energy.gov.au/sites/default/files/Australian%20Energy%20Statistics%202026%20Table%20J.xlsx"
TABLE_J_FILE <- "Australian Energy Statistics 2026 Table J.xlsx"
if (!file.exists(TABLE_J_FILE))
  stop("Table J is missing. energy.gov.au blocks scripted downloads: open\n  ", TABLE_J_URL,
       "\nin your web browser and save the file into the Energy folder as\n  ", TABLE_J_FILE,
       "\n(see ReadME.txt).", call. = FALSE)


# ------------------------------------------------------------------------------
# 0. Chart CSV export helper
#
# Writes a metadata block (title, labels, styling) followed by the chart data.
#   extra     - grouping columns to keep (series / facet variables)
#   value_var - the numeric column to format; defaults to the y variable
#               (horizontal bar charts carry their value on x)
# ------------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

export_chart_csv <- function(p, file,
                             y_format = c("number", "percent"),
                             chart_type = "Line with point markers",
                             notes = NULL,
                             extra = NULL,
                             value_var = NULL) {
  y_format <- match.arg(y_format)
  
  xvar <- rlang::as_label(p$mapping$x)
  yvar <- rlang::as_label(p$mapping$y)
  value_var <- value_var %||% yvar
  
  dat <- as.data.table(p$data)[, c(extra, xvar, yvar), with = FALSE]
  setorderv(dat, c(extra, xvar))
  
  if (y_format == "percent") {
    dat[[value_var]] <- round(dat[[value_var]] * 100, 1)
    y_unit <- "Percent (values are 0-100); axis labels shown as whole-number %"
  } else {
    y_unit <- "Count; axis labels shown with thousands separator"
  }
  
  # Pull styling/scale info from the plot itself
  lyr_params <- p$layers[[1]]$aes_params
  y_scale    <- p$scales$get_scales("y")
  y_lims     <- if (!is.null(y_scale)) y_scale$limits else NULL
  fmt_lim    <- function(v) if (is.null(v) || is.na(v)) "auto" else as.character(v)
  
  meta <- data.table(
    field = c("title", "subtitle", "caption",
              "chart_type", "x_variable", "x_axis_label",
              "y_variable", "y_axis_label", "y_units_format",
              "y_axis_min", "y_axis_max",
              "line_colour", "line_width_mm", "point_size",
              "title_style", "notes"),
    value = c(p$labels$title    %||% "",
              p$labels$subtitle %||% "",
              p$labels$caption  %||% "",
              chart_type,
              xvar, p$labels$x %||% "(none)",
              yvar, p$labels$y %||% "(none)",
              y_unit,
              fmt_lim(y_lims[1]), fmt_lim(y_lims[2]),
              lyr_params$colour    %||% "",
              lyr_params$linewidth %||% "",
              as.character(if (length(p$layers) >= 2) p$layers[[2]]$aes_params$size %||% "" else ""),
              "Bold",
              notes %||% "")
  )
  
  fwrite(meta, file, col.names = FALSE, bom = TRUE)   # BOM so Excel reads UTF-8 cleanly
  cat("\n", file = file, append = TRUE)
  fwrite(dat, file, append = TRUE, col.names = TRUE)
  invisible(file)
}


# ------------------------------------------------------------------------------
# 1. Parsing
# ------------------------------------------------------------------------------

STATES <- c("AUS", "NSW", "VIC", "QLD", "WA", "SA", "TAS", "NT")

DROP_ROWS <- c("Total non-renewable", "Total renewable", "Total",
               "Per cent renewable generation")

RENEWABLES <- c("Hydro", "Wind", "Rooftop solar", "Utility-scale solar", "Bioenergy")

recode_fuel <- function(x) {
  x <- str_trim(str_remove(x, "\\s*\\[note [a-z]\\]$"))
  recode(x,
         "Small-scale solar PV" = "Rooftop solar",
         "Large-scale solar PV" = "Utility-scale solar",
         "Natural gas"          = "Gas",
         "Oil products"         = "Oil",
         "Bagasse, wood"        = "Bioenergy",
         "Biogas"               = "Bioenergy",
         "Biomass"              = "Bioenergy",
         .default = x)
}

# Reads one region sheet. `basis` is "FY" or "CY".
read_region <- function(region, basis) {
  read_excel(path, sheet = paste(region, basis), skip = 4) |>
    rename(fuel = 1) |>
    filter(!is.na(fuel), !str_starts(fuel, "\\["), !fuel %in% DROP_ROWS) |>
    pivot_longer(-fuel, names_to = "period", values_to = "gwh") |>
    mutate(
      region = region,
      basis  = basis,
      fuel   = recode_fuel(fuel),
      gwh    = suppressWarnings(as.numeric(gwh)),
      year   = as.integer(str_extract(period, "^\\d{4}"))
    ) |>
    filter(!is.na(gwh)) |>
    # FY splits bioenergy across two rows; collapse so FY and CY are comparable.
    group_by(region, basis, period, year, fuel) |>
    summarise(gwh = sum(gwh), .groups = "drop")
}

gen_fy <- bind_rows(lapply(STATES, read_region, basis = "FY"))
gen_cy <- bind_rows(lapply(STATES, read_region, basis = "CY"))

state_2025 <- read_excel(path, sheet = "State summary 2025", skip = 4) |>
  rename(fuel = 1) |>
  filter(!is.na(fuel), !str_starts(fuel, "\\["), !fuel %in% DROP_ROWS) |>
  pivot_longer(-fuel, names_to = "region", values_to = "gwh") |>
  mutate(fuel = recode_fuel(fuel), gwh = suppressWarnings(as.numeric(gwh))) |>
  replace_na(list(gwh = 0)) |>
  group_by(region, fuel) |>
  summarise(gwh = sum(gwh), .groups = "drop")

# --- Validation gate: published 2025 figures are 286.8 TWh and 39.5% renewable
check <- gen_cy |>
  filter(region == "AUS", year == 2025) |>
  summarise(twh = sum(gwh) / 1000,
            renew_pct = 100 * sum(gwh[fuel %in% RENEWABLES]) / sum(gwh))
print(check)
stopifnot(abs(check$twh - 286.8) < 0.5, abs(check$renew_pct - 39.5) < 0.5)


# ------------------------------------------------------------------------------
# 2. Shared aesthetics
# ------------------------------------------------------------------------------

fuel_levels <- c("Black coal", "Brown coal", "Other", "Gas", "Oil",
                 "Bioenergy", "Hydro", "Wind", "Utility-scale solar", "Rooftop solar",
                 "Geothermal")

# Related sources in related hues, each still easy to tell apart: fossil fuels
# in charcoal and browns, solar in golds, wind and hydro in blues, bioenergy
# green, nuclear and geothermal in purples, other gray.
fuel_cols <- c(
  "Black coal"    = "#2E2E2E", "Brown coal"    = "#6E6A64", "Other" = "#B4B2A9",
  "Gas"           = "#C2703D", "Oil"           = "#6B3E26",
  "Bioenergy"     = "#4E8B2E", "Hydro"         = "#1F5FA8", "Wind" = "#5BB4E0",
  "Utility-scale solar" = "#F2A900", "Rooftop solar" = "#FFD966",
  "Geothermal"    = "#C48BC0"
)

theme_csis <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title            = element_text(face = "bold", size = 13),
      plot.subtitle         = element_text(colour = "grey35", size = 10),
      plot.caption          = element_text(colour = "grey45", size = 8, hjust = 0),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      panel.grid.minor      = element_blank(),
      panel.grid.major.x    = element_blank(),
      legend.position       = "bottom",
      legend.title          = element_blank(),
      legend.key.size       = unit(0.4, "cm")
    )
}

SRC <- "Source: DCCEEW, Australian Energy Statistics, Table O."

as_fuel <- function(x) factor(x, levels = fuel_levels)

# Colour list for the CSV notes, e.g. "Black coal #2c2c2a; Wind #1baf7a"
fuel_col_note <- function(fuels) {
  fuels <- intersect(fuel_levels, fuels)
  paste(sprintf("%s %s", fuels, fuel_cols[fuels]), collapse = "; ")
}

# ------------------------------------------------------------------------------
# 1b. US electricity generation (EIA), for the Australia vs US charts
# ------------------------------------------------------------------------------
# National, calendar years: EIA Monthly Energy Review Table 7.2a (net
# generation by source, all sectors) and Table 10.6 (solar, which splits
# utility-scale and small-scale solar). Small-scale (rooftop) solar is
# included so the US matches Australia's Table O, which includes rooftop PV.
# EIA estimates small-scale solar from 2014; earlier years are utility only.
# By state, calendar years: EIA Electric Power Annual state file (net
# generation by state and source). It has no small-scale solar.
# All free downloads, no API key. Kept in Energy/raw/eia/, refreshed weekly.

EIA_DIR <- file.path("raw", "eia")
dir.create(EIA_DIR, recursive = TRUE, showWarnings = FALSE)
eia_get <- function(url, file) {
  dest <- file.path(EIA_DIR, file)
  if (!file.exists(dest) || difftime(Sys.time(), file.mtime(dest), units = "days") > 7)
    utils::download.file(url, dest, mode = "wb", quiet = TRUE, headers = c(`User-Agent` = "Mozilla/5.0"))
  dest
}
mer_annual <- function(table) {
  f <- eia_get(paste0("https://www.eia.gov/totalenergy/data/browser/csv.php?tbl=", table),
               paste0("mer_", table, ".csv"))
  read_csv(f, show_col_types = FALSE, col_types = cols(.default = "c")) |>
    filter(substr(YYYYMM, 5, 6) == "13") |>                  # annual totals
    transmute(msn = MSN, year = as.integer(substr(YYYYMM, 1, 4)),
              gwh = suppressWarnings(as.numeric(Value)))     # million kWh = GWh
}

us_raw <- bind_rows(mer_annual("T07.02A"),
                    mer_annual("T10.06") |> filter(msn %in% c("SOT5PUS", "SOT7PUS"))) |>
  filter(year >= 1990) |>
  tidyr::pivot_wider(names_from = msn, values_from = gwh, values_fill = 0)

US_MAP <- c(CLETPUS = "Coal", NGETPUS = "Gas", PAETPUS = "Oil", OJETPUS = "Other",
            NUETPUS = "Nuclear", HVETPUS = "Hydro", WYETPUS = "Wind",
            WDETPUS = "Bioenergy", WSETPUS = "Bioenergy", GEETPUS = "Geothermal",
            SOT5PUS = "Utility-scale solar", SOT7PUS = "Rooftop solar")
us_gen <- us_raw |>
  mutate(other_rest = ELETPUS - (CLETPUS + NGETPUS + PAETPUS + OJETPUS + NUETPUS + HVETPUS +
                                   HPETPUS + WYETPUS + WDETPUS + WSETPUS + GEETPUS + SOETPUS)) |>
  select(year, all_of(names(US_MAP)), other_rest) |>
  pivot_longer(-year, names_to = "msn", values_to = "gwh") |>
  mutate(fuel = if_else(msn == "other_rest", "Other", US_MAP[msn])) |>
  group_by(year, fuel) |> summarise(gwh = sum(pmax(gwh, 0)), .groups = "drop") |>
  mutate(country = "United States")
us_last <- max(us_gen$year)
stopifnot(n_distinct(us_gen$fuel) >= 10)

# Both countries in one set of fuel groups: Australia's black and brown coal
# become Coal; the US adds Nuclear.
cmp_levels <- c("Coal", "Other", "Gas", "Oil", "Nuclear", "Bioenergy", "Geothermal",
                "Hydro", "Wind", "Utility-scale solar", "Rooftop solar")
cmp_cols <- c(fuel_cols[c("Other", "Gas", "Oil", "Bioenergy", "Geothermal", "Hydro", "Wind",
                          "Utility-scale solar", "Rooftop solar")],
              Coal = "#2E2E2E", Nuclear = "#6A3D9A")
cmp_lab  <- c("Rooftop solar" = "Rooftop / small-scale solar", "Gas" = "Natural gas")
CMP_RENEW <- c("Hydro", "Wind", "Utility-scale solar", "Rooftop solar", "Bioenergy", "Geothermal")
to_cmp <- function(f) if_else(f %in% c("Black coal", "Brown coal"), "Coal", f)

# Australian financial years are labelled by the year they end, across the
# dashboard (2024-25 is 2025); Table O labels them by the year they start
au_fy_cmp <- gen_fy |> filter(region == "AUS") |>
  mutate(fuel = to_cmp(fuel)) |> group_by(year, fuel) |>
  summarise(gwh = sum(gwh), .groups = "drop") |> mutate(country = "Australia", year = year + 1L)
mix_ts <- bind_rows(au_fy_cmp, us_gen) |>
  tidyr::complete(tidyr::nesting(country, year), fuel, fill = list(gwh = 0)) |>   # no gaps in the areas
  group_by(country, year) |> mutate(share = gwh / sum(gwh)) |> ungroup() |>
  mutate(fuel = factor(fuel, levels = cmp_levels))

US_SRC <- paste0("EIA, Monthly Energy Review, Tables 7.2a and 10.6 (calendar years; small-scale solar ",
                 "estimated from 2014, so earlier U.S. solar is utility-scale only).")


# ------------------------------------------------------------------------------
# 3. CHART 1 - generation mix over time, Australia and the US
#
# Shares of total generation, so the two very different-sized grids can be
# compared. Coal peaked around 2008-09 in Australia; in the US gas replaced
# coal while nuclear held steady.
# ------------------------------------------------------------------------------

p1 <- mix_ts |>
  ggplot(aes(year, share, fill = fuel)) +
  geom_area(colour = "white", linewidth = 0.15) +
  facet_wrap(~country, ncol = 2) +
  scale_fill_manual(values = cmp_cols, labels = function(x) ifelse(x %in% names(cmp_lab), cmp_lab[x], x),
                    drop = TRUE) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(c(0, 0))) +
  scale_x_continuous(breaks = seq(1990, 2025, 10), expand = expansion(c(0, 0.02))) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(
    title    = "Electricity generation by source over time, Australia and the United States",
    subtitle = sprintf("Share of total generation, 1990–%d", us_last),
    x = NULL, y = NULL,
    caption  = paste0("Sources: DCCEEW, Australian Energy Statistics, Table O (includes rooftop solar); ",
                      US_SRC, "\nAustralian years are financial years (to June), labeled by the year they end, so the latest Australian year differs slightly from the calendar-year Source tab. Australia's black and brown coal are combined. U.S. pumped storage is left out.")
  ) +
  theme_csis() +
  theme(strip.text = element_text(face = "bold", hjust = 0.5), panel.spacing.x = unit(1.5, "lines"))

ggsave("01_fuel_mix_timeseries.png", p1, width = 10, height = 5.5, dpi = 150, bg = "white")

export_chart_csv(p1, "01_fuel_mix_timeseries.csv",
                 y_format = "percent", chart_type = "Stacked area (100%), two panels",
                 extra = c("country", "fuel"),
                 notes = paste0("Shares of total generation. Australia year = end year of the financial year ",
                                "(1989 = 1989-90); U.S. calendar years."))



# ------------------------------------------------------------------------------
# 4. CHART 2 - eight grids, not one
#
# So what: "the Australian grid" does not exist. WA is a separate market
# entirely. Any read on AUKUS basing at Henderson, critical-minerals
# processing, or data-centre siting has to be state-level. Bars are sorted by
# renewable share so the ranking is legible, which a choropleth cannot do.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 5b. Generation mix by state: Australian states and the main US data centre
#     states on one chart
# ------------------------------------------------------------------------------
# Australia: Table O state summary, calendar 2025 (includes rooftop solar).
# US: EIA Electric Power Annual, net generation by state (all producers) plus
# EIA's estimate of small-scale solar by state (Table 3.21), so both sides
# include rooftop solar. The latest US state year is a year behind Australia's.
DC_STATES <- c(VA = "Virginia", TX = "Texas", CA = "California", OR = "Oregon",
               AZ = "Arizona", GA = "Georgia", OH = "Ohio", IL = "Illinois", "US-TOTAL" = "United States")
STATE_MAP <- c("Coal" = "Coal", "Natural Gas" = "Gas", "Petroleum" = "Oil", "Other Gases" = "Other",
               "Nuclear" = "Nuclear", "Hydroelectric Conventional" = "Hydro", "Wind" = "Wind",
               "Solar Thermal and Photovoltaic" = "Utility-scale solar",
               "Wood and Wood Derived Fuels" = "Bioenergy", "Other Biomass" = "Bioenergy",
               "Geothermal" = "Geothermal", "Other" = "Other")
st_file <- eia_get("https://www.eia.gov/electricity/data/state/annual_generation_state.xls",
                   "annual_generation_state.xls")
us_states <- read_excel(st_file, skip = 1) |>
  rename(year = 1, state = 2, producer = 3, source = 4, mwh = 5) |>
  mutate(state = toupper(state)) |>                     # the US total is "US-TOTAL" in some years, "US-Total" in others
  filter(producer == "Total Electric Power Industry", state %in% names(DC_STATES),
         source %in% names(STATE_MAP)) |>
  mutate(year = as.integer(year), fuel = STATE_MAP[source], mwh = pmax(as.numeric(mwh), 0))
st_year <- max(us_states$year)

# Small-scale solar by state (thousand MWh = GWh): EPA Table 3.21 has the latest
# year and the year before; use the column for st_year.
ss_file <- eia_get("https://www.eia.gov/electricity/annual/xls/epa_03_21.xlsx", "epa_03_21.xlsx")
ss_raw <- suppressMessages(read_excel(ss_file, col_names = FALSE))
ss_hdr <- apply(ss_raw[3:6, ], 2, function(c) paste(na.omit(c), collapse = " | "))
ss_col <- which(grepl(paste0("Year ", st_year), ss_hdr) &
                  cumsum(grepl("Small Scale Generation", ss_hdr)) > 0)[1]
stopifnot("EIA Table 3.21 layout changed" = !is.na(ss_col))
ss_names <- c(DC_STATES[DC_STATES != "United States"], "US-TOTAL" = "U.S. Total")
us_small <- tibble(state = names(ss_names),
                   gwh = suppressWarnings(as.numeric(unlist(ss_raw[match(ss_names, ss_raw[[1]]), ss_col])))) |>
  mutate(fuel = "Rooftop solar")
stopifnot(!anyNA(us_small$gwh))

state_mix <- bind_rows(
  state_2025 |> filter(region != "AUS") |>
    transmute(group = "Australia
2025", place = region,
              fuel = to_cmp(fuel), gwh),
  state_2025 |> filter(region == "AUS") |>
    transmute(group = "Australia
2025", place = "Australia",
              fuel = to_cmp(fuel), gwh),
  us_states |> filter(year == st_year) |>
    transmute(group = sprintf("Main U.S. data
center states
%d", st_year),
              place = DC_STATES[state], fuel, gwh = mwh / 1000),
  us_small |> transmute(group = sprintf("Main U.S. data
center states
%d", st_year),
                        place = DC_STATES[state], fuel, gwh)
) |>
  group_by(group, place, fuel) |> summarise(gwh = sum(gwh), .groups = "drop") |>
  group_by(group, place) |>
  mutate(share = gwh / sum(gwh), twh = sum(gwh) / 1000,
         renew = sum(gwh[fuel %in% CMP_RENEW]) / sum(gwh)) |>
  ungroup() |>
  mutate(fuel  = factor(fuel, levels = cmp_levels),
         total = place %in% c("Australia", "United States"),
         label = sprintf("%s  (%s TWh)", place, comma(twh, accuracy = if_else(twh < 10, 0.1, 1))),
         # national totals at the bottom of each panel, states by renewable share
         label = fct_reorder(label, renew + if_else(total, -10, 0)))

p2 <- ggplot(state_mix, aes(share, label, fill = fuel)) +
  geom_col(width = 0.72) +
  geom_text(data = distinct(state_mix, group, label, renew),
            aes(x = 1.02, y = label, label = percent(renew, accuracy = 1)),
            inherit.aes = FALSE, hjust = 0, size = 3, colour = "grey20") +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = cmp_cols, labels = function(x) ifelse(x %in% names(cmp_lab), cmp_lab[x], x),
                    drop = TRUE) +
  scale_x_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0), limits = c(0, 1.09)) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(
    title    = "Electricity generation by source, by state",
    subtitle = sprintf("Share of generation by source, including rooftop solar: Australia calendar 2025, U.S. %d. Right-hand figure is renewable share.", st_year),
    x = NULL, y = NULL,
    caption  = paste0("Sources: DCCEEW, Australian Energy Statistics, Table O (calendar 2025); EIA, Electric Power ",
                      "Annual, net generation by state and Table 3.21\n(small-scale solar), calendar ", st_year,
                      ".\nGeneration is where power is made, not where it is used. New South Wales and Victoria have ",
                      "about 96% of Australia's data center demand (AEMO, 2025-26).")
  ) +
  theme_csis() +
  theme(strip.text.y = element_text(angle = 0, hjust = 0, face = "bold", size = 8.5),
        strip.placement = "outside", panel.spacing.y = unit(0.8, "lines"))

ggsave("02_state_fuel_mix.png", p2, width = 11, height = 7.2, dpi = 150, bg = "white")

renew_note <- state_mix |>
  distinct(place, renew) |>
  arrange(desc(renew)) |>
  with(paste(sprintf("%s %s", place, percent(renew, accuracy = 1)), collapse = "; "))

export_chart_csv(p2, "02_state_fuel_mix.csv",
                 y_format = "percent", chart_type = "Stacked horizontal bar (100%), two panels",
                 value_var = "share", extra = c("group", "fuel"),
                 notes = paste0("Value is on the x-axis. Renewable share: ", renew_note, "."))


# ------------------------------------------------------------------------------
# 5. CHART 3 - electricity generation by fuel, Australia vs US, latest year
#
# (Was primary energy vs electricity; the primary energy bar was removed.
# The file name is kept so the dashboard link does not change.)
# ------------------------------------------------------------------------------

# Same calendar year for both: Australia from Table O's calendar-year sheet,
# the US from EIA.
mix_year <- min(us_last, max(gen_cy$year[gen_cy$region == "AUS"]))
comparison <- bind_rows(
  gen_cy |> filter(region == "AUS", year == mix_year) |> mutate(fuel = to_cmp(fuel), country = "Australia"),
  us_gen |> filter(year == mix_year)
) |>
  group_by(country, fuel) |> summarise(gwh = sum(gwh), .groups = "drop") |>
  group_by(country) |> mutate(share = gwh / sum(gwh)) |> ungroup() |>
  mutate(fuel = factor(fuel, levels = cmp_levels),
         country = factor(country, levels = c("United States", "Australia")))

# Sources in order of Australia's share, largest (coal) first, left to right;
# every source kept. Dark text on the light colors so labels stay readable.
au_order <- comparison |> filter(country == "Australia") |> arrange(desc(share)) |> pull(fuel) |> as.character()
comparison <- comparison |>
  mutate(fuel = factor(fuel, levels = c(au_order, setdiff(cmp_levels, au_order))),
         ink  = if_else(fuel %in% c("Utility-scale solar", "Rooftop solar", "Wind", "Other", "Geothermal"), "dark", "light"))
p3 <- ggplot(comparison, aes(share, country, fill = fuel)) +
  geom_col(width = 0.6, position = position_stack(reverse = TRUE)) +   # shares are in the hover pop-ups
  scale_fill_manual(values = cmp_cols, labels = function(x) ifelse(x %in% names(cmp_lab), cmp_lab[x], x),
                    drop = TRUE) +
  scale_x_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +   # same order as the bars, read across
  labs(
    title    = "Electricity generation by source, Australia and the United States",
    subtitle = sprintf("Share of total generation, calendar %d", mix_year),
    x = NULL, y = NULL,
    caption = paste0("Sources: DCCEEW, Australian Energy Statistics, Table O (calendar years, includes rooftop solar);\n",
                     "EIA, Monthly Energy Review, Tables 7.2a and 10.6 (includes small-scale solar).\n",
                     "Australia is shown for the calendar year to match the U.S.; the Source over Time tab uses Australian financial years, so its latest figures differ slightly.")
  ) +
  theme_csis() +
  theme(plot.margin = margin(5.5, 18, 5.5, 5.5))   # room for the 100% label

ggsave("03_primary_vs_electricity.png", p3, width = 9, height = 3.8, dpi = 150, bg = "white")
write_hover(p3, "03_primary_vs_electricity.png", width = 9, height = 3.8, dpi = 150,
            spots = hbar_spots(function(cat, series, v)
              sprintf("<b>%s</b><br>%s: %s of generation", cat, series, percent(v, accuracy = 0.1))))

export_chart_csv(p3, "03_primary_vs_electricity.csv",
                 y_format = "percent", chart_type = "Stacked horizontal bar (100%)",
                 value_var = "share", extra = "fuel",
                 notes = sprintf("Calendar %d for both countries. File name kept from the earlier primary energy chart.",
                                 mix_year))

# ------------------------------------------------------------------------------
# 5c. Renewable share over time, Australia vs US
# ------------------------------------------------------------------------------
renew_ts <- bind_rows(
  mix_ts |>
    group_by(country, year) |>
    summarise(share = sum(share[fuel %in% CMP_RENEW]), .groups = "drop") |>
    mutate(series = if_else(country == "Australia",
                            "Australia",
                            "United States"))
  # Renewables only: nuclear (carbon-free but not renewable) is in the
  # carbon-free generation by technology chart
) |>
  filter(year >= 2000) |>
  mutate(series = factor(series, levels = c("Australia", "United States")))
renew_last <- renew_ts |> group_by(series) |> slice_max(year, n = 1) |> ungroup()

p9 <- ggplot(renew_ts, aes(year, share, colour = series, linetype = series)) +
  geom_line(linewidth = 1) +
  geom_point(data = renew_last, size = 2.4, show.legend = FALSE) +
  geom_text(data = renew_last, aes(label = percent(share, accuracy = 1)), hjust = -0.3, size = 3.4,
            show.legend = FALSE) +
  scale_colour_manual(values = c("#00205B", "#D2232A"), name = NULL) +
  scale_linetype_manual(values = c("solid", "solid"), name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA),
                     expand = expansion(c(0, 0.05))) +
  scale_x_continuous(breaks = seq(2000, 2025, 5), expand = expansion(c(0.01, 0.06))) +
  guides(colour = guide_legend(ncol = 1), linetype = guide_legend(ncol = 1)) +
  labs(
    title    = "Renewable share of electricity generation, Australia and the United States",
    subtitle = "Hydro, wind, solar (including rooftop), bioenergy and geothermal, share of total generation",
    x = NULL, y = NULL,
    caption  = paste0("Sources: DCCEEW, Australian Energy Statistics, Table O; ", US_SRC,
                      "\nAustralian years are financial years (to June), labeled by the year they end. ",
                      "Nuclear is carbon-free but not renewable, so it is not included.")
  ) +
  theme_csis() +
  theme(legend.key.width = unit(1.2, "cm"))

ggsave("09_renewable_share_au_us.png", p9, width = 9, height = 5.4, dpi = 150, bg = "white")



# ------------------------------------------------------------------------------
# 6. CHART 4 - targets: Australia's 82% and the former US 2035 goal
#
# So what: the Commonwealth target is 82% renewable electricity by 2030.
# Realised share against the straight line to that target is the single most
# useful number for judging whether the commitment is financeable, and it
# drives the transmission and storage procurement US firms bid into.
# ------------------------------------------------------------------------------

renew_hist <- gen_fy |>
  filter(region == "AUS") |>
  group_by(year) |>
  summarise(share = sum(gwh[fuel %in% RENEWABLES]) / sum(gwh), .groups = "drop") |>
  mutate(year = year + 1L) |>                                     # financial year by the year it ends
  filter(year >= 2000)

last_pt     <- slice_max(renew_hist, year)
target_line <- tibble(year = c(last_pt$year, 2030), share = c(last_pt$share, 0.82))

# The US has no current national target. The closest equivalent is the former
# federal goal of 100% carbon pollution-free electricity by 2035 (set in 2021,
# reversed by executive order in January 2025). It counted nuclear as well as
# renewables, so it is set against the US renewables-plus-nuclear share.
us_lowc <- mix_ts |>
  filter(country == "United States", year >= 2000) |>
  group_by(year) |>
  summarise(share = sum(share[fuel %in% c(CMP_RENEW, "Nuclear")]), .groups = "drop")
us_last_pt <- slice_max(us_lowc, year)
us_goal    <- tibble(year = c(us_last_pt$year, 2035), share = c(us_last_pt$share, 1))

tgt_cols <- c("Australia" = "#00205B", "United States" = "#D2232A")
p4 <- ggplot() +
  geom_line(data = renew_hist |> mutate(s = "Australia"),
            aes(year, share, colour = s), linewidth = 1.1) +
  geom_line(data = us_lowc |> mutate(s = "United States"),
            aes(year, share, colour = s), linewidth = 1.1, linetype = "22") +
  geom_line(data = target_line, aes(year, share), linetype = "11", colour = "#00205B", linewidth = 0.6) +
  geom_line(data = us_goal, aes(year, share), linetype = "11", colour = "grey60", linewidth = 0.6) +   # revoked: grey
  geom_point(data = tibble(year = 2030, share = 0.82), aes(year, share),
             shape = 21, fill = "white", colour = "#00205B", size = 3) +
  geom_point(data = tibble(year = 2035, share = 1), aes(year, share),
             shape = 21, fill = "white", colour = "grey60", size = 3) +
  # Target labels above their markers, clear of the dotted paths
  annotate("text", x = 2029.4, y = 0.9, label = "Australia: 82% renewable\nby 2030 (current target)",
           hjust = 1, vjust = 0, size = 3, colour = "#00205B", lineheight = 0.95) +
  annotate("text", x = 2035, y = 1.07, label = "U.S.: 100% carbon-free by 2035\n(former target, revoked 2025)",
           hjust = 1, vjust = 0, size = 3, colour = "grey45", lineheight = 0.95) +
  scale_colour_manual(values = tgt_cols, name = NULL,
                      labels = c("Australia" = "Australia", "United States" = "United States")) +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "22")))) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.24), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(breaks = seq(2000, 2035, 5)) +
  labs(
    title    = "Clean electricity targets, Australia and the United States",
    subtitle = "Share of generation against each country's target",
    x = NULL, y = NULL,
    caption = paste0("Sources: DCCEEW, Australian Energy Statistics, Table O; ", US_SRC,
                     "\nAustralia's line is its renewable share; the U.S. line is its carbon-free share (renewables plus nuclear), as each target is defined. ",
                     "Dotted lines: straight paths to target. Australian years are financial years (to June), labeled by the year they end. ",
                     "Australia's 82% target applies to the main grids (NEM and WA's SWIS); this all-Australia share is a close proxy. ",
                     "The U.S. has no current national target; the 2035 target (2021) was reversed in January 2025.")
  ) +
  theme_csis() +
  theme(legend.key.width = unit(1.2, "cm"))

ggsave("04_renewables_vs_target.png", p4, width = 9.5, height = 5.4, dpi = 150, bg = "white")

write_csv(bind_rows(renew_hist |> mutate(series = "Australia: renewables (year = end of financial year)"),
                    us_lowc |> mutate(series = "United States: renewables plus nuclear (calendar year)"),
                    tibble(year = c(2030, 2035), share = c(0.82, 1),
                           series = c("Australia target (82% by 2030)",
                                      "U.S. former target (100% carbon-free by 2035, revoked 2025)"))),
          "04_renewables_vs_target.csv")


# ------------------------------------------------------------------------------
# 7. CHART 5 - carbon-free generation by technology, Australia and the US
#
# Renewables plus nuclear, as shares of each country's total generation so the
# two very different-sized grids sit on one scale; the height of each column
# is the carbon-free share. Nuclear is most of the US's carbon-free power and
# none of Australia's; rooftop solar is Australia's largest single source.
# ------------------------------------------------------------------------------

CARBON_FREE <- c("Nuclear", "Geothermal", "Bioenergy", "Hydro", "Wind", "Utility-scale solar", "Rooftop solar")
cf_ts <- mix_ts |>
  filter(fuel %in% CARBON_FREE, year >= 2005) |>
  mutate(fuel = factor(as.character(fuel), levels = CARBON_FREE),
         country = factor(country, levels = c("Australia", "United States")))

p5 <- ggplot(cf_ts, aes(year, share, fill = fuel)) +
  geom_col(width = 0.75) +
  facet_wrap(~country) +
  scale_fill_manual(values = cmp_cols, labels = function(x) recode(x, !!!cmp_lab)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(c(0, 0.05))) +
  scale_x_continuous(breaks = seq(2005, 2025, 5)) +
  labs(
    title    = "Carbon-free electricity generation by technology, Australia and the United States",
    subtitle = "Share of total generation",
    x = NULL, y = NULL,
    caption  = paste0("Sources: DCCEEW, Australian Energy Statistics, Table O; ", US_SRC,
                      "\nAustralian years are financial years (to June), labeled by the year they end.")
  ) +
  theme_csis() +
  theme(strip.text = element_text(face = "bold", hjust = 0.5))   # panel titles centred

ggsave("05_carbon_free_by_technology.png", p5, width = 10, height = 5.4, dpi = 150, bg = "white")

export_chart_csv(p5, "05_carbon_free_by_technology.csv",
                 y_format = "percent", chart_type = "Stacked vertical bar, one panel per country",
                 extra = c("country", "fuel"),
                 notes = paste0("Values are shares of each country's total generation. Australia: year = end ",
                                "year of the financial year; U.S.: calendar year."))


# ------------------------------------------------------------------------------
# 7b. ENERGY MIX - energy use by source, Australia and the US
#
# Primary energy consumption by fuel, so oil (mostly transport fuel) and gas
# burned directly in industry and homes count, not only power generation.
# Australia: DCCEEW, Australian Energy Statistics, Table C (financial years;
# coal, oil, gas and renewables, which Table C does not split by technology).
# US: EIA Monthly Energy Review Table 1.3 (calendar years), grouped the same
# way, plus nuclear, which Australia does not have. Energy units differ (PJ and Btu), so
# the charts compare shares.
# ------------------------------------------------------------------------------

PE_LEVELS <- c("Coal", "Oil", "Gas", "Nuclear", "Renewables")
pe_cols   <- c(Coal = "#2c2c2a", Oil = fuel_cols[["Oil"]], Gas = fuel_cols[["Gas"]],
               Nuclear = "#b8478c", Renewables = fuel_cols[["Wind"]])
PE_SRC <- paste0("Sources: DCCEEW, Australian Energy Statistics, Table C (primary energy consumption); ",
                 "EIA, Monthly Energy Review, Table 1.3 (primary energy consumption by source).")

# Table C: one sheet per state ("AUS" is Australia), a header row starting
# "Year", then one row per financial year. Column names may carry notes ("[note b]").
read_table_c <- function(sheet) {
  x <- read_excel(TABLE_C_FILE, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  hdr <- which(trimws(x[[1]]) == "Year")[1]
  stopifnot("Table C layout changed: no 'Year' header row" = !is.na(hdr))
  names(x) <- sub("\\s*\\[.*$", "", trimws(unlist(x[hdr, ])))
  x <- x[(hdr + 1):nrow(x), ]
  x <- x[grepl("^\\d{4}-\\d{2}$", trimws(x$Year)), ]
  stopifnot("Table C layout changed: fuel columns not found" = all(c("Coal", "Oil", "Gas", "Renewables") %in% names(x)))
  tibble(year = as.integer(substr(trimws(x$Year), 1, 4)),
         Coal = x$Coal, Oil = x$Oil, Gas = x$Gas, Renewables = x$Renewables) |>
    pivot_longer(-year, names_to = "fuel", values_to = "pj") |>
    mutate(pj = coalesce(suppressWarnings(as.numeric(pj)), 0), region = sheet)   # "-" and "na" are zero
}
au_pe <- read_table_c("AUS")                                         # Australia as a whole
au_pe_last <- max(au_pe$year[au_pe$region == "AUS"])
stopifnot("Table C: Australia's total looks wrong" =
            between(sum(au_pe$pj[au_pe$region == "AUS" & au_pe$year == au_pe_last]), 4000, 8000))

# US national (quadrillion Btu)
PE_US_MAP <- c(CLTCBUS = "Coal", PMTCBUS = "Oil", NNTCBUS = "Gas", NUETBUS = "Nuclear", RETCBUS = "Renewables")
us_pe <- mer_annual("T01.03") |>
  filter(msn %in% names(PE_US_MAP), !is.na(gwh)) |>
  transmute(year, fuel = PE_US_MAP[msn], quads = pmax(gwh, 0))   # mer_annual's value column is named gwh
us_pe_last <- max(us_pe$year)

pe_ts <- bind_rows(
  au_pe |> filter(region == "AUS") |> transmute(country = "Australia", year, fuel, value = pj),
  us_pe |> transmute(country = "United States", year, fuel, value = quads)
) |>
  filter(year >= 1990) |>
  tidyr::complete(tidyr::nesting(country, year), fuel = PE_LEVELS, fill = list(value = 0)) |>
  group_by(country, year) |> mutate(share = value / sum(value)) |> ungroup() |>
  mutate(fuel = factor(fuel, levels = PE_LEVELS))

fy <- function(y) sprintf("%d-%02d", y, (y + 1) %% 100)

# ---- Energy chart 1: source, latest year ----------------------------------------
pe_now <- pe_ts |>
  filter((country == "Australia" & year == au_pe_last) | (country == "United States" & year == us_pe_last)) |>
  mutate(country = factor(recode(country, Australia = sprintf("Australia (%s)", fy(au_pe_last)),
                                 `United States` = sprintf("United States (%d)", us_pe_last)),
                          levels = c(sprintf("United States (%d)", us_pe_last), sprintf("Australia (%s)", fy(au_pe_last)))))

pe1 <- ggplot(pe_now, aes(share, country, fill = fuel)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = ifelse(share >= 0.04, percent(share, accuracy = 1), "")),
            position = position_stack(vjust = 0.5), colour = "white", size = 3.2) +
  scale_fill_manual(values = pe_cols, drop = TRUE) +
  scale_x_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
  labs(
    title    = "Energy use by source, Australia and the United States",
    subtitle = "Share of primary energy consumption, latest year. Oil is mostly transport fuel.",
    x = NULL, y = NULL,
    caption  = paste0(PE_SRC, " Labels shown for shares of 4% or more.")
  ) +
  theme_csis()

ggsave("08_energy_source_au_us.png", pe1, width = 9, height = 3.8, dpi = 150, bg = "white")
export_chart_csv(pe1, "08_energy_source_au_us.csv",
                 y_format = "percent", chart_type = "Stacked horizontal bar (100%)",
                 value_var = "share", extra = "fuel",
                 notes = sprintf("Australia financial year %s; U.S. calendar %d. Shares of primary energy consumption.",
                                 fy(au_pe_last), us_pe_last))

# ------------------------------------------------------------------------------
# 7c. ENERGY SECURITY - net energy trade by source, Australia and the US
#
# Net exports (+) or net imports (-) of each fuel, as a share of the country's
# own primary energy use, so the two very different-sized economies compare.
# Australia: DCCEEW, Australian Energy Statistics, Table J (financial years, PJ).
# US: EIA Monthly Energy Review Table 1.4c (net imports by source) and Table 1.3
# (primary energy consumption), calendar years. Neither country's energy
# statistics count uranium as energy trade, so uranium is described in the
# caption: the share of US reactor uranium bought from abroad comes from EIA's
# Uranium Marketing Annual Report, Table 2.
# ------------------------------------------------------------------------------

SEC_LEVELS <- c("Coal", "Natural gas", "Crude oil", "Refined fuels", "Biofuels and biomass", "Electricity")
sec_cols   <- c(Coal = "#2c2c2a", `Natural gas` = fuel_cols[["Gas"]], `Crude oil` = fuel_cols[["Oil"]],
                `Refined fuels` = "#c98a6b", `Biofuels and biomass` = fuel_cols[["Bioenergy"]], Electricity = "#8c96a3")

# Table J: one sheet, a "Fuel type" header row with one column per financial
# year, then blocks headed "Indigenous production", "Imports", "Exports".
tj <- read_excel(TABLE_J_FILE, sheet = "Table J", col_names = FALSE, col_types = "text", .name_repair = "minimal")
tj_hdr <- which(trimws(tj[[1]]) == "Fuel type")[1]
stopifnot("Table J layout changed: no 'Fuel type' header row" = !is.na(tj_hdr))
tj_years <- trimws(unlist(tj[tj_hdr, -1]))
tj_col   <- max(which(grepl("^\\d{4}-\\d{2}$", tj_years))) + 1        # latest financial year
tj_fy    <- tj_years[tj_col - 1]
# Financial years are shown by the year they end, across the dashboard
tj_end   <- as.integer(substr(tj_fy, 1, 4)) + 1L
tj_rows  <- tibble(item  = sub("\\s*\\[.*$", "", trimws(tj[[1]])),
                   value = coalesce(suppressWarnings(as.numeric(tj[[tj_col]])), 0))
block_of <- function(start, end) {
  i <- which(tj_rows$item == start)[1]; j <- which(tj_rows$item == end)[1]
  stopifnot("Table J layout changed: trade blocks not found" = !is.na(i) && !is.na(j) && j > i)
  tj_rows[(i + 1):(j - 1), ]
}
au_source <- function(item) dplyr::case_when(
  item %in% c("Black coal", "Brown coal", "Coke", "Briquettes", "Brown coal briquettes") ~ "Coal",
  item == "Natural gas"      ~ "Natural gas",
  item == "Crude oil and ORF" ~ "Crude oil",
  TRUE                        ~ "Refined fuels")                       # LPG and petroleum products
au_trade <- bind_rows(
  block_of("Imports", "Exports") |> mutate(flow = -1),
  block_of("Exports", "Stock changes and discrepancies") |> mutate(flow = 1)
) |>
  mutate(source = au_source(item)) |>
  group_by(source) |> summarise(net = sum(flow * value), .groups = "drop")
au_tpes <- tj_rows$value[tj_rows$item == "Total primary energy supply"][1]
stopifnot("Table J: total primary energy supply looks wrong" = between(au_tpes, 4000, 8000))

# Fuel by fuel, for the footnote and summary: share of Australia's coal and gas
# production that is (net) exported, and share of its oil use that is (net) imported
au_prod <- block_of("Indigenous production", "Imports")
au_p    <- function(x) sum(au_prod$value[au_prod$item %in% x])
au_n    <- function(x) sum(au_trade$net[au_trade$source %in% x])
au_oil_ni <- -au_n(c("Crude oil", "Refined fuels"))
per_fuel_note <- sprintf(
  "Fuel by fuel, Australia (net) exported about %.0f%% of the coal and %.0f%% of the gas it produced, and imported about %.0f%% of the oil it used.",
  100 * au_n("Coal") / au_p("Coal"), 100 * au_n("Natural gas") / au_p("Natural gas"),
  100 * au_oil_ni / (au_p(c("Crude oil and NGL", "LPG")) + au_oil_ni))
message(per_fuel_note)

# US: net imports by source (quadrillion Btu), turned round into net exports
US_NI_MAP <- c(CLNIBUS = "Coal", CCNIBUS = "Coal", NGNIBUS = "Natural gas", CONIBUS = "Crude oil",
               RQNIBUS = "Refined fuels", BMNIBUS = "Biofuels and biomass", ELNIBUS = "Electricity")
us_ni <- mer_annual("T01.04C") |> filter(msn %in% names(US_NI_MAP), !is.na(gwh))
us_sec_year <- max(us_ni$year)
us_trade <- us_ni |> filter(year == us_sec_year) |>
  transmute(source = US_NI_MAP[msn], net = -gwh) |>                   # mer_annual's value column is named gwh
  group_by(source) |> summarise(net = sum(net), .groups = "drop")
us_use <- mer_annual("T01.03") |> filter(msn == "TETCBUS", year == us_sec_year) |> pull(gwh)

# Uranium: share of US reactor purchases that was foreign-origin (EIA UMAR Table 2)
umar_file <- eia_get("https://www.eia.gov/uranium/marketing/xls/umartable2figures3n4.xlsx", "umartable2figures3n4.xlsx")
umar <- suppressMessages(read_excel(umar_file, col_names = FALSE, col_types = "text"))
umar_yrow <- which(apply(umar, 1, function(r) sum(grepl("^\\d{4}$", r)) >= 3))[1]
purchases_after <- function(label) {
  i <- which(trimws(umar[[1]]) == label)[1]
  j <- i + which(trimws(umar[[1]][(i + 1):nrow(umar)]) == "Purchases")[1]
  suppressWarnings(as.numeric(unlist(umar[j, ])))
}
umar_foreign <- purchases_after("Foreign-origin uranium")
umar_total   <- purchases_after("Total")
umar_ok      <- which(!is.na(umar_foreign) & !is.na(umar_total) & grepl("^\\d{4}$", unlist(umar[umar_yrow, ])))
uranium_note <- if (length(umar_ok)) {
  k <- max(umar_ok)
  sprintf("Uranium is not counted as energy trade: %.0f%% of uranium bought by U.S. reactors in %s was imported (EIA), and Australia exports almost all it mines.",
          100 * umar_foreign[k] / umar_total[k], unlist(umar[umar_yrow, ])[k])
} else "Uranium is not counted as energy trade by either country."

# Drawn twice: crude oil and refined fuels shown separately, and combined
draw_sec <- function(au_t, us_t, levels, cols, file, oil_note) {
  sec <- bind_rows(
    au_t |> mutate(country = sprintf("Australia (%d, year to June)", tj_end), use = au_tpes, unit = "PJ"),
    us_t |> mutate(country = sprintf("United States (%d)", us_sec_year), use = us_use, unit = "quadrillion Btu")
  ) |>
    mutate(share  = net / use,
           source = factor(source, levels = levels),
           country = factor(country, levels = rev(unique(country))))
  sec_tot <- sec |> group_by(country) |> summarise(share = sum(share), .groups = "drop") |>
    mutate(strip = sprintf("%s: net %s of %s of its total energy use", country,
                           if_else(share >= 0, "exporter", "importer"), percent(abs(share), accuracy = 1)))
  sec <- sec |> left_join(sec_tot |> select(country, strip), by = "country") |>
    group_by(country) |>
    # label a segment if it is at least 7% of its own panel's span
    mutate(lab = ifelse(abs(share) >= 0.07 * (sum(pmax(share, 0)) - sum(pmin(share, 0))),
                        paste0(if_else(share > 0, "+", ""), percent(share, accuracy = 1)), "")) |>
    ungroup()
  sec_tot <- sec_tot |> mutate(strip = factor(strip, levels = unique(sec$strip[order(sec$country, decreasing = TRUE)])))
  sec <- sec |> mutate(strip = factor(strip, levels = levels(sec_tot$strip)))

  # Segments laid out here rather than by ggplot's stacking: exports run right
  # from zero and imports left, each in the order of the source levels, so labels and
  # hover areas line up exactly
  sec <- sec |> arrange(country, source) |> group_by(country) |>
    mutate(xmax = if_else(share >= 0, cumsum(pmax(share, 0)), cumsum(pmin(share, 0))),
           xmin = xmax - share, row = 1) |>
    ungroup()
  sec_tot$row <- 1                                                       # one bar per panel
  pe5 <- ggplot(sec, aes(share, row, fill = source)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0.72, ymax = 1.28), colour = "white", linewidth = 0.3) +
    geom_point(data = sec_tot, aes(share, row), inherit.aes = FALSE, shape = 23, size = 3,
               fill = "white", colour = "black", stroke = 0.7) +
    facet_wrap(~strip, ncol = 1, scales = "free_x") +
    scale_fill_manual(values = cols, drop = TRUE) +
    scale_x_continuous(labels = function(x) ifelse(x > 0, paste0("+", percent(x, accuracy = 1)), percent(x, accuracy = 1)),
                       breaks = scales::breaks_pretty(n = 6), expand = expansion(c(0.04, 0.04))) +
    scale_y_continuous(breaks = NULL, limits = c(0.6, 1.4)) +
    labs(
      title    = "Net energy trade, Australia and the United States",
      subtitle = paste0("Net exports (right of zero) and net imports (left), % of each country's total energy use (all fuels). ",
                        "Diamond = net position; each country on its own scale."),
      x = NULL, y = NULL,
      caption  = paste0("Sources: DCCEEW, Australian Energy Statistics, Table J (year to June ", tj_end, "); ",
                        "EIA, Monthly Energy Review, Tables 1.3 and 1.4c (", us_sec_year, ").\n",
                        oil_note, " ", uranium_note, "\n", per_fuel_note)
    ) +
    theme_csis() +
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey92"),
          strip.text = element_text(face = "bold", hjust = 0, size = 10.5), panel.spacing.y = unit(0.6, "lines"),
          plot.caption = element_text(lineheight = 1.0, margin = margin(t = 4)),
          plot.subtitle = element_text(margin = margin(b = 4)),
          plot.margin = margin(5.5, 18, 5.5, 5.5))                    # room for the last axis label

  ggsave(file, pe5, width = 9.5, height = 5.9, dpi = 150, bg = "white")   # full-width card; room for the bars
  export_chart_csv(pe5, sub("\\.png$", ".csv", file),
                   y_format = "percent", chart_type = "Stacked horizontal bar (net exports positive, net imports negative)",
                   value_var = "share", extra = c("country", "source"),
                   notes = paste0("Share = net exports / total primary energy use (all fuels). Australia ", tj_fy, " (PJ, Table J); U.S. ",
                                  us_sec_year, " (quadrillion Btu, EIA MER 1.4c and 1.3). ", uranium_note))

  # Hover tooltips: each segment's value, in its own units, and the share
  write_hover(pe5, file, width = 9.5, height = 5.9, dpi = 150, spots = function(built) {
    lay <- built$layout$layout                                           # one panel per country
    v   <- sec |> filter(abs(share) > 0)
    fmt_v <- ifelse(v$unit == "PJ", sprintf("%s PJ", comma(abs(v$net), accuracy = 1)),
                    sprintf("%.2f quadrillion Btu", abs(v$net)))
    data.frame(PANEL = lay$PANEL[match(as.character(v$strip), as.character(lay$strip))],
               xmin = v$xmin, xmax = v$xmax, ymin = 0.72, ymax = 1.28,
               text = sprintf("<b>%s</b>, %s<br>Net %s %s<br>%s of its total energy use",
                              sub(" \\(.*", "", as.character(v$country)), v$source,
                              if_else(v$net >= 0, "exports", "imports"), fmt_v, percent(abs(v$share), accuracy = 0.1)))
  })
  invisible(pe5)
}

draw_sec(au_trade, us_trade, SEC_LEVELS, sec_cols, "08_energy_security.png",
         "Refined fuels: LPG, petrol, diesel, jet fuel and other petroleum products.")

# Oil combined: crude oil and refined fuels as one source
oil_combine <- function(t) t |>
  mutate(source = if_else(source %in% c("Crude oil", "Refined fuels"), "Oil (crude and refined)", source)) |>
  group_by(source) |> summarise(net = sum(net), .groups = "drop")
SEC_LEVELS_OIL <- c("Coal", "Natural gas", "Oil (crude and refined)", "Biofuels and biomass", "Electricity")
sec_cols_oil   <- c(sec_cols[c("Coal", "Natural gas", "Biofuels and biomass", "Electricity")],
                    `Oil (crude and refined)` = fuel_cols[["Oil"]])
draw_sec(oil_combine(au_trade), oil_combine(us_trade), SEC_LEVELS_OIL, sec_cols_oil,
         "08_energy_security_oil.png",
         "Oil = crude oil plus refined fuels.")


# ------------------------------------------------------------------------------
# 7d. ENERGY SECURITY (simpler, Nov 2026) - self-sufficiency by fuel
#
# Production as a share of use, fuel by fuel: below 100% the country is a net
# importer of that fuel, above 100% a net exporter. Replaces the net energy
# trade chart (08_energy_security_oil.png, still written above) on the
# dashboard; first drafted in 'For feedback/Analysis/Energy self-sufficiency.R'.
#   Australia: Table J, latest financial year. Use = production + imports -
#     exports (apparent use; stock changes left out).
#   U.S.: EIA Monthly Energy Review Tables 1.2 (production) and 1.3 (use),
#     latest calendar year.
# Oil is crude oil, condensate and natural gas liquids (including LPG) for both
# countries; use includes refined products.
# ------------------------------------------------------------------------------

ss_cols <- tj_hdr                                             # Table J header row (read in 7c)
ss_fy_cols <- which(grepl("^\\d{4}-\\d{2}$", trimws(unlist(tj[ss_cols, ]))))
ss_item <- sub("\\s*\\[.*$", "", trimws(tj[[1]]))
ss_val <- function(rows) {                                    # sum of the given rows, one value per year
  m <- sapply(ss_fy_cols, function(j) suppressWarnings(as.numeric(tj[[j]][rows])))
  m <- matrix(m, nrow = length(rows)); m[is.na(m)] <- 0
  colSums(m)
}
ss_rows <- function(start, end, items) {
  i <- which(ss_item == start)[1]; j <- which(ss_item == end)[1]
  r <- (i + 1):(j - 1); r[ss_item[r] %in% items]
}
ss_fys <- trimws(unlist(tj[ss_cols, ss_fy_cols]))
ss_au_year <- as.integer(substr(ss_fys, 1, 4)) + 1L           # financial years by the year they end
SS_COAL <- c("Black coal", "Brown coal", "Coke", "Briquettes", "Brown coal briquettes")
SS_OIL_TRADE <- c("Crude oil and ORF", "LPG", "Automotive gasoline", "Aviation gasoline", "Aviation turbine fuel",
                  "Lighting kerosene", "Power kerosene", "ADO", "IDF", "Fuel oil", "Lubes and greases", "Bitumen",
                  "Other petroleum products")
ss_au_one <- function(fuel, prod_items, trade_items) {
  p  <- ss_val(ss_rows("Indigenous production", "Imports", prod_items))
  im <- ss_val(ss_rows("Imports", "Exports", trade_items))
  ex <- ss_val(ss_rows("Exports", "Stock changes and discrepancies", trade_items))
  tibble(country = "Australia", year = ss_au_year, fy = ss_fys, fuel = fuel, production = p, use = p + im - ex)
}
ss_au <- bind_rows(ss_au_one("Coal", "Coal", SS_COAL),
                   ss_au_one("Natural gas", "Natural gas", "Natural gas"),
                   ss_au_one("Oil", c("Crude oil and NGL", "LPG"), SS_OIL_TRADE)) |>
  filter(year == max(year))
stopifnot("Table J: self-sufficiency inputs look wrong" = all(ss_au$production > 0 & ss_au$use > 0))

ss_us_raw <- bind_rows(mer_annual("T01.02"), mer_annual("T01.03")) |>
  filter(msn %in% c("CLPRBUS", "NGPRBUS", "PAPRBUS", "NLPRBUS", "CLTCBUS", "NNTCBUS", "PMTCBUS"), !is.na(gwh)) |>
  tidyr::pivot_wider(names_from = msn, values_from = gwh)     # quadrillion Btu (column named gwh by mer_annual)
ss_us_year <- max(ss_us_raw$year[complete.cases(ss_us_raw)])
ss_us <- ss_us_raw |> filter(year == ss_us_year) |>
  with(tibble(country = "United States", year = ss_us_year, fy = as.character(ss_us_year),
              fuel = c("Coal", "Natural gas", "Oil"),
              production = c(CLPRBUS, NGPRBUS, PAPRBUS + NLPRBUS), use = c(CLTCBUS, NNTCBUS, PMTCBUS)))

ss <- bind_rows(ss_au, ss_us) |>
  mutate(ratio = production / use,
         fuel = factor(fuel, levels = rev(c("Coal", "Natural gas", "Oil"))),
         series = factor(if_else(country == "Australia", sprintf("Australia (%s)", ss_au$fy[1]),
                                 sprintf("United States (%d)", ss_us_year))),
         series = factor(series, levels = rev(levels(series))))
ss_au_oil_ex_lpg <- {
  p <- ss_val(ss_rows("Indigenous production", "Imports", "Crude oil and NGL"))
  p[length(p)] / ss_au$use[ss_au$fuel == "Oil"]
}
ss_dodge <- position_dodge(width = 0.75)
ss_lev <- c(grep("^Australia", levels(ss$series), value = TRUE), grep("^United", levels(ss$series), value = TRUE))
p_ss <- ggplot(ss, aes(ratio, fuel, fill = series, group = series)) +
  geom_col(position = ss_dodge, width = 0.7) +
  geom_vline(xintercept = 1, linetype = "22", colour = "grey35", linewidth = 0.5) +
  geom_text(aes(label = percent(ratio, accuracy = 1)), position = ss_dodge, hjust = -0.15, size = 3.3, colour = "grey30") +
  annotate("text", x = 1, y = 3.45, label = "Self-sufficient", hjust = -0.05, vjust = 0, size = 3.2, colour = "grey30") +
  scale_y_discrete(expand = expansion(add = c(0.6, 0.75))) +
  annotate("segment", x = 1.04, xend = 1.85, y = 2.5, yend = 2.5, colour = "grey30", linewidth = 0.5,
           arrow = grid::arrow(length = grid::unit(0.12, "inches"), type = "closed")) +
  annotate("text", x = 1.95, y = 2.5, label = "Excess production exported (net)", hjust = 0, vjust = 0.5,
           size = 3.6, colour = "grey30") +
  scale_fill_manual(values = setNames(c("#00205B", "#D2232A"), ss_lev), breaks = ss_lev) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 8, 1), expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Energy self-sufficiency by fuel",
       subtitle = "Production as a share of use. Below 100% means net fuel importer, above 100% means net fuel exporter",
       x = NULL, y = NULL,
       caption = paste0("Sources: DCCEEW, Australian Energy Statistics, Table J; EIA, Monthly Energy Review, Tables 1.2 and 1.3.\n",
                        "Notes: Oil includes crude oil, condensate and natural gas liquids (including LPG) for both countries; use includes refined ",
                        "products. Excluding LPG, Australia's oil production covers ", percent(ss_au_oil_ex_lpg, accuracy = 1), " of use. ",
                        "Most Australian crude oil and condensate is exported, so most of the oil Australia uses is imported. ",
                        "Net exports are exports minus imports of the same fuel: a country above 100% exports more of that fuel than it imports, ",
                        "though it may still import some (e.g. the U.S. still imports crude oil).")) +
  theme_csis() +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey92"),
        legend.position = "top", legend.justification = "left", axis.text.y = element_text(size = 11))
ggsave("08_self_sufficiency_by_fuel.png", p_ss, width = 9, height = 5.7, dpi = 150, bg = "white")
ss_note <- with(ss, sprintf(
  "Australia's production equaled %s of its coal use and %s of its gas use but only %s of its oil use in the %s financial year; U.S. production equaled %s, %s and %s in %d.",
  percent(ratio[country == "Australia" & fuel == "Coal"], 1), percent(ratio[country == "Australia" & fuel == "Natural gas"], 1),
  percent(ratio[country == "Australia" & fuel == "Oil"], 1), ss_au$fy[1],
  percent(ratio[country == "United States" & fuel == "Coal"], 1), percent(ratio[country == "United States" & fuel == "Natural gas"], 1),
  percent(ratio[country == "United States" & fuel == "Oil"], 1), ss_us_year))
message(ss_note)


# ------------------------------------------------------------------------------
# 7e. CLEAN TECH IMPORTS (Nov 2026) - reliance on China
#
# China's share of each country's imports (by value) of:
#   Solar cells and panels   HS 854142 + 854143 (from 2022); HS 854140 before
#                            2022 (that code also covered LEDs and other
#                            light-sensitive devices, so earlier years overstate)
#   Lithium-ion batteries    HS 850760
#   Electric cars            HS 870380 (battery electric passenger vehicles)
# Australia: UN Comtrade (Australia's reported imports, US$), free preview API,
#   one call per year, cached in Energy/raw/comtrade (the latest two years are
#   fetched again after 30 days, as late reports come in).
# U.S.: U.S. Census Bureau international trade API (general imports). Needs a
#   free key (https://api.census.gov/data/key_signup.html) in the CENSUS_API_KEY
#   environment variable or in Energy/raw/census_api_key.txt (never committed).
#   Without a key the chart is left as it is.
# First drafted in 'For feedback/Analysis/China clean tech imports.R'.
# ------------------------------------------------------------------------------

CT_YEARS <- 2017:(as.integer(format(Sys.Date(), "%Y")) - 1L)
CT_CODES <- c("850760", "854140", "854142", "854143", "870380")
ct_product <- function(code) case_when(code == "850760" ~ "Lithium-ion batteries",
                                       code == "870380" ~ "Electric cars",
                                       TRUE ~ "Solar cells and panels")
ct_key <- Sys.getenv("CENSUS_API_KEY")
if (ct_key == "" && file.exists(file.path("raw", "census_api_key.txt")))
  ct_key <- trimws(readLines(file.path("raw", "census_api_key.txt"), warn = FALSE)[1])

if (ct_key == "") {
  message("Clean tech imports chart not updated: no Census API key (see section 7e).")
} else {
  dir.create(file.path("raw", "comtrade"), recursive = TRUE, showWarnings = FALSE)
  ct_au <- bind_rows(lapply(CT_YEARS, function(y) {
    f <- file.path("raw", "comtrade", sprintf("au_%d.json", y))
    stale <- !file.exists(f) || (y >= max(CT_YEARS) - 1 && difftime(Sys.time(), file.mtime(f), units = "days") > 30)
    if (stale) {
      Sys.sleep(3)                                            # the free preview allows about one call a second
      try(utils::download.file(sprintf("https://comtradeapi.un.org/public/v1/preview/C/A/HS?reporterCode=36&period=%d&partnerCode=0,156&cmdCode=%s&flowCode=M",
                                       y, paste(CT_CODES, collapse = ",")), f, quiet = TRUE))
    }
    if (!file.exists(f)) return(NULL)
    d <- jsonlite::fromJSON(f)$data
    if (is.null(d) || !NROW(d)) return(NULL)
    tibble(country = "Australia", year = y, code = d$cmdCode,
           partner = if_else(d$partnerCode == 156, "China", "World"), value = d$primaryValue)
  }))
  ct_us <- bind_rows(lapply(CT_YEARS, function(y) bind_rows(lapply(CT_CODES, function(code) {
    r <- tryCatch(jsonlite::fromJSON(sprintf(
      "https://api.census.gov/data/timeseries/intltrade/imports/hs?get=CTY_CODE,GEN_VAL_YR&I_COMMODITY=%s&time=%d-12&key=%s",
      code, y, ct_key)), error = function(e) NULL)
    if (is.null(r) || NROW(r) < 2) return(NULL)
    r <- as_tibble(r[-1, , drop = FALSE], .name_repair = ~ c("cty", "value", "code", "time"))
    r |> filter(cty %in% c("-", "5700")) |>
      transmute(country = "United States", year = y, code, partner = if_else(cty == "5700", "China", "World"),
                value = as.numeric(value))
  }))))
  ct <- bind_rows(ct_au, ct_us) |>
    filter(!(code == "854140" & year >= 2022), !(code %in% c("854142", "854143") & year < 2022)) |>
    mutate(product = ct_product(code)) |>
    group_by(country, year, product, partner) |> summarise(value = sum(value), .groups = "drop") |>
    tidyr::pivot_wider(names_from = partner, values_from = value, values_fill = 0) |>
    filter(World > 0) |>
    mutate(share = China / World,
           product = factor(product, levels = c("Solar cells and panels", "Lithium-ion batteries", "Electric cars")))
  # Keep years both countries report
  ct_last <- min(tapply(ct$year, ct$country, max))
  ct <- ct |> filter(year <= ct_last)
  stopifnot("Clean tech imports: missing data" = n_distinct(ct$country) == 2, all(ct$share <= 1))

  p_ct <- ggplot(ct, aes(year, share, colour = country)) +
    geom_line(linewidth = 1) + geom_point(size = 1.6) +
    facet_wrap(~product, nrow = 1) +
    scale_colour_manual(values = c(Australia = "#00205B", `United States` = "#D2232A")) +
    scale_y_continuous(labels = percent, limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous(breaks = seq(min(ct$year), ct_last, 2)) +
    labs(title = "Reliance on China for solar panels, batteries and electric cars",
         subtitle = sprintf("China's share of imports by value, %d–%d", min(ct$year), ct_last),
         x = NULL, y = NULL,
         caption = paste0("Sources: UN Comtrade (Australia); U.S. Census Bureau (U.S.).\n",
                          "Notes: Solar before 2022 is HS 854140, which also includes LEDs and other light-sensitive devices; from 2022 HS 854142-43 ",
                          "(cells and panels only). Lithium-ion batteries HS 850760; electric cars HS 870380 (battery electric).")) +
    theme_csis() +
    theme(legend.position = "top", legend.justification = "left", strip.text = element_text(face = "bold", size = 11),
          panel.grid.major.x = element_blank())
  ggsave("08_clean_tech_imports_china.png", p_ct, width = 10, height = 5, dpi = 150, bg = "white")
  ct_l <- ct |> filter(year == ct_last)
  g_ct <- function(c, p) percent(ct_l$share[ct_l$country == c & ct_l$product == p], accuracy = 1)
  ct_note <- sprintf(
    "In %d China supplied %s of Australia's imports of solar cells and panels, %s of its lithium-ion batteries and %s of its electric cars; for the U.S. the figures were %s, %s and %s.",
    ct_last, g_ct("Australia", "Solar cells and panels"), g_ct("Australia", "Lithium-ion batteries"), g_ct("Australia", "Electric cars"),
    g_ct("United States", "Solar cells and panels"), g_ct("United States", "Lithium-ion batteries"), g_ct("United States", "Electric cars"))
  message(ct_note)
}


# ------------------------------------------------------------------------------
# 8. Tidy exports
# ------------------------------------------------------------------------------

write_csv(gen_fy,     "australia_generation_fy_tidy.csv")
write_csv(gen_cy,     "australia_generation_cy_tidy.csv")
write_csv(state_2025, "australia_generation_state_2025.csv")

message("Done - five charts, five chart CSVs and three tidy CSVs written to ", getwd())

# ------------------------------------------------------------------------------
# 9. CHART 6 - cost of new solar and wind, Australia vs United States
#
# So what: levelized cost of electricity (LCOE) is the lifetime cost of a new
# plant per MWh, so it compares the two countries on the same basis whatever
# their market design. IRENA's figures exclude subsidies and tax credits.
#
# Data: IRENA (2026), Renewable Power Generation Costs in 2025, data file.
#   Fig 2.11 - onshore wind weighted-average LCOE by country, 2010-2025
#   Fig. 3.7 - utility-scale solar PV weighted-average LCOE by country, 2010-2025
# Values are 2025 US$/MWh (the onshore wind sheet's axis label says USD/kWh,
# but its numbers are per MWh, matching the report's text).
# IRENA publishes a new edition each July at a new address: when it does,
# paste the new data-file link into IRENA_URL and check the two sheet names.
# The file sits behind a download form on the report page; the direct link
# below still downloads it.
# ------------------------------------------------------------------------------

IRENA_URL  <- paste0("https://www.irena.org/-/media/Files/IRENA/Agency/Publication/",
                     "2026/Jul/IRENA_TEC_RPGC_in_2025_data_file_2026.xlsx")
irena_path <- basename(IRENA_URL)
BROWSER_UA <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
                    "(KHTML, like Gecko) Chrome/124.0 Safari/537.36")

if (!file.exists(irena_path)) {
  message("Downloading ", irena_path)
  httr2::request(IRENA_URL) |>
    httr2::req_user_agent(BROWSER_UA) |>
    httr2::req_timeout(300) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform(path = irena_path)
}
stopifnot(identical(readBin(irena_path, "raw", 2), charToRaw("PK")))   # a real .xlsx, not a block page

# Each sheet has a header row of years 2010-2025 with country names in the
# column to the left of the first year.
read_irena_lcoe <- function(sheet, technology) {
  raw <- read_excel(irena_path, sheet = sheet, col_names = FALSE,
                    col_types = "text", .name_repair = "minimal")
  raw <- as.data.frame(raw)
  hdr <- which(apply(raw, 1, function(r) sum(grepl("^20[0-9]{2}$", r)) >= 10))[1]
  yr_cols <- which(grepl("^20[0-9]{2}$", unlist(raw[hdr, ])))
  yr_cols <- yr_cols[c(TRUE, diff(yr_cols) == 1)]                 # first run of years only
  ctry_col <- min(yr_cols) - 1
  out <- raw[(hdr + 1):nrow(raw), c(ctry_col, yr_cols)]
  names(out) <- c("country", unlist(raw[hdr, yr_cols]))
  out |>
    filter(country %in% c("Australia", "United States")) |>
    pivot_longer(-country, names_to = "year", values_to = "lcoe") |>
    mutate(year = as.integer(year), lcoe = as.numeric(lcoe), technology = technology) |>
    filter(!is.na(lcoe))
}

lcoe <- bind_rows(
  read_irena_lcoe("Fig. 3.7", "Utility-scale solar PV"),
  read_irena_lcoe("Fig 2.11", "Onshore wind")
) |>
  mutate(technology = factor(technology, levels = c("Utility-scale solar PV", "Onshore wind")))

stopifnot(nrow(lcoe) > 40, n_distinct(lcoe$country) == 2)

lcoe_latest <- lcoe |> group_by(technology, country) |> slice_max(year, n = 1) |> ungroup()
lcoe_yr     <- max(lcoe_latest$year)

au_us_cols <- c("Australia" = "#00205B", "United States" = "#D2232A")   # as on every page
LINE_W <- 1   # line width on both price and cost charts

# From 2015: the 2010-12 solar costs (up to $490/MWh) would flatten the recent comparison.
lcoe_plot <- lcoe |> filter(year >= 2015)

# Drawn twice. IRENA's figures are in constant (real) US$ of lcoe_yr, so the
# A$ version converts every year at that one year's average rate.
lcoe_rate <- fx_calendar_year(lcoe_yr)
make_p6 <- function(cur) {
conv <- if (cur == "AUD") 1 / lcoe_rate else 1
lcoe_plot   <- lcoe_plot   |> mutate(lcoe = lcoe * conv)
lcoe_latest <- lcoe_latest |> mutate(lcoe = lcoe * conv)
ggplot(lcoe_plot, aes(year, lcoe, colour = country)) +
  geom_line(linewidth = LINE_W) +
  geom_point(size = 1.4) +
  # Each line named at its end, with its latest cost, instead of a legend
  ggrepel::geom_text_repel(data = lcoe_latest,
                           aes(label = paste0(if_else(country == "United States", "U.S.", as.character(country)),
                                              " ", cur_sym[[cur]], round(lcoe))),
                           nudge_x = 0.3, direction = "y", hjust = 0, size = 3.3, xlim = c(lcoe_yr + 0.3, NA),   # labels stay right of the line ends
                            fontface = "bold", family = "source_sans",
                           segment.colour = NA, min.segment.length = Inf, show.legend = FALSE) +
  facet_wrap(~ technology) +
  scale_colour_manual(values = au_us_cols, guide = "none") +
  scale_y_continuous(labels = label_dollar(prefix = cur_sym[[cur]]), limits = c(0, NA),
                     expand = expansion(c(0, 0.05))) +
  scale_x_continuous(breaks = seq(2015, 2025, 2), expand = expansion(mult = c(0.02, 0.45))) +   # room for the end labels
  labs(
    title    = "Cost of new solar and wind, Australia and the United States",
    subtitle = sprintf(paste0("Weighted-average levelized cost of electricity for new projects, ",
                              "%d %s per MWh, 2015–%d"), lcoe_yr, cur_sym[[cur]], lcoe_yr),
    x = NULL, y = NULL,
    caption  = paste0("Source: IRENA (2026), Renewable Power Generation Costs in 2025, data file ",
                      "(Fig. 2.11 and Fig. 3.7).\nLCOE excludes subsidies and other support.",
                      if (cur == "AUD") sprintf("
Converted to A$ at the RBA's %d average exchange rate.", lcoe_yr) else "")
  ) +
  theme_csis() +
  theme(strip.text = element_text(face = "bold", size = 10))
}

save_both(make_p6, "06_lcoe_au_us.png",
          function(p, f) ggsave(f, p, width = 9, height = 5, dpi = 150, bg = "white"))
p6 <- make_p6("USD")   # the chart CSV below keeps IRENA's own US$ figures

export_chart_csv(p6, "06_lcoe_au_us.csv",
                 y_format = "number", chart_type = "Faceted line with point markers",
                 extra = c("technology", "country"),
                 notes = paste0("Values are ", lcoe_yr, " US$ per MWh (not counts). Facets by technology. ",
                                "Colors: Australia #eda100; United States #00205B. ",
                                "Latest value labeled at the end of each line."))


# ------------------------------------------------------------------------------
# 10. CHART 7 - wholesale electricity prices, Australia vs California and Texas
#
# So what: spot prices are what generators earn and what large users pay
# before network charges, so they are the closest like-for-like measure of
# power costs between the two countries.
#
# Data (all free, no key needed; raw files are kept in Energy/raw/):
#   - AEMO aggregated price and demand data: regional reference price (RRP)
#     for each 5-minute dispatch interval
#   - CAISO OASIS: hourly day-ahead price (LMP) at the SP15 trading hub
#     (Southern California)
#   - RBA table F11: monthly A$/US$ exchange rate (end of month)
#
#   - ERCOT: 15-minute real-time price at the Texas hub average, from its
#     yearly historical price files (see the ERCOT notes below)
#
# Texas (ERCOT) is the closest US match to the NEM: an energy-only market
# with lots of wind and solar. PJM's data needs a registered API key, so it
# is not included. Each region's price is a simple time-weighted average
# of all intervals, so all markets are averaged the same way (AEMO's own
# headline figures are demand-weighted; those are in the tidy CSV as vwa_aud).
#
# OASIS only keeps about 39 months of hourly prices, so the chart starts at
# PRICE_START. Downloaded months are kept in Energy/raw/ and never fetched
# again, so the series grows with each refresh: do not delete that folder.
# Complete quarters only.
# ------------------------------------------------------------------------------

PRICE_START <- as.Date("2023-07-01")
last_month  <- seq(as.Date(format(Sys.Date(), "%Y-%m-01")), by = "-1 month", length.out = 2)[2]
NEM_REGIONS <- c(NSW = "NSW1", QLD = "QLD1", VIC = "VIC1", SA = "SA1")
dir.create(file.path("raw", "aemo"),  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("raw", "caiso"), recursive = TRUE, showWarnings = FALSE)

# Downloads url to dest unless a good copy is already there (and, if
# max_age_days is set, younger than that).
fetch_cached <- function(url, dest, check = function(p) file.size(p) > 1000,
                         max_age_days = Inf) {
  fresh <- file.exists(dest) &&
    difftime(Sys.time(), file.mtime(dest), units = "days") < max_age_days
  if (fresh && check(dest)) return(dest)
  httr2::request(url) |>
    httr2::req_user_agent(BROWSER_UA) |>
    httr2::req_timeout(180) |>
    httr2::req_retry(max_tries = 4, backoff = function(i) 5 * i) |>
    httr2::req_perform(path = dest)
  if (!check(dest)) stop("Download did not return data: ", url)
  dest
}

price_months <- seq(PRICE_START, last_month, by = "1 month")

# --- AEMO: monthly summaries of each region's 5-minute dispatch prices ---
nem_monthly <- expand_grid(month_start = price_months, region = names(NEM_REGIONS)) |>
  purrr::pmap(function(month_start, region) {
    file <- sprintf("PRICE_AND_DEMAND_%s_%s.csv", format(month_start, "%Y%m"), NEM_REGIONS[[region]])
    path <- fetch_cached(paste0("https://www.aemo.com.au/aemo/data/nem/priceanddemand/", file),
                         file.path("raw", "aemo", file))
    d <- read_csv(path, show_col_types = FALSE, col_select = c(TOTALDEMAND, RRP))
    tibble(month_start, region,
           minutes = nrow(d) * 5,
           twa_aud = mean(d$RRP),
           vwa_aud = sum(d$RRP * d$TOTALDEMAND) / sum(d$TOTALDEMAND))
  }) |>
  purrr::list_rbind()

# --- CAISO: hourly day-ahead SP15 prices, one request per month ---
# Requests run from local (Pacific) midnight to midnight: OASIS rejects
# windows over 31 days, which a UTC window crossing daylight saving can be.
# OASIS allows one request every few seconds, so new months are fetched slowly.
caiso_monthly <- tibble(month_start = price_months) |>
  purrr::pmap(function(month_start) {
    next_month <- seq(month_start, by = "1 month", length.out = 2)[2]
    utc_stamp  <- function(d) format(as.POSIXct(format(d), tz = "America/Los_Angeles"),
                                     "%Y%m%dT%H:%M-0000", tz = "UTC")
    dest <- file.path("raw", "caiso", sprintf("SP15_DAM_%s.zip", format(month_start, "%Y%m")))
    if (!file.exists(dest)) Sys.sleep(5)
    url <- paste0("https://oasis.caiso.com/oasisapi/SingleZip?queryname=PRC_LMP",
                  "&market_run_id=DAM&version=12&node=TH_SP15_GEN-APND",
                  "&startdatetime=", utc_stamp(month_start),
                  "&enddatetime=",   utc_stamp(next_month),
                  "&resultformat=6")
    fetch_cached(url, dest, check = function(p) {
      file.size(p) > 1000 && any(grepl("\\.csv$", utils::unzip(p, list = TRUE)$Name))
    })
    csv <- utils::unzip(dest, list = TRUE)$Name[1]
    d <- read_csv(unz(dest, csv), show_col_types = FALSE) |>
      filter(XML_DATA_ITEM == "LMP_PRC",
             format(as.Date(OPR_DT), "%Y-%m") == format(month_start, "%Y-%m"))
    tibble(month_start, minutes = nrow(d) * 60, usd = mean(d$MW))   # MW column holds the price
  }) |>
  purrr::list_rbind()

# --- ERCOT (Texas): 15-minute real-time prices at the hub average (HB_HUBAVG) ---
# Source: ERCOT "Historical RTM Load Zone and Hub Prices" (NP6-785-ER), one
# zip per year, free with no login:
#   https://www.ercot.com/mp/data-products/data-product-details?id=NP6-785-ER
# The script reads ERCOT's file list and downloads each year it needs into
# Energy/raw/ercot/. Past years are kept and not fetched again; the current
# year's file, which ERCOT updates daily, is re-fetched when over a week old.
# ercot.com refuses requests that don't look like they come from its own page,
# hence the Referer header. If the download ever fails, save the yearly zips
# from the page above into Energy/raw/ercot/ by hand and the script uses those.
# If there is no ERCOT data at all, Texas is left off the chart.

ERCOT_HUB   <- "HB_HUBAVG"
ERCOT_DIR   <- file.path("raw", "ercot")
ERCOT_PAGE  <- "https://www.ercot.com/mp/data-products/data-product-details?id=NP6-785-ER"
dir.create(ERCOT_DIR, recursive = TRUE, showWarnings = FALSE)

ercot_request <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent(BROWSER_UA) |>
    httr2::req_headers(Referer = ERCOT_PAGE) |>
    httr2::req_timeout(300) |>
    httr2::req_retry(max_tries = 3, backoff = function(i) 5 * i)
}

# Download the yearly zips covering PRICE_START to last month
fetch_ercot_files <- function() {
  docs <- ercot_request("https://www.ercot.com/misapp/servlets/IceDocListJsonWS?reportTypeId=13061") |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>                 # ERCOT sends no content type, so parse the text
    jsonlite::fromJSON()
  docs <- docs$ListDocsByRptTypeRes$DocumentList$Document
  years <- as.integer(format(PRICE_START, "%Y")):as.integer(format(last_month, "%Y"))
  for (y in years) {
    doc <- docs[docs$FriendlyName == paste0("RTMLZHBSPP_", y), ]
    if (nrow(doc) == 0) { message("ERCOT: no file listed for ", y); next }
    dest <- file.path(ERCOT_DIR, paste0("RTMLZHBSPP_", y, ".zip"))
    current_year <- y == as.integer(format(Sys.Date(), "%Y"))
    stale <- !file.exists(dest) ||
      (current_year && difftime(Sys.time(), file.mtime(dest), units = "days") > 7)
    if (!stale) next
    message("ERCOT: downloading RTMLZHBSPP_", y)
    ercot_request(paste0("https://www.ercot.com/misdownload/servlets/mirDownload?doclookupId=",
                         doc$DocID[1])) |>
      httr2::req_perform(path = dest)
    if (!identical(readBin(dest, "raw", 2), charToRaw("PK"))) {
      unlink(dest); stop("ERCOT returned something other than a zip for ", y)
    }
  }
}

# One row per 15-minute interval at the hub, from every workbook in the folder
read_ercot_files <- function() {
  for (z in list.files(ERCOT_DIR, "\\.zip$", full.names = TRUE))
    utils::unzip(z, exdir = ERCOT_DIR, overwrite = TRUE)
  books <- list.files(ERCOT_DIR, "\\.xlsx$", full.names = TRUE)
  if (length(books) == 0) return(NULL)
  purrr::map(books, function(b) {
    purrr::map(excel_sheets(b), function(sh) {
      d <- read_excel(b, sheet = sh)
      names(d) <- str_replace_all(str_to_lower(names(d)), "[^a-z]+", "_")
      if (!all(c("delivery_date", "delivery_hour", "delivery_interval",
                 "settlement_point_name", "settlement_point_price") %in% names(d)))
        return(NULL)
      if (!"repeated_hour_flag" %in% names(d)) d$repeated_hour_flag <- "N"
      d |>
        filter(settlement_point_name == ERCOT_HUB) |>
        transmute(date     = as.Date(delivery_date, tryFormats = c("%m/%d/%Y", "%Y-%m-%d")),
                  hour     = as.integer(delivery_hour),
                  interval = as.integer(delivery_interval),
                  repeated = as.character(repeated_hour_flag),
                  price    = as.numeric(settlement_point_price))
    }) |> purrr::list_rbind()
  }) |> purrr::list_rbind() |>
    distinct(date, hour, interval, repeated, .keep_all = TRUE)   # same year saved twice
}

tryCatch(fetch_ercot_files(),
         error = function(e) message("ERCOT download failed (", conditionMessage(e),
                                     "); using any files already in ", ERCOT_DIR))
ercot_raw <- read_ercot_files()

ercot_monthly <- if (!is.null(ercot_raw) && nrow(ercot_raw) > 0) {
  ercot_raw |>
    mutate(month_start = as.Date(format(date, "%Y-%m-01"))) |>
    filter(month_start %in% price_months) |>
    group_by(month_start) |>
    summarise(minutes = n() * 15, usd = mean(price), days = n_distinct(date), .groups = "drop") |>
    # whole months only (the current-year file stops part-way through a month)
    filter(days == vapply(month_start, function(m)
      as.integer(format(seq(m, by = "1 month", length.out = 2)[2] - 1, "%d")), integer(1))) |>
    select(-days)
} else NULL

has_ercot <- !is.null(ercot_monthly) && nrow(ercot_monthly) > 0
if (has_ercot) {
  message("ERCOT: ", nrow(ercot_monthly), " months of ", ERCOT_HUB, " prices")
} else {
  message("ERCOT: no data found, so Texas is left off the wholesale price chart.")
}

# --- Exchange rate: each month's average of the RBA's daily A$/US$ rates ---
# (from '0. Exchange rates.R', the same rule as every other chart's toggle)
fx <- tibble(month_start = price_months, aud_usd = fx_month(price_months)) |>
  filter(!is.na(aud_usd))

missing_fx <- setdiff(price_months, fx$month_start)
if (length(missing_fx) > 0)
  warning("No RBA exchange rate yet for ", paste(format(as.Date(missing_fx, origin = "1970-01-01"),
                                                        "%b %Y"), collapse = ", "),
          "; those months are left out.")

# --- Quarterly averages in US$/MWh and A$/MWh ---
price_qtr <- bind_rows(
  nem_monthly |>
    inner_join(fx, by = "month_start") |>
    mutate(usd = twa_aud * aud_usd, aud = twa_aud),
  caiso_monthly |> mutate(region = "California (SP15)") |>
    inner_join(fx, by = "month_start") |> mutate(aud = usd / aud_usd),
  if (has_ercot) ercot_monthly |> mutate(region = "Texas (ERCOT)") |>
    inner_join(fx, by = "month_start") |> mutate(aud = usd / aud_usd)
) |>
  mutate(quarter = as.Date(paste0(format(month_start, "%Y-"),
                                  sprintf("%02d", (as.integer(format(month_start, "%m")) - 1) %/% 3 * 3 + 1),
                                  "-01"))) |>
  group_by(region, quarter) |>
  summarise(n_months  = n(),
            price_usd = weighted.mean(usd, minutes),
            price_aud = weighted.mean(aud, minutes),
            twa_aud   = if (all(is.na(twa_aud))) NA_real_ else weighted.mean(twa_aud, minutes),
            vwa_aud   = if (all(is.na(vwa_aud))) NA_real_ else weighted.mean(vwa_aud, minutes),
            .groups = "drop") |>
  filter(n_months == 3) |>
  mutate(region = factor(region, levels = intersect(c("Texas (ERCOT)", "California (SP15)", names(NEM_REGIONS)),
                                                   unique(region))))

stopifnot(n_distinct(price_qtr$region) == 5 + has_ercot)
us_markets <- c("Texas (ERCOT)", "California (SP15)")

qtr_label    <- function(d) paste0("Q", (as.integer(format(d, "%m")) - 1) %/% 3 + 1, " ", format(d, "%Y"))
price_latest <- price_qtr |> filter(quarter == max(quarter))
price_q      <- max(price_latest$quarter)

# Two panels on the same scale: the four NEM regions (blues) on the left,
# the two US markets (reds) on the right.
price_cols <- c("Texas (ERCOT)" = "#D2232A", "California (SP15)" = "#F08A8E",
                NSW = "#00205B", VIC = "#5B8BD0", QLD = "#0098A8", SA = "#9DB9E3")
CHART_REGIONS <- names(price_cols)
price_panel <- function(region) factor(if_else(region %in% names(NEM_REGIONS), "Australian NEM regions", "United States"),
                                       levels = c("Australian NEM regions", "United States"))

# Drawn twice: US$ (NEM converted) and A$ (US markets converted).
make_p7 <- function(cur) {
price_qtr    <- price_qtr    |> filter(region %in% CHART_REGIONS) |>
  mutate(price = if (cur == "USD") price_usd else price_aud, region = droplevels(region),
         panel = price_panel(region))
price_latest <- price_latest |> filter(region %in% CHART_REGIONS) |>
  mutate(price = if (cur == "USD") price_usd else price_aud, panel = price_panel(region))
ggplot(price_qtr, aes(quarter, price, colour = region)) +
  geom_line(linewidth = LINE_W) +
  geom_point(size = 1.4) +
  # Each line named at its end, with its latest price, instead of a legend
  ggrepel::geom_text_repel(data = price_latest,
                           aes(label = paste0(sub(" \\(.*", "", region), " ", cur_sym[[cur]], round(price))),
                           nudge_x = 25, direction = "y", hjust = 0, size = 3.3, fontface = "bold", family = "source_sans",
                           nudge_y = ifelse(grepl("^California", price_latest$region), 0.1 * max(price_qtr$price, na.rm = TRUE), 0),
                           xlim = c(as.numeric(price_q) + 45, NA),   # labels stay right of the line ends, stacked
                           segment.colour = NA, min.segment.length = Inf, show.legend = FALSE) +
  facet_wrap(~ panel, nrow = 1) +                                  # same y scale in both panels
  scale_colour_manual(values = price_cols, guide = "none") +
  scale_y_continuous(labels = label_dollar(prefix = cur_sym[[cur]]), limits = c(0, NA),
                     expand = expansion(c(0, 0.05))) +
  scale_x_date(breaks = seq(price_q, min(price_qtr$quarter), by = "-1 year"),
               labels = qtr_label, expand = expansion(mult = c(0.02, 0.32))) +
  labs(
    hover_suffix = "/MWh",   # unit for the hover pop-ups (theme/chart-footer.html)
    title    = if (has_ercot) "Wholesale electricity prices, Australian NEM regions, California and Texas (ERCOT)"
               else "Wholesale electricity prices, Australian NEM regions and California",
    subtitle = sprintf(paste0("Average wholesale price, %s per MWh, calendar quarters, %s\u2013%s.
",
                              "NEM and Texas: real-time prices; California: day-ahead prices."),
                       cur_sym[[cur]], qtr_label(min(price_qtr$quarter)), qtr_label(price_q)),
    x = NULL, y = NULL,
    caption  = paste0("Sources: AEMO aggregated price and demand data (5-minute regional prices); ",
                      "CAISO OASIS (hourly day-ahead price, SP15 hub);\n",
                      if (has_ercot) "ERCOT (15-minute real-time price, hub average HB_HUBAVG); " else "",
                      "RBA daily A$/US$ exchange rates, averaged for each month to convert that month.\n",
                      "Simple time-weighted averages of all intervals.",
                      if (has_ercot) "" else " Texas (ERCOT) not shown: no ERCOT data available on this run.")
  ) +
  theme_csis() +
  theme(strip.text = element_text(face = "bold", hjust = 0.5))
}

save_both(make_p7, "07_wholesale_prices_au_us.png",
          function(p, f) ggsave(f, p, width = 9, height = 5.2, dpi = 150, bg = "white"))
p7 <- make_p7("USD")   # the chart CSV below stays in US$

export_chart_csv(p7, "07_wholesale_prices_au_us.csv",
                 y_format = "number", chart_type = "Line with point markers",
                 extra = "region",
                 notes = paste0("Values are US$ per MWh (not counts). quarter = first day of the quarter. ",
                                "Two panels on one scale: NEM regions (NSW #00205B, VIC #5B8BD0, QLD #0098A8, SA #9DB9E3) ",
                                "and U.S. markets (Texas #D2232A, California #F08A8E). Each line labeled at its end with its latest value."))

write_csv(lcoe |> arrange(technology, country, year), "lcoe_au_us_tidy.csv")
write_csv(price_qtr |> arrange(region, quarter), "wholesale_prices_au_us_tidy.csv")

# Takeaways under each chart
local({
  en <- function(file) file.path("Energy", file)
  coal <- gen_fy |> filter(region == "AUS") |> group_by(year) |>
    summarise(coal = sum(gwh[grepl("coal", fuel, ignore.case = TRUE)]) / sum(gwh), .groups = "drop")
  pk <- coal |> slice_max(coal, n = 1); lt <- coal |> slice_max(year, n = 1)
  fy <- function(y) paste0(y, "-", sprintf("%02d", (y + 1) %% 100))
  us_c <- mix_ts |> filter(country == "United States") |> group_by(year) |>
    summarise(coal = sum(share[fuel == "Coal"]), .groups = "drop")
  takeaway(en("01_fuel_mix_timeseries.png"), sprintf(
    "Coal's share of electricity fell from a peak of %.0f%% in %s to %.0f%% in %s in Australia (financial years), and from %.0f%% in %d to %.0f%% in %d in the U.S. (calendar years).",
    100 * pk$coal, fy(pk$year), 100 * lt$coal, fy(lt$year),
    100 * max(us_c$coal), us_c$year[which.max(us_c$coal)], 100 * us_c$coal[us_c$year == us_last], us_last))
  takeaway(en("04_renewables_vs_target.png"), sprintf(
    "Renewables supplied %.0f%% of Australian electricity in the %s financial year; reaching 82%% by 2030 needs about %.0f percentage points a year.",
    100 * last_pt$share, fy(last_pt$year - 1), 100 * (0.82 - last_pt$share) / (2030 - last_pt$year)))
  cf <- mix_ts |> filter(fuel %in% CARBON_FREE) |> group_by(country) |> filter(year == max(year)) |>
    summarise(year = first(year), nuclear = sum(share[fuel == "Nuclear"]), share = sum(share), .groups = "drop")   # nuclear first: share is replaced by its total
  takeaway(en("05_carbon_free_by_technology.png"), sprintf(
    "Carbon-free sources supplied %.0f%% of Australian electricity in the %s financial year and %.0f%% of U.S. electricity in calendar %d, including %.0f%% from nuclear.",
    100 * cf$share[cf$country == "Australia"], fy(cf$year[cf$country == "Australia"] - 1),
    100 * cf$share[cf$country == "United States"], cf$year[cf$country == "United States"],
    100 * cf$nuclear[cf$country == "United States"]))
  l7 <- function(cur) {
    p <- price_latest |> mutate(v = if (cur == "USD") price_usd else price_aud)
    g <- function(r) p$v[p$region == r]
    sprintf("In %s wholesale power averaged %s%.0f/MWh in NSW and %s%.0f in Victoria, against %s%.0f in Texas and %s%.0f in California.",
            qtr_label(price_q), cur_sym[[cur]], g("NSW"), cur_sym[[cur]], g("VIC"),
            cur_sym[[cur]], if (has_ercot) g("Texas (ERCOT)") else NA, cur_sym[[cur]], g("California (SP15)"))
  }
  takeaway(en("07_wholesale_prices_au_us.png"), l7("AUD"), l7("USD"))
  sm <- state_mix |> distinct(group, place, renew) |> filter(!place %in% c("Australia", "United States"))
  au_s <- sm |> filter(grepl("^Australia", group)) |> arrange(desc(renew))
  takeaway(en("02_state_fuel_mix.png"), sprintf(
    "Renewables supplied %.0f%% of electricity in New South Wales and %.0f%% in Victoria in calendar 2025, where most Australian data centers are, against %.0f%% in Virginia in 2024.",
    100 * au_s$renew[au_s$place == "NSW"], 100 * au_s$renew[au_s$place == "VIC"],
    100 * sm$renew[sm$place == "Virginia"]))
  cm <- comparison |> group_by(country) |>
    summarise(coal = sum(share[fuel == "Coal"]), gas = sum(share[fuel == "Gas"]),
              ren = sum(share[fuel %in% CMP_RENEW]), nuc = sum(share[fuel == "Nuclear"]))
  g <- function(c, v) 100 * cm[[v]][cm$country == c]
  takeaway(en("03_primary_vs_electricity.png"), sprintf(
    "In calendar %d coal made up %.0f%% of Australia's electricity and %.0f%% of the U.S.'s; gas %.0f%% and %.0f%%; renewables %.0f%% and %.0f%% (plus %.0f%% nuclear in the U.S.).",
    mix_year, g("Australia", "coal"), g("United States", "coal"), g("Australia", "gas"), g("United States", "gas"),
    g("Australia", "ren"), g("United States", "ren"), g("United States", "nuc")))
  rl <- renew_last |> mutate(k = c("au", "us")[as.integer(series)])
  v <- function(k) 100 * rl$share[rl$k == k]
  takeaway(en("09_renewable_share_au_us.png"), sprintf(
    "Renewables supplied %.0f%% of Australia's electricity in the %s financial year and %.0f%% of the U.S.'s in calendar %d.",
    v("au"), fy(rl$year[rl$k == "au"] - 1), v("us"), rl$year[rl$k == "us"]))
  takeaway(en("08_energy_security_oil.png"), per_fuel_note)
  takeaway(en("08_self_sufficiency_by_fuel.png"), ss_note)
  if (exists("ct_note")) takeaway(en("08_clean_tech_imports_china.png"), ct_note)
  l6 <- function(cur) {
    k <- if (cur == "AUD") 1 / lcoe_rate else 1
    v <- function(t, c) lcoe_latest$lcoe[lcoe_latest$technology == t & lcoe_latest$country == c] * k
    sprintf("New utility-scale solar cost %s%.0f/MWh in Australia and %s%.0f in the U.S. in %d; onshore wind %s%.0f and %s%.0f.",
            cur_sym[[cur]], v("Utility-scale solar PV", "Australia"), cur_sym[[cur]], v("Utility-scale solar PV", "United States"),
            lcoe_yr, cur_sym[[cur]], v("Onshore wind", "Australia"), cur_sym[[cur]], v("Onshore wind", "United States"))
  }
  takeaway(en("06_lcoe_au_us.png"), l6("AUD"), l6("USD"))
})

message("Done - charts 6 and 7 written to ", getwd())