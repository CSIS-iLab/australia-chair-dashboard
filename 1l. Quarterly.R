rm(list = ls())
# The dashboard folder is set once, in section 1 of '1. Refresh all data.R'.
# To run this script on its own, run that section first.
if (Sys.getenv("DASHBOARD_DIR") == "")
  stop("Dashboard folder not set. Run section 1 of '1. Refresh all data.R' first.")
DASHBOARD_DIR <- Sys.getenv("DASHBOARD_DIR")
setwd(DASHBOARD_DIR)
source(file.path(DASHBOARD_DIR, "0. Exchange rates.R"))   # A$/US$ toggle

# ==============================================================================
# Quarterly charts: new information every quarter
#
# Added Nov 2026 so that each quarterly update (timed to the ABS national
# accounts release, e.g. 3 December 2026 for the September quarter) brings new
# data to the dashboard. Each chart covers about the last five years.
#
#   1. Trade/us_au_trade_quarterly.png         Australia's goods trade with the
#        U.S. and the U.S. surplus, by quarter (ABS 5368.0, monthly, summed)
#   2. Economy/gdp_growth_quarterly_au_us.png  Real GDP growth, quarter on
#        quarter, Australia and the U.S. (ABS 5206.0; BEA NIPA Table 1.1.6)
#   3. Economy/terms_of_trade_quarterly.png    Australia's terms of trade
#        (ABS 5206.0)
#   5. Trade/us_au_services_quarterly.png      Australia's services trade with
#        the U.S. by quarter (BEA, Table 1.3); needs the BEA key, like 4
#   4. Investment/us_au_direct_investment_quarterly.png  U.S.-Australia
#        direct investment flows by quarter (BEA international transactions,
#        Table 1.3). Needs a free BEA API key (see section 4); skipped
#        without one.
#
# Everything is downloaded; nothing is prepared by hand.
# ==============================================================================

suppressPackageStartupMessages({
  library(readabs)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(showtext)
})
source(file.path(DASHBOARD_DIR, "0. Chart data.R"))   # CSV, hover map and house style for each chart

YEARS_SHOWN <- 5                       # about five years of quarters
DPI <- 150
font_add_google("Source Sans 3", "source_sans")
showtext_auto()
showtext_opts(dpi = DPI)

RAW_DIR <- file.path(DASHBOARD_DIR, "Economy", "raw")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

csis_colours <- c(navy = "#00205B", red = "#D2232A", teal = "#0098A8",
                  gold = "#F2A900", grey = "#565A5C", ltgrey = "#B1B3B3")
au_us_colours <- c("Australia" = "#00205B", "United States" = "#D2232A")

theme_csis <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "source_sans") %+replace%
    theme(
      plot.title    = element_text(face = "bold", size = rel(1.25), hjust = 0,
                                   colour = csis_colours[["navy"]], margin = margin(b = 4)),
      plot.subtitle = element_text(hjust = 0, colour = csis_colours[["grey"]], margin = margin(b = 8)),
      plot.caption  = element_text(size = rel(0.75), hjust = 0, colour = csis_colours[["grey"]],
                                   margin = margin(t = 8)),
      plot.title.position = "plot", plot.caption.position = "plot",
      panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "#E6E7E8", linewidth = 0.4),
      axis.line.x  = element_line(colour = csis_colours[["grey"]], linewidth = 0.4),
      axis.ticks.x = element_line(colour = csis_colours[["grey"]], linewidth = 0.4),
      axis.text    = element_text(colour = csis_colours[["grey"]]),
      legend.position = "top", legend.justification = "left", legend.title = element_blank(),
      complete = TRUE)
}
save_plot <- function(plot, file, width = 8, height = 4.5, dpi = DPI)
  ggsave(file, plot, width = width, height = height, dpi = dpi, bg = "white")

# Quarters are dated by their last month, as the ABS does (2026-09-01 = the
# September quarter), so the dashboard's "Data to" line reads "September 2026".
quarter_end <- function(d) {
  d <- as.Date(d)
  as.Date(sprintf("%s-%02d-01", format(d, "%Y"), ((as.integer(format(d, "%m")) - 1) %/% 3 + 1) * 3))
}
qtr_label <- function(d) paste0("Q", (as.integer(format(d, "%m")) - 1) %/% 3 + 1, " ", format(d, "%Y"))
# Axis: a label for every fourth quarter, ending on the latest one
qtr_breaks <- function(dates) { u <- sort(unique(dates)); rev(rev(u)[seq(1, length(u), by = 4)]) }
first_quarter_shown <- function(latest)
  seq(latest, by = "-3 months", length.out = 4 * YEARS_SHOWN)[4 * YEARS_SHOWN]
fmt_bn <- function(x, sym, accuracy = 1) ifelse(x < 0, paste0("-", sym, number(abs(x), accuracy = accuracy), "bn"),
                                                paste0(sym, number(x, accuracy = accuracy), "bn"))
# Axis labels in whole billions; the hover pop-ups ask for one decimal place
# (labs(hover_accuracy = 0.1), which '0. Chart data.R' passes in as accuracy)
lab_bn <- function(sym, accuracy = 1) function(x) fmt_bn(x, sym, accuracy)


# ---- 1. Quarterly goods trade with the U.S. -----------------------------------
# ABS International Trade in Goods (cat. 5368.0), Tables 14a (merchandise
# exports by country, FOB) and 14b (merchandise imports by country, customs
# value), monthly, $ million, original. Months are summed to quarters; only
# complete quarters are kept. ABS does not publish services trade by country
# each quarter, so this chart is goods only (the annual chart beside it on the
# dashboard has goods and services).

goods_m <- read_abs("5368.0", tables = c("14a", "14b"), path = RAW_DIR,
                    show_progress_bars = FALSE) |>
  filter(grepl("^United States of America", series)) |>
  mutate(flow = ifelse(grepl("14a", table_no) | grepl("xport", table_title), "au_exports", "us_exports")) |>
  select(date, flow, value)
stopifnot("ABS goods trade with the U.S. not found" = nrow(goods_m) > 0,
          setequal(unique(goods_m$flow), c("au_exports", "us_exports")))

goods_q <- goods_m |>
  mutate(quarter = quarter_end(date)) |>
  group_by(quarter, flow) |>
  summarise(value_bn = sum(value) / 1000, months = n(), .groups = "drop") |>
  filter(months == 3) |>
  pivot_wider(names_from = flow, values_from = value_bn) |>
  filter(!is.na(au_exports), !is.na(us_exports)) |>
  mutate(surplus = us_exports - au_exports) |>
  select(-months) |>
  arrange(quarter)
goods_q <- filter(goods_q, quarter >= first_quarter_shown(max(goods_q$quarter)))

flow_levels <- c("Australian exports to the U.S.", "Australian imports from the U.S.")
make_trade_q <- function(cur) {
  k <- if (cur == "USD") fx_quarter(goods_q$quarter) else 1
  sym <- cur_sym[[cur]]
  d <- goods_q |> mutate(au_exports = au_exports * k, us_exports = us_exports * k, surplus = surplus * k)
  flows <- bind_rows(
    d |> transmute(quarter, value = au_exports, series = flow_levels[1]),
    d |> transmute(quarter, value = us_exports, series = flow_levels[2])) |>
    mutate(series = factor(series, flow_levels))
  last <- d[nrow(d), ]
  ggplot() +
    geom_col(data = d, aes(quarter, surplus, fill = "U.S. goods trade surplus"), width = 62) +
    geom_hline(yintercept = 0, colour = csis_colours[["grey"]], linewidth = 0.5) +
    geom_line(data = flows, aes(quarter, value, colour = series), linewidth = 1) +
    geom_point(data = flows, aes(quarter, value, colour = series), size = 1.6) +
    scale_colour_manual(values = setNames(unname(csis_colours[c("navy", "red")]), flow_levels)) +
    scale_fill_manual(values = c("U.S. goods trade surplus" = "#F7D98B")) +
    scale_y_continuous(labels = lab_bn(sym), breaks = breaks_pretty(5)) +
    scale_x_date(breaks = qtr_breaks(d$quarter), labels = qtr_label,
                 expand = expansion(mult = c(0.02, 0.02))) +
    guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2)) +
    # Same title as the annual chart (Trade/us_au_trade_balance.png); the
    # subtitle says which trade and how often
    labs(hover_accuracy = 0.1, title = "Australia's trade with the United States",
         subtitle = "Goods, quarterly",
         x = NULL, y = NULL,
         caption = paste0(
           "Source: ABS, International Trade in Goods (cat. 5368.0), Tables 14a and 14b; monthly values summed to quarters.\n",
           "Notes: The ABS does not publish services trade by country each quarter; see the Services tab (BEA data). ",
           "U.S. surplus = Australian imports from the U.S. less Australian exports to the U.S. ",
           "Exports are free on board; imports are at customs value. Includes non-monetary gold, ",
           "which drove much of the rise in exports to the U.S. in 2025. Not seasonally adjusted.",
           if (cur == "USD") fx_note[["USD"]] else "")) +
    theme_csis() +
    theme(legend.box = "vertical", legend.box.just = "left",
          legend.spacing.y = unit(0, "pt"), legend.margin = margin(0, 0, 0, 0))
}
save_both(make_trade_q, "Trade/us_au_trade_quarterly.png", save_plot)
write.csv(goods_q |> mutate(usd_per_aud = fx_quarter(quarter),
                            surplus_usd = surplus * usd_per_aud),
          "Trade/us_au_trade_quarterly_tidy.csv", row.names = FALSE)


# ---- 2. Real GDP growth by quarter, Australia and the U.S. ---------------------
# Australia: ABS 5206.0, GDP, chain volume measures, seasonally adjusted
# (A2304402X). U.S.: BEA NIPA Table 1.1.6, real GDP, chained dollars,
# seasonally adjusted (A191RX). Growth is the % change on the previous quarter,
# not annualized, so the two are comparable (BEA's headline figure is
# annualized, about four times as large).

fetch <- function(url, dest, max_age_days = 7) {
  if (!file.exists(dest) || difftime(Sys.time(), file.mtime(dest), units = "days") > max_age_days) {
    message("Downloading ", basename(dest))
    utils::download.file(url, dest, mode = "wb", quiet = TRUE, headers = c(`User-Agent` = "Mozilla/5.0"))
  }
  dest
}
bea_nipa_q <- function(code) {
  f <- fetch("https://apps.bea.gov/national/Release/TXT/NipaDataQ.txt", file.path(RAW_DIR, "bea_NipaDataQ.txt"))
  d <- utils::read.csv(f, colClasses = "character", check.names = FALSE)
  names(d) <- c("code", "period", "value")
  d <- d[d$code == code, ]
  stopifnot("BEA series not found in NipaDataQ.txt" = nrow(d) > 0)
  data.frame(date  = as.Date(sprintf("%s-%02d-01", substr(d$period, 1, 4), as.integer(substr(d$period, 6, 6)) * 3)),
             value = as.numeric(gsub(",", "", d$value)))
}

na5206 <- read_abs("5206.0", tables = 1, path = RAW_DIR, show_progress_bars = FALSE)

gdp_q <- bind_rows(
  na5206 |> filter(series_id == "A2304402X") |> transmute(date = quarter_end(date), value, country = "Australia"),
  bea_nipa_q("A191RX") |> mutate(country = "United States")) |>
  arrange(country, date) |>
  group_by(country) |>
  mutate(growth = 100 * (value / lag(value) - 1)) |>
  ungroup() |>
  filter(!is.na(growth))
stopifnot(n_distinct(gdp_q$country) == 2)
# Same quarters for both: from five years before the latest Australian quarter
gdp_from <- first_quarter_shown(max(gdp_q$date[gdp_q$country == "Australia"]))
gdp_q <- filter(gdp_q, date >= gdp_from)

last_au <- gdp_q |> filter(country == "Australia") |> slice_max(date)
last_us <- gdp_q |> filter(country == "United States") |> slice_max(date)
p_gdp_q <- ggplot(gdp_q, aes(date, growth, fill = country)) +
  geom_hline(yintercept = 0, colour = csis_colours[["grey"]], linewidth = 0.4) +
  geom_col(position = position_dodge(width = 70), width = 66) +
  scale_fill_manual(values = au_us_colours) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.5), breaks = breaks_pretty(5)) +
  scale_x_date(breaks = qtr_breaks(gdp_q$date), labels = qtr_label, expand = expansion(mult = 0.02)) +
  labs(hover_accuracy = 0.1, title = "Quarterly real GDP growth, Australia and the U.S.",
       subtitle = sprintf("%% change on the previous quarter. Australia %+.1f%% in %s; U.S. %+.1f%% in %s",
                          last_au$growth, qtr_label(last_au$date), last_us$growth, qtr_label(last_us$date)),
       x = NULL, y = NULL,
       caption = paste0("Sources: ABS, Australian National Accounts (cat. no. 5206.0); U.S. BEA, NIPA Table 1.1.6.\n",
                        "Notes: Chain volume measures (Australia) and chained dollars (U.S.), seasonally adjusted. ",
                        "U.S. growth is not annualized, so it is about a quarter of BEA's headline rate.")) +
  theme_csis()
save_plot(p_gdp_q, "Economy/gdp_growth_quarterly_au_us.png")


# ---- 3. Australia's terms of trade --------------------------------------------
# ABS 5206.0 Table 1, terms of trade index, seasonally adjusted (A2304200A):
# the price of Australia's exports relative to the price of its imports.
tot <- na5206 |> filter(series_id == "A2304200A") |>
  transmute(date = quarter_end(date), value) |> arrange(date)
stopifnot("Terms of trade series not found" = nrow(tot) > 0)
# Reference year: the financial year whose quarters average 100
fy_of <- function(d) as.integer(format(d, "%Y")) + (as.integer(format(d, "%m")) >= 7)
ref <- tot |> mutate(fy = fy_of(date)) |> group_by(fy) |> filter(n() == 4) |>
  summarise(avg = mean(value)) |> slice_min(abs(avg - 100), n = 1)
ref_lab <- if (nrow(ref) && abs(ref$avg - 100) < 0.6) sprintf("%d-%02d = 100", ref$fy - 1, ref$fy %% 100) else "reference year = 100"
tot_shown <- filter(tot, date >= first_quarter_shown(max(tot$date)))
pk <- slice_max(tot_shown, value, n = 1)
lt <- tot_shown[nrow(tot_shown), ]
p_tot <- ggplot(tot_shown, aes(date, value)) +
  geom_line(colour = csis_colours[["navy"]], linewidth = 1) +
  geom_point(colour = csis_colours[["navy"]], size = 1.6) +
  geom_text(data = pk, aes(label = sprintf("Peak: %.1f, %s", value, qtr_label(date))),
            vjust = -1, size = 3.4, colour = csis_colours[["grey"]]) +
  geom_text(data = lt, aes(label = sprintf("%.1f", value)), hjust = -0.3, size = 3.6,
            fontface = "bold", colour = csis_colours[["navy"]]) +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.15))) +
  scale_x_date(breaks = qtr_breaks(tot_shown$date), labels = qtr_label,
               expand = expansion(mult = c(0.02, 0.09))) +
  labs(hover_accuracy = 0.1, title = "Australia's terms of trade",
       subtitle = sprintf("Export prices relative to import prices, index (%s). %s: %.1f, %+.1f%% on the previous quarter",
                          ref_lab, qtr_label(lt$date), lt$value,
                          100 * (lt$value / tot_shown$value[nrow(tot_shown) - 1] - 1)),
       x = NULL, y = NULL,
       caption = paste0("Source: ABS, Australian National Accounts (cat. no. 5206.0), Table 1.\n",
                        "Notes: Seasonally adjusted. A rise means a unit of exports buys more imports. ",
                        "Movements mainly reflect prices of Australia's commodity exports, such as iron ore, coal and gas.")) +
  theme_csis()
save_plot(p_tot, "Economy/terms_of_trade_quarterly.png")


# ---- 4. Direct investment flows with Australia (BEA) ---------------------------
# BEA international transactions accounts, Table 1.3 (expanded detail by area
# and country), Australia, quarterly, not seasonally adjusted, US$ million:
#   U.S. direct investment in Australia = DiInvAssets (net U.S. acquisition
#     of direct investment assets in Australia, directional basis)
#   Australian direct investment in the U.S. = DiInvLiabs (net U.S. incurrence
#     of direct investment liabilities to Australia, directional basis)
# These are flows (new investment in the quarter, net of withdrawals), from
# U.S. data. They differ from the ABS positions on the Investment page, which
# are stocks (the total held at 31 December), cover all types of investment,
# and are measured by the ABS: BEA counts by immediate counterpart country,
# and the two agencies differ in timing, coverage and revisions.
#
# The BEA API needs a free key: sign up at https://apps.bea.gov/API/signup/
# and save the key (one line) in Investment/raw/bea_api_key.txt, or set the
# environment variable BEA_API_KEY. Without a key this section is skipped and
# the dashboard keeps the last chart made.

bea_key <- Sys.getenv("BEA_API_KEY")
key_file <- file.path(DASHBOARD_DIR, "Investment", "raw", "bea_api_key.txt")
if (!nzchar(bea_key) && file.exists(key_file)) bea_key <- trimws(readLines(key_file, warn = FALSE)[1])

bea_api <- function(...) {
  q <- c(list(UserID = bea_key, ResultFormat = "JSON"), list(...))
  url <- paste0("https://apps.bea.gov/api/data/?",
                paste(names(q), vapply(q, function(v) utils::URLencode(as.character(v), reserved = TRUE), ""),
                      sep = "=", collapse = "&"))
  res <- jsonlite::fromJSON(url)$BEAAPI
  if (!is.null(res$Error)) stop("BEA API: ", paste(unlist(res$Error), collapse = " "))
  if (!is.null(res$Results$Error)) stop("BEA API: ", paste(unlist(res$Results$Error), collapse = " "))
  res$Results
}

if (!nzchar(bea_key)) {
  message("No BEA API key (", key_file, "): quarterly direct investment chart skipped.")
} else {
  # Directional basis: DiInvAssets = U.S. direct investment in Australia
  # (outward), DiInvLiabs = Australian direct investment in the U.S. (inward)
  ind_assets <- "DiInvAssets"
  ind_liab   <- "DiInvLiabs"
  message("BEA indicators: ", ind_assets, ", ", ind_liab)
  di <- bind_rows(lapply(c(ind_assets, ind_liab), function(i)
    bea_api(method = "GetData", DataSetName = "ITA", Indicator = i, AreaOrCountry = "Australia",
            Frequency = "QNSA", Year = "ALL")$Data)) |>
    transmute(indicator = Indicator,
              date = as.Date(sprintf("%s-%02d-01", substr(TimePeriod, 1, 4), as.integer(substr(TimePeriod, 6, 6)) * 3)),
              value_bn = suppressWarnings(as.numeric(gsub(",", "", DataValue))) / 1000) |>
    filter(!is.na(value_bn), !is.na(date)) |>
    mutate(series = ifelse(indicator == ind_assets, "U.S. direct investment in Australia",
                           "Australian direct investment in the U.S."))
  di <- filter(di, date >= first_quarter_shown(max(di$date)))
  di_levels <- c("U.S. direct investment in Australia", "Australian direct investment in the U.S.")
  di$series <- factor(di$series, di_levels)
  make_di <- function(cur) {
    sym <- cur_sym[[cur]]
    d <- if (cur == "AUD") mutate(di, value_bn = usd_to_aud(value_bn, fx_quarter(date))) else di
    tot <- d |> group_by(series) |> filter(date > max(date) - 365) |> summarise(v = sum(value_bn))
    ggplot(d, aes(date, value_bn, fill = series)) +
      geom_hline(yintercept = 0, colour = csis_colours[["grey"]], linewidth = 0.4) +
      geom_col(position = position_dodge(width = 70), width = 66) +
      scale_fill_manual(values = setNames(unname(csis_colours[c("red", "navy")]), di_levels)) +
      scale_y_continuous(labels = lab_bn(sym), breaks = breaks_pretty(5)) +
      scale_x_date(breaks = qtr_breaks(d$date), labels = qtr_label, expand = expansion(mult = 0.02)) +
      labs(hover_accuracy = 0.1, title = "U.S.-Australia direct investment flows, by quarter",
           subtitle = sprintf("New direct investment, net of withdrawals. Last four quarters: U.S. into Australia %s; Australia into the U.S. %s",
                              fmt_bn(tot$v[tot$series == di_levels[1]], sym), fmt_bn(tot$v[tot$series == di_levels[2]], sym)),
           x = NULL, y = NULL,
           caption = paste0("Source: U.S. BEA, International Transactions Accounts, Table 1.3 (Australia), directional basis.\n",
                            "Notes: Financial flows, not seasonally adjusted, as recorded by the U.S. Negative values are net withdrawals. ",
                            "These U.S. figures differ from the ABS investment positions shown in the other tabs, which are stocks at 31 December ",
                            "covering all types of investment; BEA and the ABS also differ in country attribution, timing and revisions.",
                            if (cur == "AUD") fx_note_aud_from_usd else "")) +
      theme_csis()
  }
  # US$ data: the A$ version is the default file name, as for every other chart
  save_both(make_di, "Investment/us_au_direct_investment_quarterly.png", save_plot)
  write.csv(di, "Investment/us_au_direct_investment_quarterly_tidy.csv", row.names = FALSE)
}

# ---- 5. Quarterly services trade with the U.S. (BEA) ----------------------------
# BEA international transactions accounts, Table 1.3, Australia, seasonally
# adjusted, US$ million. ExpServ = U.S. services exports to Australia
# (Australia's imports from the U.S.); ImpServ = U.S. services imports from
# Australia (Australia's exports to the U.S.). Shown beside the quarterly goods
# chart on Trade Overview. Same BEA key as section 4.
if (nzchar(bea_key)) {
  svc <- bind_rows(lapply(c("ExpServ", "ImpServ"), function(i)
    bea_api(method = "GetData", DataSetName = "ITA", Indicator = i, AreaOrCountry = "Australia",
            Frequency = "QSA", Year = "ALL")$Data)) |>
    transmute(ind = Indicator,
              quarter = as.Date(sprintf("%s-%02d-01", substr(TimePeriod, 1, 4), as.integer(substr(TimePeriod, 6, 6)) * 3)),
              value = suppressWarnings(as.numeric(gsub(",", "", DataValue))) / 1000) |>
    filter(!is.na(value)) |>
    pivot_wider(names_from = ind, values_from = value) |>
    filter(!is.na(ExpServ), !is.na(ImpServ)) |>
    transmute(quarter, au_exports = ImpServ, us_exports = ExpServ, surplus = ExpServ - ImpServ) |>
    arrange(quarter)
  svc <- filter(svc, quarter >= first_quarter_shown(max(svc$quarter)))
  make_svc_q <- function(cur) {
    k <- if (cur == "AUD") 1 / fx_quarter(svc$quarter) else 1      # BEA data are in US$
    sym <- cur_sym[[cur]]
    d <- svc |> mutate(au_exports = au_exports * k, us_exports = us_exports * k, surplus = surplus * k)
    flows <- bind_rows(
      d |> transmute(quarter, value = au_exports, series = flow_levels[1]),
      d |> transmute(quarter, value = us_exports, series = flow_levels[2])) |>
      mutate(series = factor(series, flow_levels))
    ggplot() +
      geom_col(data = d, aes(quarter, surplus, fill = "U.S. services trade surplus"), width = 62) +
      geom_hline(yintercept = 0, colour = csis_colours[["grey"]], linewidth = 0.5) +
      geom_line(data = flows, aes(quarter, value, colour = series), linewidth = 1) +
      geom_point(data = flows, aes(quarter, value, colour = series), size = 1.6) +
      scale_colour_manual(values = setNames(unname(csis_colours[c("navy", "red")]), flow_levels)) +
      scale_fill_manual(values = c("U.S. services trade surplus" = "#F7D98B")) +
      scale_y_continuous(labels = lab_bn(sym), limits = c(0, NA), breaks = breaks_pretty(5)) +
      scale_x_date(breaks = qtr_breaks(d$quarter), labels = qtr_label, expand = expansion(mult = c(0.02, 0.02))) +
      guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2)) +
      labs(hover_accuracy = 0.1, title = "Australia's trade with the United States",
           subtitle = "Services, quarterly",
           x = NULL, y = NULL,
           caption = paste0(
             "Source: U.S. BEA, International Transactions Accounts, Table 1.3 (Australia).\n",
             "Notes: U.S. data, seasonally adjusted. Australian exports to the U.S. are U.S. imports of services from Australia, ",
             "and Australian imports from the U.S. are U.S. exports to Australia. U.S. surplus = U.S. exports less U.S. imports. ",
             "BEA figures differ from the ABS services data in the annual chart (timing, valuation and coverage).",
             if (cur == "AUD") fx_note_aud_from_usd else "")) +
      theme_csis() +
      theme(legend.box = "vertical", legend.box.just = "left",
            legend.spacing.y = unit(0, "pt"), legend.margin = margin(0, 0, 0, 0))
  }
  save_both(make_svc_q, "Trade/us_au_services_quarterly.png", save_plot)
}

message("Quarterly charts done.")
