rm(list = ls())
# The dashboard folder is set once, in section 1 of '1. Refresh all data.R'.
# To run this script on its own, run that section first.
if (Sys.getenv("DASHBOARD_DIR") == "")
  stop("Dashboard folder not set. Run section 1 of '1. Refresh all data.R' first.")
setwd(file.path(Sys.getenv("DASHBOARD_DIR"), "Tech"))
source(file.path(Sys.getenv("DASHBOARD_DIR"), "0. Takeaways.R"))       # line under each chart

# ==============================================================================
# Australia in critical and emerging technologies
# Modules:
#   A  Research output share by technology, Australia vs comparators  (OpenAlex)
#   B  High-impact share - where Australia punches above weight       (OpenAlex)
#   C  Top Australian institutions per technology                     (OpenAlex)
#   D  Collaboration partners: US vs China co-authorship              (OpenAlex)
#   E  Frontier AI model development by country                       (Epoch AI)
#   F  R&D intensity vs comparators                                   (OECD MSTI)
#
# Technology categories follow DISR's List of Critical Technologies in the
# National Interest (updated July 2026) so output maps onto Australian policy
# language and AUKUS Pillar 2 advanced capabilities.
#
# CAVEAT ON METHOD: this uses keyword matching on titles and abstracts. ASPI's
# Critical Technology Tracker uses curated Web of Science query strings plus
# citation percentiles and is more defensible for publication.
# ==============================================================================


library(httr2)
library(dplyr)
library(tidyr)
library(purrr)
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

# ------------------------------------------------------------------------------
# 0. Configuration
# ------------------------------------------------------------------------------

# Free key from https://openalex.org - see https://docs.openalex.org
# Store in .Renviron as OPENALEX_KEY=... rather than hardcoding.
OA_KEY   <- Sys.getenv("OPENALEX_KEY")
OA_EMAIL <- "abruno@csis.org"   

YEARS <- 2015:2025

COMPARATORS <- c(
  au = "Australia", us = "United States", cn = "China", gb = "United Kingdom",
  jp = "Japan",     kr = "South Korea",   in_ = "India", ca = "Canada",
  de = "Germany"
)

# One colour per country across the dashboard: Australia navy, US red.
# Used the same way in every Technology chart: Australia navy (light navy for a
# second Australian series), U.S. red, China gold, all other countries gray.
AU_COL <- "#00205B"; AU_LIGHT <- "#8A9BB8"; US_COL <- "#D2232A"; CN_COL <- "#F2A900"; OTHER_COL <- "#b4b2a9"

# Keyword definitions.
# Quoted phrases are treated as phrases by OpenAlex; OR joins alternatives.
TECHNOLOGIES <- c(
  "Artificial intelligence" =
    '"machine learning" OR "deep learning" OR "neural network" OR "artificial intelligence" OR "large language model"',
  "Quantum computing" =
    '"quantum computing" OR "quantum computer" OR "qubit" OR "quantum error correction"',
  "Quantum sensing and comms" =
    '"quantum sensing" OR "quantum key distribution" OR "quantum communication" OR "quantum metrology"',
  "Advanced comms (5G/6G)" =
    '"5G network" OR "6G network" OR "millimeter wave communication" OR "massive MIMO" OR "network slicing"',
  "Cybersecurity" =
    '"cybersecurity" OR "intrusion detection" OR "post-quantum cryptography" OR "malware detection"',
  "Semiconductors" =
    '"semiconductor device" OR "integrated circuit design" OR "photonic integrated circuit" OR "compound semiconductor"',
  "Autonomous systems and robotics" =
    '"autonomous vehicle" OR "unmanned aerial vehicle" OR "swarm robotics" OR "robot navigation"',
  "Critical minerals processing" =
    '"rare earth extraction" OR "lithium extraction" OR "hydrometallurgy" OR "critical mineral processing"'
)


# ------------------------------------------------------------------------------
# 0b. Disk cache
#
# Every fetch below goes through cached(). First run hits the network and
# writes an .rds into ./cache/; later runs read from disk. Nothing re-downloads
# unless the cache file is older than MAX_AGE_DAYS or REFRESH is TRUE.
#
#   REFRESH <- TRUE                     # force a full refresh
#   cached("name", expr, refresh = TRUE) # force one dataset
#   unlink("cache", recursive = TRUE)    # nuke everything
# ------------------------------------------------------------------------------

CACHE_DIR    <- "cache"
MAX_AGE_DAYS <- 30
REFRESH      <- FALSE

dir.create(CACHE_DIR, showWarnings = FALSE)

cached <- function(name, expr, refresh = REFRESH, max_age = MAX_AGE_DAYS) {
  f <- file.path(CACHE_DIR, paste0(name, ".rds"))
  
  if (!refresh && file.exists(f)) {
    age <- as.numeric(difftime(Sys.time(), file.mtime(f), units = "days"))
    if (age <= max_age) {
      message(sprintf("cache hit : %-22s (%.1f days old)", name, age))
      return(readRDS(f))
    }
    message(sprintf("cache stale: %-22s (%.1f days old, refetching)", name, age))
  } else {
    message("fetching  : ", name)
  }
  
  value <- force(expr)
  saveRDS(value, f)
  attr(value, "fetched_at") <- Sys.time()
  value
}

# What is currently cached, and how old
cache_status <- function() {
  files <- list.files(CACHE_DIR, pattern = "\\.rds$", full.names = TRUE)
  if (!length(files)) return(message("Cache is empty."))
  tibble(
    dataset  = tools::file_path_sans_ext(basename(files)),
    fetched  = file.mtime(files),
    days_old = round(as.numeric(difftime(Sys.time(), file.mtime(files), units = "days")), 1),
    size_kb  = round(file.size(files) / 1024, 1)
  ) |> arrange(desc(fetched))
}


# ------------------------------------------------------------------------------
# 1. OpenAlex helper
#
# Uses the group_by endpoint, which returns aggregate counts without paging
# through individual records. One request per query rather than thousands.
# ------------------------------------------------------------------------------

oa_group <- function(filters, group_by, retries = 3) {
  req <- request("https://api.openalex.org/works") |>
    req_url_query(
      filter   = paste(filters, collapse = ","),
      group_by = group_by,
      mailto   = OA_EMAIL,
      per_page = 200
    ) |>
    req_user_agent(paste0("CSIS Australia Chair research (", OA_EMAIL, ")")) |>
    req_retry(max_tries = retries) |>
    req_throttle(capacity = 10, fill_time_s = 1)
  
  if (nzchar(OA_KEY)) req <- req_url_query(req, api_key = OA_KEY)
  
  out <- req |> req_perform() |> resp_body_json()
  tibble(
    key   = map_chr(out$group_by, "key"),
    label = map_chr(out$group_by, "key_display_name"),
    n     = map_int(out$group_by, "count")
  )
}

# Shared filters applied to every query.
base_filters <- function(extra = character()) {
  c(paste0("publication_year:", min(YEARS), "-", max(YEARS)),
    "type:article",
    "is_retracted:false",
    extra)
}

kw_filter <- function(tech) paste0("title_and_abstract.search:", TECHNOLOGIES[[tech]])

# The keyword lists, so readers can see exactly what each field counts
write_csv(tibble(technology = names(TECHNOLOGIES), openalex_query = unname(TECHNOLOGIES),
                 searched_in = "title and abstract", years = paste0(min(YEARS), "-", max(YEARS))),
          "technology_keywords.csv")
KW_NOTE <- "Fields are defined by keyword searches of paper titles and abstracts."
OA_NOTE <- "Publication counts are from OpenAlex, an open bibliographic database."

# OpenAlex group_by returns `key` in different shapes depending on the field:
# a bare code ("AU"), a lowercase code ("au"), or a full URI
# ("https://openalex.org/countries/AU"). Match on all of them plus the display
# name so the pipeline does not silently return zero rows.
country_is <- function(df, code, name) {
  df |> filter(
    str_to_upper(str_extract(key, "[A-Za-z]{2}$")) == str_to_upper(code) |
      str_to_lower(label) == str_to_lower(name)
  )
}

# Diagnostic - run after any fetch to see what the API actually returned.
peek <- function(df, n = 8) {
  message("rows: ", nrow(df))
  print(head(distinct(df, key, label), n))
  invisible(df)
}


# ------------------------------------------------------------------------------
# 2. MODULE A - research output by country and technology
#
# Establishes the baseline. Australia is roughly 0.3% of world
# population and produces ~3% of research output, so interesting question is
# which technologies it over- or under-indexes on relative to that baseline.
# ------------------------------------------------------------------------------

fetch_country_counts <- function(tech) {
  message("Fetching: ", tech)
  oa_group(base_filters(kw_filter(tech)),
           group_by = "authorships.countries") |>
    mutate(technology = tech)
}

country_counts <- cached(
  "A_country_counts",
  map_dfr(names(TECHNOLOGIES), fetch_country_counts)
)

write_csv(country_counts, "tech_country_counts_raw.csv")

peek(country_counts)

au_share <- country_counts |>
  group_by(technology) |>
  mutate(world = sum(n)) |>
  ungroup() |>
  country_is("AU", "Australia") |>
  transmute(technology, au_papers = n, share = n / world)

stopifnot("Module A returned no Australian rows - inspect peek() output above" =
            nrow(au_share) > 0)

# Benchmark: the same counts across all articles in OpenAlex (every field, same
# years and filters, no keyword search), shown as "All publications"
ALL_PUBS <- "All publications"
all_counts <- cached(
  "A_all_country_counts",
  oa_group(base_filters(), group_by = "authorships.countries") |> mutate(technology = ALL_PUBS)
)
counts_ab <- bind_rows(country_counts, all_counts)

# Australia, the United States and China in each field, plus all publications
world_n <- counts_ab |> group_by(technology) |> summarise(world = sum(n), .groups = "drop")
three_share <- bind_rows(
  country_is(counts_ab, "AU", "Australia") |> mutate(country = "Australia"),
  country_is(counts_ab, "US", "United States") |> mutate(country = "United States"),
  country_is(counts_ab, "CN", "China") |> mutate(country = "China")) |>
  left_join(world_n, by = "technology") |>
  transmute(technology, country, papers = n, share = n / world)
tech_order <- au_share |> arrange(share) |> pull(technology)
three_share <- three_share |>
  mutate(technology = factor(technology, levels = c(ALL_PUBS, tech_order)),        # benchmark at the bottom
         country = factor(country, levels = c("China", "United States", "Australia")))   # Australia drawn on top in each group
p_a <- three_share |>
  ggplot(aes(share, technology, fill = country)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_hline(yintercept = 1.5, colour = "grey60", linewidth = 0.4) +   # sets the benchmark apart
  scale_fill_manual(values = c(Australia = AU_COL, `United States` = US_COL, China = CN_COL),
                    breaks = c("Australia", "United States", "China")) +
  scale_y_discrete(labels = function(x) ifelse(x == ALL_PUBS, "All publications\n(all fields)", x)) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(c(0, 0.05))) +
  labs(
    title    = "Share of world publications by critical technology",
    subtitle = paste0("Share of world publications, ", min(YEARS), "-", max(YEARS)),
    x = NULL, y = NULL, fill = NULL,
    caption = paste0("Source: OpenAlex.
", OA_NOTE, " ", KW_NOTE, " All publications: the same measure across every field. Papers with authors in several countries count for each.")
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())

ggsave("A_australian_share_by_technology.png", p_a, width = 9, height = 5.5,
       dpi = 150, bg = "white")
write_hover(p_a, "A_australian_share_by_technology.png", width = 9, height = 5.5, dpi = 150,
            spots = hbar_spots(function(cat, series, v) {
              row <- three_share[three_share$technology == cat & three_share$country == series, ]
              sprintf("<b>%s</b><br>%s: %s papers, %.1f%% of the world total", cat, series,
                      comma(row$papers[1]), 100 * v)
            }))


# ------------------------------------------------------------------------------
# 2b. MODULE G - shares over time
#
# The same measure as chart A (a country's papers in a field as a share of all
# countries' papers in that field), year by year, for Australia, the United
# States and China. One OpenAlex query per field and year, cached.
# ------------------------------------------------------------------------------
fetch_country_year <- function(tech, yr) {
  message("Fetching: ", tech, " ", yr)
  oa_group(c(paste0("publication_year:", yr), "type:article", "is_retracted:false", kw_filter(tech)),
           group_by = "authorships.countries") |>
    mutate(technology = tech, year = yr)
}
country_year <- cached(
  "G_country_year_counts",
  expand_grid(tech = names(TECHNOLOGIES), yr = YEARS) |>
    pmap_dfr(function(tech, yr) fetch_country_year(tech, yr))
)
share_time <- country_year |>
  group_by(technology, year) |> mutate(world = sum(n)) |> ungroup()
share_time <- bind_rows(
  country_is(share_time, "AU", "Australia") |> mutate(country = "Australia"),
  country_is(share_time, "US", "United States") |> mutate(country = "United States"),
  country_is(share_time, "CN", "China") |> mutate(country = "China")) |>
  transmute(technology, year, country, papers = n, share = n / world) |>
  mutate(country = factor(country, levels = c("Australia", "United States", "China")),
         technology = factor(technology, levels = rev(tech_order)))
last_year <- max(YEARS)
p_g <- share_time |>
  ggplot(aes(year, share, colour = country)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ technology, ncol = 4, labeller = label_wrap_gen(22)) +
  scale_colour_manual(values = c(Australia = AU_COL, `United States` = US_COL, China = CN_COL)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
  scale_x_continuous(breaks = c(2016, 2020, 2024)) +                    # inner years, so neighbouring panels' labels don't touch
  labs(
    title    = "Share of world publications over time, by critical technology",
    subtitle = paste0("Share of world publications, ", min(YEARS), "-", last_year),
    x = NULL, y = NULL, colour = NULL,
    caption = paste0("Source: OpenAlex.
", OA_NOTE, " ", KW_NOTE, " Papers with authors in several countries count for each. ",
                     last_year, " may be incomplete while recent papers are still being indexed. All panels on the same scale.")
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), plot.title.position = "plot",
        strip.text = element_text(face = "bold", hjust = 0.5), panel.grid.minor = element_blank(),
        panel.spacing.x = unit(1.2, "lines"),
        legend.position = "top")
ggsave("G_share_over_time.png", p_g, width = 10, height = 6.2, dpi = 150, bg = "white")


# ------------------------------------------------------------------------------
# 3. MODULE B - quality, not just volume
#
# Raw counts flatter large systems. Restricting to the top decile of
# cited work is closer to ASPI's method.

# ------------------------------------------------------------------------------

fetch_top_decile <- function(tech) {
  message("Top-decile: ", tech)
  oa_group(base_filters(c(kw_filter(tech), "cited_by_percentile_year.min:90")),
           group_by = "authorships.countries") |>
    mutate(technology = tech)
}

top_decile <- cached(
  "B_top_decile",
  map_dfr(names(TECHNOLOGIES), fetch_top_decile)
)

quality_gap <- bind_rows(
  country_counts |> mutate(measure = "All papers"),
  top_decile     |> mutate(measure = "Top 10% most cited")
) |>
  group_by(technology, measure) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  country_is("AU", "Australia")

stopifnot("Module B returned no Australian rows" = nrow(quality_gap) > 0)

p_b <- quality_gap |>
  mutate(technology = fct_reorder(technology, share, .fun = max)) |>
  ggplot(aes(share, technology, colour = measure)) +
  geom_line(aes(group = technology), colour = "grey70", linewidth = 0.6) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("All papers" = AU_LIGHT,
                                 "Top 10% most cited" = AU_COL)) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title    = "Australian share of world publications: all papers and top 10% cited",
    subtitle = "Share of world output, all papers vs top decile by citations",
    x = NULL, y = NULL, colour = NULL,
    caption = paste0("Source: OpenAlex.
", OA_NOTE, " ", KW_NOTE)
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())

ggsave("B_quality_vs_volume.png", p_b, width = 9, height = 5.5, dpi = 150, bg = "white")


# ------------------------------------------------------------------------------
# 4. MODULE C - which Australian institutions
#
# Capability is concentrated in a handful of universities and CSIRO.
# Directly relevant to research-security and export-control conversations.
# ------------------------------------------------------------------------------

fetch_au_institutions <- function(tech) {
  message("Institutions: ", tech)
  oa_group(base_filters(c(kw_filter(tech), "institutions.country_code:au")),
           group_by = "institutions.id") |>
    mutate(technology = tech) |>
    slice_max(n, n = 6)
}

au_institutions <- cached(
  "C_au_institutions",
  map_dfr(names(TECHNOLOGIES), fetch_au_institutions)
)

write_csv(au_institutions, "C_australian_institutions.csv")

p_c <- au_institutions |>
  filter(technology %in% c("Artificial intelligence", "Quantum computing",
                           "Cybersecurity", "Advanced comms (5G/6G)")) |>
  mutate(label = str_remove(label, "^The "),
         # Facet-local ordering without tidytext: make each label unique per
         # facet, order globally, then strip the suffix at render time.
         label = fct_reorder(paste(label, technology, sep = "\u001f"), n)) |>
  ggplot(aes(n, label)) +
  geom_col(fill = "#00205B", width = 0.65) +
  facet_wrap(~technology, scales = "free", ncol = 2) +
  # Long names shortened and wrapped, so the bars keep most of each panel
  scale_y_discrete(labels = function(x) {
    x <- str_remove(x, "\u001f.*$")
    x <- recode(x, "Commonwealth Scientific and Industrial Research Organisation" = "CSIRO",
                "Centre for Quantum Computation and Communication Technology" = "Centre for Quantum Computation (CQC2T)")
    str_wrap(x, 24)
  }) +
  scale_x_continuous(labels = comma, expand = expansion(c(0, 0.1))) +
  labs(
    title    = "Leading Australian institutions by publications in each field",
    subtitle = paste0("Publications ", min(YEARS), "-", max(YEARS), ", top six institutions per field"),
    x = NULL, y = NULL, caption = paste0("Source: OpenAlex.
", OA_NOTE)
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 15),
        plot.title.position = "plot",
        strip.text = element_text(face = "bold", hjust = 0),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())

ggsave("C_australian_institutions.png", p_c, width = 10, height = 7, dpi = 150, bg = "white")


# ------------------------------------------------------------------------------
# 5. MODULE D - collaboration partners
#
# For each technology,what share of Australian papers are co-authored with US institutions versus
# Chinese ones? Feeds directly into research-security debates, the Defence
# Trade Controls Act amendments, and AUKUS Pillar 2 technology-sharing.
# ------------------------------------------------------------------------------

fetch_partner <- function(tech, partner) {
  oa_group(
    base_filters(c(kw_filter(tech),
                   "institutions.country_code:au",
                   paste0("institutions.country_code:", partner))),
    group_by = "publication_year"
  ) |>
    mutate(technology = tech, partner = partner)
}

partners <- cached(
  "D_collaboration",
  expand_grid(tech = names(TECHNOLOGIES), partner = c("us", "cn")) |>
    pmap_dfr(function(tech, partner) {
      message("Collab: ", tech, " x ", partner)
      fetch_partner(tech, partner)
    })
)

# Benchmark: the same shares across all Australian articles, every field
partners_all <- cached(
  "D_collaboration_all",
  map_dfr(c("us", "cn"), function(partner)
    oa_group(base_filters(c("institutions.country_code:au", paste0("institutions.country_code:", partner))),
             group_by = "publication_year") |>
      mutate(technology = ALL_PUBS, partner = partner))
)

au_totals <- counts_ab |>
  country_is("AU", "Australia") |>
  select(technology, au_total = n)

collab <- bind_rows(partners, partners_all) |>
  group_by(technology, partner) |>
  summarise(n = sum(n), .groups = "drop") |>
  left_join(au_totals, by = "technology") |>
  mutate(share = n / au_total,
         partner = recode(partner, us = "with United States", cn = "with China"))

d_order <- collab |> filter(technology != ALL_PUBS) |> group_by(technology) |>
  summarise(m = max(share), .groups = "drop") |> arrange(m) |> pull(technology)
p_d <- collab |>
  mutate(technology = factor(technology, levels = c(ALL_PUBS, d_order))) |>     # benchmark at the bottom
  ggplot(aes(share, technology, fill = partner)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_hline(yintercept = 1.5, colour = "grey60", linewidth = 0.4) +   # sets the benchmark apart
  scale_y_discrete(labels = function(x) ifelse(x == ALL_PUBS, "All publications\n(all fields)", x)) +
  scale_fill_manual(values = c("with United States" = US_COL,
                               "with China" = CN_COL)) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(c(0, 0.05))) +
  labs(
    title    = "Australian publications with U.S. and Chinese co-authors",
    subtitle = paste0("Share of all Australian publications in each field, ", min(YEARS), "-", max(YEARS),
                      ", co-authored with a U.S. or Chinese institution"),
    x = NULL, y = NULL, fill = NULL,
    caption = paste0("Source: OpenAlex.
", OA_NOTE, " Shares overlap: a paper can have both U.S. and Chinese co-authors. ", KW_NOTE,
                     " All publications: the same measure across every field.")
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())

ggsave("D_collaboration_partners.png", p_d, width = 9, height = 5.5, dpi = 150, bg = "white")



# ------------------------------------------------------------------------------
# 6. MODULE E - frontier AI model development
#
# Research output and model development are different things. Australia
# ranks respectably on AI papers and close to nowhere on notable models. The
# gap, strong research base, no frontier development capacity, is arguably the
# single most important framing for a US audience thinking about AUKUS Pillar 2
# and about where Australia is a partner versus a customer.
#
#Cite as: Epoch AI, 'Data on AI Models', epoch.ai.
# ------------------------------------------------------------------------------

# Epoch updates this dataset roughly daily, so cache it for a shorter window.
models <- cached(
  "E_epoch_models",
  read_csv("https://epoch.ai/data/notable_ai_models.csv", show_col_types = FALSE),
  max_age = 7
)

glimpse(models)   # column names change occasionally - check before relying on them

# Epoch renames columns periodically. Find the country column by pattern rather
# than hardcoding it, and fail loudly if it disappears entirely.
country_col <- names(models)[str_detect(names(models), regex("country", ignore_case = TRUE))][1]
date_col    <- names(models)[str_detect(names(models), regex("publication date", ignore_case = TRUE))][1]
stopifnot(!is.na(country_col), !is.na(date_col))
message("Using columns: ", country_col, " / ", date_col)

model_countries <- models |>
  filter(!is.na(.data[[country_col]])) |>
  separate_longer_delim(all_of(country_col), delim = ",") |>
  mutate(country = str_squish(.data[[country_col]]),
         year    = lubridate::year(.data[[date_col]])) |>
  filter(year >= 2015, nzchar(country)) |>
  # Epoch uses long-form UN names; shorten the ones that wreck an axis.
  mutate(country = recode(country,
                          "United States of America" = "United States",
                          "Korea (Republic of)"      = "South Korea",
                          "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
                          "Russian Federation"       = "Russia",
                          "Taiwan (Province of China)" = "Taiwan",
                          .default = country)) |>
  count(country, sort = TRUE)

print(model_countries, n = 25)   # confirm "Australia" appears as expected

p_e <- model_countries |>
  filter(row_number() <= 12 | country == "Australia") |>   # always show Australia
  mutate(who = case_when(country == "Australia" ~ "au", country == "United States" ~ "us",
                         country == "China" ~ "cn", TRUE ~ "other"),
         country = fct_reorder(country, n)) |>
  ggplot(aes(n, country, fill = who)) +
  geom_col(width = 0.65) +
  scale_fill_manual(values = c(au = AU_COL, us = US_COL, cn = CN_COL, other = OTHER_COL), guide = "none") +
  scale_x_continuous(labels = comma, expand = expansion(c(0, 0.04))) +
  labs(
    title    = "Notable AI models by country",
    subtitle = "Notable AI models by country of developing organization, 2015 onward",
    x = NULL, y = NULL,
    caption = paste("Source: Epoch AI, 'Data on AI Models' (CC-BY).",
                    "Note: Notable models are those Epoch AI judges to have advanced the state of the art,",
                    "been highly cited, been historically significant or been widely used.")
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())

ggsave("E_ai_models_by_country.png", p_e, width = 9, height = 5.5, dpi = 150, bg = "white")
write_hover(p_e, "E_ai_models_by_country.png", width = 9, height = 5.5, dpi = 150,
            spots = hbar_spots(function(cat, series, v)
              sprintf("<b>%s</b><br>%s notable AI model%s since 2015", cat, comma(v), if (v == 1) "" else "s")))


# ------------------------------------------------------------------------------
# 7. MODULE F - R&D intensity
#
# Australian GERD as a share of GDP has been drifting below the OECD average, and
# BERD is unusually low for a high-income economy. Every capability finding above
# sits downstream of this.
# ------------------------------------------------------------------------------

# GERD as a share of GDP from the OECD's Main Science and Technology
# Indicators (MSTI). For Australia these are the ABS's own R&D surveys (ABS
# 8104.0 and 8112.0), which run every second year, so its line has fewer points.
MSTI_URL <- paste0("https://sdmx.oecd.org/public/rest/data/OECD.STI.STP,DSD_MSTI@DF_MSTI,/",
                   "AUS+USA+CHN+GBR+JPN+KOR+CAN+DEU+ISR.A.G.PT_B1GQ......?startPeriod=2005&format=csv")

fetch_msti <- function() {
  old <- options(timeout = 300); on.exit(options(old), add = TRUE)
  tryCatch(read_csv(MSTI_URL, show_col_types = FALSE) |>
             transmute(iso3c = REF_AREA, year = as.integer(TIME_PERIOD), gerd = OBS_VALUE),
           error = function(e) { message("MSTI failed: ", conditionMessage(e)); NULL })
}

rd <- cached("F_msti_rd", fetch_msti(), max_age = 180)

if (is.null(rd) || nrow(rd) == 0) {
  unlink(file.path(CACHE_DIR, "F_msti_rd.rds"))   # don't cache a failure
  message("\n--- Module F skipped: OECD MSTI unavailable. ---\n")
} else {

  iso2 <- c(AUS = "AU", USA = "US", CHN = "CN", GBR = "GB", JPN = "JP", KOR = "KR",
            CAN = "CA", DEU = "DE", ISR = "IL")
  rd_plot <- rd |>
    filter(!is.na(gerd)) |>
    mutate(who = case_when(iso3c == "AUS" ~ "au", iso3c == "USA" ~ "us", iso3c == "CHN" ~ "cn", TRUE ~ "other"),
           iso2c = iso2[iso3c],
           country_name = c(AUS = "Australia", USA = "United States", CHN = "China", GBR = "United Kingdom", JPN = "Japan",
                            KOR = "South Korea", CAN = "Canada", DEU = "Germany", ISR = "Israel")[iso3c]) |>
    arrange(iso3c, year)

  # One label per country, at its most recent year
  rd_labels <- rd_plot |>
    group_by(iso3c) |>
    slice_max(year, n = 1, with_ties = FALSE) |>
    ungroup()

  p_f <- ggplot(rd_plot, aes(year, gerd, group = iso3c)) +
    geom_line(aes(colour = who, linewidth = who)) +
    # Country names at the line ends, spaced so they don't overlap
    ggrepel::geom_text_repel(
      data = rd_labels,
      aes(label = country_name, colour = who),
      hjust = 0, direction = "y", nudge_x = 0.4, xlim = c(max(rd_plot$year) + 0.3, NA),
      size = 3.6, segment.colour = NA, min.segment.length = Inf, show.legend = FALSE
    ) +
    scale_colour_manual(values = c(au = AU_COL, us = US_COL, cn = CN_COL, other = OTHER_COL),
                        guide = "none") +
    scale_linewidth_manual(values = c(au = 1.2, us = 1.2, cn = 0.9, other = 0.6),
                           guide = "none") +
    scale_x_continuous(expand = expansion(c(0.02, 0.22))) +                # room for country names
    labs(
      title    = "R&D intensity",
      subtitle = "Gross domestic expenditure on R&D, percent of GDP",
      x = NULL, y = NULL,
      caption = paste("Source: OECD, Main Science and Technology Indicators (from the ABS R&D surveys for Australia,",
                      "\nwhich are run every second year).")
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"),
          plot.title.position = "plot",
          panel.grid.minor = element_blank())

  ggsave("F_rd_intensity.png", p_f, width = 9, height = 5.5, dpi = 150, bg = "white")

  au_l <- rd_labels |> filter(iso3c == "AUS"); us_l <- rd_labels |> filter(iso3c == "USA")
  oecd_rank <- rd_labels |> arrange(desc(gerd)) |> mutate(r = row_number())
  takeaway("Tech/F_rd_intensity.png", sprintf(
    "Australia spent %.2f%% of GDP on R&D in %d, against %.2f%% in the United States in %d.",
    au_l$gerd, au_l$year, us_l$gerd, us_l$year))

}   # end module F conditional

# Takeaways for modules A-E
local({
  top <- au_share |> arrange(desc(share))
  takeaway("Tech/A_australian_share_by_technology.png", sprintf(
    "Australia's largest share of world research among these fields is in %s (%.1f%% of papers, %s papers); its smallest is in %s (%.1f%%).",
    top$technology[1], 100 * top$share[1], comma(top$au_papers[1]),
    top$technology[nrow(top)], 100 * top$share[nrow(top)]))
  q <- quality_gap |> select(technology, measure, share) |>
    pivot_wider(names_from = measure, values_from = share) |>
    mutate(ratio = `Top 10% most cited` / `All papers`) |> arrange(desc(ratio))
  takeaway("Tech/B_quality_vs_volume.png", if (any(q$ratio > 1)) sprintf(
    "Australia's share of the most-cited papers is higher than its share of all papers in %d of %d fields, most of all in %s.",
    sum(q$ratio > 1), nrow(q), q$technology[1]) else sprintf(
    "Australia's share of the most-cited papers is below its share of all papers in all %d fields; the gap is smallest in %s.",
    nrow(q), q$technology[1]))
  ai <- au_institutions |> filter(technology == "Artificial intelligence") |> slice_max(n, n = 1)
  takeaway("Tech/C_australian_institutions.png", sprintf(
    "%s publishes the most Australian research on artificial intelligence (%s papers, %d-%d).",
    str_remove(ai$label, "^The "), comma(ai$n), min(YEARS), max(YEARS)))
  w <- collab |> select(technology, partner, share) |>
    pivot_wider(names_from = partner, values_from = share)
  takeaway("Tech/D_collaboration_partners.png", sprintf(
    "Australian researchers co-author more with Chinese than U.S. institutions in %d of %d fields.",
    sum(w$`with China` > w$`with United States`), nrow(w)))
  au_m <- model_countries$n[model_countries$country == "Australia"]
  us_m <- model_countries$n[model_countries$country == "United States"]
  takeaway("Tech/E_ai_models_by_country.png", sprintf(
    "Organizations in the United States developed %s notable AI models since 2015; Australian organizations developed %s.",
    comma(us_m), if (length(au_m)) comma(au_m) else "none"))
})



message("Done - charts written to ", getwd())
print(cache_status())