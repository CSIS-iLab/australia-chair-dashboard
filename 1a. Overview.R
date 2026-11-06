rm(list = ls())
# The dashboard folder is set once, in section 1 of '1. Refresh all data.R'.
# To run this script on its own, run that section first.
if (Sys.getenv("DASHBOARD_DIR") == "")
  stop("Dashboard folder not set. Run section 1 of '1. Refresh all data.R' first.")
setwd(file.path(Sys.getenv("DASHBOARD_DIR"), "Overview"))
# ============================================================
# AUSTRALIA AND THE CONTIGUOUS US: SIZE, COASTLINE, POPULATION
# ============================================================
# Facts for the "Two countries at a glance" band and the map labels, written to
# Overview/country_facts.csv (the .qmd reads that file).
#
# Land area
#   Australia: CIA World Factbook, land area 7,682,300 sq km (includes
#     Lord Howe and Macquarie islands). The Factbook was retired on
#     4 February 2026; figures are from its final online edition.
#   Contiguous US (48 states + DC): US Census Bureau, State Area
#     Measurements (2010), United States land area less Alaska and Hawaii:
#     9,147,593 - 1,477,953 - 16,635 = 7,653,005 sq km.
# Coastline: CIA World Factbook, final edition (Australia 25,760 km;
#   United States 19,924 km, all states, as the Factbook gives no figure
#   for the contiguous US alone).
# Population (refreshed each run):
#   Australia: ABS, National, state and territory population, estimated
#     resident population, latest quarter (ABS Data API, dataflow ERP_Q).
#   United States: US Census Bureau, Vintage population estimates
#     (NST-EST<year>-ALLDATA), latest 1 July estimate for the whole country.
# ------------------------------------------------------------

KM2_PER_SQMI <- 2.589988110336
KM_PER_MILE  <- 1.609344

facts_file <- "country_facts.csv"
old_facts  <- if (file.exists(facts_file)) utils::read.csv(facts_file) else NULL

# Australia: ABS estimated resident population, latest quarter
au_pop <- tryCatch({
  # ABS Data API, dataflow ERP_Q: measure 1 (ERP), persons, all ages, Australia, quarterly
  x <- utils::read.csv(url(paste0("https://data.api.abs.gov.au/rest/data/ABS,ERP_Q,1.0.0/",
                                  "1.3.TOT.AUS.Q?lastNObservations=1&format=csv")))
  q <- x$TIME_PERIOD[1]                                   # e.g. "2026-Q1"
  qend <- c(Q1 = "March", Q2 = "June", Q3 = "September", Q4 = "December")[[sub(".*-", "", q)]]
  list(value = x$OBS_VALUE[1], as_at = paste(qend, sub("-.*", "", q)))
}, error = function(e) {
  message("ABS population not available (", conditionMessage(e), "); keeping the last value.")
  NULL
})

# United States: newest Census vintage that exists
us_pop <- tryCatch({
  got <- NULL
  for (v in as.integer(format(Sys.Date(), "%Y")):(as.integer(format(Sys.Date(), "%Y")) - 3)) {
    url <- sprintf("https://www2.census.gov/programs-surveys/popest/datasets/2020-%d/state/totals/NST-EST%d-ALLDATA.csv", v, v)
    d <- tryCatch(utils::read.csv(url), error = function(e) NULL)
    if (!is.null(d)) { got <- list(d = d, v = v); break }
  }
  if (is.null(got)) stop("no Census vintage found")
  us <- got$d[got$d$NAME == "United States", ]
  list(value = us[[paste0("POPESTIMATE", got$v)]], as_at = paste("July", got$v))
}, error = function(e) {
  message("Census population not available (", conditionMessage(e), "); keeping the last value.")
  NULL
})

keep_old <- function(country, field) {
  if (is.null(old_facts)) return(NA)
  old_facts[old_facts$country == country, field]
}

facts <- data.frame(
  country        = c("Australia", "United States"),
  land_note      = c("Includes Lord Howe and Macquarie islands", "Contiguous 48 states and DC, as in the map"),
  land_km2       = c(7682300, 9147593 - 1477953 - 16635),
  coastline_km   = c(25760, 19924),
  coastline_note = c("", "All US states; the Factbook gives no contiguous-only figure"),
  population     = c(if (!is.null(au_pop)) au_pop$value else keep_old("Australia", "population"),
                     if (!is.null(us_pop)) us_pop$value else keep_old("United States", "population")),
  population_as_at = c(if (!is.null(au_pop)) au_pop$as_at else keep_old("Australia", "population_as_at"),
                       if (!is.null(us_pop)) us_pop$as_at else keep_old("United States", "population_as_at")),
  population_note = c("Estimated resident population (ABS)", "Whole country, 50 states and DC (US Census Bureau)")
)
facts$land_sqmi       <- round(facts$land_km2 / KM2_PER_SQMI)
facts$coastline_miles <- round(facts$coastline_km / KM_PER_MILE)

utils::write.csv(facts, facts_file, row.names = FALSE)
print(facts)



# ============================================================
# GEOGRAPHY MAPS (Home > Geography page)
# ============================================================
# Two maps, drawn in the dashboard's house style: navy for Australia, red for
# the United States, light grey land, Source Sans 3, a source line on each.
#   australia_over_us.png          Australia over the contiguous US, same scale
#   US-Australia distance map.png  great-circle distances across the Pacific
# Boundaries: Natural Earth (1:50m countries, 1:50m states and provinces).
# ------------------------------------------------------------

library(sf)
library(ggplot2)
library(ggrepel)
library(rnaturalearth)
library(showtext)

font_add_google("Source Sans 3", "source_sans")
showtext_auto()
showtext_opts(dpi = 300)                     # must match the dpi in ggsave()

MAP_NAVY  <- "#00205B"   # Australia
MAP_RED   <- "#D2232A"   # United States
MAP_INK   <- "#1C2530"
MAP_MUTED <- "#5E6B78"
MAP_LAND  <- "#E9ECEF"
MAP_LINE  <- "#C9CED4"

map_theme <- function() {
  theme_void(base_family = "source_sans", base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 17, colour = MAP_NAVY, margin = margin(b = 3)),
      plot.subtitle = element_text(size = 11.5, colour = MAP_MUTED, margin = margin(b = 8)),
      plot.caption  = element_text(size = 8.5, colour = MAP_MUTED, hjust = 0, lineheight = 1.1,
                                   margin = margin(t = 8)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin     = margin(14, 16, 10, 16)
    )
}

countries <- ne_countries(scale = 50, returnclass = "sf")
sf_use_s2(FALSE)                               # planar clipping of boxes below


# ---- Map 1: Australia over the contiguous United States, same scale ----------
# Each country is drawn in its own Lambert azimuthal equal-area projection,
# centred on itself, so both keep their true area and close to their true
# shape. Australia is then moved (not resized) to sit over the US.

us_box  <- st_as_sfc(st_bbox(c(xmin = -125, xmax = -66, ymin = 24, ymax = 50), crs = 4326))
au_box  <- st_as_sfc(st_bbox(c(xmin = 112, xmax = 155, ymin = -44.5, ymax = -10), crs = 4326))
US_LAEA <- "+proj=laea +lat_0=39 +lon_0=-96 +datum=WGS84 +units=m"
AU_LAEA <- "+proj=laea +lat_0=-26 +lon_0=134 +datum=WGS84 +units=m"

usa48 <- suppressWarnings(st_intersection(st_make_valid(countries[countries$admin == "United States of America", ]), us_box))
aus   <- suppressWarnings(st_intersection(st_make_valid(countries[countries$admin == "Australia", ]), au_box))
prov  <- st_as_sf(rnaturalearthdata::states50)   # 1:50m states and provinces (no rnaturalearthhires needed)
prov  <- prov[prov$admin %in% c("United States of America", "Australia"), ]
us_states <- suppressWarnings(st_intersection(st_make_valid(prov[prov$admin == "United States of America", ]), us_box))
au_states <- suppressWarnings(st_intersection(st_make_valid(prov[prov$admin == "Australia", ]), au_box))

# Project, then drop the CRS so Australia can be moved freely in metres
to_plane <- function(x, crs) st_set_crs(st_geometry(st_transform(x, crs)), NA)
usa_p   <- to_plane(usa48, US_LAEA)
uss_p   <- to_plane(us_states, US_LAEA)
aus_p   <- to_plane(aus, AU_LAEA)
aust_p  <- to_plane(au_states, AU_LAEA)
shift   <- st_coordinates(st_centroid(st_union(usa_p))) - st_coordinates(st_centroid(st_union(aus_p)))
shift   <- as.numeric(shift) + c(150e3, -120e3)  # small nudge so the two sit evenly over each other
aus_s   <- aus_p + shift
aust_s  <- aust_p + shift

land <- function(country) {
  v <- if (exists("facts") && !is.null(facts)) facts$land_km2[facts$country == country] else numeric(0)
  if (length(v) && !is.na(v)) sprintf("%.2f million km²", v / 1e6) else ""
}
bb <- st_bbox(c(usa_p, aus_s))
# Labels sit beside the map: Australia on the left, the US on the right
bw <- bb[["xmax"]] - bb[["xmin"]]; bh <- bb[["ymax"]] - bb[["ymin"]]
lab_us <- data.frame(x = bb[["xmax"]] + 0.03 * bw, y = bb[["ymin"]] + 0.62 * bh,
                     label = paste0("CONTIGUOUS\nUNITED STATES\n", land("United States")))
lab_au <- data.frame(x = bb[["xmin"]] - 0.03 * bw, y = bb[["ymin"]] + 0.35 * bh,
                     label = paste0("AUSTRALIA\n", land("Australia")))

map_same_scale <- ggplot() +
  geom_sf(data = usa_p, fill = "#F3F4F6", colour = NA) +
  geom_sf(data = uss_p, fill = NA, colour = MAP_LINE, linewidth = 0.25) +
  geom_sf(data = usa_p, fill = NA, colour = MAP_RED, linewidth = 0.55) +
  geom_sf(data = aus_s, fill = MAP_NAVY, alpha = 0.55, colour = NA) +
  geom_sf(data = aust_s, fill = NA, colour = "white", linewidth = 0.3, alpha = 0.8) +
  geom_sf(data = aus_s, fill = NA, colour = MAP_NAVY, linewidth = 0.55) +
  geom_text(data = lab_us, aes(x, y, label = label), hjust = 0, vjust = 0.5, family = "source_sans",
            fontface = "bold", size = 3.6, colour = MAP_RED, lineheight = 1.05) +
  geom_text(data = lab_au, aes(x, y, label = label), hjust = 1, vjust = 0.5, family = "source_sans",
            fontface = "bold", size = 3.6, colour = MAP_NAVY, lineheight = 1.05) +
  coord_sf(datum = NA, xlim = c(bb[["xmin"]] - 0.27 * bw, bb[["xmax"]] + 0.27 * bw),
           ylim = c(bb[["ymin"]] - 0.03 * bh, bb[["ymax"]] + 0.03 * bh), expand = FALSE) +
  labs(
    title    = "Australia and the contiguous United States at the same scale",
    subtitle = "Australia (navy) laid over the 48 contiguous states and DC (red outline), each in an equal-area projection",
    caption  = paste0("Boundaries: Natural Earth (1:50m). Land areas: CIA World Factbook (Australia); US Census Bureau ",
                      "(contiguous states and DC).\nEach country is drawn in its own Lambert azimuthal equal-area ",
                      "projection; Australia is moved, not resized. Alaska and Hawaii are not shown.")
  ) +
  map_theme()

ggsave("australia_over_us.png", map_same_scale, width = 10, height = 6.4, dpi = 300, bg = "white")


# ---- Map 2: distances across the Pacific -------------------------------------
# Equal Earth projection centred on the Pacific (180 degrees). Routes are
# great circles; each is labelled with its length in miles and kilometres.

PAC_CRS <- "+proj=eqearth +lon_0=180 +datum=WGS84 +units=m"

# Great-circle distance in statute miles (haversine)
miles_between <- function(lon1, lat1, lon2, lat2, r = 3958.8) {
  d2r  <- pi / 180
  dlat <- (lat2 - lat1) * d2r
  dlon <- (lon2 - lon1) * d2r
  a <- sin(dlat / 2)^2 + cos(lat1 * d2r) * cos(lat2 * d2r) * sin(dlon / 2)^2
  2 * r * asin(pmin(1, sqrt(a)))
}
# Points along the great-circle path (longitude kept continuous across 180)
gc_path <- function(lon1, lat1, lon2, lat2, n = 120) {
  d2r <- pi / 180
  to_xyz <- function(lon, lat) c(cos(lat * d2r) * cos(lon * d2r), cos(lat * d2r) * sin(lon * d2r), sin(lat * d2r))
  p1 <- to_xyz(lon1, lat1); p2 <- to_xyz(lon2, lat2)
  omega <- acos(pmin(1, sum(p1 * p2)))
  t <- seq(0, 1, length.out = n)
  pts <- sapply(t, function(f) (sin((1 - f) * omega) * p1 + sin(f * omega) * p2) / sin(omega))
  lon <- atan2(pts[2, ], pts[1, ]) / d2r
  lat <- atan2(pts[3, ], sqrt(pts[1, ]^2 + pts[2, ]^2)) / d2r
  data.frame(lon = ifelse(lon < 0, lon + 360, lon), lat = lat)
}
# Longitude/latitude (any longitude range) to projected x, y
pac_xy <- function(lon, lat) {
  p <- st_transform(st_as_sf(data.frame(lon = ifelse(lon > 180, lon - 360, lon), lat = lat),
                             coords = c("lon", "lat"), crs = 4326), PAC_CRS)
  st_coordinates(p)
}

cities <- data.frame(
  id   = c("perth", "adelaide", "darwin", "canberra",
           "guam", "honolulu", "sandiego", "seattle", "washington"),
  name = c("Perth", "Adelaide", "Darwin", "Canberra",
           "Guam", "Honolulu", "San Diego", "Seattle", "Washington, DC"),
  lon  = c(115.86, 138.60, 130.84, 149.13, 144.75, -157.86, -117.16, -122.33, -77.04),
  lat  = c(-31.95, -34.93, -12.46, -35.28,  13.47,   21.31,   32.72,   47.61,  38.91),
  country = c(rep("Australia", 4), rep("United States", 5))
)
cities[, c("x", "y")] <- pac_xy(cities$lon, cities$lat)

routes <- data.frame(from = c("perth", "perth",    "perth",    "perth",   "canberra"),
                     to   = c("guam",  "honolulu", "sandiego", "seattle", "washington"))
route_lines <- do.call(rbind, lapply(seq_len(nrow(routes)), function(i) {
  a <- cities[cities$id == routes$from[i], ]; b <- cities[cities$id == routes$to[i], ]
  g <- gc_path(a$lon, a$lat, b$lon, b$lat)
  xy <- pac_xy(g$lon, g$lat)
  data.frame(route = i, x = xy[, 1], y = xy[, 2])
}))
routes$miles <- mapply(function(f, t) {
  a <- cities[cities$id == f, ]; b <- cities[cities$id == t, ]
  miles_between(a$lon, a$lat, b$lon, b$lat)
}, routes$from, routes$to)
print(routes)
# Each destination's distance, shown under its name
fmt_dist <- function(m) sprintf("%s mi · %s km from %s", format(round(m, -1), big.mark = ","),
                                format(round(m * 1.609344, -1), big.mark = ","), "%s")
cities$dist <- ""
for (i in seq_len(nrow(routes))) {
  k <- cities$id == routes$to[i]
  cities$dist[k] <- sprintf(fmt_dist(routes$miles[i]), cities$name[cities$id == routes$from[i]])
}

islands <- data.frame(
  name = c("Palau", "Micronesia", "Marshall Is.", "Papua New Guinea", "Solomon Is.", "Vanuatu", "Fiji",
           "Nauru", "Kiribati", "Tuvalu", "Samoa", "Tonga", "New Caledonia (FR)", "French Polynesia (FR)",
           "Cook Is. (NZ)", "Niue (NZ)", "American Samoa (US)"),
  lon  = c(134.62, 158.16, 171.38, 147.18, 159.95, 168.32, 178.44, 166.92, 172.98, 179.20, -171.77, -175.20,
           166.46, -149.57, -159.78, -169.92, -170.70),
  lat  = c(7.50, 6.92, 7.09, -9.44, -9.43, -17.73, -18.14, -0.55, 1.33, -8.52, -13.83, -21.14,
           -22.28, -17.53, -21.21, -19.06, -14.28)
)
islands[, c("x", "y")] <- pac_xy(islands$lon, islands$lat)

# Land, cut at the seam opposite 180 degrees and projected
world <- st_break_antimeridian(st_make_valid(countries), lon_0 = 180)
world <- st_transform(world, PAC_CRS)
# Area shown: 105E to 70W along the equator, 47S to 60N along 180 degrees
# (Equal Earth draws parallels as straight lines, so this is a clean box)
ex <- pac_xy(c(105, 290), c(0, 0))[, 1]
ey <- pac_xy(c(180, 180), c(-47, 60))[, 2]
view <- c(xmin = ex[1], xmax = ex[2], ymin = ey[1], ymax = ey[2])
u <- (view[["xmax"]] - view[["xmin"]]) / 100           # 1% of the map's width, for label offsets

# Label positions, set by hand so nothing collides: offset (in % of map width)
# and which side of the point the text sits on
lab_pos <- data.frame(
  id    = c("perth", "adelaide", "darwin", "canberra", "guam", "honolulu", "sandiego", "seattle", "washington"),
  dx    = c(-1.0,     0.0,       -1.0,      0.0,       -1.0,    1.0,       -1.0,        0.0,       -0.5),
  dy    = c( 1.2,    -1.6,        0.0,     -1.6,        1.6,    0.0,        1.0,        1.8,        1.8),
  hjust = c( 1,       0.5,        1,        0.5,        1,      0,          1,          0.5,        1)
)
cities <- merge(cities, lab_pos, by = "id")
cities$lx <- cities$x + cities$dx * u
cities$ly <- cities$y + cities$dy * u
cities$vj <- ifelse(cities$dy < 0, 1, 0)   # below the point: hang down; otherwise sit above
grat  <- st_transform(st_graticule(lon = seq(90, 300, 30) |> (\(l) ifelse(l > 180, l - 360, l))(),
                                   lat = seq(-60, 60, 30)), PAC_CRS)

map_pacific <- ggplot() +
  geom_sf(data = grat, colour = "#EEF1F4", linewidth = 0.3) +
  geom_sf(data = world, fill = MAP_LAND, colour = "white", linewidth = 0.2) +
  geom_path(data = route_lines, aes(x, y, group = route), colour = MAP_MUTED, linewidth = 0.45) +
  geom_point(data = islands, aes(x, y), shape = 21, size = 1.3, fill = "white", colour = MAP_MUTED, stroke = 0.35) +
  geom_text_repel(data = islands, aes(x, y, label = name), family = "source_sans", size = 2.5,
                  colour = MAP_MUTED, fontface = "italic", segment.colour = "#B5BCC4", segment.size = 0.2,
                  box.padding = 0.25, point.padding = 0.15, min.segment.length = 0.3, seed = 3,
                  bg.colour = "white", bg.r = 0.1, max.overlaps = Inf) +
  geom_point(data = cities, aes(x, y, colour = country), size = 2.3) +
  # City name, with the distance on a second line for destinations
  geom_text(data = cities, aes(lx, ly, label = ifelse(dist == "", name, paste0(name, "\n ")), colour = country,
                               hjust = hjust, vjust = vj),
            family = "source_sans", fontface = "bold", size = 3.4, lineheight = 0.95) +
  geom_text(data = cities[cities$dist != "", ], aes(lx, ly, label = paste0(" \n", dist), hjust = hjust, vjust = vj),
            family = "source_sans", size = 2.9, colour = MAP_INK, lineheight = 0.95) +
  scale_colour_manual(values = c(Australia = MAP_NAVY, `United States` = MAP_RED), guide = "none") +
  coord_sf(crs = PAC_CRS, xlim = c(view[["xmin"]], view[["xmax"]]), ylim = c(view[["ymin"]], view[["ymax"]]),
           expand = FALSE, datum = NA) +
  labs(
    title    = "Distances across the Pacific",
    subtitle = "Great-circle distances from Perth and Canberra to US cities and territories. Australia in navy, the United States in red.",
    caption  = paste0("Boundaries: Natural Earth (1:50m). Distances are great-circle (shortest path over the earth's surface), ",
                      "in statute miles and kilometres.\nGrey markers show Pacific island capitals. ",
                      "Equal Earth projection centred on the Pacific.")
  ) +
  map_theme()

# Image shaped to the map: panel height from its aspect ratio, plus room for the title and caption
map_h <- 11.4 * (view[["ymax"]] - view[["ymin"]]) / (view[["xmax"]] - view[["xmin"]]) + 1.55
ggsave("US-Australia distance map.png", map_pacific, width = 12, height = map_h, dpi = 300, bg = "white")
showtext_auto(FALSE)
