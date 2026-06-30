# ==============================================================================
#  Nature Data Cube — Guided Exploration
#  Bird nest boxes meet greenness (NDVI) on the Veluwe
# ------------------------------------------------------------------------------
#  PURPOSE (please read)
#  This is NOT a coding exercise. It is a guided tour of the Nature Data Cube
#  (NDC), using a real nest-box dataset as motivation.
#  at the end, we ask for your feedback so we can see whether you find the NDC
#  useful, understandable, and intuitive.
#
#  How it works: we start with an ecological question, meet the birds, then visit
#  the NDC to fetch "greenness" (NDVI), look at what it gives us as a table and
#  as a map, and finally link greenness to breeding. Run each block in order
#  (Ctrl/Cmd + Enter); maps and charts appear in the Viewer / Plots pane.
# ==============================================================================


# ==============================================================================
# 0.  SETUP
# ==============================================================================
# Run this block once at the start. It loads the tools and points R at the data.
# install.packages(c("ggplot2","lubridate","sf","terra","leaflet","scales","geojsonio"))

suppressPackageStartupMessages({
  library(ggplot2)    # plotting
  library(lubridate)  # dates
  library(sf)         # vector / polygon
  library(terra)      # NDVI raster
  library(leaflet)    # interactive maps (also provides the %>% pipe)
  library(scales)
  library(geojsonio)  # read/write GeoJSON
})

# ---- Where the downloaded NDC data lives -----------------------------------
data_dir <- "~/Cloud Storage/naa-vre-public/vl-nature-data-cube"   # path to the data in MinIO

f_nest    <- file.path(data_dir, "nest_data.csv")
f_summary <- file.path(data_dir, "download_summary.csv")
f_ndvi    <- file.path(data_dir, "ndvi_statistics_nestboxes_2.csv")
f_polygon <- file.path(data_dir, "nestboxes_2.gpkg")
f_raster  <- file.path(data_dir, "ndvi_geodata_nestboxes_2.tif")

# ---- Colour-blind-safe palette for the three species -----------------------
species_pal <- c(
  "blue tit"        = "#0072B2",
  "great tit"       = "#E69F00",
  "pied flycatcher" = "#009E73"
)

# ---- Helper that prints FEEDBACK prompts clearly ----------------------------
feedback <- function(...) {
  cat("\n", strrep("-", 78), "\n",
      ">>> FEEDBACK: ", paste0(...), "\n",
      strrep("-", 78), "\n", sep = "")
}

# ---- Shared ggplot theme ----------------------------------------------------
theme_ndc <- function() {
  theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(colour = "grey35"),
          panel.grid.minor = element_blank(),
          legend.position = "bottom")
}


# ==============================================================================
# 1.  THE ECOLOGICAL QUESTION  (why we are here)
# ==============================================================================
# Songbirds time breeding to the spring flush of vegetation, which drives the
# caterpillar peak their chicks depend on. So our motivating question is:
#
#     Do birds breed earlier in years when the landscape greens up more,
#     compared to less-green years?
#
# We can't answer that from the birds alone — we need a measure of how green each
# spring was. That is exactly what the Nature Data Cube can give us. Keep this
# question in mind: everything below builds toward answering it in Section 8.


# ==============================================================================
# 2.  BACKGROUND IN ONE BREATH
# ==============================================================================
# A few words so the rest makes sense:
# * Nest box : artificial cavity; here at fixed locations on the Veluwe.
# * Lay date : day the first egg is laid (also given as day-of-year, ld_doy).
# * Clutch   : number of eggs.
# * NDVI     : satellite "greenness", ~0 (bare) to ~1 (dense vegetation).
# The idea we are testing: greener springs -> more insect food -> birds may breed
# earlier and/or lay larger clutches in those years.


# ==============================================================================
# 3.  EXPLORE THE NEST-BOX DATA  (meet the birds)
# ==============================================================================
# First we get to know the birds on their own — how much data we have, what the
# clutches look like, and crucially when they lay. This is the "response" side of
# our question; greenness comes later.
nest <- read.csv(f_nest, stringsAsFactors = FALSE)
nest$lay_date <- as.Date(nest$lay_date)
nest$species  <- factor(nest$species, levels = names(species_pal))

str(nest)   # 2,065 records | 2019-2025 | 439 boxes | 3 species

# ---- 3a. Records per year, per species -------------------------------------
# Are some years better sampled than others? If effort is steady, later
# differences between years reflect the birds, not how hard we looked.
year_counts <- as.data.frame(table(year = nest$year, species = nest$species),
                             responseName = "n")
year_counts$year    <- as.integer(as.character(year_counts$year))
year_counts$species <- factor(year_counts$species, levels = names(species_pal))

p_year <- ggplot(year_counts, aes(year, n, fill = species)) +
  geom_col() +
  scale_fill_manual(values = species_pal, name = NULL) +
  scale_x_continuous(breaks = 2019:2025) +
  labs(title = "Nest records per year",
       subtitle = "Consistent monitoring effort across 2019-2025",
       x = NULL, y = "Number of nests") +
  theme_ndc()
p_year
# Effort is steady, so year-to-year differences reflect the birds, not sampling.

# ---- 3b. Clutch-size distribution ------------------------------------------
# How many eggs do these species typically lay? This is the "how many" half of
# our question, before greenness enters the picture.
p_clutch <- ggplot(nest, aes(clutch_size, fill = species)) +
  geom_histogram(binwidth = 1, colour = "white", alpha = 0.9) +
  facet_wrap(~species, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = species_pal, guide = "none") +
  labs(title = "Clutch-size distribution", subtitle = "Most clutches are 6-12 eggs",
       x = "Clutch size (eggs)", y = "Number of nests") +
  theme_ndc()
p_clutch
# Clutches centre on 8-9 eggs with a long single-digit tail — nothing unusual.

# ---- 3c. Timing of laying --------------------------------------------------
# This is the key plot for our question: when in the year do eggs appear? Each
# line counts the number of nests started in ~5-day windows through the spring.
p_laydate <- ggplot(nest, aes(ld_doy, colour = species)) +
  geom_freqpoly(binwidth = 5, linewidth = 1) +
  scale_colour_manual(values = species_pal, name = NULL) +
  labs(title = "Timing of egg-laying",
       subtitle = "Number of nests started, in 5-day windows",
       x = "Lay date (day of year)", y = "Number of nests") +
  theme_ndc()
p_laydate
# Laying peaks in mid-late April (day ~100-120). That spring window is where we
# will look for a greenness signal later on.

# ---- 3d. Where are the boxes? ----------------------------------------------
# The boxes sit at fixed spots. Their locations matter because in Section 8 we
# will read the greenness right at each box from the NDVI map.
nest_sf <- st_as_sf(nest, coords = c("lon_dd", "lat_dd"), crs = 4326, remove = FALSE)

leaflet(nest_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(lng = ~lon_dd, lat = ~lat_dd, radius = 4, stroke = FALSE,
                   fillOpacity = 0.8,
                   color = ~unname(species_pal[as.character(species)]),
                   popup = ~paste0("<b>Box:</b> ", nestbox_id,
                                   "<br><b>Species:</b> ", species,
                                   "<br><b>Year:</b> ", year,
                                   "<br><b>Clutch:</b> ", clutch_size,
                                   "<br><b>Lay date:</b> ", lay_date)) %>%
  addLegend("bottomright", colors = unname(species_pal),
            labels = names(species_pal), title = "Species", opacity = 1)

feedback("Before opening the NDC: what environmental context would you most ",
         "want it to provide for this area?")


# ==============================================================================
# 4.  VISIT THE NATURE DATA CUBE  (go get the greenness)
# ==============================================================================
# Now we leave the birds for a moment and fetch the environment. Open the NDC and
# *mimic* a download of NDVI for our area. The data is already saved locally, so
# you do NOT need to actually download anything — the point is to experience the
# workflow and judge it.
#
#   Open:  https://lter-life-experience.org/naturedatacube/
#
#   1. Select project: Nestboxes
#   2. Zoom to Arnhem; select the polygon north of the A12 ("De Hoge Veluwe")
#   3. Choose Biosphere, then the NDVI (greenness) dataset
#   4. Select Geodata
#   5. Set the period: 2019-01 -> 2025-12
#   6. Add to overview
#   7. Then also add Statistics for the same area and period
#
#   (Remember: for this exercise you do not actually need to download anything —
#    it is already saved locally.)


# ==============================================================================
# 5.  WHAT THE DOWNLOAD CONTAINS  (read the receipt)
# ==============================================================================
# Every NDC download comes with a manifest that lists exactly what you got. Let's
# read it, so we know which files feed the rest of the tutorial.
download_summary <- read.csv(f_summary, stringsAsFactors = FALSE)
print(download_summary[, c("dataset", "view", "file_type", "status", "note")])

# For this area we received (all three "ok"):
#   * NDVI Statistics (a table of monthly mean greenness)   -> Section 6
#   * NDVI Geodata    (monthly NDVI raster layers)          -> Section 7
#   * the reference polygon (the area outline)


# ==============================================================================
# 6.  STATISTICAL NDVI OUTPUTS  (greenness as a number)
# ==============================================================================
# STORY SO FAR: we have the birds (Section 3) and we have fetched greenness from
# the NDC (Section 4). To connect the two, we now need to actually SEE greenness.
# The NDC gives it to us in two forms — as numbers (this section) and as a map
# (Section 7). We start with the numbers.
#
# This first output is a table: one average greenness value per month for the
# whole area. Let's watch how greenness moves through the year.
ndvi <- read.csv(f_ndvi, stringsAsFactors = FALSE)
ndvi$date    <- ymd(paste0(ndvi$month, "-01"))
ndvi$yr      <- year(ndvi$date)
ndvi$mon     <- month(ndvi$date)
ndvi$mon_lab <- month(ndvi$date, label = TRUE)
ndvi <- ndvi[order(ndvi$date), ]

# ---- 6a. Monthly NDVI time series ------------------------------------------
# The monthly series, with a band for how variable greenness is within the
# area each month.
p_ts <- ggplot(ndvi, aes(date, ndvi_mean)) +
  geom_ribbon(aes(ymin = ndvi_mean - ndvi_std, ymax = ndvi_mean + ndvi_std),
              fill = "#009E73", alpha = 0.15) +
  geom_line(colour = "#009E73", linewidth = 0.8) +
  geom_point(colour = "#009E73", size = 1.4) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Monthly NDVI for the nest-box area",
       subtitle = "Shaded band = +/- 1 SD within the area",
       x = NULL, y = "NDVI (greenness)") +
  theme_ndc()
p_ts
# Clean seasonal cycle: low in winter (~0.5), peaking in summer (~0.77).

# ---- 6b. Seasonal pattern ------------------------------------
# Stacking all years on top of each other shows the typical year. The steep
# March -> May rise is "green-up" — the breeding-relevant window.
ndvi_clim <- aggregate(ndvi_mean ~ mon + mon_lab, data = ndvi, FUN = mean)
names(ndvi_clim)[names(ndvi_clim) == "ndvi_mean"] <- "ndvi"
ndvi_clim <- ndvi_clim[order(ndvi_clim$mon), ]
p_season <- ggplot(ndvi_clim, aes(mon_lab, ndvi, group = 1)) +
  geom_line(colour = "#009E73", linewidth = 0.9) +
  geom_point(colour = "#009E73", size = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Typical greenness through the year",
       subtitle = "Averaged over 2019-2025", x = NULL, y = "Mean NDVI") +
  theme_ndc()
p_season

# ---- 6c. One "spring greenness" number per year ----------------------------
# WHY THIS STEP: in Section 8 we want to ask "in greener springs, do birds lay
# earlier?" To do that we need one number per year that captures how green that
# spring was. So for each year we average the spring months (March, April, May)
# into a single value: the year's "spring NDVI". The bars below are simply those
# seven yearly averages (the y-axis is zoomed in because the year-to-year
# differences are small, but real).
spring_rows <- ndvi[ndvi$mon %in% c(3, 4, 5), ]
spring_ndvi <- aggregate(ndvi_mean ~ yr, data = spring_rows, FUN = mean)
names(spring_ndvi)[names(spring_ndvi) == "ndvi_mean"] <- "spring_ndvi"
p_spring <- ggplot(spring_ndvi, aes(factor(yr), spring_ndvi)) +
  geom_col(fill = "#009E73", alpha = 0.85) +
  coord_cartesian(ylim = range(spring_ndvi$spring_ndvi) + c(-0.02, 0.02)) +
  labs(title = "Early-spring greenness by year",
       subtitle = "Average March-May NDVI (whole area) — one value per year",
       x = NULL, y = "Spring NDVI") +
  theme_ndc()
p_spring

feedback("Are these statistics clear on their own, or do you also need the ",
         "NDVI map to interpret and trust them?")

# BRIDGE: the table gave us greenness over TIME, but only as one number for the
# whole area — it can't tell us WHERE the green is. That is what the map adds.


# ==============================================================================
# 7.  GEOSPATIAL NDVI  (greenness as a map)
# ==============================================================================
# The second NDC output is a raster: greenness for every 10 m pixel, every month
# (layers named NDVI_YYYYMM), already in NDVI units (0-1). The table told us
# greenness over time; the map shows it over space
ndvi_r <- rast(f_raster)

# Which calendar month does each layer belong to? (NDVI_YYYYMM -> the "MM" part)
lyr_mon <- as.integer(substr(names(ndvi_r), 10, 11))

# Clip to the reference polygon so the map shows only the study area.
area_poly <- st_read(f_polygon, quiet = TRUE)
poly_utm  <- st_transform(area_poly, crs(ndvi_r))
ndvi_r    <- mask(crop(ndvi_r, vect(poly_utm)), vect(poly_utm))

# Build one average map per SEASON so we can watch the area green up and fade.
season_months <- list(
  "Winter (Dec-Feb)" = c(12, 1, 2),
  "Spring (Mar-May)" = c(3, 4, 5),
  "Summer (Jun-Aug)" = c(6, 7, 8),
  "Autumn (Sep-Nov)" = c(9, 10, 11)
)
season_maps <- lapply(season_months, function(mm) {
  app(ndvi_r[[which(lyr_mon %in% mm)]], mean, na.rm = TRUE)
})

# One shared green colour scale across all seasons, so they compare honestly.
rng <- range(unlist(lapply(season_maps, function(r) values(r))), na.rm = TRUE)
pal <- colorNumeric(c("#ffffe5","#d9f0a3","#78c679","#238443","#004529"),
                    domain = rng, na.color = "transparent")

m <- leaflet() %>% addProviderTiles("CartoDB.Positron")
for (nm in names(season_maps)) {
  m <- addRasterImage(m, season_maps[[nm]], colors = pal,
                      opacity = 0.85, project = TRUE, group = nm)
}
m %>%
  addCircleMarkers(data = nest_sf, lng = ~lon_dd, lat = ~lat_dd,
                   radius = 3, stroke = TRUE, weight = 1, color = "black",
                   fillColor = ~unname(species_pal[as.character(species)]),
                   fillOpacity = 0.9, group = "Nest boxes",
                   popup = ~paste0("<b>Box:</b> ", nestbox_id,
                                   "<br><b>Species:</b> ", species)) %>%
  addLegend("bottomright", pal = pal, values = rng,
            title = "Mean NDVI", opacity = 1) %>%
  addLayersControl(
    baseGroups = names(season_months),    # one season at a time (radio buttons)
    overlayGroups = "Nest boxes",
    options = layersControlOptions(collapsed = FALSE))
# Use the control (top-right) to step through the seasons and toggle the boxes.
# Watch the whole area brighten from winter to summer — and notice that some
# patches stay greener than others all year (denser / evergreen canopy).


# ==============================================================================
# 8.  DO GREENER CONDITIONS RELATE TO BREEDING?  (the payoff)
# ==============================================================================
# We now hold both halves: the birds (Section 3) and greenness as numbers
# (Section 6) and as a map (Section 7). Time to put them together and answer the
# question we opened with. We look through two lenses, because they tell us
# different things: between years (is a green spring an early spring?) and
# between species (do the three birds differ, and is that about greenness?).

# ---- 8a. BETWEEN YEARS (n = 7) — a weather/phenology signal -----------------
# Line up each year's spring greenness (from 6c) against how early the birds laid
# that year. Hypothesis: greener (warmer) springs -> earlier laying.
agg_mean <- aggregate(ld_doy ~ year, data = nest, FUN = mean)
agg_n    <- aggregate(ld_doy ~ year, data = nest, FUN = length)
annual   <- data.frame(yr           = agg_mean$year,
                       mean_lay_doy = agg_mean$ld_doy,
                       n_nests      = agg_n$ld_doy)
annual   <- merge(annual, spring_ndvi, by = "yr")

ct_t  <- cor.test(annual$spring_ndvi, annual$mean_lay_doy)
note_t <- sprintf("r = %.2f, p = %.2f (n = %d years) — ILLUSTRATIVE ONLY",
                  ct_t$estimate, ct_t$p.value, nrow(annual))
p_t <- ggplot(annual, aes(spring_ndvi, mean_lay_doy)) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey40",
              fill = "grey80", linetype = "dashed") +
  geom_point(aes(size = n_nests), colour = "#0072B2", alpha = .85) +
  geom_text(aes(label = yr), vjust = -1.1, size = 3, colour = "grey30") +
  scale_size_continuous(name = "Nests in that year", range = c(4, 10)) +
  labs(title = "Between years: greener springs, earlier laying",
       subtitle = note_t, x = "Area-mean spring NDVI (Mar-May)",
       y = "Mean lay date (DOY)") +
  theme_ndc()
p_t   # static ggplot, so the point-size legend (sample size) shows; years labelled
# Direction matches the classic phenology expectation, but n = 7 is not enough
# to be significant — we show it to illustrate the kind of analysis the NDC
# enables, not to claim an effect.

# ---- 8b. BETWEEN SPECIES — is timing about greenness, or about the bird? -----
# Using the MAP, we read the spring greenness at each box, then ask: do the three
# species nest in DIFFERENT greenness, and does that explain their very different
# laying times? First, the per-box spring NDVI:
box_loc <- unique(nest[, c("nestbox_id", "lon_dd", "lat_dd")])
box_sf  <- st_as_sf(box_loc, coords = c("lon_dd", "lat_dd"), crs = 4326)
box_sf  <- st_transform(box_sf, crs(ndvi_r))
spring_stack <- ndvi_r[[which(lyr_mon %in% c(3, 4, 5))]]
ex <- terra::extract(spring_stack, vect(box_sf))            # ID + spring layers
box_loc$box_spring_ndvi <- rowMeans(ex[, -1], na.rm = TRUE) # mean spring NDVI/box

# Attach each box's greenness to its nests, then summarise per species (base R):
nest_ndvi <- merge(nest, box_loc[, c("nestbox_id", "box_spring_ndvi")],
                   by = "nestbox_id", all.x = TRUE)
species_summary <- do.call(rbind, lapply(
  split(nest_ndvi, nest_ndvi$species),
  function(d) data.frame(
    species          = d$species[1],
    mean_spring_ndvi = mean(d$box_spring_ndvi, na.rm = TRUE),
    mean_lay_doy     = mean(d$ld_doy),
    mean_clutch      = mean(d$clutch_size),
    n_nests          = nrow(d))))
rownames(species_summary) <- NULL
print(species_summary)

# Plot: one point per species. x = greenness it nests in, y = when it lays,
# point size = clutch. The x-axis is on a real NDVI scale so you can see how
# SIMILAR the three are in greenness, despite very different timing.
p_sp <- ggplot(species_summary, aes(mean_spring_ndvi, mean_lay_doy)) +
  geom_point(aes(size = mean_clutch, colour = species), alpha = 0.9) +
  geom_text(aes(label = species), vjust = -1.8, size = 3.5, colour = "grey25") +
  scale_colour_manual(values = species_pal, guide = "none") +
  scale_size_continuous(name = "Mean clutch (eggs)", range = c(6, 14)) +
  coord_cartesian(xlim = c(0.45, 0.75)) +
  labs(title = "Between species: same greenness, very different timing",
       subtitle = "Each point a species — they overlap on greenness but differ ~18 days in laying",
       x = "Mean spring NDVI where the species nests",
       y = "Mean lay date (DOY)") +
  theme_ndc()
p_sp  # static ggplot, so the point-size legend (clutch) shows; species labelled

# ---- 8c. WHAT THE TWO LENSES TELL US -----------------------
# BETWEEN YEARS, greenness tracks timing: a greener spring is an earlier spring
# for everyone (a weather / phenology response).
# BETWEEN SPECIES, the picture is different: blue tits, great tits and pied
# flycatchers all nest in almost the same greenness, yet pied flycatchers lay
# ~18 days later and ~3 fewer eggs than blue tits. Those differences are about
# the BIRD (its life history), not about local greenness.
# Together: greenness helps explain change over TIME, not the differences
# BETWEEN species.
#
# HONEST CAVEATS (we are scientists):
#   * NDVI is canopy greenness, a PROXY for food — not food itself.
#   * Between-year n = 7 is illustrative; between-species n = 3.
#   * Correlation, not causation. Habitat, weather, and species co-vary.


# ==============================================================================
# 9.  YOUR OVERALL FEEDBACK
# ==============================================================================
cat("
Please jot down a few thoughts:

  1. WORKFLOW    — Did exploring the NDC feel intuitive? Where did it not?
  2. CLARITY     — Did the statistics and the map communicate the data clearly?
  3. MISSING     — What information or output did you expect but not find?
  4. TRUST       — Would you rely on these outputs in your research? Why / why not?
  5. ONE CHANGE  — If you could change ONE thing about the NDC, what would it be?

Thank you for helping us improve the Nature Data Cube.
")
# ==============================================================================
# END
# ==============================================================================
