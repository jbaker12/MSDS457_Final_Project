# data-raw/process_data.R -------------------------------------------------
# Build the {nflrest} bundled schedule sample -> data/nfl_schedules_sample.rda
#
# A subset of 2024-2025 NFL schedule data pulled with nflreadr, trimmed to the
# schedule/rest/venue columns the package's analytics operate on. Bundling a
# small sample keeps examples and tests runnable with no network access.
#
# nflverse records the designated home team's stadium for neutral-site games
# (and, for the 2025 international series, had not updated the venue at all),
# so `venue_overrides` below maps those games to their actual venue. Without
# it, travel and body-clock math would silently use the wrong city.
#
# Run from the package root, after data-raw/nfl_stadiums.R:
#   source("data-raw/nfl_stadiums.R")
#   source("data-raw/process_data.R")

for (pkg in c("nflreadr", "tibble")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("The data build needs the '", pkg, "' package.", call. = FALSE)
  }
}
if (!file.exists("data/nfl_stadiums.rda")) {
  stop("Run data-raw/nfl_stadiums.R first.", call. = FALSE)
}
load("data/nfl_stadiums.rda")

# 1. Fetch 2024-2025 schedule data
raw <- nflreadr::load_schedules(seasons = 2024:2025)
retrieved <- attr(raw, "nflverse_timestamp")

# 2. Select relevant schedule/rest/venue columns
cols_to_keep <- c(
  "game_id", "season", "game_type", "week", "gameday",
  "home_team", "away_team", "home_rest", "away_rest",
  "location", "div_game", "stadium_id"
)
nfl_schedules_sample <- tibble::as_tibble(as.data.frame(raw)[, cols_to_keep])

# 3. Point neutral-site games at the venue they were actually played in.
#    Keys are game_id, values are stadium_id in data/nfl_stadiums.rda.
venue_overrides <- c(
  "2024_01_GB_PHI"  = "SAO00", # Sao Paulo, Brazil
  "2024_05_NYJ_MIN" = "LON02", # London (Tottenham)
  "2024_06_JAX_CHI" = "LON02", # London (Tottenham)
  "2024_07_NE_JAX"  = "LON00", # London (Wembley)
  "2024_10_NYG_CAR" = "GER00", # Munich, Germany
  "2024_19_MIN_LA"  = "PHO00", # relocated to Glendale, AZ (wildfires)
  "2024_22_KC_PHI"  = "NOR00", # Super Bowl LIX, New Orleans
  "2025_01_KC_LAC"  = "SAO00", # Sao Paulo, Brazil
  "2025_04_MIN_PIT" = "DUB01", # Dublin, Ireland (Croke Park)
  "2025_05_MIN_CLE" = "LON02", # London (Tottenham)
  "2025_06_DEN_NYJ" = "LON02", # London (Tottenham)
  "2025_07_LA_JAX"  = "LON00", # London (Wembley)
  "2025_10_ATL_IND" = "BER01", # Berlin, Germany (Olympiastadion)
  "2025_11_WAS_MIA" = "MAD01", # Madrid, Spain (Bernabeu)
  "2025_22_SEA_NE"  = "SFO01"  # Super Bowl LX, Santa Clara
)

neutral <- nfl_schedules_sample$location != "Home"
unknown_neutral <- setdiff(
  nfl_schedules_sample$game_id[neutral],
  names(venue_overrides)
)
if (length(unknown_neutral)) {
  stop(
    "Neutral-site games without a venue override: ",
    paste(unknown_neutral, collapse = ", "),
    call. = FALSE
  )
}
hit <- match(nfl_schedules_sample$game_id, names(venue_overrides))
nfl_schedules_sample$stadium_id <- ifelse(
  is.na(hit),
  nfl_schedules_sample$stadium_id,
  unname(venue_overrides[hit])
)

# 4. Attach the canonical venue name from the package's own lookup
vid <- match(nfl_schedules_sample$stadium_id, nfl_stadiums$stadium_id)
if (anyNA(vid)) {
  stop(
    "Unknown stadium_id in schedule: ",
    paste(unique(nfl_schedules_sample$stadium_id[is.na(vid)]), collapse = ", "),
    call. = FALSE
  )
}
nfl_schedules_sample$stadium <- nfl_stadiums$stadium[vid]

# 5. Save as bundled package dataset
stopifnot(
  !anyDuplicated(nfl_schedules_sample$game_id),
  !anyNA(nfl_schedules_sample),
  nrow(nfl_schedules_sample) == 570L
)
message(
  "Built nfl_schedules_sample: ", nrow(nfl_schedules_sample), " games, ",
  "nflverse timestamp ", format(retrieved, tz = "UTC"), "."
)
save(nfl_schedules_sample, file = "data/nfl_schedules_sample.rda",
     compress = "xz", version = 3)
