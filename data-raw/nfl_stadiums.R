# data-raw/nfl_stadiums.R -------------------------------------------------
# Build the {nflrest} venue reference table -> data/nfl_stadiums.rda
#
# One row per stadium that appears in the bundled schedule sample, keyed by
# the nflverse `stadium_id`. Coordinates are the stadium coordinates in
# decimal degrees (WGS84), taken from the Wikipedia article for each venue
# (see `source` below). `time_zone` is an IANA name, so body-clock shifts can
# be computed offline.
#
# The table covers all 30 NFL venues plus the neutral-site venues used by the
# 2024-2025 international series and neutral postseason games. `DUB01`,
# `BER01` and `MAD01` are {nflrest} identifiers: nflverse did not record the
# 2025 Dublin/Berlin/Madrid venues in `load_schedules()` at the time this
# sample was built, so `data-raw/process_data.R` maps those games here.
#
# Run from the package root:
#   source("data-raw/nfl_stadiums.R")

nfl_stadiums <- tibble::tribble(
  ~stadium_id, ~stadium,                        ~city,           ~country,         ~latitude,   ~longitude,  ~time_zone,
  "KAN00",     "Arrowhead Stadium",             "Kansas City",   "United States",   39.0489,    -94.4839,    "America/Chicago",
  "SAO00",     "Arena Corinthians",             "Sao Paulo",     "Brazil",         -23.5453,    -46.4742,    "America/Sao_Paulo",
  "ATL97",     "Mercedes-Benz Stadium",         "Atlanta",       "United States",   33.7556,    -84.4000,    "America/New_York",
  "BUF00",     "Highmark Stadium",              "Orchard Park",  "United States",   42.7731,    -78.7922,    "America/New_York",
  "CHI98",     "Soldier Field",                 "Chicago",       "United States",   41.8623,    -87.6167,    "America/Chicago",
  "CIN00",     "Paycor Stadium",                "Cincinnati",    "United States",   39.0950,    -84.5160,    "America/New_York",
  "IND00",     "Lucas Oil Stadium",             "Indianapolis",  "United States",   39.7601,    -86.1638,    "America/Indiana/Indianapolis",
  "MIA00",     "Hard Rock Stadium",             "Miami Gardens", "United States",   25.9581,    -80.2389,    "America/New_York",
  "NOR00",     "Caesars Superdome",             "New Orleans",   "United States",   29.9508,    -90.0811,    "America/Chicago",
  "NYC01",     "MetLife Stadium",               "East Rutherford","United States",  40.8136,    -74.0744,    "America/New_York",
  "LAX01",     "SoFi Stadium",                  "Inglewood",     "United States",   33.9530,   -118.3390,    "America/Los_Angeles",
  "SEA00",     "Lumen Field",                   "Seattle",       "United States",   47.5952,   -122.3316,    "America/Los_Angeles",
  "CLE00",     "Huntington Bank Field",         "Cleveland",     "United States",   41.5061,    -81.6994,    "America/New_York",
  "TAM00",     "Raymond James Stadium",         "Tampa",         "United States",   27.9758,    -82.5033,    "America/New_York",
  "DET00",     "Ford Field",                    "Detroit",       "United States",   42.3400,    -83.0456,    "America/Detroit",
  "SFO01",     "Levi's Stadium",                "Santa Clara",   "United States",   37.4030,   -121.9700,    "America/Los_Angeles",
  "BAL00",     "M&T Bank Stadium",              "Baltimore",     "United States",   39.2781,    -76.6228,    "America/New_York",
  "CAR00",     "Bank of America Stadium",       "Charlotte",     "United States",   35.2258,    -80.8528,    "America/New_York",
  "DAL00",     "AT&T Stadium",                  "Arlington",     "United States",   32.7478,    -97.0928,    "America/Chicago",
  "GNB00",     "Lambeau Field",                 "Green Bay",     "United States",   44.5014,    -88.0622,    "America/Chicago",
  "JAX00",     "EverBank Stadium",              "Jacksonville",  "United States",   30.3239,    -81.6375,    "America/New_York",
  "MIN01",     "U.S. Bank Stadium",             "Minneapolis",   "United States",   44.9740,    -93.2580,    "America/Chicago",
  "BOS00",     "Gillette Stadium",              "Foxborough",    "United States",   42.0910,    -71.2640,    "America/New_York",
  "NAS00",     "Nissan Stadium",                "Nashville",     "United States",   36.1664,    -86.7714,    "America/Chicago",
  "WAS00",     "Northwest Stadium",             "Landover",      "United States",   38.9078,    -76.8644,    "America/New_York",
  "PHO00",     "State Farm Stadium",            "Glendale",      "United States",   33.5280,   -112.2630,    "America/Phoenix",
  "DEN00",     "Empower Field at Mile High",    "Denver",        "United States",   39.7439,   -105.0200,    "America/Denver",
  "HOU00",     "NRG Stadium",                   "Houston",       "United States",   29.6847,    -95.4108,    "America/Chicago",
  "PHI00",     "Lincoln Financial Field",       "Philadelphia",  "United States",   39.9008,    -75.1675,    "America/New_York",
  "PIT00",     "Acrisure Stadium",              "Pittsburgh",    "United States",   40.4467,    -80.0158,    "America/New_York",
  "VEG00",     "Allegiant Stadium",             "Paradise",      "United States",   36.0906,   -115.1839,    "America/Los_Angeles",
  "LON02",     "Tottenham Hotspur Stadium",     "London",        "England",         51.6044,     -0.0664,    "Europe/London",
  "LON00",     "Wembley Stadium",               "London",        "England",         51.5556,     -0.2794,    "Europe/London",
  "GER00",     "Allianz Arena",                 "Munich",        "Germany",         48.2188,     11.6248,    "Europe/Berlin",
  "DUB01",     "Croke Park",                    "Dublin",        "Ireland",         53.3608,     -6.2511,    "Europe/Dublin",
  "BER01",     "Olympiastadion",                "Berlin",        "Germany",         52.5147,     13.2394,    "Europe/Berlin",
  "MAD01",     "Santiago Bernabeu Stadium",     "Madrid",        "Spain",           40.4531,     -3.6883,    "Europe/Madrid"
)

# Stable row order keeps the generated .rda diff-friendly.
nfl_stadiums <- nfl_stadiums[order(nfl_stadiums$stadium_id), ]

stopifnot(
  !anyDuplicated(nfl_stadiums$stadium_id),
  all(nfl_stadiums$latitude >= -90 & nfl_stadiums$latitude <= 90),
  all(nfl_stadiums$longitude >= -180 & nfl_stadiums$longitude <= 180),
  all(nfl_stadiums$time_zone %in% OlsonNames())
)

if (!dir.exists("data")) dir.create("data")
save(nfl_stadiums, file = "data/nfl_stadiums.rda", compress = "xz", version = 3)
