# data-raw/nfl_airports.R -------------------------------------------------
# Build the {nflrest} airport reference table -> data/nfl_airports.rda
#
# One row per NFL team's home (or nearest major) airport. Coordinates are
# decimal degrees (WGS84); `time_zone` is an IANA name. Compiled from public
# IATA/FAA airport reference data so travel-distance and body-clock math can
# run fully offline.
#
# Run from the package root:
#   source("data-raw/nfl_airports.R")

nfl_airports <- tibble::tribble(
  ~team, ~airport, ~airport_name,                              ~city,          ~latitude, ~longitude, ~time_zone,
  "ARI", "PHX",   "Phoenix Sky Harbor International",          "Phoenix",         33.4342,  -112.0116, "America/Phoenix",
  "ATL", "ATL",   "Hartsfield-Jackson Atlanta International",  "Atlanta",         33.6407,   -84.4277, "America/New_York",
  "BAL", "BWI",   "Baltimore/Washington International",        "Baltimore",       39.1774,   -76.6684, "America/New_York",
  "BUF", "BUF",   "Buffalo Niagara International",             "Buffalo",         42.9405,   -78.7322, "America/New_York",
  "CAR", "CLT",   "Charlotte Douglas International",           "Charlotte",       35.2144,   -80.9473, "America/New_York",
  "CHI", "ORD",   "Chicago O'Hare International",              "Chicago",         41.9742,   -87.9073, "America/Chicago",
  "CIN", "CVG",   "Cincinnati/Northern Kentucky International","Cincinnati",      39.0488,   -84.6678, "America/New_York",
  "CLE", "CLE",   "Cleveland Hopkins International",           "Cleveland",       41.4117,   -81.8498, "America/New_York",
  "DAL", "DFW",   "Dallas/Fort Worth International",           "Dallas",          32.8998,   -97.0403, "America/Chicago",
  "DEN", "DEN",   "Denver International",                      "Denver",          39.8561,  -104.6737, "America/Denver",
  "DET", "DTW",   "Detroit Metropolitan Wayne County",         "Detroit",         42.2162,   -83.3554, "America/New_York",
  "GB",  "GRB",   "Green Bay Austin Straubel International",   "Green Bay",       44.4851,   -88.1296, "America/Chicago",
  "HOU", "IAH",   "George Bush Intercontinental",              "Houston",         29.9902,   -95.3368, "America/Chicago",
  "IND", "IND",   "Indianapolis International",                "Indianapolis",    39.7173,   -86.2944, "America/New_York",
  "JAX", "JAX",   "Jacksonville International",                "Jacksonville",    30.4941,   -81.6879, "America/New_York",
  "KC",  "MCI",   "Kansas City International",                 "Kansas City",     39.2976,   -94.7139, "America/Chicago",
  "LA",  "LAX",   "Los Angeles International",                 "Los Angeles",     33.9416,  -118.4085, "America/Los_Angeles",
  "LAC", "LAX",   "Los Angeles International",                 "Los Angeles",     33.9416,  -118.4085, "America/Los_Angeles",
  "LV",  "LAS",   "Harry Reid International",                  "Las Vegas",       36.0840,  -115.1537, "America/Los_Angeles",
  "MIA", "MIA",   "Miami International",                       "Miami",           25.7959,   -80.2870, "America/New_York",
  "MIN", "MSP",   "Minneapolis-Saint Paul International",      "Minneapolis",     44.8848,   -93.2223, "America/Chicago",
  "NE",  "BOS",   "Boston Logan International",                "Boston",          42.3656,   -71.0096, "America/New_York",
  "NO",  "MSY",   "Louis Armstrong New Orleans International", "New Orleans",     29.9934,   -90.2580, "America/Chicago",
  "NYG", "EWR",   "Newark Liberty International",              "Newark",          40.6895,   -74.1745, "America/New_York",
  "NYJ", "EWR",   "Newark Liberty International",              "Newark",          40.6895,   -74.1745, "America/New_York",
  "PHI", "PHL",   "Philadelphia International",                "Philadelphia",    39.8744,   -75.2424, "America/New_York",
  "PIT", "PIT",   "Pittsburgh International",                  "Pittsburgh",      40.4915,   -80.2329, "America/New_York",
  "SEA", "SEA",   "Seattle-Tacoma International",              "Seattle",         47.4502,  -122.3088, "America/Los_Angeles",
  "SF",  "SFO",   "San Francisco International",               "San Francisco",   37.6213,  -122.3790, "America/Los_Angeles",
  "TB",  "TPA",   "Tampa International",                       "Tampa",           27.9755,   -82.5332, "America/New_York",
  "TEN", "BNA",   "Nashville International",                   "Nashville",       36.1263,   -86.6774, "America/Chicago",
  "WAS", "IAD",   "Washington Dulles International",           "Washington",      38.9531,   -77.4565, "America/New_York"
)

# Stable row order keeps the generated .rda diff-friendly.
nfl_airports <- nfl_airports[order(nfl_airports$team), ]

# Usual home venue per team, so neutral-site integrity checks can tell when a
# "Neutral" game is still recorded at the designated home team's own stadium
# (nflverse does this for the 2025 international series).
home_stadiums <- c(
  ARI = "PHO00", ATL = "ATL97", BAL = "BAL00", BUF = "BUF00",
  CAR = "CAR00", CHI = "CHI98", CIN = "CIN00", CLE = "CLE00",
  DAL = "DAL00", DEN = "DEN00", DET = "DET00", GB  = "GNB00",
  HOU = "HOU00", IND = "IND00", JAX = "JAX00", KC  = "KAN00",
  LA  = "LAX01", LAC = "LAX01", LV  = "VEG00", MIA = "MIA00",
  MIN = "MIN01", NE  = "BOS00", NO  = "NOR00", NYG = "NYC01",
  NYJ = "NYC01", PHI = "PHI00", PIT = "PIT00", SEA = "SEA00",
  SF  = "SFO01", TB  = "TAM00", TEN = "NAS00", WAS = "WAS00"
)
nfl_airports$home_stadium_id <- unname(home_stadiums[nfl_airports$team])
nfl_airports <- nfl_airports[, c(
  "team", "airport", "airport_name", "city", "latitude", "longitude",
  "time_zone", "home_stadium_id"
)]

stopifnot(
  nrow(nfl_airports) == 32L,
  !anyDuplicated(nfl_airports$team),
  !anyNA(nfl_airports$home_stadium_id),
  all(nfl_airports$latitude >= -90 & nfl_airports$latitude <= 90),
  all(nfl_airports$longitude >= -180 & nfl_airports$longitude <= 180),
  all(nfl_airports$time_zone %in% OlsonNames())
)

if (!dir.exists("data")) dir.create("data")
save(nfl_airports, file = "data/nfl_airports.rda", compress = "xz", version = 3)