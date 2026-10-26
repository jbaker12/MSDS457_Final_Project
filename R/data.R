# data.R ------------------------------------------------------------------
# Documentation for datasets shipped with {nflrest}.

#' NFL team home airports
#'
#' A reference table mapping each NFL team to its home (or nearest major)
#' airport, with coordinates and an IANA time zone. Used by
#' [calculate_travel_miles()] as the origin of each team's travel.
#'
#' @format A tibble with 32 rows and 8 variables:
#' \describe{
#'   \item{team}{Three-letter team abbreviation (nflverse convention).}
#'   \item{airport}{IATA airport code.}
#'   \item{airport_name}{Full airport name.}
#'   \item{city}{City served by the airport.}
#'   \item{latitude}{Latitude in decimal degrees (WGS84).}
#'   \item{longitude}{Longitude in decimal degrees (WGS84).}
#'   \item{time_zone}{IANA time zone name (e.g. `"America/Chicago"`).}
#'   \item{home_stadium_id}{The team's usual home venue, joinable to [nfl_stadiums].}
#' }
#' @source Compiled from public IATA/FAA airport reference data; see
#'   `data-raw/nfl_airports.R` for the build script.
#' @examples
#' nfl_airports
#'
#' # A single team's home airport
#' subset(nfl_airports, team == "KC")
"nfl_airports"

#' NFL stadiums and neutral-site venues
#'
#' One row per venue used by the bundled schedule sample, keyed by the
#' nflverse `stadium_id`. Coordinates are stadium coordinates in decimal
#' degrees (WGS84) with an IANA time zone, so travel distance runs with no network access. The table covers all 30 NFL venues plus
#' the international and neutral sites used in 2024-2025.
#'
#' @format A tibble with 37 rows and 7 variables:
#' \describe{
#'   \item{stadium_id}{nflverse stadium identifier (e.g. `"KAN00"`).}
#'   \item{stadium}{Venue name.}
#'   \item{city}{City the venue sits in.}
#'   \item{country}{Country the venue sits in.}
#'   \item{latitude}{Latitude in decimal degrees (WGS84).}
#'   \item{longitude}{Longitude in decimal degrees (WGS84).}
#'   \item{time_zone}{IANA time zone name (e.g. `"Europe/London"`).}
#' }
#' @details
#' `DUB01` (Croke Park), `BER01` (Olympiastadion) and `MAD01` (Santiago
#' Bernabeu Stadium) are {nflrest} identifiers. nflverse did not record the
#' 2025 Dublin, Berlin, and Madrid venues in `load_schedules()` when the
#' bundled sample was built, so `data-raw/process_data.R` maps those games
#' here.
#' @source Coordinates taken from the Wikipedia article for each venue; see
#'   `data-raw/nfl_stadiums.R` for the build script.
#' @examples
#' nfl_stadiums
#'
#' # Venues outside the United States
#' subset(nfl_stadiums, country != "United States")
"nfl_stadiums"

#' NFL schedule sample (2024-2025)
#'
#' The complete 2024 and 2025 NFL schedule, trimmed to the game-level
#' schedule, rest, and venue columns used by the package's analytics.
#' Bundled so examples and tests run with no network access.
#'
#' @format A tibble with 570 rows and 13 variables:
#' \describe{
#'   \item{game_id}{Game identifier (nflverse convention, e.g. `"2024_01_BAL_KC"`).}
#'   \item{season}{Season year.}
#'   \item{game_type}{Game type: `"REG"`, `"WC"`, `"DIV"`, `"CON"`, or `"SB"`.}
#'   \item{week}{Week number within the season.}
#'   \item{gameday}{Game date as an ISO 8601 string; coerce with [as.Date()].}
#'   \item{home_team}{Three-letter home team abbreviation.}
#'   \item{away_team}{Three-letter away team abbreviation.}
#'   \item{home_rest}{Days of rest for the home team entering the game.}
#'   \item{away_rest}{Days of rest for the away team entering the game.}
#'   \item{location}{`"Home"` or `"Neutral"`.}
#'   \item{div_game}{`1` if a divisional matchup, else `0`.}
#'   \item{stadium_id}{Venue identifier, joinable to [nfl_stadiums].}
#'   \item{stadium}{Venue name, joinable to [nfl_stadiums].}
#' }
#' @details
#' `stadium_id` is the venue the game was actually played in, including the
#' 15 neutral-site games (12 international, two Super Bowls, and one
#' wildfire relocation). `location` flags neutral sites, so travel functions
#' know both teams flew. Note `gameday` is stored as an ISO 8601 character
#' string, not a `Date`.
#' @source Retrieved with `nflreadr::load_schedules(seasons = 2024:2025)`;
#'   see `data-raw/process_data.R` for the build script and venue overrides.
#' @examples
#' nfl_schedules_sample
#'
#' # Regular-season games with the largest rest disparity
#' subset(
#'   nfl_schedules_sample,
#'   game_type == "REG" & home_rest - away_rest >= 7
#' )
"nfl_schedules_sample"
