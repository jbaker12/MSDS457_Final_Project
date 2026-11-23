# travel.R ----------------------------------------------------------------
# Travel is estimated as great-circle mileage from a team's home airport to
# the venue. The default itinerary model chains consecutive road games; the
# round-trip model is retained as a sensitivity check.

# .itinerary_trip_miles ---------------------------------------------------
# Allocate travel to each team-game when consecutive road games are treated
# as one trip. The first road game starts at the team's home airport; later
# road games start at the previous venue. The return leg is charged to the
# last road game before the team plays at home (or to the season finale).
.itinerary_trip_miles <- function(long, home_lat, home_lon,
                                  venue_lat, venue_lon) {
  n <- nrow(long)
  out <- rep(NA_real_, n)

  season <- if ("season" %in% names(long)) long$season else rep(NA, n)
  key <- .group_key(list(season, long$team))
  ord <- order(key, long$gameday, long$game_id)
  groups <- split(ord, key[ord], drop = TRUE)

  for (idx in groups) {
    at_home <- long$at_home[idx]
    team_lat <- home_lat[idx]
    team_lon <- home_lon[idx]
    dest_lat <- venue_lat[idx]
    dest_lon <- venue_lon[idx]
    m <- length(idx)

    # Only chain consecutive weeks. A bye (or a long gap) resets the team to
    # its home airport, because clubs normally return home during the bye.
    if ("week" %in% names(long)) {
      consecutive <- c(FALSE, diff(as.numeric(long$week[idx])) == 1)
    } else {
      gap_days <- c(Inf, diff(as.numeric(as.Date(long$gameday[idx]))))
      consecutive <- gap_days <= 10
    }
    link <- c(FALSE, !at_home[-m] & !at_home[-1] & consecutive[-1])

    origin_lat <- dest_lat
    origin_lon <- dest_lon
    for (j in seq_len(m)) {
      if (at_home[j]) {
        origin_lat[j] <- team_lat[j]
        origin_lon[j] <- team_lon[j]
      } else if (link[j]) {
        origin_lat[j] <- dest_lat[j - 1L]
        origin_lon[j] <- dest_lon[j - 1L]
      } else {
        origin_lat[j] <- team_lat[j]
        origin_lon[j] <- team_lon[j]
      }
    }

    inbound <- .haversine_miles(origin_lat, origin_lon, dest_lat, dest_lon)
    inbound[at_home] <- 0

    outbound <- rep(0, m)
    for (j in seq_len(m)) {
      returns_home <- j == m || at_home[j + 1L] ||
        (j < m && !link[j + 1L])
      if (!at_home[j] && returns_home) {
        outbound[j] <- .haversine_miles(
          dest_lat[j], dest_lon[j], team_lat[j], team_lon[j]
        )
      }
    }

    out[idx] <- inbound + outbound
  }

  out
}

# calculate_travel_miles --------------------------------------------------
#' Calculate per-team travel miles for each game
#'
#' Builds one row per team per game and estimates travel from the team's home
#' airport to the venue. Home games are zero miles; neutral-site games require
#' both teams to travel. Venue coordinates come from [nfl_stadiums] via
#' `stadium_id`, falling back to the home team's airport when the venue is
#' unknown and the game is a normal home game.
#'
#' @param schedule A data frame with `game_id`, `home_team`, and `away_team`.
#'   `stadium_id` identifies the venue; `location` (`"Home"`/`"Neutral"`)
#'   determines whether the designated home team travels. `season`, `week`,
#'   and `gameday` are carried through when present. `gameday` is required for
#'   the default `"itinerary"` model.
#' @param team_home Team home-base lookup: a data frame with `team`,
#'   `airport`, `latitude`, and `longitude`. Defaults to [nfl_airports].
#' @param venues Venue lookup: a data frame with `stadium_id`, `stadium`,
#'   `latitude`, and `longitude`. Defaults to [nfl_stadiums].
#' @param trip_model How to count a road trip. `"itinerary"` (default) chains
#'   consecutive road and neutral-site games: a team that stays on the road is
#'   charged the leg from its previous venue, and the return leg is charged to
#'   the last game before it returns home. `"round_trip"` treats every road or
#'   neutral game as an independent out-and-back trip, which overstates mileage
#'   for multi-game road stretches.
#'
#' @return A tibble with one row per team per game: `game_id`, `season`,
#'   `week`, `gameday`, `team`, `opponent`, `is_home`, `location`,
#'   `venue_stadium_id`, `venue`, `venue_source`, `home_airport`,
#'   `trip_miles`, `round_trip`, and `travel_model`.
#'
#' @details
#' `trip_miles` is the mileage attributed to a team-game. Under the itinerary
#' model, the final game of a road trip includes the flight home, so summing
#' `trip_miles` over a season gives the full itinerary. Consecutive road games
#' in adjacent weeks are chained; a bye or a longer gap resets the team to its
#' home airport, because clubs normally return home during the bye. Distances
#' are great-circle (haversine) miles, not actual flight routing.
#'
#' `round_trip` is a legacy flag: `TRUE` means the team left its home base,
#' including a neutral-site game in which it was the designated home team. It
#' does not mean the `"round_trip"` model was used; see `travel_model`.
#'
#' @examples
#' games <- subset(nfl_schedules_sample, game_id %in% c("2024_01_GB_PHI", "2024_02_BUF_MIA"))
#' travel <- calculate_travel_miles(games)
#' travel[, c("game_id", "team", "venue", "trip_miles")]
#'
#' # Compare the itinerary with the older out-and-back assumption
#' calculate_travel_miles(
#'   subset(nfl_schedules_sample, home_team == "JAX" | away_team == "JAX"),
#'   trip_model = "round_trip"
#' )
#' @export
calculate_travel_miles <- function(schedule, team_home = nfl_airports,
                                   venues = nfl_stadiums,
                                   trip_model = c("itinerary", "round_trip")) {
  trip_model <- match.arg(trip_model)
  .check_frame(schedule, c("game_id", "home_team", "away_team"))
  .check_frame(team_home, c("team", "airport", "latitude", "longitude"),
               arg = "team_home")
  .check_frame(venues, c("stadium_id", "stadium", "latitude", "longitude"),
               arg = "venues")

  games <- tibble::as_tibble(schedule)
  .check_rows(games)
  .check_game_ids(games)
  if (trip_model == "itinerary" && !"gameday" %in% names(games)) {
    cli::cli_abort(
      c(
        "`gameday` is required for the itinerary travel model.",
        "i" = "Use {.code trip_model = \"round_trip\"} for date-free input."
      )
    )
  }
  .check_team_lookup(games, team_home, hint = TRUE)

  venue <- .game_venue_lookup(games, team_home, venues)
  if (any(venue$suspect)) {
    .warn_affected(
      unique(games$game_id[venue$suspect]),
      "Neutral-site games are recorded at the home team's own stadium; travel miles are `NA`.",
      "Upstream venue data is stale; supply the real {.field stadium_id}."
    )
  }
  if (any(venue$missing_coordinates)) {
    .warn_affected(
      unique(games$game_id[venue$missing_coordinates]),
      "Neutral-site games without venue coordinates; travel miles are `NA`.",
      "Add the venue to `venues` or set {.field stadium_id}."
    )
  }

  long <- .game_team_long(games, keep = c("season", "week", "gameday"))
  long$location <- rep(venue$location, 2L)
  long$stadium_id <- rep(venue$stadium_id, 2L)
  long$venue_stadium_id <- rep(venue$venue_stadium_id, 2L)
  long$venue <- rep(venue$venue, 2L)
  long$venue_source <- rep(venue$venue_source, 2L)

  team_row <- match(long$team, team_home$team)
  long$home_airport <- team_home$airport[team_row]
  long$at_home <- long$is_home & rep(venue$at_home, 2L)
  long$round_trip <- !long$at_home
  long$travel_model <- trip_model

  home_lat <- team_home$latitude[team_row]
  home_lon <- team_home$longitude[team_row]
  venue_lat <- rep(venue$venue_latitude, 2L)
  venue_lon <- rep(venue$venue_longitude, 2L)
  one_way <- .haversine_miles(home_lat, home_lon, venue_lat, venue_lon)

  if (trip_model == "round_trip") {
    long$trip_miles <- ifelse(long$round_trip, 2 * one_way, 0)
    long$trip_miles[!long$round_trip & is.na(one_way)] <- 0
  } else {
    long$trip_miles <- .itinerary_trip_miles(
      long, home_lat, home_lon, venue_lat, venue_lon
    )
  }
  long$trip_miles[rep(venue$suspect, 2L)] <- NA_real_

  out <- long[, c(
    intersect(c("game_id", "season", "week", "gameday"), names(long)),
    "team", "opponent", "is_home", "location", "venue_stadium_id", "venue",
    "venue_source", "home_airport", "trip_miles", "round_trip", "travel_model"
  )]
  out <- out[order(out$game_id, !out$is_home), ]
  tibble::as_tibble(out)
}

# summarize_travel_miles --------------------------------------------------
#' Summarise travel miles for each team
#'
#' Aggregates the per-trip frame produced by [calculate_travel_miles()] into
#' season totals, mean road-trip length, the longest single trip, and the
#' furthest opponent visited. With the default itinerary travel model, totals
#' include the return leg charged to the final game of each road trip.
#'
#' @param travel A per-team-game travel frame from
#'   [calculate_travel_miles()], or any data frame with `team` and
#'   `trip_miles`. `is_home`, `round_trip`, and `opponent` are used when
#'   present.
#' @param by Character vector of grouping columns. Defaults to `team` plus
#'   `season` when `season` is available.
#'
#' @return A tibble with one row per group: the grouping columns, `games`,
#'   `travel_games` (games played away from home, including neutral-site
#'   "home" games), `total_miles`, `mean_trip_miles`, `longest_trip_miles`,
#'   `furthest_opponent`, and `missing_trips` (games with unknown mileage).
#'
#' @examples
#' travel <- calculate_travel_miles(nfl_schedules_sample)
#' summarize_travel_miles(travel)[1:5, ]
#' @export
summarize_travel_miles <- function(travel, by = NULL) {
  .check_frame(travel, c("team", "trip_miles"), arg = "travel")
  .check_rows(travel, arg = "travel")

  if (is.null(by)) {
    by <- intersect(c("team", "season"), names(travel))
  }
  if (!is.character(by) || !length(by) || !all(by %in% names(travel))) {
    cli::cli_abort("`by` must name columns present in `travel`.")
  }

  has_home <- "is_home" %in% names(travel)
  has_round <- "round_trip" %in% names(travel)
  has_opponent <- "opponent" %in% names(travel)

  out <- .group_rows(travel, by, function(d) {
    miles <- d$trip_miles
    traveled <- if (has_round) d$round_trip else !is.na(miles) & miles > 0

    row <- lapply(by, function(column) d[[column]][1])
    names(row) <- by
    row$games <- nrow(d)
    row$travel_games <- if (has_round) {
      sum(d$round_trip, na.rm = TRUE)
    } else if (has_home) {
      sum(!d$is_home, na.rm = TRUE)
    } else {
      NA_integer_
    }
    row$total_miles <- sum(miles, na.rm = TRUE)
    row$mean_trip_miles <- .mean_or_na(miles[traveled])
    row$longest_trip_miles <- .max_or_na(miles)
    candidates <- which(traveled & !is.na(miles))
    row$furthest_opponent <- if (has_opponent && length(candidates)) {
      d$opponent[candidates[which.max(miles[candidates])]]
    } else {
      NA_character_
    }
    row$missing_trips <- sum(is.na(miles))
    tibble::as_tibble(row)
  })

  out <- out[do.call(order, lapply(by, function(column) out[[column]])), ]
  rownames(out) <- NULL
  out
}
