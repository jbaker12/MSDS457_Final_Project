# rest_calculations.R -----------------------------------------------------
# Pure data transformation logic. No network calls, no nflreadr usage.
# Every exported analytical function accepts a data.frame/tibble input so
# tests can run against offline static fixtures.
#
# Status: work in progress.
#   Implemented: calculate_rest_disparity(), calculate_travel_miles(),
#                summarize_travel_miles().
#   Planned:     flag_time_zone_shifts(), calculate_schedule_advantage().
#
# Conventions:
#   * `gameday` may be Date, POSIXct, or ISO 8601 character; it is returned
#     as Date.
#   * Rest is measured in days between a team's games within a season. The
#     first game of a team's season has `NA` rest (nflverse reports 7).
#   * Positive rest disparity favours the home team.
#   * Travel uses an out-and-back model: a team that travels flies home
#     afterwards, so trip miles are twice the one-way great-circle distance.

# .check_frame ------------------------------------------------------------
# Lightweight, dependency-free input validation.
.check_frame <- function(x, cols, arg = "schedule") {
  if (!is.data.frame(x)) {
    cli::cli_abort("`{arg}` must be a data frame or tibble.")
  }
  missing <- setdiff(cols, names(x))
  if (length(missing)) {
    cli::cli_abort(
      c(
        "`{arg}` is missing required columns.",
        "x" = "Missing: {.and {.field {missing}}}."
      )
    )
  }
  invisible(TRUE)
}

# .as_gameday -------------------------------------------------------------
.as_gameday <- function(x, arg = "gameday") {
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (is.character(x)) {
    out <- tryCatch(
      as.Date(x),
      error = function(e) rep(as.Date(NA), length(x))
    )
    bad <- is.na(out) & !is.na(x)
    if (any(bad)) {
      cli::cli_abort(
        c(
          "`{arg}` contains values that are not ISO 8601 dates.",
          "x" = "First bad value: {.val {x[which(bad)[1]]}}."
        )
      )
    }
    return(out)
  }
  cli::cli_abort("`{arg}` must be a Date, POSIXct, or ISO 8601 character vector.")
}

# .haversine_miles --------------------------------------------------------
# Great-circle distance in miles (mean Earth radius 3958.7613 mi).
.haversine_miles <- function(lat1, lon1, lat2, lon2) {
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat / 2)^2 +
    cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  2 * 3958.7613 * asin(pmin(1, sqrt(a)))
}

# calculate_rest_disparity ------------------------------------------------
#' Calculate rest disparity between home and away teams
#'
#' Computes each team's days of rest entering every game from `gameday`, then
#' derives the rest disparity, short-week flags, and off-bye flags. Rest is
#' computed within a season, so the first game of each team's season has
#' `NA` rest (nflverse reports 7 for season openers).
#'
#' @param schedule A data frame with `game_id`, `gameday`, `home_team`, and
#'   `away_team`. `season` is used for grouping when present.
#' @param short_week_days Rest below this many days is flagged as a short
#'   week. Defaults to 6, which catches the 4-5 day Thursday turnarounds.
#' @param bye_rest_days Rest at or above this many days is flagged as coming
#'   off a bye. Defaults to 13, the shortest realistic post-bye rest.
#'
#' @return `schedule` with `gameday` coerced to Date, plus
#'   `home_rest_days`, `away_rest_days`, `rest_disparity`,
#'   `home_short_week`, `away_short_week`, `home_off_bye`, and
#'   `away_off_bye`. Positive `rest_disparity` means the home team is better
#'   rested.
#'
#' @examples
#' games <- subset(nfl_schedules_sample, home_team == "KC" | away_team == "KC")
#' rest <- calculate_rest_disparity(games)
#' head(rest[, c("game_id", "home_rest_days", "away_rest_days", "rest_disparity")])
#' @export
calculate_rest_disparity <- function(schedule, short_week_days = 6,
                                     bye_rest_days = 13) {
  .check_frame(schedule, c("game_id", "gameday", "home_team", "away_team"))

  if (!is.numeric(short_week_days) || length(short_week_days) != 1L ||
      is.na(short_week_days) || short_week_days <= 0) {
    cli::cli_abort("`short_week_days` must be a single positive number.")
  }
  if (!is.numeric(bye_rest_days) || length(bye_rest_days) != 1L ||
      is.na(bye_rest_days) || bye_rest_days <= 0) {
    cli::cli_abort("`bye_rest_days` must be a single positive number.")
  }

  games <- tibble::as_tibble(schedule)
  games$gameday <- .as_gameday(games$gameday)

  if (anyNA(games$gameday)) {
    cli::cli_abort("`gameday` must not contain missing dates.")
  }
  if (anyDuplicated(games$game_id)) {
    cli::cli_abort("`game_id` must uniquely identify games.")
  }

  season <- if ("season" %in% names(games)) games$season else rep(NA, nrow(games))
  n <- nrow(games)

  long <- tibble::tibble(
    game_id = rep(games$game_id, 2L),
    team = c(games$home_team, games$away_team),
    is_home = rep(c(TRUE, FALSE), each = n),
    gameday = rep(games$gameday, 2L),
    season = rep(season, 2L)
  )

  key <- paste(long$season, long$team)
  long <- long[order(key, long$gameday, long$game_id), ]
  long$rest_days <- NA_real_
  for (i in split(seq_len(nrow(long)), paste(long$season, long$team))) {
    long$rest_days[i] <- c(NA_real_, as.numeric(diff(long$gameday[i])))
  }

  home <- long[long$is_home, ]
  away <- long[!long$is_home, ]
  home <- home[match(games$game_id, home$game_id), ]
  away <- away[match(games$game_id, away$game_id), ]

  games$home_rest_days <- home$rest_days
  games$away_rest_days <- away$rest_days
  games$rest_disparity <- games$home_rest_days - games$away_rest_days
  games$home_short_week <- !is.na(games$home_rest_days) &
    games$home_rest_days < short_week_days
  games$away_short_week <- !is.na(games$away_rest_days) &
    games$away_rest_days < short_week_days
  games$home_off_bye <- !is.na(games$home_rest_days) &
    games$home_rest_days >= bye_rest_days
  games$away_off_bye <- !is.na(games$away_rest_days) &
    games$away_rest_days >= bye_rest_days

  games
}

# calculate_travel_miles --------------------------------------------------
#' Calculate per-team travel miles for each game
#'
#' Builds one row per team per game and estimates round-trip travel from the
#' team's home airport to the venue. Home games are zero miles; neutral-site
#' games require both teams to travel. Venue coordinates come from
#' [nfl_stadiums] via `stadium_id`, falling back to the home team's airport
#' when the venue is unknown and the game is a normal home game.
#'
#' @param schedule A data frame with `game_id`, `home_team`, and `away_team`.
#'   `stadium_id` identifies the venue; `location` (`"Home"`/`"Neutral"`)
#'   determines whether the designated home team travels. `season`, `week`,
#'   and `gameday` are carried through when present.
#' @param team_home Team home-base lookup: a data frame with `team`,
#'   `airport`, `latitude`, and `longitude`. Defaults to [nfl_airports].
#' @param venues Venue lookup: a data frame with `stadium_id`, `stadium`,
#'   `latitude`, and `longitude`. Defaults to [nfl_stadiums].
#'
#' @return A tibble with one row per team per game: `game_id`, `season`,
#'   `week`, `gameday`, `team`, `opponent`, `is_home`, `location`,
#'   `venue_stadium_id`, `venue`, `venue_source`, `home_airport`,
#'   `trip_miles`, and `round_trip`.
#'
#' @examples
#' games <- subset(nfl_schedules_sample, game_id %in% c("2024_01_GB_PHI", "2024_02_BUF_MIA"))
#' travel <- calculate_travel_miles(games)
#' travel[, c("game_id", "team", "venue", "trip_miles")]
#' @export
calculate_travel_miles <- function(schedule, team_home = nfl_airports,
                                   venues = nfl_stadiums) {
  .check_frame(schedule, c("game_id", "home_team", "away_team"))
  .check_frame(team_home, c("team", "airport", "latitude", "longitude"),
               arg = "team_home")
  .check_frame(venues, c("stadium_id", "stadium", "latitude", "longitude"),
               arg = "venues")

  games <- tibble::as_tibble(schedule)
  n <- nrow(games)

  if (anyDuplicated(games$game_id)) {
    cli::cli_abort("`game_id` must uniquely identify games.")
  }

  location <- if ("location" %in% names(games)) {
    as.character(games$location)
  } else {
    rep("Home", n)
  }
  location[is.na(location)] <- "Home"

  stadium_id <- if ("stadium_id" %in% names(games)) {
    as.character(games$stadium_id)
  } else {
    rep(NA_character_, n)
  }

  team_contract <- setdiff(
    unique(c(games$home_team, games$away_team)),
    team_home$team
  )
  if (length(team_contract)) {
    cli::cli_abort(
      c(
        "Teams in `schedule` are missing from `team_home`.",
        "x" = "Unknown: {.and {.val {team_contract}}}.",
        "i" = "Historical abbreviations need their own home-airport rows."
      )
    )
  }

  long <- tibble::tibble(
    game_id = rep(games$game_id, 2L),
    team = c(games$home_team, games$away_team),
    opponent = c(games$away_team, games$home_team),
    is_home = rep(c(TRUE, FALSE), each = n),
    location = rep(location, 2L),
    stadium_id = rep(stadium_id, 2L),
    home_team = rep(games$home_team, 2L)
  )
  for (col in c("season", "week", "gameday")) {
    if (col %in% names(games)) {
      long[[col]] <- rep(games[[col]], 2L)
    }
  }

  team_row <- match(long$team, team_home$team)
  venue_row <- match(long$stadium_id, venues$stadium_id)

  venue_lat <- venues$latitude[venue_row]
  venue_lon <- venues$longitude[venue_row]
  venue_name <- venues$stadium[venue_row]

  # A missing venue is only recoverable for normal home games, where the
  # home team's airport is a serviceable stand-in. For neutral games it
  # would silently produce the wrong city, so leave it NA.
  home_team_row <- match(long$home_team, team_home$team)
  fallback <- is.na(venue_lat) & long$location == "Home"
  venue_lat[fallback] <- team_home$latitude[home_team_row[fallback]]
  venue_lon[fallback] <- team_home$longitude[home_team_row[fallback]]
  venue_name[fallback] <- team_home$airport[home_team_row[fallback]]

  long$venue_stadium_id <- long$stadium_id
  long$venue <- venue_name
  long$venue_source <- ifelse(
    !is.na(venue_row), "stadium",
    ifelse(fallback, "home airport", "missing")
  )
  long$home_airport <- team_home$airport[team_row]

  one_way <- .haversine_miles(
    team_home$latitude[team_row], team_home$longitude[team_row],
    venue_lat, venue_lon
  )
  long$round_trip <- !(long$is_home & long$location == "Home")
  long$trip_miles <- ifelse(long$round_trip, 2 * one_way, 0)
  long$trip_miles[!long$round_trip & is.na(one_way)] <- 0

  # A neutral game recorded at the designated home team's own stadium means
  # upstream venue data is stale. nflverse does exactly this for the 2025
  # international series, and silently charging travel to the wrong city is
  # worse than returning NA.
  home_stadium <- if ("home_stadium_id" %in% names(team_home)) {
    team_home$home_stadium_id[home_team_row]
  } else {
    rep(NA_character_, nrow(long))
  }
  suspect <- long$location != "Home" & !is.na(home_stadium) &
    !is.na(long$stadium_id) & long$stadium_id == home_stadium
  if (any(suspect)) {
    long$trip_miles[suspect] <- NA_real_
    long$venue_source[suspect] <- "home venue (neutral game)"
    affected <- unique(long$game_id[suspect])
    cli::cli_warn(
      c(
        "Neutral-site games are recorded at the home team's own stadium; travel miles are `NA`.",
        "i" = "Affected games: {.and {.val {affected}}}.",
        "i" = "Upstream venue data is stale; supply the real {.field stadium_id} (see {.code nfl_stadiums})."
      )
    )
  }

  missing_venue <- long$location != "Home" & long$venue_source == "missing"
  if (any(missing_venue)) {
    affected <- unique(long$game_id[missing_venue])
    cli::cli_warn(
      c(
        "Neutral-site games without venue coordinates; travel miles are `NA`.",
        "i" = "Affected games: {.and {.val {affected}}}.",
        "i" = "Add the venue to `venues` or set {.field stadium_id}."
      )
    )
  }

  out <- long[, c(
    intersect(c("game_id", "season", "week", "gameday"), names(long)),
    "team", "opponent", "is_home", "location", "venue_stadium_id", "venue",
    "venue_source", "home_airport", "trip_miles", "round_trip"
  )]
  out <- out[order(out$game_id, !out$is_home), ]
  tibble::as_tibble(out)
}

# summarize_travel_miles --------------------------------------------------
#' Summarise travel miles for each team
#'
#' Aggregates the per-trip frame produced by [calculate_travel_miles()] into
#' season totals, mean road trip length, the longest single trip, and the
#' furthest opponent visited.
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

  if (is.null(by)) {
    by <- intersect(c("team", "season"), names(travel))
  }
  if (!is.character(by) || !length(by) || !all(by %in% names(travel))) {
    cli::cli_abort("`by` must name columns present in `travel`.")
  }

  if (!nrow(travel)) {
    cli::cli_abort("`travel` must have at least one row.")
  }

  has_home <- "is_home" %in% names(travel)
  has_round <- "round_trip" %in% names(travel)
  has_opponent <- "opponent" %in% names(travel)

  groups <- split(seq_len(nrow(travel)),
                  do.call(paste, c(lapply(by, function(b) travel[[b]]),
                                   sep = "\r")))

  rows <- lapply(groups, function(i) {
    d <- travel[i, , drop = FALSE]
    miles <- d$trip_miles
    traveled <- if (has_round) d$round_trip else !is.na(miles) & miles > 0
    row <- lapply(by, function(b) d[[b]][1])
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
    row$mean_trip_miles <- if (any(traveled, na.rm = TRUE)) {
      mean(miles[traveled], na.rm = TRUE)
    } else {
      NA_real_
    }
    row$longest_trip_miles <- if (any(!is.na(miles))) {
      max(miles, na.rm = TRUE)
    } else {
      NA_real_
    }
    row$furthest_opponent <- if (has_opponent && any(!is.na(miles))) {
      d$opponent[which.max(ifelse(is.na(miles), -Inf, miles))]
    } else {
      NA_character_
    }
    row$missing_trips <- sum(is.na(miles))
    tibble::as_tibble(row)
  })

  out <- do.call(rbind, rows)
  out <- out[do.call(order, lapply(by, function(b) out[[b]])), ]
  rownames(out) <- NULL
  out
}
