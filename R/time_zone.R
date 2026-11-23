# time_zone.R -------------------------------------------------------------
# Date-aware body-clock shifts. Kickoff times in nflverse schedules are U.S.
# Eastern time, so the same instant is converted separately to each team's
# home time zone and the venue's time zone.
#
# Conventions:
#   * `gametime` is a 24-hour "HH:MM" string in America/New_York.
#   * A positive `tz_shift_hours` means the team travels east (body clock must
#     advance); negative means west.
#   * Eastward travel is weighted more heavily than westward travel because
#     the circadian clock adapts more slowly when it must phase-advance.

# .parse_gametime ---------------------------------------------------------
.parse_gametime <- function(x, n, default = "13:00") {
  if (is.null(x)) {
    x <- rep(default, n)
  }
  if (length(x) != n) {
    cli::cli_abort("`gametime` must have one value per game.")
  }
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- default
  ok <- grepl("^([01][0-9]|2[0-3]):[0-5][0-9]$", x)
  if (!all(ok)) {
    cli::cli_abort(
      c(
        "`gametime` must use 24-hour {.val HH:MM} format.",
        "x" = "First bad value: {.val {x[which(!ok)[1]]}}."
      )
    )
  }
  x
}

# .kickoff_instant --------------------------------------------------------
.kickoff_instant <- function(gameday, gametime, tz = "America/New_York") {
  as.POSIXct(paste(as.character(gameday), gametime), tz = tz)
}

# .wall_seconds -----------------------------------------------------------
# Wall-clock seconds for an instant in a time zone. Formatting in the target
# zone and re-parsing as UTC turns the local clock reading into a number, so
# subtracting two zones gives the signed clock difference (DST included).
.wall_seconds <- function(instant, tz) {
  if (is.na(instant) || is.na(tz)) {
    return(NA_real_)
  }
  as.numeric(as.POSIXct(format(instant, tz = tz, usetz = FALSE), tz = "UTC"))
}

# .format_local -----------------------------------------------------------
.format_local <- function(instant, tz, fmt = "%Y-%m-%d %H:%M") {
  if (is.na(instant) || is.na(tz)) {
    return(NA_character_)
  }
  format(instant, tz = tz, format = fmt)
}

# flag_time_zone_shifts ---------------------------------------------------
#' Flag body-clock shifts for each team and game
#'
#' Computes the signed time-zone shift a team experiences relative to its home
#' time zone, using the scheduled kickoff instant. The same instant is
#' converted to each team's home zone and the venue zone, so daylight-saving
#' changes are handled by the operating system's time-zone database.
#'
#' @param schedule A data frame with `game_id`, `gameday`, `home_team`, and
#'   `away_team`. `gametime` is used when present; otherwise `default_kickoff`
#'   is assumed. `stadium_id`, `location`, `season`, and `week` are carried
#'   through when present.
#' @param team_home Team home-base lookup with `team` and `time_zone`.
#'   Defaults to [nfl_airports].
#' @param venues Venue lookup with `stadium_id` and `time_zone`. Defaults to
#'   [nfl_stadiums].
#' @param default_kickoff Kickoff time in U.S. Eastern time used when
#'   `gametime` is absent or missing. Defaults to `"13:00"`.
#' @param east_weight Multiplier for eastward clock shifts. Defaults to 1.
#' @param west_weight Multiplier for westward clock shifts. Defaults to 0.5,
#'   since eastward shifts are usually harder on the body clock.
#'
#' @return A tibble with one row per team per game: `game_id`, `season`,
#'   `week`, `gameday`, `team`, `opponent`, `is_home`, `location`, `venue`,
#'   `team_time_zone`, `venue_time_zone`, `kickoff_utc`,
#'   `kickoff_home_local`, `kickoff_venue_local`, `tz_shift_hours`,
#'   `tz_direction`, `eastward`, `westward`, and `body_clock_penalty`.
#'   Positive `tz_shift_hours` means the team travels east.
#'
#' @details
#' A normal home game has zero shift. Neutral-site games can shift the
#' designated home team too, because it is playing away from its home time
#' zone. The body-clock penalty is
#' `east_weight * max(shift, 0) + west_weight * max(-shift, 0)`.
#'
#' @examples
#' games <- subset(nfl_schedules_sample, game_id == "2024_02_SEA_NE")
#' shifts <- flag_time_zone_shifts(games)
#' shifts[, c("team", "team_time_zone", "venue_time_zone", "tz_shift_hours")]
#' @export
flag_time_zone_shifts <- function(schedule, team_home = nfl_airports,
                                  venues = nfl_stadiums,
                                  default_kickoff = "13:00",
                                  east_weight = 1, west_weight = 0.5) {
  .check_frame(schedule, c("game_id", "gameday", "home_team", "away_team"))
  .check_frame(team_home, c("team", "time_zone"), arg = "team_home")
  .check_frame(venues, c("stadium_id", "time_zone"), arg = "venues")
  .check_nonnegative(east_weight, "east_weight")
  .check_nonnegative(west_weight, "west_weight")
  .check_clock(default_kickoff, "default_kickoff")

  games <- tibble::as_tibble(schedule)
  .check_rows(games)
  .check_game_ids(games)
  .check_team_lookup(games, team_home)
  .check_time_zone_column(team_home$time_zone, "team_home$time_zone")
  .check_time_zone_column(
    venues$time_zone, "venues$time_zone", allow_na = TRUE
  )

  games$gameday <- .as_gameday(games$gameday)
  if (anyNA(games$gameday)) {
    cli::cli_abort("`gameday` must not contain missing dates.")
  }

  gametime <- if ("gametime" %in% names(games)) games$gametime else NULL
  gametime <- .parse_gametime(gametime, nrow(games), default_kickoff)
  kickoff <- .kickoff_instant(games$gameday, gametime)
  if (anyNA(kickoff)) {
    cli::cli_abort("`gameday` and `gametime` must combine into valid kickoff times.")
  }

  venue <- .game_venue_lookup(games, team_home, venues)
  if (any(venue$suspect)) {
    .warn_affected(
      unique(games$game_id[venue$suspect]),
      "Neutral-site games are recorded at the home team's own stadium; time-zone shifts are `NA`.",
      "Supply the actual venue {.field stadium_id} to compute the shift."
    )
  }
  if (any(venue$missing_time_zone)) {
    .warn_affected(
      unique(games$game_id[venue$missing_time_zone]),
      "Neutral-site games without a venue time zone; time-zone shifts are `NA`.",
      "Add the venue to `venues` or set {.field stadium_id}."
    )
  }

  long <- .game_team_long(games, keep = c("season", "week", "gameday"))
  long$location <- rep(venue$location, 2L)
  long$venue <- rep(venue$venue, 2L)
  long$team_time_zone <- team_home$time_zone[match(
    long$team, team_home$team
  )]
  long$venue_time_zone <- rep(venue$venue_time_zone, 2L)
  long$kickoff_utc <- rep(as.POSIXct(kickoff, tz = "UTC"), 2L)

  home_wall <- mapply(
    .wall_seconds, long$kickoff_utc, long$team_time_zone,
    USE.NAMES = FALSE
  )
  venue_wall <- mapply(
    .wall_seconds, long$kickoff_utc, long$venue_time_zone,
    USE.NAMES = FALSE
  )

  long$kickoff_home_local <- mapply(
    .format_local, long$kickoff_utc, long$team_time_zone,
    USE.NAMES = FALSE
  )
  long$kickoff_venue_local <- mapply(
    .format_local, long$kickoff_utc, long$venue_time_zone,
    USE.NAMES = FALSE
  )

  long$tz_shift_hours <- (venue_wall - home_wall) / 3600
  long$tz_direction <- ifelse(
    long$tz_shift_hours > 0, "east",
    ifelse(long$tz_shift_hours < 0, "west", "none")
  )
  long$eastward <- pmax(long$tz_shift_hours, 0)
  long$westward <- pmax(-long$tz_shift_hours, 0)
  long$body_clock_penalty <- east_weight * long$eastward +
    west_weight * long$westward

  out <- long[, c(
    intersect(c("game_id", "season", "week", "gameday"), names(long)),
    "team", "opponent", "is_home", "location", "venue", "team_time_zone",
    "venue_time_zone", "kickoff_utc", "kickoff_home_local",
    "kickoff_venue_local", "tz_shift_hours", "tz_direction", "eastward",
    "westward", "body_clock_penalty"
  )]
  out <- out[order(out$game_id, !out$is_home), ]
  tibble::as_tibble(out)
}
