# season_summary.R --------------------------------------------------------
# Season-level roll-up of rest, travel, body-clock, and composite advantage.

# .rank_within ------------------------------------------------------------
.rank_within <- function(x, group) {
  out <- rep(NA_real_, length(x))
  keys <- unique(group)
  for (key in keys) {
    idx <- if (is.na(key)) which(is.na(group)) else which(group == key)
    out[idx] <- rank(-x[idx], ties.method = "min", na.last = "keep")
  }
  out
}

# calc_schedule_disparity -------------------------------------------------
#' Summarise schedule disparity by team and season
#'
#' Rolls the game-level components from [calculate_schedule_advantage()] up to
#' one row per team-season. The result is the season-level companion to the
#' per-game composite: it shows how much rest, travel, and body-clock burden a
#' team accumulated, and where it ranked relative to the rest of the league.
#'
#' @param schedule A schedule accepted by [calculate_schedule_advantage()].
#' @param team_home Team home-base lookup. Defaults to [nfl_airports].
#' @param venues Venue lookup. Defaults to [nfl_stadiums].
#' @param trip_model Travel model passed to [calculate_travel_miles()].
#'   Defaults to `"itinerary"`.
#' @param ... Additional arguments passed to
#'   [calculate_schedule_advantage()], such as component weights.
#'
#' @return A tibble with one row per team-season: `season`, `team`, `games`,
#'   `home_games`, `away_games`, `travel_games`, `total_travel_miles`,
#'   `mean_trip_miles`, `longest_trip_miles`, `missing_trips`, `short_weeks`,
#'   `off_byes`, `mean_rest_days`, `mean_rest_disparity`, `eastward_trips`,
#'   `westward_trips`, `mean_tz_shift`, `max_eastward_shift`,
#'   `mean_schedule_advantage`, `total_schedule_advantage`, `advantage_rank`,
#'   `travel_rank`, and `rest_rank`.
#'
#' @details
#' Ranks are within season. `advantage_rank = 1` is the most favorable
#' schedule (highest mean `schedule_advantage`), `travel_rank = 1` is the most
#' travel, and `rest_rank = 1` is the most rest relative to opponents.
#' `mean_schedule_advantage` is from the team's perspective: the game-level
#' home-positive composite is negated for road games.
#'
#' @examples
#' season <- calc_schedule_disparity(nfl_schedules_sample)
#' subset(season, season == 2024)[1:5, c(
#'   "team", "total_travel_miles", "mean_schedule_advantage", "advantage_rank"
#' )]
#' @export
calc_schedule_disparity <- function(
    schedule, team_home = nfl_airports, venues = nfl_stadiums,
    trip_model = c("itinerary", "round_trip"), ...) {
  trip_model <- match.arg(trip_model)
  adv <- calculate_schedule_advantage(
    schedule, team_home = team_home, venues = venues,
    trip_model = trip_model, ...
  )

  n <- nrow(adv)
  location <- if ("location" %in% names(adv)) {
    as.character(adv$location)
  } else {
    rep("Home", n)
  }

  long <- .game_team_long(adv, keep = "season")
  if (!"season" %in% names(long)) {
    long$season <- NA
  }
  long$location <- rep(location, 2L)
  long$rest_days <- c(adv$home_rest_days, adv$away_rest_days)
  long$rest_disparity <- c(adv$rest_disparity, -adv$rest_disparity)
  long$short_week <- c(adv$home_short_week, adv$away_short_week)
  long$off_bye <- c(adv$home_off_bye, adv$away_off_bye)
  long$trip_miles <- c(adv$home_trip_miles, adv$away_trip_miles)
  long$tz_shift <- c(adv$home_tz_shift_hours, adv$away_tz_shift_hours)
  long$body_penalty <- c(
    adv$home_body_clock_penalty, adv$away_body_clock_penalty
  )
  long$advantage <- c(adv$schedule_advantage, -adv$schedule_advantage)
  long$traveled <- ifelse(long$is_home, long$location != "Home", TRUE)

  out <- .group_rows(long, c("season", "team"), function(d) {
    east <- d$tz_shift[!is.na(d$tz_shift) & d$tz_shift > 0]
    tibble::tibble(
      season = d$season[1],
      team = d$team[1],
      games = nrow(d),
      home_games = sum(d$is_home),
      away_games = sum(!d$is_home),
      travel_games = sum(d$traveled, na.rm = TRUE),
      total_travel_miles = .sum_or_na(d$trip_miles),
      mean_trip_miles = .mean_or_na(d$trip_miles[d$traveled]),
      longest_trip_miles = .max_or_na(d$trip_miles),
      missing_trips = sum(is.na(d$trip_miles)),
      short_weeks = sum(d$short_week, na.rm = TRUE),
      off_byes = sum(d$off_bye, na.rm = TRUE),
      mean_rest_days = .mean_or_na(d$rest_days),
      mean_rest_disparity = .mean_or_na(d$rest_disparity),
      eastward_trips = sum(d$tz_shift > 0, na.rm = TRUE),
      westward_trips = sum(d$tz_shift < 0, na.rm = TRUE),
      mean_tz_shift = .mean_or_na(d$tz_shift),
      max_eastward_shift = if (length(east)) max(east) else 0,
      mean_schedule_advantage = .mean_or_na(d$advantage),
      total_schedule_advantage = .sum_or_na(d$advantage)
    )
  })

  out$advantage_rank <- .rank_within(out$mean_schedule_advantage, out$season)
  out$travel_rank <- .rank_within(out$total_travel_miles, out$season)
  out$rest_rank <- .rank_within(out$mean_rest_disparity, out$season)
  out <- out[order(out$season, out$team), ]
  rownames(out) <- NULL
  tibble::as_tibble(out)
}
