# schedule_advantage.R ----------------------------------------------------
# Composite schedule advantage: rest + travel + body-clock penalties.
#
# Positive values favor the home team. Each component is converted to points
# with an adjustable weight and then summed. The defaults are a starting point,
# not fitted values; eval_rest_impact() is there to check them against results.

# calculate_schedule_advantage --------------------------------------------
#' Calculate a composite schedule advantage
#'
#' Combines rest disparity, travel disparity, and body-clock disparity into a
#' single pre-game score. Positive `schedule_advantage` favors the home team.
#' The raw parts and their point values are kept in the output so the weighting
#' can be checked or changed.
#'
#' @param schedule A data frame accepted by [calculate_rest_disparity()],
#'   [calculate_travel_miles()], and [flag_time_zone_shifts()]. It needs
#'   `game_id`, `gameday`, `home_team`, and `away_team`; `gametime`,
#'   `stadium_id`, and `location` improve accuracy when present.
#' @param team_home Team home-base lookup. Defaults to [nfl_airports].
#' @param venues Venue lookup. Defaults to [nfl_stadiums].
#' @param trip_model Travel model passed to [calculate_travel_miles()].
#'   Defaults to `"itinerary"`.
#' @param points_per_rest_day Points per day of rest disparity. Defaults to
#'   0.25.
#' @param points_per_1000_travel_miles Points per 1,000 miles of extra travel.
#'   Defaults to 0.5.
#' @param points_per_tz_hour Points per hour of body-clock penalty disparity.
#'   Defaults to 0.35.
#' @param east_weight,west_weight Multipliers for eastward and westward clock
#'   shifts passed to [flag_time_zone_shifts()]. Defaults are 1 and 0.5.
#'
#' @return `schedule` with the rest columns from
#'   [calculate_rest_disparity()], plus `home_trip_miles`,
#'   `away_trip_miles`, `travel_disparity`, `home_tz_shift_hours`,
#'   `away_tz_shift_hours`, `home_body_clock_penalty`,
#'   `away_body_clock_penalty`, `tz_penalty_disparity`, `rest_points`,
#'   `travel_points`, `time_zone_points`, `schedule_advantage`,
#'   `components_missing`, and `advantage_category`.
#'
#' @details
#' The default score is
#' `0.25 * rest_disparity + 0.5 * travel_disparity / 1000 +
#' 0.35 * tz_penalty_disparity`, where
#' `travel_disparity = away_trip_miles - home_trip_miles` and
#' `tz_penalty_disparity = away_body_clock_penalty - home_body_clock_penalty`.
#' A missing component (for example, rest in a season opener) contributes zero
#' and increments `components_missing`; the raw component columns keep the `NA`
#' so the omission is visible. The default weights are a starting point, not
#' fitted values. Use [eval_rest_impact()] to compare the components with wins
#' and spread covers.
#'
#' @examples
#' games <- subset(nfl_schedules_sample, game_id == "2024_02_SEA_NE")
#' advantage <- calculate_schedule_advantage(games)
#' advantage[, c("game_id", "rest_points", "travel_points",
#'               "time_zone_points", "schedule_advantage")]
#' @export
calculate_schedule_advantage <- function(
    schedule, team_home = nfl_airports, venues = nfl_stadiums,
    trip_model = c("itinerary", "round_trip"),
    points_per_rest_day = 0.25,
    points_per_1000_travel_miles = 0.5,
    points_per_tz_hour = 0.35,
    east_weight = 1, west_weight = 0.5) {
  trip_model <- match.arg(trip_model)

  weights <- list(
    points_per_rest_day = points_per_rest_day,
    points_per_1000_travel_miles = points_per_1000_travel_miles,
    points_per_tz_hour = points_per_tz_hour
  )
  for (arg in names(weights)) {
    .check_nonnegative(weights[[arg]], arg)
  }

  rest <- calculate_rest_disparity(schedule)
  travel <- calculate_travel_miles(
    schedule, team_home = team_home, venues = venues,
    trip_model = trip_model
  )
  tz <- flag_time_zone_shifts(
    schedule, team_home = team_home, venues = venues,
    east_weight = east_weight, west_weight = west_weight
  )

  out <- rest
  out$home_trip_miles <- .side_column(
    travel, rest$game_id, TRUE, "trip_miles"
  )
  out$away_trip_miles <- .side_column(
    travel, rest$game_id, FALSE, "trip_miles"
  )
  out$travel_disparity <- out$away_trip_miles - out$home_trip_miles

  out$home_tz_shift_hours <- .side_column(
    tz, rest$game_id, TRUE, "tz_shift_hours"
  )
  out$away_tz_shift_hours <- .side_column(
    tz, rest$game_id, FALSE, "tz_shift_hours"
  )
  out$home_body_clock_penalty <- .side_column(
    tz, rest$game_id, TRUE, "body_clock_penalty"
  )
  out$away_body_clock_penalty <- .side_column(
    tz, rest$game_id, FALSE, "body_clock_penalty"
  )
  out$tz_penalty_disparity <- out$away_body_clock_penalty -
    out$home_body_clock_penalty

  as_points <- function(x) ifelse(is.na(x), 0, x)
  out$rest_points <- weights$points_per_rest_day *
    as_points(out$rest_disparity)
  out$travel_points <- weights$points_per_1000_travel_miles *
    as_points(out$travel_disparity) / 1000
  out$time_zone_points <- weights$points_per_tz_hour *
    as_points(out$tz_penalty_disparity)

  out$schedule_advantage <- out$rest_points + out$travel_points +
    out$time_zone_points
  out$components_missing <- is.na(out$rest_disparity) +
    is.na(out$travel_disparity) + is.na(out$tz_penalty_disparity)
  out$advantage_category <- ifelse(
    out$schedule_advantage >= 0.5, "home",
    ifelse(out$schedule_advantage <= -0.5, "away", "even")
  )

  tibble::as_tibble(out)
}
