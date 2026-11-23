# rest.R ------------------------------------------------------------------
# Rest is measured in calendar days between a team's games within a season.
# The first game of a team's season has `NA` rest (nflverse reports 7).
# Positive rest disparity favors the home team.

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
  .check_positive(short_week_days, "short_week_days")
  .check_positive(bye_rest_days, "bye_rest_days")

  games <- tibble::as_tibble(schedule)
  .check_rows(games)
  .check_game_ids(games)
  games$gameday <- .as_gameday(games$gameday)
  if (anyNA(games$gameday)) {
    cli::cli_abort("`gameday` must not contain missing dates.")
  }

  long <- .game_team_long(games, keep = c("season", "gameday"))
  if (!"season" %in% names(long)) {
    long$season <- NA
  }

  key <- .group_key(list(long$season, long$team))
  ord <- order(key, long$gameday, long$game_id)
  long <- long[ord, , drop = FALSE]
  key <- key[ord]

  long$rest_days <- NA_real_
  for (i in split(seq_len(nrow(long)), key)) {
    long$rest_days[i] <- c(NA_real_, as.numeric(diff(long$gameday[i])))
  }

  home <- long[long$is_home, , drop = FALSE]
  away <- long[!long$is_home, , drop = FALSE]
  home <- home[match(games$game_id, home$game_id), , drop = FALSE]
  away <- away[match(games$game_id, away$game_id), , drop = FALSE]

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
