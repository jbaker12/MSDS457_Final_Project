# eval_rest_impact.R ------------------------------------------------------
# Outcome checks for the schedule-advantage components. These are descriptive
# comparisons, not a causal model: they show whether disadvantaged teams won,
# covered, or had more injured players often enough to be worth a closer look.

# Build a named count of unavailable players by season-week-team. A team-week
# with no listed players is a true zero only when that week appears in the
# injury table at all; otherwise it stays missing.
.injury_lookup <- function(injuries, statuses) {
  .check_frame(injuries, c("season", "week", "team"), arg = "injuries")
  status_col <- intersect(c("report_status", "status"), names(injuries))
  if (!length(status_col)) {
    cli::cli_abort(
      "`injuries` must contain a `report_status` or `status` column."
    )
  }
  status_col <- status_col[1]
  status <- as.character(injuries[[status_col]])

  flagged <- injuries[status %in% statuses, , drop = FALSE]
  if ("gsis_id" %in% names(flagged)) {
    flagged <- flagged[!duplicated(flagged[, c(
      "season", "week", "team", "gsis_id"
    )]), , drop = FALSE]
  }

  key <- .group_key(list(flagged$season, flagged$week, flagged$team))
  if (length(key)) {
    tab <- tapply(rep(1L, length(key)), key, sum)
    counts <- stats::setNames(as.integer(tab), names(tab))
  } else {
    counts <- integer()
  }

  covered_weeks <- unique(.group_key(list(injuries$season, injuries$week)))
  list(counts = counts, covered_weeks = covered_weeks)
}

.injury_counts_for <- function(season, week, team, lookup) {
  key <- .group_key(list(season, week, team))
  season_week <- .group_key(list(season, week))
  out <- unname(lookup$counts[key])
  out[is.na(out) & season_week %in% lookup$covered_weeks] <- 0L
  out
}

# Win, margin, and spread-cover summaries for one schedule component.
.score_impact <- function(edge, home_score, away_score, spread_line,
                          min_edge = 0) {
  valid <- !is.na(edge) & !is.na(home_score) & !is.na(away_score)
  non_tie <- valid & edge != 0 & abs(edge) >= min_edge

  home_won <- home_score > away_score
  away_won <- away_score > home_score
  home_tie <- home_score == away_score

  disadv_won <- ifelse(
    edge > 0, away_won,
    ifelse(edge < 0, home_won, NA)
  )
  adv_won <- ifelse(
    edge > 0, home_won,
    ifelse(edge < 0, away_won, NA)
  )
  win_games <- non_tie & !home_tie
  win_n <- sum(win_games)
  disadv_wins <- sum(disadv_won[win_games], na.rm = TRUE)
  adv_wins <- sum(adv_won[win_games], na.rm = TRUE)

  margin <- ifelse(
    edge > 0,
    away_score - home_score,
    ifelse(edge < 0, home_score - away_score, NA)
  )

  cover_margin <- (home_score - away_score) - spread_line
  disadv_covered <- ifelse(
    edge > 0, cover_margin < 0,
    ifelse(edge < 0, cover_margin > 0, NA)
  )
  adv_covered <- ifelse(
    edge > 0, cover_margin > 0,
    ifelse(edge < 0, cover_margin < 0, NA)
  )
  cover_games <- non_tie & !is.na(cover_margin) & cover_margin != 0
  cover_n <- sum(cover_games)
  disadv_covers <- sum(disadv_covered[cover_games], na.rm = TRUE)
  adv_covers <- sum(adv_covered[cover_games], na.rm = TRUE)

  list(
    games = sum(valid),
    disadvantaged_games = sum(non_tie),
    disadvantaged_win_games = win_n,
    disadvantaged_wins = disadv_wins,
    disadvantaged_win_rate = .rate(disadv_wins, win_n),
    advantaged_win_rate = .rate(adv_wins, win_n),
    win_rate_diff = .rate(disadv_wins - adv_wins, win_n),
    mean_margin_disadvantaged = .mean_or_na(margin[non_tie]),
    cover_games = cover_n,
    disadvantaged_covers = disadv_covers,
    disadvantaged_cover_rate = .rate(disadv_covers, cover_n),
    advantaged_cover_rate = .rate(adv_covers, cover_n),
    cover_rate_diff = .rate(disadv_covers - adv_covers, cover_n)
  )
}

# Injury summaries for the same disadvantaged/advantaged split.
.injury_impact <- function(edge, home_score, away_score, season, week,
                           home_team, away_team, lookup, min_edge = 0) {
  if (is.null(lookup)) {
    return(list(
      injury_games = 0L,
      mean_injuries_disadvantaged = NA_real_,
      mean_injuries_advantaged = NA_real_,
      injury_diff = NA_real_
    ))
  }

  non_tie <- !is.na(edge) & edge != 0 & abs(edge) >= min_edge &
    !is.na(home_score) & !is.na(away_score)
  home_injuries <- .injury_counts_for(season, week, home_team, lookup)
  away_injuries <- .injury_counts_for(season, week, away_team, lookup)

  disadv_injuries <- ifelse(
    edge > 0, away_injuries,
    ifelse(edge < 0, home_injuries, NA)
  )
  adv_injuries <- ifelse(
    edge > 0, home_injuries,
    ifelse(edge < 0, away_injuries, NA)
  )
  injury_ok <- non_tie & !is.na(disadv_injuries) & !is.na(adv_injuries)
  disadv_mean <- .mean_or_na(disadv_injuries[injury_ok])
  adv_mean <- .mean_or_na(adv_injuries[injury_ok])

  list(
    injury_games = sum(injury_ok),
    mean_injuries_disadvantaged = disadv_mean,
    mean_injuries_advantaged = adv_mean,
    injury_diff = disadv_mean - adv_mean
  )
}

# eval_rest_impact --------------------------------------------------------
#' Evaluate whether schedule disadvantages show up in outcomes
#'
#' Joins the composite schedule advantage to game results and, optionally,
#' injury reports, then compares disadvantaged and advantaged teams. The
#' output is descriptive: win rates, spread cover rates, scoring margins, and
#' injury counts for games where one team had an edge. It does not estimate a
#' causal effect of rest or travel.
#'
#' @param schedule A schedule accepted by [calculate_schedule_advantage()].
#' @param outcomes Optional data frame with `game_id`, `home_score`, and
#'   `away_score` (and optionally `spread_line`). Use this when the schedule
#'   does not already carry results. Columns in `outcomes` override matching
#'   columns in `schedule`.
#' @param injuries Optional injury report with `season`, `week`, `team`, and a
#'   `report_status` or `status` column, such as the output of
#'   `nflreadr::load_injuries()`. When `NULL`, injury columns are `NA`.
#' @param advantage Optional precomputed output from
#'   [calculate_schedule_advantage()]. When `NULL`, it is computed from
#'   `schedule` (and `...`).
#' @param metrics Which components to evaluate: any of `"rest"`, `"travel"`,
#'   `"time_zone"`, and `"composite"`. Defaults to all four.
#' @param injury_statuses Injury statuses counted as unavailable. Defaults to
#'   `c("Out", "Doubtful")`.
#' @param min_edge Minimum absolute edge required to count a game as
#'   disadvantaged. Defaults to 0, which includes any non-zero edge. The units
#'   follow the selected metric (rest days, miles, body-clock hours, or
#'   composite points); use something like `0.5` for the composite.
#' @param ... Arguments passed to [calculate_schedule_advantage()] when
#'   `advantage` is `NULL`.
#'
#' @return A tibble with one row per metric. `games` is the number of games
#'   with a non-missing component; `disadvantaged_games` counts games at or
#'   above `min_edge`. Win rates exclude ties from their denominators, and
#'   cover rates exclude pushes. Injury summaries cover games with injury data
#'   for both teams.
#'
#' @details
#' A positive component favors the home team, so the away team is the
#' disadvantaged side; a negative component makes the home team disadvantaged.
#' Games with `abs(edge) < min_edge` are left out of the
#' disadvantaged/advantaged comparison. `win_rate_diff`, `cover_rate_diff`,
#' and `injury_diff` are always disadvantaged minus advantaged, so a negative
#' value means the disadvantaged side did worse.
#'
#' @examples
#' advantage <- calculate_schedule_advantage(nfl_schedules_sample)
#' eval_rest_impact(nfl_schedules_sample, advantage = advantage)
#' @export
eval_rest_impact <- function(schedule, outcomes = NULL, injuries = NULL,
                             advantage = NULL,
                             metrics = c("rest", "travel", "time_zone",
                                         "composite"),
                             injury_statuses = c("Out", "Doubtful"),
                             min_edge = 0,
                             ...) {
  metric_cols <- c(
    rest = "rest_disparity",
    travel = "travel_disparity",
    time_zone = "tz_penalty_disparity",
    composite = "schedule_advantage"
  )

  if (!is.character(metrics) || !length(metrics) ||
      !all(metrics %in% names(metric_cols))) {
    cli::cli_abort(
      c(
        "`metrics` must contain one or more supported metric names.",
        "i" = "Supported: {.and {.val {names(metric_cols)}}}."
      )
    )
  }
  if (!is.character(injury_statuses) || !length(injury_statuses) ||
      anyNA(injury_statuses)) {
    cli::cli_abort("`injury_statuses` must be a character vector without `NA`.")
  }
  .check_nonnegative(min_edge, "min_edge")

  if (is.null(advantage)) {
    advantage <- calculate_schedule_advantage(schedule, ...)
  } else {
    .check_frame(advantage, "game_id", arg = "advantage")
    advantage <- tibble::as_tibble(advantage)
  }

  needed_metrics <- unname(metric_cols[metrics])
  missing_metrics <- setdiff(needed_metrics, names(advantage))
  if (length(missing_metrics)) {
    cli::cli_abort(
      c(
        "`advantage` is missing the requested metric columns.",
        "x" = "Missing: {.and {.field {missing_metrics}}}.",
        "i" = "Use {.fn calculate_schedule_advantage} or supply all components."
      )
    )
  }
  if (!nrow(advantage)) {
    cli::cli_abort("`advantage` must have at least one game.")
  }

  if (!is.null(outcomes)) {
    .check_frame(
      outcomes, c("game_id", "home_score", "away_score"),
      arg = "outcomes"
    )
    if (anyDuplicated(outcomes$game_id)) {
      cli::cli_abort("`outcomes$game_id` must uniquely identify games.")
    }
    idx <- match(advantage$game_id, outcomes$game_id)
    for (col in c("home_score", "away_score", "spread_line")) {
      if (col %in% names(outcomes)) {
        advantage[[col]] <- outcomes[[col]][idx]
      }
    }
  }

  needed <- c("home_score", "away_score")
  missing_outcomes <- setdiff(needed, names(advantage))
  if (length(missing_outcomes)) {
    cli::cli_abort(
      c(
        "Game outcomes are required to evaluate schedule impact.",
        "x" = "Missing: {.and {.field {missing_outcomes}}}.",
        "i" = "Add them to `schedule` or supply `outcomes`."
      )
    )
  }
  if (!is.numeric(advantage$home_score) || !is.numeric(advantage$away_score)) {
    cli::cli_abort("`home_score` and `away_score` must be numeric.")
  }
  if (!"spread_line" %in% names(advantage)) {
    advantage$spread_line <- NA_real_
  }
  if (!is.numeric(advantage$spread_line)) {
    cli::cli_abort("`spread_line` must be numeric when supplied.")
  }

  lookup <- if (is.null(injuries)) NULL else .injury_lookup(
    injuries, injury_statuses
  )
  if (!is.null(lookup)) {
    if (!all(c("season", "week") %in% names(advantage))) {
      cli::cli_abort(
        "`schedule` needs `season` and `week` columns when `injuries` is supplied."
      )
    }
  }

  rows <- lapply(metrics, function(metric) {
    edge <- advantage[[metric_cols[[metric]]]]
    tibble::as_tibble(c(
      list(metric = metric),
      .score_impact(
        edge, advantage$home_score, advantage$away_score,
        advantage$spread_line, min_edge
      ),
      .injury_impact(
        edge, advantage$home_score, advantage$away_score,
        advantage$season, advantage$week, advantage$home_team,
        advantage$away_team, lookup, min_edge
      )
    ))
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out <- tibble::as_tibble(out)
  attr(out, "injury_statuses") <- injury_statuses
  attr(out, "min_edge") <- min_edge
  out
}
