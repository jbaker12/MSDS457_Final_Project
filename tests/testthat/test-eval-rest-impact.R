# test-eval-rest-impact.R -------------------------------------------------
# Outcome comparisons for the composite and its components.

test_that("eval_rest_impact compares disadvantaged and advantaged teams", {
  res <- eval_rest_impact(
    .fixture_advantage, advantage = .fixture_advantage,
    metrics = "composite"
  )

  expect_equal(res$metric, "composite")
  expect_equal(res$games, 6L)
  expect_equal(res$disadvantaged_games, 4L)
  expect_equal(res$disadvantaged_win_games, 4L)
  expect_equal(res$disadvantaged_wins, 2L)
  expect_equal(res$disadvantaged_win_rate, 0.5)
  expect_equal(res$advantaged_win_rate, 0.5)
  expect_equal(res$win_rate_diff, 0)
  expect_equal(res$mean_margin_disadvantaged, -2.75)
  expect_equal(res$cover_games, 4L)
  expect_equal(res$disadvantaged_covers, 2L)
  expect_equal(res$disadvantaged_cover_rate, 0.5)
  expect_equal(res$advantaged_cover_rate, 0.5)
  expect_equal(res$cover_rate_diff, 0)
  expect_true(is.na(res$mean_injuries_disadvantaged))
})

test_that("eval_rest_impact can ignore small edges", {
  res <- eval_rest_impact(
    .fixture_advantage, advantage = .fixture_advantage,
    metrics = "composite", min_edge = 6
  )

  expect_equal(res$disadvantaged_games, 2L)
  expect_equal(res$disadvantaged_wins, 0L)
  expect_equal(res$disadvantaged_win_rate, 0)
  expect_equal(res$mean_margin_disadvantaged, -12)
})

test_that("eval_rest_impact joins injury reports by team-week", {
  res <- eval_rest_impact(
    .fixture_advantage, advantage = .fixture_advantage,
    injuries = .fixture_injuries, metrics = "composite"
  )

  expect_equal(res$injury_games, 4L)
  expect_equal(res$mean_injuries_disadvantaged, 1.75)
  expect_equal(res$mean_injuries_advantaged, 0.75)
  expect_equal(res$injury_diff, 1)
})

test_that("eval_rest_impact respects metric selection and outcomes", {
  rest_only <- eval_rest_impact(
    .fixture_advantage, advantage = .fixture_advantage,
    metrics = "rest"
  )
  expect_equal(rest_only$metric, "rest")
  expect_equal(rest_only$disadvantaged_games, 4L)

  travel_only <- eval_rest_impact(
    .fixture_advantage, advantage = .fixture_advantage,
    metrics = "travel"
  )
  expect_equal(travel_only$disadvantaged_games, 0L)
  expect_true(is.na(travel_only$disadvantaged_win_rate))

  outcomes <- tibble::tibble(
    game_id = .fixture_advantage$game_id,
    home_score = .fixture_advantage$home_score,
    away_score = .fixture_advantage$away_score,
    spread_line = .fixture_advantage$spread_line
  )
  outcomes$home_score[1] <- 10
  outcomes$away_score[1] <- 24
  changed <- eval_rest_impact(
    .fixture_advantage, advantage = .fixture_advantage,
    outcomes = outcomes, metrics = "composite"
  )
  expect_equal(changed$disadvantaged_wins, 3L)
  expect_equal(changed$disadvantaged_win_rate, 0.75)
})

test_that("eval_rest_impact validates outcomes, metrics, and injuries", {
  expect_error(
    eval_rest_impact(
      .fixture_advantage[, c("game_id", "rest_disparity")],
      advantage = .fixture_advantage[, c("game_id", "rest_disparity")],
      metrics = "rest"
    ),
    "outcomes are required"
  )
  expect_error(
    eval_rest_impact(
      .fixture_advantage,
      advantage = .fixture_advantage[, c("game_id", "rest_disparity")]
    ),
    "missing the requested metric"
  )
  expect_error(
    eval_rest_impact(.fixture_advantage,
                     advantage = .fixture_advantage,
                     metrics = "weather"),
    "supported metric"
  )
  expect_error(
    eval_rest_impact(.fixture_advantage,
                     advantage = .fixture_advantage,
                     min_edge = -1),
    "non-negative"
  )
  expect_error(
    eval_rest_impact(.fixture_advantage,
                     advantage = .fixture_advantage,
                     injuries = data.frame(season = 2024, week = 1)),
    "missing required columns"
  )
  expect_error(
    eval_rest_impact(.fixture_advantage,
                     advantage = .fixture_advantage,
                     injuries = data.frame(
                       season = 2024, week = 1, team = "A", note = "x"
                     )),
    "report_status"
  )
})
