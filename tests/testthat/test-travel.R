# test-travel.R -----------------------------------------------------------
# Travel miles, neutral-site venues, and per-team summaries.

test_that("calculate_travel_miles leaves home teams at zero", {
  travel <- calculate_travel_miles(.fixture_travel_schedule)
  home_game <- travel[travel$game_id == "H1_KC_BUF", ]

  expect_equal(home_game$trip_miles[home_game$team == "KC"], 0)
  expect_equal(home_game$round_trip[home_game$team == "KC"], FALSE)
  expect_equal(
    home_game$trip_miles[home_game$team == "BUF"],
    2 * .expected_buf_to_kan,
    tolerance = 0.01
  )
  expect_equal(home_game$venue_source, c("stadium", "stadium"))
})

test_that("calculate_travel_miles sends both teams to neutral sites", {
  travel <- calculate_travel_miles(.fixture_travel_schedule)
  neutral <- travel[travel$game_id == "N1_PHI_GB", ]

  expect_true(all(neutral$round_trip))
  expect_equal(
    neutral$trip_miles[neutral$team == "PHI"],
    2 * .expected_phl_to_sao,
    tolerance = 0.01
  )
  expect_equal(
    neutral$trip_miles[neutral$team == "GB"],
    2 * .expected_grb_to_sao,
    tolerance = 0.01
  )
  expect_equal(unique(neutral$venue), "Arena Corinthians")
})

test_that("calculate_travel_miles falls back to the home airport only at home", {
  home <- .fixture_travel_schedule[1, ]
  home$stadium_id <- "UNKNOWN"
  travel <- calculate_travel_miles(home)

  expect_equal(unique(travel$venue_source), "home airport")
  expect_equal(travel$trip_miles[travel$team == "KC"], 0)
  expect_gt(travel$trip_miles[travel$team == "BUF"], 0)

  neutral <- .fixture_travel_schedule[2, ]
  neutral$stadium_id <- "UNKNOWN"
  expect_warning(
    travel <- calculate_travel_miles(neutral),
    "without venue coordinates"
  )
  expect_true(all(is.na(travel$trip_miles)))
  expect_equal(unique(travel$venue_source), "missing")
})

test_that("neutral games recorded at the home venue are rejected", {
  stale <- .fixture_travel_schedule[2, ]
  stale$stadium_id <- nfl_airports$home_stadium_id[nfl_airports$team == "PHI"]

  expect_warning(
    travel <- calculate_travel_miles(stale),
    "home team's own stadium"
  )
  expect_true(all(is.na(travel$trip_miles)))
  expect_equal(unique(travel$venue_source), "home venue (neutral game)")
})

test_that("calculate_travel_miles validates teams and columns", {
  bad <- transform(.fixture_travel_schedule, home_team = "XYZ")
  expect_error(calculate_travel_miles(bad), "missing from `team_home`")
  expect_error(
    calculate_travel_miles(.fixture_travel_schedule[, c("game_id", "home_team")]),
    "missing required columns"
  )
})

test_that("summarize_travel_miles aggregates per team and season", {
  travel <- calculate_travel_miles(.fixture_travel_schedule)
  summary <- summarize_travel_miles(travel)
  buf <- summary[summary$team == "BUF", ]
  kc <- summary[summary$team == "KC", ]

  expect_equal(names(summary)[1:2], c("team", "season"))
  expect_equal(buf$total_miles, 2 * .expected_buf_to_kan, tolerance = 0.01)
  expect_equal(buf$longest_trip_miles, buf$total_miles)
  expect_equal(buf$mean_trip_miles, buf$total_miles)
  expect_equal(buf$furthest_opponent, "KC")
  expect_equal(buf$travel_games, 1L)
  expect_equal(kc$total_miles, 0)
  expect_equal(kc$travel_games, 0L)
  expect_true(is.na(kc$mean_trip_miles))
  expect_equal(buf$missing_trips, 0L)
})

test_that("summarize_travel_miles works without optional columns", {
  travel <- tibble::tibble(
    team = c("A", "A", "B"),
    trip_miles = c(0, 100, 250)
  )
  summary <- summarize_travel_miles(travel)

  expect_equal(summary$team, c("A", "B"))
  expect_equal(summary$total_miles, c(100, 250))
  expect_equal(summary$missing_trips, c(0L, 0L))
  expect_true(all(is.na(summary$furthest_opponent)))
})

test_that("summarize_travel_miles validates its input", {
  expect_error(summarize_travel_miles(data.frame(team = "A")), "trip_miles")
  expect_error(
    summarize_travel_miles(data.frame(team = character(), trip_miles = numeric())),
    "at least one row"
  )
  expect_error(
    summarize_travel_miles(data.frame(team = "A", trip_miles = 1),
                           by = "season"),
    "must name columns"
  )
})
