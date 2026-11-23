# test-season-summary.R ---------------------------------------------------
# Season roll-up, team-perspective signs, ranks, and travel totals.

test_that("calc_schedule_disparity returns one row per team-season", {
  season <- calc_schedule_disparity(.fixture_advantage_schedule)

  expect_equal(nrow(season), 4L)
  expect_setequal(season$team, c("KC", "BUF", "MIA", "NYJ"))
  expect_true(all(c(
    "games", "total_travel_miles", "mean_trip_miles",
    "longest_trip_miles", "short_weeks", "off_byes",
    "mean_schedule_advantage", "advantage_rank", "travel_rank", "rest_rank"
  ) %in% names(season)))
  expect_equal(season$games, rep(2L, 4))
  expect_equal(sort(season$advantage_rank), 1:4)
  expect_equal(sort(season$travel_rank), 1:4)
  expect_equal(
    season$rest_rank,
    rank(-season$mean_rest_disparity, ties.method = "min")
  )
})

test_that("season travel totals reconcile with the game-level frame", {
  travel <- calculate_travel_miles(.fixture_advantage_schedule)
  season <- calc_schedule_disparity(.fixture_advantage_schedule)

  expect_equal(sum(season$total_travel_miles), sum(travel$trip_miles))
  expect_equal(
    season$total_travel_miles,
    vapply(season$team, function(team) {
      sum(travel$trip_miles[travel$team == team])
    }, numeric(1), USE.NAMES = FALSE),
    tolerance = 1e-8
  )
})

test_that("team-perspective advantages sum to zero across each game", {
  season <- calc_schedule_disparity(.fixture_advantage_schedule)
  expect_equal(sum(season$total_schedule_advantage), 0, tolerance = 1e-8)
})

test_that("calc_schedule_disparity supports the round-trip model", {
  itinerary <- calc_schedule_disparity(.fixture_road_trip)
  round_trip <- calc_schedule_disparity(
    .fixture_road_trip, trip_model = "round_trip"
  )

  expect_gt(
    sum(round_trip$total_travel_miles),
    sum(itinerary$total_travel_miles)
  )
  expect_error(
    calc_schedule_disparity(.fixture_road_trip, trip_model = "walking"),
    "arg"
  )
})
