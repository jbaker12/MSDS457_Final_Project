# test-schedule-advantage.R ----------------------------------------------
# Composite scoring, documented point conversion, and weight validation.

test_that("calculate_schedule_advantage returns components that add up", {
  adv <- calculate_schedule_advantage(.fixture_advantage_schedule)

  expect_equal(
    adv$schedule_advantage,
    adv$rest_points + adv$travel_points + adv$time_zone_points
  )
  expect_equal(
    adv$rest_points,
    0.25 * ifelse(is.na(adv$rest_disparity), 0, adv$rest_disparity)
  )
  expect_equal(
    adv$travel_points,
    0.5 * ifelse(is.na(adv$travel_disparity), 0, adv$travel_disparity) / 1000
  )
  expect_equal(
    adv$time_zone_points,
    0.35 * ifelse(is.na(adv$tz_penalty_disparity), 0,
                  adv$tz_penalty_disparity)
  )
  expect_equal(adv$components_missing[adv$week == 1L], c(1L, 1L))
  expect_true(all(adv$advantage_category %in% c("home", "away", "even")))
})

test_that("rest disparity sign favors the better-rested team", {
  adv <- calculate_schedule_advantage(.fixture_advantage_schedule)
  week_two <- adv[adv$week == 2L, ]

  expect_equal(week_two$rest_disparity, c(-3, -3))
  expect_true(all(week_two$rest_points < 0))
  expect_true(all(week_two$schedule_advantage > 0))
})

test_that("component weights can zero out parts of the score", {
  adv <- calculate_schedule_advantage(
    .fixture_advantage_schedule,
    points_per_rest_day = 0,
    points_per_1000_travel_miles = 0,
    points_per_tz_hour = 0
  )
  expect_equal(adv$schedule_advantage, rep(0, nrow(adv)))
  expect_equal(adv$advantage_category, rep("even", nrow(adv)))
})

test_that("calculate_schedule_advantage validates weights and input", {
  expect_error(
    calculate_schedule_advantage(.fixture_advantage_schedule,
                                 points_per_rest_day = -1),
    "non-negative"
  )
  expect_error(
    calculate_schedule_advantage(.fixture_advantage_schedule,
                                 points_per_tz_hour = c(1, 2)),
    "non-negative"
  )
  expect_error(
    calculate_schedule_advantage(.fixture_advantage_schedule[, "game_id"]),
    "missing required columns"
  )
})
