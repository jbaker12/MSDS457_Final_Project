# test-rest_calculations.R ------------------------------------------------
# Rest disparity, short weeks, byes, and the nflverse oracle comparison.

test_that("calculate_rest_disparity handles byes and short weeks", {
  rest <- calculate_rest_disparity(.fixture_schedule)

  expect_s3_class(rest$gameday, "Date")
  expect_equal(rest$home_rest_days, c(NA, NA, 7, 4, 10, 14, 14))
  expect_equal(rest$away_rest_days, c(NA, NA, 7, NA, 7, 17, 7))
  expect_equal(rest$rest_disparity, c(NA, NA, 0, NA, 3, -3, 7))
  expect_equal(
    rest$home_short_week,
    c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
  )
  expect_equal(rest$away_short_week, rep(FALSE, 7))
  expect_equal(
    rest$home_off_bye,
    c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
  )
  expect_equal(
    rest$away_off_bye,
    c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)
  )
})

test_that("calculate_rest_disparity matches nflverse rest exactly", {
  rest <- calculate_rest_disparity(nfl_schedules_sample)
  known <- !is.na(rest$home_rest_days)

  expect_equal(rest$home_rest_days[known], nfl_schedules_sample$home_rest[known])
  expect_equal(rest$away_rest_days[known], nfl_schedules_sample$away_rest[known])
  # 16 season-opening games per season, 2024 and 2025.
  expect_equal(sum(!known), 32L)
})

test_that("calculate_rest_disparity is order-independent", {
  shuffled <- .fixture_schedule[c(7, 2, 1, 5, 3, 4, 6), ]
  sorted <- calculate_rest_disparity(.fixture_schedule)
  mixed <- calculate_rest_disparity(shuffled)

  expect_equal(
    mixed$rest_disparity[match(sorted$game_id, mixed$game_id)],
    sorted$rest_disparity
  )
})

test_that("calculate_rest_disparity resets at each season", {
  second <- .fixture_schedule[1:2, ]
  second$season <- 2025L
  second$game_id <- paste0("S2_", second$game_id)
  second$gameday <- as.character(as.Date(second$gameday) + 365)
  two_seasons <- rbind(.fixture_schedule[1:2, ], second)

  rest <- calculate_rest_disparity(two_seasons)

  expect_true(is.na(rest$home_rest_days[3]))
  expect_true(is.na(rest$away_rest_days[3]))
})

test_that("calculate_rest_disparity validates its input", {
  expect_error(
    calculate_rest_disparity(.fixture_schedule[, c("game_id", "gameday")]),
    "missing required columns"
  )
  expect_error(
    calculate_rest_disparity(transform(.fixture_schedule, gameday = "not-a-date")),
    "ISO 8601"
  )
  expect_error(
    calculate_rest_disparity(rbind(.fixture_schedule, .fixture_schedule[1, ])),
    "uniquely identify"
  )
  expect_error(
    calculate_rest_disparity(transform(
      .fixture_schedule,
      gameday = rep(as.Date(NA), nrow(.fixture_schedule))
    )),
    "missing dates"
  )
  expect_error(calculate_rest_disparity("nope"), "data frame")
})
