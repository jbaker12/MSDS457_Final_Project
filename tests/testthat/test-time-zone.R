# test-time-zone.R --------------------------------------------------------
# Date-aware body-clock shifts, directional weighting, and validation.

test_that("flag_time_zone_shifts computes signed clock shifts", {
  tz <- flag_time_zone_shifts(.fixture_tz_schedule)

  kc_game <- tz[tz$game_id == "TZ1_BUF_KC", ]
  expect_equal(kc_game$team, c("KC", "BUF"))
  expect_equal(kc_game$tz_shift_hours, c(0, -1))
  expect_equal(kc_game$tz_direction, c("none", "west"))
  expect_equal(kc_game$body_clock_penalty, c(0, 0.5))

  sea_game <- tz[tz$game_id == "TZ2_SEA_NYJ", ]
  expect_equal(sea_game$tz_shift_hours, c(0, 3))
  expect_equal(sea_game$body_clock_penalty, c(0, 3))
  expect_equal(sea_game$kickoff_home_local[sea_game$team == "SEA"],
               "2024-09-15 10:00")
  expect_equal(sea_game$kickoff_venue_local[sea_game$team == "SEA"],
               "2024-09-15 13:00")

  london <- tz[tz$game_id == "TZ3_NE_JAX", ]
  expect_equal(london$tz_shift_hours, c(5, 5))
  expect_equal(london$tz_direction, c("east", "east"))

  sao <- tz[tz$game_id == "TZ4_GB_PHI", ]
  expect_equal(sao$tz_shift_hours, c(1, 2))
})

test_that("flag_time_zone_shifts handles missing gametime and custom weights", {
  no_time <- .fixture_tz_schedule[, setdiff(
    names(.fixture_tz_schedule), "gametime"
  )]
  tz <- flag_time_zone_shifts(no_time)
  expect_equal(tz$tz_shift_hours[tz$game_id == "TZ1_BUF_KC"], c(0, -1))

  weighted <- flag_time_zone_shifts(
    .fixture_tz_schedule, east_weight = 2, west_weight = 0
  )
  expect_equal(weighted$body_clock_penalty[
    weighted$game_id == "TZ1_BUF_KC"
  ], c(0, 0))
  expect_equal(weighted$body_clock_penalty[
    weighted$game_id == "TZ2_SEA_NYJ"
  ], c(0, 6))
})

test_that("flag_time_zone_shifts warns on unknown neutral venues", {
  neutral <- .fixture_tz_schedule[.fixture_tz_schedule$game_id == "TZ3_NE_JAX", ]
  neutral$stadium_id <- "UNKNOWN"

  expect_warning(
    tz <- flag_time_zone_shifts(neutral),
    "without a venue time zone"
  )
  expect_true(all(is.na(tz$tz_shift_hours)))
})

test_that("flag_time_zone_shifts rejects stale neutral venues", {
  stale <- .fixture_tz_schedule[.fixture_tz_schedule$game_id == "TZ4_GB_PHI", ]
  stale$stadium_id <- "PHI00"

  expect_warning(
    tz <- flag_time_zone_shifts(stale),
    "home team's own stadium"
  )
  expect_true(all(is.na(tz$tz_shift_hours)))
})

test_that("flag_time_zone_shifts validates its input", {
  expect_error(
    flag_time_zone_shifts(.fixture_tz_schedule[, c("game_id", "gameday")]),
    "missing required columns"
  )
  expect_error(
    flag_time_zone_shifts(transform(.fixture_tz_schedule, home_team = "XYZ")),
    "missing from `team_home`"
  )
  expect_error(
    flag_time_zone_shifts(transform(.fixture_tz_schedule, gametime = "25:00")),
    "HH:MM"
  )
  expect_error(
    flag_time_zone_shifts(.fixture_tz_schedule, east_weight = -1),
    "non-negative"
  )
  expect_error(
    flag_time_zone_shifts(rbind(.fixture_tz_schedule,
                                .fixture_tz_schedule[1, ])),
    "uniquely identify"
  )
  expect_error(
    flag_time_zone_shifts(.fixture_tz_schedule[0, ]),
    "at least one row"
  )
})
