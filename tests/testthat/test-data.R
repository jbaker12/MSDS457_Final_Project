# test-data.R -------------------------------------------------------------
# Integrity checks on the bundled reference data.

test_that("bundled datasets have the documented shape", {
  expect_s3_class(nfl_airports, "tbl_df")
  expect_equal(dim(nfl_airports), c(32L, 8L))
  expect_equal(
    names(nfl_airports),
    c("team", "airport", "airport_name", "city", "latitude", "longitude",
      "time_zone", "home_stadium_id")
  )

  expect_s3_class(nfl_stadiums, "tbl_df")
  expect_equal(dim(nfl_stadiums), c(37L, 7L))
  expect_equal(
    names(nfl_stadiums),
    c("stadium_id", "stadium", "city", "country", "latitude", "longitude",
      "time_zone")
  )

  expect_s3_class(nfl_schedules_sample, "tbl_df")
  expect_equal(dim(nfl_schedules_sample), c(570L, 13L))
  expect_equal(
    names(nfl_schedules_sample),
    c("game_id", "season", "game_type", "week", "gameday", "home_team",
      "away_team", "home_rest", "away_rest", "location", "div_game",
      "stadium_id", "stadium")
  )
})

test_that("reference data is internally consistent", {
  expect_false(anyDuplicated(nfl_airports$team) > 0)
  expect_false(anyDuplicated(nfl_stadiums$stadium_id) > 0)
  expect_true(all(nfl_airports$time_zone %in% OlsonNames()))
  expect_true(all(nfl_stadiums$time_zone %in% OlsonNames()))
  expect_true(all(nfl_airports$latitude >= -90 & nfl_airports$latitude <= 90))
  expect_true(all(nfl_stadiums$latitude >= -90 & nfl_stadiums$latitude <= 90))
  expect_true(all(nfl_airports$home_stadium_id %in% nfl_stadiums$stadium_id))
})

test_that("the schedule sample joins cleanly to both lookups", {
  teams <- unique(c(nfl_schedules_sample$home_team,
                    nfl_schedules_sample$away_team))

  expect_setequal(teams, nfl_airports$team)
  expect_true(all(nfl_schedules_sample$stadium_id %in% nfl_stadiums$stadium_id))
  expect_false(anyNA(nfl_schedules_sample))
  expect_false(anyDuplicated(nfl_schedules_sample$game_id) > 0)
})

test_that("neutral-site games are not recorded at the home team's venue", {
  home_games <- nfl_schedules_sample[nfl_schedules_sample$location == "Home", ]
  usual <- tapply(
    home_games$stadium_id,
    home_games$home_team,
    function(x) names(sort(table(x), decreasing = TRUE))[1]
  )
  neutral <- nfl_schedules_sample[nfl_schedules_sample$location == "Neutral", ]

  expect_equal(nrow(neutral), 15L)
  expect_false(any(neutral$stadium_id == usual[neutral$home_team]))
  expect_true(all(neutral$stadium_id %in% nfl_stadiums$stadium_id))
})
