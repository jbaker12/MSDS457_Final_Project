# test-ingestion.R --------------------------------------------------------
# Validation and cache lifecycle for the optional live-data layer.

test_that("load_nfl_schedule validates seasons and flags", {
  expect_error(load_nfl_schedule("2024"), "integer")
  expect_error(load_nfl_schedule(c(2024, NA)), "integer")
  expect_error(load_nfl_schedule(1990), "outside")
  expect_error(load_nfl_schedule(2024, refresh = NA), "TRUE")
  expect_error(load_nfl_schedule(2024, cache = NA), "TRUE")
})

test_that("the in-memory cache can be used and cleared", {
  store <- nflrest:::.nflrest_cache()
  store$set("nflrest-test", 42)
  expect_equal(store$get("nflrest-test"), 42)

  expect_true(clear_nflrest_cache())
  expect_false(store$exists("nflrest-test"))
})

test_that("load_nfl_schedule returns a cached schedule without downloading", {
  store <- nflrest:::.nflrest_cache()
  cached <- tibble::tibble(game_id = "cached-game")
  store$set("schedules_2024", cached)
  on.exit(clear_nflrest_cache(), add = TRUE)

  expect_identical(load_nfl_schedule(2024), cached)
  expect_identical(load_nfl_schedule(2024, refresh = FALSE), cached)
})

test_that("live neutral venues are repaired from the bundled sample", {
  stale <- tibble::tibble(
    game_id = "2025_04_MIN_PIT",
    season = 2025L,
    week = 4L,
    home_team = "PIT",
    away_team = "MIN",
    location = "Neutral",
    stadium_id = "PIT00",
    stadium = "Acrisure Stadium"
  )
  fixed <- nflrest:::.repair_neutral_venues(stale)

  expect_equal(fixed$stadium_id, "DUB01")
  expect_equal(fixed$stadium, "Croke Park")
})

test_that("live ingestion works when enabled", {
  skip_if(Sys.getenv("NFLREST_LIVE_TESTS") != "true")
  skip_if_not_installed("nflreadr")

  schedule <- load_nfl_schedule(2024, cache = FALSE)
  expect_s3_class(schedule, "tbl_df")
  expect_true(nrow(schedule) > 0)
})
