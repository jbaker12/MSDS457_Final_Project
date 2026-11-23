# test-travel-itinerary.R ------------------------------------------------
# The itinerary model chains consecutive road games and resets on a bye.

.airport <- function(team) {
  nfl_airports[nfl_airports$team == team, ]
}
.stadium <- function(id) {
  nfl_stadiums[nfl_stadiums$stadium_id == id, ]
}
.distance <- function(a, b) {
  nflrest:::.haversine_miles(
    a$latitude, a$longitude, b$latitude, b$longitude
  )
}

test_that("itinerary chains consecutive road games", {
  buf <- .airport("BUF")
  kan <- .stadium("KAN00")
  den <- .stadium("DEN00")

  d_buf_kan <- .distance(buf, kan)
  d_kan_den <- .distance(kan, den)
  d_den_buf <- .distance(den, buf)

  itinerary <- calculate_travel_miles(.fixture_road_trip)
  round_trip <- calculate_travel_miles(
    .fixture_road_trip, trip_model = "round_trip"
  )

  buf_it <- itinerary[itinerary$team == "BUF", ]
  buf_rt <- round_trip[round_trip$team == "BUF", ]
  expect_equal(
    buf_it$trip_miles[match(
      c("RT1_BUF_KC", "RT2_BUF_DEN", "RT3_MIA_BUF"), buf_it$game_id
    )],
    c(d_buf_kan, d_kan_den + d_den_buf, 0),
    tolerance = 1e-6
  )
  expect_equal(
    buf_rt$trip_miles[match(
      c("RT1_BUF_KC", "RT2_BUF_DEN", "RT3_MIA_BUF"), buf_rt$game_id
    )],
    c(2 * d_buf_kan, 2 * d_den_buf, 0),
    tolerance = 1e-6
  )
  expect_lt(sum(buf_it$trip_miles), sum(buf_rt$trip_miles))
  expect_equal(unique(itinerary$travel_model), "itinerary")
})

test_that("itinerary resets after a bye", {
  buf <- .airport("BUF")
  kan <- .stadium("KAN00")
  den <- .stadium("DEN00")

  d_buf_kan <- .distance(buf, kan)
  d_buf_den <- .distance(buf, den)

  itinerary <- calculate_travel_miles(.fixture_bye_trip)
  buf_it <- itinerary[itinerary$team == "BUF", ]

  expect_equal(
    buf_it$trip_miles[match(
      c("BY1_BUF_KC", "BY2_BUF_DEN", "BY3_MIA_BUF"), buf_it$game_id
    )],
    c(2 * d_buf_kan, 2 * d_buf_den, 0),
    tolerance = 1e-6
  )
})

test_that("itinerary requires dates and validates the model", {
  no_date <- .fixture_road_trip[, setdiff(
    names(.fixture_road_trip), "gameday"
  )]
  expect_error(calculate_travel_miles(no_date), "gameday")
  expect_error(
    calculate_travel_miles(.fixture_road_trip, trip_model = "walking"),
    "arg"
  )
  expect_error(
    calculate_travel_miles(.fixture_travel_schedule, trip_model = "itinerary",
                           team_home = nfl_airports[0, ]),
    "missing from `team_home`"
  )
})
