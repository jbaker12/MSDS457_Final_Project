# test-plot-travel-matrix.R ----------------------------------------------
# Diagnostic heatmap construction and validation.

test_that("plot_travel_matrix returns a ggplot", {
  skip_if_not_installed("ggplot2")
  travel <- calculate_travel_miles(.fixture_travel_schedule)
  plot <- plot_travel_matrix(travel, season = 2024)

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$title, "Travel miles by team and venue")
  expect_equal(plot$labels$fill, "Mean trip miles")
})

test_that("plot_travel_matrix filters seasons", {
  skip_if_not_installed("ggplot2")
  travel <- calculate_travel_miles(.fixture_travel_schedule)

  expect_error(
    plot_travel_matrix(travel, season = 2025),
    "no trips for the requested"
  )
  expect_error(
    plot_travel_matrix(travel[, setdiff(names(travel), "season")],
                       season = 2024),
    "must contain a `season`"
  )
  expect_error(
    plot_travel_matrix(travel, season = NA_real_),
    "non-missing numeric"
  )
})

test_that("plot_travel_matrix validates input", {
  skip_if_not_installed("ggplot2")
  expect_error(
    plot_travel_matrix(data.frame(team = "KC")),
    "missing required columns"
  )
  expect_error(
    plot_travel_matrix(data.frame(
      team = character(), venue = character(), trip_miles = numeric()
    )),
    "no non-missing"
  )
  expect_error(
    plot_travel_matrix(
      calculate_travel_miles(.fixture_travel_schedule),
      fill_label = c("a", "b")
    ),
    "single string"
  )
})
