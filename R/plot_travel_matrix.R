# plot_travel_matrix.R ----------------------------------------------------
# Heatmap of team-by-venue travel mileage.

#' Plot a team-by-venue travel matrix
#'
#' Draws a heatmap of mean travel miles for every team-venue pair in a travel
#' frame. Useful for spotting long trips, neutral-site outliers, and teams
#' whose schedules concentrate travel in a few cities.
#'
#' @param travel A per-team-game travel frame from
#'   [calculate_travel_miles()], or any data frame with `team`, `venue`, and
#'   `trip_miles`. When `round_trip` is present, home games are dropped so the
#'   plot shows travel rather than zero-mile home dates.
#' @param fill_label Legend label. Defaults to `"Mean trip miles"`.
#' @param season Optional season or seasons to keep before plotting. Requires a
#'   `season` column in `travel`. If `NULL`, all seasons in `travel` are
#'   averaged together.
#'
#' @return A `ggplot` object.
#'
#' @details
#' Requires the suggested package \pkg{ggplot2}. Cells are the mean
#' `trip_miles` for that team-venue pair; missing combinations are blank.
#'
#' @examples
#' travel <- calculate_travel_miles(nfl_schedules_sample)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_travel_matrix(travel, season = 2024)
#' }
#' @export
plot_travel_matrix <- function(travel, fill_label = "Mean trip miles",
                               season = NULL) {
  .check_frame(travel, c("team", "venue", "trip_miles"), arg = "travel")
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "The {.pkg ggplot2} package is required for `plot_travel_matrix()`.",
        "i" = "Install it with {.run install.packages(\"ggplot2\")}."
      )
    )
  }
  if (!is.character(fill_label) || length(fill_label) != 1L ||
      is.na(fill_label)) {
    cli::cli_abort("`fill_label` must be a single string.")
  }
  if (!is.null(season)) {
    if (!is.numeric(season) || !length(season) || anyNA(season)) {
      cli::cli_abort("`season` must be a non-missing numeric vector.")
    }
    if (!"season" %in% names(travel)) {
      cli::cli_abort(
        "`travel` must contain a `season` column when `season` is supplied."
      )
    }
  }

  d <- travel
  if (!is.null(season)) {
    d <- d[d$season %in% season, , drop = FALSE]
    if (!nrow(d)) {
      cli::cli_abort("`travel` has no trips for the requested `season`.")
    }
  }
  if ("round_trip" %in% names(d)) {
    d <- d[!is.na(d$round_trip) & d$round_trip, , drop = FALSE]
  }
  d <- d[!is.na(d$trip_miles) & !is.na(d$team) & !is.na(d$venue), ,
         drop = FALSE]
  if (!nrow(d)) {
    cli::cli_abort("`travel` has no non-missing team-venue trips to plot.")
  }

  mat <- stats::aggregate(
    trip_miles ~ team + venue, data = d, FUN = mean, na.rm = TRUE
  )

  ggplot2::ggplot(
    mat,
    ggplot2::aes(x = venue, y = team, fill = trip_miles)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::scale_fill_gradient(
      low = "#f7fbff", high = "#08519c", na.value = "#e9ecef"
    ) +
    ggplot2::labs(
      x = NULL, y = NULL, fill = fill_label,
      title = "Travel miles by team and venue"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank()
    )
}
