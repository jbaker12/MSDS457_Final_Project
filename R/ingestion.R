# ingestion.R -------------------------------------------------------------
# Optional live ingestion through nflreadr with an in-memory cache. The
# bundled sample keeps examples and tests offline; this layer is for users who
# want current or historical schedules without managing their own downloads.

.nflrest_cache <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- cachem::cache_mem(max_size = 512 * 1024^2)
    }
    cache
  }
})

# .neutral_venue_map ------------------------------------------------------
# The bundled sample records the actual venue for the 2024-2025 neutral-site
# games. nflverse still reports the designated home team's stadium for several
# 2025 international games, so use the sample as an override when the live
# venue is missing or points at the home team's own stadium.
.neutral_venue_map <- function() {
  sample <- nfl_schedules_sample
  neutral <- sample$location != "Home"
  stats::setNames(sample$stadium_id[neutral], sample$game_id[neutral])
}

.repair_neutral_venues <- function(schedule) {
  needed <- c("game_id", "location", "stadium_id", "home_team")
  if (!all(needed %in% names(schedule))) {
    return(schedule)
  }
  map <- .neutral_venue_map()
  idx <- match(schedule$game_id, names(map))
  home_stadium <- nfl_airports$home_stadium_id[match(
    schedule$home_team, nfl_airports$team
  )]
  stale <- is.na(schedule$stadium_id) |
    (!is.na(home_stadium) & schedule$stadium_id == home_stadium)
  neutral <- !is.na(schedule$location) & schedule$location != "Home"
  hit <- neutral & !is.na(idx) & stale
  if (any(hit)) {
    schedule$stadium_id[hit] <- unname(map[idx[hit]])
    if ("stadium" %in% names(schedule)) {
      schedule$stadium[hit] <- nfl_stadiums$stadium[match(
        schedule$stadium_id[hit], nfl_stadiums$stadium_id
      )]
    }
  }
  schedule
}

.finish_schedule <- function(schedule, fix_neutral_venues) {
  if (fix_neutral_venues) .repair_neutral_venues(schedule) else schedule
}

# load_nfl_schedule -------------------------------------------------------
#' Load NFL schedules through nflreadr
#'
#' Downloads schedule data with `nflreadr::load_schedules()` and caches the
#' result in memory for the rest of the R session. The cached object is keyed
#' by the requested seasons, so repeated calls with the same seasons do not
#' re-download.
#'
#' @param seasons Integer vector of seasons to load. nflverse schedule data
#'   starts in 1999.
#' @param refresh If `TRUE`, ignore any cached value and download again.
#' @param cache If `FALSE`, bypass the cache for this call.
#' @param fix_neutral_venues If `TRUE` (default), repair neutral-site games
#'   whose live `stadium_id` is missing or still points at the designated home
#'   team's own stadium. The bundled 2024-2025 sample supplies the actual
#'   venue for those games; seasons and venues outside the sample are left
#'   unchanged.
#'
#' @return A tibble of schedule data as returned by `nflreadr::load_schedules()`,
#'   including `game_id`, `gameday`, `gametime`, teams, scores, rest, spread,
#'   `location`, and `stadium_id` when available.
#'
#' @details
#' Requires the suggested package \pkg{nflreadr}. The first call needs network
#' access; later calls in the same session reuse the in-memory copy. Use
#' [clear_nflrest_cache()] to release it. The returned columns are whatever
#' `nflreadr::load_schedules()` provides, except that known 2024-2025
#' neutral-site venues can be repaired with `fix_neutral_venues`.
#'
#' @examples
#' \dontrun{
#' schedule <- load_nfl_schedule(2024:2025)
#' clear_nflrest_cache()
#' }
#' @export
load_nfl_schedule <- function(seasons = 2024:2025, refresh = FALSE,
                              cache = getOption("nflrest.cache", TRUE),
                              fix_neutral_venues = TRUE) {
  if (!is.numeric(seasons) || !length(seasons) || anyNA(seasons) ||
      any(!is.finite(seasons)) || any(seasons != as.integer(seasons))) {
    cli::cli_abort("`seasons` must be a non-missing integer vector.")
  }
  seasons <- sort(unique(as.integer(seasons)))
  max_season <- as.integer(format(Sys.Date(), "%Y")) + 1L
  if (any(seasons < 1999L) || any(seasons > max_season)) {
    cli::cli_abort(
      c(
        "`seasons` is outside the range supported by nflverse schedules.",
        "i" = "Use seasons between 1999 and {max_season}."
      )
    )
  }
  .check_flag(refresh, "refresh")
  .check_flag(cache, "cache")
  .check_flag(fix_neutral_venues, "fix_neutral_venues")

  key <- paste0("schedules_", paste(seasons, collapse = "_"))
  store <- .nflrest_cache()
  if (cache && !refresh && store$exists(key)) {
    return(.finish_schedule(store$get(key), fix_neutral_venues))
  }

  if (!requireNamespace("nflreadr", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "The {.pkg nflreadr} package is required for live schedule data.",
        "i" = "Install it with {.run install.packages(\"nflreadr\")}.",
        "i" = "The bundled {.data nfl_schedules_sample} works offline."
      )
    )
  }

  out <- tryCatch(
    nflreadr::load_schedules(seasons = seasons),
    error = function(e) {
      cli::cli_abort(
        c(
          "Could not download schedules from nflverse.",
          "x" = conditionMessage(e),
          "i" = "Check the network connection or use {.data nfl_schedules_sample}."
        ),
        parent = e
      )
    }
  )
  out <- tibble::as_tibble(out)
  if (cache) {
    store$set(key, out)
  }
  .finish_schedule(out, fix_neutral_venues)
}

# clear_nflrest_cache -----------------------------------------------------
#' Clear the nflrest schedule cache
#'
#' Empties the in-memory cache used by [load_nfl_schedule()].
#'
#' @return Invisibly, `TRUE`.
#'
#' @examples
#' clear_nflrest_cache()
#' @export
clear_nflrest_cache <- function() {
  .nflrest_cache()$reset()
  invisible(TRUE)
}
