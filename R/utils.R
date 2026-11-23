# utils.R -----------------------------------------------------------------
# Internal helpers used by the exported functions.

# Basic input checks ------------------------------------------------------
.check_frame <- function(x, cols, arg = "schedule") {
  if (!is.data.frame(x)) {
    cli::cli_abort("`{arg}` must be a data frame or tibble.")
  }
  missing <- setdiff(cols, names(x))
  if (length(missing)) {
    cli::cli_abort(
      c(
        "`{arg}` is missing required columns.",
        "x" = "Missing: {.and {.field {missing}}}."
      )
    )
  }
  invisible(TRUE)
}

.check_rows <- function(x, arg = "schedule") {
  if (!nrow(x)) {
    cli::cli_abort("`{arg}` must have at least one row.")
  }
  invisible(TRUE)
}

.check_game_ids <- function(x) {
  if (anyDuplicated(x$game_id)) {
    cli::cli_abort("`game_id` must uniquely identify games.")
  }
  invisible(TRUE)
}

.check_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("`{arg}` must be `TRUE` or `FALSE`.")
  }
  invisible(TRUE)
}

.check_positive <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x <= 0) {
    cli::cli_abort("`{arg}` must be a single positive number.")
  }
  invisible(TRUE)
}

.check_nonnegative <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0) {
    cli::cli_abort("`{arg}` must be a single non-negative number.")
  }
  invisible(TRUE)
}

.check_team_lookup <- function(games, team_home, hint = FALSE) {
  unknown <- setdiff(
    unique(c(as.character(games$home_team), as.character(games$away_team))),
    as.character(team_home$team)
  )
  if (length(unknown)) {
    message <- c(
      "Teams in `schedule` are missing from `team_home`.",
      "x" = "Unknown: {.and {.val {unknown}}}."
    )
    if (isTRUE(hint)) {
      message <- c(
        message,
        "i" = "Historical abbreviations need their own home-airport rows."
      )
    }
    cli::cli_abort(message)
  }
  invisible(TRUE)
}

# Time-zone and time-of-day checks ---------------------------------------
.check_time_zone_column <- function(x, arg, allow_na = FALSE) {
  bad <- if (allow_na) {
    !is.na(x) & !x %in% OlsonNames()
  } else {
    is.na(x) | !x %in% OlsonNames()
  }
  if (any(bad)) {
    cli::cli_abort("`{arg}` must contain IANA time-zone names.")
  }
  invisible(TRUE)
}

.check_clock <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !grepl("^([01][0-9]|2[0-3]):[0-5][0-9]$", x)) {
    cli::cli_abort("`{arg}` must use 24-hour {.val HH:MM} format.")
  }
  invisible(TRUE)
}

# Small utilities ---------------------------------------------------------
# Return a column when present, otherwise a typed default of length `n`.
.column_or <- function(x, name, default, n = nrow(x)) {
  if (name %in% names(x)) x[[name]] else rep(default, n)
}

.as_gameday <- function(x, arg = "gameday") {
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (is.character(x)) {
    out <- tryCatch(
      as.Date(x),
      error = function(e) rep(as.Date(NA), length(x))
    )
    bad <- is.na(out) & !is.na(x)
    if (any(bad)) {
      cli::cli_abort(
        c(
          "`{arg}` contains values that are not ISO 8601 dates.",
          "x" = "First bad value: {.val {x[which(bad)[1]]}}."
        )
      )
    }
    return(out)
  }
  cli::cli_abort("`{arg}` must be a Date, POSIXct, or ISO 8601 character vector.")
}

# Geometry ---------------------------------------------------------------
# Great-circle distance in miles (mean Earth radius 3958.7613 mi).
.haversine_miles <- function(lat1, lon1, lat2, lon2) {
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat / 2)^2 +
    cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  2 * 3958.7613 * asin(pmin(1, sqrt(a)))
}

# Grouping and summaries --------------------------------------------------
.group_key <- function(cols) {
  do.call(paste, c(cols, sep = "\r"))
}

# Apply `fn` to each group and bind the one-row results.
.group_rows <- function(data, by, fn) {
  key <- .group_key(lapply(by, function(column) data[[column]]))
  groups <- split(seq_len(nrow(data)), key)
  rows <- lapply(groups, function(i) fn(data[i, , drop = FALSE]))
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

.mean_or_na <- function(x) {
  if (any(!is.na(x))) mean(x, na.rm = TRUE) else NA_real_
}

.sum_or_na <- function(x) {
  if (any(!is.na(x))) sum(x, na.rm = TRUE) else NA_real_
}

.max_or_na <- function(x) {
  if (any(!is.na(x))) max(x, na.rm = TRUE) else NA_real_
}

.rate <- function(numerator, denominator) {
  if (isTRUE(denominator > 0)) numerator / denominator else NA_real_
}

.warn_affected <- function(affected, message, hint = NULL) {
  bullets <- c(
    message,
    "i" = "Affected games: {.and {.val {affected}}}."
  )
  if (!is.null(hint)) {
    bullets <- c(bullets, "i" = hint)
  }
  cli::cli_warn(bullets)
}

# Game/team helpers -------------------------------------------------------
# One row per team per game. Home rows come first, then away rows, matching
# the order used by every downstream join and by `side_column()`.
.game_team_long <- function(games, keep = character()) {
  n <- nrow(games)
  out <- tibble::tibble(
    game_id = rep(games$game_id, 2L),
    team = c(as.character(games$home_team), as.character(games$away_team)),
    opponent = c(as.character(games$away_team), as.character(games$home_team)),
    is_home = rep(c(TRUE, FALSE), each = n)
  )
  for (column in keep) {
    if (column %in% names(games)) {
      out[[column]] <- rep(games[[column]], 2L)
    }
  }
  out
}

# Pull one side's column back into game order.
.side_column <- function(data, game_ids, is_home, column) {
  side <- data[data$is_home == is_home, , drop = FALSE]
  side[[column]][match(game_ids, side$game_id)]
}

# Resolve each game's venue from the stadium table, falling back to the home
# team's airport only for normal home games. Returns game-level columns plus
# flags for stale neutral venues and missing coordinates/time zones.
.game_venue_lookup <- function(games, team_home, venues) {
  n <- nrow(games)
  location <- as.character(.column_or(games, "location", "Home", n))
  location[is.na(location)] <- "Home"
  stadium_id <- as.character(
    .column_or(games, "stadium_id", NA_character_, n)
  )

  home_row <- match(games$home_team, team_home$team)
  venue_row <- match(stadium_id, venues$stadium_id)

  latitude <- .column_or(venues, "latitude", NA_real_, nrow(venues))[venue_row]
  longitude <- .column_or(venues, "longitude", NA_real_, nrow(venues))[venue_row]
  venue <- .column_or(venues, "stadium", NA_character_, nrow(venues))[venue_row]
  time_zone <- .column_or(
    venues, "time_zone", NA_character_, nrow(venues)
  )[venue_row]

  home_airport <- .column_or(
    team_home, "airport", NA_character_, nrow(team_home)
  )[home_row]
  home_latitude <- .column_or(
    team_home, "latitude", NA_real_, nrow(team_home)
  )[home_row]
  home_longitude <- .column_or(
    team_home, "longitude", NA_real_, nrow(team_home)
  )[home_row]
  home_time_zone <- .column_or(
    team_home, "time_zone", NA_character_, nrow(team_home)
  )[home_row]

  normal_home <- location == "Home"
  fallback <- is.na(latitude) & normal_home
  latitude[fallback] <- home_latitude[fallback]
  longitude[fallback] <- home_longitude[fallback]
  venue[fallback] <- home_airport[fallback]

  # A normal home game is always in the home team's time zone, regardless of
  # whether the venue lookup is complete.
  time_zone[normal_home] <- home_time_zone[normal_home]
  venue[normal_home & is.na(venue)] <-
    home_airport[normal_home & is.na(venue)]

  home_stadium <- if ("home_stadium_id" %in% names(team_home)) {
    team_home$home_stadium_id[home_row]
  } else {
    rep(NA_character_, n)
  }
  suspect <- !normal_home & !is.na(home_stadium) &
    !is.na(stadium_id) & stadium_id == home_stadium
  if (any(suspect)) {
    latitude[suspect] <- NA_real_
    longitude[suspect] <- NA_real_
    venue[suspect] <- NA_character_
    time_zone[suspect] <- NA_character_
  }

  source <- ifelse(
    !is.na(venue_row), "stadium",
    ifelse(fallback, "home airport", "missing")
  )
  source[suspect] <- "home venue (neutral game)"

  tibble::tibble(
    location = location,
    stadium_id = stadium_id,
    venue_stadium_id = stadium_id,
    venue = venue,
    venue_source = source,
    venue_latitude = latitude,
    venue_longitude = longitude,
    venue_time_zone = time_zone,
    at_home = normal_home,
    suspect = suspect,
    missing_coordinates = !normal_home & is.na(latitude) & !suspect,
    missing_time_zone = !normal_home & is.na(time_zone) & !suspect
  )
}
