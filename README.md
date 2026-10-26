# nflrest

Compute team rest disparities and travel metrics for NFL games, using schedule
data from [nflverse](https://github.com/nflverse/nflverse-data) /
[nflreadr](https://github.com/nflverse/nflreadr).

**Status: work in progress.** Rest and travel metrics are implemented and
tested against bundled data. Time-zone shifts and a composite schedule
advantage are planned; see [ROADMAP.md](ROADMAP.md).

## Installation

Not yet on CRAN. Install from a local checkout:

```r
# install.packages("remotes")
remotes::install_local("path/to/nflrest")
```

## What works today

```r
library(nflrest)

# Rest disparity, short weeks, and byes
rest <- calculate_rest_disparity(nfl_schedules_sample)

# One row per team-game with round-trip travel miles
travel <- calculate_travel_miles(nfl_schedules_sample)

# Team-season travel totals
summarize_travel_miles(travel)
```

`nfl_schedules_sample` contains the 2024 and 2025 schedules with rest and
venue columns, so these examples run without a network connection. Live
ingestion through nflreadr is planned but not implemented yet.

## Bundled data

| Dataset | Rows | What it is |
|---|---|---|
| `nfl_schedules_sample` | 570 | 2024-2025 schedule with rest and venue columns |
| `nfl_airports` | 32 | Each team's home airport, coordinates, and time zone |
| `nfl_stadiums` | 37 | NFL and neutral-site venues with coordinates and time zones |

## Planned

- `flag_time_zone_shifts()` for date-aware body-clock shifts
- `calculate_schedule_advantage()` combining rest, travel, and time zones
- Memoised nflreadr ingestion and a cache
- Vignette and pkgdown site

## Code of Conduct

Please note that this project is released with a
[Contributor Code of Conduct](https://www.contributor-covenant.org/).
By participating you agree to uphold this code.
