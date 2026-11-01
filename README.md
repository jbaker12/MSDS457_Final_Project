# nflrest

Compute team rest disparities and travel metrics for NFL games, using schedule
data from [nflverse](https://github.com/nflverse/nflverse-data) /
[nflreadr](https://github.com/nflverse/nflreadr).

**Status: work in progress.** Rest and travel metrics are implemented and
tested against bundled data. Time-zone shifts and a composite schedule
advantage are planned; see [ROADMAP.md](ROADMAP.md).

## Intended audience

`nflrest` is for analysts, students, and sports researchers who want
reproducible rest and travel metrics without building their own data pipeline.

## Installation

Not yet on CRAN. Install from GitHub:

```r
# install.packages("remotes")
remotes::install_github("jbaker12/MSDS457_Final_Project")
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

## Assumptions and limitations

- Rest is calendar days between a team's games within a season. Season openers
  are `NA` because the sample has no prior game for that team; nflverse reports
  7 days for openers, but this package does not.
- Home games are zero travel miles. Neutral-site games charge both teams a
  round trip.
- Travel is a round-trip great-circle distance from the team's home airport to
  the venue, not actual flight routing. Consecutive road or neutral-site games
  are treated as separate out-and-back trips, so the model overstates travel
  for those stretches.
- Time-zone shifts and a composite schedule advantage are not implemented.

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

## License

MIT. See [LICENSE](LICENSE).

## Citation

If you use `nflrest`, cite the package and the nflverse schedule data:

> Baker, Jake. 2026. *nflrest: Compute Team Rest Disparities and Travel
> Metrics for NFL Games*. R package version 0.0.0.9000.
> https://github.com/jbaker12/MSDS457_Final_Project.
>
> nflverse. 2026. *nflverse Data*.
> https://github.com/nflverse/nflverse-data.

## Code of Conduct

Please note that this project is released with a
[Contributor Code of Conduct](https://www.contributor-covenant.org/version/2/1/code_of_conduct/).
By participating you agree to uphold this code.
