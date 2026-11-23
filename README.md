# nflrest

Compute team rest disparities, travel metrics, and date-aware body-clock
shifts for NFL games, then combine them into a composite schedule-advantage
score. Data come from [nflverse](https://github.com/nflverse/nflverse-data) /
[nflreadr](https://github.com/nflverse/nflreadr).

**Status: core analysis implemented and tested.** The vignette and written
deliverables are still in progress.

## Intended audience

`nflrest` is for analysts, students, and sports researchers who want
reproducible rest and travel metrics without building their own data pipeline.

## Installation

Not yet on CRAN. Install from GitHub:

```r
# install.packages("remotes")
remotes::install_github("jbaker12/MSDS457_Final_Project")
```

The repository is named `MSDS457_Final_Project`; the package it contains is
`nflrest`.

## Usage

```r
library(nflrest)

# Rest disparity, short weeks, and byes
rest <- calculate_rest_disparity(nfl_schedules_sample)

# One row per team-game with travel miles. The default itinerary model
# chains consecutive road games; use trip_model = "round_trip" for the
# older out-and-back version.
travel <- calculate_travel_miles(nfl_schedules_sample)

# Team-season travel totals
summarize_travel_miles(travel)

# Signed clock shifts and east/west body-clock penalties
shifts <- flag_time_zone_shifts(nfl_schedules_sample)

# Composite score; positive values favor the home team
advantage <- calculate_schedule_advantage(nfl_schedules_sample)

# Season totals and ranks
season <- calc_schedule_disparity(nfl_schedules_sample)

# Compare outcomes for disadvantaged teams; min_edge drops tiny edges
eval_rest_impact(nfl_schedules_sample, advantage = advantage, min_edge = 0.5)

# Team-by-venue travel heatmap
plot_travel_matrix(travel)
```

`nfl_schedules_sample` contains the 2024 and 2025 schedules with kickoff
times, final scores, closing spreads, rest, and venue columns, so these
examples run without a network connection.

The functions accept any data frame with the required columns, not only the
bundled sample. `calculate_rest_disparity()` needs `game_id`, `gameday`,
`home_team`, and `away_team`; `calculate_travel_miles()` needs `game_id`,
`home_team`, and `away_team`, and uses `stadium_id`, `location`, `week`, and
`gameday` when present. `flag_time_zone_shifts()` uses `gametime` when present
and assumes 1:00 p.m. Eastern otherwise. A schedule from
`nflreadr::load_schedules()` with those columns can be passed directly.

Live ingestion is available through the optional `nflreadr` dependency:

```r
schedule <- load_nfl_schedule(2024:2025)
clear_nflrest_cache()
```

The first call needs network access; later calls reuse an in-memory cache.
Install `nflreadr` separately because it is a suggested dependency.

## Composite score

`calculate_schedule_advantage()` converts each component to points and sums
them:

- rest: `0.25` points per day of rest disparity
- travel: `0.5` points per 1,000 miles of extra travel
- time zone: `0.35` points per hour of body-clock penalty

Travel disparity is the away team's miles minus the home team's; time-zone
disparity is the away team's penalty minus the home team's. All weights are
arguments, and the raw parts are returned with the score. The defaults are a
starting point, not fitted values; `eval_rest_impact()` is the check against
outcomes. A missing part, such as rest in a season opener, counts as zero and
shows up in `components_missing`.

## Assumptions

- Rest is calendar days between a team's games within a season. Season openers
  are `NA` because the package does not invent a prior game; nflverse reports
  7 days for openers.
- Home games are zero travel miles. Neutral-site games charge both teams a
  trip.
- Travel is great-circle distance from the team's home airport to the venue,
  not actual flight routing. The default itinerary model chains road games in
  adjacent weeks and resets on a bye; the `"round_trip"` model treats every
  road game as an independent out-and-back trip and overstates multi-game road
  stretches.
- Time-zone shifts use the scheduled kickoff instant and IANA time zones, so
  daylight-saving changes are handled. Eastward shifts receive a larger
  default body-clock penalty than westward shifts.
- Injury evaluation depends on an injury report supplied by the user; no
  injury data are bundled.
- Historical team abbreviations (for example `OAK`, `SD`, `STL`) are not yet
  mapped to airport rows.

## Bundled data

| Dataset | Rows | What it is |
|---|---|---|
| `nfl_schedules_sample` | 570 | 2024-2025 schedule with kickoff, score, spread, rest, and venue columns |
| `nfl_airports` | 32 | Each team's home airport, coordinates, and time zone |
| `nfl_stadiums` | 37 | NFL and neutral-site venues with coordinates and time zones |

## License

MIT. See [LICENSE](LICENSE).

## Citation

If you use `nflrest`, cite the package and the nflverse schedule data:

> Baker, Jake. 2026. *nflrest: Compute NFL Rest, Travel, and Schedule-Advantage
> Metrics*. R package version 0.0.0.9000.
> https://github.com/jbaker12/MSDS457_Final_Project.
>
> nflverse. 2026. *nflverse Data*.
> https://github.com/nflverse/nflverse-data.

`citation("nflrest")` returns the package entry.
