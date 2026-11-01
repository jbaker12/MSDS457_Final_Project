# Data build

This directory contains the scripts that create the three datasets in `data/`.
Run them from the package root, in this order:

```r
source("data-raw/nfl_airports.R")
source("data-raw/nfl_stadiums.R")
source("data-raw/process_data.R")
```

`nfl_airports.R` and `nfl_stadiums.R` write `data/nfl_airports.rda` and
`data/nfl_stadiums.rda`. `process_data.R` reads the stadium table, retrieves
the 2024-2025 schedule with `nflreadr::load_schedules()`, applies the
neutral-site venue overrides, and writes `data/nfl_schedules_sample.rda`.

## Requirements

- R and the `tibble` package.
- `nflreadr` for `process_data.R`.
- Network access for the initial nflverse download; the saved datasets are
  bundled so examples and tests run offline.

## Sources and overrides

- Airport coordinates are compiled from public IATA/FAA airport reference
  data.
- Stadium coordinates come from the Wikipedia article for each venue.
- `process_data.R` maps neutral-site games to the venue where they were
  actually played. `DUB01`, `BER01`, and `MAD01` are package identifiers for
  venues that nflverse did not record in `load_schedules()` when the sample
  was built.

The scripts end with `stopifnot()` checks and save each `.rda` file with xz
compression and version 3. Rebuild all three files if a source table or the
nflverse schedule changes.
