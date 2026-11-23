# globals.R ---------------------------------------------------------------
# Data objects used in function defaults and ggplot aesthetics are not
# detected by codetools.

utils::globalVariables(c(
  "nfl_airports", "nfl_stadiums", "nfl_schedules_sample", "team", "venue",
  "trip_miles"
))
