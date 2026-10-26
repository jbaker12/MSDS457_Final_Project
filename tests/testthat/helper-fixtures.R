# helper-fixtures.R -------------------------------------------------------
# Offline static fixtures shared across test files. No network access.

# .fixture_schedule ------------------------------------------------------
# Six games covering a bye, a short week, and equal rest. Fake team codes
# keep the arithmetic readable; travel tests use real teams instead.
.fixture_schedule <- tibble::tibble(
  game_id = c("W1_A_B", "W1_C_E", "W2_C_A", "W2_B_D", "W3_D_A", "W4_C_B",
              "W5_A_C"),
  season = 2024L,
  week = c(1L, 1L, 2L, 2L, 3L, 4L, 5L),
  gameday = c(
    "2024-09-08", "2024-09-08", "2024-09-15", "2024-09-12",
    "2024-09-22", "2024-09-29", "2024-10-06"
  ),
  home_team = c("A", "C", "C", "B", "D", "C", "A"),
  away_team = c("B", "E", "A", "D", "A", "B", "C")
)

# .fixture_travel_schedule -----------------------------------------------
# One normal home game and one neutral-site game with known coordinates.
.fixture_travel_schedule <- tibble::tibble(
  game_id = c("H1_KC_BUF", "N1_PHI_GB"),
  season = 2024L,
  week = c(2L, 1L),
  gameday = c("2024-09-15", "2024-09-06"),
  home_team = c("KC", "PHI"),
  away_team = c("BUF", "GB"),
  location = c("Home", "Neutral"),
  stadium_id = c("KAN00", "SAO00")
)

# .expected_buf_to_kan ---------------------------------------------------
# Buffalo airport -> Arrowhead Stadium, one way, in miles.
.expected_buf_to_kan <- 862.78
# Philadelphia airport -> Arena Corinthians, one way.
.expected_phl_to_sao <- 4757.92
# Green Bay airport -> Arena Corinthians, one way.
.expected_grb_to_sao <- 5385.91
