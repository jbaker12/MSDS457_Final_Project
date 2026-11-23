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

# .fixture_tz_schedule ---------------------------------------------------
# Real teams and venues with known clock shifts. Game identifiers follow
# nflverse's away_home convention.
.fixture_tz_schedule <- tibble::tibble(
  game_id = c("TZ1_BUF_KC", "TZ2_SEA_NYJ", "TZ3_NE_JAX", "TZ4_GB_PHI"),
  season = 2024L,
  week = c(2L, 2L, 7L, 1L),
  gameday = c("2024-09-15", "2024-09-15", "2024-10-20", "2024-09-06"),
  gametime = c("13:00", "13:00", "09:30", "20:15"),
  home_team = c("KC", "NYJ", "JAX", "PHI"),
  away_team = c("BUF", "SEA", "NE", "GB"),
  location = c("Home", "Home", "Neutral", "Neutral"),
  stadium_id = c("KAN00", "NYC01", "LON00", "SAO00")
)

# .fixture_road_trip -----------------------------------------------------
# Buffalo plays two consecutive road games, then returns home. The itinerary
# model should charge BUF -> KC + KC -> DEN, then DEN -> BUF.
.fixture_road_trip <- tibble::tibble(
  game_id = c("RT1_BUF_KC", "RT2_BUF_DEN", "RT3_MIA_BUF"),
  season = 2024L,
  week = c(2L, 3L, 4L),
  gameday = c("2024-09-15", "2024-09-22", "2024-09-29"),
  gametime = rep("13:00", 3),
  home_team = c("KC", "DEN", "BUF"),
  away_team = c("BUF", "BUF", "MIA"),
  location = rep("Home", 3),
  stadium_id = c("KAN00", "DEN00", "BUF00")
)

# .fixture_bye_trip ------------------------------------------------------
# Same as the road trip, but a bye separates the two road games. The model
# should reset Buffalo to home between them.
.fixture_bye_trip <- tibble::tibble(
  game_id = c("BY1_BUF_KC", "BY2_BUF_DEN", "BY3_MIA_BUF"),
  season = 2024L,
  week = c(2L, 4L, 5L),
  gameday = c("2024-09-15", "2024-09-29", "2024-10-06"),
  gametime = rep("13:00", 3),
  home_team = c("KC", "DEN", "BUF"),
  away_team = c("BUF", "BUF", "MIA"),
  location = rep("Home", 3),
  stadium_id = c("KAN00", "DEN00", "BUF00")
)

# .fixture_advantage_schedule --------------------------------------------
# Two week-1 games with different rest, then week-2 games where the road team
# is better rested (10 days versus 7). Real teams keep the travel and
# time-zone calculations meaningful.
.fixture_advantage_schedule <- tibble::tibble(
  game_id = c("AD1_BUF_KC", "AD2_MIA_NYJ", "AD3_BUF_MIA", "AD4_KC_NYJ"),
  season = 2024L,
  week = c(1L, 1L, 2L, 2L),
  gameday = c("2024-09-05", "2024-09-08", "2024-09-15", "2024-09-15"),
  gametime = c("20:15", "13:00", "13:00", "13:00"),
  home_team = c("KC", "NYJ", "MIA", "NYJ"),
  away_team = c("BUF", "MIA", "BUF", "KC"),
  location = rep("Home", 4),
  stadium_id = c("KAN00", "NYC01", "MIA00", "NYC01")
)

# .fixture_advantage -----------------------------------------------------
# Precomputed game-level edge for outcome tests. Positive values favor home.
.fixture_advantage <- tibble::tibble(
  game_id = paste0("E", 1:6),
  season = 2024L,
  week = c(1L, 1L, 2L, 2L, 3L, 3L),
  home_team = c("A", "B", "C", "D", "E", "F"),
  away_team = c("B", "A", "D", "C", "F", "E"),
  home_score = c(24L, 17L, 20L, 10L, 30L, 23L),
  away_score = c(17L, 20L, 20L, 27L, 20L, 20L),
  spread_line = c(3, 3, 0, -7, -3, 3),
  rest_disparity = c(7, 5, 0, -7, -5, 0),
  travel_disparity = c(0, 0, 0, 0, 0, 0),
  tz_penalty_disparity = c(0, 0, 0, 0, 0, 0),
  schedule_advantage = c(7, 5, 0, -7, -5, 0)
)

# .fixture_injuries ------------------------------------------------------
# Unavailable players for the disadvantaged side in each non-zero-edge game.
# Weeks 1-3 appear in the table, so unlisted teams in those weeks count as
# zero rather than missing.
.fixture_injuries <- tibble::tibble(
  season = 2024L,
  week = c(1L, 1L, 1L, 2L, 2L, 3L, 3L),
  team = c("B", "B", "A", "D", "D", "E", "E"),
  report_status = c("Out", "Out", "Doubtful", "Out", "Out",
                    "Out", "Out")
)
