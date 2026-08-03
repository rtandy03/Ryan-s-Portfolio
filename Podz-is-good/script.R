
### Brandin Podziemski Supremecy --------------------------------------------

getwd()
setwd("C:/Users/ryant/OneDrive/Documents/Ryan-s-Portfolio/Podz-is-good")

library(tidyverse)

# Load Datasets 

games_df <- read.csv("data/Games.csv")
players_df <- read.csv("data/Players.csv")
player_stats_df <- read.csv("data/PlayerStatistics.csv")
player_statsE_df <- read.csv("data/PlayerStatisticsExtended.csv")
team_stats_df <- read.csv("data/TeamStatistics.csv")
team_statsE_df <- read.csv("data/TeamStatisticsExtended.csv")


### Data cleaning -----------------------------------------------------------


# List of first and second string point guards

point_guards <- c(
  "Trae Young",
  "C.J. McCollum",
  "Ryan Nembhard",
  "Derrick White",
  "Payton Pritchard",
  "Dennis Schroder",
  "Keon Ellis",
  "LaMelo Ball",
  "Tre Mann",
  "Josh Giddey",
  "Rob Dillingham",
  "James Harden",
  "Dennis Schroder",
  "Kyrie Irving",
  "Dante Exum",
  "Jamal Murray",
  "Tyus Jones",
  "Cade Cunningham",
  "Marcus Sasser",
  "Stephen Curry",
  "De'Anthony Melton",
  "Brandin Podziemski",
  "Fred VanVleet",
  "Marcus Smart",
  "Tyrese Haliburton",
  "T.J. McConnell",
  "Darius Garland",
  "Kris Dunn",
  "Luka Doncic",
  "Collin Sexton",
  "Ja Morant",
  "Derrick Rose",
  "Davion Mitchell",
  "Kasparas Jakucionis",
  "Kevin Porter Jr.",
  "AJ Green",
  "Mike Conley",
  "Monte Morris",
  "Dejounte Murray",
  "Jose Alvarado",
  "Jalen Brunson",
  "Miles McBride",
  "Shai Gilgeous-Alexander",
  "Cason Wallace",
  "Anthony Black",
  "Cole Anthony",
  "Tyrese Maxey",
  "Kyle Lowry",
  "Devin Booker",
  "Tyus Jones",
  "Anfernee Simons",
  "Scoot Henderson",
  "De'Aaron Fox",
  "Dylan Harper",
  "De'Aaron Fox",
  "Dylan Harper",
  "Immanuel Quickley",
  "Davion Mitchell",
  "Keyonte George",
  "Jordan Clarkson",
  "Tyler Kolek",
  "Jordan Poole"
)

warriors_roster <- c(
  "Gary Payton II",
  "Yaxel Lendeborg",
  "Brandin Podziemski",
  "Will Richard",
  "Moses Moody",
  "Kristaps Porziņģis",
  "De'Anthony Melton",
  "Jimmy Butler III",
  "Gui Santos",
  "LJ Cryer",
  "Nate Williams",
  "Al Horford",
  "Draymond Green",
  "Lajae Jones",
  "Charles Bassey",
  "Stephen Curry",
  "Seth Curry",
  "Malevy Leons"
)

current_pg_df <- players_df %>% 
  unite(name, firstName, lastName, sep = " ") %>% 
  filter(name %in% point_guards) %>% 
  full_join(player_statsE_df, by = "personId")

warriors_df <- players_df %>% 
  unite(name, firstName, lastName, sep = " ") %>% 
  filter(name %in% warriors_roster) %>% 
  full_join(player_statsE_df, by = "personId")

colnames(current_pg_df)  


# Vector of columns I deemed important

imp_cols <- c(
  "comment", "personId", "name", "heightInches", "gameId", "gameDateTimeEst",
  "gameType", "gameLabel", "gameSubLabel", "seriesGameNumber", "win",
  "home", "playerteamId", "opponentteamId", "playerteamName", "opponentteamName",
  "numMinutes", "points", "assists", "reboundsTotal",
  "reboundsOffensive", "reboundsDefensive", "fieldGoalsMade", "fieldGoalsAttempted",
  "fieldGoalsPercentage", "threePointersMade", "threePointersAttempted",
  "threePointersPercentage", "freeThrowsMade", "freeThrowsAttempted",
  "freeThrowsPercentage", "steals", "blocks", "turnovers", "foulsPersonal",
  "plusMinusPoints", "offensiveRating", "defensiveRating", "netRating",
  "assistPercentage", "assistToTurnoverRatio", "assistRatio", "reboundPercentage",
  "teamTurnoverPercentage", "effectiveFieldGoalPercentage", "trueShootingPercentage",
  "usagePercentage", "pace", "pacePer40", "possessions", "pointsOffTurnovers", 
  "pointsSecondChance", "pointsFastBreak","pointsInPaint", "percentPoints2Point",
  "percentPoints3Point", "percentPointsFastBreak"
)


# Select important columns and more separating

current_pg_df <- current_pg_df %>% 
  select(imp_cols) %>% 
  separate(gameDateTimeEst, c("game_date", "time_est"), sep = " ") %>% 
  separate(game_date, c("year", "month", "day"), sep = "-")

warriors_df <- warriors_df %>% 
  select(imp_cols) %>% 
  separate(gameDateTimeEst, c("game_date", "time_est"), sep = " ") %>% 
  separate(game_date, c("year", "month", "day"), sep = "-")


# Months of first and second half of season for new column purposes

first_half <- c(10, 11, 12)
second_half <- c(1, 2, 3, 4, 5, 6)

current_pg_df <- current_pg_df %>% 
  mutate(
    year = as.numeric(year),
    month = as.numeric(month),
    day = as.numeric(day),
    season = case_when(
      month %in% first_half ~ paste0(year, "-", substr(year + 1, 3, 4)),
      month %in% second_half ~ paste0(year - 1, "-", substr(year, 3, 4))
    )
  ) %>% 
  select(season, everything())

warriors_df <- warriors_df %>% 
    mutate(
    year = as.numeric(year),
    month = as.numeric(month),
    day = as.numeric(day),
    season = case_when(
      month %in% first_half ~ paste0(year, "-", substr(year + 1, 3, 4)),
      month %in% second_half ~ paste0(year - 1, "-", substr(year, 3, 4))
    )
  ) %>% 
  select(season, everything())


# Make yearly totals dataframe

yearly_totals_df <- current_pg_df %>% 
  mutate(numMinutes = as.double(numMinutes),
         off_impact = effectiveFieldGoalPercentage * (points + reboundsTotal + assists - turnovers) / max(c(1,possessions)) * usagePercentage * 1000) %>% 
  filter(gameType %in% c("Regular Season", "Playoffs"), 
         numMinutes > 0, 
         !is.na(name)) %>% 
  group_by(gameType, season, personId, name) %>% 
  summarise(total_points = sum(points),
            total_assists = sum(assists, na.rm = TRUE),
            total_rebounds = sum(reboundsTotal, na.rm = TRUE),
            total_steals = sum(steals, na.rm = TRUE),
            total_blocks = sum(blocks, na.rm = TRUE),
            total_minutes = sum(numMinutes, na.rm = TRUE),
            total_fgm = sum(fieldGoalsMade, na.rm = TRUE),
            total_fga = sum(fieldGoalsAttempted, na.rm = TRUE),
            total_3ptm = sum(threePointersMade, na.rm = TRUE),
            total_3pta = sum(threePointersAttempted, na.rm = TRUE),
            total_ftm = sum(freeThrowsMade, na.rm = TRUE),
            total_fta = sum(freeThrowsAttempted, na.rm = TRUE),
            total_turnovers = sum(turnovers, na.rm = TRUE),
            total_personal_fouls = sum(foulsPersonal, na.rm = TRUE),
            total_plusMinus = sum(plusMinusPoints, na.rm = TRUE),
            total_off_rating = sum(offensiveRating, na.rm = TRUE),
            total_def_rating = sum(defensiveRating, na.rm = TRUE),
            total_net_rating = sum(netRating, na.rm = TRUE),
            total_eff_fg_percentage = sum(effectiveFieldGoalPercentage, na.rm = TRUE),
            total_true_shooting_percentage = sum(trueShootingPercentage, na.rm = TRUE),
            total_usage = sum(usagePercentage, na.rm = TRUE),
            total_pace = sum(pace, na.rm = TRUE),
            total_possessions = sum(possessions, na.rm = TRUE),
            total_second_chance_pts = sum(pointsSecondChance, na.rm = TRUE),
            total_fast_break_pts = sum(pointsFastBreak, na.rm = TRUE),
            total_efg_perc = sum(effectiveFieldGoalPercentage, na.rm = TRUE),
            total_off_impact = sum(off_impact, na.rm = TRUE),
            total_minutes = sum(numMinutes, na.rm = TRUE),
            total_games = n(),
            .groups = "drop") %>% 
  transmute(
    gameType = gameType,
    personId = personId,
    season = season,
    name = name,
    gameType = gameType,
    ppg = round(total_points / total_games, 2),
    apg = round(total_assists / total_games, 2),
    rpg = round(total_rebounds / total_games,2),
    steals_pg = round(total_steals / total_games, 2),
    blocks_pg = round(total_blocks / total_games, 2),
    mpg = round(total_minutes / total_games, 2),
    avg_fgm = round(total_fgm / total_games, 2),
    avg_fga = round(total_fga / total_games, 2),
    avg_3ptm = round(total_3ptm / total_games, 2),
    avg_3pta = round(total_3pta / total_games, 2),
    avg_ftm = round(total_ftm / total_games, 2),
    avg_fta = round(total_fta / total_games, 2),
    avg_turnovers = round(total_turnovers / total_games, 2),
    avg_personalFoul = round(total_personal_fouls / total_games, 2),
    avg_plusMinus = round(total_plusMinus / total_games, 2),
    avg_off_rating = round(total_off_rating / total_games, 2),
    avg_def_rating = round(total_def_rating / total_games, 2),
    eff_fg_percent_pg = round(total_eff_fg_percentage / total_games, 2),
    avg_true_shooting = round(total_true_shooting_percentage / total_games, 2),
    avg_usage = round(total_usage / total_games, 2),
    avg_pace = round(total_pace / total_games, 2),
    avg_possessions = round(total_possessions / total_games, 2),
    avg_secondChance_pts = round(total_second_chance_pts / total_games, 2),
    avg_fastBreak_pts = round(total_fast_break_pts / total_games, 2),
    perc_eff_fg = round(total_eff_fg_percentage / total_games * 100, 2),
    perc_fg = round(avg_fgm / avg_fga * 100, 2),
    perc_3pt = round(avg_3ptm / avg_3pta * 100, 2),
    perc_ft = round(avg_ftm / avg_fta * 100, 2),
    avg_minutes = round(total_minutes / total_games, 2),
    avg_off_impact = total_off_impact / total_games,
    image = paste0(
      "https://cdn.nba.com/headshots/nba/latest/1040x760/",
      personId,
      ".png"
    )
  )

# Filter Regular Season and Playoffs and BP era

bp_era = c("2023-24", "2024-25", "2025-26")

reg_season_df <- yearly_totals_df %>% 
  filter(gameType == "Regular Season"
         & season %in% bp_era)

playoffs_df <- yearly_totals_df %>% 
  filter(gameType == "Playoffs"
         & season %in% bp_era)


# Visualizations ----------------------------------------------------------

library(ggimage)

ggplot(data = reg_season_df, aes(x = avg_minutes, y = avg_off_impact)) +
  geom_image(aes(image = image)) +
  facet_wrap(~ season) + 
  labs(
    title = "Offensive Impact vs Minutes (23/24-25/26)",
    x = "Minutes per game",
    y = "Offensive Impact"
  ) +
  theme_minimal()
ggsave("Offensive-Impact_vs_Minutes.pdf",
       width = 12,
       height = 12,
       units = "cm")
ggsave("Offensive-Impact_vs_Minutes.pdf")

  






