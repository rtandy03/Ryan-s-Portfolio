
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

current_pg_df <- players_df %>% 
  unite(name, firstName, lastName, sep = " ") %>% 
  filter(name %in% point_guards) %>% 
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


# Make yearly totals dataframe

yearly_totals_df <- current_pg_df %>% 
  mutate(numMinutes = as.double(numMinutes)) %>% 
  filter(gameType %in% c("Regular Season", "Playoffs"), 
         numMinutes > 0, 
         !is.na(name)) %>% 
  group_by(gameType, season, personId, name, year) %>% 
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
            total_games = n(),
            .groups = "drop") %>% 
  mutate(
    ppg = total_points / total_games,
    apg = total_assists / total_games,
    rpg = total_rebounds / total_games,
    steals_pg = total_steals / total_games,
    blocks_pg = total_blocks / total_games,
    mpg = total_minutes / total_games,
    avg_fgm = total_fgm / total_games,
    avg_fga = total_fga / total_games,
    avg_3ptm = total_3ptm / total_games,
    avg_3pta = total_3pta / total_games,
    avg_ftm = total_ftm / total_games,
    avg_fta = total_fta / total_games,
    avg_turnovers = total_turnovers / total_games,
    avg_personalFoul = total_personal_fouls / total_games,
    avg_plusMinus = total_plusMinus / total_games,
    avg_off_rating = total_off_rating / total_games,
    avg_def_rating = total_def_rating / total_games,
    eff_fg_percent_pg = total_eff_fg_percentage / total_games,
    avg_true_shooting = total_true_shooting_percentage / total_games,
    avg_usage = total_usage / total_games,
    avg_pace = total_pace / total_games,
    avg_possessions = total_possessions / total_games,
    avg_secondChance_pts = total_second_chance_pts / total_games,
    avg_fastBreak_pts = total_fast_break_pts / total_games
  )




