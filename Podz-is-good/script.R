
# Brandin Podziemski Supremecy --------------------------------------------

getwd()
setwd("C:/Users/ryant/OneDrive/Documents/Ryan-s-Portfolio/Podz-is-good")

library(tidyverse)

games_df <- read.csv("data/Games.csv")
players_df <- read.csv("data/Players.csv")
player_stats_df <- read.csv("data/PlayerStatistics.csv")
player_statsE_df <- read.csv("data/PlayerStatisticsExtended.csv")
team_stats_df <- read.csv("data/TeamStatistics.csv")
team_statsE_df <- read.csv("data/TeamStatisticsExtended.csv")


# Data cleaning -----------------------------------------------------------


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

imp_cols <- c(
  "personId", "name", "heightInches", "gameId", "gameDateTimeEst",
  "gameType", "gameLabel", "gameSubLabel", "seriesGameNumber", "win",
  "home", "playerteamId", "opponentteamId", "playerteamName", "opponentteamName",
  "startingPosition", "numMinutes", "points", "assists", "reboundsTotal",
  "reboundsOffensive", "reboundsDefensive", "fieldGoalsMade", "fieldGoalsAttempted",
  "fieldGoalsPercentage", "threePointersMade", "threePointersAttempted",
  "threePointersPercentage", "freeThrowsMade", "freeThrowsAttempted",
  "freeThrowsPercentage", "steals", "blocks", "turnovers", "foulsPersonal",
  "plusMinusPoints", "offensiveRating", "defensiveRating", "netRating",
  "assistPercentage", "assistToTurnoverRatio", "assistRatio", "reboundPercentage",
  "teamTurnoverPercentage", "effectiveFieldGoalPercentage", "trueShootingPercentage",
  "usagePercentage", "pace", "pacePer40", "playerImpactEstimate",
  "possessions", "pointsOffTurnovers", "pointsSecondChance", "pointsFastBreak",
  "pointsInPaint", "percentPoints2Point", "percentPoints3Point", "percentPointsFastBreak",
  "percentPointsFreeThrow", "percentTeamPoints"
)

current_pg_df <- current_pg_df %>% 
  select(imp_cols) %>% 
  separate(gameDateTimeEst, c("game_date", "time_est"), sep = " ") %>% 
  separate(game_date, c("year", "month", "day"), sep = "-")

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




