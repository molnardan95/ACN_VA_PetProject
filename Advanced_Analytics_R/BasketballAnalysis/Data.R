# Install.packages("dplyr")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

# DATA
players <- read.csv("../../data/Players.csv", header=TRUE, sep=",")
season_stats <- read.csv("../../data/Seasons_Stats.csv", header=TRUE, sep=",")
player_data <- read.csv(file="../../data/player_data.csv", header=TRUE, sep=",")



# JOIN AND RENAME ---------------------------------------------------------


# CLEANING
season_stats$Player <- gsub("[*]", "", season_stats$Player) # Some players have * after their name
players$Player <- gsub("[*]", "", players$Player)
season_stats <- season_stats %>% filter(Player != "")


# CREATE ID

# FIRST AND LAST SEASON METHOD
## CREATE PLAYER_ID IN SEASON STATS

season_first_season <- season_stats %>% 
  mutate(player_born = Year - Age,
         name_born = paste(Player, player_born)) %>% 
  arrange(name_born, Year) %>% 
  group_by(name_born) %>% 
  mutate(first_season = first(Year)) %>% 
  ungroup() %>% 
  mutate(player_first_season = paste(Player, first_season))

player_ids <- season_first_season %>% 
  select(Player, first_season, player_first_season) %>% 
  unique() %>% 
  group_by(Player) %>% 
  mutate(id = 1:n()) %>% 
  ungroup() %>% 
  mutate(player_id = paste(Player, id)) %>% 
  select(player_first_season, player_id)
  
season_stats <- season_first_season %>% 
  select(-c(player_born, name_born, first_season, X)) %>% 
  left_join(player_ids, by = c("player_first_season" = "player_first_season"))


rm(season_first_season, player_ids)


## CREATE PLAYER_ID IN PLAYER_DATA

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

player_data_tmp <- player_data %>% 
  mutate(tmp_player_born = as.character(birth_date),
         player_born = substrRight(tmp_player_born, 4),
         name_born = paste(name, player_born)) %>% 
  arrange(name_born) %>% 
  group_by(name_born) %>% 
  mutate(first_season = first(year_start),
         player_first_season = paste(name, first_season)) %>% 
  ungroup()

player_ids <- player_data_tmp %>% 
  select(name, first_season, player_first_season) %>% 
  unique() %>% 
  group_by(name) %>% 
  mutate(id = 1:n()) %>% 
  ungroup() %>% 
  mutate(player_id = paste(name, id)) %>% 
  select(player_first_season, player_id)

player_data <- player_data_tmp %>% 
  select(-c(tmp_player_born, player_born, name_born, first_season)) %>% 
  left_join(player_ids, by = c("player_first_season" = "player_first_season"))

rm(player_data_tmp, player_ids)


data <- season_stats %>% 
  left_join(player_data, by = c("player_id" = "player_id"))

rm(players, season_stats, player_data)


data <- data %>% 
  rename(
    year = Year,
    player = Player,
    position_in_team = Pos,
    age = Age,
    team = Tm,
    games_played = G,
    games_started = GS,
    mins_played = MP,
    player_efficience_rating = PER,
    perc_true_shooting = TS.,
    rate_3_point = X3PAr,
    rate_free_throw = FTr,
    perc_off_rebound = ORB.,
    perc_def_rebound = DRB.,
    perc_tot_rebound = TRB.,
    perc_assist = AST.,
    perc_steal = STL.,
    perc_block = BLK.,
    perc_trunover = TOV.,
    perc_usage = USG.,
    win_share_off = OWS,
    win_share_def = DWS,
    win_share = WS,
    win_share_per_48 = WS.48,
    plus_minus_box_off = OBPM,
    plus_minus_box_def = DBPM,
    plus_minus_box = BPM,
    value_over_replacement = VORP,
    field_goals = FG,
    field_goal_attempts = FGA,
    perc_field_goal = FG.,
    field_goal_3_point = X3P,
    field_goal_3_point_attempts = X3PA,
    perc_field_goal_3_point = X3P.,
    field_goal_2_point = X2P,
    field_goal_2_point_attempts = X2PA,
    perc_field_goal_2_point = X2P.,
    perc_field_goal_effective = eFG.,
    free_throws = FT,
    free_throw_attempts = FTA,
    perc_free_throw = FT.,
    rebounds_off = ORB,
    rebounds_def = DRB,
    rebounds = TRB,
    assists = AST,
    steals = STL,
    blocks = BLK,
    turnovers = TOV,
    fouls = PF,
    points = PTS
  )

data <- data %>% select(-c(player_first_season.x, player_first_season.y, name))

write.csv(data, '../../data/data_work.csv')

# SEASON DATA -------------------------------------------------------------
## Season data contains for each and every players every season spent in the NBA
## If a player played in more then one team in a season, the total should be used with additional column indicating transfer

data_season <- data %>% 
  mutate(per_game_3_point = field_goal_3_point/games_played,
         per_game_2_point = field_goal_2_point/games_played,
         per_game_points = points/games_played,
         per_game_rebounds = rebounds/games_played,
         per_game_assists = assists/games_played,
         per_game_blocks = blocks/games_played,
         per_game_steals = steals/games_played)

test <- data_season %>% 
  group_by(year, player) %>% 
  mutate(player_teams = n()) %>% 
  filter(player_teams > 1) %>% 
  select(year, player, position, age, team, games_played, player_teams)


# PLAYER DATA -------------------------------------------------------------


# TEAM DATA ---------------------------------------------------------------


