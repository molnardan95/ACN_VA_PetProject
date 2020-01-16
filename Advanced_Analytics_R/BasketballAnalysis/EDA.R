# PACKAGES
#install.packages("tidyverse")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

# DATA
#player_data <- read.csv(file="../../data/player_data.csv", header=TRUE, sep=",")
data <- read.csv("../../data/data_work.csv", header=TRUE, sep=",")

data <- data %>% 
  mutate(flag_1980_after = ifelse(year >= 1980, 'Y', 'N'))

data_per_game <- data %>% 
  mutate(pg_mins_played = mins_played/games_played,
         pg_field_goals = field_goals/games_played,
         pg_field_goal_attempts = field_goal_attempts/games_played,
         pg_3_point = field_goal_3_point/games_played,
         pg_3_point_attempts = field_goal_3_point_attempts/games_played,
         pg_2_point = field_goal_2_point/games_played,
         pg_2_point_attempts = field_goal_2_point_attempts/games_played,
         pg_free_throws = free_throws/games_played,
         pg_free_throw_attempts = free_throw_attempts/games_played,
         pg_rebounds_off = rebounds_off/games_played,
         pg_rebounds_def = rebounds_def/games_played,
         pg_rebounds = rebounds/games_played,
         pg_assists = assists/games_played,
         pg_steals = steals/games_played,
         pg_blocks = blocks/games_played,
         pg_turnovers = turnovers/games_played,
         pg_fouls = fouls/games_played,
         pg_points = points/games_played)





### EXPLORATORY DATA ANALYSIS

## 1. ARE THERE ANY FEATURES MISSING FOR CERTAIN YEARS

columns <- colnames(data) # Need columns to loop trough
df_stat_calc_from <- tibble() # Initiate ouput df

for (colname in columns) {
  non_na_per_column <- data %>% 
    select(year, colname) %>% 
    na.omit() # Select data fro column where it's not NA
  
  first_calculated_year <- min(non_na_per_column$year) # Store min year in a variable
  
  df_stat_calc_from_col <- tibble(first_calculated_year, colname) # Store it in a df
  
  df_stat_calc_from <- bind_rows(df_stat_calc_from, df_stat_calc_from_col) # Output df 
}

data1980 <- data %>% 
  filter(year >= 1980)

# TURNS OUT WE HAVE ALL STATS FROM 1980

## 2. MAIN STATS BY AGE

# AGE DISTRIBUTION
# METHOD 1 - geom_histogram

length(unique(data$age))

data_plyr_sn %>% 
  ggplot(aes(age))+
  geom_histogram(bins = 28)

# METHOD 2 - geom_bar()

data_plyr_sn %>% 
  ggplot(aes(age))+
  geom_bar() +
  geom_text()


# STATS

data_plyr_sn %>% 
  ggplot(aes(age, per_game_points)) +
  geom_point() +
  geom_smooth() # "method = lm" could be used for linear model


# 
data %>% 
  filter(team != "TOT" & flag_1980_after == "Y") %>% 
  ggplot(aes(age, points, color = position_in_team)) +
  geom_point() 


data %>% 
  filter(team != "TOT" & flag_1980_after == "Y") %>% 
  ggplot(aes(year, points, color = position_in_team)) +
  geom_point() 

data_per_game %>% 
  filter(team != "TOT" & flag_1980_after == "Y") %>% 
  ggplot(aes(age, pg_points)) +
  geom_point() +
  geom_smooth()


cor(data$age, data$points, method = "pearson")

cor.test(data$age, data$points, method = c("pearson", "kendall", "spearman"))
