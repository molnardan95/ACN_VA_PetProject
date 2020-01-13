# PACKAGES
#install.packages("tidyverse")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

# DATA
#player_data <- read.csv(file="../../data/player_data.csv", header=TRUE, sep=",")
data <- read.csv("../../data/data_work.csv", header=TRUE, sep=",")

data_plyr_sn <- data %>% 
  mutate(per_game_3_point = field_goal_3_point/games_played,
         per_game_2_point = field_goal_2_point/games_played,
         per_game_points = points/games_played,
         per_game_rebounds = rebounds/games_played,
         per_game_assists = assists/games_played,
         per_game_blocks = blocks/games_played,
         per_game_steals = steals/games_played)



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


