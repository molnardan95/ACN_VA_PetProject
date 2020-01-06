# PACKAGES
#install.packages("tidyverse")
library(dplyr)
library(tidyverse)
library(ggplot2)

# DATA
#player_data <- read.csv(file="../../data/player_data.csv", header=TRUE, sep=",")
players <- read.csv("../../data/Players.csv", header=TRUE, sep=",")
season_stats <- read.csv("../../data/Seasons_Stats.csv", header=TRUE, sep=",")

# CLEANING
season_stats$Player <- gsub("[*]", "", season_stats$Player) # Some players have * after their name

data <- season_stats %>% 
  left_join(players)


#EDA

data %>% 
  filter(FT > 10) %>% 
  ggplot(aes(FT.)) +
  geom_histogram(binwidth = 0.05)
