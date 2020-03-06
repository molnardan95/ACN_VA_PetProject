library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)


df <- data.table(a = c(1,1,1,2,2,3,4,5),
                 b = c(10,10,11,20,20,30,40,50))
df

# Duplicates --------------------------------------------------------------

df <- unique(df)

# Missing values ----------------------------------------------------------

df <- data.table(a = c(1,2,3,4),
                 b = c(10, NA, 30, 40),
                 c = c(NA, NA, NA, 400))
is.na(df)

summary(df) # will count na rows as well

df_only_full_rows <- df[complete.cases(df), ] # data.table
df_only_full_rows <- tidyr::drop_na(df, names(df)) # dplyr

df_rows_where_b_is_not_missing <- df[is.na(b) == FALSE] # data.table
df_rows_where_b_is_not_missing <- df %>% filter(is.na(b) == FALSE) # dplyr

# Replace missing values

mean_b <- mean(df$b, na.rm=TRUE)

df_missing_b_replaced_with_mean <- df[is.na(b) == TRUE, b := mean_b] # DATA.TABLE
df_missing_b_replaced_with_mean <- df %>% 
  mutate(b = replace(b, is.na(b) == TRUE, mean_b)) # DPLYR

df_missing_b_replaced_with_mean_missing_c_replaced_with_zero <- df %>% 
  mutate(b = replace_na(b, mean_b),
         c = replace_na(c, 0)) # DPLYR and TIDYR - both packages are part of the tidyverse



# Numeric Data ------------------------------------------------------------

df <- data.table(a = c(1,1,1,2,2,3,4,5),
                 b = c(10,10,11,20,NA,NA,40,50),
                 c = c('apple','apple','plum','pear','plum','apple','apple','apple'))
str(df)

typeof(df$c)

max(df$a) # data.table/base R method
max(df$c)

df %>% 
  select(a) %>%
  max() # DPLYR method

unique(df) # returns unique rows
uniqueN(df) # return number of unique rows


# Dates -------------------------------------------------------------------

df <- data.table(a = c('2019-01-01 14:13', '2019-02-01 14:13'),
                 b = c(1546351980, 1549030380))

df <- df %>%
  mutate(a_asdate = ymd_hm(df$a),
         b_asdate = as_datetime(df$b)) 
# also part of the tidyverse, lubridate is a fantastic package for date manipulation

df <- df %>% 
  mutate(year = year(df$a_asdate),
         month = month(df$a_asdate),
         day = day(df$a_asdate),
         dayofweek = weekdays(df$a_asdate),
         is_leap_year = leap_year(df$a_asdate))
  


# Filtering --------------------------------------------------------------

df <- data.table(a = c(1,1,1,2,2,3,4,5),
                 b = c(10,10,11,20,0,0,40,50),
                 c = c('apple','apple','plum','pear','plum','apple','apple','apple'))

df[a < 3]
df %>% filter(a < 3)

df[c == "apple"]
df %>% filter(c == "apple")

df[b == 10 & c == "apple"]
df %>% filter(b == 10, c == "apple")

df[a<b]


# Grouping ----------------------------------------------------------------

getwd()
setwd("C:/Users/daniel.molnar/Documents/Projects/Mashup/Advanced_Analytics_R/Practice/")
# The above step is not really necessary. only if you changed the working directory before.

movies <- read.csv("data/movies.csv")
# read.csv gives us a data.frame and not a data.table. Thus, the data.table approaches won't work.
# If you want to use that, use the fread function.
# I will use dplyr from now on.

movies %>% 
  group_by(title) %>% 
  summarise(n = n())

### Again, this part highlights the difference between data.table and data.frame

#movies[, .N, by = title]
#test <- data.table(movies)
#test[, .N, by =title]
#str(movies)
#str(test)

###

movies %>% 
  group_by(title) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

movies %>% 
  group_by(title) %>% 
  summarise(rating_mean = mean(rating),
            nb_rating = n()) %>% 
  arrange(desc(rating_mean))


# Plotting ----------------------------------------------------------------

library(ggplot2)

ratings_by_genre <- movies %>% 
  group_by(genre) %>% 
  summarise(mean_rating = mean(rating))

ggplot(ratings_by_genre, aes(reorder(genre, mean_rating), mean_rating)) +
  geom_bar(stat='identity', fill = "steelblue") +
  ggtitle("Mean rating by genre")

by_year_genre <- movies %>% 
  group_by(year, genre) %>% 
  summarise(mean_rating = mean(rating))

ggplot(by_year_genre, aes(fill = genre, x = year, y = mean_rating)) +
  geom_bar(stat='identity', position="dodge") + #position="stack" fro stacked version
  ggtitle("Mean rating by genre")



by_year_genre2 <- movies %>% 
  group_by(year, genre) %>% 
  summarise(count = n_distinct(userId)) %>% 
  arrange(year)

ggplot(by_year_genre2, aes(fill = genre, x = year, y = count)) +
  geom_bar(stat='identity', position="dodge") + #position="stack" fro stacked version
  ggtitle("Mean rating by genre")


## In pithon, we created these pivoted dataframes. 
## In R, you don't necessarily need to do that to create these charts.
## Here is an example code for pivoting, regardless
pivoted_df <- by_year_genre %>% 
  spread(key = genre,
         value = mean_rating)

pivoted_df2 <- by_year_genre2 %>% 
  spread(key = genre,
         value = count)




# EXERCISE ----------------------------------------------------------------

# Your task is to analyze bike rental data.
# There os a trip_filled.csv file in the data folder.
# The task is to visualiza the difference in intra-day trends in the Customer and Subscriber groups.
# You should also do a weekend/weekday split.
# You can find some examples how the charts should look like - taken from Python, will look slighlt different in R
