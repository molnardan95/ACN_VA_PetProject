# In R, there are project and Script files. On the bottom right one can track folders and files
# The project I have created is called Practice.Rproj.
# Projects are useful for structure. 
# Also, it makes sense to store project related files in one directory. 


getwd()
setwd("example_data_structure_folder/")
# You don't necessarily need to change the working directory. 
# You can easily read files from anywhere you want by asigning the path to the function below
#filenames <-  list.files("example_data_structure_folder/")
filenames <-  list.files()


# Lists -------------------------------------------------------------------

filenames
typeof(filenames)
length(filenames)
filenames[1]



# For and IF statements ---------------------------------------------------

for (f in filenames) {
  print(f)
}

first_filename = filenames[1]
substr(first_filename, 1, 1) 

if (substr(first_filename, 1, 1) == "b") {
  print("test")
}

for (f in filenames) {
  print(f)
  if (substr(f, 1, 1) == "a") {
    print("This filename starts with a")
  }
  if (substr(f, 1, 1) == "b") {
    print("This filename starts with b")
  }
}


# Adding to list ----------------------------------------------------------

mylist = list()

a_data_list <- list()
b_data_list <- list()

i <- 1
for (f in filenames) {
  if (substr(f, 1, 1) == "a") {
    a_data_list <- c(a_data_list, f)
  }
  if (substr(f, 1, 1) == "b") {
    b_data_list <- c(b_data_list, f)
  }
}

typeof(a_data_list)
typeof(filenames)

print(a_data_list)


# Tables - data.table and dplyr -------------------------------------------

# R has two different methods for handling data.tables.
# In most cases data.table and dplyr are interchangeble. 
# I personally prefer dplyr, because of it's readability and the ability to chain commands, but you can use either.


# DATA.TABLE --------------------------------------------------------------


library(data.table)

first_a_data_filename <- a_data_list[[1]]
first_a_df <- fread(first_a_data_filename)
typeof(first_a_df)

first_a_df[[1]] # returns a vector
first_a_df[, 1]  # returns a data.table
first_a_df[, "aa"]

nrow(first_a_df)
ncol(first_a_df)


second_a_data_filename <- a_data_list[[2]]
second_a_df <- fread(second_a_data_filename)

concat_a_df <- rbind(first_a_df, second_a_df)

concat_a_df <- data.table()
for (a in a_data_list) {
  a_df = fread(a)
  concat_a_df <- rbind(concat_a_df, a_df)
}

concat_b_df <- data.table()
for (b in b_data_list) {
  b_df = fread(b)
  concat_b_df <- rbind(concat_b_df, b_df)
}

fwrite(concat_a_df, 'concat_a_df.csv')


# DPLYR -------------------------------------------------------------------

library(dplyr)

first_a_df <- read.csv(first_a_data_filename)

first_a_df[[1]] # returns a vector
first_a_df[, 1]  # returns a data.table
select(first_a_df, aa)

second_a_df <- read.csv(second_a_data_filename)

concat_a_df <- bind_rows(first_a_df, second_a_df)

concat_a_df <- data.table()
for (a in a_data_list) {
  a_df = read.csv(a)
  concat_a_df <- bind_rows(concat_a_df, a_df)
}

concat_b_df <- data.table()
for (b in b_data_list) {
  b_df = fread(b)
  concat_b_df <- rbind(concat_b_df, b_df)
}

write.csv(concat_a_df, "concat_a_df.csv")

# EXERCISE ----------------------------------------------------------------

# You can find a practice_data subfolder in this project. 
# Your task is to create 3 distinct tables, called trip, weather and station out of the csv files found there
# The output should be 3 unioned csv files called trip.csv, weather.csv, station.csv

