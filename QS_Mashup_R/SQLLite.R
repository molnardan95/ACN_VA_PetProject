#install.packages("RSQLite")
library(DBI)

# ESTABLISH CONNECTION WITH DB FILE
con <- dbConnect(RSQLite::SQLite(), "nba_player_data_R.db")

dbListTables(con)

con

# READ DATA FILES
player_data <- read.csv(file="../data/player_data.csv", header=TRUE, sep=",")
players <- read.csv("../data/Players.csv", header=TRUE, sep=",")
season_stats <- read.csv("../data/Seasons_Stats.csv", header=TRUE, sep=",")

# SAVE DATA FILES TO DB
dbWriteTable(con, "player_data", player_data, overwrite = TRUE)
dbWriteTable(con, "players", players, overwrite = TRUE)
dbWriteTable(con, "season_stats", season_stats, overwrite = TRUE)


# EXAMPLES FOR READING TABLE
dbReadTable(con, "players")
res <- dbSendQuery(con, "SELECT * FROM players WHERE height > 200")

dbFetch(res)
dbClearResult(res)

# CONNECTION SHOULD BE CLOSED IN THE END
dbDisconnect(con)

