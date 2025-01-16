#loading packages

library(DBI)
library(RSQLite)

#Creating connection to a new SQLite database

db_path <- "financial_data.sqlite"
conn <- dbConnect(SQLite(), db_path)

print(conn)