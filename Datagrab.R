###Getting yahoo finace data through tidyquant

library(tidyquant)

indices<- c("^GDAXI", "^FTSE", "^FCHI", "^IBEX", "^STOXX50E",  
            "^GSPC", "^DJI", "^IXIC", "^RUT", "^VIX") 


index_data <- tq_get(indices, from = "2020-01-01", to = "2023-12-31")

#Store data in SQLite database

dbWriteTable(conn,"indices_data",index_data,overwrite=TRUE)

dbListTables(conn)