##Correlation Study

library(dplyr)
library(tidyr)
library(lubridate)

##Query indices data


indices_data <- dbGetQuery(conn,"
   SELECT symbol, date, close
   FROM indices_data
   WHERE symbol IN ('^GDAXI', '^GSPC', '^FTSE', '^IXIC')
   ORDER BY DATE")


##Convert date, close and calcualte returns
indices_data <- indices_data %>%
  mutate(date = as.Date(date),
         close = as.numeric(close))%>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(return = (close / lag(close)) - 1) %>%
  na.omit()

#Pivor to wide
returns_wide <- indices_data %>%
  select(symbol, date, return) %>%
  pivot_wider(names_from = symbol, values_from = return) %>%
  na.omit()


#Calculating rolling correlations
