##Correlation Study
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(plotly)

##Query indices data


indices_data <- dbGetQuery(conn,"
   SELECT symbol, date, close
   FROM indices_data
   WHERE symbol IN ('^GDAXI', '^GSPC', '^FTSE', '^DJI')
   AND date BETWEEN '2020-01-01' AND '2023-12-31'
   ORDER BY date")


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

#Define rolling window size

rolling_window <- 30

#Calculate rolling correaltions

returns_wide <- returns_wide %>%
  mutate(
    corr_dax_sp500 = rollapplyr(data=returns_wide[, c("^GDAXI", "^GSPC")], rolling_window, function(x) cor(x[,1], x[,2], use = "complete.obs"),by.column = FALSE,fill=NA),
    corr_dax_dji = rollapplyr(data=returns_wide[, c("^GDAXI", "^DJI")], rolling_window, function(x) cor(x[,1], x[,2], use = "complete.obs"),by.column = FALSE,fill=NA)
  )


##Visualize

#Pivor long

correlations <- returns_wide %>%
  select(date, corr_dax_sp500, corr_dax_dji ) %>%
  pivot_longer(-date, names_to = "pair", values_to = "correlations")%>%
  filter(date>=as.Date("2020-02-14"))

#Plot

ggplot(correlations, aes(x = date, y = correlations, color = pair)) +
  geom_line(size = 1) +
  labs(
    title = "Rolling Correlations Between Index Pairs",
    subtitle = "30-Day Rolling Window",
    x = "Date",
    y = "Correlation",
    color = "Index Pair"
  ) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA)) +
  scale_color_manual(values = c("corr_dax_sp500" = "blue", 
                                "corr_dax_dji" = "red"))

#+ annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-04-30"),
#ymin = -1, ymax = 1, alpha = 0.2, fill = "gray")


#interactive version
ggplotly()
ggsave("rolling_correlations.png", width = 10, height = 6)


