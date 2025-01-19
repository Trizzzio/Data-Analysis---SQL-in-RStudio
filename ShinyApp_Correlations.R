#####Creating Shiny App

###Query data

shiny_data <- dbGetQuery(conn,"
                         SELECT symbol, date, close
                         FROM indices_data
                         ORDER BY DATE")

##Fixing date, pivot to long and calculate correlations

shiny_data <- shiny_data %>%
  mutate(date = as.Date(date),
         close = as.numeric(close))%>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(return = (close / lag(close)) - 1) %>%
  na.omit() %>%
  select(symbol, date, return) %>%
  pivot_wider(names_from = symbol, values_from = return) %>%
  na.omit()


#Define rolling window size

rolling_window <- 30

#Calculate rolling correlations (using loop)

index_cols <- colnames(shiny_data)[-1]

rolling_correlations <-tibble()

for(i in 1:(length(index_cols)-1)){
  for(j in (i+1):length(index_cols)) {
    
    index_1 <- index_cols[i]
    index_2 <- index_cols[j]
 
  #Calculate rolling correlations
  roll_corr <- rollapplyr(data=cbind(shiny_data[[index_1]], shiny_data[[index_2]]),
                          rolling_window, function(x) cor(x[,1], x[,2], use = "complete.obs"),by.column = FALSE,fill=NA)
  
  rolling_correlations <- rbind(
    rolling_correlations,
    data.frame(
      date = shiny_data$date,
      pair = paste(index_1, index_2, sep = "_"),
      correlation = roll_corr
    )
  )
  

 }
}

#Change back to wide format

rolling_correlations<-rolling_correlations%>%na.omit()


######Actually creating Shiny app

library(shiny)

#Creating User Interfacte for app

ui <- fluidPage(
  titlePanel("Index Pair Correlation Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "pair",
        label = "Choose an Index Pair:",
        choices = unique(correlations$pair),
        selected = unique(correlations$pair)[1]
      ),
      dateRangeInput(
        inputId = "time_frame",
        label = "Select Time Frame:",
        start = min(correlations$date),
        end = max(correlations$date),
        format = "yyyy-mm-dd"
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "correlation_plot")
    )
  )
)


#Create server logic 

server <- function(input, output) {
  
  # Reactive data filtered based on user inputs
  filtered_data <- reactive({
    rolling_correlations %>%
      filter(pair == input$pair, 
             date >= input$time_frame[1], 
             date <= input$time_frame[2])
  })
  
  # Render the correlation plot
  output$correlation_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = date, y = correlation)) +
      geom_line(size = 1, color = "blue") +
      labs(
        title = paste("Correlation for", input$pair),
        x = "Date",
        y = "Correlation"
      ) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white", color = NA))
  })
}

#combine UI and Server

shinyApp(ui=ui, server=server)

