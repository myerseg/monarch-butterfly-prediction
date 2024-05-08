library(shiny)

#define user interface 
ui <- fluidPage(
  titlePanel("Monarch Butterfly Counts Prediction"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("temp_input", "Temperature:", min = min(merged.cali$Mean.Temp), max = max(merged.cali$Mean.Temp), value = median(merged.cali$Mean.Temp))
    ), 
    mainPanel(
      plotOutput("prediction_plot")
    )
  )
)

#define server logic 
server <- function(input, output, session) {
  get_predicted_counts <- reactive ({
    new_data <- data.frame(Mean.Temp = input$temp_input)
    predicted_counts <- predict(calitempmodel, newdata = new_data, type = "response")
    return(predicted_counts)
  })
  
  #render the plot 
  output$prediction_plot <- renderPlot({
    predicted_counts <- get_predicted_counts()
    
    ggplot(data.frame(Mean.Temp = input$temp_input, Predicted_Count = predicted_counts), aes(x = Mean.Temp , y = Predicted_Count)) + 
      geom_point(color = "darkorange", size = 5, shape = 18) +
      geom_text(aes(label = round(Predicted_Count, 2)), vjust = -0.5, hjust = 0.5) +
      labs(x = "Temperature", y = "Predicted Monarch Butterfly Counts") +
      theme_classic(base_size = 18) 
  })
  
  observeEvent(input$temp_input, {
    output$prediction_plot <- renderPlot({
      predicted_counts <- get_predicted_counts()
      
      ggplot(data.frame(Mean.Temp = input$temp_input, Predicted_Count = predicted_counts), aes(x = Mean.Temp, y = Predicted_Count)) +
        geom_point(color = "darkorange", size = 5, shape = 18) +
        geom_text(aes(label = round(Predicted_Count, 1)), vjust = -0.5, hjust = 1) +
        coord_cartesian(ylim = c(154000, 290000), xlim = c(55, 62)) +
        labs( x = expression("Temperature (" * degree * "Fahrenheit)"), y = "Predicted Monarch Counts") +
        theme_classic(base_size = 18) 
    })
  })
}

#run the app
shinyApp(ui = ui, server = server)

