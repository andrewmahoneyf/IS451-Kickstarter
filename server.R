
source("processdata.R")
# Loads Libraries 
library(scales)
library(dplyr)
library(plotly)
library(rsconnect)
library(ggplot2)
library(forcats)

red.text <- element_text(size=16, face = "bold.italic", color = "#f93e3e")

GetPredictorSelected <- function(input){
  if(input$predictor == "Category") {
    predictor_selected <- category
  }else if(input$predictor == "Country"){
    predictor_selected <- country
  }else if(input$predictor == "Total Days"){
    predictor_selected <- total_days
  }else if(input$predictor == "Goal (USD)"){
    predictor_selected <- goal
  }
  return(predictor_selected)
}

shinyServer(function(input, output) {
  output$predictorPlot <- renderPlot({
    
    predictor_selected <- GetPredictorSelected(input)
  
    country_plot2 <- ggplot(data = kickstarter.successful.df, aes(predictor_selected), position = position_stack(reverse = TRUE)) +
      geom_bar() +
      coord_flip() +
      labs(title = "Top Countries")
  
  })
  
  output$data <- renderDataTable({
    kickstarter.df
  }, options = list(pageLength = 50)) 

  output$tree <- renderText({
    tree
  })
  
})