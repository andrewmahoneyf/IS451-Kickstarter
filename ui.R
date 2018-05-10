#Require shiny

library(shiny)
library(shinythemes)

#Define the functionality of the user interface
shinyUI(fluidPage(
  theme = shinytheme("darkly"),
  
  # Add a descriptive application title
  titlePanel("Kickstarter Analysis"),
  
  # Add the app interactivity
  sidebarLayout(
    sidebarPanel(
      tags$style(
        type = "text/css",
        ".shiny-output-error { visibility: hidden; }",
        ".shiny-output-error:before { visibility: hidden; }"
      ),
      selectInput(
        inputId = "predictor",
        label = "Predictor:",
        choices = c("Category", "Country", "Total Days", "Goal (USD)"),
        selected = "Category"
      )
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Summary",
          h3(
            "Google Search Activity vs. Cryptocurrency Prices: Is there a pattern here?"
          ),
          p(
            "Our group wanted to look at the relation of Google keyword search and historical cryptocurrency market prices
            had on each other. We pulled data resources from various users and locations, cleaned that dataset to fit our
            study, then hosted our visualizations as shiny app. To answer the question of whether or not higher query
            hits of cryptocurrency keywords relates to a higher crypto- market value, we built a visualization containing
            two types of graphs and a filtering section to match the focus of our study and our users."
          ),
          
          h3("Data Sources"),
          p(
            "We used data from GitHub user JesseVent's \"crypto\" repository, which containeds historical cryptocurrency
            prices and market data for all different cryptocurrency tokens. The dataset includes 1515 different coins,
            13 variables, and over 700,000 observations with dates from 2013 until now. We downloaded the crypto history
            data through R using a custom function provided by the development library from the developer. Additionally,
            we used Google Trends data related to our selected cryptocurrencies. We then pulled all of the data ranging back
            to the start of 2017, as cryptocurrency activity has been more prominent (and arguably more interesting) recently
            than ever before, due to rising popularity and a heightened sense of legitimacy from the public."
          ),
          
          h3("Where we got our data"),
          a(href = "https://www.kaggle.com/jessevent/all-crypto-currencies", "Hystorical Cryptocurrency Dataset - Kaggle"),
          
          h3("GitHub Repo Link to Final Project"),
          a(href = "https://github.com/andrewmahoneyf/Info-201-Final", "BA - PAJJ / Final Project")
          ),
        tabPanel("Data", dataTableOutput("data")),
        tabPanel("Predictor Plots",
                 fluidRow(
                   verbatimTextOutput("info"),
                   plotOutput("predictorPlot",  click = "plot_click")
                 )),
        tabPanel("Tree", dataTableOutput("tree")),
        tabPanel("Linear Regression", dataTableOutput("linear"))
          )
        )
      )
  )
)
  