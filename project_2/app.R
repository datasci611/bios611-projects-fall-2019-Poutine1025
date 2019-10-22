library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
source("helper_functions.R")

UMD=read_tsv(url("https://raw.githubusercontent.com/biodatascience/datasci611/gh-pages/data/project1_2019/UMD_Services_Provided_20190719.tsv"))
UMD_selected=clean_data(UMD)
UMD_selected2=clean_data2(UMD_selected)
UMD_selected3=clean_data3(UMD_selected2)

ui <- navbarPage("Project 2: UMD",
                 tabPanel("Background",
                   fluidPage(
                     titlePanel("Background"),
                     sidebarLayout(
                       sidebarPanel("Introduction to this project"),
                       mainPanel(
                         textOutput(outputId = "text1"),
                         textOutput(outputId = "text2"),
                         textOutput(outputId = "text3"),
                         textOutput(outputId = "text4"),
                         textOutput(outputId = "text5")
                       )
                     )
                   )
                 ),
                 
                 tabPanel("Relationship among Variables",
                   fluidPage(
                     titlePanel("Find the relationship among variables"),
                     sidebarLayout(
                       sidebarPanel(
                         radioButtons(inputId = "Xvariable", "Select a variable for X-axis:",
                                      c("Food Pounds" = "Food Pounds",
                                        "Number of people per family" =  "Number of people per family",
                                        "Clothing Items" = "Clothing Items")
                                      ),
                         
                         radioButtons(inputId = "Yvariable", "Select a variable for Y-axis:",
                                      c("Food Pounds" = "Food Pounds",
                                        "Number of people per family" =  "Number of people per family",
                                        "Clothing Items" = "Clothing Items")
                         ),
                         
                         textOutput(outputId = "description")
                       ),
                       mainPanel(
                         plotOutput(outputId = "Rvariable")
                       )
                     )
                   )
                 ),
                 
                 tabPanel("Time influence",
                   fluidPage(
                     titlePanel("Find time influence on food and clothing items"),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "Year", "Range of year:",
                                      min = 2005, max = 2018, value = c(2005,2018)),
                          textOutput(outputId = "yearrange"),
                          
                          checkboxGroupInput(inputId = "Month",  "Select month:",
                                             c("Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4,
                                               "May" = 5, "Jun" = 6, "Jul" = 7, "Aug" = 8,
                                               "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12),
                                             )
                        ),
                        mainPanel(
                          plotOutput(outputId = "Time")
                        )
                      )
                    )
                 ),
                 tabPanel("Food amount prediction",
                   fluidPage(
                     titlePanel("Predict the food amount for a given future year"),
                     sidebarLayout(
                       sidebarPanel(
                         numericInput(inputId = "Fyear", "Input the year you want to predict:",
                                      2019, min = 2019, max = 2099, step = 1),
                         textOutput(outputId = "Fyear")
                       ),
                       mainPanel(
                         plotOutput(outputId = "lm"),
                         textOutput(outputId = "explaination")
                       )
                     )
                   )
                 )
)

server <- function(input, output){
  output$text1 <- renderText("The programs of Urban Ministries of Durham (UMD) end homelessness by providing 
                             neighbors with emergency shelter and case management to help them overcome barriers 
                             such as unemployment, medical and mental health problems, past criminal convictions 
                             and addiction. The data is provided by UMD, including 79838 observations with 18 
                             variables such as Date, Client File Number, Food Pounds and Clothing Items.")
  output$text2 <- renderText("The questions I want to answer are")
  output$text3 <- renderText("1. What is the relationship among Food Pounds, Clothing Items and Number of people in 
                             the family for which food was provided?")
  output$text4 <- renderText("2. How does time influence the amount of food and clothing items?")
  output$text5 <- renderText("3. What is the amount of food need to be provided in a given future year?")
  
  output$description <- renderText(paste("You have selected ", input$Xvariable, " as X-axis and ",
                                         input$Yvariable, "as Y-axis."))
  output$Rvariable <- renderPlot({
    Vrelation(UMD_selected, Xvariable = input$Xvariable, Yvariable = input$Yvariable)
  })
  
  output$yearrange <- renderText(paste("You have selected data from  ", input$Year[1], " to ", input$Year[2], "."))
  output$Time <- renderPlot({
    Timeplot(UMD_selected2, input$Year, input$Month)
  })
  
  output$Fyear <- renderText(paste("You have  input year", input$Fyear, "."))
  output$lm <- renderPlot({
    linearmodel(UMD_selected3, input$Fyear)
  })
  output$explaination <- renderText({
    model_FoodvsYear=lm(`Food Pounds`~Year, UMD_selected3)
    Food=predict(model_FoodvsYear, newdata = data.frame(Year=input$Fyear))
    paste("The predicted value of Food Pounds of ", input$Fyear, " is", Food, ".")
  })
}

shinyApp(ui = ui, server = server)
