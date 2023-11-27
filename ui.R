library(shiny)
library(caret)
library(shiny)
library(DT)

data("GermanCredit")

shinyUI(fluidPage(
    titlePanel("Summaries for German Credit Data"),
    
    sidebarLayout(
        sidebarPanel(
            h4("This data set comes from the", 
               HTML("<a href='https://topepo.github.io/caret'>caret package</a>  - originally from the UCI machine learning repository")),
            br(),
            h6("You can create a few bar plots using the radio buttons below."),
            radioButtons("radio_choice", "Select the Plot Type",
                         choices = c("Just Classification", 
                                     "Classification and Unemployed", 
                                     "Classification and Foreign"),
                         selected = "Just Classification"),
            br(),
            h5("You can find the ", HTML("<b>sample mean</b>"),  " for a few variables below: "),
            selectInput("variables_to_summarize", "Variables to Summarize",
                        choices = c("Age", "Duration", "Amount"),
                        selected = "Age"),
            numericInput("numeric_value", "Select the number of digits for rounding", value = 2, min = 0, step = 1)
            
        ),
        mainPanel(
            plotOutput("summary_plot"),
            DTOutput("summary_table")
        ) )
))