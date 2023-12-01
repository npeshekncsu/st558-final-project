library(shiny)
library(caret)
library(shiny)
library(DT)

#data("GermanCredit")


#tabPanel(“Tab-1”, fluidRow(h1(“Welcome to the home page of SimpleApp”)

predictors  <- c("ph", "Hardness", "Solids", "Chloramines", "Sulfate",
                 "Conductivity", "Organic_carbon", "Trihalomethanes", 
                 "Turbidity")
shinyUI(
    
    navbarPage(title='Final Project App',
               
               
               tabPanel('About', #, 
                        #br(),
                        #div(),
                        #img(src='final_project_logo.jpg', 
                        #    align = "center", 
                        #    width="20%", 
                        #    height ="85px"),
                        fluidRow(
                            column(8, offset = 0.5, align = "left",  # Adjust the column width and alignment as needed
                                   # Additional content on the right (e.g., image and text)
                                   img(src = "final_project_logo.jpg", width = "20%"),
                                   h2("ST 558 Final Project Application"),
                                   
                                   p('This part of the homepage contains data in text format. This text data is totally random.'), 
                                   
                                   h3("The purpose of the app"),
                                   br(),
                                   
                                   h3("Data"),
                                   br()
                                   
                            ),
                            #box(title='SimpleApp left box', width= 6,
                            
                        ) ),
                        #fluidRow(h4('ST 558 final project application', align = 'center')) ),
               
               
               
               tabPanel('Data Exploration', 
                        h2("Exploratory Data Analysis"),
                        
                        br(),
                        
                        
                        sidebarLayout(
                            sidebarPanel(
                                #h3("Exploratory plots"),
                                h3("Distribution plots"),
                                br(),
                                selectInput("variables_to_summarize", "Select variable",
                                            choices = c("ph", "Hardness", "Solids", 
                                                        "Chloramines", "Sulfate", "Conductivity", 
                                                        "Organic_carbon", "Trihalomethanes", "Turbidity"),
                                            selected = "Chloramines" ),
                                
                                selectInput("graph_type", "Select graph type",
                                        choices = c("Violin", "Density", "Boxplot"),
                                        selected = "Density" ),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                
                                h3("Scatter plots"),
                                selectInput("x_var", "x",
                                            choices = c("ph", "Hardness", "Solids", 
                                                        "Chloramines", "Sulfate", "Conductivity", 
                                                        "Organic_carbon", "Trihalomethanes", "Turbidity"),
                                            selected = "Hardness" ),
                                
                                selectInput("y_var", "y",
                                            choices = c("ph", "Hardness", "Solids", 
                                                        "Chloramines", "Sulfate", "Conductivity", 
                                                        "Organic_carbon", "Trihalomethanes", "Turbidity"),
                                            selected = "Solids" ),
                                
                                ),
                            
                                
                                
                                            #numericInput("numeric_value", 
                                            #             "Select the number of digits for rounding", 
                                            #             value = 2, min = 0, step = 1)),
                            mainPanel(
                                h4("Distribution plot", align = 'center'),
                                plotOutput("summary_plot"),
                                br(),
                                h4("Scatter plot", align = 'center'),
                                plotOutput("scatter_plot"),
                                br(),
                                h4("Correlation matrix", align = 'center'),
                                radioButtons("corr_plot_type", "Select the Plot Type",
                                                                   choices = c("Number", 
                                                                               "Color", 
                                                                               "Elipse"),
                                                                   selected = "Color"),
                                plotOutput("corr_plot"),
                                br(),
                                h4("Histograms", align = 'center'),
                                selectInput("histogram_var", "Select variable",
                                            choices = c("Solids", 
                                                        "Sulfate", "Chloramines"),
                                            selected = "Solids"),
                                plotOutput("quantile_plot")
                            )
                            )
                        ),
               tabPanel('Modeling', 
                        tabsetPanel(
                            tabPanel("Model Info",
                                     h3('Text')), 
                            tabPanel("Model Fitting",
                                     br(),
                                     sliderInput("data_split", "Data split",
                                                 min = 10, max = 100, value = 70, step = 1),
                                     checkboxGroupInput(
                                         "glm_predictor_selector",
                                         h4("Select Generalized linear regression model predictors"),
                                         choices = predictors,
                                         inline = TRUE,
                                         selected = predictors,
                                         #width = '350px'
                                     ),
                                     br(),
                                     checkboxGroupInput(
                                         "rf_predictor_selector",
                                         h4("Select Random Forest model predictors"),
                                         choices = predictors,
                                         inline = TRUE,
                                         selected = predictors,
                                         #width = '350px'
                                     ),
                                     actionButton('train', 'Train models', class = "btn-primary btn-lg"),
                                  
                                     textOutput('glm_summary'),
                                     textOutput('rf_summary')
                                     #checkboxInput("change_symbol_checkbox", h5("test"))
                                     #conditionalPanel(
                                     #  #condition = "input.train==true",
                                     #  condition = "input.train != 0",    
                                     #  textOutput('glm_summary'),
                                     #  textOutput('rf_summary')
                                       
                                     #  checkboxInput("change_symbol_checkbox", h5("Also change symbol based on REM sleep?"))
                                     #  #textOutput('glm_summary')
                                     #)
                            ),
                                
                            
                             
                            
                            
                            tabPanel("Prediction",
                                     h3("GLM predict"),
                                     br(),
                                     uiOutput("glm_predictors"),
                                     h3('RF predict'),
                                     ),
                           )
                        
                        
                        ))
    
    
    #fluidPage(
    # titlePanel("Summaries for German Credit Data"),
    # 
    # sidebarLayout(
    #     sidebarPanel(
    #         h4("This data set comes from the", 
    #            HTML("<a href='https://topepo.github.io/caret'>caret package</a>  - originally from the UCI machine learning repository")),
    #         br(),
    #         h6("You can create a few bar plots using the radio buttons below."),
    #         radioButtons("radio_choice", "Select the Plot Type",
    #                      choices = c("Just Classification", 
    #                                  "Classification and Unemployed", 
    #                                  "Classification and Foreign"),
    #                      selected = "Just Classification"),
    #         br(),
    #         h5("You can find the ", HTML("<b>sample mean</b>"),  " for a few variables below: "),
    #         selectInput("variables_to_summarize", "Variables to Summarize",
    #                     choices = c("Age", "Duration", "Amount"),
    #                     selected = "Age"),
    #         numericInput("numeric_value", "Select the number of digits for rounding", value = 2, min = 0, step = 1)
    #         
    #     ),
    #     mainPanel(
    #         plotOutput("summary_plot"),
    #         DTOutput("summary_table")
    #     ) )
#)

)