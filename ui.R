library(shiny)
library(caret)
library(shiny)
library(DT)




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
                                   #h2("About this project"),
                                  
                                   
                                   h3("Purpose of the application"),
                                   p('This application allows to predict water potability based on water quality metrics.'),
                                   h3("Data"),
                                   p("Dataset to train models is taken from ",  a(href="https://www.kaggle.com/datasets/devanshibavaria/water-potability-dataset-with-10-parameteres",
                                                                "Kaggle"), '.'),
                                   h4("Features in dataset"),
                                   h5('pH value'),
                                   p('PH is an important parameter in evaluating the acid–base balance of water. It is also the indicator of acidic or alkaline condition of water status. WHO has recommended maximum permissible limit of pH from 6.5 to 8.5. The current investigation ranges were 6.52–6.83 which are in the range of WHO standards.'),
                                   h5('Hardness'),
                                   p('Hardness is mainly caused by calcium and magnesium salts. These salts are dissolved from geologic deposits through which water travels. The length of time water is in contact with hardness producing material helps determine how much hardness there is in raw water. Hardness was originally defined as the capacity of water to precipitate soap caused by Calcium and Magnesium.'),
                                   h5("Solids"),
                                   p('Water has the ability to dissolve a wide range of inorganic and some organic minerals or salts such as potassium, calcium, sodium, bicarbonates, chlorides, magnesium, sulfates etc. These minerals produced un-wanted taste and diluted color in appearance of water. This is the important parameter for the use of water. The water with high TDS value indicates that water is highly mineralized. Desirable limit for TDS is 500 mg/l and maximum limit is 1000 mg/l which prescribed for drinking purpose.'),
                                   h5('Chloramines'),
                                   p('Chlorine and chloramine are the major disinfectants used in public water systems. Chloramines are most commonly formed when ammonia is added to chlorine to treat drinking water. Chlorine levels up to 4 milligrams per liter (mg/L or 4 parts per million (ppm)) are considered safe in drinking water.'),
                                   h5('Sulfate'),
                                   p('Sulfates are naturally occurring substances that are found in minerals, soil, and rocks. They are present in ambient air, groundwater, plants, and food. The principal commercial use of sulfate is in the chemical industry. Sulfate concentration in seawater is about 2,700 milligrams per liter (mg/L). It ranges from 3 to 30 mg/L in most freshwater supplies, although much higher concentrations (1000 mg/L) are found in some geographic locations.'),
                                   h5('Conductivity'),
                                   p('Pure water is not a good conductor of electric current rather’s a good insulator. Increase in ions concentration enhances the electrical conductivity of water. Generally, the amount of dissolved solids in water determines the electrical conductivity. Electrical conductivity (EC) actually measures the ionic process of a solution that enables it to transmit current. According to WHO standards, EC value should not exceeded 400 μS/cm.'),
                                   h5('Organic_carbon'),
                                   p('Total Organic Carbon (TOC) in source waters comes from decaying natural organic matter (NOM) as well as synthetic sources. TOC is a measure of the total amount of carbon in organic compounds in pure water. According to US EPA < 2 mg/L as TOC in treated / drinking water, and < 4 mg/Lit in source water which is use for treatment.'),
                                   h5('Trihalomethanes'),
                                   p('THMs are chemicals which may be found in water treated with chlorine. The concentration of THMs in drinking water varies according to the level of organic material in the water, the amount of chlorine required to treat the water, and the temperature of the water that is being treated. THM levels up to 80 ppm is considered safe in drinking water.'),
                                   h5('Turbidity'),
                                   p('The turbidity of water depends on the quantity of solid matter present in the suspended state. It is a measure of light emitting properties of water and the test is used to indicate the quality of waste discharge with respect to colloidal matter. The mean turbidity value obtained for Wondo Genet Campus (0.98 NTU) is lower than the WHO recommended value of 5.00 NTU.'),
                                   h5('Potability'),
                                   p('Indicates if water is safe for human consumption where 1 means Potable and 0 means Not potable.'),
                                   h3("Application structure"),
                                   h5('About tab'),
                                   p('Provides description of the application purpose and dataset used to build models.'),
                                   h5('Data exploration tab'),
                                   p('The purpose of this tab is to conduct exploratory data analyses on the water potability dataset.'),
                                   h5('Modeling/Model Info tab'),
                                   p('This page contains description of the models which this app allows to build.'),
                                   h5('Modeling/Model fitting')
                                   
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
                                     h3('Logistic Regression model'),
                                     br(),
                                     p(withMathJax ('Logistic regression is used for binary classification problems where the outcome variable is categorical and has two classes. Logistic regression models the probability that a given instance belongs to a particular category. It uses the logistic function (sigmoid function) to map predicted values to probabilities between 0 and 1.  The logistic regression model is represented as: $$P(Y=1) = \\frac{1}{1+e^{-(\\beta_0 + \\beta_1 X_{1} + \\beta_2 X_2 + ... + \\beta_n X_n)}}$$  The model is trained using the maximum likelihood estimation method, minimizing the logistic loss function.')),
                                     h4('Advantages of logistic regression model'),
                                     p('Logistic regression is simple and interpretable. It works well when  the relationship between features and target is approximately linear.'),
                                     h4('Weaknesses of logistic regression model'),
                                     p('Logistic regression assumes a linear relationship between the features and the log-odds of the response variable. Logistic regression assumes that errors are independent of each other. ogistic regression does not provide direct measures of feature importance.'),
                                     br(),
                                     h3('Random Forest model'),
                                     br(),
                                     p('Random Forest is a versatile algorithm that can be used for both classification and regression tasks.'),
                                     p('Random Forest builds multiple decision trees during training and merges them together to get a more accurate and stable prediction. Each tree is trained on a random subset of the data and a random subset of features, which introduces diversity and reduces overfitting.'),
                                     p('A Random Forest is an ensemble of decision trees. The final prediction is made by averaging (for regression) or voting (for classification) the predictions of individual trees.'),
                                     p('Each tree in the forest is trained independently on a random subset of the training data, using a random subset of features at each split. The randomness helps in reducing overfitting and improving generalization.'),
                                     h4('Advantages of random forest model'),
                                     p('Random Forest model handles non-linearity and complex relationships well. It is robust to outliers and noise. Random Forest provides feature importance information.'),
                                     h4('Weaknesses of random forest model'),
                                     p('Random Forests can be computationally expensive, especially with a large number of trees and deep trees. Random Forest models can be challenging to interpret, especially when dealing with a large number of trees.')
                                   
                                     
                                     ), 
                            tabPanel("Model Fitting",
                                     br(),
                                     h2("Train/validation data split"),
                                     sliderInput("data_split", "Data split, %",
                                                 min = 10, max = 100, value = 70, step = 1),
                                     br(),
                                     h2("Model settings"),
                                     checkboxGroupInput(
                                         "glm_predictor_selector",
                                         h4("Generalized linear regression model predictors"),
                                         choices = predictors,
                                         inline = TRUE,
                                         selected = predictors,
                                         #width = '350px'
                                     ),
                                     br(),
                                     h4('Cross validation'),
                                     textInput('folds_glm', 'Number of folds', value = '5'),
                                     br(),
                                     br(),
                                     checkboxGroupInput(
                                         "rf_predictor_selector",
                                         h4("Random Forest model predictors"),
                                         choices = predictors,
                                         inline = TRUE,
                                         selected = predictors,
                                         #width = '350px'
                                     ),
                                     br(),
                                     h4('Tuning grid for number of variables to randomly sample as candidates at each split'),
                                     textInput('minMtry', 'Min value for MTRY hyperparameter', value = '1'),
                                     textInput('maxMtry', 'Max value for MTRY hyperparameter', value = '5'),
                                     br(),
                                     h4('Cross validation'),
                                     textInput('folds_rf', 'Number of folds', value = '5'),
                                     actionButton('train', 'Train models', class = "btn-primary btn-lg"),
                                     br(),
                                     br(),
                                     
                                     conditionalPanel(condition = "input.train",
                                                      h3('Performance of the fitted models'),
                                                      br(),
                                                      h4('On training dataset'),
                                                      textOutput('glm_summary'),
                                                      textOutput('rf_summary'),
                                                      br(),
                                                      h5('Random Forest model plot'),
                                                      plotOutput('rf_model_plot'),
                                                      br(),
                                                      h5('Random Forest variable importance'),
                                                      dataTableOutput('var_imprt_rf'),
                                                      h4('On validation dataset'),
                                                      textOutput('glm_accuracy_val_glm'),
                                                      textOutput('glm_accuracy_val_rf'),
                                                      br(),
                                                      h4("GLM confusion matrix"),
                                                      verbatimTextOutput("cnf_matrix_glm"),
                                                      br(),
                                                      h4("Random Forest confusion matrix"),
                                                      verbatimTextOutput("cnf_matrix_rf"),
                                                      #br()
                                                      )
                                                      #verbatimTextOutput('var_imprt_rf')),
                                                      
                                     #h3('Performance of the fitted models'),
                                  
                                     #textOutput('glm_summary'),
                                     #textOutput('rf_summary'),
                                     #br(),
                                     #h4('On validation dataset'),
                                     #textOutput('glm_accuracy_val_glm'),
                                     #textOutput('glm_accuracy_val_rf'),
                                     
                                     #verbatimTextOutput("cnf_matrix_glm"),
                                     #verbatimTextOutput("cnf_matrix_rf")
                                     
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
                                     uiOutput("rf_predictors"),
                                     actionButton('predict', 'Predict', class = "btn-primary btn-lg"),
                                     br(),
                                     br(),
                                     textOutput('pred_glm'),
                                     br(),
                                     textOutput('pred_rf')
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