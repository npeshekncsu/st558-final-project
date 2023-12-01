library(shiny)
library(caret)
library(tidyverse)
library(DT)
library(ggplot2)
library(corrplot)
library(caret)
#data("GermanCredit")


data = read_csv('./data/water_potability.csv')
data = na.omit(data)

data <- data %>%
    mutate(Sulfate_quartiles = case_when(
        Sulfate <= quantile(data$Sulfate)[[1]] ~ "Q1",
        Sulfate > quantile(data$Sulfate)[[1]] & Sulfate <= quantile(data$Sulfate)[[2]] ~ "Q2",
        Sulfate > quantile(data$Sulfate)[[2]] & Sulfate <= quantile(data$Sulfate)[[3]] ~ "Q3",
        Sulfate > quantile(data$Sulfate)[[3]] ~ "Q4"
    )) %>% mutate(Sulfate_quartiles = as.factor(Sulfate_quartiles))


data <- data %>%
    mutate(Chloramines_quartiles = case_when(
        Chloramines <= quantile(data$Chloramines)[[1]] ~ "Q1",
        Chloramines > quantile(data$Chloramines)[[1]] & Chloramines <= quantile(data$Chloramines)[[2]] ~ "Q2",
        Chloramines > quantile(data$Chloramines)[[2]] & Chloramines <= quantile(data$Chloramines)[[3]] ~ "Q3",
        Chloramines > quantile(data$Chloramines)[[3]] ~ "Q4"
    )) %>% mutate(Chloramines_quartiles = as.factor(Chloramines_quartiles))


data <- data %>%
    mutate(Solids_quartiles = case_when(
        Solids <= quantile(data$Solids)[[1]] ~ "Q1",
        Solids > quantile(data$Solids)[[1]] & Solids <= quantile(data$Solids)[[2]] ~ "Q2",
        Solids > quantile(data$Solids)[[2]] & Solids <= quantile(data$Solids)[[3]] ~ "Q3",
        Solids > quantile(data$Solids)[[3]] ~ "Q4"
    )) %>% mutate(Solids_quartiles = as.factor(Solids_quartiles))


split_data <- function(partition, data) {
    
    results <- list()
    trainIndex <- createDataPartition(data$Potability, p = partition, 
                                      list = FALSE, 
                                      times = 1)
    train_data = data[trainIndex, ]
    val_data = data[-trainIndex, ]
    
    results = append(results, train_data)
    results = append(results, val_data)
    
    return (results)
}


shinyServer(function(input, output) {
    
    output$summary_plot <- renderPlot({
        if(input$graph_type == 'Density') {
            ggplot(data, aes(x=get(input$variables_to_summarize), fill=as.factor(Potability)))+
                                 geom_density(alpha=.5) +
                                 scale_fill_manual(
                                     values = c("0" = "grey", "1" = "red"),
                                     labels = c("0" = "Not potable", "1" = "Potable")
                                 ) +
                                 labs(fill = "Water Potability")
            
        }
        else if(input$graph_type == 'Violin') {
            ggplot(data, aes(x = as.factor(Potability), 
                             y = get(input$variables_to_summarize), 
                             group = Potability, 
                             fill = as.factor(Potability)) ) +
                labs(#title = "Violin plot of sulfate by potability status",
                     x = "potability",
                     y = "Sulfate") + 
                geom_violin(trim = FALSE, alpha=.5) + scale_fill_manual(
                    values = c("0" = "grey", "1" = "red"),
                    labels = c("0" = "Not potable", "1" = "Potable")) +
                labs(fill = "Water Potability")
        }
        else if(input$graph_type == 'Boxplot') {
            ggplot(data, aes(x = Potability, 
                             y = get(input$variables_to_summarize), fill = as.factor(Potability))) + 
                geom_boxplot(alpha=.5) + scale_fill_manual(
                    values = c("0" = "grey", "1" = "red"),
                    labels = c("0" = "Not potable", "1" = "Potable")) +
                labs(fill = "Water Potability")
        }
    })
    
    output$scatter_plot <- renderPlot ({
        ggplot(data, aes(x=get(input$x_var), y=get(input$y_var), color=as.factor(Potability))) + 
            geom_point() + scale_fill_manual(
                values = c("0" = "grey", "1" = "red"),
                labels = c("0" = "Not potable", "1" = "Potable")) +
            labs(fill = "Water Potability")
    })
    
    output$quantile_plot <- renderPlot({
        
        if (input$histogram_var == 'Sulfate') {
            ggplot(data, aes(x = Sulfate_quartiles, fill = as.factor(Potability), group = Potability)) +
                geom_bar(position = "stack", alpha=.5) +
                labs(
                    title = "Number of cases of potable and not potable water for each sulfate quantile",
                    x = "Sulfate quantiles",
                    y = "Number of Cases"
                ) +
                scale_fill_manual(
                    values = c("0" = "grey", "1" = "red"),
                    labels = c("0" = "Not potable", "1" = "Potable")
                ) +
                labs(fill = "Water Potability")
        }
        else if (input$histogram_var == 'Solids') {
            ggplot(data, aes(x = Solids_quartiles, fill = as.factor(Potability), group = Potability)) +
                geom_bar(position = "stack", alpha=.5) +
                labs(
                    title = "Number of cases of potable and not potable water for each sulfate quantile",
                    x = "Solids quantiles",
                    y = "Number of Cases"
                ) +
                scale_fill_manual(
                    values = c("0" = "grey", "1" = "red"),
                    labels = c("0" = "Not potable", "1" = "Potable")
                ) +
                labs(fill = "Water Potability")
        }
        else if (input$histogram_var == 'Chloramines') {
            ggplot(data, aes(x = Chloramines_quartiles, fill = as.factor(Potability), group = Potability)) +
                geom_bar(position = "stack", alpha=.5) +
                labs(
                    title = "Number of cases of potable and not potable water for each sulfate quantile",
                    x = "Chloramines quantiles",
                    y = "Number of Cases"
                ) +
                scale_fill_manual(
                    values = c("0" = "grey", "1" = "red"),
                    labels = c("0" = "Not potable", "1" = "Potable")
                ) +
                labs(fill = "Water Potability")
        }
        
        
    })
    
    output$corr_plot <- renderPlot({
        
        if (input$corr_plot_type == 'Color') {
            corrplot(cor(as.matrix(data %>% select(-Sulfate_quartiles, -Solids_quartiles, -Chloramines_quartiles))), 
                     #corrplot(cor(as.matrix(data)), 
                     type="upper", 
                     #method = 'number',
                     method = 'color', order = 'alphabet',
                     #method = 'ellipse', order = 'AOE', type = 'upper',
                     tl.pos = "lt")
        }
        else if (input$corr_plot_type == 'Number') {
            corrplot(cor(as.matrix(data %>% select(-Sulfate_quartiles, -Solids_quartiles, -Chloramines_quartiles))), 
                     #corrplot(cor(as.matrix(data)), 
                     type="upper", 
                     method = 'number',
                     #method = 'color', order = 'alphabet',
                     #method = 'ellipse', order = 'AOE', type = 'upper',
                     tl.pos = "lt")
        }
        
        else if (input$corr_plot_type == 'Elipse') {
            corrplot(cor(as.matrix(data %>% select(-Sulfate_quartiles, -Solids_quartiles, -Chloramines_quartiles))), 
                     #corrplot(cor(as.matrix(data)), 
                     #type="upper", 
                     #method = 'number',
                     #method = 'color', order = 'alphabet',
                     method = 'ellipse', order = 'AOE', type = 'upper',
                     tl.pos = "lt")
        }
        
        
        #eventReactive(input$train, {
        observeEvent(input$train, {
            showNotification("This is a notification.")
            #split_results = split_data(partition = 0.8, data)
            
            trainIndex <- createDataPartition(data$Potability, p = 0.8, 
                                              list = FALSE, 
                                              times = 1)
            train_data = data[trainIndex, ]
            val_data = data[-trainIndex, ]
            
            train_data$Potability = as.factor(train_data$Potability)
            val_data$Potability = as.factor(val_data$Potability)
            
            
            glm_predictors = input$glm_predictor_selector
            glm_model = train(reformulate(glm_predictors, "Potability"), 
                               data = train_data,
                               method = "glm", 
                               family="binomial"
                               #metric="logLoss",
                               #trControl = train.control
            )
            
            rf_predictors = input$rf_predictor_selector
            
            
            observe({print((input$rf_predictor_selector)) })
            
            #print(input$rf_predictor_selector)
            #print(head(train_data))
            
            print(glm_predictors)
            print(summary(glm_model))
            print('here')
            print(glm_model$results$Accuracy)
            
            #glm_summary <- renderText({
            #    #summary(glm_model$results$Accuracy)
            #    'my text'
            #})
            #output$glm_summary <- renderText({paste("You have selected", "input$var")})
            #output$glm_summary <- renderText({glm_model$results$Accuracy})
            output$glm_summary <- renderText({paste('Accuaracy for GLM model:', glm_model$results$Accuracy)})
            
            
        })
        
       
        
        
        
    })
    
        
        
        #ggplot(data, aes(x=Sulfate, fill=as.factor(Potability)))+
        ##ggplot(data, aes(x=input$variables_to_summarize, fill=as.factor(Potability)))+
        #    geom_density(alpha=.5) +
        #    scale_fill_manual(
        #        values = c("0" = "grey", "1" = "red"),
        #        labels = c("0" = "Not potable", "1" = "Potable")
        #    ) +
        #    labs(fill = "Water Potability")
    #})
    
    
    # output$summary_plot <- renderPlot({
    #     if (input$radio_choice == 'Just Classification') {
    #         
    #         ggplot(GermanCredit, aes(x = Class)) +
    #             geom_bar() +
    #             labs(x = "Class",
    #                  y = "count")
    #         
    #     } else if (input$radio_choice == 'Classification and Unemployed') {
    #         
    #         ggplot(GermanCredit, aes(x = Class, fill = as.factor(EmploymentDuration.Unemployed))) +
    #             geom_bar(position = "dodge", stat = "count") +
    #             labs(x = "Class",
    #                  y = "count") +
    #             labs(fill = "Unemployment status") +
    #             scale_fill_manual(
    #                 values = c("0" = "#F8766D", "1" = "#00BFC4"),
    #                 labels = c("0" = "Employed", "1" = "Unemployed")
    #             )
    #         
    #         
    #     } else {
    #         ggplot(GermanCredit, aes(x = Class, fill = as.factor(ForeignWorker))) +
    #             geom_bar(position = "dodge", stat = "count") +
    #             labs(x = "Class",
    #                  y = "count") +
    #             labs(fill = "Status") +
    #             scale_fill_manual(
    #                 values = c("0" = "#F8766D", "1" = "#00BFC4"),
    #                 labels = c("0" = "German", "1" = "Foreign")
    #             )
    #         
    #     }
    # })
    # 
    # output$summary_table <- renderDataTable({
    #     round = as.numeric(input$numeric_value)
    #     var = input$variables_to_summarize
    #     GermanCredit %>%
    #         select("Class", "InstallmentRatePercentage", var) %>%
    #         group_by(Class, InstallmentRatePercentage) %>%
    #         summarize(mean = round(mean(get(var)), round))
    # })
})