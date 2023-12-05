library(shiny)
library(caret)
library(tidyverse)
library(DT)
library(ggplot2)
library(corrplot)
library(Metrics)


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
    mutate(Hardness_quartiles = case_when(
        Hardness <= quantile(data$Hardness)[[1]] ~ "Q1",
        Hardness > quantile(data$Hardness)[[1]] & Hardness <= quantile(data$Hardness)[[2]] ~ "Q2",
        Hardness > quantile(data$Hardness)[[2]] & Hardness <= quantile(data$Hardness)[[3]] ~ "Q3",
        Hardness > quantile(data$Hardness)[[3]] ~ "Q4"
    )) %>% mutate(Hardness_quartiles = as.factor(Hardness_quartiles))


data <- data %>%
    mutate(Solids_quartiles = case_when(
        Solids <= quantile(data$Solids)[[1]] ~ "Q1",
        Solids > quantile(data$Solids)[[1]] & Solids <= quantile(data$Solids)[[2]] ~ "Q2",
        Solids > quantile(data$Solids)[[2]] & Solids <= quantile(data$Solids)[[3]] ~ "Q3",
        Solids > quantile(data$Solids)[[3]] ~ "Q4"
    )) %>% mutate(Solids_quartiles = as.factor(Solids_quartiles))


data <- data %>%
    mutate(Organic_carbon_quartiles = case_when(
        Organic_carbon <= quantile(data$Organic_carbon)[[1]] ~ "Q1",
        Organic_carbon > quantile(data$Organic_carbon)[[1]] & Organic_carbon <= quantile(data$Organic_carbon)[[2]] ~ "Q2",
        Organic_carbon > quantile(data$Organic_carbon)[[2]] & Organic_carbon <= quantile(data$Organic_carbon)[[3]] ~ "Q3",
        Organic_carbon > quantile(data$Organic_carbon)[[3]] ~ "Q4"
    )) %>% mutate(Organic_carbon_quartiles = as.factor(Organic_carbon_quartiles))

if_water_potable <- function(binary_value) {
    if (binary_value == 0) {
        return ('Not potable water')
    }
    else {
        return ('Potable water')
    }
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
                                 labs(fill = "Water Potability",
                                      title = "Density plot",
                                      x = '', 
                                      y = '',) + 
                theme_bw()
            
        }
        else if(input$graph_type == 'Violin') {

            ggplot(data, aes(x = as.factor(Potability), 
                             y = get(input$variables_to_summarize), 
                             group = Potability, 
                             fill = as.factor(Potability)) ) +
                labs(
                    title = "Violin plot",
                    x = '', 
                    y = '',
                    fill = "Water Potability"
                ) + 
                geom_violin(trim = FALSE, alpha = 0.5) + 
                scale_fill_manual(
                    values = c("0" = "grey", "1" = "red"),
                    labels = c("0" = "Not potable", "1" = "Potable")
                ) +
                theme_bw()
            
            
            
        }
        else if(input$graph_type == 'Boxplot') {
            ggplot(data, aes(x = Potability, 
                             y = get(input$variables_to_summarize), fill = as.factor(Potability))) + 
                geom_boxplot(alpha=.5) + scale_fill_manual(
                    values = c("0" = "grey", "1" = "red"),
                    labels = c("0" = "Not potable", "1" = "Potable")) +
                labs(fill = "Water Potability", 
                     title = "Boxplot",
                     x = '', 
                     y = '',) + theme_bw()
        }
    })
    
    output$scatter_plot <- renderPlot ({
       
        ggplot(data, aes(x=get(input$x_var), y=get(input$y_var), color=as.factor(Potability))) + 
            geom_point() + scale_color_manual(
                values = c("0" = "grey", "1" = "red"),
                labels = c("0" = "Not potable", "1" = "Potable"),
                name = "Water Potability") +
            theme_bw() + 
            labs(fill = "Water Potability", 
                 x = '', 
                 y = '')
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
                labs(fill = "Water Potability") + theme_bw()
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
                labs(fill = "Water Potability") + theme_bw()
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
                labs(fill = "Water Potability") + theme_bw()
        }
        else if (input$histogram_var == 'Hardness') {
            ggplot(data, aes(x = Hardness_quartiles, fill = as.factor(Potability), group = Potability)) +
                geom_bar(position = "stack", alpha=.5) +
                labs(
                    title = "Number of cases of potable and not potable water for each hardness quantile",
                    x = "Hardness quantiles",
                    y = "Number of Cases"
                ) +
                scale_fill_manual(
                    values = c("0" = "grey", "1" = "red"),
                    labels = c("0" = "Not potable", "1" = "Potable")
                ) +
                labs(fill = "Water Potability") + theme_bw()
        } 
        else if(input$histogram_var == 'Organic_carbon') {
            ggplot(data, aes(x = Organic_carbon_quartiles, fill = as.factor(Potability), group = Potability)) +
                geom_bar(position = "stack", alpha=.5) +
                labs(
                    title = "Number of cases of potable and not potable water for each organic carbon quantile",
                    x = "Hardness quantiles",
                    y = "Number of Cases"
                ) +
                scale_fill_manual(
                    values = c("0" = "grey", "1" = "red"),
                    labels = c("0" = "Not potable", "1" = "Potable")
                ) +
                labs(fill = "Water Potability") + theme_bw()
        }
        
        
    })
    
    output$corr_plot <- renderPlot({
        if (input$corr_plot_type == 'Color') {
            corrplot(cor(as.matrix(data %>% select(-Sulfate_quartiles, -Solids_quartiles, 
                                                   -Chloramines_quartiles, -Hardness_quartiles,
                                                   -Organic_carbon_quartiles))), 
                     type="upper", 
                     method = 'color', order = 'alphabet',
                     tl.pos = "lt")
        }
        else if (input$corr_plot_type == 'Number') {
            corrplot(cor(as.matrix(data %>% select(-Sulfate_quartiles, -Solids_quartiles, 
                                                   -Chloramines_quartiles, -Hardness_quartiles,
                                                   -Organic_carbon_quartiles))), 
                     type="upper", 
                     method = 'number',
                     tl.pos = "lt")
        }
        else if (input$corr_plot_type == 'Elipse') {
            corrplot(cor(as.matrix(data %>% select(-Sulfate_quartiles, -Solids_quartiles, 
                                                   -Chloramines_quartiles, -Hardness_quartiles,
                                                   -Organic_carbon_quartiles))), 
                     method = 'ellipse', 
                     order = 'AOE', 
                     type = 'upper',
                     tl.pos = "lt")
        }
    })
    
    output$counts_for_levels <- renderPlot({
        ggplot(data, aes(x = as.factor(Potability), fill = as.factor(Potability))) +
            geom_bar(stat = "count", alpha = 0.5) +
            geom_text(stat = "count", aes(label = after_stat(count), vjust = -0.5)) +
            scale_fill_manual(
                values = c("0" = "grey", "1" = "red"),
                name = "Potability",
                breaks = c("0", "1"),
                labels = c("Non Potable", "Potable")
            ) +
            labs(
                title = "Counts for potability levels",
                x = "Potability",
                y = "Count"
            ) +
            theme_bw()
    })
        
    observeEvent(input$train, {
    
        showNotification("Training started")
        
        trainIndex <- createDataPartition(data$Potability, p = input$data_split/100,
                                          list = FALSE,
                                          times = 1)
        train_data = data[trainIndex, ]
        val_data = data[-trainIndex, ]

        train_data$Potability = as.factor(train_data$Potability)
        val_data$Potability = as.factor(val_data$Potability)

        glm_predictors = input$glm_predictor_selector
        train.control = trainControl(method = "cv", number = as.numeric(input$folds_glm) )
        glm_model = train(reformulate(glm_predictors, "Potability"),
                          data = train_data,
                          method = "glm",
                          family="binomial", #)
                          metric="Accuracy",
                          trControl = train.control)
        saveRDS(glm_model, "glm_model.RDS")

        
        rf_predictors = input$rf_predictor_selector
        train_control <- trainControl(
            method = "cv",
            number = as.numeric(input$folds_rf),
            search = "grid"
        )
        rf_model = train(reformulate(rf_predictors, "Potability"),
                         data = train_data,
                         method = "rf",
                         tuneGrid = expand.grid(mtry=c(as.numeric(input$minMtry):as.numeric(input$maxMtry)) ),
                         metric="Accuracy",
                         trControl = train_control,
                         importance = TRUE
                         )
        saveRDS(rf_model, "rf_model.RDS")

        glm_accuracy = glm_model$results$Accuracy
        rf_accuracy = max(rf_model$results$Accuracy)
        rf_best_result = rf_model$bestTune$mtry
             
        output$glm_summary <- renderText({paste('Accuaracy for logistic regression model on training dataset:', glm_accuracy)})
        output$rf_summary <- renderText({paste('Accuaracy for random forest model on training dataset:', rf_accuracy)})
        output$rf_best_result_mtry <- renderText({paste('Best result for hyperparameter MTRY:', rf_best_result)})
        
        test_predictions_glm <- predict(glm_model,  val_data %>% select(-Potability))
        test_predictions_rf <- predict(rf_model, val_data %>% select(-Potability))
        
        output$cnf_matrix_glm <- renderPrint({
            table(test_predictions_glm, val_data$Potability)
        })
        output$cnf_matrix_rf <- renderPrint({
            table(test_predictions_rf, val_data$Potability)
        })
        
        output$glm_accuracy_val_glm <- renderText({paste('Accuaracy for logistic regression model on validation dataset:', 
                                                accuracy(val_data$Potability, test_predictions_glm))})
        output$glm_accuracy_val_rf <- renderText({paste('Accuaracy for random forest model on validation dataset:', 
                                               accuracy(val_data$Potability, test_predictions_rf))})
            
        output$rf_model_plot <- renderPlot({
            plot(rf_model)
        })
        output$var_imprt_rf <- renderDataTable(varImp(rf_model)[[1]])
            
        observe({(input$rf_predictor_selector)})
        observe({(input$glm_predictor_selector)})
        })
        
        
    output$glm_predictors <- renderUI({
        glm_predictors_vec <- input$glm_predictor_selector
        n <- length(glm_predictors_vec)
        selectInputs <- lapply(1:n, function(i) {
            textInput(paste0("glm_", glm_predictors_vec[i]), label = glm_predictors_vec[i]) }) 
            do.call(tagList, selectInputs)
        })
        
    output$rf_predictors <- renderUI({
        rf_predictors_vec <- input$rf_predictor_selector
        n <- length(rf_predictors_vec)
        selectInputs <- lapply(1:n, function(i) {
            textInput(paste0("rf_", rf_predictors_vec[i]), label = rf_predictors_vec[i]) 
            }) 
            do.call(tagList, selectInputs)
        })
        
        
    observeEvent(input$predict, {
            showNotification("Prediction started.")
            glm_model_from_file <- readRDS("glm_model.RDS")
            rf_model_from_file <- readRDS("rf_model.RDS")
            
            glm_predictors_vec <- input$glm_predictor_selector
            rf_predictors_vec <- input$rf_predictor_selector
            
            new_data_glm = data.frame()
            values = c()
            for (i in 1:length(glm_predictors_vec)) {
                input_id = paste0('glm_', glm_predictors_vec[i])
                values <- append(values, input[[input_id]])
            }
            new_data_glm = rbind(new_data_glm, as.numeric(values) )
            colnames(new_data_glm) = glm_predictors_vec
            res_glm = predict(glm_model_from_file, newdata = new_data_glm)
            
            new_data_rf = data.frame()
            values = c()
            for (i in 1:length(rf_predictors_vec)) {
                input_id = paste0('rf_', rf_predictors_vec[i])
                values <- append(values, input[[input_id]])
            }
            
            new_data_rf = rbind(new_data_rf, as.numeric(values) )
            colnames(new_data_rf) = rf_predictors_vec
            res_rf = predict(rf_model_from_file, newdata = new_data_rf)
            
            output$pred_glm <- renderText({paste('Prediction from logistic regression model:', 
                                                 if_water_potable(res_glm) )})
            output$pred_rf <- renderText({paste('Prediction from random forest model:', 
                                                if_water_potable (res_rf))})
            br()
    })
})