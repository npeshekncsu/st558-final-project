library(shiny)
library(caret)
library(tidyverse)
library(DT)
library(ggplot2)
library(corrplot)
library(caret)
library(Metrics)
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

if_water_potable <- function(binary_value) {
    if (binary_value == 0) {
        return ('Not potable water')
    }
    else {
        return ('Potable water')
    }
}

current_glm_model = NULL
current_rf_model = NULL


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
    })
        
        
    observeEvent(input$train, {
    #eventReactive(input$train, {
    
        showNotification("Training started")
        
        print(input$data_split)
            
        trainIndex <- createDataPartition(data$Potability, p = input$data_split/100,
                                          list = FALSE,
                                          times = 1)
        train_data = data[trainIndex, ]
        val_data = data[-trainIndex, ]

        train_data$Potability = as.factor(train_data$Potability)
        val_data$Potability = as.factor(val_data$Potability)

        glm_predictors = input$glm_predictor_selector
        #train.control = trainControl(method = "cv", number = 3)
        
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
            #number = 3,
            search = "grid"
        )
        rf_model = train(reformulate(rf_predictors, "Potability"),
                         data = train_data,
                         method = "rf",
                         #tuneGrid = data.frame(mtry = c(1:5)),
                         #tuneGrid = expand.grid(mtry=c(1:5)),
                         tuneGrid = expand.grid(mtry=c(as.numeric(input$minMtry):as.numeric(input$maxMtry)) ),
                         #tuneGrid = data.frame(mtry = c(input$minMtry:input$maxMtry)),
                         metric="Accuracy",
                         trControl = train_control,
                         importance = TRUE
                         )
        saveRDS(rf_model, "rf_model.RDS")

        
        print(summary(glm_model))
        print(summary(rf_model) )
       
            
        glm_accuracy = glm_model$results$Accuracy
        rf_accuracy = max(rf_model$results$Accuracy)
            
        rf_best_result = rf_model$bestTune$mtry
             
        #glm_summary <- renderText({
        #    #summary(glm_model$results$Accuracy)
        #    'my text'
        #})
        #output$glm_summary <- renderText({paste("You have selected", "input$var")})
        #output$glm_summary <- renderText({glm_model$results$Accuracy})
        #output$glm_summary <- renderText({paste('Accuaracy for GLM model:', glm_model$results$Accuracy)})
        output$glm_summary <- renderText({paste('Accuaracy for GLM model on training dataset:', glm_accuracy)})
        output$rf_summary <- renderText({paste('Accuaracy for RF model on training dataset:', rf_accuracy)})
        
        
        test_predictions_glm <- predict(glm_model,  val_data %>% select(-Potability))
        
        test_predictions_rf <- predict(rf_model, val_data %>% select(-Potability))
        
        print(table(test_predictions_glm, val_data$Potability))
        
        print(table(test_predictions_rf, val_data$Potability))
        
        output$cnf_matrix_glm <- renderPrint({
            table(test_predictions_glm, val_data$Potability)
        })
        
        output$cnf_matrix_rf <- renderPrint({
            table(test_predictions_rf, val_data$Potability)
        })
        
        
        
        print(accuracy(val_data$Potability, test_predictions_rf))
        print(accuracy(val_data$Potability, test_predictions_glm))
        
        
        output$glm_accuracy_val_glm <- renderText({paste('Accuaracy for GLM model on validation dataset:', 
                                                accuracy(val_data$Potability, test_predictions_glm))})
        output$glm_accuracy_val_rf <- renderText({paste('Accuaracy for RF model on validation dataset:', 
                                               accuracy(val_data$Potability, test_predictions_rf))})
            
        #output$rf_summary <- renderText({paste('Accuaracy for random forest model:', rf_model$results$Accuracy)})
        
        output$rf_model_plot <- renderPlot({
            plot(rf_model)
        })
        
        #output$var_imprt_rf <- renderText({paste('test', varImp(rf_model) )})
        output$var_imprt_rf <- renderDataTable(varImp(rf_model)[[1]])
            
        observe({(input$rf_predictor_selector)})
        observe({(input$glm_predictor_selector)})
        
        })
        
        
    output$glm_predictors <- renderUI({
        #observe({(input$glm_predictor_selector)})
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
            
            print(new_data_glm)
            
            res_glm = predict(glm_model_from_file, newdata = new_data_glm)#, type = "prob")
            print(res_glm)
            
            
            
            new_data_rf = data.frame()
            values = c()
            for (i in 1:length(rf_predictors_vec)) {
                input_id = paste0('rf_', rf_predictors_vec[i])
                values <- append(values, input[[input_id]])
            }
            
            new_data_rf = rbind(new_data_rf, as.numeric(values) )
            colnames(new_data_rf) = rf_predictors_vec
            
            print(new_data_rf)
            
            res_rf = predict(rf_model_from_file, newdata = new_data_rf)# , type = "prob")
            print(res_rf)
            
            output$pred_glm <- renderText({paste('Prediction from GLM model:', 
                                                 if_water_potable(res_glm) )})
            output$pred_rf <- renderText({paste('Prediction from Random Forest model:', 
                                                if_water_potable (res_rf))})
            br()
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
#})