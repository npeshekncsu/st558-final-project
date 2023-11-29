library(shiny)
library(caret)
library(tidyverse)
library(DT)
library(ggplot2)
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


shinyServer(function(input, output) {
    
    
    output$summary_plot <- renderPlot({
        
        if (input$variables_to_summarize == 'Sulfate') {
            
            if(input$graph_type == 'Density') {
                ggplot(data, aes(x=Sulfate, fill=as.factor(Potability)))+
                    geom_density(alpha=.5) +
                    scale_fill_manual(
                        values = c("0" = "grey", "1" = "red"),
                        labels = c("0" = "Not potable", "1" = "Potable")
                    ) +
                    labs(fill = "Water Potability")
            }
            
            else if(input$graph_type == 'Violin') {
                ggplot(data, aes(x = as.factor(Potability), 
                                 y = Sulfate, group = Potability, fill = as.factor(Potability))) +
                    labs(
                        title = "Violin plot of sulfate by potability status",
                        x = "potability",
                        y = "Sulfate"
                    ) +
                    geom_violin(trim = FALSE, alpha=.5) +
                    scale_fill_manual(
                        values = c("0" = "grey", "1" = "red"),
                        labels = c("0" = "Not potable", "1" = "Potable")
                    ) +
                    labs(fill = "Water Potability")
            }
            
            
        }
        else if  (input$variables_to_summarize == 'Hardness') {
            if(input$graph_type == 'Density') {
                ggplot(data, aes(x=Hardness, fill=as.factor(Potability)))+
                    geom_density(alpha=.5) +
                    scale_fill_manual(
                        values = c("0" = "grey", "1" = "red"),
                        labels = c("0" = "Not potable", "1" = "Potable")
                    ) +
                    labs(fill = "Water Potability")
                
            }
            else if (input$graph_type == 'Violin') {
                ggplot(data, aes(x = as.factor(Potability), 
                                 y = Hardness, group = Potability, fill = as.factor(Potability))) +
                    labs(
                        title = "Violin plot of hardness by potability status",
                        x = "potability",
                        y = "Hardness"
                    ) +
                    geom_violin(trim = FALSE, alpha=.5) +
                    scale_fill_manual(
                        values = c("0" = "grey", "1" = "red"),
                        labels = c("0" = "Not potable", "1" = "Potable")
                    ) +
                    labs(fill = "Water Potability")
            }
            
            
        
            
        }
        
        else if  (input$variables_to_summarize == 'Solids') {
            
            if(input$graph_type == 'Density') {
                ggplot(data, aes(x=Solids, fill=as.factor(Potability)))+
                    geom_density(alpha=.5) +
                    scale_fill_manual(
                        values = c("0" = "grey", "1" = "red"),
                        labels = c("0" = "Not potable", "1" = "Potable")
                    ) +
                    labs(fill = "Water Potability")
            }
            else if (input$graph_type == 'Violin') {
                ggplot(data, aes(x = as.factor(Potability), 
                                 y = Solids, group = Potability, fill = as.factor(Potability))) +
                    labs(
                        title = "Violin plot of solids by potability status",
                        x = "potability",
                        y = "Solids"
                    ) +
                    geom_violin(trim = FALSE, alpha=.5) +
                    scale_fill_manual(
                        values = c("0" = "grey", "1" = "red"),
                        labels = c("0" = "Not potable", "1" = "Potable")
                    ) +
                    labs(fill = "Water Potability")
            }
            
            
            
        }
        
        else if  (input$variables_to_summarize == 'Chloramines') {
            
            if(input$graph_type == 'Density') {
                ggplot(data, aes(x=Chloramines, fill=as.factor(Potability)))+
                    geom_density(alpha=.5) +
                    scale_fill_manual(
                        values = c("0" = "grey", "1" = "red"),
                        labels = c("0" = "Not potable", "1" = "Potable")
                    ) +
                    labs(fill = "Water Potability")
            }
            else if (input$graph_type == 'Violin') {
                ggplot(data, aes(x = as.factor(Potability), 
                                 y = Chloramines, group = Potability, fill = as.factor(Potability))) +
                    labs(
                        title = "Violin plot of chloramines by potability status",
                        x = "potability",
                        y = "Chloramines"
                    ) +
                    geom_violin(trim = FALSE, alpha=.5) +
                    scale_fill_manual(
                        values = c("0" = "grey", "1" = "red"),
                        labels = c("0" = "Not potable", "1" = "Potable")
                    ) +
                    labs(fill = "Water Potability")
            }
            
        }
        
        else if  (input$variables_to_summarize == 'Conductivity') {
            if(input$graph_type == 'Density') {
                ggplot(data, aes(x=Conductivity, fill=as.factor(Potability)))+
                    geom_density(alpha=.5) +
                    scale_fill_manual(
                        values = c("0" = "grey", "1" = "red"),
                        labels = c("0" = "Not potable", "1" = "Potable")
                    ) +
                    labs(fill = "Water Potability")
            }
            else if (input$graph_type == 'Violin') {
                ggplot(data, aes(x = as.factor(Potability), 
                                 y = Conductivity, group = Potability, fill = as.factor(Potability))) +
                    labs(
                        title = "Violin plot of conductivity by potability status",
                        x = "potability",
                        y = "Conductivity"
                    ) +
                    geom_violin(trim = FALSE, alpha=.5) +
                    scale_fill_manual(
                        values = c("0" = "grey", "1" = "red"),
                        labels = c("0" = "Not potable", "1" = "Potable")
                    ) +
                    labs(fill = "Water Potability")
            }
            
          
        }
        
        else if  (input$variables_to_summarize == 'Organic_carbon') {
            
            if(input$graph_type == 'Density') {
                ggplot(data, aes(x=Organic_carbon, fill=as.factor(Potability)))+
                    geom_density(alpha=.5) +
                    scale_fill_manual(
                        values = c("0" = "grey", "1" = "red"),
                        labels = c("0" = "Not potable", "1" = "Potable")
                    ) +
                    labs(fill = "Water Potability")
            }
            else if (input$graph_type == 'Violin') {
                ggplot(data, aes(x = as.factor(Potability), 
                                 y = Organic_carbon, group = Potability, fill = as.factor(Potability))) +
                    labs(
                        title = "Violin plot of organic carbon by potability status",
                        x = "potability",
                        y = "Conductivity"
                    ) +
                    geom_violin(trim = FALSE, alpha=.5) +
                    scale_fill_manual(
                        values = c("0" = "grey", "1" = "red"),
                        labels = c("0" = "Not potable", "1" = "Potable")
                    ) +
                    labs(fill = "Water Potability")
            }
            
            
        }
        
        else if  (input$variables_to_summarize == 'Trihalomethanes') {
            
            if(input$graph_type == 'Density') {
                ggplot(data, aes(x=Trihalomethanes, fill=as.factor(Potability)))+
                    geom_density(alpha=.5) +
                    scale_fill_manual(
                        values = c("0" = "grey", "1" = "red"),
                        labels = c("0" = "Not potable", "1" = "Potable")
                    ) +
                    labs(fill = "Water Potability")
            }
            else if (input$graph_type == 'Violin') {
                ggplot(data, aes(x = as.factor(Potability), 
                                 y = Trihalomethanes, group = Potability, fill = as.factor(Potability))) +
                    labs(
                        title = "Violin plot of trihalomethanes by potability status",
                        x = "potability",
                        y = "Conductivity"
                    ) +
                    geom_violin(trim = FALSE, alpha=.5) +
                    scale_fill_manual(
                        values = c("0" = "grey", "1" = "red"),
                        labels = c("0" = "Not potable", "1" = "Potable")
                    ) +
                    labs(fill = "Water Potability")
            }
            
            
            
        }
        
        else if  (input$variables_to_summarize == 'Turbidity') {
            
            if(input$graph_type == 'Density') {
                ggplot(data, aes(x=Turbidity, fill=as.factor(Potability)))+
                    geom_density(alpha=.5) +
                    scale_fill_manual(
                        values = c("0" = "grey", "1" = "red"),
                        labels = c("0" = "Not potable", "1" = "Potable")
                    ) +
                    labs(fill = "Water Potability")
            }
            else if (input$graph_type == 'Violin') {
                ggplot(data, aes(x = as.factor(Potability), 
                                 y = Turbidity, group = Potability, fill = as.factor(Potability))) +
                    labs(
                        title = "Violin plot of turbidity by potability status",
                        x = "potability",
                        y = "Conductivity"
                    ) +
                    geom_violin(trim = FALSE, alpha=.5) +
                    scale_fill_manual(
                        values = c("0" = "grey", "1" = "red"),
                        labels = c("0" = "Not potable", "1" = "Potable")
                    ) +
                    labs(fill = "Water Potability")
            }
            
        }
        
        else if  (input$variables_to_summarize == 'ph') {
            
            if(input$graph_type == 'Density') {
                ggplot(data, aes(x=ph, fill=as.factor(Potability)))+
                    geom_density(alpha=.5) +
                    scale_fill_manual(
                        values = c("0" = "grey", "1" = "red"),
                        labels = c("0" = "Not potable", "1" = "Potable")
                    ) +
                    labs(fill = "Water Potability")
            }
            else if (input$graph_type == 'Violin') {
                ggplot(data, aes(x = as.factor(Potability), 
                                 y = ph, group = Potability, fill = as.factor(Potability))) +
                    labs(
                        title = "Violin plot of ph by potability status",
                        x = "potability",
                        y = "Conductivity"
                    ) +
                    geom_violin(trim = FALSE, alpha=.5) +
                    scale_fill_manual(
                        values = c("0" = "grey", "1" = "red"),
                        labels = c("0" = "Not potable", "1" = "Potable")
                    ) +
                    labs(fill = "Water Potability")
            }
            
            
            
        }
    })
    
    output$scatter_plot <- renderPlot ({
        ggplot(data, aes(x=get(input$x_var), y=get(input$y_var), color=as.factor(Potability))) + geom_point()
    })
    
    output$quantile_plot <- renderPlot({
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