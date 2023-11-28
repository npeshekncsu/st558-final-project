library(shiny)
library(caret)
library(tidyverse)
library(DT)
library(ggplot2)
#data("GermanCredit")

shinyServer(function(input, output) {
    
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