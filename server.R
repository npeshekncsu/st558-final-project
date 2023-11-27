library(shiny)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output, session) {
    
    getData <- reactive({
        newData <- msleep %>% filter(vore == input$vore)
    })
    
    #create plot
    output$sleepPlot <- renderPlot({
        #get filtered data
        newData <- getData()
        
        #create plot
        g <- ggplot(newData, aes(x = bodywt, y = sleep_total))
        
        observe({updateSliderInput(session, "size", value = input$size)})
        
        if(input$conservation & input$change_symbol_checkbox){
            
            observe({updateSliderInput(session, "size",  min = 3)})

            g <- ggplot(newData, aes(x = bodywt, y = sleep_total, alpha=newData$sleep_rem))
            g + geom_point(size = input$size, aes(col = conservation)) + labs(alpha = "sleep_rem")
            
        } else if(input$conservation) {
            
            observe({updateSliderInput(session, "size",  min = 1)})
            
            g + geom_point(size = input$size, aes(col = conservation))
            
        } else {
            g + geom_point(size = input$size)
        }
        
    })
    
    #create text info
    output$info <- renderText({
        #get filtered data
        newData <- getData()
        
        paste("The average body weight for order", input$vore, "is", round(mean(newData$bodywt, na.rm = TRUE), 2), "and the average total sleep time is", round(mean(newData$sleep_total, na.rm = TRUE), 2), sep = " ")
    })
    
    #create output of observations    
    output$table <- renderTable({
        getData()
    })
    
    output$titlePanel <- renderUI({
        text <- paste0("Investigation of ", str_to_title(input$vore), "vore Mammal Sleep Data") 
        titlePanel(h1(text))
    })
    
})