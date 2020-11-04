library(shiny)
library(datasets)

#Shiny is an R package that allows you to easily build interactive web applications.
#The benefit of using Shiny is that it makes it possible to extend your R code to the web that would essentially help to expand its usability to a wider community (i.e. from being used by a single user to being used by hundreds or thousands of users via the internet).

datasetinuse <- mtcars
datasetinuse$am <- factor(datasetinuse$am, labels = c("Automatic", "Manual"))

shinyServer(function(input, output) {
    
    givencontent <- reactive({
        paste("mpg ~", input$variable)
    })
    
    givencontentPoint <- reactive({
        paste("mpg ~", "as.integer(", input$variable, ")")
    })
    
    modelinuse <- reactive({
        lm(as.formula(givencontentPoint()), data=datasetinuse)
    })
    
    output$caption <- renderText({
        givencontent()
    })
    
    output$mpgBoxPlot <- renderPlot({
        boxplot(as.formula(givencontent()), 
                data = datasetinuse,
                outline = input$outliers)
    })
    
    output$modelinuse <- renderPrint({
        summary(modelinuse())
    })
    
    output$mpgPlot <- renderPlot({
        with(datasetinuse, {
            plot(as.formula(givencontentPoint()))
            abline(modelinuse(), col=2)
        })
    })
    
})

