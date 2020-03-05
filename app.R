#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(DALEX)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Exploring models"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(4, 
      verbatimTextOutput("summary")
      ),
     column(4,
      plotOutput("predictPlot",
                 hover = "plot_hover",
                 width = "500px")
      ),
    column(4,
      p("Hover over a predicted point to view the data and see a breakdown 
        of the prediction."),
      verbatimTextOutput("selectedPoint"),
      plotOutput("plotExplainObservation",
                 width = "500px")
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  my_data <- mtcars
  
  # Find linear regression model
  fit <- lm(
    formula = "mpg ~ .",
    data = mtcars
  )
  
  # Predict values using lm model
  my_data$predicted <- predict(fit, mtcars)
  
  # Rearrange data
  my_data$IDno <- rank(my_data$mpg)
  plotData <- tidyr::gather(data = my_data,
                            key = "plot",
                            value = "value",
                            mpg, predicted)
  plotData <- plotData %>%
    select(c(IDno, plot, value))
  plotData <- as.data.frame(plotData)
  
  # Output summary of model fit
  output$summary <- renderPrint({
    return(summary(fit))
  })
  
  # Reactive - data
  plotData <- reactive({
    my_data <- mtcars
    
    # Find linear regression model
    fit <- lm(
      formula = "mpg ~ .",
      data = mtcars
    )
    
    # Predict values using lm model
    my_data$predicted <- predict(fit, mtcars)
    
    # Rearrange data
    my_data$IDno <- rank(my_data$mpg)
    plotData <- tidyr::gather(data = my_data,
                              key = "plot",
                              value = "value",
                              mpg, predicted)
    
    plotData <- as.data.frame(plotData)
  })
  
  # Reactive - selected point
  selectedPoint <- reactive({
    if(is.null(input$plot_hover)) return(NULL)
    
    # With base graphics, need to tell it what the x and y variables are.
    selectedPoint <- nearPoints(plotData(), 
                                input$plot_hover, 
                                #threshold = 10, 
                                maxpoints = 1,
                                allRows = TRUE)
    
    # # Isolate row number 
    # selectedPoint <- selectedPoint %>% 
    #   dplyr::filter(selected_ == TRUE)
  })
  
  # Output plot of observed vs actuals
  output$predictPlot <- renderPlot({
    ggplot2::ggplot() +
      geom_point(data = plotData(), 
                 aes(x = IDno,
                     y = value,
                     color = factor(plot, labels = c(
                       "Actuals",
                       "Predicted"
                     )),
                     size = 1)) +
    labs(color = "Data",
         x = " ",
         y = " ") + 
      theme_bw() 
  })
  
  # Find point if hovered over by user
  output$selectedPoint <- renderPrint({
    if(is.null(input$plot_hover)) return("NULL\n")
    
    text <- selectedPoint() %>%
      dplyr::filter(selected_ == TRUE)

    print(text)
  })

  # library(lime)
  # 
  # ind <- sample(1:nrow(mtcars), 5)
  # data_explain <- mtcars[ind, 1:4]
  # data_train <- mtcars[-ind, 1:4]
  # data_lab <- mtcars[[5]][-ind]
  # 
  # library(caret)
  # model <- train(data_train,
  #                data_lab,
  #                method = "rf")
  # 
  # model_lm <- lm("mpg ~ .", mtcars)
  # 
  # explainer <- lime::lime(data_train,
  #                         model,
  #                         bin_continuous = TRUE,
  #                         n_bins = 4,
  #                         quantile_bins = TRUE)
  # 
  # explainer_lm <- lime::lime(mtcars,
  #                         model_lm,
  #                         bin_continuous = TRUE,
  #                         n_bins = 4,
  #                         quantile_bins = TRUE)
  # 
  # explanation <- lime::explain(
  #   data_explain,
  #   explainer,
  #   n_labels = 1,
  #   n_features = 3,
  #   n_permutations = 5000,
  #   feature_select = "auto"
  # )
  # 
  # 
  # explanation_lm <- lime::explain(
  #   mtcars[1:4],
  #   explainer_lm,
  #   n_labels = 1,
  #   n_permutations = 5000,
  #   feature_select = "auto"
  # )
  # 
  # plot_features(explanation)
  
  output$plotExplainObservation <- renderPlot({
    if (is.null(selectedPoint())) {
      return(NULL)
    } else {
      # Fetch data and filter out data for selected data point
      allData <- selectedPoint() 
      
      selectedData <- allData %>%
        dplyr::filter(selected_ == TRUE)
      
      # check if point is predicted
      if (selectedData[, "plot"] == "predicted") {
        # Setup explainer variable
        #browser()
        explainer_lm <- DALEX::explain(model = fit,
                                       data = mtcars,
                                       y = mtcars$mpg)
        
        # remove extra columns that would confuse DALEX
        selectedData <- spread(allData, plot, value) %>%
          dplyr::filter(selected_ == TRUE)
       
        selectedData <- selectedData %>%
          select(names(mtcars))
        selectedData <- as.data.frame(selectedData)
        
        selectedData$`(Intercept)` <- 1
        
        # Get explaination for the selected prediction
        sv_lm <- DALEX::single_prediction(
              explainer_lm,
              observation = selectedData
            )
        
        # plot result
        plot(sv_lm)
      } else {
        return(NULL)
      }
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

