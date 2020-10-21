library(shiny)
library(RNASeqPower)
library(dplyr)
library(tidyverse)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Sample Size Estimator for RNA-Seq"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      textInput('power', 'Enter Desired Power (comma delimited)', "0.8,0.9,0.95"),
      textInput('effect', 'Enter Mean Fold Changes (comma delimited)', "1.25,1.5,1.75,3"),
      sliderInput("alpha",  "Alpha",
                  min = 0.01, max = 0.99, value = 0.05),
      numericInput("cv", "Coefficient of Variation (group 1)", value = 0.4),
      numericInput("cv2", "Coefficient of Variation (group 2)", value = 0.4),
      numericInput("depth", "Sequencing Depth", value = 20),
    ),
    
    mainPanel(
      
      plotOutput(outputId = "ssPlot")
      
    )
  )
)

server <- function(input, output) {
  
  output$ssPlot <- renderPlot({
    
    effect <- as.numeric(unlist(strsplit(input$effect,",")))
    power <- as.numeric(unlist(strsplit(input$power,",")))
    alpha <- as.numeric(input$alpha)
    
    samplesizes <- rnapower(depth=input$depth, cv=input$cv, cv2 = input$cv2, effect = effect,
                            alpha= alpha, power=power)
    
    samplesizes<-samplesizes%>%
      as.tibble(samplesizes)%>%
      add_column(effect)%>%
      pivot_longer(!effect, names_to="power", values_to = "samplesize")
    
    
    ggplot(samplesizes)+
      geom_line(mapping = aes(x = effect, y =samplesize, color = power))+
      labs(title = "Sample Size Calculator from desired Power and Effect", x = "Effect (Mean Fold Change)", y = "Sample Size", color = "Desired Power")+
      theme_linedraw()
    
  })
  
}

shinyApp(ui = ui, server = server)



















