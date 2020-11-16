library(shiny)
library(quantmod)
library('devtools')
library('ncells')
library(readxl)
library(tidyverse)

data <- read_excel(here::here("data", "ncells-data.xlsx"))%>%
  filter(!is.na(power))%>%
  filter(!sigma == 1.7)

# Define UI ----
ui <- fluidPage(
  titlePanel(h1("Power Calculator for scRNA-seq Experiment")),
 
  
  mainPanel( 
    plotOutput("plot"),
    h2("Power Estimate"),
    textOutput("n_cells"),
    textOutput("ncells")
    #tableOutput("ncells")
   
  ),
  
 
  
  sidebarPanel(
    
    h3("Parameter Input for Plot"),
    
    selectInput("fold_ch_plot",
                h5("Fold Change of Sub Pop"),
                c("1.5" = 1.5,
                  "2" = 2)
    ),
    
    selectInput("mean_expression_plot",
                h5("Mean Gene Expression"),
                c("1" = 1,
                  "1.5" = 1.5)),
    
    selectInput("standard_deviation_plot",
                h5("Standard Deviation of Mean Expression"),
                c("1" = 1,
                  "1.5" = 1.5)),
  ),
  
  sidebarPanel(
    
    fluidRow(
      h3("Parameter Input for Power Estimate"),
column(5, 
       
      
      sliderInput("frac_m_genes",
                  h5("Fraction of Marker Genes"),
                  min = 0.1, max = 10, value = 1),
      
      sliderInput("n_cells",
                  h5("Sample Size"),
                  min = 50, max = 500, value = 100),
      
      sliderInput("percent_sub_pop",
                  h5("Percentage of Sub Population"),
                  min = 5, max = 50, value = 10),
      
      sliderInput("fold_ch",
                  h5("Fold Change of Sub Pop"),
                  min = 1, max = 10, value = 2, step = 0),
      
      numericInput("drop_rate",  h5("Drop out Rate"),
                  min = 0.6, max = 0.999, value = 0.6)
),

column(7,
      
      
      numericInput("unique_gene_count",
                  h5("Unique Genes"),
                  min = 5000, max = 25000 ,value = 17000),
      
      sliderInput("mean_expression",
                  h5("Mean Gene Expression"),
                  min = 0.5, max = 10, value = 1.5),
      
      sliderInput("standard_deviation",
                  h5("Standard Deviation of Mean Expression"),
                  min = 0.5, max = 10, value = 2),
      
      sliderInput("alpha",  h5("Alpha"),
                  min = 0.01, max = 0.99, value = 0.05),
      
      
      h4("This may take a couple of minutes to run"),
      
      actionButton("go", "Go")
)
    )
    
    
  )
)

# Define server logic ----
server <- function(input, output) {

  
  
  size_reaction <- eventReactive( input$go, paste("You have selected a sample size of", input$n_cells) )
    
  output$n_cells <- renderText({
    size_reaction()
  })
  
  
  power_reaction <- eventReactive(input$go, 
            
    powerEst <- ncells(m1 = input$frac_m_genes, pi1=input$percent_sub_pop, foldchange = input$fold_ch, dropout = input$drop_rate, p = input$unique_gene_count, 
                                                     n = input$n_cells, mu = input$mean_expression, sigma = input$standard_deviation, type1 = input$alpha, dfactor= T, B = 100),
                                  
                                  )
 
  output$ncells <- renderText({
    
    paste("Power = ", as.numeric(power_reaction()))
  
  })
  
  output$plot <- renderPlot({
    
    data%>%
      filter(mu == input$mean_expression_plot)%>%
      filter(sigma == input$standard_deviation_plot)%>%
      filter(fold_change == input$fold_ch_plot)%>%
      mutate(n_cells = as.character(n_cells))%>%
      ggplot()+
      geom_line(mapping = aes(x = `frac_sub_pop (%)`, y = power, colour = n_cells))+
      ylim(c(0, 1))+
      labs(title = "Plot of Power by Fraction of Sub-Population", x = "Fraction of Sub-Population", y = "Power", colour = "Sample Size",
           caption = "For this plot, the fraction of marker genes is held constant at 1%, dropout rate is held constant at 60%, 
           \n alpha is held constant at 0.05, and number of unique genes included in the simulation is 17,000.")
    
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)

