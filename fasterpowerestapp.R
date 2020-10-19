library(shiny)
library(quantmod)
library('devtools')

# Define UI ----
ui <- fluidPage(
  titlePanel(h1("Power Calculator for scRNA-seq Experiment")),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h3("Parameter Input"),
      
      sliderInput("frac_m_genes",
                  h5("Fraction of Marker Genes"),
                  min = 0.1, max = 10, value = 1),
      
      sliderInput("n_cells",
                  h5("Sample Size"),
                  min = 50, max = 2000, value = 100),
      
      sliderInput("percent_sub_pop",
                  h5("Percentage of Sub Population"),
                  min = 5, max = 50, value = 10),
      
      sliderInput("fold_ch",
                  h5("Fold Change of Sub Pop"),
                  min = 1, max = 10, value = 1, step = 0),
      
      sliderInput("drop_rate",  h5("Drop out Rate"),
                  min = 0.6, max = 0.999, value = 0.6),
      
      sliderInput("unique_gene_count",
                  h5("Unique Genes"),
                  min = 5000, max = 25000 ,value = 10000),
      
      sliderInput("mean_expression",
                  h5("Mean Gene Expression"),
                  min = 0.5, max = 10, value = 1.5),
      
      sliderInput("standard_deviation",
                  h5("Standard Deviation of Mean Expression"),
                  min = 0.5, max = 10, value = 2),
      
      sliderInput("alpha",  h5("Alpha"),
                  min = 0.01, max = 0.99, value = 0.05) 
      
    ),
    
    mainPanel( 
      
      h2("Power Estimate"),
      textOutput("n_cells"),
      textOutput("ncells")
      
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  output$n_cells <- renderText({
    paste("You have selected a sample size of", input$n_cells)
  })
  
  output$ncells <- renderText({
    
    powerEst <- ncells(m1 = input$frac_m_genes, pi1=input$percent_sub_pop, foldchange = input$fold_ch, dropout = input$drop_rate, p = input$unique_gene_count, 
                       n = input$n_cells, mu = input$mean_expression, sigma = input$standard_deviation, type1 = input$alpha, dfactor= TRUE, 
                       seed = seed_number, B = replicates, ncore = cores)
    
    paste("Power = ", powerEst)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

