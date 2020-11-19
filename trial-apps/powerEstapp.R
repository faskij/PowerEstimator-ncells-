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
      
      numericInput("n_cells",
                   h5("Sample Size"),
                   value = 100),
      
      sliderInput("percent_sub_pop",
                  h5("Percentage of Sub Population"),
                  min = 5, max = 50, value = 5),
      
      sliderInput("fold_ch",
                  h5("Fold Change of Sub Pop"),
                  min = 1, max = 32, value = 1),
      
      sliderInput("drop_rate",  h5("Drop out Rate"),
                  min = 0.6, max = 0.999, value = 0.6),
      
      numericInput("unique_gene_count",
                   h5("Unique Genes"),
                   value = 20000),
      
      numericInput("mean_expression",
                   h5("Mean Gene Expression"),
                   value = 1),
      
      numericInput("standard_deviation",
                   h5("Standard Deviation of Mean Expression"),
                   value = 2),
      
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
    
    frac_m_genes <- input$frac_m_genes
    
    percent_sub_pop <- input$percent_sub_pop
    
    fold_ch <- input$fold_ch
    
    drop_rate <- input$drop_rate
    
    unique_gene_count <- input$unique_gene_count
    
    n_cells <- input$n_cells
    
    mean_expression <- input$mean_expression
    
    standard_deviation <- input$standard_deviation
    
    
    powerEst <- ncells(m1 = frac_m_genes, pi1=percent_sub_pop, foldchange = fold_ch, dropout = drop_rate, p = unique_gene_count, n = n_cells, 
                       mu = mean_expression, sigma = standard_deviation, type1 = 0.05, dfactor= TRUE)
    
    paste("Power = ", powerEst)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
