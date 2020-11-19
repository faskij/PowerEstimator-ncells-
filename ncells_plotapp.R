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
  titlePanel(h1("Power Plot for scRNA-seq Experiment")),
  plotOutput("plot"),

  sidebarLayout(
    
    sidebarPanel(
      
      h3("Parameter Input"),
      
    
      
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
    
    mainPanel( 
      
      h2("Power Estimate"),
      textOutput("n_cells"),
      textOutput("ncells")
      #tableOutput("ncells")
    )
  )
 
)

# Define server logic ----
server <- function(input, output) {
  output$plot <- renderPlot({
    
    data%>%
      filter(mu == input$mean_expression_plot)%>%
      filter(sigma == input$standard_deviation_plot)%>%
      filter(fold_change == input$fold_ch_plot)%>%
      mutate(n_cells = as.character(n_cells))%>%
      ggplot()+
      geom_line(mapping = aes(x = `frac_sub_pop (%)`, y = power, colour = n_cells))+
      ylim(c(0, 1))
    
  })
 
}

# Run the app ----
shinyApp(ui = ui, server = server)
