#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(palmerpenguins)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(purrr)

# You can also run code once here (nonreactive)
min_mass <- min(penguins$body_mass_g, na.rm = TRUE)
max_mass <- max(penguins$body_mass_g, na.rm = TRUE)

quant_vars <- penguins |> keep(is.numeric)
cat_vars <- penguins |> keep(is.factor)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Exploring Sleep Data"),
  
  inputPanel(
    selectInput('xcol', label = 'X Variable', choices = colnames(quant_vars)),
    selectInput('ycol', label = 'Y Variable', choices = colnames(quant_vars), selected = colnames(quant_vars)[2]),
    selectInput('color', label = 'Color by', choices = colnames(cat_vars), selected = "species"),
    sliderInput('size', label = 'Point Size', min = 1, max = 10, value = 2),
    sliderInput('mass', label = 'Bounds for boday mass', 
                min = min_mass, 
                max = max_mass, 
                value = c(min_mass, max_mass)),
    checkboxInput('background', label = "Remove background", value = FALSE)
  ),
  
  # Show a plot of the selected relationship
  plotOutput(outputId = "scatterPlot"),
  plotOutput(outputId = "histogram")
)

# Define server logic required to draw a scatterplot
server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    p <- penguins |>
      filter(between(body_mass_g, input$mass[1], input$mass[2])) |>
      ggplot(aes(x = .data[[input$xcol]], y = .data[[input$ycol]], color = .data[[input$color]])) +
      geom_point(aes(shape = species), size = input$size) +
      scale_color_colorblind()
    if(input$background) {
      p <- p + theme_bw()
    }
    
    p
  })
  
  output$histogram <- renderPlot({
    ggplot(penguins, aes(x = .data[[input$xcol]], fill = .data[[input$color]])) +
      geom_histogram() +
      scale_fill_colorblind()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
