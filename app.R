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
min_dur <- min(full_data_clean$sleep_duration, na.rm = TRUE)
max_dur <- max(full_data_clean$sleep_duration, na.rm = TRUE)

quant_vars <- full_data_clean |> keep(is.numeric)
cat_vars <- full_data_clean |> keep(is.factor)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Exploring Sleep Data"),
  
  inputPanel(
    selectInput('xcol_f', label = 'X Variable', choices = colnames(quant_vars)),
    selectInput('ycol_f', label = 'Y Variable', choices = colnames(quant_vars), selected = colnames(quant_vars)[2]),
    # selectInput('color_f', label = 'Color by', choices = colnames(cat_vars)),
    sliderInput('size_f', label = 'Point Size', min = 1, max = 10, value = 2),
    sliderInput('duration_f', label = 'Bounds for sleep duration', 
                min = min_dur, 
                max = max_dur, 
                value = c(min_dur, max_dur)),
    checkboxInput('background', label = "Remove background", value = FALSE)
  ),
  
  # Show a plot of the selected relationship
  plotOutput(outputId = "scatterPlot"),
  plotOutput(outputId = "histogram")
)

# Define server logic required to draw a scatterplot
server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    p <- full_data_clean |>
      filter(between(sleep_duration, input$duration_f[1], input$duration_f[2])) |>
      ggplot(aes(x = .data[[input$xcol_f]], y = .data[[input$ycol_f]])) +
      geom_point(aes(), size = input$size_f) +
      scale_color_colorblind()
    if(input$background) {
      p <- p + theme_bw()
    }
    
    p
  })
  
  output$histogram <- renderPlot({
    ggplot(full_data_clean, aes(x = .data[[input$xcol_f]])) +
      geom_histogram() +
      scale_fill_colorblind()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
