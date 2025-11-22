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
min_dur_slp <- min(sleep_clean_name$sleep_duration, na.rm = TRUE)
max_dur_slp <- max(sleep_clean_name$sleep_duration, na.rm = TRUE)
quant_vars_slp <- sleep_clean_name |> keep(is.numeric)
cat_vars_slp <- sleep_clean_name |> keep(is.factor)

min_dur_std <- min(student_clean_name$sleep_hours, na.rm = TRUE)
max_dur_std <- max(student_clean_name$sleep_hours, na.rm = TRUE)
quant_vars_std <- student_clean_name |> keep(is.numeric)
cat_vars_std <- student_clean_name |> keep(is.factor)

min_dur_fct <- min(factors_clean_name$sleep_hours, na.rm = TRUE)
max_dur_fct <- max(factors_clean_name$sleep_hours, na.rm = TRUE)
quant_vars_fct <- factors_clean_name |> keep(is.numeric)
cat_vars_fct <- factors_clean_name |> keep(is.factor)

min_dur_f <- min(full_data_clean$sleep_duration, na.rm = TRUE)
max_dur_f <- max(full_data_clean$sleep_duration, na.rm = TRUE)
quant_vars_f <- full_data_clean |> keep(is.numeric)
cat_vars_f <- full_data_clean |> keep(is.factor)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Exploring Sleep Data"),
  
  # Sleep data
  inputPanel(
    selectInput('xcol_slp', label = 'X_slp Variable', choices = colnames(quant_vars_slp)),
    selectInput('ycol_slp', label = 'Y_slp Variable', choices = colnames(quant_vars_slp), selected = colnames(quant_vars_slp)[2]),
    # selectInput('color_slp', label = 'Color by', choices = colnames(cat_vars_slp)),
    sliderInput('size_slp', label = 'Point Size', min = 1, max = 10, value = 2),
    sliderInput('duration_slp', label = 'Bounds for sleep duration', 
                min = min_dur_slp, 
                max = max_dur_slp, 
                value = c(min_dur_slp, max_dur_slp)),
    checkboxInput('background_slp', label = "Remove background", value = FALSE)
  ),
  
  # Show a plot of the selected relationship
  plotOutput(outputId = "scatterPlot_slp"),
  plotOutput(outputId = "histogram_slp"),
  
  # ----------------------------------------------------------------
  
  # Student data
  inputPanel(
    selectInput('xcol_std', label = 'X_std Variable', choices = colnames(quant_vars_std)),
    selectInput('ycol_std', label = 'Y_std Variable', choices = colnames(quant_vars_std), selected = colnames(quant_vars_std)[2]),
    # selectInput('color_std', label = 'Color by', choices = colnames(cat_vars_std)),
    sliderInput('size_std', label = 'Point Size', min = 1, max = 10, value = 2),
    sliderInput('duration_std', label = 'Bounds for sleep hours', 
                min = min_dur_std, 
                max = max_dur_std, 
                value = c(min_dur_std, max_dur_std)),
    checkboxInput('background_std', label = "Remove background", value = FALSE)
  ),
  
  # Show a plot of the selected relationship
  plotOutput(outputId = "scatterPlot_std"),
  plotOutput(outputId = "histogram_std"),
  
  # ----------------------------------------------------------------
  
  # Factors data
  inputPanel(
    selectInput('xcol_fct', label = 'X_fct Variable', choices = colnames(quant_vars_fct)),
    selectInput('ycol_fct', label = 'Y_fct Variable', choices = colnames(quant_vars_fct), selected = colnames(quant_vars_fct)[2]),
    # selectInput('color_fct', label = 'Color by', choices = colnames(cat_vars_fct)),
    sliderInput('size_fct', label = 'Point Size', min = 1, max = 10, value = 2),
    sliderInput('duration_fct', label = 'Bounds for sleep duration', 
                min = min_dur_fct, 
                max = max_dur_fct, 
                value = c(min_dur_fct, max_dur_fct)),
    checkboxInput('background_fct', label = "Remove background", value = FALSE)
  ),
  
  # Show a plot of the selected relationship
  plotOutput(outputId = "scatterPlot_fct"),
  plotOutput(outputId = "histogram_fct"),
  
  # ----------------------------------------------------------------
  
  # Full cleaned data
  inputPanel(
    selectInput('xcol_f', label = 'X Variable', choices = colnames(quant_vars_f)),
    selectInput('ycol_f', label = 'Y Variable', choices = colnames(quant_vars_f), selected = colnames(quant_vars_f)[2]),
    # selectInput('color_f', label = 'Color by', choices = colnames(cat_vars_f)),
    sliderInput('size_f', label = 'Point Size', min = 1, max = 10, value = 2),
    sliderInput('duration_f', label = 'Bounds for sleep duration', 
                min = min_dur_f, 
                max = max_dur_f, 
                value = c(min_dur_f, max_dur_f)),
    checkboxInput('background', label = "Remove background", value = FALSE)
  ),
  
  # Show a plot of the selected relationship
  plotOutput(outputId = "scatterPlot"),
  plotOutput(outputId = "histogram")
)

# Define server logic required to draw a scatterplot
server <- function(input, output) {
  
  # Sleep data
  output$scatterPlot_slp <- renderPlot({
    p <- sleep_clean_name |>
      filter(between(sleep_duration, input$duration_slp[1], input$duration_slp[2])) |>
      ggplot(aes(x = .data[[input$xcol_slp]], y = .data[[input$ycol_slp]])) +
      geom_point(aes(), size = input$size_slp) +
      scale_color_colorblind()
    if(input$background_slp) {
      p <- p + theme_bw()
    }
    
    p
  })
  
  output$histogram_slp <- renderPlot({
    ggplot(sleep_clean_name, aes(x = .data[[input$xcol_slp]])) +
      geom_histogram() +
      scale_fill_colorblind()
  })
  
  # -------------------------------------------------------------------------------
  
  # Student data
  output$scatterPlot_std <- renderPlot({
    p <- student_clean_name |>
      filter(between(sleep_hours, input$duration_std[1], input$duration_std[2])) |>
      ggplot(aes(x = .data[[input$xcol_std]], y = .data[[input$ycol_std]])) +
      geom_point(aes(), size = input$size_std) +
      scale_color_colorblind()
    if(input$background_std) {
      p <- p + theme_bw()
    }
    
    p
  })
  
  output$histogram_std <- renderPlot({
    ggplot(student_clean_name, aes(x = .data[[input$xcol_std]])) +
      geom_histogram() +
      scale_fill_colorblind()
  })
  
  # -------------------------------------------------------------------------------
  
  # factors data
  output$scatterPlot_fct <- renderPlot({
    p <- factors_clean_name |>
      filter(between(sleep_hours, input$duration_fct[1], input$duration_fct[2])) |>
      ggplot(aes(x = .data[[input$xcol_fct]], y = .data[[input$ycol_fct]])) +
      geom_point(aes(), size = input$size_fct) +
      scale_color_colorblind()
    if(input$background_fct) {
      p <- p + theme_bw()
    }
    
    p
  })
  
  output$histogram_fct <- renderPlot({
    ggplot(factors_clean_name, aes(x = .data[[input$xcol_fct]])) +
      geom_histogram() +
      scale_fill_colorblind()
  })
  
  # -------------------------------------------------------------------------------
  
  # Full data
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
