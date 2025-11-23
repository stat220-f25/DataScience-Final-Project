# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(palmerpenguins)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(purrr)

# You can also run code once here (nonreactive)
x_input_label <- "Plot-X"
y_input_label <- "Scatterplot-Y"

quant_vars_slp <- sleep_clean_name |> keep(is.numeric)
cat_vars_slp <- sleep_clean_name |> keep(is.factor)

quant_vars_std <- student_clean_name |> keep(is.numeric)
cat_vars_std <- student_clean_name |> keep(is.factor)

quant_vars_fct <- factors_clean_name |> keep(is.numeric)
cat_vars_fct <- factors_clean_name |> keep(is.factor)

quant_vars_f <- full_data_clean |> keep(is.numeric)
cat_vars_f <- full_data_clean |> keep(is.factor)


# Define UI for application that draws a histogram
ui <- page_navbar(
  title = "Exploring Sleep Data",
  bg = "#2D89C8",
  inverse = TRUE,
  
  
  
  nav_panel(
    title = "Sleep Dataset", 
    layout_sidebar(
      sidebar = sidebar(
        selectInput('xcol_slp', label = x_input_label, choices = colnames(quant_vars_slp)),
        selectInput('ycol_slp', label = y_input_label, choices = colnames(quant_vars_slp), selected = colnames(quant_vars_slp)[2]),
        
        uiOutput("x_range_slider_slp"),
        
        sliderInput('size_slp', label = 'Point Size', min = 1, max = 10, value = 2),
        checkboxInput('scatterline_slp', label = "Add best fit line", value = FALSE),
        checkboxInput('scattercurve_slp', label = "Add best fit curve", value = FALSE),
        checkboxInput('background_slp', label = "Remove background", value = FALSE)
      ),
      textOutput(outputId = "correlation_summary_slp"),
      plotOutput(outputId = "scatterPlot_slp")
    )
    
  ),
  
  nav_panel(
    title = "Student Dataset", 
    layout_sidebar(
      sidebar = sidebar(
        selectInput('xcol_std', label = x_input_label, choices = colnames(quant_vars_std)),
        selectInput('ycol_std', label = y_input_label, choices = colnames(quant_vars_std), selected = colnames(quant_vars_std)[2]),
        
        uiOutput("x_range_slider_std"),
        
        sliderInput('size_std', label = 'Point Size', min = 1, max = 10, value = 2),
        checkboxInput('scatterline_std', label = "Add best fit line", value = FALSE),
        checkboxInput('scattercurve_std', label = "Add best fit curve", value = FALSE),
        checkboxInput('background_std', label = "Remove background", value = FALSE)
      ),
      textOutput(outputId = "correlation_summary_std"),
      plotOutput(outputId = "scatterPlot_std")
    )
    
  ),
  
  nav_panel(
    title = "Factors Dataset", 
    layout_sidebar(
      sidebar = sidebar(
        selectInput('xcol_fct', label = x_input_label, choices = colnames(quant_vars_fct)),
        selectInput('ycol_fct', label = y_input_label, choices = colnames(quant_vars_fct), selected = colnames(quant_vars_fct)[2]),
        
        uiOutput("x_range_slider_fct"),
        
        sliderInput('size_fct', label = 'Point Size', min = 1, max = 10, value = 2),
        checkboxInput('scatterline_fct', label = "Add best fit line", value = FALSE),
        checkboxInput('scattercurve_fct', label = "Add best fit curve", value = FALSE),
        checkboxInput('background_fct', label = "Remove background", value = FALSE)
      ),
      textOutput(outputId = "correlation_summary_fct"),
      plotOutput(outputId = "scatterPlot_fct")
    )
    
  ),
  
  nav_panel(
    title = "Joined Dataset", 
    layout_sidebar(
      sidebar = sidebar(
        selectInput('xcol_f', label = x_input_label, choices = colnames(quant_vars_f)),
        selectInput('ycol_f', label = y_input_label, choices = colnames(quant_vars_f), selected = colnames(quant_vars_f)[2]),
        
        uiOutput("x_range_slider_f"),
        
        sliderInput('size_f', label = 'Point Size', min = 1, max = 10, value = 2),
        checkboxInput('scatterline_f', label = "Add best fit line", value = FALSE),
        checkboxInput('scattercurve_f', label = "Add best fit curve", value = FALSE),
        checkboxInput('background_f', label = "Remove background", value = FALSE)
      ),
      textOutput(outputId = "correlation_summary_f"),
      plotOutput(outputId = "scatterPlot_f")
    )
    
  ),
  
  nav_panel(
    title = "Sleep Hours Distribution",
    layout_columns(
      plotOutput(outputId = "sleep_histogram"),
      textOutput(outputId = "sleep_hours_analysis"),
      col_widths = c(6, 6) # Assign relative widths
    )
    
  )
    
  
  #nav_spacer(),
  #nav_menu(
  #  title = "Links",
  #  align = "right",
  #  nav_item(tags$a("Posit", href = "https://posit.co")),
  #  nav_item(tags$a("Shiny", href = "https://shiny.posit.co"))
  #)
)


# Define server logic required to draw a scatterplot
server <- function(input, output) {
  # General
  
  generate_correlation_summary <- function(r) {
    if (is.na(r) || is.null(r) || is.infinite(r)) {
      return("Correlation could not be calculated (data may be constant or insufficient).")
    }
    
    direction <- if (r > 0) "positive" else "negative"
    
    abs_r <- abs(r)
    strength <- case_when(
      abs_r >= 0.7 ~ "strong",
      abs_r >= 0.5 ~ "moderate",
      abs_r >= 0.3 ~ "weak",
      abs_r >= 0.1 ~ "very weak",
      TRUE ~ "negligible"
    )
    
    return(sprintf("These variables has a %s %s relationship (r = %.2f).", strength, direction, r))
  }
  
  #----------------------------------------------------------------------------------
  
  # Sleep Data
  output$x_range_slider_slp <- renderUI({
    selected_x_var_slp <- input$xcol_slp
    data_vector <- sleep_clean_name[[selected_x_var_slp]]
    
    min_val <- min(data_vector, na.rm = TRUE)
    max_val <- max(data_vector, na.rm = TRUE)
    
    sliderInput(
      inputId = "x_range_slp",
      label = paste("Bounds for", selected_x_var_slp),
      min = min_val,
      max = max_val,
      value = c(min_val, max_val)
    )
  })
  
  filtered_data_slp <- reactive({
    req(input$xcol_slp, input$x_range_slp)
    sleep_clean_name |>
      filter(between(.data[[input$xcol_slp]], input$x_range_slp[1], input$x_range_slp[2]))
  })
  
  output$correlation_summary_slp <- renderText({
    req(input$xcol_slp, input$ycol_slp)
    data <- filtered_data_slp()
    
    x_var <- data[[input$xcol_slp]]
    y_var <- data[[input$ycol_slp]]
    
    cor(x_var, y_var, method = "pearson", use = "complete.obs") |> generate_correlation_summary()
  })
  
  output$scatterPlot_slp <- renderPlot({
    req(input$xcol_slp, input$x_range_slp, input$ycol_slp)
    
    p <- filtered_data_slp() |>
      ggplot(aes(x = .data[[input$xcol_slp]], y = .data[[input$ycol_slp]])) +
      geom_point(aes(), size = input$size_slp) +
      scale_color_colorblind()
    if(input$background_slp) {
      p <- p + theme_bw()
    }
    if(input$scatterline_slp) {
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "darkblue")
    }
    if(input$scattercurve_slp) {
      p <- p + geom_smooth(se = TRUE, color = "red")
    }
    
    p
  })
  
  # -------------------------------------------------------------------------------
  
  # Student data
  output$x_range_slider_std <- renderUI({
    selected_x_var_std <- input$xcol_std
    data_vector <- student_clean_name[[selected_x_var_std]]
    
    min_val <- min(data_vector, na.rm = TRUE)
    max_val <- max(data_vector, na.rm = TRUE)
    
    sliderInput(
      inputId = "x_range_std",
      label = paste("Bounds for", selected_x_var_std),
      min = min_val,
      max = max_val,
      value = c(min_val, max_val)
    )
  })
  
  filtered_data_std <- reactive({
    req(input$xcol_std, input$x_range_std)
    student_clean_name |>
      filter(between(.data[[input$xcol_std]], input$x_range_std[1], input$x_range_std[2]))
  })
  
  output$correlation_summary_std <- renderText({
    req(input$xcol_std, input$ycol_std)
    data <- filtered_data_std()
    
    x_var <- data[[input$xcol_std]]
    y_var <- data[[input$ycol_std]]
    
    cor(x_var, y_var, method = "pearson", use = "complete.obs") |> generate_correlation_summary()
  })
  
  output$scatterPlot_std <- renderPlot({
    req(input$xcol_std, input$x_range_std, input$ycol_std)
    
    p <- filtered_data_std() |>
      ggplot(aes(x = .data[[input$xcol_std]], y = .data[[input$ycol_std]])) +
      geom_point(aes(), size = input$size_std) +
      scale_color_colorblind()
    if(input$background_std) {
      p <- p + theme_bw()
    }
    if(input$scatterline_std) {
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "darkblue")
    }
    if(input$scattercurve_std) {
      p <- p + geom_smooth(se = TRUE, color = "red")
    }
    
    p
  })
  
  # -------------------------------------------------------------------------------
  
  # factors data
  output$x_range_slider_fct <- renderUI({
    selected_x_var_fct <- input$xcol_fct
    data_vector <- factors_clean_name[[selected_x_var_fct]]
    
    min_val <- min(data_vector, na.rm = TRUE)
    max_val <- max(data_vector, na.rm = TRUE)
    
    sliderInput(
      inputId = "x_range_fct",
      label = paste("Bounds for", selected_x_var_fct),
      min = min_val,
      max = max_val,
      value = c(min_val, max_val)
    )
  })
  
  filtered_data_fct <- reactive({
    req(input$xcol_fct, input$x_range_fct)
    factors_clean_name |>
      filter(between(.data[[input$xcol_fct]], input$x_range_fct[1], input$x_range_fct[2]))
  })
  
  output$correlation_summary_fct <- renderText({
    req(input$xcol_fct, input$ycol_fct)
    data <- filtered_data_fct()
    
    x_var <- data[[input$xcol_fct]]
    y_var <- data[[input$ycol_fct]]
    
    cor(x_var, y_var, method = "pearson", use = "complete.obs") |> generate_correlation_summary()
  })
  
  output$scatterPlot_fct <- renderPlot({
    req(input$xcol_fct, input$x_range_fct, input$ycol_fct)
    
    p <- filtered_data_fct() |>
      ggplot(aes(x = .data[[input$xcol_fct]], y = .data[[input$ycol_fct]])) +
      geom_point(aes(), size = input$size_fct) +
      scale_color_colorblind()
    if(input$background_fct) {
      p <- p + theme_bw()
    }
    if(input$scatterline_fct) {
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "darkblue")
    }
    if(input$scattercurve_fct) {
      p <- p + geom_smooth(se = TRUE, color = "red")
    }
    
    p
  })
  
  # -------------------------------------------------------------------------------
  
  # Full data
  output$x_range_slider_f <- renderUI({
    selected_x_var_f <- input$xcol_f
    data_vector <- full_data_clean[[selected_x_var_f]]
    
    min_val <- min(data_vector, na.rm = TRUE)
    max_val <- max(data_vector, na.rm = TRUE)
    
    sliderInput(
      inputId = "x_range_f",
      label = paste("Bounds for", selected_x_var_f),
      min = min_val,
      max = max_val,
      value = c(min_val, max_val)
    )
  })
  
  filtered_data_f <- reactive({
    req(input$xcol_f, input$x_range_f)
    full_data_clean |>
      filter(between(.data[[input$xcol_f]], input$x_range_f[1], input$x_range_f[2]))
  })
  
  output$correlation_summary_f <- renderText({
    req(input$xcol_f, input$ycol_f)
    data <- filtered_data_f()
    
    x_var <- data[[input$xcol_f]]
    y_var <- data[[input$ycol_f]]
    
    cor(x_var, y_var, method = "pearson", use = "complete.obs") |> generate_correlation_summary()
  })
  
  output$scatterPlot_f <- renderPlot({
    req(input$xcol_f, input$x_range_f, input$ycol_f)
    
    p <- filtered_data_f() |>
      ggplot(aes(x = .data[[input$xcol_f]], y = .data[[input$ycol_f]])) +
      geom_point(aes(), size = input$size_f) +
      scale_color_colorblind()
    if(input$background_f) {
      p <- p + theme_bw()
    }
    if(input$scatterline_f) {
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "darkblue")
    }
    if(input$scattercurve_f) {
      p <- p + geom_smooth(se = TRUE, color = "red")
    }
    
    p
  })
  
  #---------------------------------------------------------------------
  
  # Histogram of decimal sleep values
  
  output$sleep_histogram <- renderPlot({
    ggplot(sleep, aes(sleep_duration)) +
      geom_histogram(bins = 20) +
      theme_minimal() +
      labs(
        title = "Distribution of Sleep Duration",
        x = "Sleep hours",
        y = "Count"
      )
  })
  
  output$sleep_hours_analysis <- renderText({
    "test test"
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
