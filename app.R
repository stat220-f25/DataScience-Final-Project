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
library(rlang)

# You can also run code once here (nonreactive)
x_input_label <- "X Variable"
y_input_label <- "Y Variable"

data_source <- list("slp" = sleep_final_data, 
                    "std" = student_final_data, 
                    "fct" = factors_final_data, 
                    "full" = full_data_clean)

quant_vars <- list(
  "slp" = data_source[["slp"]] |> keep(is.numeric),
  "std" = data_source[["std"]] |> keep(is.numeric),
  "fct" = data_source[["fct"]] |> keep(is.numeric),
  "full" = data_source[["full"]] |> keep(is.numeric)
)

create_data_panel <- function(title, suffix) {
  
  # suffix for output/input IDs and for indexing quant_vars
  nav_panel(
    title = title,
    layout_sidebar(
      sidebar = sidebar(
        uiOutput(paste0("url_", suffix)),
        selectInput(paste0('xcol_', suffix), label = x_input_label, 
                    choices = colnames(quant_vars[[suffix]])),
        selectInput(paste0('ycol_', suffix), label = y_input_label, 
                    choices = colnames(quant_vars[[suffix]]), 
                    selected = colnames(quant_vars[[suffix]])[2]),
        
        uiOutput(paste0("x_range_slider_", suffix)),
        
        sliderInput(paste0('size_', suffix), label = 'Point Size', min = 1, max = 10, value = 2),
        checkboxInput(paste0('scatterline_', suffix), label = "Add best fit line", value = FALSE),
        checkboxInput(paste0('scattercurve_', suffix), label = "Add best fit curve", value = FALSE),
        checkboxInput(paste0('background_', suffix), label = "Remove background", value = FALSE)
      ),
      
      textOutput(outputId = paste0("correlation_summary_", suffix)),
      plotOutput(outputId = paste0("scatterPlot_", suffix))
    )
  )
}


# All Panels
panel_specs <- list(
  list(title = "Raw Sleep Health Data", suffix = "slp"),
  list(title = "Raw Student Performance Data", suffix = "std"),
  list(title = "Raw Student Performance Factors Data", suffix = "fct"),
  list(title = "Joined and Curated Data", suffix = "full")
)

scatterplot_panels <- lapply(panel_specs, function(spec) {
  create_data_panel(
    title = spec$title,
    suffix = spec$suffix
  )
})

# Define UI for application that draws a histogram
ui <- page_navbar(
  title = "Exploring Sleep Data",
  bg = "#2D89C8",
  inverse = TRUE,
  
  !!!scatterplot_panels,
  
  nav_panel(
    title = "Sleep Hours Distribution",
    layout_columns(
      plotOutput(outputId = "sleep_histogram"),
      textOutput(outputId = "sleep_hours_analysis"),
      col_widths = c(6, 6) # Assign relative widths
    )
    
  ),
  
  nav_panel(
    title = "Analysis", 
    mainPanel(
      
      tags$h2("Introduction"),
      tags$p("Sleep is one of the first things students sacrifice when school gets busy, but it plays a major role in stress, focus, and academic performance. Late-night studying and inconsistent routines often create a 'night-owl' pattern that feels normal - but might carry hidden costs. In this project, we explore how sleep duration relates to sleep quality, stress levels, study habits, and academic outcomes. To do this, we use three real datasets obtained from Kaggle: the Sleep Health and Lifestyle dataset, which includes sleep duration, sleep quality, stress, age, and gender; the Student Performance dataset, which contains performance index, previous scores, study time, and reported sleep hours; and the Student Performance Factors dataset, which adds information on exam scores, study hours, and additional behavioral variables."),
      tags$p("By analyzing these datasets individually and together, we look for consistent patterns connecting sleep to academic performance and well-being. Our visualizations, summaries, and interactive Shiny app help us ask whether students who sleep more report lower stress, whether sleep quality improves with longer sleep, and whether better sleep is linked to stronger academic outcomes. Although sleep is not the only factor shaping performance, our results suggest that adequate sleep is associated with better well-being and slightly higher academic achievement. This helped us highlight the importance of healthy sleep habits in a student’s daily life."),
      
      tags$h2("Research Questions"),
      tags$p("We focused on how sleeping relates to stress and academic performance using three datasets on sleep and student outcomes. Our project addresses three questions. The first one was: What do general sleep patterns look like in the Sleep Health and Lifestyle data? This entails sub-questions such as: How much do people sleep on average? How are sleep duration, sleep quality, and stress levels related? The second question was: How are sleep and academic performance related in student-level data? This also covers sub-questions like: Is more sleep associated with higher performance index and exam scores? How does sleep compare to study time in predicting performance? And the last question was: How do sleep and study behaviors interact? This also had sub-questions like: Do students who sleep more also study more (or are they more efficient)? Are there 'trade-offs' between sleep, stress, and performance?"),
      
      tags$h2("Analysis"),
      tags$p("After looking around, we have discovered a few trends in the data:"),
      tags$p("The scatterplot of sleep duration vs. sleep quality from the Sleep Health and Lifestyle Dataset shows a clear positive relationship: as sleep duration increases, average sleep quality rises. The regression line slopes upward, suggesting that, on average, sleeping more is associated with better-reported sleep quality. In contrast, sleep duration vs. stress level displays a negative relationship: the regression line slopes downward, indicating that individuals who sleep less tend to report higher stress."),
      tags$p("In the Student Performance dataset, we see a weak positive relationship between hours studied and performance index. As hours studied increase, the regression line rises, indicating that students who study more tend to achieve slightly higher performance scores, although there is substantial variability at each study level. The plot of sleep hours vs. performance index shows a very slight positive slope. Performance does not dramatically jump for any particular sleep value, but students with more sleep tend to have marginally higher performance. This suggests that sleep is beneficial but not the only driver of grades. When we examine previous scores vs. performance index, higher previous scores are associated with higher current performance. This is consistent with the idea that prior academic preparation and ability play a large role in current outcomes. Overall, this dataset suggests that both study time and prior performance are important predictors of academic outcomes, while sleep hours have a small but favorable association with performance."),
      tags$p("In the Student Performance Factors dataset, study hours vs. exam score shows a generally positive association: as study hours increase, the regression line slopes upward. The effect is not huge, and scores vary a lot at each study level, but the pattern is consistent with the idea that more study is weakly linked to better exam performance. The sleep hours vs. exam score plot shows a nearly flat line with a very small slope. Exam scores do not change dramatically across the observed range of sleep hours. This suggests that, in this particular dataset, study time has a more obvious direct relationship with scores than sleep does, although sleep might still matter indirectly through stress, focus, or consistency."),
      
      tags$p("To link the three datasets (student, factors, and sleep), we aggregated each one by rounded sleep hours and then joined the summaries."),
      tags$ul(
        tags$li(HTML("Student Dataset: Average performance index and average previous scores.")),
        tags$li(HTML("Factors Dataset: Average exam score and average study hours.")),
        tags$li(HTML("Sleep Dataset: Average sleep quality and average stress level."))
      ),
      
      tags$p("Across these combined summaries, several patterns emerge:"),
      
      tags$ul(
        tags$li(HTML("Academic Performance: The average performance index is slightly higher for students sleeping closer to the recommended 7–8 hours. The differences are not huge, but there is no evidence that very short sleep improves performance.")),
        tags$li(HTML("Stable Factors: Average previous scores and average study hours do not change dramatically across sleep levels, reinforcing that study habits and prior preparation are relatively stable, while sleep is an additional layer on top of those behaviors.")),
        tags$li(HTML("Exam Scores: Average exam scores vary only modestly with sleep hours, again showing small but generally positive effects of sufficient sleep.")),
        tags$li(HTML("Well-being Metrics: On the sleep side, average sleep quality rises and average stress level falls as sleep hours increase from short to recommended levels."))
      ),
      
      tags$h2("Takeaway"),
      tags$p("Altogether, this suggests that sleep is not a magic bullet that determines academic outcomes by itself. However, students who sleep within the recommended range tend to experience:"),
      tags$ul(
        tags$li(HTML("Better sleep quality")),
        tags$li(HTML("Lower stress")),
        tags$li(HTML("Slightly better academic performance than short sleepers."))
      ),
      tags$p("This supports the idea that having consistently short sleep may come with subtle but meaningful academic and well-being costs.")
      
    )
  ),
  
  nav_panel(
    title = "Citations", 
    mainPanel(
      p("Tharmalingam, L. (2023). Sleep Health and Lifestyle Dataset. Retrieved from https://www.kaggle.com/datasets/uom190346a/sleep-health-and-lifestyle-dataset"),
      p("Narayan, N. (2023). Student Performance (Multiple Linear Regression). Retrieved from https://www.kaggle.com/datasets/nikhil7280/student-performance-multiple-linear-regression"),
      p("Ng., L. (Aug 2025). Student Performance Factors. Retrieved from https://www.kaggle.com/datasets/lainguyn123/student-performance-factor"),
      p("Posit. (Jan 10, 2024). Application layout guide. Retrieved from https://shiny.posit.co/r/articles/build/layout-guide/"),
      p("DeanAttali. (Feb 5, 2017). Create URL hyperlink in R Shiny?. Retrieved from https://stackoverflow.com/a/42048943"),
      p("GeekForGeeks (Jul 23, 2025). apply(), lapply(), sapply(), and tapply() in R. Retrieved from https://www.geeksforgeeks.org/r-language/apply-lapply-sapply-and-tapply-in-r/")
    )
  )
    
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
  
  # Loop for the 4 panels
  suffixes <- c("slp", "std", "fct", "full")
  
  url_names <- list("slp" = "Sleep Health and Lifestyle Dataset", 
                    "std" = "Student Performance (Multiple Linear Regression)", 
                    "fct" = "Student Performance Factors", 
                    "full" = "Joined and Curated Dataset")
  
  url_refs <- list("slp" = "https://www.kaggle.com/datasets/uom190346a/sleep-health-and-lifestyle-dataset", 
                    "std" = "https://www.kaggle.com/datasets/nikhil7280/student-performance-multiple-linear-regression", 
                    "fct" = "https://www.kaggle.com/datasets/lainguyn123/student-performance-factors", 
                    "full" = "")
  
  filtered_data <- list("slp" = NULL, 
                        "std" = NULL, 
                        "fct" = NULL, 
                        "full" = NULL)
  
  for (suffix in suffixes) {
    
    local({
      
      suf <- suffix
      
      # URL of the source datasets, and a placeholder for the created dataset
      # Example of non-loop code:
      # output$url_slp <- renderUI({
      #   tagList(a("Sleep Health and Lifestyle Dataset", href="https://www.kaggle.com/datasets/uom190346a/sleep-health-and-lifestyle-dataset"))
      # })
      output[[paste0("url_", suf)]] <- renderUI({
        if (nchar(url_refs[[suf]]) > 0) {
          tagList(
            a(url_names[[suf]], 
              href = url_refs[[suf]], 
              target = "_blank")
          )
        } else {
          tagList(
            tags$p(url_names[[suf]])
          )
        }
      })
      
      #---------------------------------------------------------------------------
      
      # Slider for the x variable, can't do y due to risk of no datapoint within the double bound
      # Example code:
      # output$x_range_slider_slp <- renderUI({
      #   selected_x_var_slp <- input$xcol_slp
      #   data_vector <- sleep_clean_name[[selected_x_var_slp]]
      #   
      #   min_val <- min(data_vector, na.rm = TRUE)
      #   max_val <- max(data_vector, na.rm = TRUE)
      #   
      #   sliderInput(
      #     inputId = "x_range_slp",
      #     label = paste("Bounds for", selected_x_var_slp),
      #     min = min_val,
      #     max = max_val,
      #     value = c(min_val, max_val)
      #   )
      # })
      
      output[[paste0("x_range_slider_", suf)]] <- renderUI({
        selected_x_var <- input[[paste0("xcol_", suf)]]
        data_vector <- data_source[[suf]][[selected_x_var]]
        
        min_val <- min(data_vector, na.rm = TRUE)
        max_val <- max(data_vector, na.rm = TRUE)
        
        sliderInput(
          inputId = paste0("x_range_", suf),
          label = paste("Bounds for", selected_x_var),
          min = min_val,
          max = max_val,
          value = c(min_val, max_val)
        )
      })
      
      #---------------------------------------------------------------------------
      
      # Filtered dataset based on the range input
      # Example code:
      # filtered_data_slp <- reactive({
      #   req(input$xcol_slp, input$x_range_slp)
      #
      #   x_col_name <- input$xcol_slp
      #   x_min <- input$x_range_slp[1]
      #   x_max <- input$x_range_slp[2]
      #
      #   sleep_clean_name |>
      #     filter(between(.data[[x_col_name]], x_min, x_max))
      # })
      
      filtered_data[[suf]] <- reactive({
        required_xcol <- input[[paste0("xcol_", suf)]]
        required_xrange <- input[[paste0("x_range_", suf)]]
        req(required_xcol, required_xrange)
        
        x_col_name <- required_xcol
        x_min <- required_xrange[1]
        x_max <- required_xrange[2]
        
        data_source[[suf]] |>
          filter(between(.data[[x_col_name]], x_min, x_max))
      })
      
      #---------------------------------------------------------------------------
      
      # Correlation of the 2 currently selected variables of the current panel
      # Example code:
      # output$correlation_summary_slp <- renderText({
      #   req(input$xcol_slp, input$ycol_slp)
      #   data <- filtered_data_slp()
      #   
      #   x_var <- data[[input$xcol_slp]]
      #   y_var <- data[[input$ycol_slp]]
      #   
      #   cor(x_var, y_var, method = "pearson", use = "complete.obs") |> generate_correlation_summary()
      # })
      
      output[[paste0("correlation_summary_", suf)]] <- renderText({
        req(input[[paste0("xcol_", suf)]], input[[paste0("ycol_", suf)]])
        data <- filtered_data[[suf]]()
        
        x_var <- data[[input[[paste0("xcol_", suf)]]]]
        y_var <- data[[input[[paste0("ycol_", suf)]]]]
        
        cor(x_var, y_var, method = "pearson", use = "complete.obs") |> generate_correlation_summary()
      })
      
      #---------------------------------------------------------------------------
      
      # Scatterplot of the 2 variables
      # Example Code:
      # output$scatterPlot_slp <- renderPlot({
      #   req(input$xcol_slp, input$x_range_slp, input$ycol_slp)
      #   
      #   p <- filtered_data_slp() |>
      #     ggplot(aes(x = .data[[input$xcol_slp]], y = .data[[input$ycol_slp]])) +
      #     geom_point(aes(), size = input$size_slp) +
      #     scale_color_colorblind()
      #   if(input$background_slp) {
      #     p <- p + theme_bw()
      #   }
      #   if(input$scatterline_slp) {
      #     p <- p + geom_smooth(method = "lm", se = TRUE, color = "darkblue")
      #   }
      #   if(input$scattercurve_slp) {
      #     p <- p + geom_smooth(se = TRUE, color = "red")
      #   }
      #   
      #   p
      # })
      
      output[[paste0("scatterPlot_", suf)]] <- renderPlot({
        req(input[[paste0("xcol_", suf)]], input[[paste0("x_range_", suf)]], input[[paste0("ycol_", suf)]])
        
        p <- filtered_data[[suf]]() |>
          ggplot(aes(x = .data[[input[[paste0("xcol_", suf)]]]], y = .data[[input[[paste0("ycol_", suf)]]]])) +
          geom_point(aes(), size = input[[paste0("size_", suf)]]) +
          scale_color_colorblind()
        if(input[[paste0("background_", suf)]]) {
          p <- p + theme_bw()
        }
        if(input[[paste0("scatterline_", suf)]]) {
          p <- p + geom_smooth(method = "lm", se = TRUE, color = "darkblue")
        }
        if(input[[paste0("scattercurve_", suf)]]) {
          p <- p + geom_smooth(se = TRUE, color = "red")
        }
        
        p
      })
      
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
  
  #---------------------------------------------------------------------
  
  # Histogram analysis
  
  output$sleep_hours_analysis <- renderText({
    " This tab gives context for the other views: 
    The histogram of `sleep_duration` shows how common different sleep amounts are in the Sleep Health dataset.
    The accompanying text explains the main pattern: most individuals are clustered around 6 - 8 hours of sleep, 
with relatively few people at Very short or very long sleep.

Use this tab as a starting point to understand what “typical” sleep looks like before digging into relationships in the other tabs."
  })
  
  }
}

# Run the application 
shinyApp(ui = ui, server = server)