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

suffixes <- c("slp", "std", "fct", "full")

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

panel_info <- list(
  "slp" = data_source[["slp"]] |> keep(is.numeric),
  "std" = data_source[["std"]] |> keep(is.numeric),
  "fct" = data_source[["fct"]] |> keep(is.numeric),
  "full" = data_source[["full"]] |> keep(is.numeric)
)

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

panel_info_text <- list(
  
  "slp" = tagList(
    tags$h5("Exploring Sleep Health and Stress"),
    tags$p("This tab uses the Sleep Health and Lifestyle dataset."),
    tags$p("Here you're mainly looking at sleep duration, sleep quality, and stress level, along with age and gender."),
    tags$ul(
      tags$li(HTML("X = <code>sleep_duration</code>, Y = <code>stress_level</code>: A negative correlation means that people who sleep more tend to report lower stress.")),
      tags$li(HTML("X = <code>sleep_duration</code>, Y = <code>sleep_quality</code>: A positive correlation suggests that more sleep is associated with better sleep quality."))
    ),
    tags$p(HTML("Use this tab to explore our first research question: <em>What do general sleep patterns look like, and how do sleep duration, quality, and stress relate to each other?</em>"))
  ),
  
  "std" = tagList(
    tags$h5("Exploring Sleep vs. Study Habits"),
    tags$p("This tab uses the Student Performance dataset."),
    tags$p("Key variables here include: `performance_index`, `previous_scores`, `hours_studied`, and `sleep_hours`."),
    tags$ul(
      tags$li(HTML("X = <code>sleep_hours</code>, Y = <code>performance_index</code>: Shows how nightly sleep relates to current academic performance. A positive slope suggests that students who sleep more tend to do a bit better.")),
      tags$li(HTML("X = <code>hours_studied</code>, Y = <code>performance_index</code>: Shows how study time relates to performance. A positive slope suggests “more study, slightly higher performance.”"))
    ),
    tags$p("The correlation summary helps compare relationships: a weak but positive correlation for sleep compared to a strong positive correlation for previous scores shows that sleep helps, but is not the only factor."),
    tags$p(HTML("This tab helps explore: <em>How are sleep and academic performance related, and how does sleep compare to study time and prior performance?</em>"))
  ),
  
  "fct" = tagList(
    tags$h5("Exploring Behavioral Factors and Exam Scores"),
    tags$p("This tab uses the Student Performance Factors dataset."),
    tags$p("Important variables include: `study_hours`, `sleep_hours`, and `score`."),
    tags$ul(
      tags$li(HTML("X = <code>study_hours</code>, Y = <code>score</code>: Shows how exam scores respond to increased study.")),
      tags$li(HTML("X = <code>sleep_hours</code>, Y = <code>score</code>: Shows whether students with more sleep tend to get higher scores."))
    ),
    tags$p("A moderate positive correlation between `study_hours` and `score` would support the idea that more study leads to better outcomes."),
    tags$p(HTML("This tab focuses on the behavioral side of our question: <em>How do sleep and study habits connect to exam performance?</em>"))
  ),
  
  "full" = tagList(
    tags$h5("Combined Summary by Sleep Hours"),
    tags$p("The Joined Dataset tab uses a combined dataset that aggregates all three sources by rounded sleep hours. For each sleep-hour value, we summarize:"),
    tags$ul(
      tags$li(HTML("Average performance index and previous scores.")),
      tags$li(HTML("Average exam scores and study hours.")),
      tags$li(HTML("Average sleep quality and stress levels."))
    ),
    tags$p(HTML("This tab supports our final story: <em>Students who sleep within the recommended range tend to show higher sleep quality, lower stress, and slightly better academic performance than short sleepers.</em>"))
  )
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
      
      layout_columns(
        # Left Chart and Correlation Summary
        tagList(
          tags$h4(paste("Scatterplot of Variables in", title)), 
          plotOutput(outputId = paste0("scatterPlot_", suffix)),
          tags$hr(),
          textOutput(outputId = paste0("correlation_summary_", suffix))
        ),
        
        # Right: Info Text
        tagList(
          tags$h4(paste("Introduction to ", title)),
          uiOutput(outputId = paste0("info_text_", suffix)) 
        ),
        
        col_widths = c(6, 6) 
      )
    )
  )
}


# All Panels
panel_specs <- list(
  list(title = "Sleep Health Data", suffix = "slp"),
  list(title = "Student Performance Data", suffix = "std"),
  list(title = "Student Performance Factors Data", suffix = "fct"),
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
  
  nav_panel(
    title = "Homepage",
    layout_sidebar(
      sidebar = NULL,
      tags$h1(HTML("<strong>Welcome!</strong>")),
      tags$p(
        "This app lets you explore how sleep, stress, study behavior, and academic performance are related. 
    Each tab shows one of the datasets we used in our project, and you can choose your own variables to plot on the x– and y–axes. 
    We decided to make a separate tab for the histogram distribution because we thought it tied directly into answering the major question that was lingering in our heads."
      ),
    )
  ),
  
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
  
  for (suffix in suffixes) {
    
    local({
      
      suf <- suffix
      
      # URL of the source datasets, and a placeholder for the created dataset
      
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
      
      output[[paste0("correlation_summary_", suf)]] <- renderText({
        req(input[[paste0("xcol_", suf)]], input[[paste0("ycol_", suf)]])
        data <- filtered_data[[suf]]()
        
        x_var <- data[[input[[paste0("xcol_", suf)]]]]
        y_var <- data[[input[[paste0("ycol_", suf)]]]]
        
        cor(x_var, y_var, method = "pearson", use = "complete.obs") |> generate_correlation_summary()
      })
      
      #---------------------------------------------------------------------------
      
      # Scatterplot of the 2 variables
      
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
      
      #---------------------------------------------------------------------------
      
      # Information about the panel
      
      output[[paste0("info_text_", suf)]] <- renderUI({
        panel_info_text[[suf]]
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