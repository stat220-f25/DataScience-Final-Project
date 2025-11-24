# Final Portfolio Project - Stat 220

**Authors:** Seth Bonney and Cuong Tran  
**Title:** Academic Impact of Being a Night Owl

## Project Overview
This project investigates how sleeping hours relates to stress, sleep quality, and academic performance. 
We combine three datasets on sleep and student performance to answer questions such as:

- What do sleep patterns look like in our sample?
- How are sleep duration, sleep quality, and stress connected?
- How are sleep, study time, and exam performance related?
Our goal is to use real data to tell a clear story about why sleep matters for students.

## Data
We use three CSV datasets:

1. `Sleep_health_and_lifestyle_dataset.csv`  
   - Sleep duration, sleep quality, stress level, age, gender.
   
2. `Student_Performance.csv`  
   - Performance index, previous scores, hours studied, sleep hours.
   
3. `StudentPerformanceFactors.csv`  
   - Study hours, sleep hours, exam scores, and related factors.

All data files are stored in the [`Data/`] folder.

## Repository Structure
- `Finalproject.qmd`  
  Main Quarto document containing the analysis, visualizations, and narrative.
  
- `app.R`   
  Files used to create the web-based presentation of results.

- `Data/`  
  Contains the three CSV data files used in the analysis.

- `README.md`  
  This file. Describes the project and points to key files.

# Techincal Report

## Data Cleaning and Wrangling
We began by importing all three CSV files with `readr::read_csv()`, then standardized variable names using `janitor::clean_names()`. 
For each dataset we:
- Selected only the variables relevant to sleep, stress, and academic performance.
- Converted character variables to factors where appropriate.
- Dropped rows with missing values in key columns using `drop_na()`.
In the sleep dataset, we also created a sleep_category variable that groups observations into “Short sleep” (<7 hours), “Recommended sleep” (7–9 hours), and “Long sleep” (≥9 hours).

To combine information across datasets for Research Question 3, we aggregated each dataset by rounded sleep hours:
- From the factors dataset, we computed average study hours and exam scores by `sleep_hours`.
- From the sleep dataset, we rounded `sleep_duration` to the nearest hour, then computed average sleep quality and stress for each sleep hour.
- From the student performance dataset, we computed average performance index, previous scores, and hours studied by `sleep_hours`.

We then joined these summaries by `sleep_hours` using `left_join()` and `full_join()` to create a single `full_clean_data` table for plotting.

## Visualization
Most of our analysis is based on tidyverse graphics using `ggplot2`. We created:
- Histogram to describe the distribution of sleep duration.
- Scatterplots with `geom_point()` and `geom_smooth()` to explore relationships between variables
 
## Tools Used
- tidyverse (`dplyr`, `ggplot2`, `readr`, `tibble`) for wrangling and visualization.
- janitor for cleaning column names.
- broom for tidy model output.
- shiny to make the Shiny app

## Web Presentation
Our web-based presentation Shiny app is available at by running the `app.R` file

## Reproducibility
All code is contained in our Quarto document(s) and R scripts in the GitHub repository. 
Anyone with access to the data files and an R installation with the required packages can reproduce our results by knitting the .qmd file or rerunning the scripts.

## How to Reproduce
1. Clone the repository from GitHub.
2. Open the project in RStudio.
3. Install required packages if needed:
   ```r
   install.packages(c("tidyverse", "janitor", "broom", "ggplot2", "shiny", "bslib"))
   ```
4. Run `Finalproject.qmd` by rendering for the pdf format and `app.R` for the web presentation

## Final Product
Click [here](https://tranc2-data-science-finals.shinyapps.io/DataScience-Final-Project/) to access the project.