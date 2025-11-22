# Final Portfolio Project - Stat 220

**Authors:** Seth Bonney and Cuong Tran  
**Title:** Academic Impact of Being a Night Owl

## Project Overview
This project investigates how “being a night owl” - consistently getting short sleep - relates to stress, sleep quality, and academic performance. 
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

All data files are stored in the [`data/`] folder. I am planing on doing this

## Repository Structure
- `Finalproject.qmd`  
  Main Quarto document containing the analysis, visualizations, and narrative.
  
- `technical_report.txt`
  Brief technical description of our methodology and tools.

- `app.R`   
  Files used to create the web-based presentation of results.

- `data/`  
  Contains the three CSV data files used in the analysis.

- `README.md`  
  This file. Describes the project and points to key files.

## Web Presentation
Our web-based presentation Shiny app is available at by running the `app.R` file

## How to Reproduce
1. Clone the repository from GitHub.
2. Open the project in RStudio.
3. Install required packages if needed:
   ```r
   install.packages(c("tidyverse", "janitor", "broom", "ggplot2"))
4. Run `Finalproject.qmd` by rendering for the pdf format and `app.R` for the web presentation
