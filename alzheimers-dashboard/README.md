# Alzheimer’s Study Analyzer (R Shiny)

This Shiny app provides an interactive analysis environment for **four datasets related to Alzheimer’s disease**, each treated as a separate study and explored through a unified interface.

## Data Source

The datasets were downloaded from the **Rdatasets** repository curated by **Vincent Arel-Bundock**, which maintains over 2,000 publicly available educational datasets spanning statistics, econometrics, medicine, and social science:

https://vincentarelbundock.github.io/Rdatasets/datasets.html

## App Overview

- **Home page**: Four study cards arranged in a 2×2 grid. Each card links to the analysis pane for a specific dataset.
- **Study analysis pane**: A sidebar-driven workflow for exploratory and inferential analysis.

### Available Analyses (per study)

- **Summary Data**  
  Variable names, types, number of observations, missing counts, and summary statistics (min, max, mean, median for numeric variables).

- **Data Overview**  
  Interactive table view of the full dataset.

- **Two-Way Correlations**  
  User-selected correlation heatmap (up to 5 numeric variables).

- **Linear Regression**  
  - User-selected dependent variable and up to 5 predictors  
  - Scatterplot with regression line and confidence band  
  - Regression equation, adjusted R², and AIC

- **Logistic Regression**  
  - Binary outcome with up to 5 predictors  
  - Predicted probability plot with confidence band  
  - Logit equation, McFadden’s pseudo-R², and AIC

## Technical Details

- Built with **R Shiny** using a modular structure
- Data manipulation and modeling rely on **tidyverse** conventions
- Visualization via **ggplot2**
- Regression outputs via **broom**
- UI styled with **bslib** using a medical/biostatistics-themed palette

## File Structure

app/
├── app.R
├── study1.csv
├── study2.csv
├── study3.csv
├── study4.csv
└── README.md


All four `.csv` files must be located in the same directory as `app.R`.

## Running the App

#Dependencies
install.packages("shiny","bslib",","tidyverse","readr","DT","broom")

shiny::runApp()


Intended Use

This app is designed for educational and exploratory biostatistical analysis, demonstrating common workflows used in Alzheimer’s research such as correlation screening, linear modeling, and logistic regression.