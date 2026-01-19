# Load required libraries
library(shiny)
library(bslib)

# Create hardcoded data frame with 100 rows
set.seed(123)
sales_data <- data.frame(
  date = seq(as.Date("2024-01-01"), by = "day", length.out = 100),
  product = sample(c("Laptop", "Phone", "Tablet", "Monitor", "Keyboard"), 100, replace = TRUE),
  region = sample(c("North", "South", "East", "West"), 100, replace = TRUE),
  amount = round(runif(100, 100, 5000), 2),
  units = sample(1:20, 100, replace = TRUE)
)