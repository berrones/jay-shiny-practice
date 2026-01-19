# Sales Dashboard – Shiny App

An interactive Shiny dashboard for exploring sales data across products, regions, and time. Designed to be clean, responsive, and immediately usable.

## Features

- 📊 Tabbed dashboard with **Data Overview**, **Sales Trends**, and **Regional Comparisons**
- 🎯 Reactive filtering by product and date range
- 📈 Sales Trends line chart (total sales over time)
- 🧮 Real-time summary metrics (total sales, average amount, total units)
- 🗂 Data preview table
- 🌍 Regional Comparisons with four monthly sales bar charts (North, South, East, West)
  - Consistent month-based coloring across regions
  - Region selector automatically disabled when not applicable
- 🎨 Clean, modern UI powered by `bslib`

## Installation

```r
install.packages(c("shiny", "bslib", "shinyjs"))

Running the App

# From an R console in the app directory
shiny::runApp()

# Or open in RStudio and click "Run App"

File Structure

├── global.R    # Libraries and data
├── ui.R        # User interface
├── server.R    # Server logic
└── README.md

Data

The app uses a built-in example dataset with:

Daily dates starting from 2024-01-01

Products: Laptop, Phone, Tablet, Monitor, Keyboard

Regions: North, South, East, West

Sales amounts ($100–$5000)

Units sold (1–20)


Dependencies

R ≥ 4.0.0

shiny

bslib

shinyjs


License

MIT