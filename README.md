# Sales Dashboard - Shiny App

A minimal, responsive Shiny dashboard built with only `shiny` and `bslib`.

## Features

- 📊 Interactive sales data visualization
- 🎯 Filter by product and region
- 📈 Real-time summary statistics
- 💼 Clean, modern UI with bslib
- ⚡ Lightweight - only 2 dependencies

## Installation

```r
install.packages(c("shiny", "bslib"))
```

## Running the App

```r
# From R console in the app directory
shiny::runApp()

# Or from RStudio - open any file and click "Run App"
```

## File Structure

```
├── global.R    # Data setup and libraries
├── ui.R        # User interface
├── server.R    # Server logic
└── README.md   # This file
```

## Data

The app uses a hardcoded dataset of 100 sales records with:
- Date (100 days starting from 2024-01-01)
- Product (Laptop, Phone, Tablet, Monitor, Keyboard)
- Region (North, South, East, West)
- Sales Amount ($100-$5000)
- Units Sold (1-20)

## Dependencies

- R >= 4.0.0
- shiny
- bslib

## License

