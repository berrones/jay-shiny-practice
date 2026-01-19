ui <- page_sidebar(
  title = "Sales Dashboard",
  theme = bs_theme(
    bootswatch = "sandstone",
    bg = "#123456",
    fg = "#F8DE7E",
    border_color = "#FF7F50"),
  
  sidebar = sidebar(
    selectInput("product", "Product:", 
                choices = c("All", unique(sales_data$product))),
    selectInput("region", "Region:", 
                choices = c("All", unique(sales_data$region))),
    dateRangeInput(
      "dates", "Date range:",
      start = min(sales_data$date),
      end = max(sales_data$date),
      min = min(sales_data$date),
      max = max(sales_data$date)
    )
  ),
  
navset_tab(
  nav_panel("Data Overview",
    card(
      card_header(NULL)
    ),
    layout_columns(
      value_box("Total Sales", textOutput("total_sales"), showcase = icon("dollar-sign")),
      value_box("Avg Amount", textOutput("avg_amount"), showcase = icon("chart-line")),
      value_box("Total Units", textOutput("total_units"), showcase = icon("box"))
    ),
    card(
      card_header("Sales Data"),
      tableOutput("table")
    )
  ),

  nav_panel("Sales Trends",
    card(
      card_header(NULL)
    ),
    card(card_header("Total Sales Over Time"), 
    plotOutput("sales_trend_plot", height=300)
    )
  ),

  nav_panel("Regional Comparisons",
    card(
      card_header(NULL)
    ),
    card(card_header("Sales Box Plots for Selected Regions"), "...")
    # TODO: Add box plot for selected regions.
  )
)
)