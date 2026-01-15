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
                choices = c("All", unique(sales_data$region)))
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
)