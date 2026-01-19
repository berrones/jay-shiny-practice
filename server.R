server <- function(input, output, session) {
  
  observeEvent(input$tabs, {
    if (input$tabs == "Regional Comparisons") {
      shinyjs::disable("region")
    } else {
      shinyjs::enable("region")
    }
  })

  filtered_data <- reactive({
    data <- sales_data
    if(!is.null(input$dates)) {
      data <- data[data$date >= input$dates[1] & data$date <= input$dates[2], ]
    }
    if (input$product != "All") data <- data[data$product == input$product, ]
    if (input$region != "All") data <- data[data$region == input$region, ]
    data
  })

  filtered_data_allregion <- reactive({
    data <- sales_data
    if(!is.null(input$dates)) {
      data <- data[data$date >= input$dates[1] & data$date <= input$dates[2], ]
    }
    if (input$product != "All") data <- data[data$product == input$product, ]
    data
  })

  output$sales_trend_plot <- renderPlot({
    df <- filtered_data()
    daily <- aggregate(amount ~ date, data=df, sum)

    plot(
      daily$date, daily$amount/1000,
      type = "l",
      xlab = "Date",
      ylab = "Sales (in thousands $)"
    )
  })
  
  output$total_sales <- renderText({
    paste0("$", format(sum(filtered_data()$amount), big.mark = ",", nsmall = 0))
  })
  
  output$avg_amount <- renderText({
    paste0("$", format(round(mean(filtered_data()$amount)), big.mark = ","))
  })
  
  output$total_units <- renderText({
    format(sum(filtered_data()$units), big.mark = ",")
  })
  
  output$table <- renderTable({
    df <- head(filtered_data(), 20)
    df$date <- format(df$date, "%d %b %Y")
    df
  })

  output$region_month_hists <- renderPlot({
    df <- filtered_data_allregion()

    df$month_date <- as.Date(format(df$date, "%Y-%m-01"))
    months_all <- sort(unique(df$month_date))
    month_labels <- format(months_all, "%b '%y")
    month_cols <- grDevices::hcl.colors(length(months_all), palette = "Dark 3")

    regions <- c("North", "South", "East", "West")

    op <- par(mfrow = c(2,2), mar = c(7,4,3,1))
    on.exit(par(op), add=TRUE)

    for (r in regions) {
      sub <- df[df$region == r, ]

      monthly <- aggregate(amount~month_date, data=sub, sum)
      monthly <- monthly[order(monthly$month), ]

      y <- monthly$amount[match(months_all, monthly$month_date)]
      y[is.na(y)] <- 0

      barplot(
        height = y/1000,
        names.arg = month_labels,
        col = month_cols,
        las = 1,
        main = r,
        xlab = "Month",
        ylab = "Sales (in thousands $)"
      )
    }
  })
}