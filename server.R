server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- sales_data
    if (input$product != "All") data <- data[data$product == input$product, ]
    if (input$region != "All") data <- data[data$region == input$region, ]
    data
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
    head(filtered_data(), 20)
  })
}