# app.R
library(shiny)

ui <- fluidPage(
  titlePanel("CSV Column Selector"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "cols",
        "Choose columns:",
        choices = NULL,      # filled after reading CSV
        selected = NULL,     # start empty
        multiple = TRUE,
        options = list(
          placeholder = "Click to select columns...",
          plugins = list("remove_button")
        )
      )
    ),
    mainPanel(
      tableOutput("tbl")
    )
  )
)

server <- function(input, output, session) {

  df <- reactive({
    read.csv("testdata.csv", check.names = FALSE)
  })

  observe({
    req(df())
    updateSelectizeInput(
      session, "cols",
      choices  = names(df()),
      selected = character(0),
      server   = TRUE
    )
  })

  output$tbl <- renderTable({
    d <- df()
    cols <- input$cols
    if (length(cols) == 0) return(d[0, 0, drop = FALSE])  # empty table until selection
    d[, cols, drop = FALSE]
  })
}

shinyApp(ui, server)
