# app.R
library(shiny)
library(wavelets)

make_signal <- function(type, n, noise_sd) {
  t <- seq(0, 1, length.out = n)
  x <- switch(
    type,
    "Sine"  = sin(2*pi*5*t),
    "Chirp" = sin(2*pi*(2 + 15*t)*t),
    "Step"  = ifelse(t < 0.5, -1, 1)
  )
  x + rnorm(n, 0, noise_sd)
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background: linear-gradient(135deg, #0b1b3a, #3a0b2d); color: #ffffff; }
    .card { background: rgba(255,255,255,0.10); border: 1px solid rgba(255,255,255,0.18);
            border-radius: 18px; padding: 18px; margin-top: 12px; box-shadow: 0 8px 22px rgba(0,0,0,0.25); }
    .bigTitle { text-align:center; font-size: 40px; font-weight: 800; letter-spacing: 0.5px; margin-top: 8px; }
    .bigSub { text-align:center; font-size: 20px; opacity: 0.95; margin-top: 6px; }
    .stepPill { text-align:center; font-size: 16px; opacity: 0.9; margin-top: 8px; }
    .center { text-align:center; }
    .shiny-input-container label { color: #ffffff !important; font-weight: 700; }
    .btn { border-radius: 14px; font-weight: 800; }
    .btn-default { background: rgba(255,255,255,0.18); color: #fff; border: 1px solid rgba(255,255,255,0.25); }
    .btn-default:hover { background: rgba(255,255,255,0.28); color:#fff; }
    .plotWrap { background: rgba(0,0,0,0.18); border-radius: 16px; padding: 10px; }
  "))),

  div(class = "bigTitle", "Wavelet Dashboard"),
  div(class = "bigSub", "Click through a short, hands-on tour of the DWT: "),
  div(class = "Signal → DWT → coefficients"),
  div(class = "stepPill", textOutput("step_caption")),

  sidebarLayout(
    sidebarPanel(
      class = "card",

      div(class = "center",
          actionButton("prev", "\u25C0  Prev"),
          actionButton("next_step", "Next  \u25B6")
      ),
      tags$hr(style="border-color: rgba(255,255,255,0.25);"),

      uiOutput("controls")
    ),

    mainPanel(
      uiOutput("panel")
    )
  )
)

server <- function(input, output, session) {
  step <- reactiveVal(1)
  n_steps <- 3

  observeEvent(input$prev, step(max(1, step() - 1)))
  observeEvent(input$next_step, step(min(n_steps, step() + 1)))

  output$step_caption <- renderText({
    paste0("Step ", step(), " / ", n_steps)
  })

  output$controls <- renderUI({
    s <- step()
    if (s == 1) {
      tagList(
        div(class="bigSub", "Wavelets are used to break sequences of numeric signal data into two pieces (smooth and detailed) at different scales."),
        tags$hr(style="border-color: rgba(255,255,255,0.25);"),
        div(class="bigSub", "Click Next to build a signal.")
      )
    } else {
      tagList(
        selectInput("sig_type", "Choose a signal",
                    choices = c("Sine", "Chirp", "Step"),
                    selected = "Chirp"),
        sliderInput("n", "Signal length", min = 128, max = 1024, value = 512, step = 128),
        sliderInput("noise_sd", "Noise (SD)", min = 0, max = 1, value = 0.2, step = 0.05),
        selectInput("wf", "Wavelet",
                    choices = c("haar", "d4"),
                    selected = "haar"),
        sliderInput("J", "Levels", min = 1, max = 6, value = 4, step = 1),
        if (s == 3) helpText("Tip: Level 1 = finest detail; higher = broader detail.")
      )
    }
  })

  signal <- reactive({
    req(input$sig_type, input$n, input$noise_sd)
    make_signal(input$sig_type, input$n, input$noise_sd)
  })

  dwt_obj <- reactive({
    req(signal(), input$wf, input$J)
    dwt(signal(), filter = input$wf, n.levels = input$J, boundary = "periodic")
  })

  output$panel <- renderUI({
    s <- step()

    if (s == 1) {
      div(class="card",
          div(class="bigTitle", "What is a wavelet?"),
          div(style = "height: 12px;"),
          div(class="bigSub", "A wavelet is a small, localized wave.\n"),
          div(style = "height: 12px;"),
          div(class="bigSub", "The discrete wavelet transform (DWT) is an operation that uses wavelets to describe signal features at multiple scales.\n"),
          div(style = "height: 12px;"),
          div(class="bigSub", "Output of the DWT has many applications, like compressing signal data, cleaning noisy signals, and finding hidden patterns in signals."),
          tags$hr(style="border-color: rgba(255,255,255,0.25);"),

      div(class="iconRow",
          div(class="iconBox", style = "font-size: 36px; text-align: center", "🌊 🔍 📈"),
          div(style = "height: 12px;"),
          div(
            div(class="bigSub",
                "Wavelets are technically ",
                tags$b(tags$i("mathematical functions")),
                " which are strategically applied transform signal data."
            ),
            div(style="height:12px;"),
            div(class="bigSub",
                "The math is intriguing, but it takes a bit of background to explain. Stay tuned to learn more, and to see what wavelets actually look like!"
            )
          )
      )

      )

    } else if (s == 2) {
      div(class="card",
          div(class="bigTitle", "Signal → DWT"),
          div(class="bigSub", "We generate a signal, add noise, then decompose it."),
          div(class="plotWrap", plotOutput("plot_signal", height = 320)),
          tags$hr(style="border-color: rgba(255,255,255,0.25);"),
          div(class="bigSub", textOutput("dwt_one_liner"))
      )

    } else {
      div(class="card",
          div(class="bigTitle", "Detail coefficients"),
          div(class="bigSub", "Details capture “changes” at different scales."),
          div(class="plotWrap", plotOutput("plot_details_simple", height = 420)),
          tags$hr(style="border-color: rgba(255,255,255,0.25);"),
          div(class="bigSub", "Try switching Haar ↔ d4 and watch the detail patterns change.")
      )
    }
  })

  output$plot_signal <- renderPlot({
    x <- signal()
    plot(x, type = "l", lwd = 2,
         main = "Generated signal (noisy)",
         xlab = "Index", ylab = "Value")
    grid()
  })

  output$dwt_one_liner <- renderText({
    obj <- dwt_obj()
    J <- length(obj@W)
    paste0("Computed DWT with wavelet = ", input$wf,
           "   |   levels J = ", J,
           "   |   boundary = ", obj@boundary)
  })

  output$plot_details_simple <- renderPlot({
    obj <- dwt_obj()
    W <- obj@W
    J <- length(W)

    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

    plot(W[[1]], type = "l", lwd = 2,
         main = "Level 1 detail (finest scale)",
         xlab = "Index", ylab = "coef")
    abline(h = 0); grid()

    plot(W[[J]], type = "l", lwd = 2,
         main = paste0("Level ", J, " detail (coarsest scale)"),
         xlab = "Index", ylab = "coef")
    abline(h = 0); grid()
  })
}

shinyApp(ui, server)
