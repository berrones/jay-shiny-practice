# app.R
library(shiny)
library(bslib)
library(tidyverse)
library(readr)
library(DT)
library(broom)

# ---- configure your study files here ----
study_files <- c(
  "ad_data.csv",
  "alzheimer_smoking_df.csv",
  "LewyDLBad.csv"
)
study_names <- c("Study 1: Craig-Schapiro et al. 2011", "Study 2: Salib-Hillier 1997", "Study 3: Nedelksa et al. 2015")

# ---- helpers ----
safe_read_csv <- function(path) {
  validate(need(file.exists(path), paste0("Missing file: ", path)))
  readr::read_csv(path, show_col_types = FALSE)
}

is_binary_01 <- function(x) {
  if (is.logical(x)) return(TRUE)
  if (!is.numeric(x) && !is.integer(x)) return(FALSE)
  ux <- sort(unique(x[!is.na(x)]))
  length(ux) == 2 && all(ux %in% c(0, 1))
}

summary_table <- function(df) {
  tibble(
    variable = names(df),
    type = map_chr(df, ~ class(.x)[1]),
    n = map_int(df, ~ sum(!is.na(.x))),
    missing = map_int(df, ~ sum(is.na(.x))),
    min = map_chr(df, ~ if (is.numeric(.x) || is.integer(.x)) as.character(min(.x, na.rm = TRUE)) else ""),
    max = map_chr(df, ~ if (is.numeric(.x) || is.integer(.x)) as.character(max(.x, na.rm = TRUE)) else ""),
    mean = map_chr(df, ~ if (is.numeric(.x) || is.integer(.x)) as.character(mean(.x, na.rm = TRUE)) else ""),
    median = map_chr(df, ~ if (is.numeric(.x) || is.integer(.x)) as.character(median(.x, na.rm = TRUE)) else "")
  )
}

eqn_string <- function(fit) {
  td <- broom::tidy(fit)
  # keep clean names
  terms <- td$term
  est <- td$estimate
  b0 <- est[terms == "(Intercept)"]
  others <- tibble(term = terms[terms != "(Intercept)"], est = est[terms != "(Intercept)"])
  rhs <- paste0(
    if (length(b0) == 0) "0" else formatC(b0[1], digits = 4, format = "fg"),
    if (nrow(others) == 0) "" else paste0(
      " + ",
      paste0(formatC(others$est, digits = 4, format = "fg"), "·", others$term, collapse = " + ")
    )
  )
  paste0("y = ", rhs)
}

mcfadden_r2 <- function(fit) {
  # McFadden's pseudo-R2 = 1 - (LL_model / LL_null)
  ll_full <- as.numeric(logLik(fit))
  ll_null <- as.numeric(logLik(update(fit, . ~ 1)))
  1 - (ll_full / ll_null)
}

plot_lm_with_band <- function(df, fit, xvar, yvar, other_preds) {
  # prediction grid: vary x, hold others at mean (numeric) / first level (factor)
  grid_n <- 100
  g <- tibble(!!sym(xvar) := seq(min(df[[xvar]], na.rm = TRUE), max(df[[xvar]], na.rm = TRUE), length.out = grid_n))

  for (p in other_preds) {
    if (is.numeric(df[[p]]) || is.integer(df[[p]])) {
      g[[p]] <- mean(df[[p]], na.rm = TRUE)
    } else {
      # treat as factor/character
      lev <- df[[p]] |> as.factor() |> levels()
      g[[p]] <- factor(lev[1], levels = lev)
    }
  }

  pr <- predict(fit, newdata = g, se.fit = TRUE)
  g <- g |>
    mutate(
      .fit = pr$fit,
      .lwr = pr$fit - 1.96 * pr$se.fit,
      .upr = pr$fit + 1.96 * pr$se.fit
    )

  ggplot(df, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_point(alpha = 0.65) +
    geom_ribbon(data = g, aes(ymin = .lwr, ymax = .upr), inherit.aes = FALSE, alpha = 0.25) +
    geom_line(data = g, aes(y = .fit), inherit.aes = FALSE, linewidth = 1) +
    labs(x = xvar, y = yvar)
}

plot_glm_prob_with_band <- function(df, fit, xvar, yvar, other_preds) {
  grid_n <- 120
  g <- tibble(!!sym(xvar) := seq(min(df[[xvar]], na.rm = TRUE), max(df[[xvar]], na.rm = TRUE), length.out = grid_n))

  for (p in other_preds) {
    if (is.numeric(df[[p]]) || is.integer(df[[p]])) {
      g[[p]] <- mean(df[[p]], na.rm = TRUE)
    } else {
      lev <- df[[p]] |> as.factor() |> levels()
      g[[p]] <- factor(lev[1], levels = lev)
    }
  }

  pr <- predict(fit, newdata = g, type = "link", se.fit = TRUE)
  g <- g |>
    mutate(
      .eta = pr$fit,
      .eta_lwr = pr$fit - 1.96 * pr$se.fit,
      .eta_upr = pr$fit + 1.96 * pr$se.fit,
      .p = plogis(.eta),
      .p_lwr = plogis(.eta_lwr),
      .p_upr = plogis(.eta_upr)
    )

  # y in {0,1} (or logical) for points
  yy <- if (is.logical(df[[yvar]])) as.integer(df[[yvar]]) else df[[yvar]]

  ggplot(df, aes(x = .data[[xvar]], y = yy)) +
    geom_jitter(height = 0.05, width = 0, alpha = 0.5) +
    geom_ribbon(data = g, aes(x = .data[[xvar]], ymin = .p_lwr, ymax = .p_upr),
                inherit.aes = FALSE, alpha = 0.25) +
    geom_line(data = g, aes(x = .data[[xvar]], y = .p), inherit.aes = FALSE, linewidth = 1) +
    scale_y_continuous(limits = c(-0.05, 1.05), breaks = c(0, 0.5, 1)) +
    labs(x = xvar, y = paste0("Pr(", yvar, " = 1)"))
}

# ---- per-study module ----
study_ui <- function(id, label) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      radioButtons(
        ns("panel"),
        "Analysis",
        choices = c("Dataset Variables", "Full Dataset", "Two-Way Correlations", "Linear Regression", "Logistic Regression"),
        selected = "Summary Data"
      ),
      tags$hr(),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Two-Way Correlations'", ns("panel")),
        selectizeInput(ns("cor_vars"), "Select up to 5 numeric variables", choices = NULL, multiple = TRUE,
                       options = list(maxItems = 5))
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Linear Regression'", ns("panel")),
        selectInput(ns("lm_y"), "Dependent variable (numeric)", choices = NULL),
        selectizeInput(ns("lm_xs"), "Predictors (up to 5)", choices = NULL, multiple = TRUE,
                       options = list(maxItems = 5)),
        selectInput(ns("lm_plot_x"), "X variable for plot", choices = NULL)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Logistic Regression'", ns("panel")),
        selectInput(ns("glm_y"), "Dependent variable (binary 0/1 or logical)", choices = NULL),
        selectizeInput(ns("glm_xs"), "Predictors (up to 5)", choices = NULL, multiple = TRUE,
                       options = list(maxItems = 5)),
        selectInput(ns("glm_plot_x"), "X variable for plot", choices = NULL)
      )
    ),
    card(
      card_header(label),
      uiOutput(ns("main"))
    )
  )
}

study_server <- function(id, csv_path) {
  moduleServer(id, function(input, output, session) {

    df_raw <- reactive({
      safe_read_csv(csv_path)
    })

    observeEvent(df_raw(), {
      df <- df_raw()

      num_vars <- df |> select(where(~ is.numeric(.x) || is.integer(.x))) |> names()
      bin_vars <- df |> select(where(is_binary_01)) |> names()
      all_vars <- names(df)

      updateSelectizeInput(session, "cor_vars", choices = num_vars, selected = head(num_vars, 5))

      updateSelectInput(session, "lm_y", choices = num_vars, selected = if (length(num_vars) > 0) num_vars[1] else character(0))
      updateSelectizeInput(session, "lm_xs", choices = setdiff(all_vars, input$lm_y), selected = head(setdiff(num_vars, input$lm_y), 2))
      updateSelectInput(session, "lm_plot_x", choices = all_vars)

      updateSelectInput(session, "glm_y", choices = bin_vars, selected = if (length(bin_vars) > 0) bin_vars[1] else character(0))
      updateSelectizeInput(session, "glm_xs", choices = setdiff(all_vars, input$glm_y), selected = head(setdiff(num_vars, input$glm_y), 2))
      updateSelectInput(session, "glm_plot_x", choices = all_vars)
    }, ignoreInit = TRUE)

    output$main <- renderUI({
      req(df_raw())
      df <- df_raw()

      if (input$panel == "Dataset Variables") {
        tagList(
          DTOutput(session$ns("sum_tbl"))
        )
      } else if (input$panel == "Full Dataset") {
        tagList(
          DTOutput(session$ns("data_tbl"))
        )
      } else if (input$panel == "Two-Way Correlations") {
        tagList(
          plotOutput(session$ns("cor_plot"), height = "420px")
        )
      } else if (input$panel == "Linear Regression") {
        tagList(
          plotOutput(session$ns("lm_plot"), height = "420px"),
          tags$hr(),
          uiOutput(session$ns("lm_stats"))
        )
      } else {
        tagList(
          plotOutput(session$ns("glm_plot"), height = "420px"),
          tags$hr(),
          uiOutput(session$ns("glm_stats"))
        )
      }
    })

    output$sum_tbl <- renderDT({
      df <- df_raw()
      summary_table(df) |>
        DT::datatable(options = list(pageLength = 15, scrollX = TRUE))
    })

    output$data_tbl <- renderDT({
      df <- df_raw()
      DT::datatable(df, options = list(pageLength = 15, scrollX = TRUE))
    })

    output$cor_plot <- renderPlot({
      df <- df_raw()
      req(input$cor_vars)
      vars <- input$cor_vars
      validate(need(length(vars) >= 2, "Select at least 2 numeric variables."))

      mat <- df |>
        select(all_of(vars)) |>
        cor(use = "pairwise.complete.obs")

      cor_df <- as.data.frame(mat) |>
        rownames_to_column("var1") |>
        pivot_longer(-var1, names_to = "var2", values_to = "r")

      ggplot(cor_df, aes(x = var1, y = var2, fill = r)) +
        geom_tile() +
        geom_text(aes(label = sprintf("%.2f", r)), size = 3) +
        coord_equal() +
        labs(x = NULL, y = NULL) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    # ---- Linear regression ----
    lm_fit <- reactive({
      df <- df_raw()
      req(input$lm_y, input$lm_xs)
      y <- input$lm_y
      xs <- input$lm_xs
      validate(need(length(xs) >= 1, "Choose at least 1 predictor."))

      f <- as.formula(paste(y, "~", paste(xs, collapse = " + ")))
      stats::lm(f, data = df)
    })

    output$lm_plot <- renderPlot({
      df <- df_raw()
      fit <- lm_fit()

      req(input$lm_plot_x, input$lm_y, input$lm_xs)
      xvar <- input$lm_plot_x
      yvar <- input$lm_y

      validate(need(xvar %in% input$lm_xs, "Pick an X variable that is included among the predictors."))
      validate(need(is.numeric(df[[xvar]]) || is.integer(df[[xvar]]), "X variable for the plot must be numeric."))

      other_preds <- setdiff(input$lm_xs, xvar)
      plot_lm_with_band(df, fit, xvar, yvar, other_preds)
    })

    output$lm_stats <- renderUI({
      fit <- lm_fit()
      g <- broom::glance(fit)
      tagList(
        tags$p(tags$b("Equation: "), eqn_string(fit)),
        tags$p(tags$b("Adjusted R²: "), sprintf("%.4f", g$adj.r.squared)),
        tags$p(tags$b("AIC: "), sprintf("%.2f", AIC(fit)))
      )
    })

    # ---- Logistic regression ----
    glm_fit <- reactive({
      df <- df_raw()
      req(input$glm_y, input$glm_xs)
      y <- input$glm_y
      xs <- input$glm_xs
      validate(need(length(xs) >= 1, "Choose at least 1 predictor."))

      validate(need(is_binary_01(df[[y]]), "Dependent variable must be binary (0/1) or logical."))

      # ensure 0/1 numeric for glm if logical
      df2 <- df |> mutate(!!sym(y) := if (is.logical(.data[[y]])) as.integer(.data[[y]]) else .data[[y]])

      f <- as.formula(paste(y, "~", paste(xs, collapse = " + ")))
      stats::glm(f, data = df2, family = stats::binomial())
    })

    output$glm_plot <- renderPlot({
      df <- df_raw()
      fit <- glm_fit()

      req(input$glm_plot_x, input$glm_y, input$glm_xs)
      xvar <- input$glm_plot_x
      yvar <- input$glm_y

      validate(need(xvar %in% input$glm_xs, "Pick an X variable that is included among the predictors."))
      validate(need(is.numeric(df[[xvar]]) || is.integer(df[[xvar]]), "X variable for the plot must be numeric."))

      other_preds <- setdiff(input$glm_xs, xvar)
      plot_glm_prob_with_band(df, fit, xvar, yvar, other_preds)
    })

    output$glm_stats <- renderUI({
      fit <- glm_fit()
      tagList(
        tags$p(tags$b("Logit model equation (linear predictor): "), eqn_string(fit)),
        tags$p(tags$b("McFadden R²: "), sprintf("%.4f", mcfadden_r2(fit))),
        tags$p(tags$b("AIC: "), sprintf("%.2f", AIC(fit)))
      )
    })
  })
}

# ---- theme (medical/biostats) ----
med_theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  bg = "#F7FBFC",
  fg = "#0B1F2A",
  primary = "#0B7285",  # teal
  secondary = "#1C7ED6",# blue
  success = "#2F9E44",
  base_font = font_google("Inter")
)
