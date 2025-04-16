library(shiny)
library(survival)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# ----- Generate mock historical employee data -----
generate_employee_data <- function(n = 1500) {
  set.seed(123)
  df <- data.frame(
    employee_id = 1:n,
    level_group = sample(c("Junior", "Mid", "Senior"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
    job_function = sample(c("ENG", "G&A", "Product"), n, replace = TRUE),
    tenure_months = sample(1:60, n, replace = TRUE)
  )
  base_rate <- ifelse(df$level_group == "Junior", 0.05, ifelse(df$level_group == "Mid", 0.03, 0.015))
  df$time_to_term <- rexp(n, rate = base_rate)
  df$is_termed <- ifelse(df$time_to_term <= df$tenure_months, 1, 0)
  df$observed_time <- pmin(df$tenure_months, df$time_to_term)
  df
}

# ----- Generate mock candidate data -----
generate_candidate_data <- function(n = 2000) {
  set.seed(42)
  df <- data.frame(
    candidate_id = 1:n,
    level_group = sample(c("Junior", "Mid", "Senior"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    job_function = sample(c("ENG", "G&A", "Product"), n, replace = TRUE)
  )
  rate <- ifelse(df$level_group == "Junior", 0.3, ifelse(df$level_group == "Mid", 0.15, 0.08))
  df$time_to_hire <- rexp(n, rate = rate)
  df$is_hired <- rbinom(n, 1, 0.1)
  df$observed_time <- pmin(df$time_to_hire, 12)
  df
}

# ----- Fit models -----
fit_attrition_model <- function(df) {
  coxph(Surv(observed_time, is_termed) ~ level_group + job_function, data = df)
}

fit_hiring_model <- function(df) {
  coxph(Surv(observed_time, is_hired) ~ level_group + job_function, data = df)
}

# ----- Simulation engine -----
simulate_headcount <- function(initial_df, n_hires, attr_model, hire_model, job_function, months = 24, runs = 100) {
  levels <- names(n_hires)
  results <- list()
  for (lvl in levels) {
    attr_sf <- survfit(attr_model, 
                       newdata = data.frame(level_group = lvl, job_function = job_function))
    attr_probs <- approx(attr_sf$time, attr_sf$surv, xout = 0:months, rule = 2)$y
    
    hire_sf <- survfit(hire_model, newdata = data.frame(level_group = lvl, job_function = job_function))
    hire_cdf <- 1 - approx(hire_sf$time, hire_sf$surv, xout = 0:months, rule = 2)$y
    
    init <- initial_df() %>%
      filter(level_group == lvl, job_function == job_function) %>%
      summarise(headcount = sum(headcount), .groups = "drop") %>%
      pull(headcount)
    if (length(init) == 0) init <- 0
    
    mat <- matrix(NA, nrow = runs, ncol = months + 1)
    mat[, 1] <- init
    
    for (t in 2:(months + 1)) {
      prev <- mat[, t - 1]
      attrit <- rbinom(runs, prev, 1 - attr_probs[t] / attr_probs[t - 1])
      hire_fill <- rbinom(runs, n_hires[[lvl]], prob = hire_cdf[t] - hire_cdf[t - 1])
      mat[, t] <- pmax(0, prev - attrit + hire_fill)
    }
    
    df <- as.data.frame(mat)
    colnames(df) <- paste0("Month_", 0:months)
    df$Sim <- 1:runs
    df$level_group <- lvl
    results[[lvl]] <- df
  }
  bind_rows(results) %>%
    pivot_longer(cols = starts_with("Month_"), names_to = "Month", names_prefix = "Month_", values_to = "Headcount") %>%
    mutate(Month = as.integer(Month), Sim = factor(Sim))
}

# ----- UI -----
ui <- fluidPage(
  titlePanel("Survival-Based Headcount Simulation Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("job_func", "Job Function:", c("ENG", "G&A", "Product")),
      numericInput("junior", "New Junior Hires", 20),
      numericInput("mid", "New Mid Hires", 10),
      numericInput("senior", "New Senior Hires", 5),
      actionButton("simulate", "Run Simulation")
    ),
    mainPanel(
      h3("Projected Headcount Over Time"),
      plotlyOutput("headcountPlot"),
      sliderInput("month_sim", "Month", min = 0, max = 24, value = 0, step = 1, animate = animationOptions(500)),
      
      h3("Median Time to Attrition: Raw vs. Survival"),
      plotlyOutput("comparePlot"),
      sliderInput("month_compare", "Month", min = 0, max = 24, value = 0, step = 1, animate = animationOptions(500)),
      
      h3("Relative Bias: Raw vs. Survival Median"),
      plotlyOutput("biasPlot"),
      sliderInput("month_bias", "Month", min = 0, max = 24, value = 0, step = 1, animate = animationOptions(500)),
      
      h3("Cumulative Headcount Decay: Raw vs. Survival"),
      plotlyOutput("decayPlot"),
      sliderInput("month_decay", "Month", min = 0, max = 24, value = 0, step = 1, animate = animationOptions(500))
    )
  )
)

# ----- Server -----
server <- function(input, output, session) {
  sim_data <- reactiveVal()
  attr_model <- reactiveVal()
  hire_model <- reactiveVal()
  emp_data <- reactiveVal()
  initial_df <- reactiveVal()  
  
  observeEvent(input$simulate, {
    emp <- generate_employee_data()
    emp_data(emp)
    cand_data <- generate_candidate_data()
    attr_model(fit_attrition_model(emp))
    hire_model(fit_hiring_model(cand))
    
    init <- emp %>%
      group_by(level_group, job_function) %>%
      summarise(headcount = n(), .groups = "drop")
    
    initial_df(init)
    
    hires <- list(Junior = input$junior, Mid = input$mid, Senior = input$senior)
    
    sim_data(simulate_headcount(init, hires, attr_model(), hire_model(), input$job_func, months = 24))
    
    }) 
    
    # Universal slider template
    renderTimeSlider <- function(id) {
      renderUI({
        sliderInput(id, "Month", min = 0, max = max_months, value = 0, step = 1, animate = animationOptions(interval = 500))
      })
    }
    
    # Sliders
    output$simSlider <- renderTimeSlider("month_sim")
    output$compareSlider <- renderTimeSlider("month_compare")
    output$biasSlider <- renderTimeSlider("month_bias")
    output$decaySlider <- renderTimeSlider("month_decay")
    
    
    output$headcountPlot <- renderPlotly({
      req(sim_data(), initial_df())
      
      # Recalculate initial headcount safely
      initial_counts <- initial_df() %>%
        filter(job_function == input$job_func) 
      
      get_hc <- function(df, lvl) {
        val <- df %>% filter(level_group == lvl) %>% pull(headcount)
        if (length(val) == 0) return(0) else return(val)
      }
      
      junior_hc <- get_hc(initial_counts, "Junior")
      mid_hc    <- get_hc(initial_counts, "Mid")
      senior_hc <- get_hc(initial_counts, "Senior")
      

      subtitle_text <- paste0(
        "Initial HC — Junior: ", junior_hc,
        ", Mid: ", mid_hc,
        ", Senior: ", senior_hc
      )
      
      df <- sim_data() %>%
        filter(Month <= input$month_sim) %>%
        group_by(level_group, Month) %>%
        summarise(
          median = median(Headcount),
          q10 = quantile(Headcount, 0.10),
          q90 = quantile(Headcount, 0.90),
          .groups = "drop"
        )
      
      plot_ly(df, x = ~Month, y = ~median, color = ~level_group, type = 'scatter', mode = 'lines') %>%
        add_ribbons(data = df,
                    ymin = ~q10,
                    ymax = ~q90,
                    x = ~Month,
                    color = ~level_group,
                    opacity = 0.2,
                    line = list(width = 0),
                    showlegend = FALSE) %>%
        layout(title = list(
          text = paste("Simulated Headcount up to Month", input$month_sim,
                       "<br><sub>", subtitle_text, "</sub>"),
          x = 0
        ),
          yaxis = list(title = "Headcount"), xaxis = list(title = "Month"))
    })
    
    output$comparePlot <- renderPlotly({
      req(attr_model())
      df <- emp_data()
      df$tenure_months <- pmin(df$tenure_months, input$month_compare)
      
      raw <- df %>%
        filter(is_termed == 1 & observed_time <= input$month_compare) %>%
        group_by(level_group) %>%
        summarise(Raw = median(observed_time), .groups = "drop")
      
      surv <- data.frame(level_group = c("Junior", "Mid", "Senior"))
      surv$Survival <- sapply(surv$level_group, function(lg) {
        sf <- survfit(attr_model, newdata = data.frame(level_group = lg, job_function = input$job_func))
        summary(sf)$table["median"]
      })
      
      plot_df <- left_join(raw, surv, by = "level_group") %>%
        pivot_longer(cols = c("Raw", "Survival"), names_to = "Method", values_to = "Median")
      
      plot_ly(plot_df, x = ~level_group, y = ~Median, 
              color = ~Method, type = 'bar', barmode = 'group') %>%
        layout(title = paste("Raw vs. Adjusted Time to Attrition - Month ", input$month_compare),
               yaxis = list(title = "Median Time (months)"))
    })
    
    output$biasPlot <- renderPlotly({
      req(attr_model())
      df <- emp_data()
      df$tenure_months <- pmin(df$tenure_months, input$month_bias)
      
      raw <- df %>%
        filter(is_termed == 1 & observed_time <= input$month_bias) %>%
        group_by(level_group) %>%
        summarise(Raw = median(observed_time), .groups = "drop")
      
      surv <- data.frame(level_group = c("Junior", "Mid", "Senior"))
      surv$Survival <- sapply(surv$level_group, function(lg) {
        sf <- survfit(attr_model, newdata = data.frame(level_group = lg, job_function = input$job_func))
        summary(sf)$table["median"]
      })
      
      bias_df <- left_join(raw, surv, by = "level_group") %>%
        mutate(RelBias = (Raw - Survival) / Survival)
      
      plot_ly(bias_df, x = ~level_group, y = ~RelBias, color = ~level_group, type = "bar") %>%
        layout(title = paste("Relative Bias - Month", input$month_bias),
               yaxis = list(title = "Relative Bias", tickformat = ".0%"))
    })
    
    output$decayPlot <- renderPlotly({
      req(attr_model())
      initial <- 100
      months <- 0:24
      raw_decay <- tibble(Month = months,
                          Raw = ifelse(Month <= 12, initial, initial * 0.5))
      sf <- survfit(attr_model, newdata = data.frame(level_group = "Mid", job_function = input$job_func))
      surv_decay <- tibble(Month = months,
                           Survival = approx(sf$time, sf$surv, xout = months, rule = 2)$y * initial)
      decay_df <- left_join(raw_decay, surv_decay, by = "Month") %>%
        pivot_longer(cols = c("Raw", "Survival"), names_to = "Method", values_to = "Headcount")
      
      plot_ly(decay_df %>% filter(Month <= input$month_decay), 
              x = ~Month, y = ~Headcount, color = ~Method, type = "scatter", mode = "lines+markers") %>%
        layout(title = paste("Cumulative Headcount Decay - Month", input$month_decay),
               yaxis = list(title = "Remaining Headcount"))
    })
}

shinyApp(ui, server)
