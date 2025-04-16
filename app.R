library(shiny)
library(survival)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(gifski)
library(survminer)

# ----- Simulate Historical Data -----
set.seed(42)
n <- 1500
mock_data <- data.frame(
  employee_id = 1:n,
  level_group = sample(c("Junior", "Mid", "Senior"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
  job_function = sample(c("Eng", "PM", "DS"), n, replace = TRUE),
  tenure_months = sample(1:60, n, replace = TRUE)
)
mock_data$time_to_term <- round(rexp(n, rate = ifelse(mock_data$level_group == "Junior", 0.06,
                                                      ifelse(mock_data$level_group == "Mid", 0.04, 0.025))), 0)
mock_data$is_termed <- ifelse(mock_data$time_to_term <= mock_data$tenure_months, 1, 0)
mock_data$observed_time <- pmin(mock_data$tenure_months, mock_data$time_to_term)

# ----- Fit Cox Model -----
cox_model <- coxph(Surv(observed_time, is_termed) ~ level_group + job_function, data = mock_data)

# ----- UI -----
ui <- fluidPage(
  titlePanel("Monte Carlo Simulation with Survival-Based Attrition"),
  sidebarLayout(
    sidebarPanel(
      selectInput("job_func", "Select Job Function:", choices = c("Eng", "PM", "DS")),
      numericInput("months", "Simulation Months:", 12, min = 1),
      numericInput("runs", "Number of Simulations:", 100, min = 10, max = 500),
      numericInput("junior", "Initial Junior Headcount:", 50),
      numericInput("mid", "Initial Mid Headcount:", 50),
      numericInput("senior", "Initial Senior Headcount:", 30),
      actionButton("go", "Run Simulation")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Simulation", imageOutput("animatedPlot", height = "500px")),
        tabPanel("Survival Curves", plotOutput("survPlot")),
        tabPanel("Final Distribution", plotOutput("finalDistPlot"))
      )
    )
  )
)

# ----- Server -----
server <- function(input, output) {
  survfit_pred <- reactive({
    req(input$job_func)
    newdata <- data.frame(
      level_group = c("Junior", "Mid", "Senior"),
      job_function = rep(input$job_func, 3)
    )
    survfit(cox_model, newdata = newdata)
  })
  
  surv_probs_by_month <- function(level, job_function, months) {
    sf <- survfit(cox_model, newdata = data.frame(level_group = level, job_function = job_function))
    approx(sf$time, sf$surv, xout = months, rule = 2)$y
  }
  
  simulate_segmented_headcount <- function(initial_counts, job_function, months = 12, runs = 100) {
    segments <- names(initial_counts)
    all_results <- list()
    
    for (seg in segments) {
      surv_probs <- surv_probs_by_month(seg, job_function, 0:months)
      headcount_matrix <- matrix(0, nrow = runs, ncol = months + 1)
      headcount_matrix[, 1] <- initial_counts[[seg]]
      
      for (t in 2:(months + 1)) {
        current_headcount <- headcount_matrix[, t - 1]
        monthly_survival_rate <- surv_probs[t] / surv_probs[t - 1]
        monthly_survival_rate[is.na(monthly_survival_rate)] <- 1
        attritions <- rbinom(runs, current_headcount, 1 - monthly_survival_rate)
        headcount_matrix[, t] <- pmax(0, current_headcount - attritions)
      }
      
      df <- as.data.frame(headcount_matrix)
      colnames(df) <- paste0("Month_", 0:months)
      df$Sim <- 1:runs
      df$Segment <- seg
      all_results[[seg]] <- df
    }
    
    bind_rows(all_results) %>%
      pivot_longer(cols = starts_with("Month_"),
                   names_to = "Month", names_prefix = "Month_", values_to = "Headcount") %>%
      mutate(Month = as.integer(Month),
             Sim = factor(Sim),
             Segment = factor(Segment))
  }
  
  sim_result <- eventReactive(input$go, {
    initial_counts <- list(
      Junior = input$junior,
      Mid = input$mid,
      Senior = input$senior
    )
    simulate_segmented_headcount(initial_counts, input$job_func, input$months, input$runs)
  })
  
  output$animatedPlot <- renderImage({
    req(sim_result())
    df <- sim_result()
    
    df_summary <- df %>%
      group_by(Segment, Month) %>%
      summarise(
        q10 = quantile(Headcount, 0.10),
        q90 = quantile(Headcount, 0.90),
        median = median(Headcount),
        .groups = "drop"
      )
    
    p <- ggplot() +
      geom_ribbon(data = df_summary, aes(x = Month, ymin = q10, ymax = q90, fill = Segment), alpha = 0.2) +
      geom_line(data = df_summary, aes(x = Month, y = median, color = Segment), size = 1.2) +
      labs(title = "Simulated Headcount Over Time",
           subtitle = "Showing 10th–90th Percentile Ribbon and Median",
           x = "Month", y = "Headcount") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      transition_reveal(Month)
    
    gif_file <- tempfile(fileext = ".gif")
    animate(p, renderer = gifski_renderer(gif_file), width = 800, height = 500, fps = 10, duration = 6, end_pause = 30)
    
    list(src = gif_file, contentType = "image/gif", width = 800, height = 500)
  }, deleteFile = TRUE)
  
  output$survPlot <- renderPlot({
    fit <- survfit_pred()
    g <- ggsurvplot(fit,
                    data = mock_data,
                    risk.table = TRUE,
                    conf.int = TRUE,
                    pval = TRUE,
                    palette = c("blue", "orange", "green"),
                    title = paste("Survival Curve for Job Function:", input$job_func),
                    ggtheme = theme_minimal())
    g$plot +
      geom_text(data = data.frame(
        Segment = c("Junior", "Mid", "Senior"),
        Risk = round(predict(cox_model, newdata = data.frame(level_group = c("Junior", "Mid", "Senior"),
                                                             job_function = input$job_func)), 2)
      ),
      aes(x = 10, y = 0.3, label = paste(Segment, "Risk Score:", Risk)),
      inherit.aes = FALSE, vjust = -0.5, hjust = 0)
  })
  
  output$finalDistPlot <- renderPlot({
    req(sim_result())
    df <- sim_result()
    last_month <- input$months
    
    df_summary <- df %>%
      filter(Month == last_month) %>%
      group_by(Segment) %>%
      summarise(median_val = median(Headcount), .groups = "drop")
    
    ggplot(df %>% filter(Month == last_month), aes(x = Headcount, fill = Segment)) +
      geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
      geom_vline(data = df_summary, aes(xintercept = median_val, color = Segment), linetype = "dashed", size = 1) +
      geom_text(data = df_summary, aes(x = median_val, y = 5, label = paste("Median:", median_val), color = Segment),
                angle = 90, vjust = -0.5, hjust = -0.1) +
      facet_wrap(~Segment, scales = "free") +
      labs(title = paste("Final Headcount Distribution at Month", last_month),
           x = "Final Headcount", y = "Frequency") +
      theme_minimal()
  })
}

shinyApp(ui, server)
