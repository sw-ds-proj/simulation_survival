library(shiny)
library(plotly)
library(dplyr)
library(tidyr)

# Simulate attrition process (simplified)
simulate_headcount <- function(init_counts, months = 12, runs = 100) {
  segs <- names(init_counts)
  all <- list()
  
  for (s in segs) {
    matrix_sim <- matrix(0, nrow = runs, ncol = months + 1)
    matrix_sim[, 1] <- init_counts[[s]]
    
    for (t in 2:(months + 1)) {
      prev <- matrix_sim[, t - 1]
      attritions <- rbinom(runs, prev, prob = 0.03)  # static attrition for demo
      matrix_sim[, t] <- pmax(0, prev - attritions)
    }
    
    df <- as.data.frame(matrix_sim)
    colnames(df) <- paste0("Month_", 0:months)
    df$Sim <- 1:runs
    df$Segment <- s
    all[[s]] <- df
  }
  
  long_df <- bind_rows(all) %>%
    pivot_longer(cols = starts_with("Month_"), names_to = "Month", names_prefix = "Month_", values_to = "Headcount") %>%
    mutate(Month = as.integer(Month), Sim = factor(Sim), Segment = factor(Segment))
  
  long_df
}

# UI
ui <- fluidPage(
  titlePanel("Interactive Headcount Simulation"),
  fluidRow(
    column(3,
           actionButton("go", "Run Simulation"),
           sliderInput("time", "Month", min = 0, max = 12, value = 0, animate = FALSE, step = 1),
           checkboxInput("autoplay", "Auto-play", value = TRUE)
    ),
    column(9, plotlyOutput("headcountPlot"))
  )
)

# Server
server <- function(input, output, session) {
  sim_data <- reactiveVal(NULL)
  timer <- reactiveVal(0)
  
  observeEvent(input$go, {
    timer(0)
    updateSliderInput(session, "time", value = 0)
    
    sim_data(
      simulate_headcount(
        init_counts = list(Junior = 50, Mid = 60, Senior = 40),
        months = 12,
        runs = 100
      )
    )
  })
  
  observe({
    req(input$autoplay)
    invalidateLater(300, session)
    t <- isolate(input$time)
    if (!is.null(sim_data()) && t < 12) {
      updateSliderInput(session, "time", value = t + 1)
    }
  })
  
  output$headcountPlot <- renderPlotly({
    req(sim_data())
    df <- sim_data()
    current_month <- input$time
    
    plot_df <- df %>%
      filter(Month == current_month) %>%
      group_by(Segment) %>%
      summarise(median = median(Headcount),
                p10 = quantile(Headcount, 0.1),
                p90 = quantile(Headcount, 0.9),
                .groups = "drop")
    
    p <- plot_ly(plot_df, x = ~Segment, y = ~median, type = 'bar', name = "Median") %>%
      add_trace(y = ~p10, type = 'scatter', mode = 'lines+markers', name = "10th Percentile", line = list(dash = 'dot')) %>%
      add_trace(y = ~p90, type = 'scatter', mode = 'lines+markers', name = "90th Percentile", line = list(dash = 'dot')) %>%
      layout(title = paste("Headcount at Month", current_month),
             yaxis = list(title = "Headcount"),
             barmode = 'overlay')
    
    p
  })
}

shinyApp(ui, server)
