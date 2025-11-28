library(shiny)
library(ggplot2)
library(dplyr)

# Simulation function
simulate_S <- function(t, lambda, beta, nrep=5000){
  Svals <- numeric(nrep)
  for(i in 1:nrep){
    N <- rpois(1, lambda * t)
    if(N > 0){
      Svals[i] <- sum(rexp(N, beta))
    }
  }
  return(Svals)
}

ui <- fluidPage(
  titlePanel("Compound Poisson Process S(t) – Sensitivity Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("lambda", "Interarrival Rate λ:", 
                  min = 0.1, max = 5, value = 1, step = 0.1),
      
      sliderInput("beta", "Jump Size Rate β:", 
                  min = 0.1, max = 5, value = 1, step = 0.1),
      
      sliderInput("nrep", "Number of Simulations:", 
                  min = 500, max = 20000, value = 5000, step = 500),
      
      actionButton("simulate", "Simulate Now", 
                   class = "btn btn-primary")  # ← NEW BUTTON
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        # ---------------- HISTOGRAM TAB ----------------
        tabPanel("Histograms",
                 h3("Histograms of S(t)"),
                 fluidRow(
                   column(6, plotOutput("hist10")),
                   column(6, plotOutput("hist100"))
                 ),
                 fluidRow(
                   column(6, plotOutput("hist1000")),
                   column(6, plotOutput("hist10000"))
                 )
        ),
        
        # ---------------- SUMMARY STAT TAB ----------------
        tabPanel("Summary Statistics",
                 h3("Summary Statistics for S(t)"),
                 tableOutput("summaryTable")
        )
      )
    )
  )
)

server <- function(input, output){
  
  # Run all simulations ONLY when the user presses the button
  allSims <- eventReactive(input$simulate, {
    
    list(
      sim10 = simulate_S(10, input$lambda, input$beta, input$nrep),
      sim100 = simulate_S(100, input$lambda, input$beta, input$nrep),
      sim1000 = simulate_S(1000, input$lambda, input$beta, input$nrep),
      sim10000 = simulate_S(10000, input$lambda, input$beta, input$nrep)
    )
    
  })
  
  # ---------------- HISTOGRAMS ----------------
  output$hist10 <- renderPlot({
    req(allSims())
    hist(allSims()$sim10, col="blue", main="S(10)", xlab="Value", breaks=40)
  })
  
  output$hist100 <- renderPlot({
    req(allSims())
    hist(allSims()$sim100, col="turquoise", main="S(100)", xlab="Value", breaks=40)
  })
  
  output$hist1000 <- renderPlot({
    req(allSims())
    hist(allSims()$sim1000, col="lightgreen", main="S(1000)", xlab="Value", breaks=40)
  })
  
  output$hist10000 <- renderPlot({
    req(allSims())
    hist(allSims()$sim10000, col="red", main="S(10000)", xlab="Value", breaks=40)
  })
  
  # ---------------- SUMMARY STATISTICS ----------------
  output$summaryTable <- renderTable({
    
    req(allSims())
    
    summarise_stats <- function(x){
      data.frame(
        Mean = mean(x),
        SD = sd(x),
        Min = min(x),
        Q1 = quantile(x, 0.25),
        Median = median(x),
        Q3 = quantile(x, 0.75),
        Max = max(x)
      )
    }
    
    stats <- rbind(
      cbind(Time="t = 10", summarise_stats(allSims()$sim10)),
      cbind(Time="t = 100", summarise_stats(allSims()$sim100)),
      cbind(Time="t = 1000", summarise_stats(allSims()$sim1000)),
      cbind(Time="t = 10000", summarise_stats(allSims()$sim10000))
    )
    
    stats
  }, rownames = FALSE)
  
}

shinyApp(ui = ui, server = server)
