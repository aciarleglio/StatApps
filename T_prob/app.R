library(shiny)
library(ggplot2)

area_t <- function(min_val, max_val, df) {
  
  min_val <- as.numeric(min_val)
  max_val <- as.numeric(max_val)
  df <- as.numeric(df)
  
  if(min_val > max_val) stop("Need min_val > max_val.")
  if(df < 1) stop("Need df >= 1.")
  
  min_val_name <- as.character(min_val)
  max_val_name <- as.character(max_val)

  prob_want <- paste0("P(", min_val," < T < ", max_val, ")")

  if(min_val == -Inf & max_val != Inf) {
    min_val <- -8
    min_val_name <- "-Inf"
    prob_want <- paste0("P(T < ", max_val,")")
  }  
  if(max_val == Inf & min_val != -Inf) {
    max_val <- 8
    max_val_name <- "Inf"
    prob_want <- paste0("P(T > ", min_val,")")
  }
  
  if(min_val == -Inf & max_val == Inf) {
    min_val <- -8
    min_val_name <- "-Inf"
    max_val <- 8
    max_val_name <- "Inf"
    prob_want <- "P(-Inf < T < Inf)"
  }
  
  dt_spec <- function(x_val) {
    dt(x = x_val, df = df)
  }
  
  t_area <- pt(max_val, df = df) - pt(min_val, df = df)
  breaks_t <- c(seq(-4,4), min_val, max_val)
  
  p_orig <- 
    ggplot(data.frame(x = c(-4, 4)), aes(x)) +
    stat_function(fun = dt_spec, n = 1000) + 
    stat_function(fun = dt_spec, 
                  xlim = c(min_val, max_val),
                  geom = "area", fill = "dodgerblue1", alpha = .3) +
    scale_x_continuous("t Values", breaks = breaks_t, 
                       labels = breaks_t) +
    theme(axis.text.x = element_text(color = c(rep("grey",11), "blue", "blue"),
                                     face = c(rep("plain",11), "bold", "bold"))) +
    ylab("Density") +
    geom_vline(xintercept = min_val, lty = 2, col = "red") + 
    geom_vline(xintercept = max_val, lty = 2, col = "red") +
    annotate("text", x = 3, y = 0.2, 
             label = paste("Probability =", round(t_area, 2)), color = "blue", size = 5) +
    ggtitle(paste0("t Distribution: T ~ t(",df,")"),
            subtitle =  paste0("Want ", prob_want))
  
  p_orig
}

ui <- shinyUI(
  fluidPage(
    titlePanel("Calculating Probabilities for a t Random Variable"),
    sidebarLayout(
      sidebarPanel(textInput("df", "Degrees of Freedom (df)", "1"),
                   textInput("min_val", "Minimum Value of X", "-1"),
                   textInput("max_val", "Maximum Value of X", "1")),
      mainPanel(plotOutput("plot_out"))
    ),
    hr(),
    print("Created by Adam Ciarleglio.")
  )
)

server <- function(input, output) {
  output$plot_out <- renderPlot({
    min_val <- input$min_val
    max_val <- input$max_val
    df <- input$df
    area_t(min_val, max_val, df)
  })
  
}
shinyApp(ui = ui, server = server)
