library(shiny)
library(tidyverse)

binomial_plot <- function(N, P) {
  X <- 0:N
  dat <- tibble(X, Y = dbinom(X, N, P))
  bin_plot <- 
  dat %>% 
    ggplot(aes(x = factor(X), y = Y)) + 
    geom_bar(stat = "identity", col = "dodgerblue", fill = "dodgerblue") +
    geom_text(aes(label = round(Y, 3)), position = position_dodge(width = 0.9), 
              vjust = -0.25, size = 4.5, color = "maroon") +
    ylab("Probability") + xlab("X") + 
    ggtitle(paste0("X ~ Bin(n = ", N, ", p = ", P,")" )) +
    theme(plot.title = element_text(size = 22))
  bin_plot
}

ui <- shinyUI(
  fluidPage(
    titlePanel("Calculating Binomial Probabilities"),
    sidebarLayout(
      sidebarPanel(textInput("N", "Number of Trials", "1"),
                   textInput("P", "Probability of Success", "1")),
      mainPanel(plotOutput("plot_out"))
    ),
    hr(),
    print("Created by Adam Ciarleglio.")
  )
)

server <- function(input, output) {
  output$plot_out <- renderPlot({
    N <- as.numeric(input$N)
    P <- as.numeric(input$P)
    binomial_plot(N,P)
  })
  
}
shinyApp(ui = ui, server = server)
