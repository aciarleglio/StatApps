library(shiny)
library(ggplot2)
library(gridExtra)

chisq_prob <- function(df, X2_value) {
  prob <- round(pchisq(X2_value, df, lower.tail = FALSE), 3)
  
  dchisq_spec <- function(x_val) {
    dchisq(x = x_val, df)
  }
  
  p_orig <-
    ggplot(data.frame(x = c(0, df*5)), aes(x)) +
    stat_function(fun = dchisq_spec, n = 1000) +
    stat_function(fun = dchisq_spec,
                  xlim = c(X2_value, df*5),
                  geom = "area", fill = "dodgerblue1", alpha = .3) +
    scale_x_continuous("Chi-Squared Values") + ylab("Density") +
    geom_vline(xintercept = X2_value, lty = 2, col = "red") +
    ggtitle(paste0("Chi-Squared Distribution on ", df, " degrees of freedom"),
            subtitle = paste0("P(X-Squared > ", X2_value, ") = ", prob))
  
  return(p_orig)
}

ui <- shinyUI(
  fluidPage(
    titlePanel("Calculating Probability in the Upper Tail of an Chi-Squared Distribution"),
    sidebarLayout(
      sidebarPanel(textInput("df", "DF", "1"),
                   textInput("X2_value", "X-Squared", "1")),
      mainPanel(plotOutput("plot_out"))
    ),
    hr(),
    print("Created by Adam Ciarleglio.")
  )
)

server <- function(input, output) {
  output$plot_out <- renderPlot({
    df <- as.numeric(input$df)
    X2_value <- as.numeric(input$X2_value)
    chisq_prob(df,X2_value)
  })
  
}
shinyApp(ui = ui, server = server)