library(shiny)
library(ggplot2)
library(gridExtra)

F_prob <- function(df1, df2, F_value) {
  prob <- round(pf(F_value, df1, df2, lower.tail = FALSE), 3)
  
  dF_spec <- function(x_val) {
    df(x = x_val, df1, df2)
  }
  
  p_orig <- 
    ggplot(data.frame(x = c(0, F_value + 5)), aes(x)) +
    stat_function(fun = dF_spec, n = 1000) + 
    stat_function(fun = dF_spec, 
                  xlim = c(F_value, F_value + 5),
                  geom = "area", fill = "dodgerblue1", alpha = .3) +
    scale_x_continuous("F Values") + ylab("Density") +
    geom_vline(xintercept = F_value, lty = 2, col = "red") +
    ggtitle(paste0("F Distribution on ", df1, ", ", df2, " degrees of freedom"), 
            subtitle = paste0("P(F > ", F_value, ") = ", prob))

  return(p_orig)
}

ui <- shinyUI(
  fluidPage(
    titlePanel("Calculating Probability in the Upper Tail of an F-Distribution"),
    sidebarLayout(
      sidebarPanel(textInput("df1", "DF1", "2"),
                   textInput("df2", "DF2", "100"),
                   textInput("F_value", "F Value", "1")),
      mainPanel(plotOutput("plot_out"))
    ),
    hr(),
    print("Created by Adam Ciarleglio.")
  )
)

server <- function(input, output) {
  output$plot_out <- renderPlot({
    df1 <- as.numeric(input$df1)
    df2 <- as.numeric(input$df2)
    F_value <- as.numeric(input$F_value)
    F_prob(df1, df2, F_value)
  })
  
}
shinyApp(ui = ui, server = server)
