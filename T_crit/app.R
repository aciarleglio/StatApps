library(shiny)
library(ggplot2)

plot_t_crit <- function(tail_type, tail_prob, df) {
  tail_type <- as.numeric(tail_type)
  tail_prob <- as.numeric(tail_prob)
  df <- as.numeric(df)
  dtfunc <- function(x) dt(x, df = df)
  
  if(tail_type == 1) { # upper tail
    crit_val <- round(qt(tail_prob, df = df, lower.tail = FALSE), 3)
    p_t <- 
      ggplot(data.frame(x = c(-6, 6)), aes(x)) +
      stat_function(fun = dtfunc, n = 1000) + 
      stat_function(fun = dtfunc, 
                    xlim = c(crit_val, 6),
                    geom = "area", fill = "dodgerblue1", alpha = .3) + 
      scale_x_continuous("t Values", breaks = c(-6, crit_val, 6),
                         labels = c(-6, crit_val, 6)) +
      theme(axis.text.x = element_text(color = c(rep("grey",11), "blue", "blue"),
                                       face = c(rep("plain",11), "bold", "bold"))) +
      ylab("Density") +
      geom_vline(xintercept = crit_val, lty = 2, col = "red") + 
      annotate("text", x = 3, y = 0.2, 
               label = paste("Critical Value =", crit_val), color = "blue", size = 5) +
      ggtitle(paste0("Critical Value = ", crit_val, " cuts off ", tail_prob, " area in upper tail of t(", df,")."))
    
  }
  if(tail_type == 2) { # lower tail
    crit_val <- round(qt(tail_prob, df = df, lower.tail = TRUE), 3)
    p_t <- 
      ggplot(data.frame(x = c(-6, 6)), aes(x)) +
      stat_function(fun = dtfunc, n = 1000) + 
      stat_function(fun = dtfunc, 
                    xlim = c(-6, crit_val),
                    geom = "area", fill = "dodgerblue1", alpha = .3) + 
      scale_x_continuous("t Values", breaks = c(-6, crit_val, 6),
                         labels = c(-6, crit_val, 6)) +
      theme(axis.text.x = element_text(color = c(rep("grey",11), "blue", "blue"),
                                       face = c(rep("plain",11), "bold", "bold"))) +
      ylab("Density") +
      geom_vline(xintercept = crit_val, lty = 2, col = "red") + 
      annotate("text", x = -3, y = 0.2, 
               label = paste("Critical Value =", crit_val), color = "blue", size = 5) +
      ggtitle(paste0("Critical Value = ", crit_val, " cuts off ", tail_prob, " area in lower tail of t(", df,")."))
  } 
  if(tail_type == 3) { # both tails
    crit_val_up <- round(qt(tail_prob/2, df = df, lower.tail = FALSE), 3)
    crit_val_lw <- -crit_val_up
    p_t <- 
      ggplot(data.frame(x = c(-6, 6)), aes(x)) +
      stat_function(fun = dtfunc, n = 1000) + 
      stat_function(fun = dtfunc, 
                    xlim = c(-6, crit_val_lw),
                    geom = "area", fill = "dodgerblue1", alpha = .3) + 
      stat_function(fun = dtfunc, 
                    xlim = c(crit_val_up, 6),
                    geom = "area", fill = "dodgerblue1", alpha = .3) +
      scale_x_continuous("t Values", breaks = c(-6, crit_val_lw, crit_val_up, 6),
                         labels = c(-6, crit_val_lw, crit_val_up, 6)) +
      theme(axis.text.x = element_text(color = c(rep("grey",11), "blue", "blue"),
                                       face = c(rep("plain",11), "bold", "bold"))) +
      ylab("Density") +
      geom_vline(xintercept = crit_val_lw, lty = 2, col = "red") +
      geom_vline(xintercept = crit_val_up, lty = 2, col = "red") +
      annotate("text", x = 0, y = 0.35, 
               label = paste("Critical Values =", crit_val_lw, " and ", crit_val_up), color = "blue", size = 5) +
      ggtitle(paste0("Critical Values = ", crit_val_lw, " and ", crit_val_up, " cut off ", tail_prob, " area in both tails of t(", df,")."))
  }
  p_t
}


ui <- shinyUI(
  fluidPage(
    titlePanel("Calculating Critical Values for a t Random Variable"),
    sidebarLayout(
      sidebarPanel(textInput("df", "Degrees of Freedom", "30"),
                   radioButtons("radio", "Area in:",
                                choices = list("Upper Tail" = 1, "Lower Tail" = 2,
                                               "Both Tails" = 3), selected = 1),
                   textInput("tail_prob", "Tail Probability", "0.05")),
      mainPanel(plotOutput("plot_out"))
    ),
    hr(),
    print("Created by Adam Ciarleglio.")
  )
)

server <- function(input, output) {
  output$plot_out <- renderPlot({
    df <- input$df
    tail_type <- input$radio
    tail_prob <- input$tail_prob
    plot_t_crit(tail_type, tail_prob, df)
  })
  
}
shinyApp(ui = ui, server = server)