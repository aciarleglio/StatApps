library(shiny)
library(ggplot2)

plot_norm_crit <- function(tail_type, tail_prob) {
  if(tail_type == 1) { # upper tail
    crit_val <- round(qnorm(tail_prob, lower.tail = FALSE), 2)
    p_standard <- 
      ggplot(data.frame(x = c(-4, 4)), aes(x)) +
      stat_function(fun = dnorm, n = 1000) + 
      stat_function(fun = dnorm, 
                    xlim = c(crit_val, 4),
                    geom = "area", fill = "dodgerblue1", alpha = .3) + 
      scale_x_continuous("Z Values", breaks = c(-4, crit_val, 4),
                         labels = c(-4, crit_val, 4)) +
      theme(axis.text.x = element_text(color = c(rep("grey",11), "blue", "blue"),
                                       face = c(rep("plain",11), "bold", "bold"))) +
      ylab("Density") +
      geom_vline(xintercept = crit_val, lty = 2, col = "red") + 
      annotate("text", x = 3, y = 0.2, 
               label = paste("Critical Value =", crit_val), color = "blue", size = 5) +
      ggtitle(paste0("Critical Value = ", crit_val, " cuts off ", tail_prob, " area in upper tail."))
    
  }
  if(tail_type == 2) { # lower tail
    crit_val <- round(qnorm(tail_prob, lower.tail = TRUE), 2)
    p_standard <- 
      ggplot(data.frame(x = c(-4, 4)), aes(x)) +
      stat_function(fun = dnorm, n = 1000) + 
      stat_function(fun = dnorm, 
                    xlim = c(-4, crit_val),
                    geom = "area", fill = "dodgerblue1", alpha = .3) + 
      scale_x_continuous("Z Values", breaks = c(-4, crit_val, 4),
                         labels = c(-4, crit_val, 4)) +
      theme(axis.text.x = element_text(color = c(rep("grey",11), "blue", "blue"),
                                       face = c(rep("plain",11), "bold", "bold"))) +
      ylab("Density") +
      geom_vline(xintercept = crit_val, lty = 2, col = "red") + 
      annotate("text", x = -3, y = 0.2, 
               label = paste("Critical Value =", crit_val), color = "blue", size = 5) +
      ggtitle(paste0("Critical Value = ", crit_val, " cuts off ", tail_prob, " area in lower tail."))
  } 
  if(tail_type == 3) { # both tails
    crit_val_up <- round(qnorm(tail_prob/2, lower.tail = FALSE), 2)
    crit_val_lw <- -crit_val_up
    p_standard <- 
      ggplot(data.frame(x = c(-4, 4)), aes(x)) +
      stat_function(fun = dnorm, n = 1000) + 
      stat_function(fun = dnorm, 
                    xlim = c(-4, crit_val_lw),
                    geom = "area", fill = "dodgerblue1", alpha = .3) + 
      stat_function(fun = dnorm, 
                    xlim = c(crit_val_up, 4),
                    geom = "area", fill = "dodgerblue1", alpha = .3) +
      scale_x_continuous("Z Values", breaks = c(-4, crit_val_lw, crit_val_up, 4),
                         labels = c(-4, crit_val_lw, crit_val_up, 4)) +
      theme(axis.text.x = element_text(color = c(rep("grey",11), "blue", "blue"),
                                       face = c(rep("plain",11), "bold", "bold"))) +
      ylab("Density") +
      geom_vline(xintercept = crit_val_lw, lty = 2, col = "red") +
      geom_vline(xintercept = crit_val_up, lty = 2, col = "red") +
      annotate("text", x = 0, y = 0.35, 
               label = paste("Critical Values =", crit_val_lw, " and ", crit_val_up), color = "blue", size = 5) +
      ggtitle(paste0("Critical Values = ", crit_val_lw, " and ", crit_val_up, " cut off ", tail_prob, " area in both tails."))
  }
  p_standard
}


ui <- shinyUI(
  fluidPage(
    titlePanel("Calculating Critical Values for a Standard Normal Random Variable"),
    sidebarLayout(
      sidebarPanel(radioButtons("radio", "Area in:",
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
    tail_type <- input$radio
    tail_prob <- as.numeric(input$tail_prob)
    plot_norm_crit(tail_type, tail_prob)
  })
  
}
shinyApp(ui = ui, server = server)