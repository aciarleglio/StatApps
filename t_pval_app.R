library(shiny)
library(ggplot2)
library(gridExtra)

p_val_t <- function(test_stat_val, deg_fr, alpha_val = 0.05, alternative = "two.sided") {
  
  dt_spec <- function(x_val) {
    dt(x = x_val, df = deg_fr)
  }
  vert_loc_mid1 <- max(dt_spec(seq(-5,5, length.out = 100))) / 2
  vert_loc_mid2 <- vert_loc_mid1 - 0.11*vert_loc_mid1
  vert_loc_mid3 <- vert_loc_mid2 - 0.11*vert_loc_mid1
  vert_loc_mid4 <- vert_loc_mid3 - 0.11*vert_loc_mid1
  
  if(alternative == "two.sided") {
    prob_want <- paste0("P(|T| > ", test_stat_val, ")")
    crit_val <- abs(qt(alpha_val/2, df = deg_fr))
    p <- 2*pt(abs(test_stat_val), df = deg_fr, lower.tail = FALSE)
    p_t_plot <- 
      ggplot(data.frame(x = c(-5, 5)), aes(x)) +
      stat_function(fun = dt_spec) + 
      stat_function(fun = dt_spec, 
                    xlim = c(-5, -abs(test_stat_val)),
                    geom = "area", fill = "dodgerblue1", alpha = .5) + 
      stat_function(fun = dt_spec, 
                    xlim = c(abs(test_stat_val), 5),
                    geom = "area", fill = "dodgerblue1", alpha = .5) +
      geom_vline(xintercept = crit_val, lty = 2, col = "red") +
      geom_vline(xintercept = -crit_val, lty = 2, col = "red") +
      geom_vline(xintercept = test_stat_val, lty = 1, lwd = 1.2, col = "tomato3") +
      scale_x_continuous(breaks = -5:5, labels = -5:5) +
      xlab("t values") + ylab("Density") +
      annotate("text", x = 1.05*c(-crit_val, crit_val), y = vert_loc_mid1, color = "blue",
               label = c("Lower", "Upper"), hjust = c(1, 0)) +
      annotate("text", x = 1.05*c(-crit_val, crit_val), y = vert_loc_mid2, color = "blue",
               label = c("Critical", "Critical"), hjust = c(1, 0)) +
      annotate("text", x = 1.05*c(-crit_val, crit_val), y = vert_loc_mid3, color = "blue",
               label = c("Value", "Value"), hjust = c(1, 0)) +
      annotate("text", x = 1.05*c(-crit_val, crit_val), y = vert_loc_mid4, color = "blue",
               label = c(round(-crit_val, 2), round(crit_val, 2)), hjust = c(1, 0)) +
      annotate("text", x = 1.2*test_stat_val, y = 2*vert_loc_mid1, angle = 90, color = "blue",
               label = paste0("Test Stat. = ", test_stat_val), hjust = 1) +
      ggtitle(paste0("t Distribution with ", deg_fr, " Degrees of Freedom"), 
              subtitle = bquote(.(paste0("p-value =  ", prob_want, " = ", round(p, 4), "; Critical Value = ±", round(crit_val, 2), " for ")) ~
                                  alpha ~ .(paste0("= ", alpha_val))))
  }
  
  if(alternative == "greater") {
    prob_want <- paste0("P(T > ", test_stat_val, ")")
    crit_val <- qt(alpha_val, df = deg_fr, lower.tail = FALSE)
    p <- pt(test_stat_val, df = deg_fr, lower.tail = FALSE)
    p_t_plot <- 
      ggplot(data.frame(x = c(-5, 5)), aes(x)) +
      stat_function(fun = dt_spec) + 
      stat_function(fun = dt_spec, 
                    xlim = c(test_stat_val, 5),
                    geom = "area", fill = "dodgerblue1", alpha = .5) +
      geom_vline(xintercept = crit_val, lty = 2, col = "blue") +
      geom_vline(xintercept = test_stat_val, lty = 1, lwd = 1.2, col = "tomato3") +
      scale_x_continuous(breaks = -5:5, labels = -5:5) +
      xlab("t values") + ylab("Density") +
      annotate("text", x = 1.05*crit_val, y = vert_loc_mid1, color = "blue", label = "Critical", hjust = 0) +
      annotate("text", x = 1.05*crit_val, y = vert_loc_mid2, color = "blue", label = "Value", hjust = 0) +
      annotate("text", x = 1.05*crit_val, y = vert_loc_mid3, color = "blue", label = round(crit_val, 2), hjust = 0) +
      annotate("text", x = 1.2*test_stat_val, y = 2*vert_loc_mid1, angle = 90, color = "tomato3",
               label = paste0("Test Stat. = ", test_stat_val), hjust = 1) +
      ggtitle(paste0("t Distribution with ", deg_fr, " Degrees of Freedom"), 
              subtitle = bquote(.(paste0("p-value =  ", prob_want, " = ", round(p, 4), "; Critical Value = ", round(crit_val, 2), " for ")) ~
                                  alpha ~ .(paste0("= ", alpha_val))))
  }
  
  if(alternative == "less") {
    prob_want <- paste0("P(T < ", test_stat_val, ")")
    crit_val <- qt(alpha_val, df = deg_fr, lower.tail = TRUE)
    p <- pt(test_stat_val, df = deg_fr, lower.tail = TRUE)
    p_t_plot <- 
      ggplot(data.frame(x = c(-5, 5)), aes(x)) +
      stat_function(fun = dt_spec) + 
      stat_function(fun = dt_spec, 
                    xlim = c(-5, test_stat_val),
                    geom = "area", fill = "dodgerblue1", alpha = .5) +
      geom_vline(xintercept = crit_val, lty = 2, col = "red") +
      geom_vline(xintercept = test_stat_val, lty = 1, lwd = 1.2, col = "tomato3") +
      scale_x_continuous(breaks = -5:5, labels = -5:5) +
      xlab("t values") + ylab("Density") +
      annotate("text", x = 1.05*crit_val, y = vert_loc_mid1, color = "blue", label = "Critical", hjust = 1) +
      annotate("text", x = 1.05*crit_val, y = vert_loc_mid2, color = "blue", label = "Value", hjust = 1) +
      annotate("text", x = 1.05*crit_val, y = vert_loc_mid3, color = "blue", label = round(crit_val, 2), hjust = 1) +
      annotate("text", x = 1.2*test_stat_val, y = 2*vert_loc_mid1, angle = 90, color = "blue",
               label = paste0("Test Stat. = ", test_stat_val), hjust = 1) +
      ggtitle(paste0("t Distribution with ", deg_fr, " Degrees of Freedom"), 
              subtitle = bquote(.(paste0("p-value =  ", prob_want, " = ", round(p, 4), "; Critical Value = ", round(crit_val, 2), " for ")) ~
                                  alpha ~ .(paste0("= ", alpha_val))))
  }
  return(p_t_plot)
}


ui <- shinyUI(
  fluidPage(
    titlePanel("Calculating P-Values for t-Tests"),
    sidebarLayout(
      sidebarPanel(textInput("test_stat_val", "Test Statistic Value", "0"),
                   textInput("deg_fr", "Degrees of Freedom", "30"),
                   textInput("alpha_val", "Significance Level", "0.05"),
                   selectInput("alternative", "Alternative Hypothesis:",
                               c("Greater" = "greater",
                                 "Less" = "less",
                                 "Two Sided" = "two.sided"))),
      mainPanel(plotOutput("plot_out"))
    ),
    hr(),
    print("Created by Adam Ciarleglio.")
  )
)

server <- function(input, output) {
  output$plot_out <- renderPlot({
    test_stat_val <- as.numeric(input$test_stat_val)
    deg_fr <- as.numeric(input$deg_fr)
    alpha_val <- as.numeric(input$alpha_val)
    alternative <- input$alternative
    p_val_t(test_stat_val, deg_fr, alpha_val, alternative)
  })
  
}
shinyApp(ui = ui, server = server)
