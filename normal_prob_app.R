library(shiny)
library(ggplot2)
library(gridExtra)

area_normal <- function(min_val, max_val, mean_val, sd_val) {
  
  min_val <- as.numeric(min_val)
  max_val <- as.numeric(max_val)
  mean_val <- as.numeric(mean_val)
  sd_val <- as.numeric(sd_val)
  
  if(min_val > max_val) stop("Need min_val > max_val.")
  if(sd_val < 0) stop("Need sd_val >= 0.")
  
  min_val_name <- as.character(min_val)
  max_val_name <- as.character(max_val)
  
  z_min <- (min_val - mean_val) / sd_val
  z_max <- (max_val - mean_val) / sd_val
  
  min_z_name <- as.character(round(z_min, 2))
  max_z_name <- as.character(round(z_max, 2))
  
  prob_want <- paste0("P(", min_val," < X < ", max_val, ")")
  prob_want_z <- paste0("P(", min_z_name," < Z < ", max_z_name, ")")
  if(min_val == -Inf & max_val != Inf) {
    min_val <- mean_val - 5*sd_val
    min_val_name <- min_z_name <- "-Inf"
    z_min <- (min_val - mean_val) / sd_val
    prob_want <- paste0("P(X < ", max_val,")")
    prob_want_z <- paste0("P(Z < ", max_z_name,")")
  }  
  if(max_val == Inf & min_val != -Inf) {
    max_val <- mean_val + 5*sd_val
    max_val_name <- max_z_name <- "Inf"
    z_max <- (max_val - mean_val) / sd_val
    prob_want <- paste0("P(X > ", min_val,")")
    prob_want_z <- paste0("P(Z > ", min_z_name,")")
  }
  
  if(min_val == -Inf & max_val == Inf) {
    min_val <- mean_val - 5*sd_val
    min_val_name <- min_z_name <- "-Inf"
    z_min <- (min_val - mean_val) / sd_val
    max_val <- mean_val + 5*sd_val
    max_val_name <- max_z_name <- "Inf"
    z_max <- (max_val - mean_val) / sd_val
    prob_want <- "P(-Inf < X < Inf)"
    prob_want_z <- "P(-Inf < Z < Inf)"
  }
  
  dnorm_spec <- function(x_val) {
    dnorm(x = x_val, mean = mean_val, sd = sd_val)
  }
  
  z_area <- pnorm(z_max) - pnorm(z_min)
  
  breaks_orig <- c(mean_val - (5:1)*sd_val, mean_val, mean_val + (1:5)*sd_val, min_val, max_val)
  breaks_z <- c(seq(-5,5), z_min, z_max)
  
  p_orig <- 
    ggplot(data.frame(x = c(mean_val - 5*sd_val, mean_val + 5*sd_val)), aes(x)) +
    stat_function(fun = dnorm_spec) + 
    stat_function(fun = dnorm_spec, 
                  xlim = c(min_val, max_val),
                  geom = "area", fill = "dodgerblue1", alpha = .3) +
    scale_x_continuous("X = Original Values", breaks = breaks_orig, 
                       labels = c(breaks_orig[1:11], min_val_name, max_val_name)) +
    theme(axis.text.x = element_text(color = c(rep("grey",11), "blue", "blue"),
                                     face = c(rep("plain",11), "bold", "bold"))) +
    ylab("Density") +
    geom_vline(xintercept = mean_val, lty = 2, col = "blue") +
    geom_vline(xintercept = min_val, lty = 2, col = "red") + 
    geom_vline(xintercept = max_val, lty = 2, col = "red") +
    #ggtitle(paste0("Original Distribution: Want ", prob_want))
    ggtitle(paste0("Original Distribution: X ~ N(", mean_val, ",", sd_val,"^2)"),
            subtitle =  paste0("Want ", prob_want))
  
  p_standard <- 
    ggplot(data.frame(x = c(-5, 5)), aes(x)) +
    stat_function(fun = dnorm) + 
    stat_function(fun = dnorm, 
                  xlim = c(z_min, z_max),
                  geom = "area", fill = "dodgerblue1", alpha = .3) + 
    scale_x_continuous("Z = Standardized Values", breaks = breaks_z,
                       labels = c(-5:5, min_z_name, max_z_name)) +
    theme(axis.text.x = element_text(color = c(rep("grey",11), "blue", "blue"),
                                     face = c(rep("plain",11), "bold", "bold"))) +
    ylab("Density") +
    geom_vline(xintercept = 0, lty = 2, col = "blue") +
    geom_vline(xintercept = z_min, lty = 2, col = "red") + 
    geom_vline(xintercept = z_max, lty = 2, col = "red") +
    annotate("text", x = 3, y = 0.2, 
             label = paste("Probability =", round(z_area, 2)), color = "blue", size = 5) +
    ggtitle("Standard Normal Distribution: Z ~ N(0,1)", subtitle = paste0("Equivalent to ", prob_want_z))
  
  grid.arrange(p_orig, p_standard, nrow = 2)
}

ui <- shinyUI(
  fluidPage(
  titlePanel("Calculating Probabilities for a Normal Random Variable"),
  sidebarLayout(
    sidebarPanel(textInput("mean_val", "Mean of X", "0"),
                 textInput("sd_val", "Standard Deviation of X", "1"),
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
    mean_val <- input$mean_val
    sd_val <- input$sd_val
    area_normal(min_val, max_val, mean_val, sd_val)
  })
                                  
}
shinyApp(ui = ui, server = server)
