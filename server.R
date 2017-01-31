library(ggplot2)
library(shiny)

Piv1 <- read.csv("C:/Personal/R/Pivot1.csv",header=TRUE,
               quote="\"",
               stringsAsFactors= TRUE,
               strip.white = TRUE)

server <- function(input, output) {
  env <- environment()
  output$plot1 <- renderPlot({
    if (input$plot_type == "base") {
      plot(Piv1$lane.kms, Piv1$cnts, pch = 21, col = "red")
    } else if (input$plot_type == "ggplot2") {
      ggplot(Piv1, aes_string("lane.kms", "cnts"), environment = env) + 
        geom_point(aes_string(color = "CDNAME"), environment = env)
    }
  })
  
  output$click_info <- renderPrint({
    cat("input$plot_click:\n")
    str(input$plot_click)
  })
  output$hover_info <- renderPrint({
    cat("input$plot_hover:\n")
    str(input$plot_hover)
  })
  output$dblclick_info <- renderPrint({
    cat("input$plot_dblclick:\n")
    str(input$plot_dblclick)
  })
  output$brush_info <- renderPrint({
    cat("input$plot_brush:\n")
    str(input$plot_brush)
  })
  
}