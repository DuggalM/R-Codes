library(ggplot2)

Piv1 <- read.csv("C:/Personal/R/Pivot1.csv",header=TRUE,
                 quote="\"",
                 stringsAsFactors= TRUE,
                 strip.white = TRUE)

ui <- fluidPage(
  fluidRow(
    column(width = 6,
           plotOutput("plot1", height = 350,
                      click = "plot1_click",
                      brush = brushOpts(
                        id = "plot1_brush"
                      )
           ),
           actionButton("exclude_toggle", "Toggle points"),
           actionButton("exclude_reset", "Reset")
    )
  )
)

server <- function(input, output) {
  # For storing which rows have been excluded
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(Piv1))
  )
  
  output$plot1 <- renderPlot({
    # Plot the kept and excluded points as two separate data sets
    keep    <- Piv1[ vals$keeprows, , drop = FALSE]
    exclude <- Piv1[!vals$keeprows, , drop = FALSE]
    
    ggplot(keep, aes(lane.kms, cnts)) + geom_point(aes(color = factor(vdf))) +
      geom_smooth(method = lm, fullrange = TRUE, shape = 21, color = "black") +
      geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25) +
      coord_cartesian(xlim = c(0, 1200), ylim = c(0,300))
  })
  
  # Toggle points that are clicked
  observeEvent(input$plot1_click, {
    res <- nearPoints(Piv1, input$plot1_click, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(Piv1, input$plot1_brush, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(Piv1))
  })
  
}

shinyApp(ui, server)