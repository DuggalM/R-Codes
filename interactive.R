library(ggplot2)

Piv1 <- read.csv("C:/Personal/R/Pivot1.csv",header=TRUE,
                 quote="\"",
                 stringsAsFactors= TRUE,
                 strip.white = TRUE)


ui <- fluidPage(
  fluidRow(
    column(width = 4,
           plotOutput("plot1", height = 300,
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      click = "plot1_click",
                      brush = brushOpts(
                        id = "plot1_brush"
                      )
           )
    )
  ),
  fluidRow(
    column(width = 6,
           h4("Points near click"),
           verbatimTextOutput("click_info")
    ),
    column(width = 6,
           h4("Brushed points"),
           verbatimTextOutput("brush_info")
    )
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(Piv1, aes(lane.kms, cnts)) + geom_point(aes(color = factor(vdf)))
  })
  
  output$click_info <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    nearPoints(Piv1, input$plot1_click, addDist = TRUE)
  })
  
  output$brush_info <- renderPrint({
    brushedPoints(Piv1, input$plot1_brush)
  })
}

shinyApp(ui, server)