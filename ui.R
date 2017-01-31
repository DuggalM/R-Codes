library(ggplot2)
library(shiny)

ui <- fluidPage(
  # Some custom CSS for a smaller font for preformatted text
  tags$head(
    tags$style(HTML("
                    pre, table.table {
                    font-size: smaller;
                    }
                    "))
    ),
  
  fluidRow(
    column(width = 4, wellPanel(
      radioButtons("plot_type", "Plot type",
                   c("base", "ggplot2")
      )
    )),
    column(width = 4,
           # In a plotOutput, passing values for click, dblclick, hover, or brush
           # will enable those interactions.
           plotOutput("plot1", height = 350,
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      click = "plot_click",
                      dblclick = dblclickOpts(
                        id = "plot_dblclick"
                      ),
                      hover = hoverOpts(
                        id = "plot_hover"
                      ),
                      brush = brushOpts(
                        id = "plot_brush"
                      )
           )
    )
  ),
  fluidRow(
    column(width = 3,
           verbatimTextOutput("click_info")
    ),
    column(width = 3,
           verbatimTextOutput("dblclick_info")
    ),
    column(width = 3,
           verbatimTextOutput("hover_info")
    ),
    column(width = 3,
           verbatimTextOutput("brush_info")
    )
  )
    )