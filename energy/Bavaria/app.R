#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(scales)

energy_data <- read_csv("bavaria.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    tags$head(
      tags$style(HTML("
                      #info {
                      max-width: 500px;
                      }
                      "))  
    ),
    
    # Application title
    titlePanel("Energy Conflict in Bavaria"),


        # Show a plot of the generated distribution
        mainPanel(
            width = "100%",
            div(
                style = "position: relative",
                plotOutput("plot",
                           width = "90%",
                           hover = hoverOpts(
                               "plot_hover", delay = 100, delayType = "debounce"
                           )),
                uiOutput("hover_info")
            ),
           dateRangeInput("daterange", "Select Date Range (YYYY-MM-DD)", start = "1994-01-01", end = "2019-12-31", min = "1994-01-01",
                          max = "2019-12-31", format = "yyyy-mm-dd", startview = "decade", weekstart = 0,
                          separator = " to ", width = NULL),
           verbatimTextOutput("info", placeholder = FALSE)
        )
)
        

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$plot <- renderPlot({
        ggplot(energy_data, aes(x = date)) +
            geom_line(aes(y = count, color = type)) +
            scale_x_date(date_breaks = "1 year", date_labels = "%b-%Y", expand = c(0,0), limits = input$daterange) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
                  panel.grid.minor = element_blank())
    })
    
    #Set up the hover boxes
    output$hover_info <- renderUI({
        hover <- input$plot_hover
        point <- nearPoints(energy_data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)
        
        # calculate point position INSIDE the image as percent of total dimensions
        # from left (horizontal) and from top (vertical)
        left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
        top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
        
        # calculate distance from left and bottom side of the picture in pixels
        left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
        
        # create style property fot tooltip
        # background color is set so tooltip is a bit transparent
        # z-index is set so we are sure are tooltip will be on top
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px + 2, "px; top:", top_px + 2, "px;")
        
        output$info <- renderText({
            paste("Articles: ", point$title, sep="\n")
        })
        
        
        #actual toolitp created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<b>Category: </b>", point$type, "</br>",
                          "<b>Publication Date: </b>", point$date, "</br>",
                          "<b>Mentions: </b>", point$count, "</br>"
                          #"<b>Articles: </b></br>", point$title, "</br>"
                          
            )
            
            )
            )
        )
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
