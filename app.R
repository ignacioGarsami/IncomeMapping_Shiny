#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(leaflet)
library(RColorBrewer)
source('utils/utils.R')

data = data_downloader()

state_names = unique(data$State_Name)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


dataSelection = absolutePanel(top = 10, right = 10,
                               sliderInput("range", "Mean Income", min(data$Mean), max(data$Mean),
                                           value = range(data$Mean), step = 0.1
                               ),
                               selectInput("selState", 
                                           label="Select State", 
                                           multiple = TRUE,
                                           choices = state_names),
                               selectInput("colors", "Color Scheme",
                                           rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                               ),
                               checkboxInput("legend", "Show legend", TRUE)
                )

# headerRow = div(id='header', useShinyjs(),
#                 absolutePanel(
#                               sliderInput("range", "Mean Income", min(data$Mean), max(data$Mean),
#                                           value = range(data$Mean), step = 0.1
#                               ),
#                               selectInput("selState", 
#                                           label="Select State", 
#                                           multiple = TRUE,
#                                           choices = state_names),
#                               selectInput("colors", "Color Scheme",
#                                           rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
#                               ),
#                               checkboxInput("legend", "Show legend", TRUE)
# ))


map = leafletOutput("map", width = "100%", height = "90vh")

mapPanel = tabPanel('Map of incomes in the US',
                    fluidRow(
                        column(width = 10, map),
                        column(width = 2, dataSelection)
                    )
            )


dataPanel <- tabPanel("Data",
                      tableOutput('dataTable')
                      
)


# ui = navbarPage('US Income', id = 'navBar',header = headerRow, mapPanel, dataPanel)
ui = navbarPage('US Income', id = 'navBar', mapPanel, dataPanel)


# Define server logic required to draw a histogram
server <- function(input, output) {
# 
#     observe(if(input$navBar=="Map of incomes in the US") {
#         cat(file=stderr(), input$navBar, "\n")
#         shinyjs::hide("header")
#     } else {
#         cat(file=stderr(), input$navBar, "\n")
#         shinyjs::show("header")
#     })
#     
    
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
        if(is.null(input$selState)){
            data[data$Mean >= input$range[1] & data$Mean <= input$range[2],]
        }else{
            data[data$Mean >= input$range[1] & data$Mean <= input$range[2] & data$State_Name == input$selState,]
        }

    })
    
    
    # This reactive expression represents the palette function,
    # which changes as the user makes selections in UI.
    colorpal <- reactive({
        colorNumeric(input$colors, data$Mean)
    })
    
    output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated)
        leaflet(data) %>% addTiles() %>%
            fitBounds(~min(Lon), ~min(Lat), ~max(Lon), ~max(Lat))
    
    })
    
    # Incremental changes to the map (in this case, replacing the
    # circles when a new color is chosen) should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
        pal <- colorpal()
        
        leafletProxy("map", data = filteredData()) %>%
            clearMarkers() %>%
            addCircleMarkers( weight = 1, color = "#777777",
                       fillColor = ~pal(Mean), fillOpacity = 0.7, popup = ~paste("State:", State_Name, "<br>",
                                                                                "County:", County, "<br>",
                                                                                "City:", City, "<br>",
                                                                                "Place:", Place, "<br>",
                                                                                "Zip Code:", Zip_Code, "<br>",
                                                                                "Mean Income:", Mean
                           
                       )
            )
    })
    
    
    # Use a separate observer to recreate the legend as needed.
    observe({
        proxy <- leafletProxy("map", data = data)
        
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% clearControls()
        if (input$legend) {
            pal <- colorpal()
            proxy %>% addLegend(position = "bottomright",
                                pal = pal, values = ~Mean
            )
        }
    })
    
    output$dataTable <- renderTable({
        filteredData()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


