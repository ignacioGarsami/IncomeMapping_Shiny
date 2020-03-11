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
library(tidyverse)
library(plotly)
library(DT)
library(magrittr)
library(rmarkdown)
source('utils/utils.R')

data = data_downloader()

state_names = unique(data$State_Name)
county_names = unique(data$County)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


dataSelection = absolutePanel(top = 10, right = 10,
                              useShinyjs(),
                              sliderInput("range", "Mean Income", min(data$Mean), max(data$Mean),
                                          value = range(data$Mean), step = 0.1
                              ),
                              selectInput("selState", 
                                          label="Select State", 
                                          multiple = TRUE,
                                          choices = state_names),
                              selectInput("selCounty", 
                                          label="Select County", 
                                          multiple = TRUE,
                                          choices = county_names),
                              selectInput("colors", "Color Scheme",
                                          rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                              ),
                              checkboxInput("legend", "Show legend", TRUE),
                              h5(tags$b('Selected data')),
                              downloadButton(label = 'Download','downloadData'),
                              h5(tags$b('Statistical report')),
                              actionButton("generate", "Generate Report"),
                              br(),
                              br(),
                              conditionalPanel(condition = "output.reportbuilt", downloadButton("reportButton", "Download"))
)





map = leafletOutput("map", width = "100%", height = "90vh")

mapPanel = tabPanel('Map of incomes in the US',
                    fluidRow(
                        column(width = 10, map),
                        column(width = 2, dataSelection)
                    )
)


dataPanel <- tabPanel("Data",
                      h2('Income visualizations', align = 'center',style = "font-family: Courier New;"),
                      fluidRow(
                          column(width = 6,style='height:70vh',
                                 plotly::plotlyOutput('plotly_income')),
                          column(width = 6,style='height:70vh',
                                 plotly::plotlyOutput("plotly_bar")
                          )
                      ),
                      h2('Raw data', align = 'center',style = "font-family: Courier New;"),
                      fluidRow(id="Data_table",
                               column(width = 12,
                                      DT::dataTableOutput(outputId  = 'dataTable')
                               )
                      )
)



reportDownload <- tabPanel('Download report',
                           downloadButton('downloadReport', 'Download selected data'))



ui = navbarPage('US Income', id = 'navBar', mapPanel, dataPanel)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
        if(is.null(input$selState) & is.null(input$selCounty)){
            data[data$Mean >= input$range[1] & data$Mean <= input$range[2],]
        }else if(is.null(input$selState) & is.null(input$selCounty) == FALSE){
            data[data$Mean >= input$range[1] & data$Mean <= input$range[2] & data$County %in% input$selCounty,]
        }else if(is.null(input$selState) == FALSE & is.null(input$selCounty)){
            data[data$Mean >= input$range[1] & data$Mean <= input$range[2] & data$State_Name %in% input$selState,]
        }else if(is.null(input$selState) == FALSE & is.null(input$selCounty) == FALSE){
            data[(data$Mean >= input$range[1] & data$Mean <= input$range[2]) & (data$County %in% input$selCounty | data$State_Name %in% input$selState),]
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
                                                                                        "Mean Income:", Mean, "<br>",
                                                                                        'Std Deviation:', Stdev
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
    
    output$dataTable <- DT::renderDT(filteredData())
    
    output$downloadData = downloadHandler(
        filename = function() {
            paste('income_data', '.csv', sep='')
        },
        content = function(con) {
            write.csv(filteredData(), con)
        }
    )
    
    
    # output$report <- downloadHandler(
    #     # For PDF output, change this to "report.pdf"
    #     filename = function() {
    #         paste('report.pdf')
    #     },
    #     content = function(file) {
    #         # Set up parameters to pass to Rmd document
    #         params <- list(
    #             selState = isolate(input$selState),
    #             selCounty = isolate(input$selCounty),
    #             income_min = isolate(input$range[1]),
    #             income_max = isolate(input$range[2]),
    #             data = filteredData()
    #         )
    # 
    #         # Knit the document, passing in the `params` list, and eval it in a
    #         # child of the global environment (this isolates the code in the document
    #         # from the code in this app).
    #         rmarkdown::render(output_dir = getwd(),'report.Rmd' ,params = params,  envir = new.env(parent = globalenv()) )
    #     }
    # )
    # 
    
    
    #This second options that somehow works
    # output$report <- downloadHandler(
    #     filename = function() {
    #         paste('report.pdf')
    #     },
    #     
    #     content = function(file) {
    #         src <- normalizePath('report.Rmd')
    #         path = getwd()
    #         
    #         # temporarily switch to the temp dir, in case you do not have write
    #         # permission to the current working directory
    #         owd <- setwd(tempdir())
    #         on.exit(setwd(owd))
    #         file.copy(src, 'report.Rmd', overwrite = TRUE)
    #         setwd(path)
    #         
    #         params <- list(
    #             selState = isolate(input$selState),
    #             selCounty = isolate(input$selCounty),
    #             income_min = isolate(input$range[1]),
    #             income_max = isolate(input$range[2]),
    #             data = filteredData()
    #         )
    #         
    #         library(rmarkdown)
    #         rmarkdown::render('report.Rmd' ,params = params,  envir = new.env(parent = globalenv()) )
    #         
    #     }
    # )
    # 
    
    report <- reactiveValues(filepath = NULL)
    
    observeEvent(input$generate, {
        
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "Gathering data and building report.", 
                     detail = "This may take a while. This window will disappear  
                     when the report is ready.", value = 1)
        
        params <- list(
            selState = isolate(input$selState),
            selCounty = isolate(input$selCounty),
            income_min = isolate(input$range[1]),
            income_max = isolate(input$range[2]),
            data = filteredData()
        )
        
        tmp_file <- paste0(tempfile(), ".pdf") #Creating the temp where the .pdf is going to be stored
        
        tmp_file = render('report.Rmd' ,params = params,  envir = new.env(parent = globalenv()))
        
        report$filepath <- tmp_file #Assigning in the temp file where the .pdf is located to the reactive file created above
        
    })
    
    
    # Hide download button until report is generated
    output$reportbuilt <- reactive({
        return(!is.null(report$filepath))
    })
    outputOptions(output, 'reportbuilt', suspendWhenHidden= FALSE)
    
    #Download report  
    output$reportButton <- downloadHandler(
        
        # This function returns a string which tells the client
        # browser what name to use when saving the file.
        filename = function() {
            paste0(input$client, "_", Sys.Date(), ".pdf") %>%
                gsub(" ", "_", .)
        },
        
        # This function should write data to a file given to it by
        # the argument 'file'.
        content = function(file) {
            
            file.copy(report$filepath, file)
            
        }
    )
    
    
    output$plotly_bar <- plotly::renderPlotly(
        
        if(is.null(input$selState) & is.null(input$selCounty)){
            p = ggplot(NULL) + labs(y = 'Income standard deviation',x = 'County')
            ggplotly(p, height = 650)
        }else{
            p = ggplot(filteredData()) + aes(x=County, y=Stdev, fill=State_Name) +
                geom_bar(stat="identity", position=position_dodge()) + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_blank()) +
                labs(y = 'Income standard deviation', fill = 'State name')
            ggplotly(p, height = 650)
        }
        
        
        
    )
    
    output$plotly_income <- plotly::renderPlotly(
        
        if(is.null(input$selState) & is.null(input$selCounty) ){
            p = ggplot(NULL) + labs(y = 'Mean income',x = 'County')
            ggplotly(p, height = 650)
            
        }else{
            p = ggplot(filteredData(), aes(x=County, y=Mean, fill=State_Name)) +
                geom_boxplot(position = position_dodge2()) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.background = element_blank()) +
                labs(y = 'Mean income', fill = 'State name')
            ggplotly(p, height = 650)
        }
        
    )
}

# Run the application 
shinyApp(ui = ui, server = server)







