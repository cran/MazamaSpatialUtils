library(MazamaCoreUtils)
library(MazamaSpatialUtils)
library(sp)
library(shiny)

# shiny options
cacheDir <- file.path(dirname(tempdir()), "map_app_cache")
shinyOptions(cache = diskCache(cacheDir))

# Prep
# make sure SpatialDataDir is set. 
getSpatialDataDir()
loadSpatialData('NaturalEarthAdm1')
wa_outline <- subset(NaturalEarthAdm1, countryCode == "US" & stateCode == "WA")
logger.setup()
logger.setLevel("TRACE")

# Source files
helperFiles <- list.files("helpers")
for (file in helperFiles) {
  source(file.path("helpers", file))
}


# UI ---------------------------------------------------------------------------


ui <- fluidPage(
  titlePanel("National Bridge Inventory Aggregated Data for Washington"),
  sidebarLayout(
    sidebarPanel(
      
      h2("Plot Settings"),
      
      # Plot settings
      selectInput('SFDF', 'Spatial Polygons DataFrame',
                  c(HUC4 = 'WBDHU4',
                    HUC6 = 'WBDHU6',
                    HUC8 = 'WBDHU8',
                    HUC10 = 'WBDHU10',
                    HUC12 = 'WBDHU12'),
                  selected = 'WBDHU6'),
      selectInput('variable', 'Variable to Aggregate', 
                  c("yearBuilt", 
                    "averageCarCount",
                    "age"),
                  selected = "yearBuilt"),
      selectInput('FUN', 'Function',
                  c(MIN = 'min',
                    MEAN = 'mean',
                    MAX = 'max'),
                  selected = 'mean'),
      selectInput('style', 'Desired output figure style',
                  c(`Colored SFDF only` = 'base_spdf',
                    `Colored SFDF with points` = 'base_spdf_plus_points',
                    `Points only` = 'points_plus_state')),
      
      # Download data settings
      selectInput('output_file', 'Desired output file contents',
                  c(`Summary values only` = 'summary_df',
                    `Original data plus summary values` = 'original_plus_summary')),
      downloadButton('downloadData', 'Download')
      
    ),
    
    # output plot
    mainPanel(
      plotOutput('bridgeMap', width="80%", height = "80%"),
      tableOutput('bridgeTable')
    )
  )
)


# Server -----------------------------------------------------------------------

server <- function(input, output, session){
  
  # Define reactive variables
  inputData <- get(load("data/wa_bridges.RData"))
  
  reactiveSFDF <- reactive({
    
    logger.trace("reactiveSFDF")
    
    if (!exists(input$SFDF)) {
      logger.trace("loading %s", input$SFDF)
      loadSpatialData(input$SFDF)
    }
    SFDF <- subset(eval(parse(text = input$SFDF)), stateCode == 'WA')
    return(SFDF)
  })
  
  reactiveOutputData <- reactive({
    # Do data manipulation once and cache it
    
    logger.trace("reactiveOutputData()")
    uniqueCode <- digest::digest(c("outputData", input$inputDatafile$name, input$SFDF, input$FUN, input$variable))
    filePath <- paste0(cacheDir, "/", uniqueCode, ".RData")
    
    if (!file.exists(filePath)) {
      # Get data
      data <- inputData
      SFDF <- reactiveSFDF()
      if (!"polygonID" %in% names(SFDF)) {
        SFDF$polygonID <- SFDF$HUC
      }
      
      # Aggregate data based on FUN
      logger.trace("Aggregating data by %s", input$variable)
      df <- summarizeByPolygon(data$longitude, 
                               data$latitude,
                               value = data[[input$variable]],
                               SFDF = SFDF, 
                               FUN = eval(parse(text = input$FUN)))
      logger.trace("Successfully aggregated data")
      df[is.na(df)] <- 0
      
      # Get the correct plot order
      plotOrder <- SFDF$HUC[SFDF$polygonID %in% df$polygonID]
      plotOrder <- data.frame(polygonID = plotOrder, stringsAsFactors = FALSE)
      df <- dplyr::left_join(plotOrder, df, by='polygonID')
      
      logger.trace("saving %s", filePath)
      save(df, file = filePath)
    } else {
      logger.trace("loading %s", filePath)
      df <- get(load(filePath))
    }
    
    return(df)
    
  })
  
  
  # Define output
  # https://shiny.rstudio.com/articles/plot-caching.html
  output$bridgeMap <- renderCachedPlot(
    expr = {
      title <- paste(parse(text=input$SFDF), "with", parse(text=input$FUN), "function")
      bridgePlot(inputData, 
                 reactiveOutputData(), 
                 reactiveSFDF(), 
                 input$FUN, 
                 input$style, 
                 title,
                 wa_outline)
    },
    cacheKeyExpr = {
      list(input$inputDataFile$name, input$SFDF, input$FUN, input$style, input$variable)
    },
    sizePolicy = function(dims){return(c(750,750))}
  )
  
  output$bridgeTable <- renderTable({
    
    bridgeTable(inputData, 
                reactiveOutputData(), 
                reactiveSFDF(), 
                input$FUN, 
                input$output_file,
                input$variable)
    
  })
  
  output$downloadData <- downloadHandler(

    filename = 'summary_data.csv',
    content = function(file) {
      logger.trace("download data")
      write.csv(inputData, file)
    }
  )
  
}


# Start the app -----------------------------------------------------------

shinyApp(ui, server)
