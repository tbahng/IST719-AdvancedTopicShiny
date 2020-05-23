# This is a Shiny app that allows users to compare sales reps with multivariate visualizations 

library(shiny)
library(d3heatmap)
library(magrittr)
library(DT)
source('utils.R')
##########################################################################################################################

ui <- fluidPage(
  titlePanel("Sales Rep Analysis"),
  sidebarLayout(
    sidebarPanel(
      # selection list: choose viz type
      selectInput(
        inputId = "viz",
        label = "Choose Visualization",
        choices = c(
          "HeatMap", "ChernoffFaces", "StarPlot","ParallelCoordinates",
          "MultiDimensionalScaling","Dendrogram","Table"
        )
      ),
      # reactive short description of viz type
      textOutput("short_desc"),
      br(),
      # button to generate plot
      actionButton(inputId = "go", label = "Visualize Parts", icon = icon("bar-chart-o"))
      # width of sidebarPanel
      , width = 3
    ),
    mainPanel(
      textOutput("plttitle"),
      tags$head(tags$style("#plttitle{color: gray;
                                 font-size: 20px;
                     font-style: italic;
                     }"
                           )
      ),
      br(),
      uiOutput("all"),
      width = 6
    )
  )
)


server <- function(input, output) {
  
  # Reactive variable: short descriptions and plotting function per viz type selection
  viz <- reactive({
    switch(
      input$viz, 
      "HeatMap" = list("desc" = shortdesc.hm, "func" = getHeatMap),
      "ChernoffFaces" = list("desc" = shortdesc.ch, "func" = getChernoff),
      "StarPlot" = list("desc" = shortdesc.st, "func" = getStar),
      "ParallelCoordinates" = list("desc" = shortdesc.pc, "func" = getParallelCoord),
      "MultiDimensionalScaling" = list("desc" = shortdesc.md, "func" = getMDS),
      "Dendrogram" = list("desc" = shortdesc.dd, "func" = getDendro),
      "Table" = list("desc" = shortdesc.df, "func" = GetDataTable)
    )
  })
  
  # output: short description about viz type selected  
  output$short_desc <- renderText({
    viz()$desc
  })
  
  observeEvent(input$go, {
    output$plttitle <- renderText({
      viz()$desc
    })
  })
  
  # Reactive variable: get data per action go
  dat <- eventReactive(input$go, {
    getData()
  })
  
  # Render: interactive heatmap
  output$heatmap <- renderD3heatmap({viz()$func(dat())})
  # Render: plot
  output$plt <- renderPlot({viz()$func(dat())})
  # Render: data table
  output$dt <- DT::renderDataTable({viz()$func(dat())})
  
  # generate HTML of the output
  output$all <- renderUI({
    if (input$viz == "HeatMap")
      d3heatmapOutput('heatmap')
    else if (input$viz == "Table")
      DT::dataTableOutput('dt')
    else
      plotOutput('plt')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

