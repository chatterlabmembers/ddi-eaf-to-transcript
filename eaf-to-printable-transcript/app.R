#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("ddi-convert-eaf.R")

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("DDI transcript conversion (eaf->txt)"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Annotation file ----
      fileInput("file1", "Choose your ELAN annotation file (<something>.eaf)",
                accept = c("application/xml",
                           ".eaf")),
      # Submit button:
      actionButton("submit", "Update")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      uiOutput("convertedTranscript")
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  report <- eventReactive(input$submit, {
    req(input$file1)
    convert.eaf.ddi(input$file1$datapath, input$file1$name)
  })
  
  output$convertedTranscript <- renderUI({
    # Output file name
    time.now <- gsub('-|:', '', as.character(Sys.time()))
    time.now <- gsub(' ', '_', time.now)
    
    ptcp <- report()$ptcp
    tscr <- report()$trsc
    
    output$convertedTranscriptHandler <- downloadHandler(
      function() {
        paste0(ptcp, ".txt")
        },
      content = function(file) {
        write_lines(tscr, file)
        },
      contentType = "text/txt"
    )
    
    downloadButton("convertedTranscriptHandler",
                   "Download the plain-text transcript")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
