# Loading libraries required
library(shiny)
# Define UI for application
shinyUI(fluidPage(
  # Application title
  titlePanel("EyeChrom: Visualizing Chromosome Count Data From Plants"),
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    # Information for the panel
    sidebarPanel( p("Thank you for using this tool! It's goal is to show chromosmal data per genus. Select the genus, and the plot will show the records found for it in the Chromosome Counts Database. If any data is missing or seems incorrect please contact: https://github.com/roszenil/CCDBcurator"),
    # Creating drop down menu to select Genus
    uiOutput("groupSelect"),
    uiOutput("cSelect"),
    uiOutput("genusSelect"),
    br(), # Line break 
    downloadButton("downloadData", "Download Data"),
    br(),
    br(),
    uiOutput("select"),
    br(),
    uiOutput("disselect"),
    verbatimTextOutput("value2"),
    verbatimTextOutput("value3"),
    uiOutput("speciesControls") # Dynamic checkboxes to select specific species
    ),
    mainPanel( # Main panel
      tabsetPanel(
        tabPanel("Plot", plotOutput("distPlot")), 
        tabPanel("Table", tableOutput("table"))
      )
      
      
    )
  )
))