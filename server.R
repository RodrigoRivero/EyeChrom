library("shiny")
library("ggplot2")
library("stringr")

# Function that takes in one original array of strings and an array of string to remove from the original one
stringRemover <- function(originalArray, arrayOfStringsToRemove){
  toRemove <- vector()
  for(j in 1:length(arrayOfStringsToRemove)){
    searchFor <- arrayOfStringsToRemove[j]
    for(i in 1:length(originalArray)){
      thisString <- originalArray[i]
      if (str_detect(thisString,searchFor)){
        toRemove <- c(toRemove,i)
      }
    }
  }
  newArray <- originalArray[-toRemove]
  return(newArray)
}

putTheColumnNames <- function(dataFrame){
  newFrame <- dataFrame
  colnames(newFrame) <-  c("species", "chromosome", "freq")
  newFrame <- subset(newFrame,newFrame$freq != 0)
  newFrame$species <- gsub("_", " ", newFrame$species) # fixing names to remove the underscor in names
  return(newFrame)
}
#--------------------------All below is connected to the UI-------------------#
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Getting the name of the current Genus
  GenusName <-reactive({
    genus[as.numeric(input$var)]
  })
  
  # Getting the data frame of the current Genus
  GenusSubset <-reactive({
    putTheColumnNames(as.data.frame(per.genus.table[[as.numeric(input$var)]]))# ggplot needs a data frame to work
  })
  
  
  
  # Creating the Checkboxes that change for each genus
  output$speciesControls <- renderUI({ 
    mySubset <- GenusSubset() # Getting subset
    species <- unique(mySubset$species) # creating vector of species
    checkboxGroupInput("cities", "Choose Species", species, selected = species) # creating checkboxes
  })
  
  output$value <- renderPrint({ input$cities })
  
  output$distPlot <- renderPlot({ # This create the output plot to be sent to the UI
    genusName <- GenusName() # Getting name of Genus
    mySubset <- GenusSubset() # Getting subset
    searchForList <- input$cities # list of species we want to see
    species <- unique(mySubset$species) # list of all species
    listToRemove <- vector() # vector that eventually will have the list of rows to remove
    # Start of if and for statements that take the information from the checkboxes
    if (length(searchForList) != 0 && length(searchForList) != length(species)){ # only happens if any boxes are checked and if not all of them are checked 
      searchForList <- stringRemover(species,searchForList) # removes the list that we want from the total, getting the list we dont want
    for(j in 1:length(searchForList)){ # happens each time per species to remove
      searchFor <- searchForList[j] # species to remove
      for(i in 1:length(mySubset$species)){ # runs through all species in this genus
        thisLine <- mySubset[i,] # gets the row
        if (str_detect(thisLine$species,searchFor)){ # if the species we want to remove is in this row
          listToRemove <- c(listToRemove,i) # the row numbers gets added to a list
        }
      }
    }
      mySubset <- mySubset[-listToRemove,] # removes list unwanted from total list
    }
    myTitle <- paste("Chromosome Data For", genusName) # title name generated dinamically with the choice of the user
     # proper names
    speciesNames <- as.list(mySubset$species)
## Plotting 
    ggplot(data = mySubset, aes(x = chromosome, y = freq, fill = species)) + #fill species indicates it will be a stacker bar graph, as well as handling different colors
      geom_bar(stat="identity") + 
      labs(title = myTitle, 
           y = "Number Of Records", 
           x = "Chromosome Number", 
           fill = "Species Name")+ # data for the axis and legend
      scale_fill_hue(l=60)+ # color adjustment
      theme(plot.title = element_text(hjust = 0.5, vjust = 0), legend.position = "bottom") # centering title
  }, height = 700)
  
  output$table <- renderTable({
    as.data.frame(GenusSubset())
  })
  
})
