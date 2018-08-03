library("shiny")
library("ggplot2")
library("stringr")

#load(file = "chromData.RData") #chromosome records
#genus <- read.csv(file = "genusNames.csv") #long list of genus names
#genus <- genus$x  #long list of genus names
#ferngenera <- genus #turning it into a vector

load(file = "angiogametophyticlist.RData")
load(file = "angiosporophyticlist.RData")
load(file = "angiocsomelist.RData")
load(file = "ferngametophyticlist.RData")
load(file = "fernsporophyticlist.RData")
load(file = "ferngenera.RData")
load(file = "angiogenera.RData")
#load(file = "ferncsomelist.RData")
load(file = "fernchsomelist.RData")

ferngenera <- sort(ferngenera)
fernList <- vector(mode="list", length=length(sort(ferngenera)))
names(fernList) <- ferngenera
for(i in 1:length(ferngenera)){
  fernList[i] <- i
}

angiogenera <- sort(angiogenera)
angioList <- vector(mode="list", length=length(angiogenera))
names(angioList) <- angiogenera
for(i in 1:length(angiogenera)){
  angioList[i] <- i
}

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
# This other funtions just puts the proper names, used for the table not the chart
putTheColumnNamesForTable <- function(dataFrame){
  newFrame <- dataFrame
  colnames(newFrame) <-  c("Species", "Chromosome Number", "Number Of Records")
  return(newFrame)
}

# This functions takes the dataframe for the genus and the list of checked species to see. Returns a new data frame without any unchecked species
onlyChecked <- function(dataFrame, listChecked){
  listToRemove <- vector()
  searchForList <- listChecked
  species <- unique(dataFrame$species) 
  if (length(searchForList) != 0 && length(searchForList) != length(species)){ # only happens if any boxes are checked and if not all of them are checked 
    searchForList <- stringRemover(species,searchForList) # removes the list that we want from the total, getting the list we dont want
    for(j in 1:length(searchForList)){ # happens each time per species to remove
      searchFor <- searchForList[j] # species to remove
      for(i in 1:length(dataFrame$species)){ # runs through all species in this genus
        thisLine <- dataFrame$species[i] # gets the row
        if (str_detect(thisLine,searchFor)){ # if the species we want to remove is in this row
          listToRemove <- c(listToRemove,i)# the row numbers gets added to a list
        }
      }
    }
    dataFrame <- dataFrame[-listToRemove,] # removes list unwanted from total list
  }
  return(dataFrame)
}

# A function that calculates the right margin for the ggplot
correctMargin <- function(dataFrame){
  bottomMargin <- 1000
  if (length(dataFrame$species) > 10) { bottomMargin <- 1000}
  if (length(dataFrame$species) > 20) { bottomMargin <- 800}
  if (length(dataFrame$species) > 30) { bottomMargin <- 600}
  if (length(dataFrame$species) > 40) { bottomMargin <- 100}
  return(bottomMargin)
}

correctGroupList <- function(y){
  if (length(y) == 0) {
    y = "a"
  }
  switch(y, 
         a = angioList,
         f = fernList
  )}

correctGenusVector <- function(y){
  switch(y, 
         a = angiogenera,
         f = ferngenera
  )}

correctGenusName<- function(group,number){
  if (length(group) == 0) { group <- "ag"}
  if (length(number) == 0) { number <- "1"}
  genusVector <- correctGenusVector(group)
  return(genusVector[number])
  }

correctGroupAndChromosome <- function(y){
  switch(y, 
         ag = angiogametophyticlist,
         as = angiosporophyticlist,
         ac = angiocsomelist,
         fg = ferngametophyticlist,
         fs = fernsporophyticlist,
         fc = ferncsomelist
  )}

correctGenus <- function(groupChrom, genusNumber){
  myGroup <- correctGroupAndChromosome(groupChrom)
  return(as.data.frame(myGroup[[genusNumber]]))
}

correctTitle <- function(y){
  switch(y, 
         ag = "Gametophyitic",
         as = "Sporophyitic",
         ac = "Aggregated",
         fg = "Gametophyitic",
         fs = "Sporophyitic",
         fc = "Aggregated"
  )}

#--------------------------All below is connected to the UI-------------------#
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Getting the name of the current Genus
  GenusName <-reactive({
    correctGenusName(input$group,as.numeric(input$var))
  })
  
  # Getting the data frame of the current Genus
  GenusSubset <-reactive({
    putTheColumnNames(correctGenus(paste(input$group,input$chromosome, sep = ""),as.numeric(input$var)))# ggplot needs a data frame to work
  })
  
  output$groupSelect <- renderUI({
    selectInput ("group", label = h3("Group"), choices = list("Angiosperms" = "a","Ferns" = "f"), selected = "a")
    })
  
  output$cSelect <-  renderUI({
    selectInput ("chromosome", label = h3("Chromosome"), choices = list("Gametophyte (n)" = "g","Sporophyte (2n)" = "s" , "Chromosome (2n)" = "c"), selected = "g")
  })
  
  output$genusSelect <- renderUI({ 
    selectInput ("var", label = h3("Genus"), choices = correctGroupList(input$group), selected = 1)
  })
  
  # Button for selecting/disselecting all
  output$select <- renderUI({
    actionButton("select", label = "Select/Disselect All")  
  })
  
  # Creating the Checkboxes that change for each genus
  output$speciesControls <- renderUI({ 
    mySubset <- GenusSubset() # Getting subset
    species <- unique(mySubset$species)# creating vector of species
    checked <- species
    selected <- 4 %% 2 # this alternates the select/disselect button
    if (selected == 0){ checked <- NULL}
    checkboxGroupInput("speciesChecked", "Choose Species", species, selected = checked)
    # creating checkboxes
  })
  
  output$distPlot <- renderPlot({ # This create the output plot to be sent to the UI
    genusName <- GenusName() # Getting name of Genus
    mySubset <- onlyChecked(GenusSubset(),input$speciesChecked) # Getting subset to plot. Uses function only to get checked species
    # Plotting Time
    ggplot(data = mySubset, aes(x = chromosome, y = freq, fill = species)) + #fill species indicates it will be a stacker bar graph, as well as handling different colors
      geom_bar(stat="identity") + 
      labs(title = paste((correctTitle(paste(input$group,input$chromosome, sep = ""))),"Chromosome Counts For", genusName), # Title gets generated dinamically
           y = "Number Of Records", 
           x = "Chromosome Number", 
           fill = "Species Name")+ # data for the axis and legend
      scale_fill_hue(l=60)+ # color adjustment
      theme(plot.title = element_text(hjust = 0.5, vjust = 0), plot.margin = margin(0,0,correctMargin(mySubset),0), legend.position = "bottom") # centering title. Also calculates the proper bottom margin to have so that the legend shows properly.
  }, height = 1500)
  
  # output table to be downloaded
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(GenusName(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(onlyChecked(GenusSubset(),input$speciesChecked), file, row.names = FALSE)
    }
  )
  
  # Creating table
  output$table <- renderTable({
    putTheColumnNamesForTable(putTheColumnNames(correctGenus(paste(input$group,input$chromosome, sep = ""),as.numeric(input$var))))
  })
})
