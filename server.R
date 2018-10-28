# Loading required libraries
library("shiny")
library("ggplot2")
library("stringr")
library("plotly")
library("data.table")
library("plyr")
library("scales")

# Loading Angiosperm Data
load(file = "angiogametophyticlist.RData")
load(file = "angiosporophyticlist.RData")
load(file = "angiocsomelist.RData")
load(file = "angiogametophyticfulltable.Rdata")
load(file = "angiosporophyticfulltable.Rdata")
load(file = "angiocsomefulltable.Rdata")
load(file = "angiogenera.RData")
angiogenera <- genus

# Loading Fern Data
load(file = "ferngametophyticlist.RData")
load(file = "fernsporophyticlist.RData")
load(file = "ferngenera.RData")
load(file = "ferncsomelist.Rdata")
load(file = "ferngametophyticfulltable.RData")
load(file = "fernsporophyticfulltable.RData")
load(file = "ferncsomefulltable.RData")
ferngenera <- genus

# Read color vector
colors <- as.vector(unlist(read.csv( file = "contrastingcolors.csv", header = FALSE)))

# Generating list of fern genera with number
ferngenera <- sort(ferngenera)
fernList <- vector(mode="list", length=length(sort(ferngenera)))
names(fernList) <- ferngenera
for(i in 1:length(ferngenera)){
  fernList[i] <- i
}

# Generating list of angio genera with number
angiogenera <- sort(angiogenera)
angioList <- vector(mode="list", length=length(angiogenera))
names(angioList) <- angiogenera
for(i in 1:length(angiogenera)){
  angioList[i] <- i
}

# NOTE: Because of limitations from shiny, many functions were needed to be written here that otherwise should have been somewhere else

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


# Function that to give tables a proper name, and fix any underlines 
putTheColumnNames <- function(dataFrame){
  newFrame <- dataFrame
  colnames(newFrame) <-  c("species", "chromosome", "freq")
  newFrame <- subset(newFrame,newFrame$freq != 0)
  newFrame$species <- gsub("_", " ", newFrame$species) # fixing names to remove the underscor in names
  newFrame <- arrange(newFrame, newFrame$species) # makes them alphabetical
  return(newFrame)
}

# This other funtions just puts the proper names, used for the table not the chart
putTheColumnNamesForTable <- function(dataFrame){
  newFrame <- dataFrame
  colnames(newFrame) <-  c("Species", "Chromosome Number", "Number Of Records")
  newFrame <- arrange(newFrame, newFrame$Species)
  return(newFrame)
}

tableNames <- function(dataFrame, genusName){
  newFrame <- dataFrame
  colnames(newFrame) <-  c("Species", "Chromosome Number", "Translated Count", "Original Count", "Type")
  newFrame <- arrange(newFrame, newFrame$Species)
  i <- sapply(newFrame, is.factor)
  newFrame[i] <- lapply(newFrame[i], as.character)
  newNames <- vector()
  for (i in 1:nrow(newFrame)){
    newNames[i] <- paste(genusName,newFrame$Species[i], sep = " ")
  }
  newFrame$Species <- as.factor(newNames)
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

# Function that returns the correct list, either angio or fern
correctGroupList <- function(y){
  if (length(y) == 0) {
    y = "a"
  }
  switch(y, 
         a = angioList,
         f = fernList
  )}

# Returns correct list of genus
correctGenusVector <- function(y){
  switch(y, 
         a = angiogenera,
         f = ferngenera
  )}

# Returns name of genus
correctGenusName<- function(group,number){
  if (length(group) == 0) { group <- "ag"}
  if (length(number) == 0) { number <- "1"}
  genusVector <- correctGenusVector(group)
  return(genusVector[number])
  }

# Returns right dataset
correctGroupAndChromosome <- function(y){
  switch(y, 
         ag = angiogametophyticlist,
         as = angiosporophyticlist,
         ac = angiocsomelist,
         fg = ferngametophyticlist,
         fs = fernsporophyticlist,
         fc = ferncsomelist
  )}

correctGroupAndChromosomeForTable <- function(y){
  switch(y, 
         ag = angiogametophyticfulltable,
         as = angiosporophyticfulltable,
         ac = angiocsomefulltable,
         fg = ferngametophyticfulltable,
         fs = fernsporophyticfulltable,
         fc = ferncsomefulltable
  )}

# Returns right frame for a genus
correctGenus <- function(groupChrom, genusNumber){
  myGroup <- correctGroupAndChromosome(groupChrom)
  return(myGroup[[genusNumber]])
}

correctGenusForTable <- function(groupChrom, genusNumber){
  myGroup <- correctGroupAndChromosomeForTable(groupChrom)
  return(myGroup[[genusNumber]])
}

# Gets right title
correctTitle <- function(y){
  switch(y, 
         ag = "Gametophyitic",
         as = "Sporophyitic",
         ac = "Aggregated",
         fg = "Gametophyitic",
         fs = "Sporophyitic",
         fc = "Aggregated"
  )}

# Creates Data Frame for heatmap
heatmapFrame <- function(dataFrame){
  dataFrame <- melt(dataFrame)
  colnames(dataFrame) <-  c("species", "chromosome", "frequency")
  rescale <- vector()
  toRemove <- vector()
  for (i in 1:nrow(dataFrame)){
    thisSpeciesName <- dataFrame$species[i]
    speciesTotal <- sum(dataFrame[dataFrame$species == thisSpeciesName,3])
    thisSpecies <- dataFrame[i,3]
    rescale <- c(rescale, (thisSpecies/speciesTotal))
    if (speciesTotal == 0){
      toRemove <- c(toRemove,as.character(thisSpeciesName))
    }
  }
  dataFrame$rescale <- rescale
  toRemove <- unique(toRemove)
  if (length(toRemove) > 0){
    for (j in 1:length(toRemove)){
      thisS <- toRemove[j]
      dataFrame <- dataFrame[dataFrame$species != thisS, ]
    }
  }
  i <- sapply(dataFrame, is.factor)
  dataFrame[i] <- lapply(dataFrame[i], as.character)
  #dataFrame <- data.frame(lapply(dataFrame, as.character), stringsAsFactors=FALSE)
  newNames <- vector()
  for (v in 1:nrow(dataFrame)){
    thisSpeciesName <- dataFrame$species[v]
    speciesTotal <- sum(dataFrame[dataFrame$species == thisSpeciesName,3])
    newSpeciesCountText <- paste(" (no. records= ", as.character(speciesTotal), sep = "")
    newSpeciesCountText <- paste(newSpeciesCountText, ")", sep = "")
    newNames <- c(newNames,paste(dataFrame$species[v], newSpeciesCountText, sep = ""))
  }
  dataFrame$species <- as.factor(newNames)
  dataFrame$species <- gsub("_", " ", dataFrame$species)
  dataFrame <- arrange(dataFrame, dataFrame$species)
  dataFrame[is.na(dataFrame)] <- 0
  dataFrame$chromosome <- as.factor(dataFrame$chromosome)
  return(dataFrame)
}

colorVector <- function(dataFrame){
  numberOfColors <- length(unique(dataFrame$Species))
  colorList <- vector()
  if (numberOfColors > length(colors)){
    for (i in 1:floor(numberOfColors/length(colors))){
      colorList <- c(colors,colorList)
    }
    colorList <- c(colorList,(colors[1:(numberOfColors%%length(colors))]))
  } else {
    colorList <- colors[1:numberOfColors]
  }
  return(colorList)
}

#--------------------------All below is connected to the UI-------------------#
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Getting the name of the current Genus
  GenusName <-reactive({
    correctGenusName(input$group,as.numeric(input$var))
  })
  
  # Getting the data frame of the current Genus
  GenusSubset <-reactive({
    putTheColumnNames(as.data.frame(correctGenus(paste(input$group,input$chromosome, sep = ""),as.numeric(input$var))))# ggplot needs a data frame to work
  })
  
  # Getting data frame for heatmap
  genusForHeatmap <- reactive({
    heatmapFrame(correctGenus(paste(input$group,input$chromosome, sep = ""),as.numeric(input$var)))
  })
  
  # UI to pick between angio and fern
  output$groupSelect <- renderUI({
    selectInput ("group", label = h3("Group"), choices = list("Angiosperms" = "a","Ferns" = "f"), selected = "a")
    })
  
  # UI to pick different chromosome types
  output$cSelect <-  renderUI({
    selectInput ("chromosome", label = h3("Chromosome"), choices = list("Gametophyte (n)" = "g","Sporophyte (2n)" = "s" , "Combined (2n)" = "c"), selected = "g")
  })
  
  # UI To select Genus
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
    selected <- input$select %% 2 # this alternates the select/disselect button
    if (selected == 0){ checked <- NULL}
    checkboxGroupInput("speciesChecked", "Choose Species", species, selected = checked)
    # creating checkboxes
  })
  
  output$interactivePlot <- renderPlotly({ # This create the output plot to be sent to the UI
    genusName <- GenusName() # Getting name of Genus
    mySubset <- onlyChecked(GenusSubset(),input$speciesChecked) # Getting subset to plot. Uses function only to get checked species
    # Plotting Time
    colnames(mySubset) <- c("Species", "Chromosome", "Frequency")
    ggplotly(ggplot(data = mySubset, aes(x = Chromosome, y = Frequency, fill = Species)) + #fill species indicates it will be a stacker bar graph, as well as handling different colors
      geom_bar(stat="identity") + 
      labs(title = paste((correctTitle(paste(input$group,input$chromosome, sep = ""))),"Chromosome Counts For", genusName), # Title gets generated dinamically
           y = "Number Of Records", 
           x = "Chromosome Number (note: not sequential)", 
           fill = "Species Name")+ # data for the axis and legend
      scale_fill_hue(l=60)+ # color adjustment
      scale_fill_manual(values=colorVector(mySubset))+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5, vjust = 0), plot.margin = margin(0,0,0,0), legend.position = "bottom"), height = 600)%>%
      layout(showlegend = FALSE)%>% 
      config(displayModeBar = F)
      # centering title. Also calculates the proper bottom margin to have so that the legend shows properly.
  })
  
  output$regularPlot <- renderPlot({ # This create the output plot to be sent to the UI
    genusName <- GenusName() # Getting name of Genus
    mySubset <- onlyChecked(GenusSubset(),input$speciesChecked) # Getting subset to plot. Uses function only to get checked species
    colnames(mySubset) <- c("Species", "Chromosome", "Frequency")
    ggplot(data = mySubset, aes(x = Chromosome, y = Frequency, fill = Species)) + #fill species indicates it will be a stacker bar graph, as well as handling different colors
      geom_bar(stat="identity") + 
      labs(title = paste((correctTitle(paste(input$group,input$chromosome, sep = ""))),"Chromosome Counts For", genusName), # Title gets generated dinamically
           fill = "Species Name",
           y = "Number Of Records", 
           x = "Chromosome Number (note: not sequential)" 
           )+ # data for the axis and legend
      scale_fill_hue(l=60)+ # color adjustment
      scale_fill_manual(values=colorVector(mySubset))+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5, vjust = 0), plot.margin = margin(0,0,0,0), legend.position = "bottom") # centering title. Also calculates the proper bottom margin to have so that the legend shows properly.
  },height = 600)
  
  output$heatmapPlot <- renderPlot({ # This create the output plot to be sent to the UI
    genusName <- GenusName() # Getting name of Genus
    genus <- genusForHeatmap() 
    #mySubset <- onlyChecked(GenusSubset(),input$speciesChecked) # Getting subset to plot. Uses function only to get checked species
    # Plotting Time
    ggplot(genus, aes(chromosome, species)) + geom_tile(aes(fill = rescale), colour = "grey50") + 
      scale_fill_gradient(low = "white", high = "steelblue") + 
      labs(title = paste((correctTitle(paste(input$group,input$chromosome, sep = ""))),"Chromosome Information For", genusName), # Title gets generated dinamically
           y = "Species", 
           x = "Chromosome Number (note: not sequential)",
           fill = "Percentage of Records")+
      theme_bw()
  })
  
  # output table to be downloaded
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(GenusName(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(tableNames(as.data.frame(correctGenusForTable(paste(input$group,input$chromosome, sep = ""),as.numeric(input$var)))), file, row.names = FALSE)
    }
  )
  
  # Creating table
  output$table <- renderTable({
    genusName <- GenusName()
    tableNames(as.data.frame(correctGenusForTable(paste(input$group,input$chromosome, sep = ""),as.numeric(input$var))), genusName)
  })
})
