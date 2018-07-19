library("shiny")
library("ggplot2")
library("stringr")

# Loading data
fern.dataset<-read.csv("fernccdbclean.csv") #partially clean chromosome number datasets
haploid.number<-rep(0,11993) # This is the number of entries in this dirty dataset, it is larger in the full

# This for loop calculates the haploid number based on whether there is an actual record of gametophytic number, but if absent haploid2 has sporophytic divided by 2 
for (i in 1:11993){
  if(!is.na(fern.dataset$haploid1[i])==TRUE){
    haploid.number[i]<-fern.dataset$haploid1[i]}else{
      if(!is.na(fern.dataset$haploid2[i])==TRUE)
        haploid.number[i]<-fern.dataset$haploid2[i]else{
          haploid.number[i]<-NA
        }}}

#Building a dataset with the haploid number
fern.dataset<-cbind(fern.dataset,haploid.number)
taxa<-unique(fern.dataset[,1]) #3353 is the total of different taxa
smallest<-rep(0,3353)

#This for loop looks into which is the smallest haploid number observed for each taxon. This is useful just to match with the phylogeny
for(i in 1:3353){
  aux<-which(fern.dataset[,1]==taxa[i])
  aux2<-min(fern.dataset$haploid.number[aux])
  smallest[i]<-aux2
}

#Building a small data set to match with the phylogeny
smallest.dataset<-data.frame(taxa,smallest)


#Remove NAs
aux3<-which(!is.na(smallest.dataset$smallest))
smallest.dataset<-smallest.dataset[aux3,]#3041

#Create individual genus
genus<-unique(fern.dataset$genus)#362


# I'm making 2 lists one that helped building the tables and the histograms with information
per.genus.counts<-list()
per.genus.table<-list() #293
for(j in 1:362){
  aux4<-which(fern.dataset$genus==genus[j])
  per.genus.counts[[j]]<-data.frame(species=as.character(fern.dataset$resolvedchromer[aux4]), haploid.number=fern.dataset$haploid.number[aux4])
  per.genus.table[[j]]<-table(per.genus.counts[[j]]$species, per.genus.counts[[j]]$haploid.number)
}
names(per.genus.counts)<-genus

per.genus.table

# There is some limitation from what can be done by a shiny application, so somefunctions needed to be written. These are them:
# Function that takes in one original vector of strings and a vector of strings to remove from the original one
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
# This function puts the correct columns names to a data frame, removes records with a 0, and fixes names with by removing "_"
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
  colnames(newFrame) <-  c("Species", "Chromosome Number", "Number Of Chromosomes")
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
  if (length(dataFrame$species) > 20) { bottomMargin <- 800}
  if (length(dataFrame$species) > 30) { bottomMargin <- 600}
  if (length(dataFrame$species) > 40) { bottomMargin <- 400}
  return(bottomMargin)
}

#--------------------------All above is backend stuff-------------------------#
#--------------------------All below is connected to the UI-------------------#
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Getting the name of the current Genus
  GenusName <-reactive({ genus[as.numeric(input$var)] })
  
  # Button for selecting/disselecting all
  output$select <- renderUI({
    actionButton("select", label = "Select/Disselect All")  
  })
  
  # Getting the data frame of the current Genus. Uses the column names function.
  GenusSubset <-reactive({
    putTheColumnNames(as.data.frame(per.genus.table[[as.numeric(input$var)]]))
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
  
  output$distPlot <- renderPlot({ # This create the output plot to be sent to the UI
    genusName <- GenusName() # Getting name of Genus
    mySubset <- onlyChecked(GenusSubset(),input$speciesChecked) # Getting subset to plot. Uses function only to get checked species
    # Plotting Time
    ggplot(data = mySubset, aes(x = chromosome, y = freq, fill = species)) + #fill species indicates it will be a stacker bar graph, as well as handling different colors
      geom_bar(stat="identity") + 
      labs(title = paste("Chromosome Data For", genusName), # Title gets generated dinamically
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
    putTheColumnNamesForTable(as.data.frame(GenusSubset()))
  })
})
