library("shiny")
library("ggplot2")

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

#--------------------------All above is backend stuff-------------------------#
#--------------------------All below is connected to the UI-------------------#
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$distPlot <- renderPlot({ # This create the output plot to be sent to the UI
    genusPlot <- as.numeric(input$var) #necessary change to make sure the variable passed is a number and not a string. Here the input$var is coming from the UI
    myTitle <- paste("Chromosome Data For", genus[genusPlot]) # title name generated dinamically with the choice of the user
    subset <- as.data.frame(per.genus.table[[genusPlot]]) # ggplot needs a data frame to work
    colnames(subset) <- c("species", "chromosome", "freq") # proper names
    subset$species <- gsub("_", " ", subset$species) # names have an underscore so I got rid of it to make it look nicer
    
    ## Plotting 
    ggplot(data = subset, aes(x = chromosome, y = freq, fill = species)) + #fill species indicates it will be a stacker bar graph, as well as handling different colors
      geom_bar(stat="identity") + 
      labs(title = myTitle, 
           y = "Number Of Records", 
           x = "Chromosome Number", 
           fill = "Species Name")+ # data for the axis and legend
      scale_fill_hue(l=60)+ # color adjustment
      theme(plot.title = element_text(hjust = 0.5, vjust = 0), legend.position = "bottom") # centering title
  }, height = 1000)
})
