# Rodrigo: Here are the RData objects we need to process queries
#1. Angiosperm chromosome number tables by genus
load("angiocsomelist.RData")
#1.1 Angiosperm chromosome number full table by genus
load("data/angiocsomefulltable.RData")
#2. Angiosperm gametophytic number tables by genus  (some are empty or have NA)
load("angiogametophyticlist.RData")
#2.2 Angiosperm gametophytic number full table by genus
load("angiogametophyticfulltable.Rdata")
#3. Angiosperm sporophytic number tables by genus  (some are empty or have NA)
load("angiosporophyticlist.RData")
#3.3 Angiosperm sporphytic full table by genus
load("angiosporophyticfulltable.Rdata")
#4. Fern chromosome number tables by genus  
load("ferncsomelist.RData")
#5. Fern gametophytic number tables by genus  (some are empty or have NA)
load("ferngametophyticlist.RData")
#6. Fern gametophytic number tables by genus  (some are empty or have NA)
load("fernsporophyticlist.RData")
# 7. List of angiosperm genera names
load("angiogenera.RData")
#8. List of fern  genera names
load("ferngenera.RData")

########
# Examples
ferngenera[3] #Antigramma
angiogenera[1870] #Rhipsalis

## Accessing a table for plots 
angiocsomelist[[1870]] # this are the chromosome numbers of Rhipsalis 
## or
angiocsomelist$Rhipsalis

ferngametophyticlist[[3]] # These are the gametophytic numbers of Antigramma
## or
ferngametophyticlist$Antigramma
# I don't know what would be easier for calling in the Rshiny, but one of those two should be the solution



######Don't change anything here

# ####################
######################
### Here is the code to reproduce building the tables we don't need to touch it right now
####################### # Loading data
 # angioccdbclean<-read.table(file="~/Dropbox/CCDBcuratorexample/ccdbAngiospermsOct2018.txt",as.is=TRUE,header=TRUE,sep="$", comment.char="&")
 # head(angioccdbclean)
 # sort3.df <- with(angioccdbclean, angioccdbclean[order(angioccdbclean$Genus, angioccdbclean$Species) , ]) # Organized alphabetically
 # sort3.df<- sort3.df[-seq(1,11,1),] #Removed some weird records at the beginnning
 # angioccdbclean<-sort3.df
 # save(angioccdbclean, file="data/angiospermsCCDBOct2018.RData")
#   csome.number<-rep(0,dim(angioccdbclean)[1])
#   sporophytic<-which(angioccdbclean$Type=="sporophytic")
#   gametophytic<-which(angioccdbclean$Type=="gametophytic")
#   csome.number[sporophytic]<-angioccdbclean$CountTranslation[sporophytic]
#   csome.number[gametophytic]<-angioccdbclean$CountTranslation[gametophytic]*2
#   angioccdbclean<-cbind(angioccdbclean,csome.number)
#   angiorecordsclean2018<-angioccdbclean
#   save(angiorecordsclean2018, file="data/angiorecordsclean2018.RData")  
#   
# 
#  fernccdbclean <- read.delim2("~/Dropbox/CCDBcuratorexample/ccdbFernOct2018.txt", stringsAsFactors=FALSE, sep="$") #partially clean chromosome number datasets
# 
#   sort3.df <- with(fernccdbclean, fernccdbclean[order(fernccdbclean$Genus, fernccdbclean$Species) , ]) # Organized alphabetically
#   head(sort3.df) #Removed some weird records at the beginnning
#   fernccdbclean<-sort3.df
#   save(fernccdbclean, file="data/fernsCCDBOct2018.RData")
#   csome.number<-rep(0,dim(fernccdbclean)[1])
#   sporophytic<-which(fernccdbclean$Type=="sporophytic")
#   gametophytic<-which(fernccdbclean$Type=="gametophytic")
#   csome.number[sporophytic]<-fernccdbclean$CountTranslation[sporophytic]
#   csome.number[gametophytic]<-fernccdbclean$CountTranslation[gametophytic]*2
#   fernccdbclean<-cbind(fernccdbclean,csome.number)
#   fernrecordsclean2018<-fernccdbclean
#   save(fernrecordsclean2018, file="data/fernrecordsclean2018.RData")  


########## We don't need to process this part again at all.

######### Now processing all the tables


# #Chromosome number tables for angiosperms
load("data/angiorecordsclean2018.RData")
genus<-unique(angiorecordsclean2018$Genus)
genus<-sort(genus)
long1<- length(genus) #7896
# # I'm making 2 lists one that helped building the tables and the histograms with information
 per.genus.counts<-list()
 per.genus.table<-list()
# 
# 
for(j in 1:long1){
    print(j)
    aux4<-which(angiorecordsclean2018$Genus==genus[j])
    per.genus.counts[[j]]<-data.frame(species=as.character(angiorecordsclean2018$Species[aux4]), csome.number=angiorecordsclean2018$csome.number[aux4],count.translation=angiorecordsclean2018$CountTranslation[aux4],count.original=angiorecordsclean2018$CountOriginal[aux4],type=angiorecordsclean2018$Type[aux4])
    per.genus.table[[j]]<-table(paste(genus[j],per.genus.counts[[j]]$species,sep="_"), per.genus.counts[[j]]$csome.number)
 }
 names(per.genus.counts)<-genus
 names(per.genus.table)<-genus
 angiocsomelist<-per.genus.table
 save(angiocsomelist,file="data/angiocsomelist.RData")
 angiocsomefulltable<- per.genus.counts 
 save(angiocsomefulltable,file="data/angiocsomefulltable.RData")
 

 ############## Gametophytic tables for angiosperms
 gametophytic<-which(angiorecordsclean2018$Type=="gametophytic")
 gametophytic.counts<-angiorecordsclean2018[gametophytic,]
 per.genus.counts<-list()
 per.genus.table<-list()

 for(j in 1:long1){
   print(j)
   aux4<-which(gametophytic.counts$Genus==genus[j])
   if(length(aux4)>0){
   per.genus.counts[[j]]<-data.frame(species=as.character(gametophytic.counts$Species[aux4]), csome.number=gametophytic.counts$csome.number[aux4],count.translation=gametophytic.counts$CountTranslation[aux4],count.original=gametophytic.counts$CountOriginal[aux4],type=gametophytic.counts$Type[aux4])
   per.genus.table[[j]]<-table(paste(genus[j],per.genus.counts[[j]]$species,sep="_"), per.genus.counts[[j]]$csome.number)
   }else{
   per.genus.counts[[j]]=NA
   per.genus.table[[j]]=NA
 }}
 names(per.genus.counts)<-genus
 names(per.genus.table)<-genus
 angiogametophyticlist<-per.genus.table
 save(angiogametophyticlist,file="data/angiogametophyticlist.RData")
 angiogametophyticfulltable<-per.genus.counts
 save(angiogametophyticfulltable, file="data/angiogametophyticfulltable.RData")

# ############ Sporophytic tables for angiosperms
 sporophytic<-which(angiorecordsclean2018$Type=="sporophytic")
 sporophytic.counts<-angiorecordsclean2018[sporophytic,]
 per.genus.counts<-list()
 per.genus.table<-list() #363
 
 for(j in 1:long1){
   print(j)
   aux4<-which(sporophytic.counts$Genus==genus[j])
   if(length(aux4)>0){
   per.genus.counts[[j]]<-data.frame(species=as.character(sporophytic.counts$Species[aux4]), csome.number=sporophytic.counts$csome.number[aux4],count.translation=sporophytic.counts$CountTranslation[aux4],count.original=sporophytic.counts$CountOriginal[aux4],type=sporophytic.counts$Type[aux4])
   per.genus.table[[j]]<-table(paste(genus[j],per.genus.counts[[j]]$species,sep="_"), per.genus.counts[[j]]$csome.number)
   }else{
     per.genus.counts[[j]]=NA
    per.genus.table[[j]]=NA
 }}
 names(per.genus.counts)<-genus
 names(per.genus.table)<-genus
 angiosporophyticlist<-per.genus.table
 save(angiosporophyticlist,file="data/angiosporophyticlist.RData")
 angiosporophyticfulltable<-per.genus.counts
 save(angiosporophyticfulltable,file="data/angiosporophyticfulltable.RData")
 save(genus,file="angiogenera.RData")
 
 
 # #Chromosome number tables for Ferns
 load("data/fernrecordsclean2018.RData")
 genus<-unique(fernrecordsclean2018$Genus)
 genus<-sort(genus)
 long1<- length(genus) #365
 # # I'm making 2 lists one that helped building the tables and the histograms with information
 per.genus.counts<-list()
 per.genus.table<-list()
 # 
 # 
 for(j in 1:long1){
   print(j)
   aux4<-which(fernrecordsclean2018$Genus==genus[j])
   per.genus.counts[[j]]<-data.frame(species=as.character(fernrecordsclean2018$Species[aux4]), csome.number=fernrecordsclean2018$csome.number[aux4],count.translation=fernrecordsclean2018$CountTranslation[aux4],count.original=fernrecordsclean2018$CountOriginal[aux4],type=fernrecordsclean2018$Type[aux4])
   per.genus.table[[j]]<-table(paste(genus[j],per.genus.counts[[j]]$species,sep="_"), per.genus.counts[[j]]$csome.number)
 }
 names(per.genus.counts)<-genus
 names(per.genus.table)<-genus
 ferncsomelist<-per.genus.table
 save(ferncsomelist,file="data/ferncsomelist.RData")
 ferncsomefulltable<- per.genus.counts 
 save(ferncsomefulltable,file="data/ferncsomefulltable.RData")
 
 
 ############## Gametophytic tables for ferns
 gametophytic<-which(fernrecordsclean2018$Type=="gametophytic")
 gametophytic.counts<-fernrecordsclean2018[gametophytic,]
 per.genus.counts<-list()
 per.genus.table<-list()
 
 for(j in 1:long1){
   print(j)
   aux4<-which(gametophytic.counts$Genus==genus[j])
   if(length(aux4)>0){
     per.genus.counts[[j]]<-data.frame(species=as.character(gametophytic.counts$Species[aux4]), csome.number=gametophytic.counts$csome.number[aux4],count.translation=gametophytic.counts$CountTranslation[aux4],count.original=gametophytic.counts$CountOriginal[aux4],type=gametophytic.counts$Type[aux4])
     per.genus.table[[j]]<-table(paste(genus[j],per.genus.counts[[j]]$species,sep="_"), per.genus.counts[[j]]$csome.number)
   }else{
     per.genus.counts[[j]]=NA
     per.genus.table[[j]]=NA
   }}
 names(per.genus.counts)<-genus
 names(per.genus.table)<-genus
 ferngametophyticlist<-per.genus.table
 save(ferngametophyticlist,file="data/ferngametophyticlist.RData")
 ferngametophyticfulltable<-per.genus.counts
 save(ferngametophyticfulltable, file="data/ferngametophyticfulltable.RData")
 
 # ############ Sporophytic tables for ferns
 sporophytic<-which(fernrecordsclean2018$Type=="sporophytic")
 sporophytic.counts<-fernrecordsclean2018[sporophytic,]
 per.genus.counts<-list()
 per.genus.table<-list() #365
 
 for(j in 1:long1){
   print(j)
   aux4<-which(sporophytic.counts$Genus==genus[j])
   if(length(aux4)>0){
     per.genus.counts[[j]]<-data.frame(species=as.character(sporophytic.counts$Species[aux4]), csome.number=sporophytic.counts$csome.number[aux4],count.translation=sporophytic.counts$CountTranslation[aux4],count.original=sporophytic.counts$CountOriginal[aux4],type=sporophytic.counts$Type[aux4])
     per.genus.table[[j]]<-table(paste(genus[j],per.genus.counts[[j]]$species,sep="_"), per.genus.counts[[j]]$csome.number)
   }else{
     per.genus.counts[[j]]=NA
     per.genus.table[[j]]=NA
   }}
 names(per.genus.counts)<-genus
 names(per.genus.table)<-genus
 fernsporophyticlist<-per.genus.table
 save(fernsporophyticlist,file="data/fernsporophyticlist.RData")
 fernsporophyticfulltable<-per.genus.counts
 save(fernsporophyticfulltable,file="data/fernsporophyticfulltable.RData")
 save(genus,file="ferngenera.Rdata")
 

#--------------------------All above is backend stuff-------------------------#
