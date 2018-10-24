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
angioccdb<-read.table(file="~/Dropbox/CCDBcuratorexample/ccdbAngiospermsOct2018.txt",as.is=TRUE,header=TRUE,sep="$", comment.char="&")
head(angioccdb)

sort3.df <- with(angioccdb, angioccdb[order(angioccdb$Genus, angioccdb$Species) , ]) # Organized alphabetically
sort3.df<- sort3.df[-seq(1,11,1),] #Removed some weird records at the beginnning
angioccdb<-sort3.df
save(angioccdb, file="data/angiospermsCCDBOct2018.RData")
  csome.number<-rep(0,dim(angioccdb)[1])
  sporophytic<-which(angioccdb$Type=="sporophytic")
  gametophytic<-which(angioccdb$Type=="gametophytic")
  csome.number[sporophytic]<-angioccdb$CountTranslation[sporophytic]
  csome.number[gametophytic]<-angioccdb$CountTranslation[gametophytic]*2
  angioccdb<-cbind(angioccdb,csome.number)
  angiorecordsclean2018<-angioccdb
  save(angiorecordsclean2018, file="data/angiorecordsclean2018.RData")  
  

#fernrecordsclean2016 <- read.delim2("~/Dropbox/CCDBcuratorexample/cleanpteridophyterecords.txt", stringsAsFactors=FALSE) #partially clean chromosome number datasets
# save(fernrecordsclean2016, file="fernrecordsclean2016.RData")
#load("angiorecordsclean2018.RData")
#load("fernrecordsclean2016.RData")



########## We don't need to process this part again at all.
# # # This for loop calculates the chormosome number number based on whether there is an actual record of gametophytic number, but if absent haploid2 has sporophytic divided by 2
# ##Fern Database is still in old format. Updates will come soon
#numberfernrecords<-dim(fernrecordsclean2016)[1] #25246 that were translated
#numberangiorecords<-dim(angiorecordsclean2018)[1] #410435 that were translated

# csome.number<-rep(0,numberfernrecords)
# for (i in 1:numberfernrecords){
#    if(is.na(fernrecordsclean2016$CountTranslation[i])==TRUE){
#      csome.number[i]<-NA}else{
#        if(fernrecordsclean2016$Type[i]=="sporophytic"){
#          csome.number[i]<-fernrecordsclean2016$CountTranslation[i]}else{
#            csome.number[i]<-2*(fernrecordsclean2016$CountTranslation[i])
#          }}}
# 
# fernrecordsclean2016<-cbind(fernrecordsclean2016,csome.number)
# save(fernrecordsclean2016, file="fernrecordsclean2016.RData")
#
#csome.number<-rep(0,numberangiorecords)
# for (i in 1:numberangiorecords){
#   if(is.na(angiorecordsclean2018$CountTranslation[i])==TRUE){
#     csome.number[i]<-NA}else{
#       if(angiorecordsclean2018$Type[i]=="sporophytic"){
#         csome.number[i]<-angiorecordsclean2018$CountTranslation[i]}else{
#           csome.number[i]<-2*(angiorecordsclean2018$CountTranslation[i])
#         }}}

#angiorecordsclean2018<-cbind(angiorecordsclean2018,csome.number)
#angiorecordsclean2018<-angiorecordsclean2018[-c(257892, 257893,257894,257896,321353),] #This had NAs as genus need to figure out why
#save(angiorecordsclean2018, file="angiorecordsclean2018.RData")

#########Finish not processing

##########Here is what we need to correct from now on if we do changes to data

# #Create individual genus
# genus<-unique(fernrecordsclean2016$Genus)
# genus<-sort(genus)
# long1<-length(genus)
# # I'm making 2 lists one that helped building the tables and the histograms with information
# per.genus.counts<-list()
# per.genus.table<-list() #363

# #Chromosome Numbers tables for ferns
# for(j in 1:long1){
#   print(j)
#   aux4<-which(fernrecordsclean2016$Genus==genus[j])
#   per.genus.counts[[j]]<-data.frame(species=as.character(fernrecordsclean2016$Species[aux4]), csome.number=fernrecordsclean2016$csome.number[aux4])
#   per.genus.table[[j]]<-table(per.genus.counts[[j]]$species, per.genus.counts[[j]]$csome.number)
# }
# names(per.genus.counts)<-genus
# names(per.genus.table)<-genus
# ferncsomelist<-per.genus.table
# save(ferncsomelist,file="ferncsomelist.Rdata")

# ############## Gametophytic tables for ferns
# gametophytic<-which(fernrecordsclean2016$Type=="gametophytic")
# gametophytic.counts<-fernrecordsclean2016[gametophytic,]
# per.genus.counts<-list()
# per.genus.table<-list() #363
# #Chromosome Numbers tables for ferns
# for(j in 1:long1){
#   print(j)
#   aux4<-which(gametophytic.counts$Genus==genus[j])
#   per.genus.counts[[j]]<-data.frame(species=as.character(gametophytic.counts$Species[aux4]), csome.number=gametophytic.counts$csome.number[aux4])
#   per.genus.table[[j]]<-table(per.genus.counts[[j]]$species, per.genus.counts[[j]]$csome.number)
# }
# names(per.genus.counts)<-genus
# names(per.genus.table)<-genus
# ferngametophyticlist<-per.genus.table
# save(ferngametophyticlist,file="ferngametophyticlist.Rdata")

# ############ Sporophytic tables for ferns
# sporophytic<-which(fernrecordsclean2016$Type=="sporophytic")
# sporophytic.counts<-fernrecordsclean2016[sporophytic,]
# per.genus.counts<-list()
# per.genus.table<-list() #363
# #Chromosome Numbers tables for ferns
# for(j in 1:long1){
#   print(j)
#   aux4<-which(sporophytic.counts$Genus==genus[j])
#   per.genus.counts[[j]]<-data.frame(species=as.character(sporophytic.counts$Species[aux4]), csome.number=sporophytic.counts$csome.number[aux4])
#   per.genus.table[[j]]<-table(per.genus.counts[[j]]$species, per.genus.counts[[j]]$csome.number)
# }
# names(per.genus.counts)<-genus
# names(per.genus.table)<-genus
# fernsporophyticlist<-per.genus.table
# save(fernsporophyticlist,file="fernsporophyticlist.Rdata")


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
 save(angiocsomelist,file="data/angiocsomelist.Rdata")
 angiocsomefulltable<- per.genus.counts 
 save(angiocsomefulltable,file="data/angiocsomefulltable.Rdata")
 

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
 save(angiogametophyticlist,file="data/angiogametophyticlist.Rdata")
 angiogametophyticfulltable<-per.genus.table
 save(angiogametophyticfulltable, file="data/angiogametophyticfulltable.Rdata")

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
 save(angiosporophyticlist,file="data/angiosporophyticlist.Rdata")
 angiosporophyticfulltable<-per.genus.counts
 save(angiosporophyticfulltable,file="data/angiosporophyticfulltable.Rdata")
# #########Data tables ferns
# genus<-unique(fernrecordsclean2016$Genus)
# genus<-sort(genus)
# long1<-length(genus)
# per.genus.data<-list()
# for(j in 1:long1){
#   print(j)
#   aux4<-which(fernrecordsclean2016$Genus==genus[j])
#   per.genus.data[[j]]<-fernrecordsclean2016[aux4,-1]
# }
# names(per.genus.data)<-genus
# ferndatasets<-per.genus.data
# ferngenera<-genus
# save(ferndatasets, file="ferndatasets.Rdata")
# save(ferngenera,file="ferngenera.Rdata")

# #########Data tables angiosperms
# genus<-unique(angiorecordsclean2018$Genus)
# genus<-sort(genus)
# long1<-length(genus)
# per.genus.data<-list()
# for(j in 1:long1){
#   print(j)
#   aux4<-which(angiorecordsclean2018$Genus==genus[j])
#   per.genus.data[[j]]<-angiorecordsclean2018[aux4,]
# }
# names(per.genus.data)<-genus
# angiodatasets<-per.genus.data
# angiogenera<-genus
# save(angiodatasets, file="angiodatasets.Rdata")
# save(angiogenera,file="angiogenera.Rdata")
#



#--------------------------All above is backend stuff-------------------------#