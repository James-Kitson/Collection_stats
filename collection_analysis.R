###### Script for analysing collection data from 2015  #####

### Clear the workspace
rm(list=ls())

### Read in my data ####
collection<-read.csv(file = "Data/2015_collection_data.csv", header = T, stringsAsFactors = F)
collection$TreeID<-as.character(paste(collection$Pair,collection$Tree, sep="_"))

collection.subs<-subset(collection, c(Visit!="1", Park!="control"))

my.sr2<-aggregate(collection.subs$ID~collection.subs$TreeID+collection.subs$Visit, FUN = function(x) length(unique(x)))

colnames(my.sr2)<-c("TreeID","Visit","SR")

unique(my.sr2$TreeID)
