###### Script for analysing collection data from 2015  #####

### Clear the workspace
rm(list=ls())

### Read in my data ####
collection<-read.csv(file = "Data/2015_collection_data.csv", header = T, stringsAsFactors = F)
### Make a unique tree ID
collection$TreeID<-as.character(paste(collection$Pair,collection$Tree, sep="_"))

### throw away visit one as we aren't using it
collection.subs<-subset(collection, Visit!="1", select=c("Visit","Park","Pair","Tree","TreeID","ID"))
### throw away all the control wells as they don't contain organisms
collection.subs<-subset(collection.subs, Park!="control")

### make a data frame containing only metadata
collection.met<-subset(collection.subs, select=c("Visit","Park","Pair","Tree","TreeID"))
### reduce this to unique combinations
collection.met.unique<-unique(collection.met)

### aggregate the original datafram to count unique values in ID (i.e. specie richness) for each combination of tree and visit
my.sr2<-aggregate(collection.subs$ID~collection.subs$TreeID+collection.subs$Visit, FUN = function(x) length(unique(x)))
### rename the columns to something sensible
colnames(my.sr2)<-c("TreeID","Visit","SR")
