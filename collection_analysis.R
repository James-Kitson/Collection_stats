###### Script for analysing collection data from 2015  #####

### Clear the workspace
rm(list=ls())

### Read in my data ####
collection<-read.csv(file = "Data/2015_collection_data.csv", header = T, stringsAsFactors = F)
