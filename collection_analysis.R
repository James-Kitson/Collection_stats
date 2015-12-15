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

my.df<-merge(collection.met.unique,my.sr2, by=c("TreeID","Visit"))

### make a sprayed/unsprayed vector
for (i in 1:length(my.df$Tree)){
my.df$Treat[i]<-substring(my.df$Tree[i], first=1, last=nchar(my.df$Tree[i])-1)
}

### make the metadata into factors
for (i in c(1:5,7)){
my.df[,i]<-as.factor(my.df[,i])
}

### look at how species richness is distributed
hist(my.df$SR)

### need to check mean and variance for validity of poisson model (i.e. mean should ~= variance)
mean(my.df$SR)
var(my.df$SR)

summary(my.df)

model1 <- glm (SR ~ Treat + Park + Visit, 
               family = poisson (link = sqrt), data = my.df)

AIC(model1)

summary(model1)

par(mfrow=c(2,2))
plot(model1)
dev.off()

boxplot(my.df$SR ~ my.df$Visit,
        ylab="Species Richness", xlab="Visit")

boxplot(my.df$SR ~ my.df$Treat,
        ylab="Species Richness", xlab="Treatment", notch=T)
