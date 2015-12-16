###### Script for analysing collection data from 2015  #######################

### Clear the workspace
rm(list=ls())

### get the libraries I need ##############################
library(lme4)
library(MuMIn)
library(LMERConvenienceFunctions)

### Read in my data ######################################
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
my.df$Visit.numeric<-my.df$Visit

### make a sprayed/unsprayed vector
for (i in 1:length(my.df$Tree)){
my.df$Treat[i]<-substring(my.df$Tree[i], first=1, last=nchar(my.df$Tree[i])-1)
}

### make the metadata into factors
for (i in c(1:5,8)){
my.df[,i]<-as.factor(my.df[,i])
}

### start looking at the data to decide how to analyse my data ############

### look at how species richness is distributed
hist(my.df$SR)

### need to check mean and variance for validity of poisson model (i.e. mean should ~= variance)
mean(my.df$SR)
var(my.df$SR)

summary(my.df)

### start building my mixed effect models #########################
model1.lmer <- glmer(SR ~ Treat + Park + Visit + (Park|Pair),
                family = poisson (link = "log"), 
                data = my.df)
AIC(model1.lmer)

sresid <- resid(model1.lmer, type = "pearson")  # Extract the standardised residuals
hist(sresid)

plot(model1.lmer)

mcp.fnc(model1.lmer)       # Provides a QQ plot and heteroscedasticity plot
plotLMER.fnc(model1.lmer)  # Plots the population mean relationship between fixed and dependent variables

summary(model1.lmer)   # Various formats of model summaries
model1.lmer
anova(model1.lmer)

### Plot the raw data ###############################

## Plotting a graph of the model
plot(my.df$SR ~ my.df$Visit, pch=21, cex=1.8,  
     xlab="Visit", ylab="Species Richness", cex.lab = 1.3,
     col = my.df$Park)

### Plot the model on to the raw data ####################

# Step 1: Making a table of prediction data (pdat)
pdat <- expand.grid(SR = seq(min(my.df$SR),max(my.df$SR),0.1),
                    Treat=levels(my.df$Treat),
                    Park=levels(my.df$Park),
                    Visit=levels((my.df$Visit)))
pdat

# Step 2: Making a file containing the predicted data (pred)  
pred <- predict(model1.lmer, newdata = pdat, re.form=NA,
                na.action = na.exclude, type= "response")
# For GAMMs, use pedict(model3$gam)
pred
# NB. standard errors not yet available for glmer predictions :(

# Step 3: combine the predictions with the predictors, 
#         into a final dataframe (predframe)
predframe <- data.frame (pdat, preds = pred)
predframe

# Step 4: plot some graphs of predicted values of y vs x

lines (predframe$preds ~ predframe$Visit, col="red", lwd = 2)  


# No SE values, so add fitted lines for each beach...
pdat <- expand.grid(Height = seq(-1.5,2.5,0.1), 
                    fBeach=levels(dframe1$fBeach))
pdat
pred <- predict(model2, newdata = pdat, 
                re.form=NULL, # makes predictions for each level of the random factor
                na.action = na.exclude, type= "response")
pred

predframe <- data.frame (pdat, preds = pred)
predframe

lines (predframe$preds ~ predframe$Height, 
       subset=predframe$fBeach=="9",
       col="blue", lwd = 2) 

##### The end ####


### do some bloxplots ####################################

boxplot(my.df$SR ~ my.df$Visit,
        ylab="Species Richness", xlab="Visit", notch=T)

boxplot(my.df$SR ~ my.df$Treat,
        ylab="Species Richness", xlab="Treatment", notch=T)
