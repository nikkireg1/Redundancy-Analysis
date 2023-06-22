#Redundancy Analysis (RDA) Tutorial 
#Author: Nicole Regimbal - Just One Bird's Opinion 
#Date: June 22, 2023

#Load in necessary libraries
#If these libraries are not yet installed, first use the function install.packages()
library(ggplot2)
library(vegan)

#Set your working directory
getwd() #This tells R that you want to get a new working directory
setwd("Insert your path here") #This tells R where to find that new directory
dir() #This tells R to retrieve this new directory to use 

#Read in your data - we will call this dataframe 'df'
df <- read.csv("env_data.csv", header = TRUE) #Header = TRUE tells R that the first row is the column names
df <- na.omit(df) #This removes NA values in the dataframe, this is important to avoid errors

#Are you using presence-absence data? Skip this step! 
  #Hellinger transformation on species abundance data 
  #This turns absolute abundance into relative abundance
  #We will call this spec.h
spec.h <- decostand(df[c(15:20)], method = "hellinger")
  #If you are using presence-absence, just take of subset of the species data
    #spec <- subset(df, select = c(15:20))


#You don't need this step, but include it if you want to plot the relationships between explanatory variables
  #Normalize and Standardization of environmental factors
  #plot the relationships
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

pairs(df[,c(12,14,23,24,27,28,31:36)], panel=panel.smooth, diag.panel=panel.hist)


#Are your environmental/explanatory variables on different scales? You need this step! 
t.env<-decostand(df[,c(14,23,24,27,28,31:33)], method="log") #Log transforming the non-standardized variables
env.stand <- cbind(t.env,df[,c(12,34:36)]) #Variables already scaled do not need to be transformed


#Time to do the Redundancy Analyis (RDA)! 
spec.rda <- rda(spec.h ~ ., env.stand)
summary(spec.rda)

#Let's plot this

model <- ordiplot(spec.rda, type = "none", scaling = 2, cex=10, xlab = "RDA1 (26.2%)", ylab = "RDA2 (9.8%)", cex.lab=1.25)
points(spec.rda, col="darkgrey", cex=1)
points(spec.rda, dis="sp", col="blue")
text(spec.rda, dis="sp", col="blue")
text(spec.rda, dis="bp", col="black")


#WAIT - We want to make sure we are using the best model with important terms
  #A step function chooses the best variables to simplify the model
spec.rda1 <- step(spec.rda, scope=formula(spec.rda), test="perm")
summary(spec.rda1)

#Inspect for collinearity to create a simpler model
  #I am omitting anything with a VIF greater than 20 (This is the usual value used, but adjust to your needs)
vif.cca(spec.rda1)
  #Let's get rid of meadow percent 

spec.rda2 <- rda(spec.h ~ Forest.Ratio + Meadow.Ratio + Total.Wetland + Habitat.Area +
                   Forest.Percent + Wetland.Percent, env.stand)
summary(spec.rda2)

#check VIF again - OK!
vif.cca(spec.rda2)

#You can calculate the adjusted r-squared here
RsquareAdj(spec.rda2)

#ANOVA
anova(spec.rda2, perm.max=1000) #tells you if entire model is significant
anova(spec.rda2, by="axis", perm.max=1000) #tells you which axes are significant
anova(spec.rda2, by="terms", perm.max=1000) #tells you which terns are significant
anova(spec.rda2, by="margin", perm.max=1000) #tells you if the order of the terms is significant

summary(spec.rda2)

#Let's plot our final, simplified RDA! 
simplified_model <- ordiplot(spec.rda2, type = "none", scaling = 2, cex=10, xlab = "RDA1 (22.8%)", ylab = "RDA2 (7.2%)", cex.lab=1.25)
points(spec.rda2, col="darkgrey", cex=1)
points(spec.rda2, dis="sp", col="blue")
text(spec.rda2, dis="sp", col="blue")
text(spec.rda2, dis="bp", col="black")

#Good luck and happy coding! 
#Check out my channel 'Just One Bird's Opinion' on YouTube if you have any questions! 

