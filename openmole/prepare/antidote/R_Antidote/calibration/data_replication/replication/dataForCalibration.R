# DirectSampling

################################
###         PACKAGES         ###
################################
library(dplyr)
library(matrixStats)
library(ggplot2)
library(reshape2)
library(plotly)
library(rgl)
library(plot3D)


################################
###        IMPORT DATA       ###
################################

res <- read.csv("dataForCalibrationReplication.csv", header = T)

# dataFrame
df = as.data.frame(res)
# dimensions
dim(df)
# change format
df$seed = as.factor(df$seed)
# utils: 
l = length(unique(df$seed)); l # total number of replications
df %>% select(seed, humansDynamic) %>%  group_by(seed) %>% summarise(count = n()) # number of steps (in outputs)
n = dim(df)[1]/l;n  # idem (steps)
length(unique(df$redCrossSize))
l / length(unique(df$redCrossSize))
# add a time column to the df
v = rep(1:n,l)
df$times = v

######################################
###     COMPUTE MEAN QUANTITIES    ###
######################################

# dynamics : mean over time

df2 = df %>% group_by(times) %>%  summarise(meanZombies = mean(zombies),
                                           meanHumans = mean(humans),
                                           meanRescued = mean(rescued),
                                           meanHumansDynamic = mean(humansDynamic)
                                           )
  
dim(df2)

# data for calibrtion:
meanZombies = df2$meanZombies[1]
meanRescued = df2$meanRescued[1]
meanHumansDynamic = df2$meanHumansDynamic

# save vectors in csv file
#write.csv(matrix(meanHumansDynamic, nrow=1), file ="meanHumansDynamicCalibration.csv", row.names=FALSE, col.names=FALSE)
write(meanHumansDynamic , "meanHumansDynamicCalibration.csv", sep = ",", ncolumns = length(meanHumansDynamic)) 
#write(area2008 ,file = paste(Dir2,"/Martin_area2008.txt",sep = ""),ncolumns = nCol) 





  
  