# PSE

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
library(scatterplot3d) 
library(hypervolume)

################################
###        IMPORT DATA       ###
################################

res <- read.csv(paste0("resultsPSE_2/resultsPSE2/population5300",".csv"), header = T)

# dataFrame
df = as.data.frame(res)
dim(df)
head(df)
#df$evolution.samples

sum(df$evolution.samples>10)
max(df$evolution.samples)
df %>% group_by(evolution.samples) %>% summarise(count = n())

## input params (in OpenMOLE)
# redCrossSize  in (0, 80),
# redCrossActivationDelay in (0, 10),
# redCrossEfficiencyProbability  in (0.8, 1.0),
# redCrossExhaustionProbability in (0.45, 1.0)),

## objectives (diversity mesures in OpenMOLE)
# zombies in (0 to 254 by 25),
# halfRescued in  (0 to 1000 by 15),
# rescued in (0 to 250 by 25),   
# pursued in (0 to 30000   by 500)),   

# zombies 
zombiesOutputRef = seq(0, 250, by= 25)
# halfRescued
halfRescuedOutputRef = seq(0,1000, by= 15)
# rescued
rescuedOutputRef = seq(0, 250, by= 25)   
# pursued
pursuedOutputRef = seq(0,  30000, by = 500)

# total nb points in outputs
nbPointsMax = length(zombiesOutputRef)*length(halfRescuedOutputRef)* length(rescuedOutputRef)*length(pursuedOutputRef)
nbPointsMax
# ratio nb points discovered by pse / nb points max (output space)
dim(df)[1] /nbPointsMax

# range
range(df$zombies)
range(df$halfRescued)
range(df$rescued)
range(df$pursued)
# some poursued are over the objective
sum(df$pursued > 30000)

df$pursued[which(df$pursued>30000)]


# projections of outputs in the (zombies, rescued) plane
plot(df$zombies, df$rescued, xlim = c(0,254), ylim = c(0,250))

# %>% filter(redCrossSize < 40)
ggplot(df , aes(x=zombies,rescued)) + 
  #geom_point() +
  geom_point(aes(color= redCrossSize)) +
  labs( title = "Projection of PSE results in the (zombies, rescued) plane") #+ 
  #labs(subtitle = paste("current antidote features: delay = 4, efficiency = 0.98, exhaustion = 0.6" ) )
#ggsave("TOMODIFT_resultPSE_proj_zombies_rescued_colorRCSize.png")
min(df$rescued)


plot(df$rescued, df$halfRescued)
plot(df$rescued, df$pursued)

ggplot(df %>% filter(redCrossSize < 41), aes(x=rescued,pursued)) + 
  #geom_point() +
  geom_point(aes(color= redCrossSize)) +
  labs( title = "Projection of PSE results in the (zombies, rescued) plane") #+


plot(df$zombies,df$pursued)
plot(df$zombies,df$pursued)


########################################
###    CONVERGENCE OF ALGORItHM ?    ###
########################################

# # number of points
# generation = seq(100,5300, by = 100)
# sampleSize = c()
# for (i in generation){
#   temp <- read.csv(paste0("resultsPSE_2/resultsPSE2/population",i,".csv"), header = T)
#   sampleSize = c(sampleSize,dim(temp)[1])
# }
# 
# plot(generation,sampleSize)
# 
# 
# # volume 
# hv = hypervolume(data = cbind(df$zombies,df$halfRescued,df$rescued,df$pursued) ,method='box')
# get_volume(hv)
# 
# volume = c()
# for (i in generation){
#   temp <- read.csv(paste0("resultsPSE_2/resultsPSE2/population",i,".csv"), header = T)
#   temp = as.data.frame(temp)
#   hv = hypervolume(data = cbind(temp$zombies,temp$halfRescued,temp$rescued,df$pursued) ,method='box')
#   volume = c(volume,get_volume(hv))
# }
# 
# 
# plot(generation,volume)
# 


