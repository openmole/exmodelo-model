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

res <- read.csv("resultsDirectSampling.csv", header = T)
df = as.data.frame(res)
dim(res)

# data type
df$seed = as.factor(df$seed)
l = length(unique(df$seed)); l # nombre de réplications

# nb de replications
df %>% group_by(redCrossEfficiencyProbability, redCrossActivationDelay, redCrossSize)  %>%  summarise(count = n())


sort(unique(df$redCrossActivationDelay)) ; length(sort(unique(df$redCrossActivationDelay)))
sort(unique(df$redCrossEfficiencyProbability)) ; length(sort(unique(df$redCrossEfficiencyProbability)))
sort(unique(df$redCrossSize)) ; length(sort(unique(df$redCrossSize)))
sort(unique(df$redCrossSize)) ; length(sort(unique(df$redCrossSize)))


#########################################################################################
###     3D PLOT / HITMAP : CONSTANT REDCROSS SIZE, OUTPUT VS DELAY AND EFFICIENCY     ###
#########################################################################################

df_aggregated = df%>% group_by(redCrossEfficiencyProbability, redCrossActivationDelay, redCrossSize) %>%  summarise(meanHumans = mean(humans),
                                                                                                                  meanRescued = mean(rescued),
                                                                                                                  meanhalfRescued = mean(halfRescued),
                                                                                                                  meanZombies = mean(zombies),
                                                                                                                  meanZombified = mean(zombified),
                                                                                                                  meanKilled = mean(killed),
                                                                                                                  sdHumans = sd(humans),
                                                                                                                  sdRescued = sd(rescued),
                                                                                                                  sdhalfRescued = sd(halfRescued),
                                                                                                                  sdZombies = sd(zombies),
                                                                                                                  sdZombified = sd(zombified),
                                                                                                                  sdKilled = sd(killed))




# select a redCross size
redCrossSizeToPlot = 20
df_RC = df %>% filter(redCrossSize==redCrossSizeToPlot)
dim(df_RC)

## 3D plot (no aggregation)
x = df_RC$redCrossEfficiencyProbability
y = df_RC$redCrossActivationDelay
z = df_RC$humans +  df_RC$rescued
# high variability !
plot3d(x,y,z) # écart type entre 30 et 40 !


## aggregation by meann and sd over replications with the selected redCross size
df_aggregated_RC = df_aggregated %>% filter(redCrossSize==redCrossSizeToPlot)

## 3D plot
dim(df_aggregated_RC)
x = df_aggregated_RC$redCrossEfficiencyProbability
y = df_aggregated_RC$redCrossActivationDelay
z = df_aggregated_RC$meanRescued
z = df_aggregated_RC$meanRescued + df_aggregated_RC$meanHumans
#z = df_aggregated_RC$sdRescued 
plot3d(x,y,z)


## hitmap
ggplot(df_aggregated_RC, aes(redCrossEfficiencyProbability,redCrossActivationDelay, fill = meanRescued + meanHumans)) + 
  geom_raster() +
  labs( title = "Hitmap of mean final humans (rescued or not)") +
  labs(subtitle = paste("redCorss Size = " , df_aggregated_RC$redCrossSize[1]) )
#ggsave("hitmap_mean_humans.png")

v <- ggplot(df_aggregated_RC, aes(redCrossEfficiencyProbability,redCrossActivationDelay, z = meanRescued + meanHumans))
v + geom_raster(aes(fill = meanRescued)) +
  geom_contour(colour = "white")

# test d'autre affichages de lignes de niveau
# v + geom_contour()
# v + geom_density_2d()
# v + geom_contour(bins = 10)
# v + geom_contour(binwidth = 5)
# v + geom_raster(aes(fill = meanRescued)) +
#   geom_contour(colour = "white")


#######################################################################
###     HITMAP FOR MEAN OUTPUTS WITH FACET WRAP ON REDCROSS SIZE    ###
#######################################################################

#df4 = df %>% filter(redCrossSize %%4 ==0)                                                                                                      
# filter(redCrossSize == 0 | redCrossSize == 5 | redCrossSize == 10 | redCrossSize == 20)
ggplot(df_aggregated %>% filter(redCrossSize == 0 | redCrossSize == 4 | redCrossSize == 10 | redCrossSize == 14 |
       redCrossSize == 18 | redCrossSize == 22 | redCrossSize == 26 | redCrossSize == 40 | redCrossSize == 60),
       aes(redCrossEfficiencyProbability,redCrossActivationDelay, fill = meanRescued + meanHumans)) + 
  facet_wrap(~redCrossSize) +
  geom_raster() +
  labs( title = "Hitmap of mean final humans (rescued or not) for different redCross size") 
#ggsave("hitmap_mean_humans_facetRC_2.png")




#######################################################################################################
###     FIND POINTS IN THE (efficiency,delay,redCrossSize) SPACE SUCH THAT... ("GOOD ANTIDOTE")    ###
#######################################################################################################

# utils for plot
plotSel <- function(sel, df= df_aggregated) {
  plot3d(df$redCrossEfficiencyProbability[sel], df$redCrossActivationDelay[sel], df$redCrossSize[sel], 
         xlim = range(df$redCrossEfficiencyProbability),
         ylim = range(df$redCrossActivationDelay),
         zlim = c(0,300),
         cex=3,
         col="red")
}

# max de human + rescued
selMaxSaved = which(df_aggregated$meanHumans + df_aggregated$meanRescued == max(df_aggregated$meanHumans + df_aggregated$meanRescued))
df_aggregated[selMaxSaved,]
selSaved = which( abs(df_aggregated$meanHumans + df_aggregated$meanRescued - max(df_aggregated$meanHumans + df_aggregated$meanRescued)) <15)
length(selSaved)
plotSel(selSaved)

# min zombies
selMinZombies = which(df_aggregated$meanZombies == min(df_aggregated$meanZombies))
df_aggregated[selMinZombies,]
# min zombified
selMinZombified = which(df_aggregated$meanZombified == min(df_aggregated$meanZombified))
df_aggregated[selMinZombified,]

# the best antidote values are as expected: delay min, efficiency max, redCrossSize max






