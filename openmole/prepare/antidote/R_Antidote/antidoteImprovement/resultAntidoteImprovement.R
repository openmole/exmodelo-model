# nsga antidote improvement

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

################################
###        IMPORT DATA       ###
################################

res <- read.csv("resultsAntidoteImprovement2/population11600.csv", header = T)


# dataFrame
df = as.data.frame(res)
dim(df)
df$evolution.samples

# le bufbet est censé varier entre 0 et 3, o n'a pas laissé assez de temps à l'algo ?
range(df$improvementBudget)

df2 = df %>% arrange(desc(evolution.samples)) %>% filter(evolution.samples > 75)
df2 = df %>% arrange(desc(evolution.samples)) %>% filter(evolution.samples ==100)
dim(df2)
sort(df2$improvementBudget)[1:6]

# fitness
plot(df2$improvementBudget, df2$obj1)
# small change to maximize rescued
plot(df2$improvementBudget, -df2$obj1)

ggplot(df2, aes(improvementBudget,obj1)) + 
  geom_point() +
  labs( title = "Pareto front on objectives (maximized rescued and minimize cost) for antidote improvment")+ 
  labs(subtitle = paste("current antidote features: delay = 4, efficiency = 0.98, exhaustion = 0.6" ) )
#ggsave("paretoFront_antidoteImprovement_11,6k.png")


# ce n'est pas fait pour ca (pas injectif), mais bon
range(df2$redCrossEfficiencyProbabilityBonus)
range(df2$redCrossExhaustionProbabilityBonus)
range(df2$redCrossActivationDelayBonus)

# params
plot3d(df2$redCrossActivationDelay, df2$redCrossEfficiencyProbability, df2$redCrossExhaustionProbability, 
       xlim = c(0,1), ylim = c(0,1), zlim = c(0,1), cex=10)
# projection
plot(df2$redCrossActivationDelay, df2$redCrossEfficiencyProbability)


myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}
cols <- myColorRamp(c("red", "blue"), df2$improvementBudget) 

scatterplot3d(df2$redCrossActivationDelayBonus, df2$redCrossEfficiencyProbabilityBonus, df2$redCrossExhaustionProbabilityBonus,
              pch = 25, color="steelblue",
              xlim = c(0,1), ylim = c(0,1), zlim = c(0,1),
              main = "Corresponding antidote features values of points in the pareto front ",
              xlab = "redCrossActivationDelay",
              ylab = "redCrossEfficiencyProbability",
              zlab = "redCrossExhaustionProbability" )#,col = cols)


#projection dans le plan exhaustion = 0
ggplot(df2, aes(x=redCrossActivationDelayBonus, y=redCrossEfficiencyProbabilityBonus)) + 
  geom_point(aes(color = improvementBudget)) +
  labs( title = "Projections of parameters on the plan exhaustion=0") #+ 
  #labs(subtitle = paste("current antidote features: delay = 4, efficiency = 0.98, exhaustion = 0.6" ) )
#ggsave("result_Projectionsparameters_antidoteImprovement.png")

#
# rgl.init()
# rgl.spheres(df2$redCrossActivationDelay, df2$redCrossEfficiencyProbability, df2$redCrossExhaustionProbability, 
#             r = 0.05, color = "yellow",
#             xlim = c(0,1), ylim = c(0,1), zlim = c(0,1)
#             )  # Scatter plot
# rgl.bbox(color=c("#333377","black"), emission="#333377",
#          specular="#3333FF", shininess=5, alpha=0.8 ) 



