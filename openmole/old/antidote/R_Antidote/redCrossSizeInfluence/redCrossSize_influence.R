# redCrossSize

################################
###         PACKAGES         ###
################################
library(dplyr)
library(matrixStats)
library(ggplot2)
library(reshape2)
library(plotly)

################################
###        IMPORT DATA       ###
################################

resBest <- read.csv("redCrossSize_eff_1_delay_1.csv", header = T)
resLowEffi <- read.csv("redCrossSize_eff_0.95_delay_1.csv", header = T)
resLongDelay <- read.csv("redCrossSize_eff_1_delay_4.csv", header = T)

# dataFrame
dfBest = as.data.frame(resBest)
dfLowEffi = as.data.frame(resLowEffi)
dfLongDelay = as.data.frame(resLongDelay)

# dimensions
dim(dfBest)
dim(dfLowEffi)
dim(dfLongDelay)

# change format
dfBest$seed = as.factor(dfBest$seed)
dfLowEffi$seed = as.factor(dfLowEffi$seed)
dfLongDelay$seed = as.factor(dfLongDelay $seed)

# utils: 
l = length(unique(dfBest$seed)); l # total number of replications
dfBest %>% select(seed, humansDynamic) %>%  group_by(seed) %>% summarise(count = n()) # number of steps (in outputs)
n = dim(dfBest)[1]/l;n  # idem (steps)
length(unique(dfBest$redCrossSize))
l / length(unique(dfBest$redCrossSize))

# add a time column to the df
v = rep(1:n,l)
dfBest$times = v
dfLowEffi$times = v
dfLongDelay$times = v

# concatenate the dfs, add a column for the experiment names (replications with same parameters) 
v_Best = rep("Best",n*l)
v_LowEffi = rep("low_eff",n*l)
v_LongDelay = rep("Long_del",n*l)
dfBest$case = v_Best
dfLowEffi$case = v_LowEffi
dfLongDelay$case = v_LongDelay

df = rbind(dfBest,dfLowEffi,dfLongDelay)
dim(df)



########################################################################
###     PLOT DYNAMICS (vs time) FOR DIFFERENT REDCROSS PROPORTION    ###
########################################################################

df$cumSumRescued = ave(df$rescuedDynamic, df$case, df$seed, FUN=cumsum)
df$cumSumKilled = ave(df$killedDynamic, df$case, df$seed, FUN=cumsum) 

df %>% select(seed, times, redCrossSize, humansDynamic) %>%  group_by(redCrossSize, times) %>% summarise(count = n()) # nombre de pas de temps par simu
# mean over replications
unique(df$case)
caseToPlot = "low_eff"  # Long_del  Best
df2 = df %>% filter(case == caseToPlot) %>% filter(redCrossSize < 50) %>% select(seed, times, redCrossSize, 
                                case, humansDynamic, zombiesDynamic, cumSumKilled, cumSumRescued, redCrossEfficiencyProbability,
                                redCrossActivationDelay) %>%  group_by(redCrossSize, times, case, redCrossEfficiencyProbability,
                                redCrossActivationDelay) %>% summarise(meanHumans = mean(humansDynamic),
                                meanZombies = mean(zombiesDynamic), meanCumSumRescued = mean(cumSumRescued), 
                                meanCumSumKilled = mean(cumSumKilled) )

# reorganize the df + choose one of the exp (value of delay and efficiency)
df3 = melt(df2 %>% select(times, case, redCrossSize, meanHumans, meanZombies, meanCumSumRescued, meanCumSumKilled, redCrossEfficiencyProbability, redCrossActivationDelay), id.vars = c("times", "case", "redCrossSize", "redCrossEfficiencyProbability", "redCrossActivationDelay"), 
             variable.name = "mean_dynamic_variable", 
             value.name = "mean_dynamic_value")
dim(df3)
head(df3)
# mean human
monggplot <-  ggplot(df3, aes(x=times, y=mean_dynamic_value , group = redCrossSize)) + facet_grid(~mean_dynamic_variable) +
   geom_line( aes(color=redCrossSize)) +
   labs( title = "Mean dynamics with respect to redCross size") +
   labs(subtitle = paste("efficiency = " , df3$redCrossEfficiencyProbability[1], ", delay = ", df3$redCrossActivationDelay[1])) 
monggplot
#ggplotly(monggplot)

# ggsave("redCrossSizeInfluence_dynamics_Low_eff.png")


# mean over replications
df2_b = df %>% filter(redCrossSize <= 50) %>% select(seed, times, redCrossSize, 
                  case, humansDynamic, zombiesDynamic, cumSumKilled, cumSumRescued, redCrossEfficiencyProbability,
                  redCrossActivationDelay) %>%  group_by(redCrossSize, times, case, redCrossEfficiencyProbability,
                  redCrossActivationDelay) %>% summarise(meanHumans = mean(humansDynamic),
                  meanZombies = mean(zombiesDynamic), meanCumSumRescued = mean(cumSumRescued), 
                  meanCumSumKilled = mean(cumSumKilled) )

# reorganize the df + choose one of the exp (value of delay and efficiency)
df3_b = melt(df2_b %>% select(times, case, redCrossSize, meanHumans, meanZombies, meanCumSumRescued, meanCumSumKilled, redCrossEfficiencyProbability, redCrossActivationDelay), id.vars = c("times", "case", "redCrossSize", "redCrossEfficiencyProbability", "redCrossActivationDelay"), 
           variable.name = "mean_dynamic_variable", 
           value.name = "mean_dynamic_value")
dim(df3_b)
head(df3_b)
# mean human
monggplot <-  ggplot(df3_b, aes(x=times, y=mean_dynamic_value , linetype= case, group = interaction(redCrossSize, case))) + facet_grid(~mean_dynamic_variable) +
  geom_line(aes(color=redCrossSize)) + 
  labs( title = "Mean dynamics with respect to redCross size") #+
  #labs(subtitle = paste("efficiency = " , df3$redCrossEfficiencyProbability[1], ", delay = ", df3$redCrossActivationDelay[1])) 
monggplot

# ggsave("redCrossSizeInfluence_dynamics_Low_eff.png")


# plot group by case (on a same graphic, vs time), facet wrap on redCross size
monggplot <-  ggplot(df3_b %>% filter(redCrossSize %% 10 ==0), aes(x=times, y=mean_dynamic_value , linetype= case, group = interaction(case, mean_dynamic_variable))) + facet_wrap(~redCrossSize) +
  geom_line(aes(color=mean_dynamic_variable)) + 
  labs( title = "Mean dynamics with respect to redCross size and redCross parameters") #+
#labs(subtitle = paste("efficiency = " , df3$redCrossEfficiencyProbability[1], ", delay = ", df3$redCrossActivationDelay[1])) 
monggplot
# ggsave("redCrossSizeInfluence_dynamics_allParams.png")


unique(df3_b$mean_dynamic_variable)
monggplot <-  ggplot(df3_b %>% filter(mean_dynamic_variable == "meanHumans"), aes(x=times, y=mean_dynamic_value , linetype= case, group = interaction(case, mean_dynamic_variable))) + facet_wrap(~redCrossSize) +
  geom_line(aes(color=redCrossSize)) + 
  labs( title = "Mean dynamics with respect to redCross size") #+
#labs(subtitle = paste("efficiency = " , df3$redCrossEfficiencyProbability[1], ", delay = ", df3$redCrossActivationDelay[1])) 
monggplot



###########################################################
###     PLOT FINAL TIME OUTPUTS WRT TO REDCROSS SIZE    ###
###########################################################

df4 = df  %>% select(seed, times, redCrossSize, case, humans, zombies, killed, rescued, halfRescued, 
                  redCrossEfficiencyProbability, redCrossActivationDelay, humans, zombies, rescued, killed) %>%  
                  group_by(seed, case, redCrossSize, redCrossEfficiencyProbability, redCrossActivationDelay) %>% 
                  summarise(humans = mean(humans), zombies = mean(zombies), rescued = mean(rescued), killed = mean(killed) )#, halfRescued = mean(halfRescued))


# reorganize the df + choose one of the exp (value of delay and efficiency)
df5 = melt(df4 %>% select(seed,case, redCrossSize, humans, zombies, rescued, killed, redCrossEfficiencyProbability, redCrossActivationDelay), 
           id.vars = c("case", "redCrossSize", "redCrossEfficiencyProbability", "redCrossActivationDelay", "seed"), 
           variable.name = "output_variable", 
           value.name = "output_value")


dim(df5)
#df5 = df4 %>% filter(times<50) %>% filter(case == "Best")
#dim(df5)

monggplot <-  ggplot(df5, aes(x= redCrossSize, y=output_value, group = case )) + facet_grid(~output_variable) +
   geom_point(aes(color=case)) +
  labs( title = "Final outputs with respect to redCross size, for 3 parameters set") 
#  labs(subtitle = paste("efficiency = " , df3$redCrossEfficiencyProbability[1], ", delay = ", df3$redCrossActivationDelay[1])) 
monggplot
#ggplotly(monggplot)
# ggsave("redCrossSizeInfluence_final_outputs.png")


#### with mean values instead of points
df4_b = df  %>% select(seed, times, redCrossSize, case, humans, zombies, killed, rescued, halfRescued, 
                     redCrossEfficiencyProbability, redCrossActivationDelay, humans, zombies, rescued, killed) %>%  
  group_by(case, redCrossSize, redCrossEfficiencyProbability, redCrossActivationDelay) %>% 
  summarise(humans = mean(humans), zombies = mean(zombies), rescued = mean(rescued), killed = mean(killed))

# reorganize the df + choose one of the exp (value of delay and efficiency)
df5_b = melt(df4_b %>% select(case, redCrossSize, humans, zombies, rescued, killed, redCrossEfficiencyProbability, redCrossActivationDelay), 
           id.vars = c("case", "redCrossSize", "redCrossEfficiencyProbability", "redCrossActivationDelay"), 
           variable.name = "mean_output_variable", 
           value.name = "mean_output_value")

dim(df5_b)

monggplot <-  ggplot(df5_b, aes(x= redCrossSize, y=mean_output_value, group = case )) + facet_grid(~mean_output_variable) +
  geom_line(aes(color=case)) +
  labs( title = "Mean final outputs with respect to redCross size, for 3 parameters set")

monggplot
#ggplotly(monggplot)
# ggsave("redCrossSizeInfluence_mean_final_outputs.png")

