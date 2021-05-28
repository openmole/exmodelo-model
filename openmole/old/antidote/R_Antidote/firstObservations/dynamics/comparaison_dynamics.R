# comparaison dynamics

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

resNoRC <- read.csv("dynamics_NoRC.csv", header = T)
resRC_full_efficiency <- read.csv("dynamics_RC_20_eff_1.csv", header = T)
resRC_low_efficiency <- read.csv("dynamics_RC_20_eff_0.95.csv", header = T)

# dataFrame
dfNoRC = as.data.frame(resNoRC)
dfRC_full_efficiency = as.data.frame(resRC_full_efficiency)
dfRC_low_efficiency = as.data.frame(resRC_low_efficiency)

# dimensions
dim(resNoRC)
dim(resRC_full_efficiency)
dim(resRC_low_efficiency)

# change format
dfNoRC$seed = as.factor(dfNoRC$seed)
dfRC_full_efficiency$seed = as.factor(dfRC_full_efficiency$seed)
dfRC_low_efficiency$seed = as.factor(dfRC_low_efficiency $seed)

# utils: 
l = length(unique(dfNoRC$seed)); l # number of replications
dfNoRC %>% select(seed, humansDynamic) %>%  group_by(seed) %>% summarise(count = n()) # number of steps (in outputs)
n = dim(dfNoRC)[1]/l;n  # idem (steps)

# add a time column to the df
v = rep(1:n,l)
dfNoRC$times = v
dfRC_full_efficiency$times = v
dfRC_low_efficiency$times = v

# concatenate the dfs, add a column for the experiment names (replications with same parameters) 
v_NoRC = rep("NoRC",n*l)
v_RC_full_efficiency = rep("eff_max",n*l)
v_RC_low_efficiency = rep("eff_low",n*l)
dfNoRC$case = v_NoRC
dfRC_full_efficiency$case = v_RC_full_efficiency
dfRC_low_efficiency$case = v_RC_low_efficiency

df = rbind(dfNoRC,dfRC_full_efficiency,dfRC_low_efficiency)
dim(df)



################################
###     PLOT TRAJECTORIES    ###
################################
# no aggregation: cumsum
df$cumSumRescued = ave(df$rescuedDynamic, df$case, df$seed, FUN=cumsum)
monggplot <-  ggplot(df, aes(x=times, y=cumSumRescued), group = seed) + facet_grid(~case) +
  geom_line(aes(color=seed)) +
  #geom_line( aes(color=seed)) +
  #geom_point( aes(color=case)) +
  xlab("time") +
  labs( title = "Rescued dynamics with/without redCross, with differents redCross efficiency") 

#monggplot
monggplot + theme(legend.position="none")
#ggsave("comparison_rescuedDynamics_variability.png")
#ggplotly(monggplot)


# just humans
df2 = df %>% group_by(case,times) %>% summarise(meanHuman = mean(humansDynamic))
monggplot <-  ggplot(df2, aes(x=times, y= meanHuman, group = case))+
  geom_line( aes(color=case))
monggplot
#ggplotly(monggplot)

# all dynamics
df$cumSumRescued = ave(df$rescuedDynamic, df$case, df$seed, FUN=cumsum)
df$cumSumKilled = ave(df$killedDynamic, df$case, df$seed, FUN=cumsum)
# ave, from https://stackoverflow.com/questions/16850207/calculate-cumulative-sum-within-each-id-group

df3 = df %>% group_by(case,times) %>% summarise(meanHuman = mean(humansDynamic), meanZombies = mean(zombiesDynamic), 
                                                meanCumSumRescued = mean(cumSumRescued), meanCumSumKilled  = mean(cumSumKilled),
                                                sdHuman = sd(humansDynamic),  sdZombies = sd(zombiesDynamic),
                                                sdRescued = sd(cumSumRescued), sdKilled = sd(cumSumKilled)
)

# reorganize the df, and just keep dynamics outputs
df4_a = melt(df3 %>% select(times, case, meanHuman, meanZombies, meanCumSumRescued, meanCumSumKilled), id.vars = c("times", "case"), 
             variable.name = "mean_dynamic_variable", 
             value.name = "mean_dynamic_value")

df4_b = melt(df3 %>% select(times, case, sdHuman, sdZombies, sdRescued, sdKilled), id.vars = c("times", "case"), 
             variable.name = "sd_dynamic_variable", 
             value.name = "sd_dynamic_value")

df4 = cbind(df4_a, sd_dynamic_value = df4_b$sd_dynamic_value)

# plot
# no sd
monggplot <-  ggplot(df4, aes(x=times, y=mean_dynamic_value , group = case))+ facet_grid(~mean_dynamic_variable) +
  geom_line( aes(color=case)) +
  xlab("time") +
  labs( title = "mean dynamics values with/without redCross, with differents redCross efficiency") +
  scale_fill_discrete(name="Conditions",
                      breaks=c("NoRO", "eff_low", "eff_max"),
                      labels=c("No RedCross", "RedCross size = 20,\n Efficiency = 0.9", "RedCross size = 20,\n Efficiency = 1"))

monggplot

# with sd / IC
monggplot <-  ggplot(df4 %>% filter(times %% 5==0), aes(x=times, y=mean_dynamic_value , group = case, color = case)) + facet_grid(~mean_dynamic_variable) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin= mean_dynamic_value - 1.96*sd_dynamic_value/sqrt(l), 
                    ymax= mean_dynamic_value + 1.96*sd_dynamic_value/sqrt(l) )) 
monggplot

#ggsave("comparison_dynamics_IC.png")


# + xlab("X") + ylab("Y") 
# + labs( title = paste0("Plot of zombification at trap location (",x,",",y,")")) 
# + labs(subtitle = paste("Nb Zombified = " , nbZombification) ) 
# leggend
# bp + scale_fill_discrete(name="Experimental\nCondition",
#                          breaks=c("ctrl", "trt1", "trt2"),
#                          labels=c("Control", "Treatment 1", "Treatment 2"))

# ggsave(paste0(nbZombification,"z_heatmap_(",x,",",y,").png"))


#################################################
###   PLOT DYNAMICS REPLICATIONS SEPARATELY   ###
#################################################

# humans
# df$humansDynamic df$rescuedDynamic df$humansGoneDynamic
monggplot <-  ggplot(df, aes(x=times, y=humansDynamic, group = seed)) + facet_grid(~case) +
  geom_line( aes(color=seed))
monggplot + theme(legend.position="none")
#ggplotly(monggplot)

# rescued humans
# df$rescuedDynamic 
monggplot <-  ggplot(df, aes(x=times, y=rescuedDynamic, group = seed)) + facet_grid(~case) +
  geom_line( aes(color=seed))
monggplot + theme(legend.position="none")
#ggplotly(monggplot)

# rescued CumSum 
df$cumSumRescued = ave(df$rescuedDynamic, df$case, df$seed, FUN=cumsum)
# rescued humans
monggplot <-  ggplot(df, aes(x=times , y=cumSumRescued, group = seed)) + facet_grid(~case) +
  geom_point( aes(color=seed))
monggplot + theme(legend.position="none")
#ggplotly(monggplot)


# zombies
# df$zombiesDynamic df$killedDynamic df$zombiesGoneDynamic
monggplot <-  ggplot(df, aes(x=times, y=zombiesDynamic, group = seed)) + facet_grid(~case)+
  geom_line( aes(color=as.factor(seed)))
monggplot + theme(legend.position="none")
# ggplotly(monggplot)

# killed zombies
# df$killedDynamic
monggplot <-  ggplot(df, aes(x=times, y=killedDynamic, group = seed)) + facet_grid(~case)+
  geom_line( aes(color=as.factor(seed)))
monggplot + theme(legend.position="none")
# ggplotly(monggplot)


# killed zombies (cumSum)
df$cumSumKilled = ave(df$killedDynamic, df$case, df$seed, FUN=cumsum)
monggplot <-  ggplot(df, aes(x=times, y=cumSumKilled, group = seed)) + facet_grid(~case)+
  geom_line( aes(color=as.factor(seed)))
monggplot + theme(legend.position="none")



#################################################
###      PLOT ONE TRAJECTORY (DYNAMICS)       ###
#################################################

# reshape dataframe
df10 = melt(df %>% select(seed, times, case, redCrossEfficiencyProbability, redCrossActivationDelay, redCrossSize, humansDynamic, zombiesDynamic, cumSumRescued, cumSumKilled), id.vars = c("seed", "times", "case",
                                                                  "redCrossEfficiencyProbability", "redCrossActivationDelay", "redCrossSize"), 
           variable.name = "dynamic_variable", 
           value.name = "dynamic_value")


unique(df$case)
casePlot = "eff_max"  # NoRC, eff_max, eff_low
df11 = df10 %>% filter(case == casePlot) 
i=1  # seed between 1 and l
seed1 = df11$seed[i]
df11 = df11 %>% filter(seed == seed1)

monggplot <-  ggplot(df10 %>% filter(seed == seed1), aes(x=times, y= dynamic_value, group = dynamic_variable))+
  geom_line( aes(color=dynamic_variable)) +
  xlab("time") +
  labs( title = "dynamics (trajectory) of one model simulation") +
  labs(subtitle = paste0("redCross size = ", df11$redCrossSize, ", efficiency = ", df11$redCrossEfficiencyProbability[i] , 
          ", delay =", df11$redCrossActivationDelay[i]) )
monggplot
#ggplotly(monggplot)
# ggsave("trajectory_one_simulation.png")




