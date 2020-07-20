# outputs laws

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

resNoRC <- read.csv("outputsLaws_NoRC.csv", header = T)
resRC_full_efficiency <- read.csv("outputsLaws_RC_20_eff_1.csv", header = T)
resRC_low_efficiency <- read.csv("outputsLaws_RC_20_eff_0.95.csv", header = T)

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

# concatenate the dfs, add a column for the experiment names (replications with same parameters) 
v_NoRC = rep("NoRC",l)
v_RC_full_efficiency = rep("eff_max",l)
v_RC_low_efficiency = rep("eff_low",l)
dfNoRC$case = v_NoRC
dfRC_full_efficiency$case = v_RC_full_efficiency
dfRC_low_efficiency$case = v_RC_low_efficiency

df = rbind(dfNoRC,dfRC_full_efficiency,dfRC_low_efficiency)
dim(df)


################################
###      LAW OF OUTPUTS      ###
################################

# INDIVIDUAL GRAPH (ONE OUTPUTS)

# dataframe for the mean and sd, to add line on plot
mu_sd <- df %>% select(case, rescued, humans, zombies, killed)  %>%  group_by(case) %>% 
                      summarise(case.rescued.mean = mean(rescued), case.rescued.sd = sd(rescued),
                                case.humans.mean = mean(humans), case.humans.sd = sd(humans),
                                case.zombies.mean = mean(zombies), case.zombies.sd = sd(humans),
                                case.killed.mean = mean(killed), case.killed.sd = sd(killed))

mu_sd

# histograms with many replications on final quantities
# rescued (last time, sum) sur des graphes différents (case)
# df$rescued
#a <- ggplot(df, aes(x = rescued)) 
a <- ggplot(df, aes(x = rescued, y=..density..)) + facet_grid(~case)  
a + geom_density(alpha=0.2)
a + geom_histogram(bins = 30, color = "black", fill = "white") + geom_density(alpha=0.2, fill = "red")
#ggsave("outputsLaws_humans.png")

# rescued (last time, sum) sur le même graphe (group by case)
## density lines
a <- ggplot(df, aes(x = rescued, y=..density.., group = case, color = case))  
a + geom_density(alpha=0.2) + geom_vline(data = mu_sd, aes(xintercept =case.rescued.mean, color=case),
           linetype="dashed")
## histogram
a <- ggplot(df, aes(x = rescued, y=..density.., group = case, fill=case, color = case))  
a + geom_histogram(bins = 30, alpha=0.2, position="identity") #+ geom_density(alpha=0.2)


# zombies (last time)
a <- ggplot(df, aes(x = zombies, y=..density.., group = case, color = case))  
a + geom_density(alpha=0.2)
a <- ggplot(df, aes(x = zombies, y=..density.., group = case, fill=case, color = case))  
a + geom_histogram(bins = 30, alpha=0.2, position="identity")

# killed zombies (end) sum # pas dans les varaibles, c'est zombified + zombiesSize (ini) - zombies 
a <- ggplot(df, aes(x = killed, y=..density.., group = case, color = case))  
a + geom_density(alpha=0.2)
a <- ggplot(df, aes(x = killed, y=..density.., group = case, fill=case, color = case))  
a + geom_histogram(bins = 30, alpha=0.2, position="identity")

# humans
a <- ggplot(df, aes(x = humans, y=..density.., group = case, color = case))  
a + geom_density(alpha=0.2)
a <- ggplot(df, aes(x = humans, y=..density.., group = case, fill=case, color = case))  
a + geom_histogram(bins = 30, alpha=0.2, position="identity")



# ALL OUTPUTS ON A SAME GRAPHIQUE

# reshape data
df$zombies
df = mutate(df, killed = zombified + zombiesSize - zombies)
# df$killed

# reorganize the df, and just keep dynamics outputs
df2 = melt(df %>% select(case, humans, zombies, rescued, killed), id.vars = c("case"), 
             variable.name = "outputs_variable", 
             value.name = "outputs_value")

monggplot <-  ggplot(df2, aes(x=outputs_value, y=..density.. , group = case, color = case)) + facet_grid(~outputs_variable) +
  labs( title = "Histogram for final outputs with/without redCross, with differents redCross efficiency") 

#monggplot + geom_density(alpha=0.2)
monggplot + geom_histogram(bins = 30, alpha=0.2, position="identity")

#ggsave("outputsLaws_rescued3_mean.png")




