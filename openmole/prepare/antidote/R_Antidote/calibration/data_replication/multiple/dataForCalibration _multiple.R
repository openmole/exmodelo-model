# Data fro calibration

# aim : create a csv file with the human dynamic array on row for each sampling parameter 
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
library(stringr)

################################
###        IMPORT DATA       ###
################################

res <- read.csv("dataForCalibrationMultiple_HumanDyamics3.csv", header = T)

# dataFrame
df = as.data.frame(res)
df = as_tibble(df)
# dimensions
dim(df)

tempMat = c()
for (i in 1:dim(df)[1]){
  temp = as.vector(df$humansDynamicArray[i])
  temp = substr(temp,2,str_length(temp)-1) #;temp
  temp = as.numeric(strsplit(temp, ",")[[1]]) #; temp
  tempMat = rbind(tempMat,temp)
}


n = 101 #= length(temp)  
colNames = paste0("humanDynamics",0:(length(temp)-1))
tempdf <- data.frame(tempMat)
colnames(tempdf) <- colNames
tempdf

df2 = cbind(df,tempdf)
df2 = as_tibble(df2)
head(df2)  

df2  = df2 %>% select(- humansDynamicArray)


######################################
###       SAVE DF IN CSV FILE      ###
######################################

write.csv(df2, file = "dataForCalibrationMultiple_HumanDyamics3_1.csv", row.names = FALSE)






  
  