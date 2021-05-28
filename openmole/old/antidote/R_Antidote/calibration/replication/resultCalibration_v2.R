# calibration nsga2
# with the aggregate version in openmole, en prenant la médiane sur les replications
# data is mean over 50 réplications

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
library(fields)

################################
###        IMPORT DATA       ###
################################

res <- read.csv("resultsNSGA_v2/resultsNSGA_v2/population4500.csv", header = T)

# dataFrame
df = as.data.frame(res)
dim(df)
df$evolution.samples


df2 = df %>% arrange(desc(evolution.samples)) %>% filter(evolution.samples ==100)
dim(df2)
df2
sel = which(df2$humansDynamic == min(df2$humansDynamic))
df2[sel,]


truePointData = c(4,1,0.6)


# bornes de la calbration  
# redCrossActivationDelay in (0, 10),
# redCrossEfficiencyProbability in (0.7, 1.0),
# redCrossExhaustionProbability in (0.45, 1.0) 

# scatter plot 3d

# param for continous color
z = df2$humansDynamic
nbcol <- heat.colors(length(z))
nbcol[zcol]
# standardize z to be from 1 to z
zcol <-  ((z-min(z))/(max(z)-min(z)))*(length(z)-1)+1
length(zcol)
s3d <- scatterplot3d(cbind(df2$redCrossActivationDelay, df2$redCrossEfficiencyProbability, df2$redCrossExhaustionProbability),
                     pch = 25, color=nbcol[zcol],
                     xlim = c(0,10), ylim = c(0.0,1.0), zlim = c(0.0,1.0),
                     main = "result of calibration (find antidote parameters)",
                     xlab = "redCrossActivationDelay",
                     ylab = "",
                     zlab = "redCrossExhaustionProbability")

s3d$points3d(c(truePointData[1]),c(truePointData[2]), c(truePointData[3]),
             col="blue", pch=16)

# chage y label
dims <- par("usr")
x <- dims[1]+ 0.85*diff(dims[1:2])
y <- dims[3]+ 0.14*diff(dims[3:4])
text(x,y,"redCrossEfficiencyProbability",srt=30)


# add continous legend color
par(mar=c(5, 4, 4, 2) + 0.1)
image.plot(legend.only=TRUE, zlim= c(min(z), max(z)), nlevel=128,
           col=heat.colors(128)) 





