library(dplyr)
library(ggplot2)
library(reshape2)

setwd(paste0(Sys.getenv('CS_HOME'),'/OpenMole/zombies/openmole/spatialsens'))

#res1 <- as.tbl(read.csv('exploration/20190330_174556_LHS_SPATIALSENS_REPLICATIONS_GRID.csv'))
#res2 <- as.tbl(read.csv('exploration/20190408_163218_LHS_SPATIALSENS_REPLICATIONS_GRID.csv'))
#res3 <- as.tbl(read.csv('exploration/20190411_090002_LHS_SPATIALSENS_REPLICATIONS_GRID.csv'))
#res <- as.tbl(read.csv('exploration/20190412_152247_LHS_SPATIALSENS_REPLICATIONS_GRID.csv'))
res <- as.tbl(read.csv('exploration/20190624_225411_LHS_SPATIALSENS_REPLICATIONS_GRID.csv'))

#res=rbind(res1,res2,res3,res4)
#resdir='results/20190330-20190411_LHS_SPATIALSENS_REPLICATIONS_GRID';dir.create(resdir)
#resdir='results/20190412_LHS_SPATIALSENS_REPLICATIONS_GRID';dir.create(resdir)
resdir='results/20190624_225411_LHS_SPATIALSENS_REPLICATIONS_GRID';dir.create(resdir)


finalTime = 50

# replications
#res %>% group_by(id) %>% summarize(count=n())
sres = res %>% group_by(generatorType,blocksMaxSize,blocksMinSize,blocksNumber,expMixtureCenters,expMixtureRadius,expMixtureThreshold,
                 percolationBordPoints,percolationLinkWidth,percolationProba,randomDensity) %>% summarize(count=n())

indics = c(paste0("humansDynamic",0:finalTime),paste0("zombiesDynamic",0:finalTime))
indic1=paste0("humansDynamic",0:finalTime)
indic2=paste0("zombiesDynamic",0:finalTime)
indic=paste0("zombifiedDynamic",0:finalTime)

params=c("generatorType","blocksMaxSize","blocksMinSize","blocksNumber",
         "expMixtureCenters","expMixtureRadius","expMixtureThreshold",
         "percolationBordPoints","percolationLinkWidth","percolationProba","randomDensity")

# define a trajectory distance function

# -> just dist to average jaude traj

reference1 = colMeans(res[res$generatorType=="jaude",indic1])
reference2 = colMeans(res[res$generatorType=="jaude",indic2])
reference = colMeans(res[res$generatorType=="jaude",indic])

#sample = sample(1:nrow(res),10000)
sample = 1:nrow(res)

tres = melt(res[sample,c(indic,params)],id.vars=params,measure.vars = indic,variable.name = "var")
tres$time = c(matrix(rep(0:finalTime,length(sample)),ncol=finalTime+1,byrow = T))
tres$indic = rep("zombified",nrow(tres))

tres1 = melt(res[sample,c(indic1,params)],id.vars=params,measure.vars = indic1,variable.name = "var")
tres1$time = c(matrix(rep(0:finalTime,length(sample)),ncol=finalTime+1,byrow = T))
tres1$indic = rep("humans",nrow(tres))
  
tres2 = melt(res[sample,c(indic2,params)],id.vars=params,measure.vars = indic2,variable.name = "var")
tres2$time = c(matrix(rep(0:finalTime,length(sample)),ncol=finalTime+1,byrow = T))
tres2$indic = rep("zombies",nrow(tres2))

tres=rbind(tres,tres2)

stres = tres %>%group_by(generatorType,blocksMaxSize,blocksMinSize,blocksNumber,expMixtureCenters,expMixtureRadius,expMixtureThreshold,
                            percolationBordPoints,percolationLinkWidth,percolationProba,randomDensity,
                         time,indic) %>%
    summarize(count=n(),sdValue=sd(value),value=mean(value),id=paste0(generatorType,blocksMaxSize,blocksMinSize,blocksNumber,expMixtureCenters,expMixtureRadius,expMixtureThreshold,
                                                                      percolationBordPoints,percolationLinkWidth,percolationProba,randomDensity)[1]) %>% filter(count > 50)

# count=stres %>% group_by(generatorType,blocksMaxSize,blocksMinSize,blocksNumber,expMixtureCenters,expMixtureRadius,expMixtureThreshold,percolationBordPoints,percolationLinkWidth,percolationProba,randomDensity,time,indic) %>% summarize(count=n())

#g=ggplot(stres,aes(x=time,y=value,colour=generatorType,linetype=indic,group=interaction(generatorType,indic)))
#g=ggplot(stres[stres$indic=='humans',],aes(x=time,y=value,colour=generatorType,group=id))
#g+geom_line(alpha=0.5)
#g+geom_smooth()
#g+geom_point()

sstres = stres %>% group_by(generatorType,time,indic)%>% summarize(sdValue=sd(value),value=mean(value))

g=ggplot(sstres[sstres$indic=='zombified'&sstres$generatorType!='random',],aes(x=time,y=value,ymin=value-sdValue,ymax=value+sdValue,colour=generatorType))
g+geom_line()+geom_point()+geom_errorbar()+
  ylab('Zombified')+xlab('Time')+stdtheme
ggsave(paste0(resdir,'/spatialsens_generators_zombified.png'),width=25,height=15,units='cm')



g=ggplot(sstres[sstres$indic=='humans'&sstres$generatorType!='random',],aes(x=time,y=value,ymin=value-sdValue,ymax=value+sdValue,colour=generatorType))
g+geom_line()+geom_point()+geom_errorbar()+
  ylab('humans')+stdtheme
ggsave(paste0(resdir,'/spatialsens_generators_humans.png'),width=20,height=15,units='cm')

g=ggplot(sstres[sstres$indic=='zombies'&sstres$generatorType!='random',],aes(x=time,y=value,ymin=value-sdValue,ymax=value+sdValue,colour=generatorType))
g+geom_line()+geom_point()+geom_errorbar()+
  ylab('zombies')+stdtheme
ggsave(paste0(resdir,'/spatialsens_generators_zombies.png'),width=20,height=15,units='cm')


  
# summary statistics on distance to reference
distances_humans = colSums(apply(res[,indic1],1,function(r){(r-reference1)^2}))
dres=cbind(res[,params],distances_humans)
mdistref = mean(distances_humans[res$generatorType=='jaude'])

g=ggplot(dres[dres$generatorType!='random',],aes(x=generatorType,y=distances_humans/mdistref))
g+geom_boxplot(outlier.size = NULL)+scale_y_log10()+xlab("Generator")+ylab("Relative distance to reference")+stdtheme
ggsave(paste0(resdir,'/reldistance.png'),width=20,height=15,units='cm')

