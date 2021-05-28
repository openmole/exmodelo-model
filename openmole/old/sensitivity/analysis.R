library(dplyr)
library(ggplot2)

setwd(paste0(Sys.getenv('CS_HOME'),'/OpenMole/zombies/openmole/sensitivity'))

#resprefix = '20190612_151834_GRID_SLURM' # NO PARAMETERS IN THIS FILE !
#resprefix='20190612_103602_LHS_SLURM' # this neither !
#resprefix='20190622_045352_GRID_GRID'
resprefix='20190623_133610_GRID_GRID'
#resprefix='20190623_134607_GRID_GRID'

res <- as.tbl(read.csv(paste0('exploration/',resprefix,'.csv')))
resdir=paste0('results/',resprefix,'/');dir.create(resdir)

# non ts indicators
indicators=c("halfZombified","peakSize","peakTime","totalZombified",
             "spatialMoranZombified","spatialSlopeZombified","spatialDistanceMeanZombified",
             "spatialEntropyZombified")

parameters = list(p1="humanFollowProbability",p2="humanInformProbability",p3="humanInformedRatio")
#parameters = list(p1="humanExhaustionProbability",p2="humanPerception",p3="humanRunSpeed")


#summary(res %>% group_by(id,replication) %>% summarize(count=n(),
#                                                       deltaHumanFollowProba=humanFollowProbability[1]-mean(humanFollowProbability),
#                                                       deltaHalfZombified=halfZombified[1]-mean(halfZombified),
#                                                       deltatotalZombified=totalZombified[1]-mean(totalZombified)
#))

ntres=res

# in case of time series
#ntres = res %>% group_by(id,replication) %>% 
#  summarize(halfZombified=halfZombified[1],peakSize=peakSize[1],peakTime=peakTime[1],totalZombified=totalZombified[1],
#            spatialMoranZombified=spatialMoranZombified[1],spatialSlopeZombified=spatialSlopeZombified[1],
#            spatialDistanceMeanZombified=spatialDistanceMeanZombified[1],spatialEntropyZombified=spatialEntropyZombified[1],
#            humanFollowProbability=humanFollowProbability[1],humanInformProbability=humanInformProbability[1],
#            humanInformedRatio=humanInformedRatio[1]
#            )

# variability
sres = ntres %>% group_by(id,
                          humanFollowProbability,humanInformProbability,humanInformedRatio
                          #humanExhaustionProbability,humanPerception,humanRunSpeed
                          ) %>% 
  summarize(halfZombifiedSd=sd(halfZombified),halfZombified=mean(halfZombified),halfZombifiedSharpe=halfZombified/halfZombifiedSd,
            peakSizeSd=sd(peakSize),peakSize=mean(peakSize),peakSizeSharpe=peakSize/peakSizeSd,
            peakTimeSd=sd(peakTime),peakTime=mean(peakTime),peakTimeSHarpe=peakTime/peakTimeSd,
            totalZombifiedSd=sd(totalZombified),totalZombified=mean(totalZombified),totalZombifiedSharpe=totalZombified/totalZombifiedSd,
            spatialMoranZombifiedSd=sd(spatialMoranZombified),spatialMoranZombified=mean(spatialMoranZombified),spatialMoranZombifiedSharpe=spatialMoranZombified/spatialMoranZombifiedSd,
            spatialSlopeZombifiedSd=sd(spatialSlopeZombified),spatialSlopeZombified=mean(spatialSlopeZombified),spatialSlopeZombifiedSharpe=spatialSlopeZombified/spatialSlopeZombifiedSd,
            spatialDistanceMeanZombifiedSd=sd(spatialDistanceMeanZombified),spatialDistanceMeanZombified=mean(spatialDistanceMeanZombified),spatialDistanceMeanZombifiedSharpe=spatialDistanceMeanZombified/spatialDistanceMeanZombifiedSd,
            spatialEntropyZombifiedSd=sd(spatialEntropyZombified),spatialEntropyZombified=mean(spatialEntropyZombified),spatialEntropyZombifiedSharpe=spatialEntropyZombified/spatialEntropyZombifiedSd
            )
summary(sres)


## summary plots of indicators

for(indic in indicators){
  g=ggplot(ntres,aes_string(x=parameters[['p1']],y=indic,group=parameters[['p2']],color=parameters[['p2']]))
  g+geom_point(pch='.')+geom_smooth()+facet_wrap(paste0("~",parameters[['p3']]))+stdtheme
  ggsave(file=paste0(resdir,indic,"_",parameters[['p1']],"_color",parameters[['p2']],"_facet",parameters[['p3']],".png"),width=32,height=30,units='cm')
}

for(indic in indicators){
  g=ggplot(ntres,aes_string(x=parameters[['p3']],y=indic,group=parameters[['p2']],color=parameters[['p2']]))
  g+geom_point(pch='.')+geom_smooth()+facet_wrap(paste0("~",parameters[['p1']]))+stdtheme
  ggsave(file=paste0(resdir,indic,"_",parameters[['p3']],"_color",parameters[['p2']],"_facet",parameters[['p1']],".png"),width=32,height=30,units='cm')
}

# -> humanInformedRatio has no effect ?

# check with morris / compare with indic variance ?


# same plots with average
for(indic in indicators){
  g=ggplot(sres,aes_string(x=parameters[['p1']],y=indic,group=parameters[['p2']],color=parameters[['p2']]))
  g+geom_point()+geom_line()+facet_wrap(paste0("~",parameters[['p3']]))+stdtheme
  ggsave(file=paste0(resdir,indic,"Average_",parameters[['p1']],"_color",parameters[['p2']],"_facet",parameters[['p3']],".png"),width=32,height=30,units='cm')
}

for(indic in indicators){
  g=ggplot(sres,aes_string(x=parameters[['p3']],y=indic,group=parameters[['p2']],color=parameters[['p2']]))
  g+geom_point()+geom_line()+facet_wrap(paste0("~",parameters[['p1']]))+stdtheme
  ggsave(file=paste0(resdir,indic,"Average_",parameters[['p3']],"_color",parameters[['p2']],"_facet",parameters[['p1']],".png"),width=32,height=30,units='cm')
}


#####
## targeted plots
p3val=0.4
for(indic in indicators){
  g=ggplot(sres[sres[,parameters[['p3']]]==p3val,],aes_string(x=parameters[['p1']],y=indic,group=parameters[['p2']],color=parameters[['p2']]))
  g+geom_point()+geom_line()+stdtheme
  ggsave(file=paste0(resdir,indic,"Average_p3Fixed",p3val,"_",parameters[['p1']],"_color",parameters[['p2']],"_facet",parameters[['p3']],".png"),width=32,height=30,units='cm')
}




## correlations

rho=cor(ntres[,indicators])

rhomin=rho;rhomax=rho
for(i1 in indicators){
  for(i2 in indicators){
    currentcor = cor.test(unlist(ntres[,i1]),unlist(ntres[,i2]))
    rho[i1,i2]=currentcor$estimate
    rhomin[i1,i2]=currentcor$conf.int[1];rhomax[i1,i2]=currentcor$conf.int[2]
  }
}

abs(rho / (rhomax - rhomin))
# -> corrs are robust





