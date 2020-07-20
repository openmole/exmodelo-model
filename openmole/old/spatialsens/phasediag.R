library(dplyr)
library(ggplot2)
library(reshape2)

setwd(paste0(Sys.getenv('CS_HOME'),'/OpenMole/zombies/openmole/spatialsens'))

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))
source('functions.R')

# ! this experiment was no world id
#res <- as.tbl(read.csv('exploration/20190417_171253_ONEFACTOR_SPATIALSENS_PHASEDIAG_GRID.csv'))
# ! this experiment has no model params
#res <- as.tbl(read.csv('exploration/20190419_085711_ONEFACTOR_SPATIALSENS_PHASEDIAG_GRID.csv'))

#resdir='results/20190417_171253_ONEFACTOR_SPATIALSENS_PHASEDIAG_GRID';dir.create(resdir)
resdir='results/20190419_085711_ONEFACTOR_SPATIALSENS_PHASEDIAG_GRID';dir.create(resdir)

finalTime = 50


# variability accross all replications
sres = res %>% group_by(replication) %>% summarize(count=n(),finalHumansPhasdiagSd = sd(humansDynamic50),
                                            finalHumansPhasediagAmplitude = max(humansDynamic50) - min(humansDynamic50),
                                            finalHumansPhasediagMin =  min(humansDynamic50),
                                            finalHumansPhasediagMax =  max(humansDynamic50),
                                            finalZombiesPhasediagSd = sd(zombiesDynamic50),
                                            finalZombiesPhasediagAmpl = max(zombiesDynamic50) - min(zombiesDynamic50)
                                            )
summary(sres)

sreslocal = res %>% group_by(id) %>% summarize(count=n(),finalHumansPhasdiagSd = sd(humansDynamic50),
                                                   finalHumansPhasediagAmplitude = max(humansDynamic50) - min(humansDynamic50),
                                                   finalHumansPhasediagMin =  min(humansDynamic50),
                                                   finalHumansPhasediagMax =  max(humansDynamic50)
)
summary(sreslocal)

sresgroupped = res %>% group_by(id,replication) %>% summarize(count=n(),finalHumansPhasdiagSd = sd(humansDynamic50),
                                                  finalHumansPhasediagAmplitude = max(humansDynamic50) - min(humansDynamic50),
                                                  finalHumansPhasediagMin =  min(humansDynamic50),
                                                  finalHumansPhasediagMax =  max(humansDynamic50)
)

# -> some param values make up to half ampl variation across the different worlds !
# -> even more variations for zombies

# TODO distances between phase diags
#
# ref = Jaude ?
# -> need world ids !

# -> worldid = one phase diag

sres = res %>% group_by(worldid) %>% summarise(count=n())

# d(phasediag,refphasediag) = f(worldid,generatorType)

# model parameters
params = c("infectionRange","humanRunSpeed","humanExhaustionProbability","humanInformProbability","humanFightBackProbability",
           "humanInformedRatio","humanFollowProbability","humanPerception","humanMaxRotation","zombieRunSpeed",
           "zombiePheromoneEvaporation","zombiePerception","zombieMaxRotation"
           )

distances = distancesToRef(
  simresults = res,reference=res[res$generatorType=='jaude',],parameters=params
  ,indicators=c("humansDynamic50","zombiesDynamic50"),
  idcol='worldid'
)







