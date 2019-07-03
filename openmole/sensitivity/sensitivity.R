library(dplyr)
library(ggplot2)

setwd(paste0(Sys.getenv('CS_HOME'),'/OpenMole/zombies/openmole/sensitivity'))

resprefix='20190618_141256_MORRIS_GRID'

res <- as.tbl(read.csv(paste0('sensitivity/',resprefix,'.csv')))
resdir=paste0('results/',resprefix,'/');dir.create(resdir)


