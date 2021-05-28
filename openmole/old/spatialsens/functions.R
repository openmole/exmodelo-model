

#'
#' @description computes distances of phase diagrams to a reference, for given indicators
#'       
#' @param simresults : dataframe with grid ids, parameter values and indicators values
#' @param reference : dataframe with parameter values and indicator values for the reference grid
#' @param parameters : names of parameters
#' @param indicators : names of indicators
#' @param idcol : name of grid id column
#' 
distancesToRef <- function(simresults,reference,parameters,indicators,idcol,distfun=function(x,y){sqrt(sum((x-y)^2)/length(x))}){
  dists=c()
  names(parameters)=parameters
  for(id in unique(simresults[[idcol]])){
    d=left_join(reference,simresults[simresults[[idcol]]==id,],by=parameters)
    # remove nas -> do it at the indicator level
    #d = d[apply(d,1,function(r){length(which(is.na(r)))==0}),]
    currentdist=0
    for(indic in indicators){
      x = d[[paste0(indic,'.x')]][(!is.na(d[[paste0(indic,'.x')]]))&(!is.na(d[[paste0(indic,'.y')]]))]
      y = d[[paste0(indic,'.y')]][(!is.na(d[[paste0(indic,'.x')]]))&(!is.na(d[[paste0(indic,'.y')]]))]
      effdist=0
      if(!is.function(distfun)){if(distfun=='emd'){
        # particular case of emd -> bind parameter columns ; normalize
        params = d[(!is.na(d[[paste0(indic,'.x')]]))&(!is.na(d[[paste0(indic,'.y')]])),parameters]
        for(j in 1:ncol(params)){
          params[,j]=(params[,j] - min(params[,j]))/(max(params[,j]) - min(params[,j]))
        }
        xx=cbind(x,params);yy=cbind(y,params)
        effdist = emd(as.matrix(xx),as.matrix(yy))
      }
      }else{
        effdist = distfun(x,y)
      }  
      if(sd(x)+sd(y)>0){currentdist=currentdist+2*effdist^2/(sd(x)^2+sd(y)^2)}
    }
    finaldist = currentdist/length(indicators)
    
    # specific case for EMD
    #x = d[complete.cases(d[,c(paste0(indicators,'.y'),paste0(indicators,'.x'))]),paste0(indicators,'.x')]
    #y = d[complete.cases(d[,c(paste0(indicators,'.y'),paste0(indicators,'.x'))]),paste0(indicators,'.y')]
    #x=cbind(rep(1,nrow(x)),x);y=cbind(rep(1,nrow(y)),y)
    #emd(x,y)
    
    dists=append(dists,finaldist)
  }
  names(dists)=unique(sres[[idcol]])
  return(dists)
  
  #show(id)
  #dists=append(dists,emd(as.matrix(sref[,c(4,1:3)]),as.matrix(sres[sres$id==id,c(6,3:5)])))
  #show(length(sres$gini[sres$id==id]))
  #dists=append(dists,2*(sd(d$gini.x-d$gini.y,na.rm = T)^2)/(sd(d$gini.x)^2+sd(d$gini.y,na.rm = T)^2))
  
}


