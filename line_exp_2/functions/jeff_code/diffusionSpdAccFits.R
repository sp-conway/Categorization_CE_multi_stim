
setwd("~/rtdists Example/")

# run this if you don't have rtdists installed
# install.packages("rtdists")
library("rtdists")


#see SpdAccData.R for details on these
#  data files
load("RatcliffThaparMcKoonDataSubset.RData")
subjects = dataList[[1]]
comboData = dataList[[2]]
fOld = dataList[[3]]
fNew = dataList[[4]]
qOld = dataList[[5]]
qNew = dataList[[6]]
fOldOv = dataList[[7]]
fNewOv = dataList[[8]]
qOldOv = dataList[[9]]
qNewOv = dataList[[10]]

#number of conditions
Ncond=dim(fOld)[1]


#load individual parameters from
#  earlier fits
load("indFitDiffusion.RData")
#matrix of parameter values, subjects
#  in rows
parDiff=indFitDiff[[1]]
#vector of g-squared values across subjects
gsqDiff=indFitDiff[[2]]


#Simulate N trials from the diffusion
#  process with parameter vector par.
#  par is:
#  boudary separation (a)
#  relative starting point (z/a)
#  range in starting point variability (sz)
#  average drift rate (v)
#  between-trial standard deviation in drift (sv)
#  minimum non-decision time (t0)
#  range in non-decision time (st0)
#  all with a diffusion coefficient of 1
#    (.1 is also a common value)
diffSample=function(N=1000,
                    par=c(1,.5,.02,1,1,.4,.2)){
  a=par[1]
  z=par[2]*par[1]
  sz=par[3]
  v=par[4]
  sv=par[5]
  t0=par[6]
  st0=par[7]
  
  sam=rdiffusion(N,a=a,v=v,t0=t0,z=z,d=0,sz=sz,sv=sv,st0=st0,
                 precision=3)
  
  r1rt=sam[which(sam[,2]=="upper"),1]
  r2rt=sam[which(sam[,2]=="lower"),1]
  qtop=as.numeric(quantile(r1rt,c(.1,.3,.5,.7,.9)))
  qbot=as.numeric(quantile(r2rt,c(.1,.3,.5,.7,.9)))
  ftop=length(r1rt)*c(.1,.2,.2,.2,.2,.1)
  fbot=length(r2rt)*c(.1,.2,.2,.2,.2,.1)
  f=c(ftop,fbot)
  ptop=sum(ftop)/sum(c(ftop,fbot))
  outList=list(qtop,qbot,f,ftop,fbot,ptop)
  return(outList)
}


#Predictions from the diffusion model
#  prediction equations. Gives the proportion
#  of responses in each RT bin, with bins
#  defined by quantiles for the top and bottom
#  boundary responses. The vector of predicted
#  bin proportions, p, has the bin proportions
#  for a top response followed by a bottom
#  response. With stadard .1,.3,.5,.7,.9
#  quantiles for each response, p will have
#  12 elements because each response has
#  6 RT bins.
#intUpLim is maximum time used for the
#  integrations in the predition equation
#  in seconds. 
diffPred=function(par,qtop,qbot,
                  intUpLim=10){
  a=par[1]
  z=par[2]*par[1]
  sz=par[3]
  v=par[4]
  sv=par[5]
  t0=par[6]
  st0=par[7]
  
  p=c()
  
  cuts=c(qtop,Inf)
  cp=pdiffusion(cuts,response="upper",a=a,v=v,
                t0=t0,z=z,d=0,sz=sz,sv=sv,st0=st0,
                precision=3,maxt=intUpLim)
  p=c(p,cp[1])
  if(length(cp)>1){
    for(i in 2:length(cp)){
      p=c(p,cp[i]-cp[i-1])
    }
  }
  
  cuts=c(qbot,Inf)
  cp=pdiffusion(cuts,response="lower",a=a,v=v,
                t0=t0,z=z,d=0,sz=sz,sv=sv,st0=st0,
                precision=3,maxt=10)
  p=c(p,cp[1])
  if(length(cp)>1){
    for(i in 2:length(cp)){
      p=c(p,cp[i]-cp[i-1])
    }
  }
  return(p)
}

#Function to apply parameter
#  constraints and apply a fit 
#  penalty for parameter sets
#  that violate the constraints.
cons=function(par){
  penf=0
  
  pi=1
  pmn=.1
  if(par[pi]<pmn){
    penf=penf+(pmn-par[pi]) 
    par[pi]=pmn
  }
  
  pi=2
  pmn=.05
  pmx=.95
  if(par[pi]<pmn){
    penf=penf+(pmn-par[pi]) 
    par[pi]=pmn
  }
  if(par[pi]>pmx){
    penf=penf+(par[pi]-pmx) 
    par[pi]=pmx
  }
  
  pi=3
  pmn=.0
  pmx=.05
  if(par[pi]<pmn){
    penf=penf+(pmn-par[pi]) 
    par[pi]=pmn
  }
  if(par[pi]>pmx){
    penf=penf+(par[pi]-pmx) 
    par[pi]=pmx
  }
  
  pi=5
  pmn=.1
  pmx=4
  if(par[pi]<pmn){
    penf=penf+(pmn-par[pi]) 
    par[pi]=pmn
  }
  if(par[pi]>pmx){
    penf=penf+(par[pi]-pmx) 
    par[pi]=pmx
  }
  
  pi=6
  pmn=0
  if(par[pi]<pmn){
    penf=penf+(pmn-par[pi]) 
    par[pi]=pmn
  }
  
  pi=7
  pmn=0
  if(par[pi]<pmn){
    penf=penf+(pmn-par[pi]) 
    par[pi]=pmn
  }
  
  outList=list(par,penf)
  return(outList)
}




#Function to compute g-squared based
#  on observed quantiles for top and
#  bottom boundary responses (qtop and qbot),
#  frequencies in the RT bins for top and
#  bottom boundary responses (ftop and fbot),
#  and a vector of parameter values for the
#  diffusion model (in the same order used
#  for the diffSample and diffPred functions.)
#medf and quantf control what happens for
#  responses with low trial counts.
# Frequencies from 0 to medf-1 are just fit
#  for the overall count without considering RT.
# Frequencies from medf to quantf-1 are fit
#  for the counts in 2 RT bins separated by the
#  median (the middle value in the q vector
#  for that response).
# Frequencies from quantf to Inf are fit for
#  the counts in 6 RT bins separated by the
#  quantiles stored in q for that response.
#intUpLim is the upper time limit for 
#  integrating the prediction equations.
gsqfun=function(par,qtop,qbot,ftop,fbot,
                medf=5,quantf=10,
                intUpLim=10){
  outList=cons(par)
  par=outList[[1]]
  penf=outList[[2]]
  
  #collapse bins for low counts
  if(sum(ftop)<medf){
    qtop=c()
    ftop=sum(ftop)
  }
  if(sum(ftop)>=medf & sum(ftop)<quantf){
    qtop=qtop[3]
    ftop=c(sum(ftop[1:3]),sum(ftop[4:6]))
  }
  if(sum(fbot)<medf){
    qbot=c()
    fbot=sum(fbot)
  }
  if(sum(fbot)>=medf & sum(fbot)<quantf){
    qbot=qbot[3]
    fbot=c(sum(fbot[1:3]),sum(fbot[4:6]))
  }
  
  
  f=c(ftop,fbot)
  
  p=diffPred(par,qtop,qbot,intUpLim)
  p[p<=0.0001 & f>0] = .0001
  p=p/sum(p) #renormalize so p adds up to 1 again
  pf=c() #predicted frequencies in each RT bin
  pf=p*sum(f)
  
  gslev=1:length(f)
  gslev=gslev[f>0]
  if(is.na(mean(log(f[gslev]/pf[gslev])))==TRUE) print(par)
  gsq=2*sum(f[gslev]*log(f[gslev]/pf[gslev]))
  #add in penalty for illegal parameter values
  gsq = gsq + penf*gsq
  
  return(gsq)
}


#The functions above work for a single
#  condition. When fitting data from
#  multiple conditions simultaneously,
#  you need to decide which parameters
#  will vary across conditions. You 
#  should be able to adapt this code to
#  any condition structure and model by
#  changing just the next 3 functions.


#  makeFullPar creates a matrix, fullPar,
#  where the rows are conditions and
#  the columns are parameters in the 
#  same order as the functions above.
#  It takes a single vector of parameters
#  across all conditions that optim will
#  use for the parameter search and maps
#  it to the fullPar matrix. 
makeFullPar=function(optPar){
  fullPar=matrix(NA,4,7)
  
  #boundary
  fullPar[1:2,1] = optPar[1]
  fullPar[3:4,1] = optPar[2]
  #starting point (as proportion of boundary)
  fullPar[1:2,2] = optPar[3]
  fullPar[3:4,2] = optPar[4]
  #starting point variability
  fullPar[1:4,3] = optPar[5]
  #drift rate
  fullPar[c(1,3),4] = optPar[6]
  fullPar[c(2,4),4] = optPar[7]
  #drift rate variability
  fullPar[1:4,5] = optPar[8]
  #minimum non-decision time
  fullPar[1:2,6] = optPar[9]
  fullPar[3:4,6] = optPar[10]
  #range in non-decision time
  fullPar[1:2,7] = optPar[11]
  fullPar[3:4,7] = optPar[12]
  
  return(fullPar)
}


#Returns default values for
#  optPar to begin parameter
#  search routines
resetOptPar=function(){
  optPar=c()
  
  #boundaries
  optPar[1:2] = 1.2
  #starting point (as proportion of boundary)
  optPar[3:4] = .5
  #starting point variability
  optPar[5] = .02
  #drift rate
  optPar[6:7] = 0
  #drift rate variability
  optPar[8] = 1.5
  #minimum non-decision time
  optPar[9:10] = .4
  #range in non-decision time
  optPar[11:12] = .2
  
  return(optPar)
}


#resets parameter vector to best fitting
#  values for overall data. These were 
#  found by running runOvFit()
#  This function is useful for individual
#  participant fits, because starting
#  them from the overall parameter values
#  should usually be pretty close to the
#  best-fitting values for a given participant.
#  You need to switch the values for different
#  data sets of course, after running runOvFit()
#  for the overall data. 
resetOptParOv=function(){
  optPar=c()

  #boundaries
  optPar[1] = 0.771
  optPar[2] = 1.209
  #starting point (as proportion of boundary)
  optPar[3] = .522
  optPar[4] = .538
  #starting point variability
  optPar[5] = .05
  #drift rate
  optPar[6] = -2.315
  optPar[7] = 1.047
  #drift rate variability
  optPar[8] = 1.503
  #minimum non-decision time
  optPar[9] = .372
  optPar[10] = .441
  #range in non-decision time
  optPar[11] = .189
  optPar[12] = .162
  
  return(optPar)
}



#This returns the g-squared value
#  across the full design (all conditions).
#qTopFull and qBotFull are matrices of
#  quantiles from all conditions with
#  conditions in rows.
#fTopFull and fBotFull are matrices of
#  RT bin counts from all conditions with
#  conditions in rows.
#medf and quantf are cutoffs for collapsing
#  RT bins for low counts.
#  (see gsqfun notes)
#intUpLimFull is vector of the upper limit
#  for integration of the prediction
#  equations across all conditions
getFullGsq=function(optPar,qTopFull,qBotFull,
                       fTopFull,fBotFull,
                       medf=5,quantf=10,
                       intUpLimFull=rep(10,Ncond)){
  
 fullPar = makeFullPar(optPar)
 
 fullGsq=0

 #loop through conditions and add up
 #  g-squared
 for(cond in 1:nrow(fullPar)){
   qtop=qTopFull[cond,]
   qbot=qBotFull[cond,]
   ftop=fTopFull[cond,]
   fbot=fBotFull[cond,]
   par=fullPar[cond,]
   intUpLim=intUpLimFull[cond]
   
   fullGsq=fullGsq + gsqfun(par,qtop,qbot,
                            ftop,fbot,
                            medf,quantf,
                            intUpLim)
 }
 
 return(fullGsq)
}


#quantile and response proportion
#  predictions across all conditions
#  based on the big vector of parameters
#  for the optim routine. Predictions
#  are derived by simulating a large
#  number of trials from diffSample
getFullPred=function(optPar){
  
  #matrix of parameters from
  #  each condition
  fullPar=makeFullPar(optPar)
  
  #objects to hold quantile and
  #  response proportion predictions
  qTopPred = matrix(NA,nrow(fullPar),5)
  qBotPred = matrix(NA,nrow(fullPar),5)
  pTopPred = c()

  #loop through conditions and fill
  #  in predictions for each
  for(cond in 1:nrow(fullPar)){
    par=fullPar[cond,]
    
    outList = diffSample(50000,par)
    
    qTopPred[cond,]=outList[[1]]
    qBotPred[cond,]=outList[[2]]
    pTopPred[cond]=outList[[6]]
  }
  
  #prepare list of output objects
  fullPred=list(qTopPred,qBotPred,pTopPred)
  return(fullPred)
}



#Perform a fit (parameter optimization)
#  for the overall data.
#startPar is starting values for all
#  parameters in the optimization
#  routine. (Goes to default if not
#  specified)
#medf and quantf are cutoffs for collapsing
#  RT bins for low counts.
#  (see gsqfun notes)
runOvFit=function(startPar=resetOptPar(),
                  medf=5,quantf=10){
  
  #start time to keep track of
  #  how long the fit takes
  sttime=as.numeric(Sys.time())
  
  #quantiles and frequencies for
  # full data (all conditions), with
  # conditions in rows
  qTopFull=qOldOv
  qBotFull=qNewOv
  fTopFull=fOldOv
  fBotFull=fNewOv

  #upper time limit of integration
  #  for prediction functions. Need
  #  to make sure this is way out on the
  #  tail of the distribution to avoid
  #  distorting the predictions, but you
  #  don't want it to be too high because
  #  it slows down the prediction function.
  #  Here I take the .9 quantile
  #  and add 2X the distance between the .1 and
  #  .9. That should make sure it is far out
  #  on the tail of each distribution, but not
  #  much longer than it needs to be.
  topLim=qTopFull[,5] + 2*(qTopFull[,5]-qTopFull[,1])
  botLim=qBotFull[,5] + 2*(qBotFull[,5]-qBotFull[,1])
  bothLim=cbind(topLim,botLim)
  intUpLimFull=apply(bothLim,1,max)
  intUpLimFull[intUpLimFull>10] = 10

  #starting parameter values
  optPar=startPar
  
  #starting g-squared value
  gsq=getFullGsq(optPar,qTopFull,qBotFull,
                  fTopFull,fBotFull,medf,quantf,
                 intUpLimFull)
  minImp=0.05 #minimal improvement in g-squared
  #  from last round of optim required
  #  to do another round
  out=0 #just a variable to control when the loop stops
  while(out==0){
    fit=optim(optPar,
              fn=getFullGsq,
              qTopFull=qTopFull,
              qBotFull=qBotFull,
              fTopFull=fTopFull,
              fBotFull=fBotFull,
              medf=medf,quantf=quantf,
              intUpLimFull=intUpLimFull)
    optPar=fit$par
    if((gsq-fit$value)<=minImp) out=1
    gsq=fit$value
    print(gsq)
  }

  #see how long it took to fit
  endtime=as.numeric(Sys.time())
  elapsed=(endtime-sttime)/60
  
  #compile list of output
  #  objects
  outList = list(optPar,gsq,elapsed)
  return(outList)
}

#Perform a fit (parameter optimization)
#  for one individual subject.
#subInd is index for the subject that
#  you want to fit
#startPar is parameter set to begin
#  optimization
#  defaults to the best-fitting
#  parameters for the overall data, found
#  with runOvFit()
#  This speeds up the parameter search
#medf and quantf are cutoffs for collapsing
#  RT bins for low counts.
#  (see gsqfun notes)
runIndFit=function(subInd,
                   startPar=resetOptParOv(),
                   medf=5,quantf=10){
  
  #start time to keep track of
  #  how long the fit takes
  sttime=as.numeric(Sys.time())
  
  #quantiles and frequencies for
  # full data (all conditions), with
  # conditions in rows
  qTopFull=qOld[,,subInd]
  qBotFull=qNew[,,subInd]
  fTopFull=fOld[,,subInd]
  fBotFull=fNew[,,subInd]

  #upper time limit of integration
  #  for prediction functions. Need
  #  to make sure this is way out on the
  #  tail of the distribution to avoid
  #  distorting the predictions, but you
  #  don't want it to be too high because
  #  it slows down the prediction function.
  #  Here I take the .9 quantile
  #  and add 2X the distance between the .1 and
  #  .9. That should make sure it is far out
  #  on the tail of each distribution, but not
  #  much longer than it needs to be.
  topLim=qTopFull[,5] + 2*(qTopFull[,5]-qTopFull[,1])
  botLim=qBotFull[,5] + 2*(qBotFull[,5]-qBotFull[,1])
  bothLim=cbind(topLim,botLim)
  intUpLimFull=apply(bothLim,1,max)
  intUpLimFull[intUpLimFull>10] = 10
  

  #Parameter set to begin optimization
  optPar=startPar
  
  #starting g-squared value
  gsq=getFullGsq(optPar,qTopFull,qBotFull,
                 fTopFull,fBotFull,medf,quantf,
                 intUpLimFull)
  minImp=0.05 #minimal improvement in g-squared
  #  from last round of optim required
  #  to do another round
  out=0 #just a variable to control when the loop stops
  while(out==0){
    fit=optim(optPar,
              fn=getFullGsq,
              qTopFull=qTopFull,
              qBotFull=qBotFull,
              fTopFull=fTopFull,
              fBotFull=fBotFull,
              medf=medf,quantf=quantf,
              intUpLimFull=intUpLimFull)
    optPar=fit$par
    if((gsq-fit$value)<=minImp) out=1
    gsq=fit$value
    print(gsq)
  }

  #see how long it took to fit
  endtime=as.numeric(Sys.time())
  elapsed=(endtime-sttime)/60
  
  #compile list of output
  #  objects
  outList = list(optPar,gsq,elapsed)
  return(outList)
}

#Loop through all participants and
#  perform a fit for each.
#spar is a matrix of starting parameter
#  values for optimization with subjects
#  in rows. If not specified, then all
#  fits use default starting parameters.
fitLoop=function(spar=NA){
  
  #matrix to hold best-fitting parameter
  #  values for each subject (row)
  fpar=matrix(NA,
              length(subjects),
              length(resetOptPar()))
  #vector to hold g-squared for each subject
  fgsq=c() 
  #vector to hold fit duration
  ftime=c()
  
  #loop across subjects
  for(subj in 1:length(subjects)){
    #progress update
    cat("\n\nFitting subject ",subj,"\n")
    if(is.na(spar[1]==T)){
      outList=runIndFit(subj)
    } else{
      outList=runIndFit(subj,
                        startPar=spar[subj,])
    }
    
    fpar[subj,]=outList[[1]]
    fgsq[subj]=outList[[2]]
    ftime[subj]=outList[[3]]
  }
  
  #compile list of output
  #  objects
  outList=list(fpar,fgsq,ftime)
  return(outList)
}


#Plot showing the quantile and response
#  propotion fit.
#partNum is participant number (1-39)
#  partNum = 0 uses overall data
#optPar is big parameter vector with
#  parameters from all conditions
#medf is minimum count to show RT
#   median, and quantf is minimum
#   count to show full RT quantiles
fitPlot=function(partNum,optPar,medf=5,quantf=10){
  
  #set up the plotting window
  par(mfcol=c(1,3),mar=c(5,6,1,1),las=1)

  #get full predictions
  fullPred=getFullPred(optPar)
  qTopPred=fullPred[[1]]
  qBotPred=fullPred[[2]]
  pTopPred=fullPred[[3]]
  
  #pull out data values to plot
  if(partNum==0){ #overall data
    #total frequency of old and new responses
    #  in each condition
    tfOld=apply(fOldOv,1,sum)
    tfNew=apply(fNewOv,1,sum)
    #proportion of old responses in each
    #  condition
    pOld=tfOld/(tfOld+tfNew)
    #"old" RT quantiles
    vqOld = as.vector(qOldOv)
    #"new" RT quantiles
    vqNew = as.vector(qNewOv)
  } else{ #individual data
    #total frequency of old and new responses
    #  in each condition
    tfOld=apply(fOld[,,partNum],1,sum)
    tfNew=apply(fNew[,,partNum],1,sum)
    #proportion of old responses in each
    #  condition
    pOld=tfOld/(tfOld+tfNew)
    #"old" RT quantiles
    vqOld = as.vector(qOld[,,partNum])
    #"new" RT quantiles
    vqNew = as.vector(qNew[,,partNum])
  }
  #response proportion plot
  plot(1:4,pOld,ylim=c(0,1),
       xlab="",xaxt="n",
       ylab="",yaxs='i',
       xlim=c(.75,4.25),
       cex.axis=1.5,cex.lab=1.5,
       cex=2)
  #add predictions
  points(1:4,pTopPred,pch=4,cex=2)
  mtext("p('Old')",2,3.75,cex=1.15,las=0)
  axis(1,at=1:4,
       labels=c("Sp.\nL",
                "Sp.\nT",
                "Ac.\nL",
                "Ac.\nT"),
       cex.axis=1.5,padj = 1)
  
  #way to not show quantiles for
  #  conditions with low frequencies
  #plotting symbol matrix based
  #  on number of observations.
  #  NA means that nothing appears on plot
  sym=matrix(1,nrow(qTopPred),ncol(qTopPred))
  sym[which(tfOld<quantf),c(1,2,4,5)]=NA
  sym[which(tfOld<medf),]=NA
  
  ymin=min(vqOld,vqNew)*.8
  ymax=max(vqOld,vqNew)*1.1
  #'old' quantile plot
  plot(rep(1:4,5),vqOld,
       xlab="",xaxt="n",
       ylab="",yaxs='i',
       xlim=c(.75,4.25),
       ylim=c(ymin,ymax),
       pch=unlist(sym),
       cex.axis=1.5,cex.lab=1.5,
       cex=1.5)
  sym[sym==1] = 4
  #add predictions
  points(rep(1:4,5),qTopPred,
         cex=1.5,pch=unlist(sym))
  mtext("'Old' RT Quantiles (s)",2,3.75,cex=1.15,las=0)
  axis(1,at=1:4,
       labels=c("Sp.\nL",
                "Sp.\nT",
                "Ac.\nL",
                "Ac.\nT"),
       cex.axis=1.5,padj = 1)

  sym=matrix(1,nrow(qBotPred),ncol(qBotPred))
  sym[which(tfNew<quantf),c(1,2,4,5)]=NA
  sym[which(tfNew<medf),]=NA
  
  #new quantile plot
  plot(rep(1:4,5),vqNew,
       xlab="",xaxt="n",
       ylab="",yaxs='i',
       xlim=c(.75,4.25),
       ylim=c(ymin,ymax),
       pch=unlist(sym),
       cex.axis=1.5,cex.lab=1.5,
       cex=1.5)
  sym[sym==1] = 4
  #add predictions
  points(rep(1:4,5),qBotPred,
         cex=1.5,pch=unlist(sym))
  mtext("'New' RT Quantiles (s)",2,3.75,cex=1.15,las=0)
  axis(1,at=1:4,
       labels=c("Sp.\nL",
                "Sp.\nT",
                "Ac.\nL",
                "Ac.\nT"),
       cex.axis=1.5,padj = 1)
}

