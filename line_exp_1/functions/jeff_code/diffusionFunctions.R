# Simulate N trials from the diffusion
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
# Sean Edits 11/19/2021 
# Returns 
# [[1]] quantiles for the "top" boundary response
# [[2]] quantiles for the "bottom" boundary response
# [[3]] vector of freqs for top rsp followed by freqs for bot rsp
# [[4]] vector of freqs for top rsp
# [[5]] vector of freqs for bot rsp
# [[6]] scalar of probability of making top rsp
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
#  integrations in the prediction equation
#  in seconds. 
diffPred=function(par,qtop,qbot,
                  intUpLim=10){
  
  #assign parameter values
  a=par[1]
  z=par[2]*par[1]
  sz=.00
  v=par[4]
  sv=1.5
  t0=par[6]
  st0=par[7]
  
  #define vector to hold predictions
  p=c()
  
  #First we will do the top boundary response
  #  by specifying response="upper" in the call.
  #  "cuts" are the RT bin cutoffs for the top
  #  response, usually based on quantiles from
  #  the observed RT distributions.
  cuts=c(qtop,Inf)
  #The pdiffusion function gives cummulative
  #  probabilities (cp)
  cp=pdiffusion(cuts,response="upper",a=a,v=v,
                t0=t0,z=z,d=0,sz=sz,sv=sv,st0=st0,
                precision=3,maxt=intUpLim)
  #Enter the bin probabilities into the prediction
  #  vector p. Bin probabilities are the cummulative
  #  probability at each cutoff (cuts) minus the
  #  cummulative probability at the previous cutoff.
  p=c(p,cp[1])
  if(length(cp)>1){
    for(i in 2:length(cp)){
      p=c(p,cp[i]-cp[i-1])
    }
  }
  
  #This is the same as above, but for bottom
  #  boundary responses, indicated by
  #  response="lower" in the call
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
  
  #output the prediction vector
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
  
  # pi=3
  # pmn=.0
  # pmx=.05
  # if(par[pi]<pmn){
  #   penf=penf+(pmn-par[pi]) 
  #   par[pi]=pmn
  # }
  # if(par[pi]>pmx){
  #   penf=penf+(par[pi]-pmx) 
  #   par[pi]=pmx
  # }
  
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
  
  # pi=6
  # pmn=0
  # if(par[pi]<pmn){
  #   penf=penf+(pmn-par[pi]) 
  #   par[pi]=pmn
  # }
  
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


#We can check to make sure the simulation
#  function and the prediction equation
#  function are returning consistent results

# #Parameter vector for example
# par=c(1.2,.5,.02,1,1,.4,.2)
# 
# #Generate a large number of sampled
# #  trials with these parameters
# outList=diffSample(100000,par)
# 
# #Create R objects to hold quantiles and
# #  frequencies in each RT bin for
# #  simulated data for top-boundary
# #  and bottom-boundary responses
# qtop=outList[[1]]
# qbot=outList[[2]]
# ftop=outList[[4]]
# fbot=outList[[5]]
# 
# #Get observed proportions in each
# #  RT bin for top and bottom boundary
# #  responses
# ptop=ftop/(sum(c(ftop,fbot)))
# pbot=fbot/(sum(c(ftop,fbot)))
# 
# #Concatenate the bin proportions into
# #  one big vector of observed proportions
# #  across both responses
# obs=c(ptop,pbot)
# 
# #Get predicted bin proportions with
# #  the prediction equations using the
# #  same parameter values and the observed
# #  quantiles as the cutoffs defining the
# #  RT bins
# p=diffPred(par,qtop,qbot)
# 
# #Compare predicted against observed bin
# #  proportions
# compare = cbind(obs,p)


#We can also check to make sure the
#  function to calculate g-squared
#  (gsqfun) is working
#par is a vector of parameter values
#  in the same order as above (make
#  sure not to use values outside
#  of the allowable ranges in the
#  cons function)
#Nsim is number of simulated data sets.
#N is number of trials for each
#  simulated data set.
#upfreq - frequency of progress 
#         updates when it is running
checkGsq=function(par=c(1,.5,.02,1,1,.4,.2),
                  Nsim=1000,
                  N=500,
                  upfreq=50){

  #to hold g-squared values for each
  #  simulated data set
  gsqVals=c()
  
  #create a bunch of simulated data
  #  sets, and for each get the g-squared
  #  value
  for(simi in 1:Nsim){
    outList=diffSample(N,par)
    qtop=outList[[1]]
    qbot=outList[[2]]
    ftop=outList[[4]]
    fbot=outList[[5]]
    
    gsqVals[simi]=gsqfun(par,
                         qtop,qbot,
                         ftop,fbot)
    
    if(simi%%upfreq==0) print(simi)
  }
  
  #set up plot window
  par(mfcol=c(1,1),mar=c(5,6,2,2),las=1)
  
  #plot a histogram of g-squared
  #  values across simulated data
  #  sets
  hist(gsqVals,31,freq=F,
       main="",
       xlab="G-squared",
       ylab="",
       cex.axis=1.25,cex.lab=1.5)
  mtext("Probability Density",2,4,
        cex=1.5,las=0)
  
  #If the functions above work, then the
  #  g-squared values should follow a
  #  chi-squared distribution with as many
  #  degrees of freedom (df) as the data. In
  #  this case, that is 11 df because we have
  #  12 RT bins (6 for each response) and one
  #  is not free to vary because they have to
  #  add up to the number of trials.
  #(Note: This might not work if you make N
  #  too small, because the g-square function
  #  collapsed RT bins when the trial counts
  #  are too low to estimate quantiles.)
  xVals=seq(min(gsqVals),max(gsqVals),
            length.out=500)
  #Add target chi-square distribution to
  #  histogram
  points(xVals,dchisq(xVals,11),type='l',
         lwd=2)
}

