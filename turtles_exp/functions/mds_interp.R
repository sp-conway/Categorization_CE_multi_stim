library(geometry)

# calculate area of a triangle from coords
tri_ar <- function(x,y){
  a <- ( x[1] * ( y[2] - y[3] ) + x[2] * ( y[3] - y[1] ) + x[3] * ( y[1] - y[2] ) ) /2
  return(a)
}

# stim - angle and radius values of stimulus to be interpreted
# phys - N x 2 matrix of physical stimulus values from scaling study
# mds - N x 2 matrix of mds values from scaling study (should be in same order as phys)
interp <- function(stim,phys,mds){
  stim <- matrix(stim,ncol = 2)
  
  # first check to make sure it doesn't have a match in MDS space
  for(i in 1:nrow(phys)){
    if( ( round(stim[,1])==round(phys[i,1]) )  & ( round(stim[,2])==round(phys[i,2]) ) ){
      psy_unknown <- mds[i,]
      return(psy_unknown)
    }
  }
  
  # dists
  d <- as.matrix(dist(rbind(phys,stim)))
  
  # ranks
  r <- rank(d[nrow(phys)+1,])
  
  # init variables for while loop
  # x and y are x and y coordinates of our triangle
  # A is the area
  # Counter is used to keep trying new physical sitm
  x <- numeric(3)
  y <- numeric(3)
  counter <- 0
  A <- 0
  while(A==0){
    counter <- counter+1
    ii <- which( sort(r) > counter )[1:3]
    i <- as.numeric(names(ii))
    tri_phys <- phys[i,]
    x <- tri_phys[,1]
    y <- tri_phys[,2]
    A <- tri_ar(x,y)
  }
  tri_phys <- phys[i,]
  tri_psy <- mds[i,]
  b <- cart2bary(tri_phys, stim)
  psy_unknown <- bary2cart(tri_psy,b)
  return(psy_unknown)
}
