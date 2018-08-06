library("rjags")
library("runjags")
library("PhenologyBayesModeling")


createModel.CP <- function(data,index){
  nchain <- 5
  data$n <- length(data$y)
  print(data$n)
  data$mean.k <- 290
  data$p.k <- 10
  data$mean.a <- 0.01
  data$p.a <- 1/(0.005)
  data$mean.b <- 0.12
  data$p.b <- 1/(0.05**2)
  data$s1 <- 0.7#0.001
  data$s2 <- 0.3#0.00001
  topHalf <- c("Chl","NDVI_H","NDRE","mND705","GNDVI","NDVI_M","LIC","Car")
  if(index %in% topHalf){
    data$mean.muL <- 0.75
    data$p.muL <- 1/(0.1**2)
  }
  else if(index == "PRI"){
    data$mean.muL <- 0.3
    data$p.muL <- 1/(0.1**2)
  }
  else if(index == "GM2" || index == "RVI1"){
    data$mean.muL <- 9
    data$p.muL <- 1/(3**2)
  }
  else if(index == "VGM"){
    data$mean.muL <- 2
    data$p.muL <- 1/(0.3**2)
  }
  else if(index=="PSRI"){
    data$mean.muL <- 0
    data$p.muL <- 1/(0.02**2)
  }
  else if(index=="RGI"){
    data$mean.muL <- 2
    data$p.muL <- 1/(1**2)
  }
  else if(index=="mSR"){
    data$mean.muL <- 4
    data$p.muL <- 1/(1**2)
  }
  else if(index=="DD"){
    data$mean.muL <- 0.1
    data$p.muL <- 1/(0.02**2)
  }
  else if(index=="PSRI"){
    data$mean.muL <- 0
    data$p.muL <- 1/(0.02**2)
  }
  else if(index=="GM1" || index=="RVI2"){
    data$mean.muL <- 4
    data$p.muL <- 1/(0.5**2)
  }
  else if(index=="RE"){
    data$mean.muL <- 0.01
    data$p.muL <- 1/(0.005**2)
  }
  else if(index=="CTR"){
    data$mean.muL <- 3.5
    data$p.muL <- 1/(0.05**2)
  }
  else if(index=="SIPI"){
    data$mean.muL <- 2
    data$p.muL <- 1/(0.5**2)
  }
  else{
    print("Unknown Index")
    return()
  }

  inits <- list()

  for(i in 1:nchain){
    inits[[i]] <- list(a=rnorm(1,0.01,0.005),b=rnorm(1,0.12,0.03),muL=rnorm(1,mean(data$y[1:10]),0.05),k=rnorm(1,285,10))
  }

  CP.model <- "
  model{
  ##priors
  a ~ dnorm(mean.a,p.a)
  k ~ dnorm(mean.k,p.k)
  b ~ dnorm(mean.b,p.b)
  prec ~ dgamma(s1,s2)
  muL ~ dnorm(mean.muL,p.muL)

  for(i in 1:n){
  muR[i] <- -a * exp(b*(x[i]-k)) + muL + a ##exp process model for decrease
  mu[i] <- ifelse(x[i]>k,muR[i],muL)   #change point process model

  y[i] ~ dnorm(mu[i],prec)  ##data model
  }
  }

  "
  j.model   <- jags.model(file = textConnection(CP.model),
                          data = data,
                          inits=inits,
                          n.chains = nchain)
  return(j.model)
}



