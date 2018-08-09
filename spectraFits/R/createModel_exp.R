library("rjags")
library("runjags")
library("PhenologyBayesModeling")


createModel.Exp <- function(data,index){
  nchain <- 5
  data$n <- length(data$y)
  print(data$n)
  #data$mean.k <- 290
  #data$p.k <- 10
  # data$mean.a <- -0.4
  # data$p.a <- 1/(0.1**2)
  # data$mean.b <- 0.02
  # data$p.b <- 1/(0.005**2)
  inits <- list()

  if(index=="PSRI" || index=="RGI"){
    data$min.a <- 0
    data$max.a <- 100
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,0.34,0.005),b=rnorm(1,0.02,0.03),c=rnorm(1,mean(data$y[1:10]),0.05))
    }
  }
  else{
    data$min.a <- -100
    data$max.a <- 0
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,-0.34,0.005),b=rnorm(1,0.02,0.005),c=rnorm(1,mean(data$y[1:5]),0.05))
    }
  }
  data$min.b <- -100
  data$max.b <- 100
  data$s1 <- 0.7#0.001
  data$s2 <- 0.3#0.00001
  topHalf <- c("Chl","NDVI_H","NDRE","mND705","GNDVI","NDVI_M","LIC","Car")
  if(index %in% topHalf){
    data$mean.c <- 0.75
    data$p.c <- 1/(0.1**2)
  }
  else if(index == "PRI"){
    data$mean.c <- 0.3
    data$p.c <- 1/(0.1**2)
  }
  else if(index == "GM2" || index == "RVI1"){
    data$mean.c <- 9
    data$p.c <- 1/(3**2)
  }
  else if(index == "VGM"){
    data$mean.c <- 2
    data$p.c <- 1/(0.3**2)
  }
  else if(index=="PSRI"){
    data$mean.c <- 0
    data$p.c <- 1/(0.02**2)
  }
  else if(index=="RGI"){
    data$mean.c <- 2
    data$p.c <- 1/(1**2)
  }
  else if(index=="mSR"){
    data$mean.c <- 4
    data$p.c <- 1/(1**2)
  }
  else if(index=="DD"){
    data$mean.c <- 0.1
    data$p.c <- 1/(0.02**2)
  }
  else if(index=="PSRI"){
    data$mean.c <- 0
    data$p.c <- 1/(0.02**2)
  }
  else if(index=="GM1" || index=="RVI2"){
    data$mean.c <- 4
    data$p.c <- 1/(0.5**2)
  }
  else if(index=="RE"){
    data$mean.c <- 0.01
    data$p.c <- 1/(0.005**2)
  }
  else if(index=="CTR"){
    data$mean.c <- 3.5
    data$p.c <- 1/(0.05**2)
  }
  else if(index=="SIPI"){
    data$mean.c <- 2
    data$p.c <- 1/(0.5**2)
  }
  else{
    print("Unknown Index")
    return()
  }

  inits <- list()



  Exp.model <- "
  model{
  ##priors
  #a ~ dnorm(mean.a,p.a)
  #b ~ dnorm(mean.b,p.b)
  a ~ dunif(min.a,max.a)
  b ~ dunif(min.b,max.b)

  c ~ dnorm(mean.c,p.c)
  prec ~ dgamma(s1,s2)


  for(i in 1:n){
  mu[i] <- a * exp(b*(x[i]-x[1])) + c ##exp process model
  y[i] ~ dnorm(mu[i],prec)  ##data model
  }
  }
  "
  j.model   <- jags.model(file = textConnection(Exp.model),
                          data = data,
                          inits=inits,
                          n.chains = nchain)
  return(j.model)
}








