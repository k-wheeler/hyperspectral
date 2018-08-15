library("rjags")
library("runjags")
library("PhenologyBayesModeling")


createModel.CP <- function(data,index){
  nchain <- 5
  data$n <- length(data$y)
  print(data$n)
  data$mean.k <- 274
  data$p.k <- 1/(20**2)

  inits <- list()

  if(index=="PSRI" || index=="RGI" || index == "CTR" || index == "SIPI"){
    data$min.a <- -100
    data$max.a <- 0
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,-0.02,0.003),b=rnorm(1,0.12,0.03),muL=rnorm(1,mean(data$y[1:10]),0.05),k=rnorm(1,285,10))
    }
    if(index=="CTR"){
      for(i in 1:nchain){
        inits[[i]] <- list(a=rnorm(1,-1,0.1),b=rnorm(1,0.07,0.02),muL=rnorm(1,mean(data$y[1:10]),0.05),k=rnorm(1,300,10))
      }
    }
  }
  else{
    data$min.a <- 0
    data$max.a <- 100
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,0.02,0.003),b=rnorm(1,0.12,0.03),muL=rnorm(1,mean(data$y[1:10]),0.05),k=rnorm(1,285,10))
    }
  }
  data$min.b <- -100
  data$max.b <- 100
  data$s1 <- 0.7#0.001
  data$s2 <- 0.3#0.00001
  topHalf <- c("chl","NDVI_H","NDRE","mND","GNDVI","NDVI_M","LIC","car")
  if(index %in% topHalf){
    data$mean.muL <- 0.75
    data$p.muL <- 1/(0.1**2)
    if(index=="chl" || index == "car"){
      for(i in 1:nchain){
        inits[[i]] <- list(a=rnorm(1,0.05,0.005),b=rnorm(1,0.07,0.01),muL=rnorm(1,mean(data$y[1:20]),0.05),k=rnorm(1,290,10))
      }
    }
    else if(index=="NDVI_H"){
      for(i in 1:nchain){
        inits[[i]] <- list(a=rnorm(1,0.08,0.005),b=rnorm(1,0.07,0.01),muL=rnorm(1,mean(data$y[1:20]),0.05),k=rnorm(1,290,10))
      }
    }
    else if(index=="NDRE"){
      for(i in 1:nchain){
        inits[[i]] <- list(a=rnorm(1,0.03,0.005),b=rnorm(1,0.07,0.01),muL=rnorm(1,mean(data$y[1:20]),0.05),k=rnorm(1,290,10))
      }
    }
    else if(index=="GNDVI"){
      for(i in 1:nchain){
        inits[[i]] <- list(a=rnorm(1,0.05,0.005),b=rnorm(1,0.07,0.01),muL=rnorm(1,mean(data$y[1:20]),0.05),k=rnorm(1,290,10))
      }
    }
    else if(index=="LIC"){
      for(i in 1:nchain){
        inits[[i]] <- list(a=rnorm(1,0.08,0.005),b=rnorm(1,0.07,0.01),muL=rnorm(1,mean(data$y[1:20]),0.05),k=rnorm(1,300,5))
      }
    }
    else if(index == "mND"){
      for(i in 1:nchain){
        inits[[i]] <- list(a=rnorm(1,0.03,0.005),b=rnorm(1,0.07,0.02),muL=rnorm(1,mean(data$y[1:10]),0.05),k=rnorm(1,280,10))
      }
    }
    else if(index == "NDVI_M"){
      for(i in 1:nchain){
        inits[[i]] <- list(a=rnorm(1,0.2,0.005),b=rnorm(1,0.08,0.01),muL=rnorm(1,mean(data$y[1:20]),0.05),k=rnorm(1,300,10))
      }
    }
  }
  else if(index == "PRI"){
    data$mean.muL <- 0.3
    data$p.muL <- 1/(0.1**2)
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,0.03,0.005),b=rnorm(1,0.07,0.01),muL=rnorm(1,mean(data$y[1:20]),0.05),k=rnorm(1,290,10))
    }
  }
  else if(index == "GM2"){
    data$mean.muL <- 9
    data$p.muL <- 1/(3**2)
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,0.1,0.1),b=rnorm(1,0.07,0.02),muL=rnorm(1,mean(data$y[1:10]),0.05),k=rnorm(1,270,10))
    }
  }
  else if(index == "RVI1"){
    data$mean.muL <- 9
    data$p.muL <- 1/(3**2)
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,0.5,0.005),b=rnorm(1,0.07,0.01),muL=rnorm(1,mean(data$y[1:20]),0.05),k=rnorm(1,275,5))
    }
  }
  else if(index == "VGM"){
    data$mean.muL <- 2
    data$p.muL <- 1/(0.3**2)
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,0.02,0.005),b=rnorm(1,0.07,0.02),muL=rnorm(1,mean(data$y[1:10]),0.05),k=rnorm(1,270,10))
    }
  }
  else if(index=="PSRI"){
    data$mean.muL <- 0
    data$p.muL <- 1/(0.02**2)
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,-0.15,0.005),b=rnorm(1,0.07,0.02),muL=rnorm(1,mean(data$y[1:10]),0.05),k=rnorm(1,300,10))
    }
  }
  else if(index=="RGI"){
    data$mean.muL <- 2
    data$p.muL <- 1/(1**2)
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,-0.3,0.05),b=rnorm(1,0.07,0.02),muL=rnorm(1,mean(data$y[1:10]),0.05),k=rnorm(1,300,10))
    }
  }
  else if(index=="mSR"){
    data$mean.muL <- 4
    data$p.muL <- 1/(1**2)
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,0.10,0.005),b=rnorm(1,0.08,0.01),muL=rnorm(1,mean(data$y[1:20]),0.05),k=rnorm(1,280,10))
    }
  }
  else if(index=="DD"){
    data$mean.muL <- 0.1
    data$p.muL <- 1/(0.02**2)
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,1,0.1),b=rnorm(1,0.07,0.02),muL=rnorm(1,mean(data$y[1:10]),0.05),k=rnorm(1,270,10))
    }
  }
  else if(index=="PSRI"){
    data$mean.muL <- 0
    data$p.muL <- 1/(0.02**2)
  }
  else if(index=="GM1"){
    data$mean.muL <- 4
    data$p.muL <- 1/(0.5**2)
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,0.03,0.005),b=rnorm(1,0.07,0.01),muL=rnorm(1,mean(data$y[1:20]),0.05),k=rnorm(1,290,10))
    }
  }
  else if(index=="RVI2"){
    data$mean.muL <- 4
    data$p.muL <- 1/(0.5**2)
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,0.15,0.005),b=rnorm(1,0.07,0.01),muL=rnorm(1,mean(data$y[1:20]),0.05),k=rnorm(1,275,5))
    }
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
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,-1,0.005),b=rnorm(1,0.07,0.01),muL=rnorm(1,mean(data$y[1:20]),0.05),k=rnorm(1,310,3))
    }
  }
  else{
    print("Unknown Index")
    print(index)
    return()
  }

  CP.model <- "
  model{
  ##priors
  a ~ dunif(min.a,max.a)
  k ~ dnorm(mean.k,p.k)
  b ~ dunif(min.b,max.b)
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



