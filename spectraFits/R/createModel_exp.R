library("rjags")
library("runjags")
library("PhenologyBayesModeling")


createModel.Exp <- function(data,index,inits){
  nchain <- 5
  data$n <- length(data$y)
  print(data$n)
  #data$mean.k <- 290
  #data$p.k <- 10
  # data$mean.a <- -0.4
  # data$p.a <- 1/(0.1**2)
  # data$mean.b <- 0.02
  # data$p.b <- 1/(0.005**2)
  #inits <- list()
  print(index)
  if(index=="PSRI" || index=="RGI" || index=="CTR" || index=="SIPI"){
    data$min.a <- -100 #0
    data$max.a <- 100
  }
  else{
    data$min.a <- -100
    data$max.a <- 100 #
    # for(i in 1:nchain){
    #   inits[[i]] <- list(a=rnorm(1,-0.34,0.005),b=rnorm(1,0.02,0.005),c=rnorm(1,mean(data$y[1:5]),0.05))
    # }
  }

  data$min.b <- -100
  data$max.b <- 100
  data$s1 <- 0.7#0.001
  data$s2 <- 0.3#0.00001
  topHalf <- c("chl","NDVI_H","NDRE","mND","GNDVI","NDVI_M","LIC","car","mND")
  if(index %in% topHalf){
    data$mean.c <- 0.75
    data$p.c <- 1/(0.1**2)
    # if(index == "chl"){
    #   # for(i in 1:nchain){
    #   #   inits[[i]] <- list(a=rnorm(1,-0.01,0.001),b=rnorm(1,0.035,0.005),c=rnorm(1,mean(data$y[1:5]),0.05))
    #   # }
    # }
    # else if(index == "car"){
    #   # for(i in 1:nchain){
    #   #   inits[[i]] <- list(a=rnorm(1,-0.0001,0.00001),b=rnorm(1,0.085,0.005),c=rnorm(1,mean(data$y[1:5]),0.05))
    #   # }
    # }
    # else if(index == "NDVI_H"){
    #   # for(i in 1:nchain){
    #   #   inits[[i]] <- list(a=rnorm(1,-0.00001,0.000001),b=rnorm(1,0.11,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
    #   # }
    # }
    # else if(index == "NDRE"){
    #   # for(i in 1:nchain){
    #   #   inits[[i]] <- list(a=rnorm(1,-0.0003,0.00001),b=rnorm(1,0.065,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
    #   # }
    # }
    # else if(index == "GNDVI"){
    #   for(i in 1:nchain){
    #     inits[[i]] <- list(a=rnorm(1,-0.0003,0.00001),b=rnorm(1,0.07,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
    #   }
    # }
    # else if(index == "LIC"){
    #   for(i in 1:nchain){
    #     inits[[i]] <- list(a=rnorm(1,-0.00008,0.00001),b=rnorm(1,0.076,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
    #   }
    # }
    # else if(index == "mND"){
    #   for(i in 1:nchain){
    #     inits[[i]] <- list(a=rnorm(1,-0.0003,0.00001),b=rnorm(1,0.08,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
    #   }
    # }
    # else if(index == "NDVI_M"){
    #   for(i in 1:nchain){
    #     inits[[i]] <- list(a=rnorm(1,-0.00005,0.000001),b=rnorm(1,0.07,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
    #   }
    # }
  }
  else if(index == "PRI"){
    data$mean.c <- 0.3
    data$p.c <- 1/(0.1**2)
    # for(i in 1:nchain){
    #   inits[[i]] <- list(a=rnorm(1,-0.0001,0.00001),b=rnorm(1,0.075,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
    # }
  }
  else if(index == "GM2" || index == "RVI1"){
    data$mean.c <- 9
    data$p.c <- 1/(3**2)
    if(index=="GM2"){
      # for(i in 1:nchain){
      #   inits[[i]] <- list(a=rnorm(1,-0.05,0.001),b=rnorm(1,0.04,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
      # }
    }
    else if(index=="RVI1"){
      # for(i in 1:nchain){
      #   inits[[i]] <- list(a=rnorm(1,-0.4,0.03),b=rnorm(1,0.035,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
      # }
    }
  }
  else if(index == "VGM"){
    data$mean.c <- 2
    data$p.c <- 1/(0.3**2)
    # for(i in 1:nchain){
    #   inits[[i]] <- list(a=rnorm(1,-0.0005,0.0001),b=rnorm(1,0.07,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
    # }
  }
  else if(index=="PSRI"){
    data$mean.c <- 0
    data$p.c <- 1/(0.02**2)
    # for(i in 1:nchain){
    #   inits[[i]] <- list(a=rnorm(1,0.00015,0.00001),b=rnorm(1,0.09,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
    # }
  }
  else if(index=="RGI"){
    data$mean.c <- 2
    data$p.c <- 1/(1**2)
    # for(i in 1:nchain){
    #   inits[[i]] <- list(a=rnorm(1,0.0000001,0.00000001),b=rnorm(1,0.165,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
    # }
  }
  else if(index=="mSR"){
    data$mean.c <- 4
    data$p.c <- 1/(1**2)
  }
  else if(index=="DD"){
    data$mean.c <- 0.1
    data$p.c <- 1/(0.02**2)
    # for(i in 1:nchain){
    #   inits[[i]] <- list(a=rnorm(1,-0.05,0.001),b=rnorm(1,0.065,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
    # }
  }
  else if(index=="PSRI"){
    data$mean.c <- 0
    data$p.c <- 1/(0.02**2)
  }
  else if(index=="GM1" || index=="RVI2"){
    data$mean.c <- 4
    data$p.c <- 1/(0.5**2)
    if(index=="GM1"){
      # for(i in 1:nchain){
      #   inits[[i]] <- list(a=rnorm(1,-0.004,0.00001),b=rnorm(1,0.06,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
      # }
    }
    else if(index=="GM1"){
      # for(i in 1:nchain){
      #   inits[[i]] <- list(a=rnorm(1,-0.005,0.00001),b=rnorm(1,0.06,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
      # }
    }
  }
  else if(index=="RE"){
    data$mean.c <- 0.01
    data$p.c <- 1/(0.005**2)
  }
  else if(index=="CTR"){
    data$mean.c <- 3.5
    data$p.c <- 1/(0.05**2)
    # for(i in 1:nchain){
    #   inits[[i]] <- list(a=rnorm(1,0.0001,0.00001),b=rnorm(1,0.105,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
    # }
  }
  else if(index=="SIPI"){
    data$mean.c <- 2
    data$p.c <- 1/(0.5**2)
    # for(i in 1:nchain){
    #   inits[[i]] <- list(a=rnorm(1,0.00000001,0.000000001),b=rnorm(1,0.21,0.03),c=rnorm(1,mean(data$y[1:5]),0.05))
    # }
  }
  else{
    print("Unknown Index")
    return()
  }

  print(data$max.a)
  print(data$min.a)
  print(inits)
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


createModel.Exp2 <- function(data,index){
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
  print(index)
  if(index=="PSRI" || index=="RGI" || index=="CTR" || index=="SIPI"){
    data$min.a <- -100 #0
    data$max.a <- 100
  }
  else{
    data$min.a <- -100
    data$max.a <- 100 #
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,-0.34,0.005),b=rnorm(1,0.02,0.005),c=rnorm(1,mean(data$y[1:5]),0.05))
    }
  }

  data$min.b <- -100
  data$max.b <- 100
  data$s1 <- 0.7#0.001
  data$s2 <- 0.3#0.00001
  topHalf <- c("chl","NDVI_H","NDRE","mND","GNDVI","NDVI_M","LIC","car","mND")
  if(index %in% topHalf){
    data$mean.c <- 0.75
    data$p.c <- 1/(0.1**2)
    if(index == "chl"){
      # for(i in 1:nchain){
      #   inits[[i]] <- list(a=rnorm(1,-0.01,0.001),b=rnorm(1,0.035,0.005),c=rnorm(1,mean(data$y[1:5]),0.05))
      # }
    }
    else if(index == "car"){
      # for(i in 1:nchain){
      #   inits[[i]] <- list(a=rnorm(1,-0.0001,0.00001),b=rnorm(1,0.085,0.005),c=rnorm(1,mean(data$y[1:5]),0.05))
      # }
    }
    else if(index == "NDVI_H"){
      # for(i in 1:nchain){
      #   inits[[i]] <- list(a=rnorm(1,-0.00001,0.000001),b=rnorm(1,0.11,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
      # }
    }
    else if(index == "NDRE"){
      # for(i in 1:nchain){
      #   inits[[i]] <- list(a=rnorm(1,-0.0003,0.00001),b=rnorm(1,0.065,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
      # }
    }
    else if(index == "GNDVI"){
      for(i in 1:nchain){
        inits[[i]] <- list(a=rnorm(1,-0.0003,0.00001),b=rnorm(1,0.07,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
      }
    }
    else if(index == "LIC"){
      for(i in 1:nchain){
        inits[[i]] <- list(a=rnorm(1,-0.00008,0.00001),b=rnorm(1,0.076,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
      }
    }
    else if(index == "mND"){
      for(i in 1:nchain){
        inits[[i]] <- list(a=rnorm(1,-0.0003,0.00001),b=rnorm(1,0.08,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
      }
    }
    else if(index == "NDVI_M"){
      for(i in 1:nchain){
        inits[[i]] <- list(a=rnorm(1,-0.00005,0.000001),b=rnorm(1,0.07,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
      }
    }
  }
  else if(index == "PRI"){
    data$mean.c <- 0.3
    data$p.c <- 1/(0.1**2)
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,-0.0001,0.00001),b=rnorm(1,0.075,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
    }
  }
  else if(index == "GM2" || index == "RVI1"){
    data$mean.c <- 9
    data$p.c <- 1/(3**2)
    if(index=="GM2"){
      for(i in 1:nchain){
        inits[[i]] <- list(a=rnorm(1,-0.05,0.001),b=rnorm(1,0.04,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
      }
    }
    else if(index=="RVI1"){
      for(i in 1:nchain){
        inits[[i]] <- list(a=rnorm(1,-0.4,0.03),b=rnorm(1,0.035,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
      }
    }
  }
  else if(index == "VGM"){
    data$mean.c <- 2
    data$p.c <- 1/(0.3**2)
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,-0.0005,0.0001),b=rnorm(1,0.07,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
    }
  }
  else if(index=="PSRI"){
    data$mean.c <- 0
    data$p.c <- 1/(0.02**2)
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,0.00015,0.00001),b=rnorm(1,0.09,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
    }
  }
  else if(index=="RGI"){
    data$mean.c <- 2
    data$p.c <- 1/(1**2)
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,0.0000001,0.00000001),b=rnorm(1,0.165,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
    }
  }
  else if(index=="mSR"){
    data$mean.c <- 4
    data$p.c <- 1/(1**2)
  }
  else if(index=="DD"){
    data$mean.c <- 0.1
    data$p.c <- 1/(0.02**2)
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,-0.05,0.001),b=rnorm(1,0.065,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
    }
  }
  else if(index=="PSRI"){
    data$mean.c <- 0
    data$p.c <- 1/(0.02**2)
  }
  else if(index=="GM1" || index=="RVI2"){
    data$mean.c <- 4
    data$p.c <- 1/(0.5**2)
    if(index=="GM1"){
      for(i in 1:nchain){
        inits[[i]] <- list(a=rnorm(1,-0.004,0.00001),b=rnorm(1,0.06,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
      }
    }
    else if(index=="GM1"){
      for(i in 1:nchain){
        inits[[i]] <- list(a=rnorm(1,-0.005,0.00001),b=rnorm(1,0.06,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
      }
    }
  }
  else if(index=="RE"){
    data$mean.c <- 0.01
    data$p.c <- 1/(0.005**2)
  }
  else if(index=="CTR"){
    data$mean.c <- 3.5
    data$p.c <- 1/(0.05**2)
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,0.0001,0.00001),b=rnorm(1,0.105,0.001),c=rnorm(1,mean(data$y[1:5]),0.05))
    }
  }
  else if(index=="SIPI"){
    data$mean.c <- 2
    data$p.c <- 1/(0.5**2)
    for(i in 1:nchain){
      inits[[i]] <- list(a=rnorm(1,0.00000001,0.000000001),b=rnorm(1,0.21,0.03),c=rnorm(1,mean(data$y[1:5]),0.05))
    }
  }
  else{
    print("Unknown Index")
    return()
  }

  print(data$max.a)
  print(data$min.a)
  print(inits)
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















