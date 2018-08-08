library("rjags")
library("runjags")
library("PhenologyBayesModeling")


createModel.logistic <- function(data){
  nchain <- 5
  data$n <- length(data$y)
  #print(n)
  #data$alpha.d <- 1
  #data$beta.d <- 8
  data$alpha.c <- 8
  data$beta.c <- 2
  data$mean.Tran <- 300
  data$p.Tran <- 10
  data$mean.b <- 0.15
  data$p.b <- 0.05

  data$s1 <- 0.7#0.001
  data$s2 <- 0.3#0.00001
  inits <- list()
  #d=rnorm(1,0.2,0.05)
  for(i in 1:nchain){
    inits[[i]] <- list(Tran=rnorm(1,300,10),c=rnorm(1,0.8,0.05),b=rnorm(1,0.10,0.02))
  }

  Logistic.model <- "
  model{
  ##priors
  Tran ~ dnorm(mean.Tran,p.Tran)
  b ~ dnorm(mean.b,p.b)
  #d ~ dbeta(alpha.d,beta.d)
  c ~ dbeta(alpha.c,beta.c)
  prec ~ dgamma(s1,s2)

  for(i in 1:n){
  mu[i] <- c/(1+exp(b*(DOY[i]-Tran)))  ##logistic process model
  y[i] ~ dnorm(mu[i],prec)  ##data model

  }
  }

  "
  j.model   <- jags.model(file = textConnection(Logistic.model),
                          data = data,
                          inits=inits,
                          n.chains = nchain)
  return(j.model)
}



