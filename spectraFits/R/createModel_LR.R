library("rjags")
library("runjags")
library("PhenologyBayesModeling")

createModel.LR <- function(data){
  nchain <- 5
  data$n <- length(data$y)
  data$min.b0 <- -100
  data$max.b0 <- 100
  data$min.b1 <- -100
  data$max.b1 <- 100
  data$s1 <- 0.001
  data$s2 <- 0.00001
  inits <- list()
  for(i in 1:nchain){
    inits[[i]] <- list(beta0=rnorm(1,8,1),beta1=rnorm(1,-0.02,0.01))
  }

  LR.model <- "
  model{
  ##priors
  beta0 ~ dunif(min.b0,max.b0)
  beta1 ~ dunif(min.b1,max.b1)
  prec ~ dgamma(s1,s2)

  for(i in 1:n){
  mu[i] <- beta0 + beta1 * x[i] ## Linear Regression Process model
  y[i] ~ dnorm(mu[i],prec) ##data model

  }
  }

  "
  j.model   <- jags.model(file = textConnection(LR.model),
                          data = data,
                          inits=inits,
                          n.chains = nchain)
  return(j.model)
}



