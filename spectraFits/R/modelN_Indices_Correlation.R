#!/usr/bin/env Rscript

library("rjags")
library("runjags")

createCorModel <- function(data){
  ##y,xobs(means),obs.prec
  nchain <- 5
  data$n <- length(data$y)
  data$min.b0 <- 0
  data$max.b0 <- 100
  data$min.b1 <- -100
  data$max.b1 <- 100
  data$s1 <- 0.001
  data$s2 <- 0.00001
  inits <- list()

  LR.model <- "
  model{
  ##priors
  beta0 ~ dunif(min.b0,max.b0)
  beta1 ~ dunif(min.b1,max.b1)

  prec ~ dgamma(s1,s2)

  for(i in 1:n){
  mu[i] <- beta0 + beta1 * x[i] ## Linear Regression Process model
  y[i] ~ dnorm(mu[i],prec) ##data model

  xobs[i] ~ dnorm(x[i],obs.prec[i])

  }
  }

  "
  j.model   <- jags.model(file = textConnection(LR.model),
                          data = data,
                          n.chains = nchain)
  return(j.model)
}
