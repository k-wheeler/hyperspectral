library("spectraFits")
library("rjags")
library("runjags")
library("PhenologyBayesModeling")

##' Create the credible interval envelope for plotting
##' 
##' @param x time range
##' @param ylo the bottom credible interval values
##' @param yhi the top credible interval values
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

phenoLR <- function(beta0,beta1,xseq){
  return(beta0+beta1*xseq)
}
pdf("Measured_Fits.pdf",width=60,height=40)
par(mfrow=c(5,9))
indices <- c("chl","car","NDVI_H","PRI","NDRE","GNDVI","GM1","RVI1","RVI2","LIC","CTR","GM2","VGM","PSRI","mSR","mND","DD","RGI","RE","NDVI_M","SIPI")
for(i in 1:length(indices)){
  print(indices[i])
  load(paste("measuredRData/",indices[i],"_","allMeasured.RData",sep=""))
  load(paste("measuredRData/",indices[i],"_all_measured_varBurn.RData",sep=""))
  out.mat <- as.matrix(var.Burn)
  beta0 <- out.mat[,1]
  beta1 <- out.mat[,2]
  prec <- out.mat[,3]
  xseq <- seq(min(data$x),max(data$x),0.01)
  
  ycred <- matrix(0,nrow=10000,ncol=length(xseq))
  ypred <- matrix(0,nrow=10000,ncol=length(xseq))
  
  for(g in 1:10000){
    Ey <- phenoLR(beta0=beta0[g],beta1=beta1[g],xseq=xseq)
    ycred[g,] <- Ey
    ypred[g,] <- rnorm(length(xseq),Ey,sqrt(1/prec[g]))
  }
  ci <- apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
  pi <- apply(ypred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
  plot(data$x,data$y,main=paste("All measured",indices[i],sep=" "),ylab="N",xlab=indices[i],pch=20)
  ciEnvelope(xseq,pi[1,],pi[3,],col="blue")
  ciEnvelope(xseq,ci[1,],ci[3,],col="lightBlue")
  points(data$x,data$y,pch=20)
  lines(xseq,ci[2,],col="red")
  for(p in 1:length(data$x)){
    xs <- seq((data$x[p]-1.96*sqrt(1/data$obs.prec[p])),(data$x[p]+1.96*sqrt(1/data$obs.prec[p])),0.001)
    ys <- rep(data$y[p],length(xs))
    lines(xs,ys,col="black")
  }
  
  treeSpecies="PO"
  load(paste("measuredRData/",indices[i],"_",treeSpecies,"_allMeasured.RData",sep=""))
  load(paste("measuredRData/",indices[i],"_",treeSpecies,"_measured_varBurn.RData",sep=""))
  out.mat <- as.matrix(var.Burn)
  beta0 <- out.mat[,1]
  beta1 <- out.mat[,2]
  prec <- out.mat[,3]
  xseq <- seq(min(data$x),max(data$x),0.01)
  
  ycred <- matrix(0,nrow=10000,ncol=length(xseq))
  ypred <- matrix(0,nrow=10000,ncol=length(xseq))
  
  for(g in 1:10000){
    Ey <- phenoLR(beta0=beta0[g],beta1=beta1[g],xseq=xseq)
    ycred[g,] <- Ey
    ypred[g,] <- rnorm(length(xseq),Ey,sqrt(1/prec[g]))
  }
  ci <- apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
  pi <- apply(ypred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
  plot(data$x,data$y,main=paste("Poplar measured",indices[i],sep=" "),ylab="N",xlab=indices[i],pch=20)
  ciEnvelope(xseq,pi[1,],pi[3,],col="blue")
  ciEnvelope(xseq,ci[1,],ci[3,],col="lightBlue")
  points(data$x,data$y,pch=20)
  lines(xseq,ci[2,],col="red")
  for(p in 1:length(data$x)){
    xs <- seq((data$x[p]-1.96*sqrt(1/data$obs.prec[p])),(data$x[p]+1.96*sqrt(1/data$obs.prec[p])),0.001)
    ys <- rep(data$y[p],length(xs))
    lines(xs,ys,col="black")
  }
  
  treeSpecies="BE"
  load(paste("measuredRData/",indices[i],"_",treeSpecies,"_allMeasured.RData",sep=""))
  load(paste("measuredRData/",indices[i],"_",treeSpecies,"_measured_varBurn.RData",sep=""))
  out.mat <- as.matrix(var.Burn)
  beta0 <- out.mat[,1]
  beta1 <- out.mat[,2]
  prec <- out.mat[,3]
  xseq <- seq(min(data$x),max(data$x),0.01)
  
  ycred <- matrix(0,nrow=10000,ncol=length(xseq))
  ypred <- matrix(0,nrow=10000,ncol=length(xseq))
  
  for(g in 1:10000){
    Ey <- phenoLR(beta0=beta0[g],beta1=beta1[g],xseq=xseq)
    ycred[g,] <- Ey
    ypred[g,] <- rnorm(length(xseq),Ey,sqrt(1/prec[g]))
  }
  ci <- apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
  pi <- apply(ypred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
  plot(data$x,data$y,main=paste("Beech Measured",indices[i],sep=" "),ylab="N",xlab=indices[i],pch=20)
  ciEnvelope(xseq,pi[1,],pi[3,],col="blue")
  ciEnvelope(xseq,ci[1,],ci[3,],col="lightBlue")
  points(data$x,data$y,pch=20)
  lines(xseq,ci[2,],col="red")
  for(p in 1:length(data$x)){
    xs <- seq((data$x[p]-1.96*sqrt(1/data$obs.prec[p])),(data$x[p]+1.96*sqrt(1/data$obs.prec[p])),0.001)
    ys <- rep(data$y[p],length(xs))
    lines(xs,ys,col="black")
  }
  
  treeSpecies="BI"
  load(paste("measuredRData/",indices[i],"_",treeSpecies,"_allMeasured.RData",sep=""))
  load(paste("measuredRData/",indices[i],"_",treeSpecies,"_measured_varBurn.RData",sep=""))
  out.mat <- as.matrix(var.Burn)
  beta0 <- out.mat[,1]
  beta1 <- out.mat[,2]
  prec <- out.mat[,3]
  xseq <- seq(min(data$x),max(data$x),0.01)
  
  ycred <- matrix(0,nrow=10000,ncol=length(xseq))
  ypred <- matrix(0,nrow=10000,ncol=length(xseq))
  
  for(g in 1:10000){
    Ey <- phenoLR(beta0=beta0[g],beta1=beta1[g],xseq=xseq)
    ycred[g,] <- Ey
    ypred[g,] <- rnorm(length(xseq),Ey,sqrt(1/prec[g]))
  }
  ci <- apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
  pi <- apply(ypred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
  plot(data$x,data$y,main=paste("measured",indices[i],sep=" "),ylab="N",xlab=indices[i],pch=20)
  ciEnvelope(xseq,pi[1,],pi[3,],col="blue")
  ciEnvelope(xseq,ci[1,],ci[3,],col="lightBlue")
  points(data$x,data$y,pch=20)
  lines(xseq,ci[2,],col="red")
  for(p in 1:length(data$x)){
    xs <- seq((data$x[p]-1.96*sqrt(1/data$obs.prec[p])),(data$x[p]+1.96*sqrt(1/data$obs.prec[p])),0.001)
    ys <- rep(data$y[p],length(xs))
    lines(xs,ys,col="black")
  }
  
  for(j in seq(1,5)){
    load(paste("measuredRData/",indices[i],"_col",j,"_measured_varBurn.RData",sep=""))
    load(paste("measuredRData/",indices[i],"_col",j,"_measured_varBurn.RData",sep=""))
    out.mat <- as.matrix(var.Burn)
    beta0 <- out.mat[,1]
    beta1 <- out.mat[,2]
    prec <- out.mat[,3]
    xseq <- seq(min(data$x),max(data$x),0.01)
    
    ycred <- matrix(0,nrow=10000,ncol=length(xseq))
    ypred <- matrix(0,nrow=10000,ncol=length(xseq))
    
    for(g in 1:10000){
      Ey <- phenoLR(beta0=beta0[g],beta1=beta1[g],xseq=xseq)
      ycred[g,] <- Ey
      ypred[g,] <- rnorm(length(xseq),Ey,sqrt(1/prec[g]))
    }
    ci <- apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
    pi <- apply(ypred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
    plot(data$x,data$y,main=paste("Measured Collection",j,sep=" "),ylab="N",xlab=indices[i],pch=20)
    ciEnvelope(xseq,pi[1,],pi[3,],col="blue")
    ciEnvelope(xseq,ci[1,],ci[3,],col="lightBlue")
    points(data$x,data$y,pch=20)
    lines(xseq,ci[2,],col="red")
    for(p in 1:length(data$x)){
      xs <- seq((data$x[p]-1.96*sqrt(1/data$obs.prec[p])),(data$x[p]+1.96*sqrt(1/data$obs.prec[p])),0.001)
      ys <- rep(data$y[p],length(xs))
      lines(xs,ys,col="black")
    }
  }
 
  
}
dev.off()