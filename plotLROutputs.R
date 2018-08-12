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

pdf("LR_Fits.pdf",width=45,height=45)
par(mfrow=c(5,5))
fitFiles <- dir(pattern="LR_varBurn.RData")
for(i in 1:length(fitFiles)){
  print(fitFiles[i])
  load(fitFiles[i])
  out.mat <- as.matrix(var.Burn)
  beta0 <- out.mat[,1]
  beta1 <- out.mat[,2]
  prec <- out.mat[,3]
  tree <- substr(fitFiles[i],1,3)
  yr <- substr(fitFiles[i],5,8)
  ind <- strsplit(fitFiles[i],split="_")[[1]][3]
  if(ind=="NDVI"){
    ind <- paste(strsplit(fitFiles[i],split="_")[[1]][3],strsplit(fitFiles[i],split="_")[[1]][4],sep="_")
  }
  print(ind)
  load(paste(tree,"_",yr,"_Data.RData",sep=""))
  xseq <- seq(min(data$DOY),max(data$DOY),1)
  
  ycred <- matrix(0,nrow=10000,ncol=length(xseq))
  ypred <- matrix(0,nrow=10000,ncol=length(xseq))
  for(g in 1:10000){
    Ey <- phenoLR(beta0=beta0[g],beta1=beta1[g],xseq=xseq)
    ycred[g,] <- Ey
    ypred[g,] <- rnorm(length(xseq),Ey,sqrt(1/prec[g]))
  }
  ci <- apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
  pi <- apply(ypred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
  load(paste(tree,"_",yr,"_Data.RData",sep=""))
  
  dat <- list()
  dat$x <- data$DOY
  if(ind=="chl"){
    dat$y <- data$chl
  }
  else if(ind=="car"){
    dat$y <- data$car
  }
  else if(ind=="NDVI_H"){
    dat$y <- data$NDVI_H
  }
  else if(ind=="PRI"){
    dat$y <- data$PRI
  }
  else if(ind=="NDRE"){
    dat$y <- data$NDRE
  }
  else if(ind=="GNDVI"){
    dat$y <- data$GNDVI
  }
  else if(ind=="GM1"){
    dat$y <- data$GM1
  }
  else if(ind=="RVI1"){
    dat$y <- data$RVI1
  }
  else if(ind=="RVI2"){
    dat$y <- data$RVI2
  }
  else if(ind=="LIC"){
    dat$y <- data$LIC
  }
  else if(ind=="CTR"){
    dat$y <- data$CTR
  }
  else if(ind=="GM2"){
    dat$y <- data$GM2
  }
  else if(ind=="mSR"){
    dat$y <- data$mSR
  }
  else if(ind=="VGM"){
    dat$y <- data$VGM
  }
  else if(ind=="PSRI"){
    dat$y <- data$PSRI
  }
  else if(ind=="mND"){
    dat$y <- data$mND
  }
  else if(ind=="DD"){
    dat$y <- data$DD
  }
  else if(ind=="RGI"){
    dat$y <- data$RGI
  }
  else if(ind=="RE"){
    dat$y <- data$RE
  }
  else if(ind=="NDVI_M"){
    dat$y <- data$NDVI_M
  }
  else if(ind=="SIPI"){
    dat$y <- data$SIPI
  }
  else{
    print("ERROR")
  }
  plot(dat$x,dat$y,main=paste(tree,ind,sep=" "),pch=20)
  ciEnvelope(xseq,pi[1,],pi[3,],col="blue")
  ciEnvelope(xseq,ci[1,],ci[3,],col="lightBlue")
  points(dat$x,dat$y,pch=20)
  lines(xseq,ci[2,],col="red")
  
}
dev.off()