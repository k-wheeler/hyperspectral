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
indices <- c("NDRE","PRI","chl","NDVI","PSRI","car","mND","GNDVI")
trees <- c("BE1","BI1","PO1")
types <- c("LR","Exp")
pdf("Collected_FigureSpecies.pdf",width=20,height=20)
par(mfrow=c(8,6), mai = c(0.25, 0.4, 0.15, 0.2))

for(i in 1:length(indices)){
  print(indices[i])
  for(tr in 1:length(trees)){
    load(paste(trees[tr],"_",yr,"_Data.RData",sep=""))
    xseq <- seq(min(data$DOY),max(data$DOY),1)
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
    
    for(ty in 1:length(types)){
      load(paste(tree,"_2016_",indices[i],"_",types[ty],"_varBurn.RData",sep=""))
      
      ycred <- matrix(0,nrow=10000,ncol=length(xseq))
      ypred <- matrix(0,nrow=10000,ncol=length(xseq))
      for(g in 1:10000){
        Ey <- phenoLR(beta0=beta0[g],beta1=beta1[g],xseq=xseq)
        ycred[g,] <- Ey
        ypred[g,] <- rnorm(length(xseq),Ey,sqrt(1/prec[g]))
      }
      ci <- apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
      pi <- apply(ypred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
      
      plot(dat$x,dat$y,pch=20,ylab="",xlab="",cex.axis=2)
      ciEnvelope(xseq,pi[1,],pi[3,],col="blue")
      ciEnvelope(xseq,ci[1,],ci[3,],col="lightBlue")
      points(dat$x,dat$y,pch=20)
      lines(xseq,ci[2,],col="red")
    }
  }
  
}
dev.off()