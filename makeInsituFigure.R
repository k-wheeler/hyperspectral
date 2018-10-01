library("rjags")
library("runjags")
library("PhenologyBayesModeling")
library("spectraFits")


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
phenoExp <- function(c,a,xseq,b){
  return(a * exp(b*(xseq-xseq[1])) + c)
}
#indices <- c("NDRE","PRI","chl","NDVI_H","PSRI","car","mND","GNDVI")
indices <- c("NDRE","PRI","chl")
trees <- c("BE1","BI1","PO1")
types <- c("LR","Exp")
yr <- "2016"
pdf("InSitu_Figure.pdf",width=20,height=10)
par(mfrow=c(3,3), mai = c(0.35, 0.4, 0.15, 0.2))

for(i in 1:length(indices)){
  print(indices[i])
  for(tr in 1:length(trees)){
    tree <- trees[tr]
    if(tree=="PO1" && indices[i]=="PRI"){
      tree <- "PO3"
    }
    load(paste(tree,"_",yr,"_Data.RData",sep=""))
    xseq <- seq(min(data$DOY),max(data$DOY),1)
    dat <- list()
    dat$x <- data$DOY
    ind <- indices[i]
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
    ty <- 1
      load(paste(tree,"_2016_",indices[i],"_",types[ty],"_varBurn.RData",sep=""))
      out.mat <- as.matrix(var.Burn)
      beta0 <- out.mat[,1]
      beta1 <- out.mat[,2]
      prec <- out.mat[,3]
      
      ycred <- matrix(0,nrow=10000,ncol=length(xseq))
      ypred <- matrix(0,nrow=10000,ncol=length(xseq))
      for(g in 1:10000){
        Ey <- phenoLR(beta0=beta0[g],beta1=beta1[g],xseq=xseq)
        ycred[g,] <- Ey
        ypred[g,] <- rnorm(length(xseq),Ey,sqrt(1/prec[g]))
      }
      ci.LR <- apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
      pi.LR <- apply(ypred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)

      ty <- 2
      load(paste(tree,"_2016_",indices[i],"_",types[ty],"_varBurn.RData",sep=""))
      out.mat <- as.matrix(var.Burn)
      print(colnames(out.mat))
      a <- out.mat[,1]
      b <- out.mat[,2]
      c <- out.mat[,3]
      prec <- out.mat[,4]
      ycred <- matrix(0,nrow=10000,ncol=length(xseq))
      ypred <- matrix(0,nrow=10000,ncol=length(xseq))
      for(g in 1:10000){
        Ey <- phenoExp(a=a[g],b=b[g],c=c[g],xseq=xseq)
        ycred[g,] <- Ey
        ypred[g,] <- rnorm(length(xseq),Ey,sqrt(1/prec[g]))
      }
      ci.Exp <- apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
      pi.Exp <- apply(ypred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
      print(dat$x)
      plot(as.Date(as.numeric(dat$x),origin="2015-12-31"),dat$y,pch=20,ylab="",xlab="",cex.axis=3,xlim=c(as.Date("2016-08-01"),as.Date("2016-11-20")))
      print("past first plot")
      ciEnvelope(as.Date(xseq,origin="2015-12-31"),pi.Exp[1,],pi.Exp[3,],col=rgb(0,0,1,0.5))
      ciEnvelope(as.Date(xseq,origin="2015-12-31"),pi.LR[1,],pi.LR[3,],col=rgb(1,0,0,0.5))
      #ciEnvelope(xseq,ci.Exp[1,],ci.Exp[3,],col=rgb(0,0,1,0.2))
      #ciEnvelope(xseq,ci.LR[1,],ci.LR[3,],col=rgb(1,0,0,0.2))
      
      points(as.Date(as.numeric(dat$x),origin="2015-12-31"),dat$y,pch=20)
      lines(as.Date(xseq,origin="2015-12-31"),ci.LR[2,],col=colors()[556],lwd=2)
      lines(as.Date(xseq,origin="2015-12-31"),ci.LR[1,],col=colors()[556],lty="dashed",lwd=2)
      lines(as.Date(xseq,origin="2015-12-31"),ci.LR[3,],col=colors()[556],lty="dashed",lwd=2)
      lines(as.Date(xseq,origin="2015-12-31"),ci.Exp[2,],col="blue",lwd=2)
      lines(as.Date(xseq,origin="2015-12-31"),ci.Exp[1,],col="blue",lty="dashed",lwd=2)
      lines(as.Date(xseq,origin="2015-12-31"),ci.Exp[3,],col="blue",lty="dashed",lwd=2)
      
    
  }
  
}
dev.off()