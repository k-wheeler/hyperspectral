##' Create the credible interval envelope for plotting
##' 
##' @param x time range
##' @param ylo the bottom credible interval values
##' @param yhi the top credible interval values
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

phenoExp <- function(c,a,xseq,b){
  return(a * exp(b*(xseq-xseq[1])) + c)
}
indices <- c("chl","car","NDVI_H","PRI","NDRE","GNDVI","GM1","RVI1","RVI2","LIC","CTR","GM2","VGM","PSRI","mSR","mND","DD","RGI","RE","NDVI_M","SIPI")
trees <- c("BE1","BE2","BE3","BE4","BE5","BI1","BI2","BI3","BI4","BI5","PO1","PO2","PO3","PO4","PO5")
i=1
t=1
pdf("Exp_Fits.pdf",width=45,height=45)
par(mfrow=c(5,5))
for(t in 1:length(trees)){
  load(paste(trees[t],"_",yr,"_Data.RData",sep=""))
  xseq <- seq(min(data$DOY),max(data$DOY),1)
  
  for(i in 1:length(indices)){
    ind <- indices[i]
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
    varFile <- paste(trees[t],"_2016_",indices[i],"_Exp_varBurn.RData",sep="")
    print(varFile)
    plot(dat$x,dat$y,main=paste(trees[t],indices[i],"Exp fit",sep=" "),pch=20)
    
    if(file.exists(varFile)){
      load(varFile)
      out.mat <- as.matrix(var.Burn)
      colnames(out.mat)
      a <- out.mat[,1]
      b <- out.mat[,2]
      c <- out.mat[,3]
      prec <- out.mat[,4]
      ycred <- matrix(0,nrow=10000,ncol=length(xseq))
      ypred <- matrix(0,nrow=10000,ncol=length(xseq))
      for(g in 1:10000){
        Ey <- phenoExp(beta0=beta0[g],beta1=beta1[g],xseq=xseq)
        ycred[g,] <- Ey
        ypred[g,] <- rnorm(length(xseq),Ey,sqrt(1/prec[g]))
      }
      ci <- apply(ycred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
      pi <- apply(ypred,2,quantile,c(0.025,0.5, 0.975), na.rm= TRUE)
      ciEnvelope(xseq,pi[1,],pi[3,],col="blue")
      ciEnvelope(xseq,ci[1,],ci[3,],col="lightBlue")
      points(dat$x,dat$y,pch=20)
      lines(xseq,ci[2,],col="red")
    }
  }
}
dev.off()