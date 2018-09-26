install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
install.packages("/projectnb/dietzelab/kiwheel/hyperspectral/spectraFits",repo=NULL)
library("spectraFits")
library("PhenologyBayesModeling")
library("rjags")
library("runjags")

library(doParallel)


#detect cores.
#n.cores <- detectCores()
n.cores <- 4

#register the cores.
registerDoParallel(cores=n.cores)
#i=1

toDo <- matrix(nrow=6,ncol=4)
#toDo[,1] <- c("BE2","BE4","BE5","BE5","BE5","BE5","BI2","BI2","BI3","PO1","PO1","PO2","PO2","PO3","PO3","PO3","PO3","PO3","PO3","PO4","PO5","PO5","PO5","PO5")
#toDo[,2] <- c("DD","DD","NDVI_H","GM2","VGM","RGI","PRI","LIC","LIC","PRI","GM2","RVI1","GM2","car","NDVI_H","RVI1","GM2","RGI","SIPI","GNDVI","NDVI_H","GNDVI","RVI2","NDVI_M")
#toDo[,1] <- c("BE2","BE4","BE5","BE5","BE5","BE5")
#toDo[,2] <- c("DD","DD","NDVI_H","GM2","VGM","RGI")
              
#toDo[,3] <- c(-0.05,-0.07,-0.0000005,-0.1,-0.0005,0.00000003)
#toDo[,4] <- c(0.065,0.09,0.145,0.035,0.07,0.18)
#toDo[,1] <- c("BI2","BI2","BI3","PO1","PO1","PO2")
#toDo[,2] <- c("PRI","LIC","LIC","PRI","GM2","RVI1")
#toDo[,3] <- c(-0.00002,a=-0.00008,-0.00008,-0.000002,-0.1,-0.0001)
#toDo[,4] <- c(0.095,0.08,0.085,0.13,0.035,0.12)
#toDo[,1] <- c("PO2","PO3","PO3","PO3","PO3","PO3")
#toDo[,2] <- c("GM2","RGI","SIPI","GNDVI","NDVI_H","GNDVI")
#toDo[,3] <- c(-0.01,-0.01,-0.0000005,-0.01,-0.01,0.000000003)#,0.000003)
#toDo[,4] <- c(0.06,0.033,0.145,0.075,0.06,0.22)#0.14
toDo[,1] <- c("PO3","PO4","PO5","PO5","PO5","PO5")
toDo[,2] <- c("SIPI","GNDVI","NDVI_H","GNDVI","RVI2","NDVI_M")
toDo[,3] <- c(0.000003,-0.0003,-0.0000005,-0.0003,-0.005,-0.0005)
toDo[,4] <- c(0.14,0.07,0.145,0.07,0.06,0.07)

toDo[,1] <- c("BE2","BE4","BE5","BE5","BE5","BE5")
toDo[,2] <- c("DD","DD","NDVI_H","NDVI_H","VGM","RGI")
toDo[,3] <- c(-0.05,-0.08,-0.00000000005,-0.1,-0.0005,0.000000003)
toDo[,4] <- c(0.0646,0.085,0.24,0.035,0.07,0.203)



output <- foreach(i=1:nrow(toDo))%dopar%{
  load(paste(toDo[i,1],"_2016_Data.RData",sep=""))
  
  ind <- toDo[i,2]
  dat <- list()
  dat$x <- as.numeric(data$DOY)

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
  inits <- list()
  nchain <- 5
  for(j in 1:nchain){
    inits[[j]] <- list(a=rnorm(1,as.numeric(toDo[i,3]),abs(as.numeric(toDo[i,3])/5)),b=rnorm(1,as.numeric(toDo[i,4]),abs(as.numeric(toDo[i,4])/5)),c=rnorm(1,mean(dat$y[1:5]),abs(mean(dat$y[1:5])/50)))
  }
  
  outFileName <- paste(toDo[i,1],"_2016_",ind,"_Exp_varBurn.RData",sep="")
  if(!file.exists(outFileName)){
    j.model <- createModel.Exp(data=dat,index=ind,inits=inits)
    var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("a","b","c","prec"),baseNum=50000,iterSize=50000,maxGBR=3,ID=paste(ind,toDo[i,1],sep="_"))
    if(typeof(var.Burn)!=typeof(FALSE)){
      save(var.Burn,file=outFileName)
    }
  }
}



