install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
install.packages("/projectnb/dietzelab/kiwheel/hyperspectral/spectraFits",repo=NULL)
library("spectraFits")
library("PhenologyBayesModeling")
library("rjags")
library("runjags")

library(doParallel)

#detect cores.
#n.cores <- detectCores()
n.cores <- 2

#register the cores.
registerDoParallel(cores=n.cores)
trees <- c("BI1","BI2","BI3","BI4","BI5","BE1","BE2","BE3","BE4","BE5","PO1","PO2","PO3","PO4","PO5")
year=2016
createFits <- function(trees,year){
  #output <- foreach (i=1:length(trees))%dopar%{
  for(i in 1:length(trees)){ 
    load(paste(trees[i],"_",year,"_Data.RData",sep=""))
    
    ##mSR
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$mSR
    outFileName <- paste(trees[i],"_",year,"_mSR_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    ##chl
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$chl
    ind <- "chl"
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    ind <- "GM2" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$GM2 #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    print(ind)
    ind <- "mND" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$mND #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    print(ind)
    
    ind <- "NDVI_M" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$NDVI_M #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
        print(ind)
    ind <- "NDRE" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$NDRE #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
        print(ind)
    ind <- "VGM" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$VGM #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
        print(ind)
    ind <- "PSRI" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$PSRI #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
        print(ind)
    ind <- "GNDVI" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$GNDVI #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
        print(ind)
    ind <- "RGI" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$RGI #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
        print(ind)
    ind <- "DD" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$DD #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
        print(ind)
    ind <- "PRI" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$PRI #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
        print(ind)
    ind <- "GM1" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$GM1 #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
        print(ind)
    ind <- "RE" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$RE #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
        print(ind)
    ind <- "NDVI_H" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$NDVI_H #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
        print(ind)
    ind <- "RVI1" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$RVI1 #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
        print(ind)
    ind <- "RVI2" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$RVI2 #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
        print(ind)
    ind <- "LIC" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$LIC #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
        print(ind)
    ind <- "CTR" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$CTR #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
        print(ind)
    ind <- "SIPI" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$SIPI #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    print(ind)
    ind <- "car" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$car #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_LR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    print(ind)
  }
}
trees <- c("BE1","BE2","BE3","BE4","BE5","PO1","PO2","PO3","PO4","PO5","BI1","BI2","BI3","BI4","BI5")

createFits(trees=trees,year=2016)
