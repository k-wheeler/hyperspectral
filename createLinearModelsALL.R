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
i=1
trees <- c("BI1","BI2","BI3")
year=2016
createFits <- function(trees,year){
  output <- foreach (i=1:length(trees))%dopar%{
    load(paste(trees[i],"_",year,"_Data.RData",sep=""))
    
    ##mSR
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$mSR
    outFileName <- paste(trees[i],"_",year,"_mSR_varBurn.RData",sep="")
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
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    ind <- "GM2" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$GM2 #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    ind <- "mND" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$mND #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    ind <- "NDVI_M" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$NDVI_M #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    ind <- "NDRE" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$NDRE #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    ind <- "VGM" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$VGM #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    ind <- "PSRI" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$PSRI #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    ind <- "GNDVI" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$GNDVI #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    ind <- "RGI" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$RGI #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    ind <- "DD" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$DD #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    ind <- "PRI" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$PRI #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    ind <- "GM1" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$GM1 #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    ind <- "SIPI" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$SIPI #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      print("createdModel")
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    # ind <- "RE" #######
    # dat <- list()
    # dat$DOY <- data$DOY
    # dat$y <- data$RE #######
    # outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    # if(!file.exists(outFileName)){
    #   j.model <- createModel.LR(data=dat)
    #   var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
    #   save(var.Burn,file=outFileName)
    # }
    
    ind <- "car" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$car #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    ind <- "NDVI_H" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$NDVI_H #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    ind <- "RVI1" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$RVI1 #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    ind <- "RVI2" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$RVI2 #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    ind <- "LIC" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$LIC #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    ind <- "CTR" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$CTR #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    ind <- "DCN" #######
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$DCN #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.LR(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
  }
}
createFits(trees=c("BI1","BI2","BI3","BI4","BI5"),year=2017)
