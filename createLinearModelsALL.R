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

createFits <- function(trees,year){
  output <- foreach (i=1:length(trees))%dopar%{
    load(paste(trees[i],"_",year,"_Data.RData",sep=""))
    
    ##mSR
    dat <- list()
    dat$DOY <- data$DOY
    dat$y <- data$mSR
    outFileName <- paste(trees[i],"_",year,"_mSR_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.linReg(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    ##chl
    dat <- list()
    dat$DOY <- data$DOY
    dat$y <- data$chl
    ind <- "chl"
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.linReg(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    ind <- "GM2" #######
    dat <- list()
    dat$DOY <- data$DOY
    dat$y <- data$GM2 #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.linReg(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
    
    ind <- "mND" #######
    dat <- list()
    dat$DOY <- data$DOY
    dat$y <- data$mND #######
    outFileName <- paste(trees[i],"_",year,"_",ind,"_varBurn.RData",sep="")
    if(!file.exists(outFileName)){
      j.model <- createModel.linReg(data=dat)
      var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=30000)
      save(var.Burn,file=outFileName)
    }
  }
}
createFits(trees=c("BI1","BI2","BI3","BI4","BI5"),year=2017)
