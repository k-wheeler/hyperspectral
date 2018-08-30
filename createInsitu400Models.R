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

output <- foreach(t=1:length(trees)) %dopar% {
  load(paste(trees[t],"_","2016_Data_400.RData",sep=""))
  dat <- list()
  dat$x <- data$DOY
  dat$y <- data$R445
  outFileName <- paste(trees[t],"_2016_445_varBurn.RData",sep="")
  if(!file.exists(outFileName)){
    j.model <- createModel.LR(data=dat)
    var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=20000)
    save(var.Burn,file=outFileName)
  }
  
  dat <- list()
  dat$x <- data$DOY
  dat$y <- data$R420
  outFileName <- paste(trees[t],"_2016_420_varBurn.RData",sep="")
  if(!file.exists(outFileName)){
    j.model <- createModel.LR(data=dat)
    var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"),baseNum=10000,iterSize=20000)
    save(var.Burn,file=outFileName)
  }
}



