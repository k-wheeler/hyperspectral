library(doParallel)
install.packages("/projectnb/dietzelab/kiwheel/NEFI_pheno/PhenologyBayesModeling",repo=NULL)
install.packages("/projectnb/dietzelab/kiwheel/hyperspectral/spectraFits",repo=NULL)
library("spectraFits")
library("PhenologyBayesModeling")
library("rjags")
library("runjags")

#detect cores.
#n.cores <- detectCores()
n.cores <- 2

#register the cores.
registerDoParallel(cores=n.cores)

indices <- c("chl","car","NDVI_H","PRI","NDRE","GNDVI","GM1","RVI1","RVI2","LIC","CTR","GM2","VGM","PSRI","mSR","mND","DD","RGI","RE","NDVI_M","SIPI")
num <- 1
i=1
foreach(i=1:length(indices)) %dopar%{
  #treeFiles <- intersect(dir(path="measuredRData",pattern=paste(indices[i],"_Data.RData",sep="")),dir(path="measuredRData",pattern=treeSpecies))
  treeFiles <- dir(path="measuredRData",pattern=paste(indices[i],"_Data.RData",sep=""))
  ind.dat <- list()
  print(indices[i])
  # outfileName <- paste("measuredRData/",indices[i],"_",treeSpecies,"_measured_varBurn.RData",sep="")
  outfileName <- paste("measuredRData/",indices[i],"_col",num,"_measured_varBurn.RData",sep="")
  if(!file.exists(outfileName)){
    for(tr in 1:length(treeFiles)){
      load(paste("measuredRData/",treeFiles[tr],sep=""))
      print(treeFiles[tr])
      # print(data$x)
      # print(data$y)
      ind.dat$x <- c(ind.dat$x,data$x[num])
      ind.dat$obs.prec <- c(ind.dat$obs.prec,data$obs.prec[num])
      ind.dat$y <- c(ind.dat$y,data$y[num])
    }
    ind.dat$x <- na.omit(ind.dat$x)
    ind.dat$obs.prec <- na.omit(ind.dat$obs.prec)
    ind.dat$y <- na.omit(ind.dat$y)
    j.model <- createCorModel(data=ind.dat)
    var.Burn <- runMCMC_Model(j.model=j.model,variableNames = c("beta0","beta1","prec"))
    save(var.Burn,file=outfileName)
  }
}