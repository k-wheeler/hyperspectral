library("spectraFits")
library("rjags")
library("runjags")
library("PhenologyBayesModeling")

library(doParallel)


#detect cores.
#n.cores <- detectCores()
n.cores <- 2

#register the cores.
registerDoParallel(cores=n.cores)

indices <- c("chl","car","NDVI_H","PRI","NDRE","GNDVI","GM1","RVI1","RVI2","LIC","CTR","GM2","VGM","PSRI","mSR","mND","DD","RGI","RE","NDVI_M","SIPI")
trees <- c("PO1","PO2","PO3","PO4","PO5","BE1","BE2","BE3","BE4","BE5","BI1","BI2","BI3","BI4","BI5")
tp <- "Exp"
output <- foreach(t=1:length(trees))%dopar%{
  #for(t in 1:length(trees)){
  
  ind <- "DD" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$DD ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  
  ind <- "NDVI_M" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$NDVI_M ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  ind <- "SIPI" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$SIPI ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  
  ind <- "RGI" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$RGI ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  
  ind <- "PSRI" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$PSRI ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  
  ind <- "mND" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$mND ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  
  ind <- "mSR" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$mSR ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  
  ind <- "chl" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$chl ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  
  ind <- "car" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$car ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  
  ind <- "NDVI_H" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$NDVI_H ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  
  ind <- "PRI" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$PRI ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  
  ind <- "NDRE" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$NDRE ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  
  ind <- "GNDVI" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$GNDVI ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  
  ind <- "GM1" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$GM1 ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  
  ind <- "RVI1" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$RVI1 ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  
  ind <- "RVI2" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$RVI2 ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  
  ind <- "LIC" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$LIC ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  
  ind <- "CTR" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$CTR ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  
  ind <- "GM2" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$GM2 ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  
  ind <- "VGM" #####
  outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  if(!file.exists(outFileName) && file.exists(inFileName)){
    load(paste(trees[t],"_2016_Data.RData",sep=""))
    dat <- list()
    dat$x <- data$DOY
    dat$y <- data$VGM ####
    load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
    j.model <- createModel.Exp(data=dat,index=ind)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
  
  # ind <- "RE" #####
  # outFileName <- paste("DIC_collected_values/",ind,"_",trees[t],"_",tp,"_DIC.RData",sep="")
  # inFileName <- paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep="")
  # if(!file.exists(outFileName) && file.exists(inFileName)){
  #   load(paste(trees[t],"_2016_Data.RData",sep=""))
  #   dat <- list()
  #   dat$x <- data$DOY
  #   dat$y <- data$RE ####
  #   load(paste(trees[t],"_2016_",ind,"_",tp,"_varBurn.RData",sep=""))
  #   j.model <- createModel.Exp(data=dat,index=ind)
  #   var.sum <- summary(var.Burn)
  #   DIC <- dic.samples(j.model,n.iter = var.sum$end)
  #   print(outFileName)
  #   save(DIC,file=outFileName)
  # }
  

}