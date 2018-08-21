library("spectraFits")
library("rjags")
library("runjags")
library("PhenologyBayesModeling")

#indices <- c("chl","car","NDVI_H","PRI","NDRE","GNDVI","GM1","RVI1","RVI2","LIC","CTR","GM2","VGM","PSRI","mSR","mND","DD","RGI","RE","NDVI_M","SIPI")
# for(i in 1:length(indices)){
#   print(indices[i])
#   outFileName <- paste("DIC_collected_values/",indices[i],"_all_DIC.RData",sep="")
#   if(!file.exists(outFileName)){
#     load(paste("measuredRData/",indices[i],"_","allMeasured.RData",sep=""))
#     load(paste("measuredRData/",indices[i],"_all_measured_varBurn.RData",sep=""))
#     j.model <- createCorModel(data=data)
#     var.sum <- summary(var.Burn)
#     DIC <- dic.samples(j.model,n.iter = var.sum$end)
#     outFileName <- paste("DIC_collected_values/",indices[i],"_all_DIC.RData",sep="")
#     print(outFileName)
#     save(DIC,file=outFileName)
#   }
# }
indices <- c("chl","car","NDVI_H","PRI","NDRE","GNDVI","GM1","RVI1","RVI2","LIC","CTR","GM2","VGM","PSRI","mSR","mND","DD","RGI","RE","NDVI_M","SIPI")
tree <- "col1"
for(i in 1:length(indices)){
  print(indices[i])
  outFileName <- paste("DIC_collected_values/",indices[i],"_",tree,"_DIC.RData",sep="")
  if(!file.exists(outFileName)){
    load(paste("measuredRData/",indices[i],"_",tree,"_allmeasured.RData",sep=""))
    load(paste("measuredRData/",indices[i],"_",tree,"_measured_varBurn.RData",sep=""))
    j.model <- createCorModel(data=data)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
}
tree <- "col2"
for(i in 1:length(indices)){
  print(indices[i])
  outFileName <- paste("DIC_collected_values/",indices[i],"_",tree,"_DIC.RData",sep="")
  if(!file.exists(outFileName)){
    load(paste("measuredRData/",indices[i],"_",tree,"_allmeasured.RData",sep=""))
    load(paste("measuredRData/",indices[i],"_",tree,"_measured_varBurn.RData",sep=""))
    j.model <- createCorModel(data=data)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
}
tree <- "col3"
for(i in 1:length(indices)){
  print(indices[i])
  outFileName <- paste("DIC_collected_values/",indices[i],"_",tree,"_DIC.RData",sep="")
  if(!file.exists(outFileName)){
    load(paste("measuredRData/",indices[i],"_",tree,"_allmeasured.RData",sep=""))
    load(paste("measuredRData/",indices[i],"_",tree,"_measured_varBurn.RData",sep=""))
    j.model <- createCorModel(data=data)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
}
tree <- "col4"
for(i in 1:length(indices)){
  print(indices[i])
  outFileName <- paste("DIC_collected_values/",indices[i],"_",tree,"_DIC.RData",sep="")
  if(!file.exists(outFileName)){
    load(paste("measuredRData/",indices[i],"_",tree,"_allmeasured.RData",sep=""))
    load(paste("measuredRData/",indices[i],"_",tree,"_measured_varBurn.RData",sep=""))
    j.model <- createCorModel(data=data)
    var.sum <- summary(var.Burn)
    DIC <- dic.samples(j.model,n.iter = var.sum$end)
    print(outFileName)
    save(DIC,file=outFileName)
  }
}
tree <- "col5"
for(i in 1:length(indices)){
  print(indices[i])
  outFileName <- paste("DIC_collected_values/",indices[i],"_",tree,"_DIC.RData",sep="")
  if(!file.exists(outFileName)){
    load(paste("measuredRData/",indices[i],"_",tree,"_allmeasured.RData",sep=""))
    load(paste("measuredRData/",indices[i],"_",tree,"_measured_varBurn.RData",sep=""))
    j.model <- createCorModel(data=data2)
    var.Burn2 <- runMCMC_Model(j.model = j.model,variableNames = c("beta0","beta1","prec"))
    var.sum2 <- summary(var.Burn2)
    DIC2 <- dic.samples(j.model,n.iter = var.sum2$end)
    DIC2
    print(outFileName)
    save(DIC,file=outFileName)
  }
}