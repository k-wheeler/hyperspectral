mdl <- "LR"
trees <- c("BE1","BE2","BE3","BE4","BE5","PO1","PO2","PO3","PO4","PO5","BI1","BI2","BI3","BI4","BI5")
indices <- c("chl","car","NDVI_H","PRI","NDRE","GNDVI","GM1","RVI1","RVI2","LIC","CTR","GM2","VGM","PSRI","mSR","mND","DD","RGI","RE","NDVI_M","SIPI")

for(tr in 1:length(trees)){
  for(i in 1:length(indices)){
    fileName <- paste(trees[tr],"_2016_",indices[i],"_",mdl,"_varBurn.RData",sep="")
    if(!file.exists(fileName)){
      print(paste(trees[tr],"_",indices[i],sep=""))
    }
  }
}
