library("spectraFits")

main <- function(trees,processModel,dataPath,year){
  ##Create data objects for each tree
  for(i in 1:length(trees)){
    leaf.files <- dir(path=dataPath,pattern=trees[i])
    DOY <- numeric()
    mSR <- numeric()
    chl <- numeric()
    NDVI_M <- numeric()
    NDRE <- numeric()
    GM2 <- numeric()
    mND <- numeric()
    VGM <- numeric()
    PSRI <- numeric()
    GNDVI <- numeric()
    RGI <- numeric()
    DD <- numeric()
    PRI <- numeric()
    GM1 <- numeric()
    car <- numeric()
    RE <- numeric()
    NDVI_H <- numeric()
    RVI1 <- numeric()
    RVI2 <- numeric()
    LIC <- numeric()
    CTR <- numeric()
    DCN <- numeric()
    
    for(j in 1:length(leaf.files)){
      print(j)
      dat <- data.frame(read.csv(paste(dataPath,"/",leaf.files[j],sep=""),header=FALSE))
      colnames(dat) <- c("wvl","ref")
      chl <- c(chl,calND(dat,780,712))
      car <- c(car,calND(dat,800,530))
      NDVI_H <- c(NDVI_H,calND(dat,800,680))
      PRI <- c(PRI,calND(dat,531,570))
      NDRE <- c(NDRE,calND(dat,790,720))
      GNDVI <- c(GNDVI,calND(dat,750,550))
      GM1 <- c(GM1,calRatio(dat,750,550))
      RVI1 <- c(RVI1,calRatio(dat,810,660))
      RVI2 <- c(RVI2,calRatio(dat,810,560))
      LIC <- c(LIC,calRatio(dat,440,690))
      CTR <- c(CTR,calRatio(dat,695,420))
      GM2 <- c(GM2,calRatio(dat,750,700))
      VGM <- c(VGM,calRatio(dat,740,720))
      PSRI <- c(PSRI,calPSRI(dat))
      mSR <- c(mSR,calmSR(dat))
      mND <- c(mND,calmND(dat))
      DCN <- c(DCN,calDCN(dat))
      DD <- c(DD,calDD(dat))
      RGI <- c(RGI,calRGI(dat))
      RE <- c(RE,calRE(dat))
      NDVI_M <- c(NDVI_M,calNDVI_M(dat))
      
      date <- strsplit(leaf.files[j],"_")[[1]][2]
      yr <- substr(date,5,6)
      mth <- substr(date,1,2)
      dy <- substr(date,3,4)
      date <- as.Date(paste("20",yr,"-",mth,"-",dy,sep=""))
      DOY <- c(DOY,format(date, "%j"))
    }
    data <- list()
    data$mSR <- mSR
    data$chl <- chl
    data$NDVI_M <- NDVI_M
    data$NDRE <- NDRE
    data$GM2 <- GM2
    data$mND <- mND
    data$VGM <- VGM
    data$PSRI <- PSRI
    data$GNDVI <- GNDVI
    data$RGI <- RGI
    data$DD <- DD
    data$PRI <- PRI
    data$GM1 <- GM1
    data$car <- car
    data$RE <- RE
    data$NDVI_H <- NDVI_H
    data$RVI1 <- RVI1
    data$RVI2 <- RVI2
    data$LIC <- LIC
    data$CTR <- CTR
    data$DCN <- DCN
   
    data$DOY <- DOY
    
    outFileName <- paste(trees[i],"_",year,"_Data.RData",sep="")
    save(data,file=outFileName)
  }
  
}

dataPath <- "WheelerSpectraData/Fall2016_FieldSpectra"
trees <- c("BI1","BI2","BI3","BI4","BI5")
year <- 2017
main(trees=trees,dataPath=dataPath,year=year)
