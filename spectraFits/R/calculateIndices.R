calND <- function(dat,ref1,ref2){
  R1 <- dat[dat$wvl==ref1,"ref"]
  R2 <- dat[dat$wvl==ref2,"ref"]
  return((R1-R2)/(R1+R2))
}

calRatio <- function(dat,ref1,ref2){
  R1 <- dat[dat$wvl==ref1,"ref"]
  R2 <- dat[dat$wvl==ref2,"ref"]
  return(R1/R2)
}
calPSRI <- function(dat){
  R680 <- dat[dat$wvl==680,"ref"]
  R500 <- dat[dat$wvl==500,"ref"]
  R750 <- dat[dat$wvl==750,"ref"]
  return((R680-R500)/R750)
}
calmSR <- function(dat){
  R445 <- dat[dat$wvl==445,"ref"]
  R705 <- dat[dat$wvl==705,"ref"]
  R750 <- dat[dat$wvl==750,"ref"]
  return((R750-R445)/(R705-R445))
}
calmND <- function(dat){
  R445 <- dat[dat$wvl==445,"ref"]
  R705 <- dat[dat$wvl==705,"ref"]
  R750 <- dat[dat$wvl==750,"ref"]
  return((R750-R705)/(R750+R705-2*R445))
}

calDCN <- function(dat){
  R720 <- dat[dat$wvl==720,"ref"]
  R700 <- dat[dat$wvl==700,"ref"]
  R670 <- dat[dat$wvl==670,"ref"]
  return((R720-R700)/(R700-R670)/(R720-R670+0.03))
}

calDD <- function(dat){
  R749 <- dat[dat$wvl==749,"ref"]
  R720 <- dat[dat$wvl==720,"ref"]
  R701 <- dat[dat$wvl==701,"ref"]
  R672 <- dat[dat$wvl==672,"ref"]
  return((R749-R720)-(R701-R672))
}

calRGI <- function(dat){
  dem <- sum(dat[dat$wvl<599 & dat$wvl>499,"ref"])
  num <- sum(dat[dat$wvl<699 & dat$wvl>599,"ref"])
  return(num/dem)
}
calRE <- function(dat){
  subDat <- dat[dat$wvl<900 & dat$wvl>599,]
  newRow <- data.frame(matrix(nrow=1,ncol=2))
  subDat2 <- rbind(c(0,0),subDat[1:(nrow(subDat)-1),])
  diffs <- subDat[,2]-subDat2[,2]
  diffVals <- cbind(subDat[,1],diffs)
  diffVals <- diffVals[2:nrow(diffVals),]
  return(max(diffVals[,2]))
}
calNDVI_M <- function(dat){
  NIR <- mean(dat[dat$wvl<877 & dat$wvl>840,"ref"])
  R <- mean(dat[dat$wvl<671 & dat$wvl>619,"ref"])
  return((NIR-R)/(NIR+R))
}
