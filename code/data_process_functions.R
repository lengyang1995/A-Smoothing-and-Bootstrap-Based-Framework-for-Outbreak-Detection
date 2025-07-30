library(forecast)
library(lubridate)
library(ggplot2)
library(EpiEstim)
library(dplyr)
library(doParallel)
library(deSolve)
library(readxl)
library(surveillance)
library(optimParallel)
################################### Calculate Rt assuming H=3
calculateR=function(dat){
  
  presum=c()
  cursum=c()
  for (i in 1:(length(dat)-9)  ) {
    presum=c(presum,sum(dat[i:(i+4)]))
  }
  for (i in 6:(length(dat)-4)  ) {
    cursum=c(cursum,sum(dat[i:(i+4)]))
  }
  RNEW=cursum/presum
  RNEW=RNEW[5:length(RNEW)]
  results=list(mean=RNEW)
  return (results)
}
################################### MA
sevenma <- function(dat){
  num=length(dat)
  ma=c()
  ma1=c()
  if (num>=7){
  for (i in 7:num) {
    ma=c(ma,sum(dat[(i-6):(i)])/7)
  }
  for (i in 1:6) {
    ma1=c(ma1,mean(dat[1:i]))
  }
  }else{
    for (i in 1:num) {
      ma1=c(ma1,mean(dat[1:i]))
    }
  }
  return (c(ma1,ma))
}
################################### MAH
sevenmaho <- function(dat,label){
  mah=c()
  dat1=as.data.frame(cbind(dat,label))
  num=length(dat)
  ma1=c()
  if (num>=7){
  for (i in 7:num) {
    count=0
    wks=0
    nwks=0
    for (j in (i-6):i) {
      if (dat1$label[j]==0){
        nwks=nwks+dat1$dat[j]
      } else {
        wks=wks+dat1$dat[j]
        count=count+1
      }
    }
    sum1=wks*5/count+nwks*2/(7-count)
    mah=c(mah,sum1/7)
  }
  for (i in 1:6) {
      ma1=c(ma1,mean(dat[1:i]))
  }
  }else{
  for (i in 1:num) {
    ma1=c(ma1,mean(dat[1:i]))
  }
  }
  return (c(ma1,mah))
}
################################### perform smoothing on data
#setwd('D:/NCID/Dayofweek/organize/overleaf_code_dorm/git/data')
case=data.frame(read_excel('covid_case.xlsx'))
#################################### 
mah=sevenmaho(case$Case,case$NWK)
mah_all=(mah)
ma=sevenma(case$Case)
ma_all=(ma)

