num_cores = detectCores()
cl = makeCluster(num_cores)
registerDoParallel(cl)
#stopCluster(cl)
####################################################################################### Set H=2 and let K vary from 1 to 4
H=2

bootstraptest1=function(H,mah_original,calculateR,fit){
  maxiter=500
  RARC=c()
  for (iter in 1:maxiter) {
    output=c()
    for (number in (index-15):index){
      output=c(output,rpois(1,fit$fitted[number]))
    } 
    output=round(output)
    output[is.na(output)]=0
    
    
    output=sum(output[(length(output)-H+1):length(output)])
    
    
    RARC[iter]=output
  }
  CIARUPPER=numeric()
  CIARLOWER=numeric()
  CIRARUPPER=numeric()
  CIRARLOWER=numeric()
  
  
  RARC=sort(RARC)  
  
  
  
  CIRARUPPER=RARC[length(RARC)*0.97]
  CIRARLOWER=RARC[length(RARC)*0.08]
  
  results = list(lower = CIRARLOWER,upper=CIRARUPPER)
  return (results)
}

bootstraptestOR2=function(t1,t2,K,H,case,dummy,calculateR,bootstraptest1){
  ress1 = foreach(jj = t1:t2,.combine='rbind',.packages=c('forecast','dplyr','data.table','stats'))%dopar%{
    index=jj-H
    mah=case$Case[1:jj]
    
    mah_original=mah[1:index]
    cur_val=sum(mah[(jj-H+1):jj])
    
    
    fit=auto.arima(mah_original)
    fit$fitted[fit$fitted<0]=0
    bt=bootstraptest1(H,mah_original,calculateR,fit)
    up=bt$upper
    c(jj,up,cur_val)
  }
  sig=c()
  for (i in 1:dim(ress1)[1]) {
    if(ress1[i,3]>ress1[i,2]){
      sig=c(sig,1)
    }else{
      sig=c(sig,0)
    }
  }
  ress1=cbind(ress1,sig)
  
  output=t2
  for (ij in K:dim(ress1)[1]) {
    if( sum(ress1[((ij-K+1):ij),4]==1)==K  ){
      output=ress1[ij,1]
      break
    }
  }
  return(output)
}

bootstraptestMA2=function(t1,t2,K,H,case,dummy,calculateR,bootstraptest1,mah_all){
  ress1 = foreach(jj = t1:t2,.combine='rbind',.packages=c('forecast','dplyr','data.table','stats'))%dopar%{
    index=jj-H
    mah=mah_all[1:jj]
    
    mah_original=mah[1:index]
    cur_val=sum(mah[(jj-H+1):jj])
    
    
    fit=auto.arima(mah_original)
    fit$fitted[fit$fitted<0]=0
    bt=bootstraptest1(H,mah_original,calculateR,fit)
    up=bt$upper
    c(jj,up,cur_val)
  }
  sig=c()
  for (i in 1:dim(ress1)[1]) {
    if(ress1[i,3]>ress1[i,2]){
      sig=c(sig,1)
    }else{
      sig=c(sig,0)
    }
  }
  ress1=cbind(ress1,sig)
  
  output=t2
  for (ij in K:dim(ress1)[1]) {
    if( sum(ress1[((ij-K+1):ij),4]==1)==K  ){
      output=ress1[ij,1]
      break
    }
  }
  return(output)
}

bootstraptestM2=function(t1,t2,K,H,case,dummy,calculateR,bootstraptest1,ma_all){
  ress1 = foreach(jj = t1:t2,.combine='rbind',.packages=c('forecast','dplyr','data.table','stats'))%dopar%{
    index=jj-H
    mah=ma_all[1:jj]
    
    mah_original=mah[1:index]
    cur_val=sum(mah[(jj-H+1):jj])
    
    
    fit=auto.arima(mah_original)
    fit$fitted[fit$fitted<0]=0
    bt=bootstraptest1(H,mah_original,calculateR,fit)
    up=bt$upper
    c(jj,up,cur_val)
  }
  sig=c()
  for (i in 1:dim(ress1)[1]) {
    if(ress1[i,3]>ress1[i,2]){
      sig=c(sig,1)
    }else{
      sig=c(sig,0)
    }
  }
  ress1=cbind(ress1,sig)
  
  output=t2
  for (ij in K:dim(ress1)[1]) {
    if( sum(ress1[((ij-K+1):ij),4]==1)==K  ){
      output=ress1[ij,1]
      break
    }
  }
  return(output)
}


pred_list2=matrix(data=NA, nrow = 4, ncol = 18)
for (K in 1:4) {
  t1=c(34,103,151,246,399,527)
  t2=c(45,110,209,327,468,565)
  timeor=c()
  for (ii in 1:length(t2)) {
    time1=t1[ii]
    time2=t2[ii]
    fit1=bootstraptestOR2(time1,time2,K,H,case,dummy,calculateR,bootstraptest1)
    timeor=c(timeor,(time2-fit1)/(time2-time1))
  }
  pred_list2[K,1:6]=timeor
}
for (K in 1:4) {
  t1=c(34,104,153,246,399,527)
  t2=c(50,115,211,331,474,565)
  timeor=c()
  for (ii in 1:length(t2)) {
    time1=t1[ii]
    time2=t2[ii]
    fit1=bootstraptestMA2(time1,time2,K,H,case,dummy,calculateR,bootstraptest1,mah_all)
    timeor=c(timeor,(time2-fit1)/(time2-time1))
  }
  pred_list2[K,7:12]=timeor
}
for (K in 1:4) {
  t1=c(34,104,153,246,399,527)
  t2=c(50,115,211,331,474,565)
  timeor=c()
  for (ii in 1:length(t2)) {
    time1=t1[ii]
    time2=t2[ii]
    fit1=bootstraptestM2 (time1,time2,K,H,case,dummy,calculateR,bootstraptest1,ma_all)
    timeor=c(timeor,(time2-fit1)/(time2-time1))
  }
  pred_list2[K,13:18]=timeor
}

c1=c(pred_list2[3,1:6])
c2=c(pred_list2[2,7:12])
c3=c(pred_list2[2,13:18])
c(mean(c1),mean(c2),mean(c3))


####################################################################################### Set H=3 and let K vary from 1 to 4
H=3

bootstraptest1=function(H,mah_original,calculateR,fit){
  maxiter=500
  RARC=c()
  for (iter in 1:maxiter) {
    output=c()
    for (number in (index-15):index){
      output=c(output,rpois(1,fit$fitted[number]))
    } 
    output=round(output)
    output[is.na(output)]=0
    
    
    output=sum(output[(length(output)-H+1):length(output)])
    
    
    RARC[iter]=output
  }
  CIARUPPER=numeric()
  CIARLOWER=numeric()
  CIRARUPPER=numeric()
  CIRARLOWER=numeric()
  
  
  RARC=sort(RARC)  
  
  
  
  CIRARUPPER=RARC[length(RARC)*0.99]
  CIRARLOWER=RARC[length(RARC)*0.08]
  
  results = list(lower = CIRARLOWER,upper=CIRARUPPER)
  return (results)
}

bootstraptestOR2=function(t1,t2,K,H,case,dummy,calculateR,bootstraptest1){
  ress1 = foreach(jj = t1:t2,.combine='rbind',.packages=c('forecast','dplyr','data.table','stats'))%dopar%{
    index=jj-H
    mah=case$Case[1:jj]
    
    mah_original=mah[1:index]
    cur_val=sum(mah[(jj-H+1):jj])
    
    
    fit=auto.arima(mah_original)
    fit$fitted[fit$fitted<0]=0
    bt=bootstraptest1(H,mah_original,calculateR,fit)
    up=bt$upper
    c(jj,up,cur_val)
  }
  sig=c()
  for (i in 1:dim(ress1)[1]) {
    if(ress1[i,3]>ress1[i,2]){
      sig=c(sig,1)
    }else{
      sig=c(sig,0)
    }
  }
  ress1=cbind(ress1,sig)
  
  output=t2
  for (ij in K:dim(ress1)[1]) {
    if( sum(ress1[((ij-K+1):ij),4]==1)==K  ){
      output=ress1[ij,1]
      break
    }
  }
  return(output)
}

bootstraptestMA2=function(t1,t2,K,H,case,dummy,calculateR,bootstraptest1,mah_all){
  ress1 = foreach(jj = t1:t2,.combine='rbind',.packages=c('forecast','dplyr','data.table','stats'))%dopar%{
    index=jj-H
    mah=mah_all[1:jj]
    
    mah_original=mah[1:index]
    cur_val=sum(mah[(jj-H+1):jj])
    
    
    fit=auto.arima(mah_original)
    fit$fitted[fit$fitted<0]=0
    bt=bootstraptest1(H,mah_original,calculateR,fit)
    up=bt$upper
    c(jj,up,cur_val)
  }
  sig=c()
  for (i in 1:dim(ress1)[1]) {
    if(ress1[i,3]>ress1[i,2]){
      sig=c(sig,1)
    }else{
      sig=c(sig,0)
    }
  }
  ress1=cbind(ress1,sig)
  
  output=t2
  for (ij in K:dim(ress1)[1]) {
    if( sum(ress1[((ij-K+1):ij),4]==1)==K  ){
      output=ress1[ij,1]
      break
    }
  }
  return(output)
}

bootstraptestM2=function(t1,t2,K,H,case,dummy,calculateR,bootstraptest1,ma_all){
  ress1 = foreach(jj = t1:t2,.combine='rbind',.packages=c('forecast','dplyr','data.table','stats'))%dopar%{
    index=jj-H
    mah=ma_all[1:jj]
    
    mah_original=mah[1:index]
    cur_val=sum(mah[(jj-H+1):jj])
    
    
    fit=auto.arima(mah_original)
    fit$fitted[fit$fitted<0]=0
    bt=bootstraptest1(H,mah_original,calculateR,fit)
    up=bt$upper
    c(jj,up,cur_val)
  }
  sig=c()
  for (i in 1:dim(ress1)[1]) {
    if(ress1[i,3]>ress1[i,2]){
      sig=c(sig,1)
    }else{
      sig=c(sig,0)
    }
  }
  ress1=cbind(ress1,sig)
  
  output=t2
  for (ij in K:dim(ress1)[1]) {
    if( sum(ress1[((ij-K+1):ij),4]==1)==K  ){
      output=ress1[ij,1]
      break
    }
  }
  return(output)
}


pred_list3=matrix(data=NA, nrow = 4, ncol = 18)
for (K in 1:4) {
  t1=c(34,103,151,246,399,527)
  t2=c(45,110,209,327,468,565)
  timeor=c()
  for (ii in 1:length(t2)) {
    time1=t1[ii]
    time2=t2[ii]
    fit1=bootstraptestOR2(time1,time2,K,H,case,dummy,calculateR,bootstraptest1)
    timeor=c(timeor,(time2-fit1)/(time2-time1))
  }
  pred_list3[K,1:6]=timeor
}
for (K in 1:4) {
  t1=c(34,104,153,246,399,527)
  t2=c(50,115,211,331,474,565)
  timeor=c()
  for (ii in 1:length(t2)) {
    time1=t1[ii]
    time2=t2[ii]
    fit1=bootstraptestMA2(time1,time2,K,H,case,dummy,calculateR,bootstraptest1,mah_all)
    timeor=c(timeor,(time2-fit1)/(time2-time1))
  }
  pred_list3[K,7:12]=timeor
}
for (K in 1:4) {
  t1=c(34,104,153,246,399,527)
  t2=c(50,115,211,331,474,565)
  timeor=c()
  for (ii in 1:length(t2)) {
    time1=t1[ii]
    time2=t2[ii]
    fit1=bootstraptestM2 (time1,time2,K,H,case,dummy,calculateR,bootstraptest1,ma_all)
    timeor=c(timeor,(time2-fit1)/(time2-time1))
  }
  pred_list3[K,13:18]=timeor
}

c1=c(pred_list2[4,1:6])
c2=c(pred_list2[3,7:12])
c3=c(pred_list2[2,13:18])
c(mean(c1),mean(c2),mean(c3))


####################################################################################### Set H=4 and let K vary from 1 to 4
H=4

bootstraptest1=function(H,mah_original,calculateR,fit){
  maxiter=500
  RARC=c()
  for (iter in 1:maxiter) {
    output=c()
    for (number in (index-15):index){
      output=c(output,rpois(1,fit$fitted[number]))
    } 
    output=round(output)
    output[is.na(output)]=0
    
    
    output=sum(output[(length(output)-H+1):length(output)])
    
    
    RARC[iter]=output
  }
  CIARUPPER=numeric()
  CIARLOWER=numeric()
  CIRARUPPER=numeric()
  CIRARLOWER=numeric()
  
  
  RARC=sort(RARC)  
  
  
  
  CIRARUPPER=RARC[length(RARC)*0.99]
  CIRARLOWER=RARC[length(RARC)*0.08]
  
  results = list(lower = CIRARLOWER,upper=CIRARUPPER)
  return (results)
}

bootstraptestOR2=function(t1,t2,K,H,case,dummy,calculateR,bootstraptest1){
  ress1 = foreach(jj = t1:t2,.combine='rbind',.packages=c('forecast','dplyr','data.table','stats'))%dopar%{
    index=jj-H
    mah=case$Case[1:jj]
    
    mah_original=mah[1:index]
    cur_val=sum(mah[(jj-H+1):jj])
    
    
    fit=auto.arima(mah_original)
    fit$fitted[fit$fitted<0]=0
    bt=bootstraptest1(H,mah_original,calculateR,fit)
    up=bt$upper
    c(jj,up,cur_val)
  }
  sig=c()
  for (i in 1:dim(ress1)[1]) {
    if(ress1[i,3]>ress1[i,2]){
      sig=c(sig,1)
    }else{
      sig=c(sig,0)
    }
  }
  ress1=cbind(ress1,sig)
  
  output=t2
  for (ij in K:dim(ress1)[1]) {
    if( sum(ress1[((ij-K+1):ij),4]==1)==K  ){
      output=ress1[ij,1]
      break
    }
  }
  return(output)
}

bootstraptestMA2=function(t1,t2,K,H,case,dummy,calculateR,bootstraptest1,mah_all){
  ress1 = foreach(jj = t1:t2,.combine='rbind',.packages=c('forecast','dplyr','data.table','stats'))%dopar%{
    index=jj-H
    mah=mah_all[1:jj]
    
    mah_original=mah[1:index]
    cur_val=sum(mah[(jj-H+1):jj])
    
    
    fit=auto.arima(mah_original)
    fit$fitted[fit$fitted<0]=0
    bt=bootstraptest1(H,mah_original,calculateR,fit)
    up=bt$upper
    c(jj,up,cur_val)
  }
  sig=c()
  for (i in 1:dim(ress1)[1]) {
    if(ress1[i,3]>ress1[i,2]){
      sig=c(sig,1)
    }else{
      sig=c(sig,0)
    }
  }
  ress1=cbind(ress1,sig)
  
  output=t2
  for (ij in K:dim(ress1)[1]) {
    if( sum(ress1[((ij-K+1):ij),4]==1)==K  ){
      output=ress1[ij,1]
      break
    }
  }
  return(output)
}

bootstraptestM2=function(t1,t2,K,H,case,dummy,calculateR,bootstraptest1,ma_all){
  ress1 = foreach(jj = t1:t2,.combine='rbind',.packages=c('forecast','dplyr','data.table','stats'))%dopar%{
    index=jj-H
    mah=ma_all[1:jj]
    
    mah_original=mah[1:index]
    cur_val=sum(mah[(jj-H+1):jj])
    
    
    fit=auto.arima(mah_original)
    fit$fitted[fit$fitted<0]=0
    bt=bootstraptest1(H,mah_original,calculateR,fit)
    up=bt$upper
    c(jj,up,cur_val)
  }
  sig=c()
  for (i in 1:dim(ress1)[1]) {
    if(ress1[i,3]>ress1[i,2]){
      sig=c(sig,1)
    }else{
      sig=c(sig,0)
    }
  }
  ress1=cbind(ress1,sig)
  
  output=t2
  for (ij in K:dim(ress1)[1]) {
    if( sum(ress1[((ij-K+1):ij),4]==1)==K  ){
      output=ress1[ij,1]
      break
    }
  }
  return(output)
}


pred_list4=matrix(data=NA, nrow = 4, ncol = 18)
for (K in 1:4) {
  t1=c(34,103,151,246,399,527)
  t2=c(45,110,209,327,468,565)
  timeor=c()
  for (ii in 1:length(t2)) {
    time1=t1[ii]
    time2=t2[ii]
    fit1=bootstraptestOR2(time1,time2,K,H,case,dummy,calculateR,bootstraptest1)
    timeor=c(timeor,(time2-fit1)/(time2-time1))
  }
  pred_list4[K,1:6]=timeor
}
for (K in 1:4) {
  t1=c(34,104,153,246,399,527)
  t2=c(50,115,211,331,474,565)
  timeor=c()
  for (ii in 1:length(t2)) {
    time1=t1[ii]
    time2=t2[ii]
    fit1=bootstraptestMA2(time1,time2,K,H,case,dummy,calculateR,bootstraptest1,mah_all)
    timeor=c(timeor,(time2-fit1)/(time2-time1))
  }
  pred_list4[K,7:12]=timeor
}
for (K in 1:4) {
  t1=c(34,104,153,246,399,527)
  t2=c(50,115,211,331,474,565)
  timeor=c()
  for (ii in 1:length(t2)) {
    time1=t1[ii]
    time2=t2[ii]
    fit1=bootstraptestM2 (time1,time2,K,H,case,dummy,calculateR,bootstraptest1,ma_all)
    timeor=c(timeor,(time2-fit1)/(time2-time1))
  }
  pred_list4[K,13:18]=timeor
}


c1=c(pred_list2[4,1:6])
c2=c(pred_list2[2,7:12])
c3=c(pred_list2[2,13:18])
c(mean(c1),mean(c2),mean(c3))