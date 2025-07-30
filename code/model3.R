totaliter=1000
scale1=10
############## EARS C1~C3 with NS and S different fpr levels
ress1 = foreach(jj = 1:totaliter,.combine='rbind',.packages=c('tseries','forecast','dplyr','data.table','stats','nnet','surveillance'))%dopar%{
  label1=c(4,5,6,7,1,2,3)
  label=rep(label1,70)
  for (i in 1:20) {
    label[30*i]=7
  }
  label3=c(1,1,0,0,1,1,1)
  label3=rep(label3,70)
  for (i in 1:20) {
    label3[30*i]=0
  }      
  
  sample=VOC=y=c(5,5,5)
  for (i in 4:330) {
    if(i<201){
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+2
      VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+2
    }else if(i>=201 & i<281){
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+2*0.96^((i-200)/3)
      VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+2*0.96^((i-200)/3)
    }
    else{
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+0.6733841*1.05^((i-280)/3)
      VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+1*1.05^((i-280)/3)
    }
  }
  
  y[y<0]=0.1
  VOC[VOC<0]=0.1
  
  label3=label3[71:330]
  sample_uns=y[71:330]*scale1*0.3
  sample_s=sevenmaho(y[71:330],label3)*scale1*0.3
  
  for (i in 4:330) {
    if(i<201){
      y[i]=y[i]+2/label[i]
      VOC[i]=VOC[i]+2/label[i]
    }else if(i>=201 & i<281){
      y[i]=y[i]+4/label[i]
      VOC[i]=VOC[i]+4/label[i]
    }
    else{
      y[i]=y[i]+1/label[i]
      VOC[i]=VOC[i]+1/label[i]
    }
  }
  
  
  
  
  y=y[71:330]*scale1
  VOC=VOC[71:330]*0.3*scale1*0.2
  sample=sample_uns
  VOC[which(VOC>sample)]=sample[which(VOC>sample)]
  ratio=VOC/sample
  y1=y*ratio
  
  
  VOC=sevenmaho(VOC,label3)
  sample=sample_s
  VOC[which(VOC>sample)]=sample[which(VOC>sample)]
  ratio=VOC/sample
  
  
  y2=sevenmaho(y*ratio,label3)
  
  
  signals=list()
  
  t1=211
  t2=max(212,which.max(y1[211:260])+210)
  
  localc=y1
  baseline=7
  
  in2011 <- c((baseline+1):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C1",baseline=baseline,alpha=0.00007)
  surv <- earsC(data, control = control)
  signals[[1]]=c(rep(0,baseline),alarms(surv)[,1]*1)
  
  in2011 <- c((baseline+3):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C2",baseline=baseline,alpha=0.00015)
  surv <- earsC(data, control = control)
  signals[[2]]=c(rep(0,baseline+2),alarms(surv)[,1]*1)
  
  in2011 <- c((baseline+5):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C3",baseline=baseline,alpha=0.00005)
  surv <- earsC(data, control = control)
  signals[[3]]=c(rep(0,baseline+4),alarms(surv)[,1]*1)
  
  index1=which(signals[[1]][211:260] == 1)[1]+210
  index2=which(signals[[2]][211:260] == 1)[1]+210
  index3=which(signals[[3]][211:260] == 1)[1]+210
  
  time1=(t2-index1)/(t2-t1)
  time2=(t2-index2)/(t2-t1)
  time3=(t2-index3)/(t2-t1)
  
  ############################################################################## 
  t1=211
  t2=max(212,which.max(y2[211:260])+210)
  localc=y2
  baseline=7
  
  in2011 <- c((baseline+1):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C1",baseline=baseline,alpha=0.00019)
  surv <- earsC(data, control = control)
  signals[[4]]=c(rep(0,baseline),alarms(surv)[,1]*1)
  
  in2011 <- c((baseline+3):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C2",baseline=baseline,alpha=0.000000000002)
  surv <- earsC(data, control = control)
  signals[[5]]=c(rep(0,baseline+2),alarms(surv)[,1]*1)
  
  in2011 <- c((baseline+5):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C3",baseline=baseline,alpha=0.00000000000000008) # 0.000000000001 0.03
  surv <- earsC(data, control = control)
  signals[[6]]=c(rep(0,baseline+4),alarms(surv)[,1]*1)
  
  index4=which(signals[[4]][211:260] == 1)[1]+210
  index5=which(signals[[5]][211:260] == 1)[1]+210
  index6=which(signals[[6]][211:260] == 1)[1]+210
  
  time4=(t2-index4)/(t2-t1)
  time5=(t2-index5)/(t2-t1)
  time6=(t2-index6)/(t2-t1)
  
  
  index=c(12:210)
  count=c()
  for (j in 1:6) {
    count1=0
    for (i in 1:length(index)) {
      if(signals[[j]][index[i]]==1){
        count1=count1+1
      }
    }
    count=c(count,count1/length(index))
  }
  
  
  
  c(count,time1,time2,time3,time4,time5,time6)
}
ress2 = foreach(jj = 1:totaliter,.combine='rbind',.packages=c('tseries','forecast','dplyr','data.table','stats','nnet','surveillance'))%dopar%{
  label1=c(4,5,6,7,1,2,3)
  label=rep(label1,70)
  for (i in 1:20) {
    label[30*i]=7
  }
  label3=c(1,1,0,0,1,1,1)
  label3=rep(label3,70)
  for (i in 1:20) {
    label3[30*i]=0
  }      
  
  sample=VOC=y=c(5,5,5)
  for (i in 4:330) {
    if(i<201){
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+2
      VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+2
    }else if(i>=201 & i<281){
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+2*0.96^((i-200)/3)
      VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+2*0.96^((i-200)/3)
    }
    else{
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+0.6733841*1.05^((i-280)/3)
      VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+1*1.05^((i-280)/3)
    }
  }
  
  y[y<0]=0.1
  VOC[VOC<0]=0.1
  
  label3=label3[71:330]
  sample_uns=y[71:330]*scale1*0.3
  sample_s=sevenmaho(y[71:330],label3)*scale1*0.3
  
  for (i in 4:330) {
    if(i<201){
      y[i]=y[i]+2/label[i]
      VOC[i]=VOC[i]+2/label[i]
    }else if(i>=201 & i<281){
      y[i]=y[i]+4/label[i]
      VOC[i]=VOC[i]+4/label[i]
    }
    else{
      y[i]=y[i]+1/label[i]
      VOC[i]=VOC[i]+1/label[i]
    }
  }
  
  
  
  
  y=y[71:330]*scale1
  VOC=VOC[71:330]*0.3*scale1*0.2
  sample=sample_uns
  VOC[which(VOC>sample)]=sample[which(VOC>sample)]
  ratio=VOC/sample
  y1=y*ratio
  
  
  VOC=sevenmaho(VOC,label3)
  sample=sample_s
  VOC[which(VOC>sample)]=sample[which(VOC>sample)]
  ratio=VOC/sample
  
  
  y2=sevenmaho(y*ratio,label3)
  
  
  signals=list()
  
  t1=211
  t2=max(212,which.max(y1[211:260])+210)
  
  localc=y1
  baseline=7
  
  in2011 <- c((baseline+1):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C1",baseline=baseline,alpha=0.001)
  surv <- earsC(data, control = control)
  signals[[1]]=c(rep(0,baseline),alarms(surv)[,1]*1)
  
  in2011 <- c((baseline+3):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C2",baseline=baseline,alpha=0.001)
  surv <- earsC(data, control = control)
  signals[[2]]=c(rep(0,baseline+2),alarms(surv)[,1]*1)
  
  in2011 <- c((baseline+5):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C3",baseline=baseline,alpha=0.0005)
  surv <- earsC(data, control = control)
  signals[[3]]=c(rep(0,baseline+4),alarms(surv)[,1]*1)
  
  index1=which(signals[[1]][211:260] == 1)[1]+210
  index2=which(signals[[2]][211:260] == 1)[1]+210
  index3=which(signals[[3]][211:260] == 1)[1]+210
  
  time1=(t2-index1)/(t2-t1)
  time2=(t2-index2)/(t2-t1)
  time3=(t2-index3)/(t2-t1)
  
  ############################################################################## 
  t1=211
  t2=max(212,which.max(y2[211:260])+210)
  localc=y2
  baseline=7
  
  in2011 <- c((baseline+1):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C1",baseline=baseline,alpha=0.0006)
  surv <- earsC(data, control = control)
  signals[[4]]=c(rep(0,baseline),alarms(surv)[,1]*1)
  
  in2011 <- c((baseline+3):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C2",baseline=baseline,alpha=0.0000000003)
  surv <- earsC(data, control = control)
  signals[[5]]=c(rep(0,baseline+2),alarms(surv)[,1]*1)
  
  in2011 <- c((baseline+5):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C3",baseline=baseline,alpha=0.00000000000000008) # 0.000000000001 0.03
  surv <- earsC(data, control = control)
  signals[[6]]=c(rep(0,baseline+4),alarms(surv)[,1]*1)
  
  index4=which(signals[[4]][211:260] == 1)[1]+210
  index5=which(signals[[5]][211:260] == 1)[1]+210
  index6=which(signals[[6]][211:260] == 1)[1]+210
  
  time4=(t2-index4)/(t2-t1)
  time5=(t2-index5)/(t2-t1)
  time6=(t2-index6)/(t2-t1)
  
  
  index=c(12:210)
  count=c()
  for (j in 1:6) {
    count1=0
    for (i in 1:length(index)) {
      if(signals[[j]][index[i]]==1){
        count1=count1+1
      }
    }
    count=c(count,count1/length(index))
  }
  
  
  
  c(count,time1,time2,time3,time4,time5,time6)
}
ress3 = foreach(jj = 1:totaliter,.combine='rbind',.packages=c('tseries','forecast','dplyr','data.table','stats','nnet','surveillance'))%dopar%{
  label1=c(4,5,6,7,1,2,3)
  label=rep(label1,70)
  for (i in 1:20) {
    label[30*i]=7
  }
  label3=c(1,1,0,0,1,1,1)
  label3=rep(label3,70)
  for (i in 1:20) {
    label3[30*i]=0
  }      
  
  sample=VOC=y=c(5,5,5)
  for (i in 4:330) {
    if(i<201){
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+2
      VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+2
    }else if(i>=201 & i<281){
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+2*0.96^((i-200)/3)
      VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+2*0.96^((i-200)/3)
    }
    else{
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+0.6733841*1.05^((i-280)/3)
      VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+1*1.05^((i-280)/3)
    }
  }
  
  y[y<0]=0.1
  VOC[VOC<0]=0.1
  
  label3=label3[71:330]
  sample_uns=y[71:330]*scale1*0.3
  sample_s=sevenmaho(y[71:330],label3)*scale1*0.3
  
  for (i in 4:330) {
    if(i<201){
      y[i]=y[i]+2/label[i]
      VOC[i]=VOC[i]+2/label[i]
    }else if(i>=201 & i<281){
      y[i]=y[i]+4/label[i]
      VOC[i]=VOC[i]+4/label[i]
    }
    else{
      y[i]=y[i]+1/label[i]
      VOC[i]=VOC[i]+1/label[i]
    }
  }
  
  
  
  
  y=y[71:330]*scale1
  VOC=VOC[71:330]*0.3*scale1*0.2
  sample=sample_uns
  VOC[which(VOC>sample)]=sample[which(VOC>sample)]
  ratio=VOC/sample
  y1=y*ratio
  
  
  VOC=sevenmaho(VOC,label3)
  sample=sample_s
  VOC[which(VOC>sample)]=sample[which(VOC>sample)]
  ratio=VOC/sample
  
  
  y2=sevenmaho(y*ratio,label3)
  
  
  signals=list()
  
  t1=211
  t2=max(212,which.max(y1[211:260])+210)
  
  localc=y1
  baseline=7
  
  in2011 <- c((baseline+1):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C1",baseline=baseline,alpha=0.0075)
  surv <- earsC(data, control = control)
  signals[[1]]=c(rep(0,baseline),alarms(surv)[,1]*1)
  
  in2011 <- c((baseline+3):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C2",baseline=baseline,alpha=0.006)
  surv <- earsC(data, control = control)
  signals[[2]]=c(rep(0,baseline+2),alarms(surv)[,1]*1)
  
  in2011 <- c((baseline+5):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C3",baseline=baseline,alpha=0.005)
  surv <- earsC(data, control = control)
  signals[[3]]=c(rep(0,baseline+4),alarms(surv)[,1]*1)
  
  index1=which(signals[[1]][211:260] == 1)[1]+210
  index2=which(signals[[2]][211:260] == 1)[1]+210
  index3=which(signals[[3]][211:260] == 1)[1]+210
  
  time1=(t2-index1)/(t2-t1)
  time2=(t2-index2)/(t2-t1)
  time3=(t2-index3)/(t2-t1)
  
  ############################################################################## 
  t1=211
  t2=max(212,which.max(y2[211:260])+210)
  localc=y2
  baseline=7
  
  in2011 <- c((baseline+1):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C1",baseline=baseline,alpha=0.002)
  surv <- earsC(data, control = control)
  signals[[4]]=c(rep(0,baseline),alarms(surv)[,1]*1)
  
  in2011 <- c((baseline+3):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C2",baseline=baseline,alpha=0.00000008)
  surv <- earsC(data, control = control)
  signals[[5]]=c(rep(0,baseline+2),alarms(surv)[,1]*1)
  
  in2011 <- c((baseline+5):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C3",baseline=baseline,alpha=0.00000000000000008) # 0.000000000001 0.03
  surv <- earsC(data, control = control)
  signals[[6]]=c(rep(0,baseline+4),alarms(surv)[,1]*1)
  
  index4=which(signals[[4]][211:260] == 1)[1]+210
  index5=which(signals[[5]][211:260] == 1)[1]+210
  index6=which(signals[[6]][211:260] == 1)[1]+210
  
  time4=(t2-index4)/(t2-t1)
  time5=(t2-index5)/(t2-t1)
  time6=(t2-index6)/(t2-t1)
  
  
  index=c(12:210)
  count=c()
  for (j in 1:6) {
    count1=0
    for (i in 1:length(index)) {
      if(signals[[j]][index[i]]==1){
        count1=count1+1
      }
    }
    count=c(count,count1/length(index))
  }
  
  
  
  c(count,time1,time2,time3,time4,time5,time6)
}
############## EPI_NS
ress4 = foreach(jj = 1:totaliter,.combine='rbind',.packages=c('tseries','forecast','dplyr','data.table','stats','nnet','surveillance'))%dopar%{
  label1=c(4,5,6,7,1,2,3)
  label=rep(label1,70)
  for (i in 1:20) {
    label[30*i]=7
  }
  label3=c(1,1,0,0,1,1,1)
  label3=rep(label3,70)
  for (i in 1:20) {
    label3[30*i]=0
  }      
  
  sample=VOC=y=c(5,5,5)
  for (i in 4:330) {
    if(i<201){
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+2
      VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+2
    }else if(i>=201 & i<281){
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+2*0.96^((i-200)/3)
      VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+2*0.96^((i-200)/3)
    }
    else{
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+0.6733841*1.05^((i-280)/3)
      VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+1*1.05^((i-280)/3)
    }
  }
  
  y[y<0]=0.1
  VOC[VOC<0]=0.1
  
  label3=label3[71:330]
  sample_uns=y[71:330]*scale1*0.3
  sample_s=sevenmaho(y[71:330],label3)*scale1*0.3
  
  for (i in 4:330) {
    if(i<201){
      y[i]=y[i]+2/label[i]
      VOC[i]=VOC[i]+2/label[i]
    }else if(i>=201 & i<281){
      y[i]=y[i]+4/label[i]
      VOC[i]=VOC[i]+4/label[i]
    }
    else{
      y[i]=y[i]+1/label[i]
      VOC[i]=VOC[i]+1/label[i]
    }
  }
  
  
  
  
  y=y[71:330]*scale1
  VOC=VOC[71:330]*0.3*scale1*0.2
  sample=sample_uns
  VOC[which(VOC>sample)]=sample[which(VOC>sample)]
  ratio=VOC/sample
  y1=y*ratio
  
  
  VOC=sevenmaho(VOC,label3)
  sample=sample_s
  VOC[which(VOC>sample)]=sample[which(VOC>sample)]
  ratio=VOC/sample
  
  
  y2=sevenmaho(y*ratio,label3)
  
  
  signals=list()
  
  t1=211
  t2=max(212,which.max(y[211:260])+210)
  
  y[y<0]=0
  dat=data.frame(y1)
  colnames(dat)=c('I')
  
  
  res <- estimate_R(
    incid = dat,
    method = "parametric_si",
    config = make_config(list(
      mean_si = 2, 
      std_si = 1
    ))
  )
  
  a=b=c=d=e=f=c()
  
  for (i in 1:dim(res$R)[1]) {
    mu=res$R$`Mean(R)`[i]
    sigma=res$R$`Std(R)`[i]
    shape <- (mu / sigma)^2
    rate  <- shape / mu
    out=qgamma(seq(0.15,0.21,by=0.01), shape = shape, rate = rate)
    a=c(a,out[1])
    b=c(b,out[2])
    c=c(c,out[3])
    d=c(d,out[4])
    e=c(e,out[5])
    f=c(f,out[6])
  }
  a=c(rep(0,7),a)
  b=c(rep(0,7),b)
  c=c(rep(0,7),c)
  d=c(rep(0,7),d)
  e=c(rep(0,7),e)
  f=c(rep(0,7),f)
  
  data=cbind(a,b,c,d,e,f)
  
  data <- ifelse(data > 1, 1, 0)
  signals=list()
  for (ii in 1:dim(data)[2]) {
    signals[[ii]]=data[,ii]
  }
  
  
  
  index1=which(signals[[1]][211:260] == 1)[1]+210
  index2=which(signals[[2]][211:260] == 1)[1]+210
  index3=which(signals[[3]][211:260] == 1)[1]+210
  
  time1=(t2-index1)/(t2-t1)
  time2=(t2-index2)/(t2-t1)
  time3=(t2-index3)/(t2-t1)
  
  
  index4=which(signals[[4]][211:260] == 1)[1]+210
  index5=which(signals[[5]][211:260] == 1)[1]+210
  index6=which(signals[[6]][211:260] == 1)[1]+210
  
  time4=(t2-index4)/(t2-t1)
  time5=(t2-index5)/(t2-t1)
  time6=(t2-index6)/(t2-t1)
  
  
  index=c(12:210)
  count=c()
  for (j in 1:6) {
    count1=0
    for (i in 1:length(index)) {
      if(signals[[j]][index[i]]==1){
        count1=count1+1
      }
    }
    count=c(count,count1/length(index))
  }
  c(count,time1,time2,time3,time4,time5,time6)
}
############## EPI_S
ress5 = foreach(jj = 1:totaliter,.combine='rbind',.packages=c('tseries','forecast','dplyr','data.table','stats','nnet','surveillance'))%dopar%{
  label1=c(4,5,6,7,1,2,3)
  label=rep(label1,70)
  for (i in 1:20) {
    label[30*i]=7
  }
  label3=c(1,1,0,0,1,1,1)
  label3=rep(label3,70)
  for (i in 1:20) {
    label3[30*i]=0
  }      
  
  sample=VOC=y=c(5,5,5)
  for (i in 4:330) {
    if(i<201){
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+2
      VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+2
    }else if(i>=201 & i<281){
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+2*0.96^((i-200)/3)
      VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+2*0.96^((i-200)/3)
    }
    else{
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+0.6733841*1.05^((i-280)/3)
      VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+1*1.05^((i-280)/3)
    }
  }
  
  y[y<0]=0.1
  VOC[VOC<0]=0.1
  
  label3=label3[71:330]
  sample_uns=y[71:330]*scale1*0.3
  sample_s=sevenmaho(y[71:330],label3)*scale1*0.3
  
  for (i in 4:330) {
    if(i<201){
      y[i]=y[i]+2/label[i]
      VOC[i]=VOC[i]+2/label[i]
    }else if(i>=201 & i<281){
      y[i]=y[i]+4/label[i]
      VOC[i]=VOC[i]+4/label[i]
    }
    else{
      y[i]=y[i]+1/label[i]
      VOC[i]=VOC[i]+1/label[i]
    }
  }
  
  
  
  
  y=y[71:330]*scale1
  VOC=VOC[71:330]*0.3*scale1*0.2
  sample=sample_uns
  VOC[which(VOC>sample)]=sample[which(VOC>sample)]
  ratio=VOC/sample
  y1=y*ratio
  
  
  VOC=sevenmaho(VOC,label3)
  sample=sample_s
  VOC[which(VOC>sample)]=sample[which(VOC>sample)]
  ratio=VOC/sample
  
  
  y2=sevenmaho(y*ratio,label3)
  
  
  signals=list()
  
  t1=211
  t2=max(212,which.max(y[211:260])+210)
  
  y[y<0]=0
  dat=data.frame(y2)
  colnames(dat)=c('I')
  
  
  res <- estimate_R(
    incid = dat,
    method = "parametric_si",
    config = make_config(list(
      mean_si = 2, 
      std_si = 1
    ))
  )
  
  a=b=c=d=e=f=c()
  
  for (i in 1:dim(res$R)[1]) {
    mu=res$R$`Mean(R)`[i]
    sigma=res$R$`Std(R)`[i]
    shape <- (mu / sigma)^2
    rate  <- shape / mu
    out=qgamma(seq(0.22,0.27,by=0.01), shape = shape, rate = rate)
    a=c(a,out[1])
    b=c(b,out[2])
    c=c(c,out[3])
    d=c(d,out[4])
    e=c(e,out[5])
    f=c(f,out[6])
  }
  a=c(rep(0,7),a)
  b=c(rep(0,7),b)
  c=c(rep(0,7),c)
  d=c(rep(0,7),d)
  e=c(rep(0,7),e)
  f=c(rep(0,7),f)
  
  data=cbind(a,b,c,d,e,f)
  
  data <- ifelse(data > 1, 1, 0)
  signals=list()
  for (ii in 1:dim(data)[2]) {
    signals[[ii]]=data[,ii]
  }
  
  
  
  index1=which(signals[[1]][211:260] == 1)[1]+210
  index2=which(signals[[2]][211:260] == 1)[1]+210
  index3=which(signals[[3]][211:260] == 1)[1]+210
  
  time1=(t2-index1)/(t2-t1)
  time2=(t2-index2)/(t2-t1)
  time3=(t2-index3)/(t2-t1)
  
  
  index4=which(signals[[4]][211:260] == 1)[1]+210
  index5=which(signals[[5]][211:260] == 1)[1]+210
  index6=which(signals[[6]][211:260] == 1)[1]+210
  
  time4=(t2-index4)/(t2-t1)
  time5=(t2-index5)/(t2-t1)
  time6=(t2-index6)/(t2-t1)
  
  
  index=c(12:210)
  count=c()
  for (j in 1:6) {
    count1=0
    for (i in 1:length(index)) {
      if(signals[[j]][index[i]]==1){
        count1=count1+1
      }
    }
    count=c(count,count1/length(index))
  }
  c(count,time1,time2,time3,time4,time5,time6)
}


ress1[ress1<0]=0
ress1[is.na(ress1)]=0
ress2[ress2<0]=0
ress2[is.na(ress2)]=0
ress3[ress3<0]=0
ress3[is.na(ress3)]=0
ress4[ress4<0]=0
ress4[is.na(ress4)]=0
ress5[ress5<0]=0
ress5[is.na(ress5)]=0

colMeans(ress1)
colMeans(ress2)
colMeans(ress3)
colMeans(ress4)
colMeans(ress5)


##################################################################################################### model 3 for logistic
bootstraptestlogi1=function(t1,t2,val1t,val2t,alpha1){
  ress1 = foreach(rs = t1:t2,.combine='rbind',.packages=c('forecast','dplyr','data.table','stats'))%dopar%{
    qp=0
    val1=val1t[1:rs]
    val2=val2t[1:rs]
    val1[val1<1]=1
    val2[val2<1]=1
    time=c()
    class=c()
    for (ij in 1:length(val1)) {
      time=c(time,rep(ij,val1[ij]))
      time=c(time,rep(ij,val2[ij]))
      class=c(class,rep(1,val1[ij]))
      class=c(class,rep(0,val2[ij]))
    }
    dat=data.frame(time,class)
    fitl=glm(data = dat,class~time,family = 'binomial')
    coef=fitl$coefficients-alpha1*sqrt(diag(vcov(fitl)))
    coef[is.na(coef)]=-1
    coef=coef[2]
    if(coef>0){
      qp=1
    }
    c(qp)
    
  }
  output=sum(ress1[,1])
  return(output)
}
bootstraptestlogi2=function(t1,t2,val1t,val2t,alpha1){
  ress1 = foreach(rs = t1:t2,.combine='rbind',.packages=c('forecast','dplyr','data.table','stats'))%dopar%{
    qp=0
    val1=val1t[1:rs]
    val2=val2t[1:rs]
    val1[val1<1]=1
    val2[val2<1]=1
    time=c()
    class=c()
    for (ij in 1:length(val1)) {
      time=c(time,rep(ij,val1[ij]))
      time=c(time,rep(ij,val2[ij]))
      class=c(class,rep(1,val1[ij]))
      class=c(class,rep(0,val2[ij]))
    }
    dat=data.frame(time,class)
    fitl=glm(data = dat,class~time,family = 'binomial')
    coef=fitl$coefficients-alpha1*sqrt(diag(vcov(fitl)))
    coef[is.na(coef)]=-1
    coef=coef[2]
    if(coef>0){
      qp=1
    }
    c(rs,qp)
    
  }
  output=t2
  for (iq in 1:dim(ress1)[1]) {
    if(ress1[,2][iq]==1){
      output=ress1[,1][iq]
      break
    }
  }
  return(output)
}





totaliter=300
scale1=10
alpha11=seq(4,5,by=0.15) 
alpha22=seq(4,5,by=0.15)

fpr1z=c()# un-smooth
timeliness1z=c() 
fpr2z=c()# smooth
timeliness2z=c()

for (jjj in 1:length(alpha11)) {
  fpr1=c()
  timeliness1=c()
  fpr2=c()
  timeliness2=c()
  alpha1=alpha11[jjj]
  alpha2=alpha22[jjj]
  for (iter in 1:totaliter) {
    label1=c(4,5,6,7,1,2,3)
    label=rep(label1,70)
    for (i in 1:20) {
      label[30*i]=7
    }
    label3=c(1,1,0,0,1,1,1)
    label3=rep(label3,70)
    for (i in 1:20) {
      label3[30*i]=0
    }      
    
    sample=VOC=y=c(5,5,5)
    for (i in 4:330) {
      if(i<201){
        y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+2
        VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+2
      }else if(i>=201 & i<281){
        y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+2*0.96^((i-200)/3)
        VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+2*0.96^((i-200)/3)
      }
      else{
        y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+0.6733841*1.05^((i-280)/3)
        VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+1*1.05^((i-280)/3)
      }
    }
    
    y[y<0]=0.1
    VOC[VOC<0]=0.1
    
    label3=label3[71:330]
    sample_uns=y[71:330]*scale1*0.3
    sample_s=sevenmaho(y[71:330],label3)*scale1*0.3
    
    for (i in 4:330) {
      if(i<201){
        y[i]=y[i]+2/label[i]
        VOC[i]=VOC[i]+2/label[i]
      }else if(i>=201 & i<281){
        y[i]=y[i]+4/label[i]
        VOC[i]=VOC[i]+4/label[i]
      }
      else{
        y[i]=y[i]+1/label[i]
        VOC[i]=VOC[i]+1/label[i]
      }
    }
    
    y=y[71:330]*scale1
    VOC=VOC[71:330]*0.3*scale1*0.2
    sample=sample_uns
    VOC[which(VOC>sample)]=sample[which(VOC>sample)]
    ratio=VOC/sample
    
    ############################################################################## logistic non-smoothing
    ############################################################################## FPR
    val1t=sample*ratio
    val2t=sample-val1t
    t1=12
    t2=210
    out=bootstraptestlogi1(t1,t2,val1t,val2t,alpha1=alpha1)
    fpr1=c(fpr1,out/(t2-t1+1))
    ############################################################################## Timeliness
    t1=211
    t2=max(212,which.max(val1t[211:260])+210)
    out=bootstraptestlogi2(t1,t2,val1t,val2t,alpha1=alpha1)
    out=as.numeric(out)
    timeliness1=c(timeliness1,(t2-out)/(t2-t1))
    ############################################################################## logistic smoothing
    ############################################################################## FPR
    y1=sevenmaho(y,label3)
    sample=sample_s
    VOC=sevenmaho(VOC,label3)
    VOC[which(VOC>sample)]=sample[which(VOC>sample)]
    ratio=VOC/sample
    val1t=sample*ratio
    val2t=sample-val1t
    
    t1=12
    t2=210
    out=bootstraptestlogi1(t1,t2,val1t,val2t,alpha1=alpha2)
    fpr2=c(fpr2,out/(t2-t1+1))
    ############################################################################## Timeliness
    t1=211
    t2=max(212,which.max(val1t[211:260])+210)
    out=bootstraptestlogi2(t1,t2,val1t,val2t,alpha1=alpha2)
    out=as.numeric(out)
    timeliness2=c(timeliness2,(t2-out)/(t2-t1))
  }
  fpr1z=c(fpr1z,mean(fpr1))
  fpr2z=c(fpr2z,mean(fpr2))
  timeliness1z=c(timeliness1z,mean(timeliness1))
  timeliness2z=c(timeliness2z,mean(timeliness2))
}

##################################################################################################### model 3 for MAH and original
totaliter=50
factor1=c(10)

bound_res1=seq(0.31,0.37,by=0.01)  
bound_res2=seq(0.02,0.14,by=0.02)   

fpr_table5=list() # smooth
timeliness_table5=list()


fpr_table6=list()# un-smooth
timeliness_table6=list()


for (iter in 1:length(factor1)) {
  scale1=factor1[iter]
  fpr11=c()
  timeliness11=c()
  fpr22=c()
  timeliness22=c()
  for (zzz in 1:length(bound_res1)) {
    fpr1=c()
    timeliness1=c()
    fpr2=c()
    timeliness2=c()
    
    bound1=bound_res1[zzz]
    bound2=bound_res2[zzz]
    for (jj in 1:totaliter) {
      label1=c(4,5,6,7,1,2,3)
      label=rep(label1,70)
      for (i in 1:20) {
        label[30*i]=7
      }
      label3=c(1,1,0,0,1,1,1)
      label3=rep(label3,70)
      for (i in 1:20) {
        label3[30*i]=0
      }      
      
      sample=VOC=y=c(5,5,5)
      for (i in 4:330) {
        if(i<201){
          y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+2
          VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+2
        }else if(i>=201 & i<281){
          y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+2*0.96^((i-200)/3)
          VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+2*0.96^((i-200)/3)
        }
        else{
          y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=1)+0.6733841*1.05^((i-280)/3)
          VOC[i]=VOC[i-1]*0.55+VOC[i-2]*0.3+rnorm(1,mean=0,sd=1)+1*1.05^((i-280)/3)
        }
      }
      
      y[y<0]=0.1
      VOC[VOC<0]=0.1
      
      label3=label3[71:330]
      sample_uns=y[71:330]*scale1*0.3
      sample_s=sevenmaho(y[71:330],label3)*scale1*0.3
      
      for (i in 4:330) {
        if(i<201){
          y[i]=y[i]+2/label[i]
          VOC[i]=VOC[i]+2/label[i]
        }else if(i>=201 & i<281){
          y[i]=y[i]+4/label[i]
          VOC[i]=VOC[i]+4/label[i]
        }
        else{
          y[i]=y[i]+1/label[i]
          VOC[i]=VOC[i]+1/label[i]
        }
      }
      bootstraptest1=function(H,mah_original,bound,fit1,fit2,samplesize_original){
        maxiter=400
        RARC=c()
        for (iter in 1:maxiter) {
          output=c()
          for (number in (index-15):index){
            output=c(output,rpois(1,fit1$fitted[number])*rpois(1,fit2$fitted[number])/samplesize_original[number] )
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
        
        
        CIRARUPPER=RARC[length(RARC)*(1-bound)]
        CIRARLOWER=RARC[length(RARC)*0.05]
        
        results = list(lower = CIRARLOWER,upper=CIRARUPPER)
        return (results)
      }
      bootstraptestMA2=function(t1,t2,K,H,bootstraptest1,mah_all,bound,VOC,sample){
        ress1 = foreach(jj = t1:t2,.combine='rbind',.packages=c('forecast','dplyr','data.table','stats'))%dopar%{
          index=jj-H
          
          mah=mah_all[1:(jj)]
          
          VOC1=VOC[1:index]
          samplesize_original=sample[1:index]
          mah_original=mah[1:index]
          
          
          fit1=auto.arima(mah_original)
          fit1$fitted[fit1$fitted<0]=0
          fit2=auto.arima(VOC1)
          fit2$fitted[fit2$fitted<0]=0
          
          est=mah_all[1:(jj)]*VOC[1:(jj)]/sample[1:(jj)]
          
          cur_val=sum(est[(jj-H+1):jj])
          
          bt=bootstraptest1(H,mah_original,bound,fit1,fit2,samplesize_original)
          up=bt$upper
          low=bt$lower
          
          
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
        
        output=0
        for (ij in K:dim(ress1)[1]) {
          if( sum(ress1[((ij-K+1):ij),4]==1)==K  ){
            output=output+1
            
          }
        }
        return(output)
      }
      ############################################################################## Un-Smooth
      y=y[71:330]*scale1
      VOC=VOC[71:330]*0.3*scale1*0.2
      sample=sample_uns
      VOC[which(VOC>sample)]=sample[which(VOC>sample)]
      ratio=VOC/sample
      
      
      
      mah_all=y
      time1=12
      time2=210
      H=2
      K=1
      out=bootstraptestMA2(time1,time2,K,H,bootstraptest1,mah_all,bound2,VOC,sample)
      fpr1=c(fpr1,mean(out)/length(c(time1:time2)))
      
      
      bootstraptestMA2=function(t1,t2,K,H,bootstraptest1,mah_all,bound,VOC,sample){
        ress1 = foreach(jj = t1:t2,.combine='rbind',.packages=c('forecast','dplyr','data.table','stats'))%dopar%{
          index=jj-H
          
          mah=mah_all[1:(jj)]
          
          VOC1=VOC[1:index]
          samplesize_original=sample[1:index]
          mah_original=mah[1:index]
          
          
          fit1=auto.arima(mah_original)
          fit1$fitted[fit1$fitted<0]=0
          fit2=auto.arima(VOC1)
          fit2$fitted[fit2$fitted<0]=0
          
          est=mah_all[1:(jj)]*VOC[1:(jj)]/sample[1:(jj)]
          
          cur_val=sum(est[(jj-H+1):jj])
          
          bt=bootstraptest1(H,mah_original,bound,fit1,fit2,samplesize_original)
          up=bt$upper
          low=bt$lower
          
          
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
      time1=211
      time2=max(212,which.max(mah_all[211:260])+210)
      out=bootstraptestMA2(time1,time2,K,H,bootstraptest1,mah_all,bound2,VOC,sample)
      timeliness1=c(timeliness1,(time2-out)/(time2-time1) )
      
      ############################################################################## Smooth
      bootstraptest1=function(H,mah_original,bound,fit1,fit2,samplesize_original){
        maxiter=400
        RARC=c()
        for (iter in 1:maxiter) {
          output=c()
          for (number in (index-15):index){
            output=c(output,rpois(1,fit1$fitted[number])*rpois(1,fit2$fitted[number])/samplesize_original[number] )
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
        
        
        CIRARUPPER=RARC[length(RARC)*(1-bound)]
        CIRARLOWER=RARC[length(RARC)*0.05]
        
        results = list(lower = CIRARLOWER,upper=CIRARUPPER)
        return (results)
      }
      bootstraptestMA2=function(t1,t2,K,H,bootstraptest1,mah_all,bound,VOC,sample){
        ress1 = foreach(jj = t1:t2,.combine='rbind',.packages=c('forecast','dplyr','data.table','stats'))%dopar%{
          index=jj-H
          
          mah=mah_all[1:(jj)]
          
          VOC1=VOC[1:index]
          samplesize_original=sample[1:index]
          mah_original=mah[1:index]
          
          
          fit1=auto.arima(mah_original)
          fit1$fitted[fit1$fitted<0]=0
          fit2=auto.arima(VOC1)
          fit2$fitted[fit2$fitted<0]=0
          
          est=mah_all[1:(jj)]*VOC[1:(jj)]/sample[1:(jj)]
          
          cur_val=sum(est[(jj-H+1):jj])
          
          bt=bootstraptest1(H,mah_original,bound,fit1,fit2,samplesize_original)
          up=bt$upper
          low=bt$lower
          
          
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
        
        output=0
        for (ij in K:dim(ress1)[1]) {
          if( sum(ress1[((ij-K+1):ij),4]==1)==K  ){
            output=output+1
            
          }
        }
        return(output)
      }
      
      y1=sevenmaho(y,label3)
      VOC=sevenmaho(VOC,label3)
      sample=sample_s
      VOC[which(VOC>sample)]=sample[which(VOC>sample)]
      ratio=VOC/sample
      #est2=y1*ratio
      
      
      mah_all=y1
      time1=12
      time2=210
      H=2
      K=1
      out=bootstraptestMA2(time1,time2,K,H,bootstraptest1,mah_all,bound1,VOC,sample)
      fpr2=c(fpr2,mean(out)/length(c(time1:time2)))
      
      
      bootstraptestMA2=function(t1,t2,K,H,bootstraptest1,mah_all,bound,VOC,sample){
        ress1 = foreach(jj = t1:t2,.combine='rbind',.packages=c('forecast','dplyr','data.table','stats'))%dopar%{
          index=jj-H
          
          mah=mah_all[1:(jj)]
          
          VOC1=VOC[1:index]
          samplesize_original=sample[1:index]
          mah_original=mah[1:index]
          
          
          fit1=auto.arima(mah_original)
          fit1$fitted[fit1$fitted<0]=0
          fit2=auto.arima(VOC1)
          fit2$fitted[fit2$fitted<0]=0
          
          est=mah_all[1:(jj)]*VOC[1:(jj)]/sample[1:(jj)]
          
          cur_val=sum(est[(jj-H+1):jj])
          
          bt=bootstraptest1(H,mah_original,bound,fit1,fit2,samplesize_original)
          up=bt$upper
          low=bt$lower
          
          
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
      time1=211
      time2=max(212,which.max(mah_all[211:260])+210)
      out=bootstraptestMA2(time1,time2,K,H,bootstraptest1,mah_all,bound1,VOC,sample)
      timeliness2=c(timeliness2,(time2-out)/(time2-time1) )
      
      
      
      
      
    }
    fpr11=c(fpr11,mean(fpr2))
    timeliness11=c(timeliness11,mean(timeliness2))
    fpr22=c(fpr22,mean(fpr1))
    timeliness22=c(timeliness22,mean(timeliness1))
  }
  fpr_table5[[iter]]=fpr11
  timeliness_table5[[iter]]=timeliness11
  fpr_table6[[iter]]=fpr22
  timeliness_table6[[iter]]=timeliness22
}






