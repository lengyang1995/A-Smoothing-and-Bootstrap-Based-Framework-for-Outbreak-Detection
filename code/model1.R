############## EARS C1~C3 with NS and S different fpr levels
totaliter=1000
ress1 = foreach(jj = 1:totaliter,.combine='rbind',.packages=c('tseries','forecast','dplyr','data.table','stats','nnet','surveillance'))%dopar%{
  label1=c(1,1,1,1,1,0,0)
  label=rep(label1,70)
  y=c(5,5,5)
  for (i in 4:330) {
    if(i<281){
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=0.5)+2
    }else{
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=0.5)+2.5
    }
  }
  y=y[71:330]*40
  label=label[71:330]
  y1=sevenmaho(y,label)
  signals=list()
  
  t1=211
  t2=max(212,which.max(y[211:260])+210)
  
  localc=y
  baseline=7
  in2011 <- c((baseline+1):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C1",baseline=baseline,alpha=0.0008)
  surv <- earsC(data, control = control)
  signals[[1]]=c(rep(0,baseline),alarms(surv)[,1]*1)
  in2011 <- c((baseline+3):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C2",baseline=baseline,alpha=0.000025)
  surv <- earsC(data, control = control)
  signals[[2]]=c(rep(0,baseline+2),alarms(surv)[,1]*1)
  in2011 <- c((baseline+5):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C3",baseline=baseline,alpha=0.00000005)
  surv <- earsC(data, control = control)
  signals[[3]]=c(rep(0,baseline+4),alarms(surv)[,1]*1)
  
  index1=which(signals[[1]][211:260] == 1)[1]+210
  index2=which(signals[[2]][211:260] == 1)[1]+210
  index3=which(signals[[3]][211:260] == 1)[1]+210
  
  time1=(t2-index1)/(t2-t1)
  time2=(t2-index2)/(t2-t1)
  time3=(t2-index3)/(t2-t1)
  
  
  
  t1=211
  t2=max(212,which.max(y1[211:260])+210)
  localc=y1
  baseline=7
  in2011 <- c((baseline+1):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C1",baseline=baseline,alpha=0.0002)
  surv <- earsC(data, control = control)
  signals[[4]]=c(rep(0,baseline),alarms(surv)[,1]*1)
  in2011 <- c((baseline+3):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C2",baseline=baseline,alpha=0.000000000001)
  surv <- earsC(data, control = control)
  signals[[5]]=c(rep(0,baseline+2),alarms(surv)[,1]*1)
  in2011 <- c((baseline+5):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C3",baseline=baseline,alpha=0.00000000000000006)
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
  label1=c(1,1,1,1,1,0,0)
  label=rep(label1,70)
  y=c(5,5,5)
  for (i in 4:330) {
    if(i<281){
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=0.5)+2
    }else{
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=0.5)+2.5
    }
  }
  y=y[71:330]*40
  label=label[71:330]
  y1=sevenmaho(y,label)
  signals=list()
  
  t1=211
  t2=max(212,which.max(y[211:260])+210)
  
  localc=y
  baseline=7
  in2011 <- c((baseline+1):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C1",baseline=baseline,alpha=0.0025)
  surv <- earsC(data, control = control)
  signals[[1]]=c(rep(0,baseline),alarms(surv)[,1]*1)
  in2011 <- c((baseline+3):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C2",baseline=baseline,alpha=0.00015)
  surv <- earsC(data, control = control)
  signals[[2]]=c(rep(0,baseline+2),alarms(surv)[,1]*1)
  in2011 <- c((baseline+5):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C3",baseline=baseline,alpha=0.000002)
  surv <- earsC(data, control = control)
  signals[[3]]=c(rep(0,baseline+4),alarms(surv)[,1]*1)
  
  index1=which(signals[[1]][211:260] == 1)[1]+210
  index2=which(signals[[2]][211:260] == 1)[1]+210
  index3=which(signals[[3]][211:260] == 1)[1]+210
  
  time1=(t2-index1)/(t2-t1)
  time2=(t2-index2)/(t2-t1)
  time3=(t2-index3)/(t2-t1)
  
  
  
  t1=211
  t2=max(212,which.max(y1[211:260])+210)
  localc=y1
  baseline=7
  in2011 <- c((baseline+1):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C1",baseline=baseline,alpha=0.0006)
  surv <- earsC(data, control = control)
  signals[[4]]=c(rep(0,baseline),alarms(surv)[,1]*1)
  in2011 <- c((baseline+3):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C2",baseline=baseline,alpha=0.0000000004)
  surv <- earsC(data, control = control)
  signals[[5]]=c(rep(0,baseline+2),alarms(surv)[,1]*1)
  in2011 <- c((baseline+5):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C3",baseline=baseline,alpha=0.01)
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
  label1=c(1,1,1,1,1,0,0)
  label=rep(label1,70)
  y=c(5,5,5)
  for (i in 4:330) {
    if(i<281){
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=0.5)+2
    }else{
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=0.5)+2.5
    }
  }
  y=y[71:330]*40
  label=label[71:330]
  y1=sevenmaho(y,label)
  signals=list()
  
  t1=211
  t2=max(212,which.max(y[211:260])+210)
  
  localc=y
  baseline=7
  in2011 <- c((baseline+1):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C1",baseline=baseline,alpha=0.0075)
  surv <- earsC(data, control = control)
  signals[[1]]=c(rep(0,baseline),alarms(surv)[,1]*1)
  in2011 <- c((baseline+3):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C2",baseline=baseline,alpha=0.001)
  surv <- earsC(data, control = control)
  signals[[2]]=c(rep(0,baseline+2),alarms(surv)[,1]*1)
  in2011 <- c((baseline+5):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C3",baseline=baseline,alpha=0.00012)
  surv <- earsC(data, control = control)
  signals[[3]]=c(rep(0,baseline+4),alarms(surv)[,1]*1)
  
  index1=which(signals[[1]][211:260] == 1)[1]+210
  index2=which(signals[[2]][211:260] == 1)[1]+210
  index3=which(signals[[3]][211:260] == 1)[1]+210
  
  time1=(t2-index1)/(t2-t1)
  time2=(t2-index2)/(t2-t1)
  time3=(t2-index3)/(t2-t1)
  
  
  
  t1=211
  t2=max(212,which.max(y1[211:260])+210)
  localc=y1
  baseline=7
  in2011 <- c((baseline+1):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C1",baseline=baseline,alpha=0.002)
  surv <- earsC(data, control = control)
  signals[[4]]=c(rep(0,baseline),alarms(surv)[,1]*1)
  in2011 <- c((baseline+3):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C2",baseline=baseline,alpha=0.00000003)
  surv <- earsC(data, control = control)
  signals[[5]]=c(rep(0,baseline+2),alarms(surv)[,1]*1)
  in2011 <- c((baseline+5):length(localc))
  data <- sts(observed = localc, frequency = 365)
  control <- list(range = in2011, method = "C3",baseline=baseline,alpha=0.00000000000000006)
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
ress4= foreach(jj = 1:totaliter,.combine='rbind',.packages=c('tseries','forecast','dplyr','data.table','stats','nnet','surveillance','EpiEstim'))%dopar%{
  label1=c(1,1,1,1,1,0,0)
  label=rep(label1,70)
  y=c(5,5,5)
  for (i in 4:330) {
    if(i<281){
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=0.5)+2
    }else{
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=0.5)+2.5
    }
  }
  y=y[71:330]*40
  label=label[71:330]
  y1=sevenmaho(y,label)
  signals=list()
  
  t1=211
  t2=max(212,which.max(y[211:260])+210)
  
  dat=data.frame(y)
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
    out=qgamma(seq(0.015,0.06,by=0.008), shape = shape, rate = rate)
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
ress5 = foreach(jj = 1:totaliter,.combine='rbind',.packages=c('tseries','forecast','dplyr','data.table','stats','nnet','surveillance','EpiEstim'))%dopar%{
  label1=c(1,1,1,1,1,0,0)
  label=rep(label1,70)
  y=c(5,5,5)
  for (i in 4:330) {
    if(i<281){
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=0.5)+2
    }else{
      y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=0.5)+2.5
    }
  }
  y=y[71:330]*40
  label=label[71:330]
  y1=sevenmaho(y,label)
  signals=list()
  
  t1=211
  t2=max(212,which.max(y[211:260])+210)
  
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
    out=qgamma(seq(0.04,0.10,by=0.01), shape = shape, rate = rate)
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
############################################################################# model 1 for MAH
totaliter=300
factor1=40
bound_res=seq(0.06,0.14,by=0.015)
fpr_table1=list()
timeliness_table1=list()
power_table1=list()
for (iter in 1:length(factor1)) {
  scale1=factor1[iter]
  fpr11=c()
  timeliness11=c()
for (zzz in 1:length(bound_res)) {
  fpr=c()
  timeliness=c()
  bound=bound_res[zzz]
  for (jj in 1:totaliter) {
    bootstraptest1=function(H,mah_original,fit,bound){
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
      
      
      
      CIRARUPPER=RARC[length(RARC)*(1-bound)]
      CIRARLOWER=RARC[length(RARC)*0.08]
      
      results = list(lower = CIRARLOWER,upper=CIRARUPPER)
      return (results)
    }
    bootstraptestMA2=function(t1,t2,K,H,bootstraptest1,mah_all,bound){
      ress1 = foreach(jj = t1:t2,.combine='rbind',.packages=c('forecast','dplyr','data.table','stats'))%dopar%{
        index=jj-H
        mah=mah_all[1:jj]
        
        mah_original=mah[1:index]
        cur_val=sum(mah[(jj-H+1):jj])
        
        
        fit=auto.arima(mah_original)
        fit$fitted[fit$fitted<0]=0
        bt=bootstraptest1(H,mah_original,fit,bound)
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
      
      output=0
      for (ij in K:dim(ress1)[1]) {
        if( sum(ress1[((ij-K+1):ij),4]==1)==K  ){
          output=output+1
          
        }
      }
      return(output)
    }
    
    H=2
    label1=c(1,1,1,1,1,0,0)
    label=rep(label1,70)
    y=c(5,5,5)
    for (i in 4:330) {
      if(i<281){
        y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=0.5)+2
      }else{
        y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=0.5)+2.5
      }
    }
    y=y[71:330]*scale1
    label=label[71:330]
    y1=sevenmaho(y,label)

    
    mah_all=y1
    time1=12
    time2=210
    
    K=1
    out=bootstraptestMA2(time1,time2,K,H,bootstraptest1,mah_all,bound)
    fpr=c(fpr,mean(out)/length(c(time1:time2)))
    
    
    bootstraptestMA2=function(t1,t2,K,H,bootstraptest1,mah_all,bound){
      ress1 = foreach(jj = t1:t2,.combine='rbind',.packages=c('forecast','dplyr','data.table','stats'))%dopar%{
        index=jj-H
        mah=mah_all[1:jj]
        
        mah_original=mah[1:index]
        cur_val=sum(mah[(jj-H+1):jj])
        
        
        fit=auto.arima(mah_original)
        fit$fitted[fit$fitted<0]=0
        bt=bootstraptest1(H,mah_original,fit,bound)
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
    
    time1=211
    time2=max(212,which.max(mah_all[211:260])+210)
    K=1
    out=bootstraptestMA2(time1,time2,K,H,bootstraptest1,mah_all,bound)
    timeliness=c(timeliness,(time2-out)/(time2-time1) )
  }
  fpr11=c(fpr11,mean(fpr))
  timeliness11=c(timeliness11,mean(timeliness))
}
  fpr_table1[[iter]]=fpr11
  timeliness_table1[[iter]]=timeliness11
}
############################################################################# model 1 for original
factor1=40
bound_res=seq(0.0005,0.003,by=0.0005)
fpr_table2=list()
timeliness_table2=list()
power_table2=list()


for (iter in 1:length(factor1)) {
  scale1=factor1[iter]
  fpr11=c()
  timeliness11=c()
  for (zzz in 1:length(bound_res)) {
    fpr=c()
    timeliness=c()
    bound=bound_res[zzz]
    for (jj in 1:totaliter) {
      bootstraptest1=function(H,mah_original,fit,bound){
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
        
        
        
        CIRARUPPER=RARC[length(RARC)*(1-bound)]
        CIRARLOWER=RARC[length(RARC)*0.08]
        
        results = list(lower = CIRARLOWER,upper=CIRARUPPER)
        return (results)
      }
      bootstraptestMA2=function(t1,t2,K,H,bootstraptest1,mah_all,bound){
        ress1 = foreach(jj = t1:t2,.combine='rbind',.packages=c('forecast','dplyr','data.table','stats'))%dopar%{
          index=jj-H
          mah=mah_all[1:jj]
          
          mah_original=mah[1:index]
          cur_val=sum(mah[(jj-H+1):jj])
          
          
          fit=auto.arima(mah_original)
          fit$fitted[fit$fitted<0]=0
          bt=bootstraptest1(H,mah_original,fit,bound)
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
        
        output=0
        for (ij in K:dim(ress1)[1]) {
          if( sum(ress1[((ij-K+1):ij),4]==1)==K  ){
            output=output+1
            
          }
        }
        return(output)
      }
      
      H=2
      label1=c(1,1,1,1,1,0,0)
      label=rep(label1,70)
      y=c(5,5,5)
      for (i in 4:330) {
        if(i<281){
          y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=0.5)+2
        }else{
          y[i]=y[i-1]*0.55+y[i-2]*0.3+rnorm(1,mean=0,sd=0.5)+2.5
        }
      }
      y=y[71:330]*scale1
      label=label[71:330]
      y1=sevenmaho(y,label)
      
      
      mah_all=y
      time1=12
      time2=210
      
      K=1
      out=bootstraptestMA2(time1,time2,K,H,bootstraptest1,mah_all,bound)
      fpr=c(fpr,mean(out)/length(c(time1:time2)))
      
      
      bootstraptestMA2=function(t1,t2,K,H,bootstraptest1,mah_all,bound){
        ress1 = foreach(jj = t1:t2,.combine='rbind',.packages=c('forecast','dplyr','data.table','stats'))%dopar%{
          index=jj-H
          mah=mah_all[1:jj]
          
          mah_original=mah[1:index]
          cur_val=sum(mah[(jj-H+1):jj])
          
          
          fit=auto.arima(mah_original)
          fit$fitted[fit$fitted<0]=0
          bt=bootstraptest1(H,mah_original,fit,bound)
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
      
      time1=211
      time2=max(212,which.max(mah_all[211:260])+210)
      K=1
      out=bootstraptestMA2(time1,time2,K,H,bootstraptest1,mah_all,bound)
      timeliness=c(timeliness,(time2-out)/(time2-time1) )
    }
    fpr11=c(fpr11,mean(fpr))
    timeliness11=c(timeliness11,mean(timeliness))
  }
  fpr_table2[[iter]]=fpr11
  timeliness_table2[[iter]]=timeliness11
}








