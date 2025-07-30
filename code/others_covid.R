############################################################################## EARSC1
local1=case$Case
date=seq.Date(from = as.Date('2021-04-01'),to=as.Date('2023-2-12'),by='day')
dat=data.frame(date,local1)
baseline=7
in2011 <- c((baseline+1):683)
data <- sts(observed = local1, frequency = 365)
control <- list(range = in2011, method = "C1",baseline=baseline,alpha=0.03) #   0.03 0.045     0.048
surv <- earsC(data, control = control)
signals=c(rep(0,baseline),alarms(surv)[,1]*1)
index=c(c(16:27),c(57:70),c(76:100),c(120:144),c(219:245),c(337:393),c(478:525), c(575:640) )
count=0
for (i in 1:length(index)) {
  if(signals[index[i]]==1){
    count=count+1
  }
}
c(count/length(index))
index1=which(signals==1)
dat1=dat[index1,]
start=c(34,103,151,246,399,527)
end=c(45,110,209,327,468,565)
output=c()
posi=c()
for (i in 1:length(start)) {
  date_start=dat$date[start[i]]
  date_end=dat$date[end[i]]
  val=0
  for (j in 1:dim(dat1)[1]) {
    if( (dat1$date[j]>=date_start) & (dat1$date[j]<=date_end)){
      val=which(dat$date==dat1$date[j])
      break
    }
  }
  if(val==0){
    val=end[i]
  }
  output=c(output,(end[i]-val) /(end[i]-start[i]) )
}
c(output,mean(output))
ec1ns=output
############################################################################## EARSC2
local1=case$Case
date=seq.Date(from = as.Date('2021-04-01'),to=as.Date('2023-2-12'),by='day')
dat=data.frame(date,local1)
baseline=7
in2011 <- c((baseline+3):683)
data <- sts(observed = local1, frequency = 365)
control <- list(range = in2011, method = "C2",baseline=baseline,alpha=0.001) # 0.001 0.03 0.045
surv <- earsC(data, control = control)
signals=c(rep(0,baseline+2),alarms(surv)[,1]*1)
index=c(c(16:27),c(57:70),c(76:100),c(120:144),c(219:245),c(337:393),c(478:525), c(575:640) )

count=0
for (i in 1:length(index)) {
  if(signals[index[i]]==1){
    count=count+1
  }
}
c(count/length(index))
index1=which(signals==1)
dat1=dat[index1,]
start=c(34,103,151,246,399,527)
end=c(45,110,209,327,468,565)
output=c()
posi=c()
for (i in 1:length(start)) {
  date_start=dat$date[start[i]]
  date_end=dat$date[end[i]]
  val=0
  for (j in 1:dim(dat1)[1]) {
    if( (dat1$date[j]>=date_start) & (dat1$date[j]<=date_end)){
      val=which(dat$date==dat1$date[j])
      break
    }
  }
  if(val==0){
    val=end[i]
  }
  output=c(output,(end[i]-val) /(end[i]-start[i]) )
}
c(output,mean(output))
ec2ns=output
############################################################################## EARSC3
local1=case$Case
date=seq.Date(from = as.Date('2021-04-01'),to=as.Date('2023-2-12'),by='day')
dat=data.frame(date,local1)
baseline=7
in2011 <- c((baseline+5):683)
data <- sts(observed = local1, frequency = 365)
control <- list(range = in2011, method = "C3",baseline=baseline,alpha = 0.000000001)# 0.000000001 0.0001 0.05
surv <- earsC(data, control = control)
signals=c(rep(0,baseline+4),alarms(surv)[,1]*1)
signals[is.na(signals)]=0
index=c(c(16:27),c(57:70),c(76:100),c(120:144),c(219:245),c(337:393),c(478:525), c(575:640) )
count=0
for (i in 1:length(index)) {
  if(signals[index[i]]==1){
    count=count+1
  }
}
c(count/length(index))
index1=which(signals==1)
dat1=dat[index1,]
start=c(34,103,151,246,399,527)
end=c(45,110,209,327,468,565)
output=c()
posi=c()
for (i in 1:length(start)) {
  date_start=dat$date[start[i]]
  date_end=dat$date[end[i]]
  val=0
  for (j in 1:dim(dat1)[1]) {
    if( (dat1$date[j]>=date_start) & (dat1$date[j]<=date_end)){
      val=which(dat$date==dat1$date[j])
      break
    }
  }
  if(val==0){
    val=end[i]
  }
  output=c(output,(end[i]-val) /(end[i]-start[i]) )
}
c(output,mean(output))
ec3ns=output
################################################################################
local1=mah_all
date=seq.Date(from = as.Date('2021-04-01'),to=as.Date('2023-2-12'),by='day')
dat=data.frame(date,local1)
baseline=7
in2011 <- c((baseline+1):683)
data <- sts(observed = local1, frequency = 365)
control <- list(range = in2011, method = "C1",baseline=baseline,alpha=0.0025)  # 0.0025 0.015 0.025
surv <- earsC(data, control = control)
signals=c(rep(0,baseline),alarms(surv)[,1]*1)
index=c(c(16:27),c(57:70),c(76:100),c(120:144),c(219:245),c(337:393),c(478:525), c(575:640) )

count=0
for (i in 1:length(index)) {
  if(signals[index[i]]==1){
    count=count+1
  }
}
c(count/length(index))
index1=which(signals==1)
dat1=dat[index1,]
start=c(34,104,153,246,399,527)
end=c(50,115,211,331,474,565)
output=c()
posi=c()
for (i in 1:length(start)) {
  date_start=dat$date[start[i]]
  date_end=dat$date[end[i]]
  val=0
  for (j in 1:dim(dat1)[1]) {
    if( (dat1$date[j]>=date_start) & (dat1$date[j]<=date_end)){
      val=which(dat$date==dat1$date[j])
      break
    }
  }
  if(val==0){
    val=end[i]
  }
  output=c(output,(end[i]-val) /(end[i]-start[i]) )
}
c(output,mean(output))
ec1s=output
############################################################################## EARSC2
local1=mah_all
date=seq.Date(from = as.Date('2021-04-01'),to=as.Date('2023-2-12'),by='day')
dat=data.frame(date,local1)
baseline=7
in2011 <- c((baseline+3):683)
data <- sts(observed = local1, frequency = 365)
control <- list(range = in2011, method = "C2",baseline=baseline,alpha=0.000001) # 0.000001 0.0001 0.02
surv <- earsC(data, control = control)
signals=c(rep(0,baseline+2),alarms(surv)[,1]*1)
index=c(c(16:27),c(57:70),c(76:100),c(120:144),c(219:245),c(337:393),c(478:525), c(575:640) )

count=0
for (i in 1:length(index)) {
  if(signals[index[i]]==1){
    count=count+1
  }
}
c(count/length(index))
index1=which(signals==1)
dat1=dat[index1,]
start=c(34,104,153,246,399,527)
end=c(50,115,211,331,474,565)
output=c()
posi=c()
for (i in 1:length(start)) {
  date_start=dat$date[start[i]]
  date_end=dat$date[end[i]]
  val=0
  for (j in 1:dim(dat1)[1]) {
    if( (dat1$date[j]>=date_start) & (dat1$date[j]<=date_end)){
      val=which(dat$date==dat1$date[j])
      break
    }
  }
  if(val==0){
    val=end[i]
  }
  output=c(output,(end[i]-val) /(end[i]-start[i]) )
}
c(output,mean(output))
ec2s=output
############################################################################## EARSC3
local1=mah_all
date=seq.Date(from = as.Date('2021-04-01'),to=as.Date('2023-2-12'),by='day')
dat=data.frame(date,local1)
baseline=7
in2011 <- c((baseline+5):683)
data <- sts(observed = local1, frequency = 365)
control <- list(range = in2011, method = "C3",baseline=baseline,alpha = 0.000000000000001) # 0.000000000000001 0.00001 0.001
surv <- earsC(data, control = control)
signals=c(rep(0,baseline+4),alarms(surv)[,1]*1)
signals[is.na(signals)]=0
index=c(c(16:27),c(57:70),c(76:100),c(120:144),c(219:245),c(337:393),c(478:525), c(575:640) )
count=0
for (i in 1:length(index)) {
  if(signals[index[i]]==1){
    count=count+1
  }
}
c(count/length(index))
index1=which(signals==1)
dat1=dat[index1,]
start=c(34,104,153,246,399,527)
end=c(50,115,211,331,474,565)
output=c()
posi=c()
for (i in 1:length(start)) {
  date_start=dat$date[start[i]]
  date_end=dat$date[end[i]]
  val=0
  for (j in 1:dim(dat1)[1]) {
    if( (dat1$date[j]>=date_start) & (dat1$date[j]<=date_end)){
      val=which(dat$date==dat1$date[j])
      break
    }
  }
  if(val==0){
    val=end[i]
  }
  output=c(output,(end[i]-val) /(end[i]-start[i]) )
}
c(output,mean(output))
ec3s=output

dat=cbind(ec1ns,ec2ns,ec3ns,ec1s,ec2s,ec3s)
c(colMeans(dat))

############################################################################## NS EPI
local1=case$Case


date=seq.Date(from = as.Date('2021-04-01'),to=as.Date('2023-2-12'),by='day')
dat=data.frame(date,local1)
colnames(dat)=c('date','I')

res <- estimate_R(
  incid = dat,
  method = "parametric_si",
  config = make_config(list(
    mean_si = 3, 
    std_si = 1.4
  ))
)

a=b=c=d=e=c()

for (i in 1:dim(res$R)[1]) {
  mu=res$R$`Mean(R)`[i]
  sigma=res$R$`Std(R)`[i]
  shape <- (mu / sigma)^2
  rate  <- shape / mu
  out=qgamma(c(0.00002,0.00001,0.0002, 0.0005, 0.005), shape = shape, rate = rate)
  a=c(a,out[1])
  b=c(b,out[2])
  c=c(c,out[3])
  d=c(d,out[4])
  e=c(e,out[5])
}
a=c(rep(0,7),a)
b=c(rep(0,7),b)
c=c(rep(0,7),c)
d=c(rep(0,7),d)
e=c(rep(0,7),e)

data=cbind(a,b,c,d,e)

data <- ifelse(data > 1, 1, 0)


fpr=c()
timeliness=list()
for (index2 in 1:dim(data)[2]) {
  signals=data[,index2]
  index=c(c(16:27),c(57:70),c(76:100),c(120:144),c(219:245),c(337:393),c(478:525), c(575:640) )
  
  count=0
  for (i in 1:length(index)) {
    if(signals[index[i]]==1){
      count=count+1
    }
  }
  fpr=c(fpr,count/length(index))
  
  index1=which(signals==1)
  dat1=dat[index1,]
  start=c(34,103,151,246,399,527)
  end=c(45,110,209,327,468,565)
  output=c()
  posi=c()
  for (i in 1:length(start)) {
    date_start=dat$date[start[i]]
    date_end=dat$date[end[i]]
    val=0
    for (j in 1:dim(dat1)[1]) {
      if( (dat1$date[j]>=date_start) & (dat1$date[j]<=date_end)){
        val=which(dat$date==dat1$date[j])
        break
      }
    }
    if(val==0){
      val=end[i]
    }
    output=c(output,(end[i]-val) /(end[i]-start[i]) )
  }
  timeliness[[index2]]=c(output,mean(output))
}
c(fpr)
c(timeliness)

############################################################################## S EPI
local1=mah_all


date=seq.Date(from = as.Date('2021-04-01'),to=as.Date('2023-2-12'),by='day')
dat=data.frame(date,local1)
colnames(dat)=c('date','I')

res <- estimate_R(
  incid = dat,
  method = "parametric_si",
  config = make_config(list(
    mean_si = 3, 
    std_si = 1.4
  ))
)

a=b=c=d=e=c()

for (i in 1:dim(res$R)[1]) {
  mu=res$R$`Mean(R)`[i]
  sigma=res$R$`Std(R)`[i]
  shape <- (mu / sigma)^2
  rate  <- shape / mu
  out=qgamma(seq(0.01,0.05,0.01), shape = shape, rate = rate)
  a=c(a,out[1])
  b=c(b,out[2])
  c=c(c,out[3])
  d=c(d,out[4])
  e=c(e,out[5])
}
a=c(rep(0,7),a)
b=c(rep(0,7),b)
c=c(rep(0,7),c)
d=c(rep(0,7),d)
e=c(rep(0,7),e)

data=cbind(a,b,c,d,e)

data <- ifelse(data > 1, 1, 0)


fpr=c()
timeliness=list()
for (index2 in 1:dim(data)[2]) {
  signals=data[,index2]
  index=c(c(16:27),c(57:70),c(76:100),c(120:144),c(219:245),c(337:393),c(478:525), c(575:640) )
  count=0
  for (i in 1:length(index)) {
    if(signals[index[i]]==1){
      count=count+1
    }
  }
  fpr=c(fpr,count/length(index))
  
  index1=which(signals==1)
  dat1=dat[index1,]
  start=c(34,104,153,246,399,527)
  end=c(50,115,211,331,474,565)
  output=c()
  posi=c()
  for (i in 1:length(start)) {
    date_start=dat$date[start[i]]
    date_end=dat$date[end[i]]
    val=0
    for (j in 1:dim(dat1)[1]) {
      if( (dat1$date[j]>=date_start) & (dat1$date[j]<=date_end)){
        val=which(dat$date==dat1$date[j])
        break
      }
    }
    if(val==0){
      val=end[i]
    }
    output=c(output,(end[i]-val) /(end[i]-start[i]) )
  }
  timeliness[[index2]]=c(output,mean(output))
}
c(fpr)
c(timeliness)