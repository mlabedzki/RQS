library(moments)
library(xts)
month = function(x){as.POSIXlt(as.Date(x),format="%d-%m-%Y")$mon+1}
day = function(x){as.POSIXlt(as.Date(x),format="%d-%m-%Y")$mday}
year = function(x){as.POSIXlt(as.Date(x),format="%d-%m-%Y")$year}
wday = function(x){as.POSIXlt(as.Date(x),format="%d-%m-%Y")$wday}
week = function(x){as.POSIXlt(as.Date(x),format="%d-%m-%Y")$week}
prev_biz_day <- function(date,k=1){
  mydate = as.Date(date)-k
  while(wday(mydate)==6 || wday(mydate)==0)
    mydate = mydate-1
  mydate
}
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}
data_folder = "C:/temp/"
download_yahoo <- function(names){
  library(quantmod)
  options("getSymbols.warning4.0"=FALSE)
  options("getSymbols.yahoo.warning"=FALSE)  
  for (i in (1:length(names))){
    sym = getSymbols(names[i],from = '1970-01-01',to = "2021-12-31",warnings = FALSE,auto.assign = TRUE)
    write.zoo(get(sym),paste0(data_folder,names[i],"_d.csv"),row.names=FALSE,sep=",")
  }
}
xtb_load <- function(name,f="d",col=4){
  xtb = read.csv(paste0(data_folder,name,"_",f,".csv"),header=FALSE)
  xtb$V1 = as.Date(xtb$V1,format="%Y.%m.%d")
  xts(xtb[,-(1:2)],xtb[,1])[,col]
}
stooq_load <- function(name,f="d") {
  stooq_path = "https://stooq.com/q/d/l/?s="  
  series = read.csv(paste0(stooq_path,name,"&d1=19710104&d2=20291231&i=",f), header=TRUE)
  series = xts(series[,-1], order.by=as.Date(as.character(series[,1])))
  series[,4]
}
local_loadoc <- function(name,f="d"){
  open = local_load(name,f="d",col=1)
  close = local_load(name,f="d",col=4)
  oc_ret = log(close/open)
  exp(cumsum(oc_ret))
}
local_load <- function(name,f="d",col=4,type="csv") {
  if (name %in% c()) series = read.csv(paste0(data_folder,name,".csv"),header=TRUE)
  else {
    if(f=="")
      series = read.csv(paste0(data_folder,name,".",type),header=TRUE)
    else
      series = read.csv(paste0(data_folder,name,"_",f,".",type),header=TRUE)
  }
  if(type!="mst") series = xts(series[,-1], order.by=as.Date(as.character(series[,1])))
  else series = xts(series[,-(1:2)], order.by=as.Date(as.character(series[,2]),format="%Y%m%d"))
  series[,col]
}
load_indices_loc <- function(names,col=4,f="d",type="csv"){
  for (i in (1:length(names))){
    if(col==4) a=5 else a=8+col
    #if(grepl("_", names[i], fixed=TRUE)) tmp = load_futures(names[i],k=1,col=a)
    #else
    tmp = local_load(names[i],f=f,col=col,type=type)
    #tmpc=coredata(tmp)
    #tmp = filler(tmp)
    if (i==1) index = tmp else index=cbind(index,tmp)
  }
  index = index['1971-01-01/']
  colnames(index) = names
  index
}
load_indices <- function(names){
  for (i in (1:length(names))){
    tmp = stooq_load(names[i])
    #tmpc=coredata(tmp)
    #tmp = filler(tmp)
    if (i==1) index = tmp else index=cbind(index,tmp)
  }
  colnames(index) = names
  index
}
download_stooq <- function(names,f="d"){
  stooq_path = "https://stooq.com/q/d/l/?s="  
  for (i in (1:length(names))){
    if(!(names[i] %in%  c("MX_SXF","^HSCE"))) {
      series = read.csv(paste0(stooq_path,names[i],"&d1=18710101&d2=20291231&i=",f), header=TRUE)
      write.csv(series,paste0(data_folder,names[i],"_d.csv"),row.names=FALSE)
    }
  }
}
download_quandl <- function(names,k=1){
  for (i in (1:length(names))){
    if(!(names[i] %in%  c("^HSCE"))) {
      series = read.csv(paste0('https://www.quandl.com/api/v3/datasets/CHRIS/',names[i],k,'.csv?api_key=GU-B7tBC6aihybbeuJBX'), header=TRUE)
      write.csv(series,paste0(data_folder,names[i],k,".csv"),row.names=FALSE)
    }
  }
}
ohlc2xts <- function(series,col=4) {
  series = xts(series[,-1], order.by=as.Date(as.character(series[,1])))
  series[,col]
}
ts2xts <- function(tmp,format="%Y-%m-%d",col=1){
  xts(as.numeric(as.character(tmp[,1+col])),as.Date(as.character(tmp[,1]),format=format))
}
load_futures_loc <- function(names,k=1,col=4){
  for (i in (1:length(names))){
    if(names[i]=='ni_' | names[i]=='zs_' | names[i]=='ca_' | names[i]=='ah_') {
      if(k==1) name_real = paste0(names[i],'c_f')
      else name_real = paste0(names[i],'3_f')
      tmp = local_load(name_real,col=col)
    }
    else {
      tmp = read.csv(paste0(data_folder,names[i],k,".csv"),header=TRUE)
      tmp = xts(tmp[,-1], order.by=as.Date(as.character(tmp[,1])))[,col]
    }
    #tmpc=coredata(tmp)
    #tmp = filler(tmp)
    if (i==1) index = tmp else index=cbind(index,tmp)
  }
  colnames(index) = names
  index  
}
quandl_apikey = "GU-B7tBC6aihybbeuJBX"
load_futures <- function(names,k=1,col=4){
  for (i in (1:length(names))){
    if(names[i]=='ni_' | names[i]=='zs_' | names[i]=='ca_' | names[i]=='ah_') {
      if(k==1) name_real = paste0(names[i],'c_f')
      else name_real = paste0(names[i],'3_f')
      tmp = local_load(name_real,col=col)
    }
    else {
      tmp = read.csv(paste0('https://www.quandl.com/api/v3/datasets/CHRIS/',names[i],k,'.csv?api_key=',quandl_apikey))
      tmp = ohlc2xts(tmp,col=col)
    }
    #tmpc=coredata(tmp)
    #tmp = filler(tmp)
    if (i==1) index = tmp else index=cbind(index,tmp)
  }
  colnames(index) = names
  index  
}
load_1future <- function(name,series,year,col=4){
  market = unlist(strsplit(name, split="_"))[1]
  code = unlist(strsplit(name, split="_"))[2]
  tmp = read.csv(paste0('https://www.quandl.com/api/v3/datasets/',market,'/',code,series,year,'.csv?api_key=',quandl_apikey))
  ohlc2xts(tmp,col=col)
}
load_rates <- function(rates_names,col_names="",f="d",col=4){
  for (i in (1:length(rates_names))){
    if(rates_names[i]=="null")
      tmp = local_load("null","d")
    else{
      #tmp = getSymbols(rates_names[i], src="FRED", auto.assign=FALSE)
      tmp = read.csv(paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=",rates_names[i]))
      tmp = xts(as.numeric(as.character(tmp[,2])),as.Date(tmp[,1]))
      tmp = tmp['1971-01-01/']
      if(f!="d") tmp = to.monthly(tmp)[,4]
    }
    if (i==1) rates = tmp
    else rates=cbind(rates,tmp)
  }
  if(col_names=="")
    colnames(rates) = rates_names
  else
    colnames(rates) = col_names
  rates/100
}
histdata <- function(name,begin=2010,end=3000,mend=0,path='') {
  #out  = matrix(NA,0,6)
  for(i in begin:end) {
    #if(i<10) ii = paste0("0",i)
    #else ii = i
    tmp = read.table(unz(paste0(path,'HISTDATA_COM_ASCII_',name,'_M1',i,'.zip'),paste0('DAT_ASCII_',name,'_M1_',i,'.csv')), sep=';', header=F)
    if(i==begin) out=tmp
    else out=rbind(out,tmp)
  }
  if(mend>0) for(i in 1:mend) {
    #tmp = read.table(unz(paste0('HISTDATA_COM_ASCII_',name,'_T20180',i,'.zip'),paste0('DAT_ASCII_',name,'_T_20180',i,'.csv')), sep=',', header=F)
    tmp = read.table(unz(paste0(path,'HISTDATA_COM_ASCII_',name,'_M1',end+1,add0(i),'.zip'),paste0('DAT_ASCII_',name,'_M1_',end+1,add0(i),'.csv')), sep=';', header=F)
    out=rbind(out,tmp)
  }  
  out
}
semisd = function(x,na.rm=TRUE){sd(x[which(x<=0)],na.rm=na.rm)}
SR = function(x,b=252,rate=0,overlap=1){#indices_rates_d[,1]) {
  if(!is.null(dim(x)))
    if(dim(x)[2]>1)
      x=std2log(xts(rowMeans(log2std(x),na.rm=TRUE),index(x)))
    avg0 = mean(na20(x)*b-rate,na.rm=TRUE)
    avg = mean(x*b-rate,na.rm=TRUE)
    all = sum(x*b-rate,na.rm=TRUE)/252
    sd0 = (sd(na20(x)-rate/b,na.rm=TRUE)*sqrt(b))
    sd = (sd(x-rate/b,na.rm=TRUE)*sqrt(b))
    sdneg0 = (semisd(na20(x)-rate/b)*sqrt(b))
    nas = sum(is.na(x))/length(x)
    pf = sum(x[which(x>=0)],na.rm=TRUE)/abs(sum(x[which(x<0)],na.rm=TRUE))
    out = round(c(avg0/sd0, avg0/sdneg0, skewness(x*b-rate,na.rm=TRUE), log2std(avg), abs(maxdrawdown(coredata(x),overlap)), nas,pf,avg/sd ),3)
    names(out)=c('SR','Sortino','Skew','Return','MaxDD','NAs%','PF','SR.NA')
    out
}
SRi = function(ind,b=252,rate=0){SR(diff(log(ind)),b,rate)}
SRS = function(r,b=12,period=21) {
  n = length(r)
  set = rep(NA,period)
  for(i in 1:period) {
    set[i] = SR(r[seq(1,n,period)],b)
  }
  mean(set)
} #for overlapping returns
trailSR <- function(ret,b=252,w=252,get=F){
  n=dim(ret)[1]
  out = rep(NA,n)
  for(i in w:n)
    out[i] = SR(ret[(i-(w-1)):(i),],b)[1]
  if(get) out
  else c(mean(out,na.rm=TRUE),sd(out,na.rm=TRUE),min(out,na.rm=TRUE))
}
SMA <- function(x_xts,n){
  if(is.xts(x_xts))
    x = coredata(x_xts)
  else
    x = x_xts  
  if(is.na(x[1])) {
    s = which(is.na(x))
    s = s[length(s)]
    #s = sum(is.na(x))
    y = c(rep(NA,s+n-1),rollmean(x[-(1:s)],n,na.rm=TRUE))
  }
  else 
    y = c(rep(NA,n-1),rollmean(x,n))
  #lag(rollmean(x,n),k=n)
  if(is.xts(x_xts))
    xts(y, order.by=index(x_xts))
  else
    y
}
EMA <- function (x_xts, ratio,begin=TRUE) {
  if(is.xts(x_xts))
    x = coredata(x_xts)
  else
    x = x_xts
  if(is.na(x[1])) {
    s = which(is.na(x))
    s = s[length(s)]
    y = c(rep(NA,s),filter(x[-(1:s)] * ratio, 1 - ratio, "recursive", init = x[s+1]))
  }
  else {
    y = c(filter(x * ratio, 1 - ratio, "recursive", init = x[1]))
  }
  if(begin) y[1:(1/ratio-1)]=NA
  if(is.xts(x_xts))
    xts(y, order.by=index(x_xts))
  else
    y
}
emavolSem <- function(returns_xts,periods=63,pinyear=252,p=1,h=1){
  help = EMA(returns_xts^2,1/periods,FALSE)
  ratio=1/periods
  if(is.xts(returns_xts))
    x = coredata(returns_xts)
  else
    x = returns_xts
  n=length(x)
  y = rep(NA,n)
  
  restict = ifelse(x*p>0,x^2,NA)
  #this is a method of choising first obs regardless of sign
  if(is.na(x[1])) {
    s = which(is.na(x))
    s = s[length(s)]
    s = s+1
  }
  else {
    s=1
  }
  y[1:s] = rep(x[s]^2,s)
  
  for(i in (s+1):n) {
    if(is.na(restict[i])) {
      if(h==1) subs = help[i]
      else subs = y[i-1]
    }
    else
      subs = restict[i]
    y[i] = (ratio * subs) + ((1 - ratio) * y[i-1])
  }
  y=sqrt(y*pinyear)
  if(is.xts(returns_xts))
    xts(y, order.by=index(returns_xts))
  else
    y
}
smavolSem <- function(returns,periods=63,pinyear=252,p=1,h=1){
  returns[which(returns<0)] = 0
  sqrt(SMA(returns^2,periods)*pinyear)
}
emakurt <- function(returns,periods=63) {
  avg = 0#EMA(returns,1/periods)
  vol = sqrt(EMA((returns-avg)^2,1/periods))
  EMA((returns-avg)^4,1/periods)/vol^4
}
emaskew <- function(returns,periods=63) {
  avg = 0#EMA(returns,1/periods)
  vol = sqrt(EMA((returns-avg)^2,1/periods))
  EMA((returns-avg)^3,1/periods)/vol^3
}
smaskew <- function(returns,periods=63) {
  avg = SMA(returns,periods)
  vol = sqrt(SMA((returns-avg)^2,periods))
  SMA((returns-avg)^3,periods)/vol^3
}
emavol <- function(returns,periods=63,pinyear=252,begin=TRUE) {
  avg = 0#EMA(returns,1/periods)
  sqrt(EMA((returns-avg)^2,1/periods,begin=begin)*pinyear)
}
smavol <- function(returns,periods=63,pinyear=252) {
  avg = SMA(returns,periods)
  sqrt(SMA((returns-avg)^2,periods)*pinyear)
}
na20<-function(x){
  x[which(is.na(x))] = 0
  x
}
parabolic <- function(newx,x,y){
  x2 = x^2
  lm1 = lm(y~x+x2)
  newd = as.data.frame(cbind(newx,newx^2))
  colnames(newd) = c("x","x2")
  predict(lm1,newdata=newd)}
finehist <- function(x){
  h<-hist(x, breaks=30, col="red", xlab="The variable", 
          main="Histogram with Normal Curve") 
  xfit<-seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),length=40) 
  yfit<-dnorm(xfit,mean=mean(x,na.rm=TRUE),sd=sd(x,na.rm=TRUE)) 
  yfit <- yfit*diff(h$mids[1:2])*length(x) 
  lines(xfit, yfit, col="blue", lwd=2)}
ecdfplot <- function(x){
  n = sum(!is.na(x))
  x.ordered = sort(x)
  
  # plot the possible values of probability (0 to 1) against the ordered ozone data (sample quantiles of ozone)
  # notice the option type = 's' for plotting the step functions
  plot(x.ordered, (1:n)/n, type = 's', ylim = c(0, 1), xlab = 'Sample Quantiles of The Variable', ylab = '', main = 'Empirical Cumluative Distribution\n The Variable')
  
  # mark the 3rd quartile
  #abline(v = 62.5, h = 0.75)
  
  # add a legend
  #legend(65, 0.7, '3rd Quartile = 63.5', box.lwd = 0)
  
  # add the label on the y-axis
  mtext(text = expression(hat(F)[n](x)), side = 2, line = 2.5)}
argmax <- function(x,y){x[which(y==max(y,na.rm=TRUE))]}
histMCr <- function(history,h=21,n=100000){
  out = rep(NA,n)
  for(j in 1:n) {
    curr = 0
    for(i in 1:h){
      curr = curr + sample(coredata(history),size=1)
    }
    out[j] = curr
  }
  out}
getSampleOnCDF <- function(cdf,n=1000){
  out = rep(NA,n)
  #for (i in 1:n)
  #  out[i]=cdf(runif(1))
  out = cdf(runif(n))
  if(length(is.na(out))>0) out=out[-which(is.na(out))]
  out
}  
demean<-function(x,na.rm=TRUE){x-mean(x,na.rm=na.rm)}
normalize<-function(x,na.rm=TRUE){demean(x,na.rm=na.rm)/sd(x,na.rm=na.rm)}
moments <- function(sample){c(mean(sample,na.rm=TRUE),sd(sample,na.rm=TRUE),skewness(sample,na.rm=TRUE),kurtosis(sample,na.rm=TRUE))}
plot2xts <- function(x,y0) {
  ind = index(x)
  x = coredata(x)
  par(mar = c(5,5,2,5))
  plot(x, type="l")
  y=coredata(y0[paste0(as.Date(ind[1]),'/')])
  par(new = T)
  plot(y, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", main="", col=2)
  #axis(side=4, at = pretty(range(x)))
  myaxis = seq(ind[1], ind[length(ind)], by = 91)
  axis(1, myaxis, format(myaxis, "%b %y"))
  grid()
  #mtext(side = 4, line = 3, 'Number genes selected')
}
plot2 <- function(x,y) {
  par(mar = c(5,5,2,5))
  plot(x, type="l",ylab = deparse(substitute(x)))
  par(new = T)
  plot(y, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", col=2)
  axis(4)#axis(side=4, at = pretty(range(y)))
  #axis(1, 1:length(x), format(myaxis, "%b %y"))
  grid()
  mtext(side = 4, line = 3, deparse(substitute(y)))
}
plot22 <- function(x,y0) {
  par(mar = c(5,5,2,5))
  n=dim(x)[1]
  y=y0[paste0(as.Date(index(x)[1]),'/')][paste0('/',as.Date(index(x)[n]))]
  d=cbindname(x,y)
  plot(coredata(d$x), type="l")
  par(new = T)
  plot(coredata(d$y), type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", main="", col=2)
  axis(side=4, at = pretty(range(d$x)))
  #mtext(side = 4, line = 3, 'Number genes selected')
}
filler_vec <- function(x){
  n = length(x)
  for (i in 2:n) {
    if(is.na(x[i])) x[i] = x[i-1]
  }
  x
}
fillerxts <- function(y,rep=NA){
  x = coredata(y)
  n = dim(x)[1]
  k = dim(x)[2]
  if(is.na(rep)){
    for (i in 2:n) {
      for (j in 1:k) {
        if(is.na(x[i,j])) x[i,j] = x[i-1,j]
      }
    }
  }
  else{
    for (i in 2:n) {
      for (j in 1:k) {
        if(is.na(x[i,j])) x[i,j] = rep[i-1,j]
      }
    }    
  }
  xts(x,index(y))
}
volnorm <- function(series,vtar=0.1,p=1,volpar=63) {
  out=tsmomenh(diff(log(series)),vtar=vtar,p=p,volpar=volpar)
  xts(exp(cumsum(na20(out))),index(out))
}
voldenorm <- function(series,vtar=0.1,p=1,volpar=63,weights=F) {
  vreturns=diff(log(series))
  vols = vreturns
  for(i in 1:dim(vols)[2]) {
    tmp = emavol(vols[,i],volpar)
    vols[,i] = tmp
  }
  for(i in 1:dim(vols)[2]) {
    vols[which(vols[,i]==0),i] = sqrt(rowMeans(vols^2,na.rm=TRUE)[which(vols[,i]==0)])
  }
  if (weights) vols^p/vtar^p
  else {
    out=levret(lag(vols)^p/vtar^p,vreturns)
    xts(exp(cumsum(na20(out))),index(out))
  }
}
splotdiff <- function(signal,control,divisor=NULL,cost=0.16){
  if(is.null(divisor)){
    dane = merge(diff((signal)),lag(control))
    colnames(dane) = c("returns","control")
  }
  else{
    tmp = cbind(control,divisor)
    dane = merge(diff((signal)),lag(tmp),join="left")
    colnames(dane) = c("returns","control","divisor")
    dane = dane[which(dane$divisor>0),]
  }
  newsignal = ifelse(dane$control <= 0, 0, dane$returns)
  changes = ifelse(dane$control <= 0, -1, 1)
  changes = cumsum(na20(changes))
  changes = ifelse(changes == lag(changes,2), -cost/2, 0)
  if(is.null(divisor)){
    cumsum(na20(newsignal+changes))
  }
  else{
    out = merge(signal,cumsum(na20(newsignal+changes)),join="left")[,2]
    fillerxts(out)
  }
}
splotdiff_1 <- function(signal,control,cost=0.16){
  dane = merge(diff((signal)),lag(control))
  colnames(dane) = c("returns","control")
  newsignal = ifelse(dane$control <= 0, -dane$returns, 0)
  changes = ifelse(dane$control <= 0, -1, 1)
  changes = cumsum(na20(changes))
  changes = ifelse(changes == lag(changes,2), -cost/2, 0)
  cumsum(na20(newsignal+changes))
}
splotdiff2 <- function(signal,control,divisor=NULL,cost=0.16){
  if(is.null(divisor)){
    dane = merge(diff((signal)),lag(control))
    colnames(dane) = c("returns","control")
  }
  else{
    tmp = cbind(control,divisor)
    dane = merge(diff((signal)),lag(tmp),join="left")
    colnames(dane) = c("returns","control","divisor")
    dane = dane[which(dane$divisor>0),]
  }
  newsignal = ifelse(dane$control <= 0, -dane$returns, dane$returns)
  changes = ifelse(dane$control <= 0, -1, 1)
  changes = cumsum(na20(changes))
  changes = ifelse(changes == lag(changes,2), -cost/2, 0)
  if(is.null(divisor)){
    cumsum(na20(newsignal+changes))
  }
  else{
    out = merge(signal,cumsum(na20(newsignal+changes)),join="left")[,2]
    fillerxts(out)
  }
}
splotd <- function(signal,control){
  dane = merge(diff(log(signal)),lag(control))
  dane = merge(diff(log(signal)),fillerxts(dane)[,2])
  dane = dane[complete.cases(dane),]
  colnames(dane) = c("returns","control")
  newsignal = ifelse(dane$control <= 0, 0, dane$returns)
  exp(cumsum(na20(newsignal)))
}
splot2 <- function(signal,control){
  dane = merge(diff(log(signal)),lag(control))
  colnames(dane) = c("returns","control")
  newsignal = ifelse(dane$control <= 0, levret(-1,dane$returns), dane$returns)
  exp(cumsum(na20(newsignal)))
}
splot22 <- function(signal,control,size){
  dane = merge(diff(log(signal)),lag(control))
  colnames(dane) = c("returns","control")
  newsignal = ifelse(dane$control > -size & dane$control < size, 0, dane$returns)
  newsignal = ifelse(dane$control < -size, -newsignal, newsignal)
  exp(cumsum(na20(newsignal)))
}
#dodac druga kolumne z oznaczeniami kierunku pozycji?
splot <- function(returns,control,divisor=NULL,cost=0.0000){
  if(is.null(divisor)){
    dane = merge(returns,control,join="left")
    colnames(dane) = c("returns","control")
  }
  else{
    tmp = cbind(control,divisor)
    dane = merge(returns,tmp,join="left")
    colnames(dane) = c("returns","control","divisor")
    dane = dane[which(dane$divisor>0),]
  }
  newsignal = ifelse(dane$control <= 0, levret(-1,dane$returns), dane$returns)
  #exp(cumsum(na20(newsignal)))
  newsignal-cost
}
splotL <- function(returns,control,divisor=NULL,cost=0.0000){
  if(is.null(divisor)){
    dane = merge(returns,control,join="left")
    colnames(dane) = c("returns","control")
  }
  else{
    tmp = cbind(control,divisor)
    dane = merge(returns,tmp,join="left")
    colnames(dane) = c("returns","control","divisor")
    #dane = dane[which(dane$divisor>0),] #this is bad way, it deletes days
    dane$returns[which(dane$divisor>0),] = NA
  }
  newsignal = ifelse(dane$control <= 0, NA, dane$returns-cost)
  #exp(cumsum(na20(newsignal)))
  newsignal
}
splotS <- function(returns,control,divisor=NULL,cost=0.0000){
  if(is.null(divisor)){
    dane = merge(returns,control,join="left")
    colnames(dane) = c("returns","control")
  }
  else{
    tmp = cbind(control,divisor)
    dane = merge(returns,tmp,join="left")
    colnames(dane) = c("returns","control","divisor")
    dane = dane[which(dane$divisor>0),]
  }
  newsignal = ifelse(dane$control <= 0, levret(-1,dane$returns)-cost, NA)
  #exp(cumsum(na20(newsignal)))
  newsignal
}
mat.levret <- function(mat_a,mat_r) {
  for(i in 1:dim(mat_a)[2]) mat_a[,i] = levret(mat_a[,i],mat_r[,i])
  mat_a
}
levret <- function(a,r) {log(1+a*(exp(r)-1))}
tsmomenh <- function(vreturns,vtar=0.15,p=1,volpar=63){
  if(length(volpar)==1) {
    vols = vreturns
    for(i in 1:dim(vols)[2]) {
      tmp = emavol(vols[,i],volpar)
      vols[,i] = tmp
    }
    for(i in 1:dim(vols)[2]) {
      vols[which(vols[,i]==0),i] = sqrt(rowMeans(vols^2,na.rm=TRUE)[which(vols[,i]==0)])
    }
    vols2 = vreturns
    for(i in 1:dim(vols)[2]) {
      tmp = emavol(vreturns[,i]/vols[,i],volpar)^p
      vols2[,i] = tmp
    }    
  }
  else
    vols = volpar
  levret(vtar^p/lag(vols)^p,vreturns)
}
volnorm <- function(series,vtar=0.15,p=1,volpar=63) {
  out=tsmomenh(diff(log(series)),vtar=vtar,p=p,volpar=volpar)
  xts(exp(cumsum(na20(out))),index(out))
}
replogseries <- function(x0,posit=0) {
  maxlev=20
  x = coredata(x0)
  n=length(x)
  balance=numeric(n)
  ret=numeric(n)
  balance[1] = log(x[1])
  a = 1/log(x[1])
  position = numeric(n)
  position[1] = a
  ret[1] = 0
  for (i in (2:n)) {
    ret[i] = x[i]/x[i-1]-1
    if (i<252) std = sd(ret[1:i])
    else std = sd(ret[(i-251):i])
    #balance[i] =  (1-a)*balance[i-1] + a*balance[i-1]*(1+ret[i])
    balance[i] =  balance[i-1]*(1+a*ret[i])
    #ln(r+1) ~~ r - r^2/2 + r^3/3 = r(1 - r/2 + r^2/3) = r(1 + r/2 - vol^2/252/3)
    a = max(-maxlev,min(maxlev,1/log(x[i])))#*(1-ret[i]/2+std^2)))
    position[i] = a
    #if (i<6) a = 1/log(x[i])*(1+ret[i]-std*sqrt(252))
    #else a = 1/log(x[i])*(1+2*ret[i-4]-1.3*std*sqrt(252))
  }
  if(posit==1) xts(position,index(x0))
  else xts(balance,index(x0))
}
repexpseries <- function(x0,posit=0) {
  maxlev=20
  x = coredata(x0)
  n = length(x)
  balance = numeric(n)
  ret = numeric(n)
  dret = numeric(n)
  balance[1] = exp(x[1])
  a = x[1]
  position = numeric(n)
  position[1] = a
  ret[1] = 0
  dret[1] = 0
  for (i in (2:n)) {
    ret[i] = x[i]/x[i-1]-1
    dret[i] = x[i]-x[i-1]
    if (i<252) std = sd(dret[1:i])
    else std = sd(dret[(i-251):i])
    balance[i] =  balance[i-1]*(1+a*ret[i])
    #ln(r+1) ~~ r - r^2/2 + r^3/3 = r(1 - r/2 + r^2/3) = r(1 + r/2 - vol^2/252/3)
    a = max(-maxlev,min(maxlev,x[i]))#*(1+dret[i]/2+std^2)))
    position[i] = a
  }
  if(posit==1) xts(position,index(x0))
  else xts(balance,index(x0))
}
repquicklog<-function(x,k=1,denom=1,l=20,pos=0){#longer version of quicklog
  a = xts(pmax(-l,pmin(l,1/lag(x,k))),index(x))
  #as.numeric(log(x[1]))+cumsum(na20(diff(x,k)*a*ilor(denom)))
  equityline = as.numeric(log(x[1]))+cumsum(na20(diff(x,k)*a*(denom)))
  if(pos==0) equityline
  else a
}
repquickretlog<-function(x,k=1,denom=1,l=20){#longer version of quickretlog
  a = xts(pmax(-l,pmin(l,1/log(lag(x,k)))),index(x))
  as.numeric(log(x[1]))*exp(cumsum(na20(std2log(ilor(denom)*a*diff(x,k)/lag(x,k)))))
}
repquicklog3<-function(xy,xz,zy,carry_xz=0,cost=0.01,k=1,denom=1,l=20,pos=3){#take 3 ccy pairs do quicklog and sum, potentialy get weights
  #example: eurchf, euraud, audchf
  #konto chf=y
  lxy = repquicklog(xy,k=k,l=l,pos=pos) #zyski w y
  lxz = repquicklog(xz,k=k,l=l,pos=pos,denom=zy) #zyski w z, trzeba normalizowac do y przez zy
  lzy = repquicklog(zy,k=k,l=l,pos=pos) #zyski w y
  equityline = -lxy+(lxz+lzy)
  if(pos==0) equityline
  else if(pos==1) cbind(-lxy,lxz,lzy)
  else if(length(carry_xz)==1) cumsum(na20((lxz-lxy)*(diff(xz))))
  else cumsum(na20((lxz-lxy)*(diff(ccys$euraud)+carry_xz*lag(xz)/365)-abs(lxz-lxy)*cost*lag(xz)/365))
}
SReq <- function(equity,b=252){
  SR(diff(log(equity)),b=b)
}
repseries <- function(xy0,x0,y0,posit=0) {
  #example: x = eurchf, y = euraud, xy = audchf
  #docelowo konto w CHF
  maxlev=10000
  maxlev2=20
  x = coredata(x0); y = coredata(y0); xy = coredata(xy0)
  n=length(x)
  z = numeric(n)
  x.balance=numeric(n); y.balance=numeric(n); yb.balance=numeric(n); z.balance=numeric(n)
  x.ret=numeric(n); y.ret=numeric(n)
  z.ret = numeric(n)
  x.balance[1] = log(x[1])
  y.balance[1] = log(y[1])
  yb.balance[1] = y.balance[1]
  z[1] = x.balance[1] - y.balance[1]
  z.balance[1] = exp(z[1])
  a.x = 1/log(x[1])
  a.y = 1/log(y[1])
  a.z = z[1]
  x.ret[1] = 0
  y.ret[1] = 0
  z.ret[1] = 0
  position = matrix(NA,n,3)
  position[1,] = c(a.z*x.balance[1]*a.x/z[1],- a.z*y.balance[1]*a.y/z[1],z.balance[1])
  for (i in (2:n)) {
    x.ret[i] = x[i]/x[i-1]-1 #pozycje sa w CHF
    y.ret[i] = y[i]/y[i-1]-1 #pozycje sa w AUD
    x.balance[i] =  x.balance[i-1] + x.balance[i-1]*a.x*x.ret[i]
    #yb.balance[i] =  yb.balance[i-1] + yb.balance[i-1]*a.y*y.ret[i] #konto w AUD
    y.balance[i] =  y.balance[i-1] + y.balance[i-1]*a.y*y.ret[i]*xy[i]/xy[i-1]
    z[i] = x.balance[i] - y.balance[i]
    #z.ret[i] = z[i]/z[i-1]-1
    #ilo = (x.balance[i] - y.balance[i])/(x.balance[i-1] - y.balance[i-1])-1
    ilo1 = max(-maxlev,min(maxlev,a.z*(x.balance[i-1]*a.x/z[i-1])))
    ilo2 = -max(-maxlev,min(maxlev,a.z*(y.balance[i-1]*a.y/z[i-1])))
    z.balance[i] =  z.balance[i-1]*(1+ilo1*x.ret[i]+ilo2*y.ret[i])
    a.x = max(-maxlev2,min(maxlev2,1/log(x[i])))
    a.y = max(-maxlev2,min(maxlev2,1/log(y[i])))
    a.z = max(-maxlev2,min(maxlev2,z[i]))
    position[i,] = c(ilo1,ilo2,z.balance[i])
  }
  if(posit==1) xts(position,index(x0))
  else xts(z.balance,index(x0))
}
cummean <- function(x) {cumsum(x) / seq_along(x)}
mat.SMA <-function(x_xts,n){
  m = dim(x_xts)[2]  
  for (i in 1:m) {
    if (i>1) smooth = merge(smooth,SMA(x_xts[,i],n))
    else smooth = SMA(x_xts[,i],n)
  }
  colnames(smooth) = colnames(x_xts)
  smooth
}
mat.EMA <-function(x_xts,n){
  m = dim(x_xts)[2]  
  for (i in 1:m) {
    if (i>1) smooth = merge(smooth,EMA(x_xts[,i],n))
    else smooth = EMA(x_xts[,i],n)
  }
  colnames(smooth) = colnames(x_xts)
  smooth
}
ts2avg <- function(x,n,m,cost,carry=0,carrycost=0,e=1) {
  avg1 = SMA(x, n)
  avg2 = SMA(x, m)
  if (length(carry)==1) returns = ilor(x)-1
  else  returns = ilor(x)-1 + carry/252
  position1 = ifelse(x>avg1,1,-1)
  position2 = ifelse(x>avg2,1,-1)
  position = position1*(position1*position2+1)/2
  poschanges = abs(diff(position))/2
  newreturns = lag(position)*returns - carrycost/252 - lag(poschanges)*cost/10000
  directions = lag(position)
  if(e==1) std2log(newreturns)
  else directions  
}
tsavg <- function(x,n,cost,carry=0,carrycost=0,dir="both",e=1) {
  avg1 = SMA((x), n)
  
  if (length(carry)==1) returns = ilor(x)-1
  else  {
    carry = merge(x,carry,join="left")[,-1]
    carry = fillerxts(carry)
    returns = ilor(x)-1 + carry/252
  }
  m = dim(x)[1]
  newreturns = rep(NA,m)
  #s=which(is.na(avg1))
  #s=s[length(s)]+1 #s=1
  s=sum(is.na(avg1))+1
  t=sum(is.na(carry))+1
  s=max(s,t)
  position = ifelse((x)>avg1,1,-1)
  poschanges = abs(diff(position))/2
  newreturns = lag(position)*returns - carrycost/252 - lag(poschanges)*cost/10000
  directions = lag(position)
  if(dir==1 | dir==-1){
    newreturns[which(directions!=dir),] = NA
    #newreturns=newreturns[which(directions==dir),]
  }
  #if(dir=="both") {
  #  if(x[s]>avg1[s]) position = 1 else position = -1 # 1 is long, -1 is short
  #position = 1
  #  for (i in (s+1):m) {
  #    newreturns[i] = position*returns[i] - carrycost/252 #- cost/10000*12/252
  #    if((x[i]>avg1[i] & position==-1) | (x[i]<avg1[i] & position==1)) {
  #      position = -1*position
  #      newreturns[i] = newreturns[i] - cost/10000
  #    }
  #  }
  #}
  if(dir=="both1") {
    if(dir==1) {
      if(x[s]>avg1[s]) position = 1 else position = -1 # 1 is long, -1 is short
      truepos = (position+1)/2
      #position = 1
      for (i in (s+1):length(x)) {
        newreturns[i] = truepos*returns[i]
        if((x[i]>avg1[i] & position==-1) | (x[i]<avg1[i] & position==1)) {
          position = -1*position
          truepos = (position+1)/2
          newreturns[i] = newreturns[i] - cost/10000
        }
      }      
    }
    else {
      if(x[s]>avg1[s]) position = -1 else position = 1 # 1 is long, -1 is short
      truepos = -1*(-1*position+1)/2
      #position = 1
      for (i in (s+1):length(x)) {
        newreturns[i] = truepos*returns[i]
        if((x[i]>avg1[i] & position==-1) | (x[i]<avg1[i] & position==1)) {
          position = -1*position
          truepos = -1*(-1*position+1)/2
          newreturns[i] = newreturns[i] - cost/10000
        }
      }
    }
  }
  #plot(log(EqLine), t="l")
  #xts(newreturns,index(x))
  if(e==1) std2log(newreturns)
  else directions
}
tsmom <- function(x,n,cost,carry,carrycost=0,e=1) {
  returnsN = diff(log(x),lag=n)
  if (length(carry)==1) returns = exp(diff(log(x)))-1
  else  returns = exp(diff(log(x)))-1+ carry/252
  position = ifelse(returnsN>0,1,-1)
  poschanges = abs(diff(position))/2
  newreturns = lag(position)*returns - carrycost/252 - lag(poschanges)*cost/10000
  directions = lag(position)
  #m = dim(x)[1]
  #newreturns = rep(NA,m)
  #s=which(is.na(returnsN))
  #s=s[length(s)]+1 #s=1
  #if(returnsN[s]>0) position = 1 else position = -1 # 1 is long, -1 is short
  #position = 1
  #for (i in (s+1):m) {
  #  newreturns[i] = position*returns[i] - carrycost/12 #- cost/10000*12/252
  #  if((returnsN[i]>0 & position==-1) | (returnsN[i]<0 & position==1)) {
  #    position = -1*position
  #    newreturns[i] = newreturns[i] - cost/10000
  #  }
  #}
  #plot(log(EqLine), t="l")
  #xts(newreturns,index(x))
  if(e==1) std2log(newreturns)
  else directions  
}
tsavgall <- function(prices,n,cost,carry=0,carrycost0=0,dir="both",e=1) {
  m = dim(prices)[2]
  for (i in 1:m) {
    if(length(carrycost0)==1) carrycost = carrycost0
    else carrycost = carrycost0[i]
    if(length(carry)==1) {
      if (i>1) returns = merge(returns,tsavg(prices[,i],n,cost[i],0,carrycost,dir=dir,e=e))
      else returns = tsavg(prices[,i],n,cost[i],0,carrycost,dir=dir,e=e)
    }
    else {
      if (i>1) returns = merge(returns,tsavg(prices[,i],n,cost[i],carry[,i],carrycost,dir=dir,e=e))
      else returns = tsavg(prices[,i],n,cost[i],carry[,i],carrycost,dir=dir,e=e)
    }
  }
  colnames(returns) = colnames(prices)
  returns
  #test = rowMeans(returns,na.rm=TRUE)
  #if (e==0) SR(test,252)
  #else exp(cumsum(c(0,test)))  
}
tsmomall <- function(prices,n,cost,carry=0,carrycost=0,e=0) {
  m = dim(prices)[2]  
  for (i in 1:m) {
    if(length(carry)==1) {
      if (i>1) returns = merge(returns,tsmom(prices[,i],n,cost[i],0,carrycost))
      else returns = tsmom(prices[,i],n,cost[i],0,carrycost)
    }
    else {
      if (i>1) returns = merge(returns,tsmom(prices[,i],n,cost[i],carry[,i],carrycost))
      else returns = tsmom(prices[,i],n,cost[i],carry[,i],carrycost)
    }
  }
  colnames(returns) = colnames(prices)
  returns
  #test = rowMeans(returns,na.rm=TRUE)
  #if (e==0) SR(test,252)
  #else exp(cumsum(c(0,test)))  
}
avg2Filter <- function(x,m,long){
  avgLong=SMA(x,long)
  newx = x-avgLong
  avgShort=SMA(newx,m)
  x-avgLong-avgShort
}
make_vols <- function(prices_d){
  vreturns = exp(diff(log(prices_d)))-1
  vols = vreturns
  for(i in 1:dim(prices_d)[2]) {
    vol = emavol(vreturns[,i],21)
    vol[vol==0] = 0.17
  }
}
histcorr <-function(x,y,m){
  EMA(x*y,1/m)/sqrt(EMA(x^2,1/m))/sqrt(EMA(y^2,1/m))
}
hestcorr <- function(x,m) {
  logret=diff(log(x))
  y = emavol(logret,m)^2
  varret=diff(y)
  EMA(logret*varret,1/m)/sqrt(EMA(logret^2,1/m))/sqrt(EMA(varret^2,1/m))
  #*VIX[,1]
}
histcorrmat <- function(mat,m){
  n = dim(mat)[2]
  out = matrix(NA,n,n)
  for (i in 1:n) {
    for (j in 1:n) {
      out[i,j] = as.numeric(last(histcorr(mat[,i],mat[,j],m)))
    }
  }
  out
}
firstcompleteobs <- function(mat){
  n = dim(mat)[2]
  obs = numeric(n)
  for(i in 1:n){
    s = which(is.na(mat[,i]))
    obs[i] = s[length(s)]+1
  }
  max(obs)
}
breakout <- function(x, ws){
  max_x = runMax(x, ws, min_periods=min(len(x),int(ws/2)))
  min_x = runMin(x, ws, min_periods=min(len(x), int(ws/2)))
  for (i in 1:length(x))
    sig[i]=breakout_one_row(i, x, max_x, min_x)
  sig=EMA(sig, span=int(ws/4.0), min_periods=int(ws/8.0))
  sig
}
breakout_one_row <- function(idx, x, max_x, min_x){
  r_px=x[idx]
  r_min=min_x[idx]
  r_max=max_x[idx]
  4*(r_px - mean(c(r_min, r_max)))/(r_max - r_min)
}
roll_max <- function(a,n) {
  nr <- NROW(a)
  out <- rep(NA,nr)
  m <- 1:nr - n + 1
  n <- (1:nr)[m>0]
  for(i in n)
    out[i] <- max(a[m[i]:i])
  out
}
roll_min <- function(a,n) {
  nr <- NROW(a)
  out <- rep(NA,nr)
  m <- 1:nr - n + 1
  n <- (1:nr)[m>0]
  for(i in n)
    out[i] <- min(a[m[i]:i])
  out
}
tsvolbreak <- function(x,xh,xl,k,cost,rf) {
  avgH = roll_max(xh, k)
  avgL = roll_min(xl, k)
  mid = avgH/2+avgL/2
  str = coredata(2 * ( x - mid ) / ( avgH - avgL ))
  if (length(rf)==1) returns = exp(diff(log(x)))-1
  else  returns = exp(diff(log(x)))-1 - rf/252 #- 0.005/252
  n = length(x)
  newreturns = rep(NA,n)
  s=which(is.na(avgH))
  s=s[length(s)]+1 #s=1
  #if(str[s]>0.75) position = 1 
  #else if(str[s]<-0.75) position = -1 # 1 is long, -1 is short
  #else position = 0
  for (i in (s+3):n) {
    newreturns[i] = str[i-1]*returns[i] - cost/10000 * abs(str[i-1]-str[i-2])
    #if((str[i]>0.25 & position!=1) | (str[i]<-0.25 & position!=-1)) {
    #  position = sign(str[i])
    #  newreturns[i] = newreturns[i] - cost/10000
    #}
    #if((str[i]<0.5 & position==1) | (str[i]>-0.5 & position==-1))
    #  position = 0
  }
  #plot(log(EqLine), t="l")
  #xts(newreturns,index(x))
  newreturns
}
crossvolbreak <- function(x,xh,xl,k) {
  str = x
  for(i in 1:dim(x)[2]) {
    avgH = roll_max(xh[,i], k) #high of higs
    avgL = roll_min(xl[,i], k)
    mid = avgH/2+avgL/2
    str[,i] = ifelse(avgH - avgL>0,2 * ( x[,i] - mid ) / ( avgH - avgL ),0)
  }
  str
}
maxproximity <- function(x,xh,k) {#use better rolldrawdown
  str = x
  for(i in 1:dim(x)[2]) {
    avgH = roll_max(xh[,i], k) #high of higs
    str[,i] = ifelse(avgH - x[,i]>0, log(x[,i]/avgH),0)
  }
  str
}
xvolbreak <- function(names,n) {
  highs = load_indices_loc(names,2)
  lows = load_indices_loc(names,3)
  closes = load_indices_loc(names,4)
  for(i in 1:length(names)) {
    highs[,i] = roll_max(highs[,i],n)
    lows[,i] = roll_min(lows[,i],n)
  }
  mids = highs/2+lows/2
  2*( closes - mids ) / ( highs - lows )
}
rollacorr <- function(x,n,k=1){
  cv=EMA(x*lag(x,k=k),n)
  v1=EMA(x^2,n)
  v2=EMA(lag(x,k=k)^2,n)
  cv/(v1*v2)^(1/2)
}
rollskew <- function(x,n){
  s=EMA(x^3,n)
  v=EMA(x^2,n)
  m=EMA(x,n)
  s/(v-m^2)^(3/2)
}
repdummy <- function(x0,vol0) {
  x = coredata(x0)
  n=length(x)
  balance=numeric(n)
  ret=numeric(n)
  balance[1] = x[1]
  a = 1
  for (i in (2:n)) {
    ret[i] = x[i]/x[i-1]-1
    balance[i] =  balance[i-1]*(1+a*ret[i])
    a = 2-5*vol0[i]
  }
  xts(balance,index(x0))
}
reprecipseries_new <- function(x0,orig=1,valrate0=1) {
  #x0 is xxxyyy ccy pair
  #outputs are denominated in YYY when orig=1 and in XXX when orig!=1
  x = coredata(x0)
  n=length(x)
  if(length(valrate0)==1) {valrate=rep(1,n)}
  else valrate=coredata(valrate0)
  balance=numeric(n)
  d=numeric(n)
  balance[1] = 1/x[1]
  a = - 1/x[1]
  if(orig==1) b=1
  else b = 1/x[1]
  for (i in (2:n)) {
    d[i] = x[i]-x[i-1]
    if(orig==1) b=1
    else b = 1/x[i] #if we wanted version denominated in ccy second in reciprocal
    balance[i] =  balance[i-1] + valrate[i]* b * a * d[i]
    a = - 1/x[i]
  }
  xts(balance,index(x0))
}
reprecipseries <- function(x0,valrate0=1) {
  # bazowa wersja kopiuje zachowanie 1/x bez zwracania uwagi na denominator
  # x0 to kurs XXXYYY. 1/x0 to kurs YYYXXX
  # valrate to kurs YYYUSD
  x = coredata(x0)
  n=length(x)
  if(length(valrate0)==1) {valrate=rep(1,n)}
  else valrate=coredata(valrate0)
  balance=numeric(n)
  ret=numeric(n)
  balance[1] = 1/(x[1])
  for (i in (2:n)) {
    ret[i] = x[i]/x[i-1]-1
    balance[i] =  balance[i-1] - valrate[i]*(balance[i-1]/valrate[i-1]*ret[i])
  }
  xts(balance,index(x0))
}
printSRs <- function(fut,f=252){
  for(i in 1:dim(fut)[2]) print(c(colnames(fut)[i],SR(diff(log(fut[,i])),f)))
}
SRb <- function(series,b=12) {
  x = diff(log(series))
  round(c(sign(mean(sign(series),na.rm=TRUE))*abs(mean(x*b,na.rm=TRUE))/(sd(x,na.rm=TRUE)*sqrt(b)), skewness(x*b,na.rm=TRUE), mean(x*b,na.rm=TRUE)),3)
}
SRc <- function(x,per,fin_cost=0,cost=0){
  y = ifelse(x == 0, NA, x) - fin_cost/per
  SR(y,per)
}
SRdl = function(x0,b=12) {
  x = diff(log(x0))
  sign(as.numeric(last(x0)))*abs(round(c(mean(x*b,na.rm=TRUE)/(sd(x,na.rm=TRUE)*sqrt(b)), skewness(x*b,na.rm=TRUE), mean(x*b,na.rm=TRUE)),3))
}
w2l = function(x,k=1){
  y = diff(x,k)
  abs(sum(pmax(y,0),na.rm=TRUE)/sum(pmin(y,0),na.rm=TRUE))-1
}
mat.fnna<-function(mat){
  k=dim(mat)[2]
  out = rep(NA,k)
  for(i in 1:k) {
    out[i]=fnna(mat[,i])
  }
  out
}
fnna<-function(x){
  if(is.na(x[1])) {
    s = which(is.na(x))
    s = s[length(s)]
    as.numeric(x[s+1])
  }
  else as.numeric(x[1])
}
triangle2 <- function(xy0,zy0,zx0,valrate0=1) {
  #valrate should be is xxxusd
  #test=first*exp(cumsum(na20(std2log(ilor(ccys$euraud)-1 - ilor(1/ccys$audchf)*(ilor(ccys$eurchf)-1)))))
  #from 2 points on triangle we are replicating ccy pair 1/xy
  xy = coredata(xy0); zy = coredata(zy0); zx = coredata(zx0)
  n=length(xy)
  if(length(valrate0)==1) {valrate=rep(1,n)}
  else valrate=coredata(valrate0)
  balance.x=numeric(n);balance.usd=numeric(n);retA=numeric(n);retB=numeric(n)
  balance.x[1] = 1/xy[1]
  balance.usd[1] = balance.x[1]
  for (i in (2:n)) {
    retA[i] = zx[i]/zx[i-1]-1
    retB[i] = zy[i]/zy[i-1]-1
    balance.x[i] =  balance.x[i-1] + balance.x[i-1]/zx[i-1]*(zx[i]-zx[i-1]) - balance.x[i-1]/(1/xy[i-1])/zy[i-1]*(zy[i]-zy[i-1])*(1/xy[i])  #konto w walucie XXX
    #balance.x[i] =  balance.x[i-1] + balance.x[i-1]/zx[i-1]*(zx[i]-zx[i-1])*(xy[i])/(xy[i-1]) - balance.x[i-1]/zy[i-1]*(zy[i]-zy[i-1])  #konto w walucie YYY
    #wchodzimy nastepnie w transakcje 1 na kwote balance0[i-1] w walucie XXX i transakcje 2 na kwote balance0[i-1]/(1/xy[i-1]) w walucie XXX
    balance.usd[i] =  balance.usd[i-1] + (zx[i]-zx[i-1])*balance.x[i-1]/zx[i-1]*valrate[i] - (zy[i]-zy[i-1])*balance.x[i-1]/(1/xy[i-1])/zy[i-1]*(1/xy[i])*valrate[i] #konto dolarowe, konwersja uzyciem valrate=XXXUSD
    #balance.usd[i] =  balance.usd[i-1] + (zx[i]-zx[i-1])*balance.x[i-1]/zx[i-1]*valrate[i]*(xy[i])/(xy[i-1]) - (zy[i]-zy[i-1])*balance.x[i-1]/zy[i-1]*valrate[i] #konto dolarowe, konwersja uzyciem valrate=YYYUSD
    
    #balance[i] =  balance[i-1] + valrate[i]*(balance0[i]-balance0[i-1]) #konto dolarowe
    #balance0[i] =  balance0[i-1] + 1*balance[i-1]*(retA[i] - (1/xy[i])/(1/xy[i-1])*retB[i])
    #balance[i] =  balance[i-1] + valrate[i]*balance0[i-1]*(retA[i] - (1/xy[i])/(1/xy[i-1])*retB[i])
    #balance[i] =  balance[i-1] + valrate[i]*balance0[i-1]*retA[i] - valrate[i]*balance0[i-1]*(1/xy[i])/(1/xy[i-1])*retB[i]
    
  }
  xts(balance.usd,index(xy0))    
}
reciparb <- function(xy0,zy0,zx0,valrate0=1,posit=0){
  #valrate should be is xxxusd
  #we are replicating reciprocal two times, first time from 2 points on triangle and second time from original x^-1
  xy = coredata(xy0); zy = coredata(zy0); zx = coredata(zx0)
  n=length(xy)
  if(length(valrate0)==1) {valrate=rep(1,n)}
  else valrate=coredata(valrate0)
  balance.usd2=numeric(n);balance.x=numeric(n);balance.usd=numeric(n);retA=numeric(n);retB=numeric(n)
  balance.x[1] = 1/xy[1]
  balance.usd[1] = balance.x[1]
  balance.usd2[1] = balance.x[1]
  a = - 1/xy[1]
  position = matrix(NA,n,3)
  position[1,] = c(1,1,1)
  for (i in (2:n)) {
    retA[i] = zx[i]/zx[i-1]-1
    retB[i] = zy[i]/zy[i-1]-1
    balance.x[i] =  balance.x[i-1] + balance.x[i-1]/zx[i-1]*(zx[i]-zx[i-1]) - balance.x[i-1]/(1/xy[i-1])/zy[i-1]*(zy[i]-zy[i-1])*(1/xy[i])  #konto w walucie XXX
    #wchodzimy nastepnie w transakcje 1 na kwote balance0[i-1] w walucie XXX i transakcje 2 na kwote balance0[i-1]/(1/xy[i-1]) w walucie XXX
    balance.usd[i] =  balance.usd[i-1] + (zx[i]-zx[i-1])*balance.x[i-1]/zx[i-1]*valrate[i] - (zy[i]-zy[i-1])*balance.x[i-1]/(1/xy[i-1])/zy[i-1]*(1/xy[i])*valrate[i] #konto dolarowe
    
    balance.usd2[i] =  balance.usd2[i-1] + valrate[i]/xy[i] * a * (xy[i]-xy[i-1])
    a = - 1/xy[i] #because we wanted version denominated in ccy second in reciprocal (so currency xxx)
    position[i,] = c(1,1,1)
  }
  if(posit==1) xts(position,index(xy0))
  else xts(balance.usd-balance.usd2,index(xy0))
}
fstart <- function(x){x-as.numeric(x[1])}
ilor<-function(x){if(length(x)>1) exp(diff(log(x))) else x} # x[i]/x[i-1]
std2log<-function(x){log(x+1)}
log2std<-function(x){exp(x)-1}
symsqrt <- function(x) {ifelse(x < 0, -sqrt(-x), sqrt(x))}
symsq <- function(x) {ifelse(x < 0, -sq(-x), sq(x))}
sq<-function(x){x^2}
lin<-function(x){x}
quickmerge<-function(x,y,s=0){exp(cumsum(na20(std2log(ilor(s+x)-1+ilor(s+y)-1))))}
quickmerge1<-function(x,s=1){exp(cumsum(na20(std2log(ilor(s+x)-1))))}
quickrecip<-function(x,denom=1){as.numeric(1/(x[1]))+cumsum(na20(diff(x)*-1/lag(x)^2*ilor(denom)))}
quickrecip2<-function(x,denom=1){as.numeric(1/(x[1]))+cumsum(na20(diff(x)*-1/lag(x)^2*denom))/as.numeric(denom[1])}
quickrecip3<-function(x){exp(cumsum(na20(std2log(-ilor(x)+1))))}
quickrecip4<-function(x,xo,o=0){
  #returns=?
  exp(cumsum(na20(std2log(returns))))
}
quickexp<-function(x,denom=1){as.numeric(exp(x[1]))+cumsum(na20(diff(x)*exp(lag(x))*ilor(denom)))}
quicklog<-function(x,k=1,denom=1){as.numeric(log(x[1]))+cumsum(na20(diff(x,k)*1/lag(x,k)*ilor(denom)))}
quicklog2<-function(x,denom=1){as.numeric(log(x[1]))+cumsum(na20(diff(x)*1/lag(x)*denom))/as.numeric(denom[1])}
quicklog3<-function(x){as.numeric(log(x[1]))*exp(cumsum(na20(std2log((ilor(x)-1)*1/lag(log(x))))))}
quicretklog<-function(x,k=1,denom=1){as.numeric(log(x[1]))*exp(cumsum(na20(std2log(1/log(lag(x,k))*diff(x,k)/lag(x,k)))))}
quickvs<-function(x,k=1,denom=1){10+cumsum(na20(10^4*1/k*diff(x,k)*diff(1/lag(x,k),k)*ilor(denom)))/fnna(x)}
quickvs<-function(x,k=1,denom=1){10+cumsum(na20(fnna(x)^2*1/k*diff(x,k)*diff(1/lag(x,k),k)*ilor(denom)))/fnna(x)}
quickvs0<-function(x,k=1,denom=1){cumsum(na20(fnna(x)^2*1/k*diff(x,k)*diff(1/lag(x,k),k)*ilor(denom)))}
quickvsloop<-function(x,k=1,denom=1){
  weights = diff(1/lag(x,1),k)
  for(i in 2:k) weights = weights + diff(1/lag(x,i),k)
  center = diff(x)*weights/k
  10+cumsum(na20(fnna(x)^2*center*ilor(denom)))/fnna(x)
}
vvolind0<-function(x,r=0.05){
  vol = emavol(diff(log(x)),63)
  ifelse(diff(vol) > r, vol,0)
}
vvolind<-function(x,r=0.05){
  vol = emavol(diff(log(x)),63)
  dvol = diff(vol)
  n=dim(x)[1]
  out = numeric(n)
  last = 0
  #out = ifelse(diff(vol) > r, as.numeric(vol), 0)
  #last = ifelse(diff(vol) > r, as.numeric(lag(vol)), NA)
  #last = fillerxts(last)
  #out = ifelse(vol > last, as.numeric(vol), out)
  out[1:63]=NA
  for(i in 64:n) {
    if(dvol[i] > r) {
      out[i] = as.numeric(vol[i])
      last = as.numeric(vol[i-1])
    }
    if(vol[i] > last )
      out[i] = as.numeric(vol[i])
    else
      out[i] = 0
  }
  xts(out,index(x))
}
#library(lubridate)
dorollvix0<-function(bb){
  rollvix = bb[,1]
  for (i in 2:dim(bb)[1]) {
    rollvix[i] = as.numeric(rollvix[i-1]) - (as.numeric(bb[i,1])- as.numeric(bb[i-1,1]))
    if(weekdays(as.Date(paste(year(index(rollvix)[i]),month(index(rollvix)[i]),6,sep = "-"))) == "Saturday"){
      if(as.POSIXlt(as.Date(index(rollvix)[i]),format="%d-%m-%Y")$mday==8)
        rollvix[i] = rollvix[i] + bb[i,2]-bb[i,1]
    }
    else if(weekdays(as.Date(paste(year(index(rollvix)[i]),month(index(rollvix)[i]),6,sep = "-"))) == "Sunday"){
      if(as.POSIXlt(as.Date(index(rollvix)[i]),format="%d-%m-%Y")$mday==7)
        rollvix[i] = rollvix[i] + bb[i,2]-bb[i,1]
    }
    else {
      if(as.POSIXlt(as.Date(index(rollvix)[i]),format="%d-%m-%Y")$mday==6)
        rollvix[i] = rollvix[i] + bb[i,2]-bb[i,1]
    }
  }
  rollvix
}
dorollvix<-function(bb,dates,cost=0.055,dir=1){
  n=dim(bb)[1]
  rollvix = bb[,1]
  rollvix[1] = 0 #as.numeric(bb[1,1])
  for (i in 2:n) {
    rollvix[i] = as.numeric(rollvix[i-1]) + dir*(as.numeric(bb[i,1])- as.numeric(bb[i-1,1]))
    if(index(rollvix)[i-1] %in% as.Date(dates))
      rollvix[i] = rollvix[i] - dir*(as.numeric(bb[i-1,2])-as.numeric(bb[i-1,1])) - cost
  }
  xts(rollvix,index(bb))
}
dorollvixr<-function(bb,dates,cost=0.055,dir=1){
  n=dim(bb)[1]
  rollvix = bb[,1]
  rollvix[1] = 1 #as.numeric(bb[1,1])
  for (i in 2:n) {
    rollvix[i] = as.numeric(rollvix[i-1]) * (1 + dir*(as.numeric(bb[i,1])/as.numeric(bb[i-1,1])-1))
    if(index(rollvix)[i-1] %in% as.Date(dates))
      rollvix[i] = rollvix[i] * (1 - dir*(as.numeric(bb[i-1,2])/as.numeric(bb[i-1,1])-1) - cost)
  }
  xts(rollvix,index(bb))
}
newrollvix<-function(bb,date=10,cost=0.0,dir=1){
  if(dir==1) rollvix = bb[,1]
  else rollvix = cumsum(na20(-diff(bb[,1])))+as.numeric(bb[1,1])
  if(index(rollvix)[i-1] %in% as.Date(dates))
      rollvix[i] = rollvix[i] - dir*(as.numeric(bb[i,2])-as.numeric(bb[i,1])) - cost
  ind = c(NA,index(bb)[-n])
  xts(rollvix,index(bb))
}
rollxtb<-function(fut,dates,spreads,cost=0.055,dir=1){
  n=dim(fut)[1]
  rollvix = fut
  rollvix[1] = 0 #as.numeric(bb[1,1])
  for (i in 2:n) {
    rollvix[i] = as.numeric(rollvix[i-1]) + dir*(as.numeric(fut[i])- as.numeric(fut[i-1]))
    if(index(rollvix)[i-1] %in% as.Date(dates)) {
      k = which(as.Date(dates)==index(rollvix)[i-1])
      rollvix[i] = rollvix[i] - dir*(spreads[k]/100) - cost
    }
  }
  xts(rollvix,index(fut))
}
allfutures<-function(fut_names,fut_dates,fut_d,fut_d2,fut_costs,fut_dirs){
  for(i in 1:length(fut_names)){
    dates = unlist(strsplit(fut_dates[fut_names[i],], split=";"))
    bb = cbind(fut_d[,fut_names[i]],fut_d2[,fut_names[i]])
    bb = bb[complete.cases(bb),]
    tt=dorollvix(bb,dates[-1],cost=as.numeric(fut_costs[i]),dir=fut_dirs[i])
    if (i==1) index = tt else index=cbind(index,tt)
  }
  #index = index['1971-01-01/']
  colnames(index) = fut_names
  index
}
futaddrate <- function(prices,rates_m){
  prices_m = mat.to.monthly(prices)
  for(i in 1:dim(prices)[2]){
    tt = cumsum(na20(rates_m/12*lag(abs(prices_m[,i]))*1))
    tt = dailization(tt,prices[,1])
    if(i==1) index=tt else index=cbind(index,tt)
  }
  colnames(index)=colnames(prices)
  prices+index
}
add_swap_points<-function(ccys,carry,n=12){
  futccy <- diff(log(ccys)) + carry/n #- indices_rates_d*2.4/252-indices_spreads*12/252
  for(i in 1:dim(ccys)[2]) futccy[,i] <- xts(exp(cumsum(na20(futccy[,i]))),index(ccys))
  for(i in 1:dim(ccys)[2]) futccy[which(futccy[,i]==1),i] <- NA
  futccy
}
add_daily_swaps<-function(prices,carry){
  add_swap_points(prices,carry,n=252)
}
monthly2daily<-function(rates,dailyxts){
  rates_d = lag(rates)
  rates_d = merge(dailyxts[,1], xts(coredata(rates_d), as.Date(index(rates_d))-1), join="left")[,-1]
  fillerxts(rates_d)
}
splot2xy <- function(signal1,signal2,control){
  dane = merge(log2std(signal1),log2std(signal2),lag(control))
  colnames(dane) = c("returns1","returns2","control")
  newsignal = ifelse(dane$control <= 0, 2*dane$returns1, dane$returns1+dane$returns2)
  std2log(newsignal)
}
rowQuantiles <- function(X,k=3) {
  n = dim(X)[1]
  y = numeric(n)
  for (i in 1:n) {
    y[i] = quantile(X[i,],na.rm=TRUE)[k]
  }
  y
}
rowSD <- function(X) {
  n = dim(X)[1]
  y = numeric(n)
  for (i in 1:n) {
    y[i] = sd(X[i,],na.rm=TRUE)
  }
  y
}
rolldates0<-function(code,letters=0){
  if(length(letters)==1) letters=c("F","G","H","J","K","M","N","Q","U","V","X","Z")
  dates = numeric(length(letters)*(2017-2005))
  k=1
  for(i in 2006:2017)
    for(j in 1:length(letters)) {
      tt=load_1future(code,letters[j],i)
      dates[k] = index(tt)[dim(tt)[1]-1]
      k=k+1
    }
  for(j in 3:1) {
    tt=load_1future(code,c("V","X","Z")[j],2005)
    dates = c(index(tt)[dim(tt)[1]-1],dates)
  }
  tt=load_1future(code,"F",2018)
  c(dates,index(tt)[dim(tt)[1]-1])
  dates
}
rolldates<-function(code,letters="",start=1990,first=1,last=1){
  if(length(letters)==1) letters=c("F","G","H","J","K","M","N","Q","U","V","X","Z")
  else letters =  unlist(strsplit(letters[code], split=";"))
  #dates = numeric(length(letters)*(2017-start+1))
  dates = character(1)
  for(j in (first:length(letters))) {
    #try({tt=load_1future(code,letters[j],start)
    #dates = paste(dates,index(tt)[dim(tt)[1]-1],sep=";")}, silent=TRUE)
  }
  for(i in (start+1):2017)
    for(j in 1:length(letters)) {
      try({tt=load_1future(code,letters[j],i)
      dates = paste(dates,index(tt)[dim(tt)[1]-1],sep=";")}, silent=TRUE)
    }
  if(last>0) for(j in (1:last)) {
    try({tt=load_1future(code,letters[j],2018)
    dates = paste(dates,index(tt)[dim(tt)[1]-1],sep=";")}, silent=TRUE)
  }
  dates
}
makevols <- function(series,volpar=63,p=1) {
  vreturns=diff(log(series))
  vols=vreturns
  for(i in 1:dim(vols)[2]) {
    tmp = emavol(vreturns[,i],volpar)^p
    vols[,i] = tmp
  }
  vols
}
naming <- function(v1) {
  deparse(substitute(v1))
}
cbindname <- function(...) {
  out = cbind(...)
  input_list <- as.list(substitute(list(...)))
  len = length(input_list)
  names = character(len-1)
  for (i in 2:len) names[i-1] = deparse(input_list[[i]])
  colnames(out) = names
  out
}
simpledominance <- function(a,b) {
  ifelse(signal,a,b) 
}
lst <- function(object_name){
  names(which(unlist(eapply(.GlobalEnv,object_name))))
}
mod<-function(a,b){
  a-(a %/% b)*b
}
more1nonNA.cases <- function(x){
  n=dim(x)[1]
  m=dim(x)[2]
  out=numeric(0)
  for(i in 1:n)
    if(sum(is.na(x[i,]))<m) out=c(out,i)
  out
}
elim_shift <- function(prices,h=4,sh=1){
  nums = 1:dim(prices)[1]
  tt = prices
  for(i in 1:dim(prices)[2]) tt[,i] = ifelse(mod(nums,h)==sh,tt[,i],NA)
  tt[more1nonNA.cases(tt),]
}
binding <- function(mat){
  xts(log(rowMeans(exp(mat),na.rm=TRUE)),index(mat))
}
bindingCF <- function(mat){
  if(is.xts(mat))
    xts(rowMeans(mat,na.rm=TRUE),index(mat))
  else
    rowMeans(mat,na.rm=TRUE)
}
binding2vol <- function(returns,n=12,tar=0.1,p=1){
  options(warn=-1)
  vols = returns
  for(i in 1:dim(vols)[2]) {
    tmp = emavol(returns[,i],n)
    vols[,i] = tmp
  }
  out=log(1+rowMeans((tar/lag(vols))^p*(exp(returns)-1),na.rm=TRUE))
  options(warn=0)
  xts(out,index(returns))
}
downsample<-function(x,overlap=1){
  subset=seq(1,length(x),overlap)
  diff(log(to.monthly(exp(cumsum(na20(x[subset]))))[,4]))
}
mat.to.numeric<-function(mat){
  for(i in 1:dim(mat)[2]) mat[,i] = as.numeric(as.character(mat[,i]))
  mat
}
mat.12na<-function(mat){
  for(i in 1:dim(mat)[2]) {mat[which(mat[,i]==1 & lag(mat[,i])==1),i] = NA; mat[1,i]=NA}
  mat
}
mat.02na<-function(mat){
  for(i in 1:dim(mat)[2]) mat[which(mat[,i]==0),i] = NA
  mat
}
mat.na20<-function(mat){
  for(i in 1:dim(mat)[2]) mat[which(is.na(mat[,i])),i] = 0
  mat
}
mat.cens<-function(mat,level){
  for(i in 1:dim(mat)[2]) mat[which(mat[,i]<level),i] = NA
  mat
}
na20<-function(x){
  x[which(is.na(x))] = 0
  x
}
z2na<-function(x){
  x[which(x==0)] = NA
  x
}
as.NA1 <-function(x) {
  y=as.numeric(x)
  y[which(!x)] = NA
  y
}
mat.SR <-function(mat,b=252) {
  n = dim(mat)[2]
  rows = matrix(NA,n,7)
  for (i in 1:n) {
    rows[i,] = SR(mat[,i],b)
  }
  rows
}
cumulate <- function(mat) {
  n = dim(mat)[2]
  for (i in 1:n) {
    tmp = mat[,i]
    if(sum(is.na(tmp))>0) tmp = tmp[-which(is.na(tmp))]
    tmp = exp(cumsum(na20(tmp)))
    if(i==1) out=tmp else out = cbind(out,tmp)
  }
  colnames(out) = colnames(mat)
  out
}
week2monthret <- function(tmp) {
  tmp = exp(cumsum(na20(tmp)))
  tmp[which(tmp==1)]=NA
  diff(log(to.monthly(tmp)[,4]))
}
mat.to.weekly <- function(mat,col=4) {
  n = dim(mat)[2]
  for (i in 1:n) {
    tmp = mat[,i]
    if(sum(is.na(tmp))>0) tmp = tmp[-which(is.na(tmp))]
    tmp = to.weekly(tmp)[,col] 
    if(i==1) out=tmp else out = cbind(out,tmp)
  }
  colnames(out) = colnames(mat)
  out
}
mat.to.monthly <- function(mat,col=4) {
  n = dim(mat)[2]
  for (i in 1:n) {
    tmp = mat[,i]
    if(sum(is.na(tmp))!=dim(tmp)[1]) {
      if(sum(is.na(tmp))>0) tmp = tmp[-which(is.na(tmp))]
      tmp = to.monthly(tmp)[,col] 
    }
    if(i==1) out=tmp else out = cbind(out,tmp)
  }
  colnames(out) = colnames(mat)
  out
}
fillerxtsup <- function(rates){
  ratesc=coredata(rates)
  #cure missings rates aka NA at the begining
  ratesc=ratesc[nrow(ratesc):1, ]
  ratesc=filler0(ratesc)
  ratesc=ratesc[nrow(ratesc):1, ]
  rates=xts(ratesc,index(rates))
}
filler <- function(x){
  n = dim(x)[1]
  k = dim(x)[2]
  for (i in 2:n) {
    for (j in 1:k) {
      if(is.na(x[i,j])) x[i,j] = x[i-1,j]
    }
  }
  x
}
filler0 <- function(x){
  n = dim(x)[1]
  k = dim(x)[2]
  for (i in 2:n) {
    for (j in 1:k) {
      if(is.na(x[i,j])) x[i,j] = 0
    }
  }
  x
}
fillerxtsW <- function(y){
  x = coredata(y)
  ind = .indexweek(y) #from quantmod
  n = dim(x)[1]
  k = dim(x)[2]
  for (i in 2:n) {
    for (j in 1:k) {
      if(is.na(x[i,j]) & ind[i]==ind[i-1]) x[i,j] = x[i-1,j]
    }
  }
  xts(x,index(y))
}
fillerxtsWh <- function(y,h=4,sh=1){
  y=lag(y,1)
  x = coredata(y)
  ind = .indexweek(y) #from quantmod
  nums = 1:dim(y)[1]
  n = dim(x)[1]
  k = dim(x)[2]
  for (j in 1:k) {
    for (i in 2:n) {
      if(is.na(x[i,j]) & !is.na(x[i-1,j]) & mod(nums[i-1]-1,h)==sh)
        for(m in 0:(h-2)) {x[i+m,j] = x[i-1,j]}
    }
  }
  xts(x,index(y))
}
fillerxtsM <- function(y){
  x = coredata(y)
  ind = months(index(y))
  n = dim(x)[1]
  k = dim(x)[2]
  for (i in 2:n) {
    for (j in 1:k) {
      if(is.na(x[i,j]) & ind[i]==ind[i-1]) x[i,j] = x[i-1,j]
    }
  }
  xts(x,index(y))
}
fillerxts0 <- function(y){
  x = coredata(y)
  n = dim(x)[1]
  k = dim(x)[2]
  for (i in 2:n) {
    for (j in 1:k) {
      if(is.na(x[i,j])) x[i,j] = 0
    }
  }
  xts(x,index(y))
}
fillerxtsNON0 <- function(y){
  x = coredata(y)
  n = dim(x)[1]
  k = dim(x)[2]
  for (i in 2:n) {
    for (j in 1:k) {
      if(!is.na(x[i,j]))
        if(x[i,j]==0) x[i,j] = x[i-1,j]
    }
  }
  xts(x,index(y))
}
dailization <- function(values_m,values_d){
  values = lag(values_m)
  values = merge(values_d[,1], xts(coredata(values), as.Date(index(values))-1))[,-1]
  values = fillerxts(values)
  values
}
drawdown <- function(pnl,overlap=1,move=0,window=0) {
  n = length(pnl)
  subset=seq(1+move,n,overlap)
  if(window!=0) {
    n=dim(pnl)[1]
    pnl = pnl[(n-window):(n),]
  }
  cum.pnl  <- c(0, cumsum(na20(pnl[subset])))
  drawdown <- cum.pnl - cummax(cum.pnl)
  return(tail(drawdown, -1))
}
rolldrawdown <- function(pnl,pnlH,k=252,overlap=1,move=0) {
  n = length(pnl)
  subset=seq(1+move,n,overlap)
  cum.pnl  <- rollsum(na20(pnl[subset]),k,fill=NA,align="right")
  cum.pnlH  <- rollsum(na20(pnlH[subset]),k,fill=NA,align="right")
  drawdown <- cum.pnl - rollmax(cum.pnlH,k,fill=NA,align="right")
  return((drawdown))
}
rollalphabeta <- function(x,bench,k=252) {
  d = cbind(x,bench)
  d = d[complete.cases(d),]
  colnames(d)=c("x","bench")
  out=rollapply(d,
                width=k,
                FUN = function(Z) 
                { 
                  lm1 = lm(x~bench,data=as.data.frame(Z))
                  return(c(lm1$coef[1],lm1$coef[2]))
                },
                by.column=FALSE, align="right")
  return(out)
}
rollbeta <- function(x,bench,k=252) {
  d = diff(log(cbind(x,bench)))
  d = d[complete.cases(d),]
  colnames(d)=c("x","bench")
  rollalphabeta(d$x,d$bench,k=k)[,2]
}
maxdrawdown <- function(pnl,overlap=1) {
  out = rep(NA,overlap)
  for(i in 1:overlap){
    out[i]=exp(min(drawdown(pnl,overlap,i-1)))-1
  }
  return(min(out))
}
avgdrawdown <- function(pnl,overlap=1) {exp(mean(drawdown(pnl,overlap)))-1}
switcher <- function(signal1,signal2,control){
  dane = merge(diff(log(signal1)),diff(log(signal2)),lag(control))
  dane = fillerxts(dane)
  colnames(dane) = c("returns1","returns2","control")
  newsignal = ifelse(dane$control <= 0, dane$returns2, dane$returns1)
  exp(cumsum(na20(newsignal)))*fnna(signal1)
}
plot.cum<-function(x,b=0){plot(cumsum(na20(x)),t="l")}
cum<-function(x,b=0){cumsum(na20(x))}
symlog<-function(x){sign(x)*log(abs(x)+1)}
pnl2ret<-function(x,b){z2na(diff(log(b+cumsum(na20(x)))))}
forwarduj<-function(x,k){c(x[-(1:k)],rep(NA,k))}
lagguj<-function(x,k){c(rep(NA,k),x[-((length(x)-k+1):length(x))])}
acorSeries <- function(n,mean,sd,cor){
  x=numeric(n)
  x[1] = rnorm(1)
  for(i in 2:n){
    x[i] = cor*x[i-1] + sqrt(1-cor^2)*rnorm(1)
  }
  return(x*sd+mean)
}
capStop <- function(logret,limit=0,levfactor=1){
  n=length(logret)
  new=numeric(n)
  dd = drawdown(logret)
  newdd=dd
  new[1] = logret[1]
  new[2] = logret[2]
  currlev = 1
  pos = 1
  for(i in 3:n){
    if(max(abs(newdd[i-1]),abs(dd[i-1]))>limit & pos==1) { new[i] = 0; currlev = 1; pos=0 }
    else {
      if(logret[i-1]*logret[i-2]>0) currlev = currlev*levfactor
      if(abs(dd[i-1])<limit) {
        new[i] = currlev*logret[i]
        pos=1
      }
    }
    newdd = drawdown(new)
  }
  return(new)
}
test_dd <- function(){
  n=252*4
  mdays=(0:(n-1))%%21
  
  mean = 0.0005
  sd = 0.0003*sqrt(252)
  SR = mean/sd*sqrt(252)
  x=rnorm(n)*sd+mean
  x=acorSeries(n,mean,sd,0.6)
  cor(x[-n],x[-1])
  
  y = cumsum(x)
  y = cbind(y,mdays)
  y = y[which(y[,2]==20),1]
  y = diff(y)
  cor(y[-47],y[-1])
  
  plot(ts(exp(cumsum(x))))
  plot(ts(exp(cumsum(new))))
  plot(ts(exp(cumsum(y))))
  
  new = capStop(x,0.01,1.005)
  
  mean(new)/sd(new)*sqrt(252)
  mean(x)/sd(x)*sqrt(252)
  mean(y)/sd(y)*sqrt(12)
  
  dd = drawdown(x)
  plot(ts(dd))
}
subdata <- function(data,h,m){
  data$m = substr(data[,1],12,13)
  data$h = substr(data[,1],10,11)
  data$d = substr(data[,1],1,8)
  out = data[which((data$h==h) & (data$m==m)),-1]
  xts(out[,1],as.Date(as.character(out$d), format = "%Y%m%d"))
}
read.posix.table <- function(name){
  data = read.table(paste0(data_folder,name),sep=",")
  xts(data,as.POSIXlt(rownames(data)))
}
subhourmin <- function(data,h,m){
  data = data[which((index(data)$h==h) & (index(data)$min==m)),]
  xts(coredata(data),as.Date(index(data)))
}
subhourmin2 <- function(data,h,m,col=4){
  options(warn=-1)
  dates = unique(as.Date(index(data)))
  all = xts(rep(NA,length(dates)),dates)
  for(i in m:(m+13)){
    if(length(which((index(data)$h==h) & (index(data)$min==i)))>0){
      out = data[which((index(data)$h==h) & (index(data)$min==i)),col]
      out = xts(coredata(out),as.Date(index(out)))
      all = cbind(all,out)
    }
  }
  allmin = -rowMax(-all)
  out = xts(coredata(allmin),as.Date(index(all)))
  out[which(abs(out)==Inf)] = NA
  options(warn=0)
  out
}
make_day_split <- function(subdaily,splitmat){
  k=dim(splitmat)[1]
  for(i in 1:k) {
    if(i==1) out = subhourmin2(subdaily,splitmat[i,1],m=splitmat[i,2],col=1) #to.daily(subdaily)[,1]
    else if(i==k) out = cbind(out,subhourmin2(subdaily,splitmat[i,1],m=splitmat[i,2])) #to.daily(xts(coredata(subdaily),index(subdaily)+60*60*8))[,4]
    else out = cbind(out,subhourmin(subdaily,h=splitmat[i,1],m=splitmat[i,2])[,4])
  }
  out0 = subhourmin(subdaily,h=splitmat[1,1]+1,m=splitmat[1,2])[,1]#for winter-summer time
  tt=index(out)[which(is.na(out[,1]))]
  for(i in 1:length(tt)){
    if(length(out0[,1][tt[i]])>0) out[,1][tt[i]]=out0[,1][tt[i]]
  }
  out
}
subdataMM <- function(data,h1,m1,h2,m2,raw=F){
  data$m = as.numeric(substr(data[,1],12,13))
  data$h = as.numeric(substr(data[,1],10,11))
  data$d = substr(data[,1],1,8)
  tt=which((data$h>=h1)); if(length(tt)>0) out = data[tt,-1]
  tt=which((out$h==h1) & (out$m<m1)); if(length(tt)>0) out = out[-tt,]
  tt=which((out$h<=h2)); if(length(tt)>0) out = out[tt,]
  tt=which((out$h==h2) & (out$m>m2)); if(length(tt)>0) out = out[-tt,]
  days = unique(out$d)
  n=length(days)
  minmax = matrix(NA,n,4)
  for(i in 1:n){
    subout = out[which(out$d==days[i]),]
    minmax[i,1] = subout[1,1]
    minmax[i,2] = max(subout[,2],na.rm=TRUE)
    minmax[i,3] = min(subout[,3],na.rm=TRUE)
    minmax[i,4] = subout[dim(subout)[1],4]
  }
  if(!raw) xts(minmax,as.Date(as.character(days), format = "%Y%m%d"))
  else out
}
add0 <-function(x){if(x>9) paste(x) else paste0(0,x)}
divide <- function(dane,asset,hours,minutes,raw=TRUE,path='',spread=0){
  k=length(hours)
  for(i in 1:k){
    #subdane = subdata(dane,h=add0(hours[i]),m=add0(minutes[i]))
    if(i==1) subdane = subdataMM(dane,h1=hours[i],m1=minutes[i],h2=hours[i+1],m2=minutes[i+1])[,1]
    else subdane = subdataMM(dane,h1=hours[i-1],m1=minutes[i-1],h2=hours[i],m2=minutes[i])[,4]
    if(i==1) out = subdane
    else out=cbind(out,subdane)
  }
  colnames(out)=paste0('x',1:k)
  if(raw) out
  else {
    out2 = out
    out3 = out
    for(i in 1:k){
      if(i==1) out2[,i] = log(out[,i]/lag(out[,k]+spread))
      else out2[,i] = log(out[,i]/(out[,i-1]+spread))
      if(i==1) out3[,i] = log(-out[,i]/lag(out[,k]-spread))
      else out3[,i] = log(-out[,i]/(out[,i-1]-spread))
    }
    period_start = paste0(add0(hours[k]),':',add0(minutes[k]))
    for(i in 1:k){
      period_end = paste0(add0(hours[i]),':',add0(minutes[i]))
      png(filename=paste0(path,asset,"_x",i,".png"))
      test=SR(out2[,i],252)[1]
      print(plot(cum(out2[,i]),main=paste0(period_start,'-',period_end,',  SR: ',test)))
      dev.off()
      png(filename=paste0(path,asset,"_s",i,".png"))
      test=SR(out3[,i],252)[1]
      print(plot(cum(out3[,i]),main=paste0(period_start,'-',period_end,',  SR: ',test)))
      dev.off()      
      period_start=period_end
    }
    
    test=SR(xtsRowSums(out2),252)[1]
    png(filename=paste0(path,asset,"_all.png"))
    print(plot(cum(xtsRowSums(out2)),main=paste0('all,  SR: ',test)))
    dev.off()    
    
    test=SR(log(out[,7]/lag(out[,7]+spread)),252)[1]
    png(filename=paste0(path,asset,"_all2.png"))
    print(plot(cum(log(out[,7]/lag(out[,7]))),main=paste0('all2,  SR: ',test)))
    dev.off()  
  }
}
xtsRowSums<-function(x,na.rm=TRUE){xts(rowSums(x,na.rm=na.rm),index(x))}
zzFilter <- function(z) {
  zz = diff(na20(sign(diff((lag(z,-1))))))
  #zz = diff((sign(diff((lag(z,-1))))))
  zz = z[which(abs(zz)>0)]
  return(zz)
}
zzRS <- function(ohlc,tresh=10,deep=2,w=5,f=250,rel=T){
  if(is.null(dim(ohlc))) ohlc=as.matrix(ohlc)
  n = dim(ohlc)[1]
  if(dim(ohlc)[2]<=4) {w=1}
  if(dim(ohlc)[2]==1) {r=1}
  else r=2:3
  x = matrix(NA,n,deep)
  y = matrix(NA,n,2)
  for(i in f:n){
    z = ZigZag(ohlc[1:i,r],tresh)
    zz = zzFilter(z)
    k=dim(zz)[1]
    m=max(k-deep+1,1)
    x[i,1:min(deep,k)] = as.numeric(coredata(zz))[k:m]
    if(i>f) y[i,] = closestRS(x[i-1,],coredata(ohlc[i,w]),rel=rel)
  }
  xts(y,index(ohlc))
}
closestRS <- function(RS,x,rel=F){
  ordered=RS[order(RS)]
  z=ordered-x
  if(length(which(z>0))>0) k=which(z>0)[1]
  else k=length(z)
  if(rel) log(ordered[c(k-1,k)]/x)
  else ordered[c(k-1,k)]
}
rollmaxna=function(x,k,fill=NA,align="right"){rollmax(x,k,fill=fill,align=align)}
sqr<-function(x){x^2}
extend_table<-function(mat){
  mat=cbind(mat,rowSums(mat))
  rbind(mat,colSums(mat))
}
acclin<-function(model,newdata=NULL,level=0.5){
  dependent_var_name=strsplit(as.character(model$call[2]),split='~')[[1]][1]
  if(is.null(newdata)){
    yhat = predict(model) > level
    dependent_var = (model$fitted.values+model$residuals) > level
  }
  else{
    yhat = predict(model,newdata) > level
    dependent_var = newdata[,dependent_var_name]
  }
  #extend_table(table(yhat,dependent_var))
  sum(diag(table(yhat,dependent_var)))/length(yhat)
}
acc<-function(model,newdata=NULL,level=0.5){
  dependent_var_name=as.character(model$formula[2])
  #dependent_var_name=strsplit(as.character(model$call[2]),split='~')[[1]][1]
  if(is.null(newdata)){
    yhat=predict(model,type='response')>level
    dependent_var=model$data[,dependent_var_name]
    #dependent_var=model$fitted.values+model$residuals
    sum(diag(table(yhat,dependent_var)))/length(yhat)
  }
  else{
    yhat=predict(model,type='response',newdata)>level
    sum(diag(table(yhat,newdata[,dependent_var_name])))/length(yhat)
  }
}
percumsum = function(x){sum(x);cumsum(x)/sum(x)}
library(pracma)
SRcurve<-function(model,signal,newdata=NULL,div=10,b=252,t=2,max_na=0.95,max_div=0.98,raw=FALSE){#max is max for NA fraction
  if(is.null(newdata)){
    yhat=predict(model,type='response') #should be changed to XTS, currently is not
  }
  else{
    yhat=xts(predict(model,type='response',newdata),index(newdata))
  }
  z=cbind(yhat,signal)
  z = z[complete.cases(z),]
  colnames(z)=c('yhat','signal')
  divisors = round(percumsum(abs(log((1:(div-1))/div))),4) #(1:(div))/div
  divisors = round((1.5 + divisors)/2.5,4) # to consider only divisors > 0.6
  mat = matrix(NA,div-2,8)
  for(i in 1:(div-2)) {
    mat[i,] = SR(ifelse(z$yhat>divisors[i],z$signal,NA),b)
  }
  colnames(mat) = names(SR(ifelse(z$yhat>divisors[i],z$signal,NA),b))
  rownames(mat) = divisors[1:(div-2)]
  mat = mat[which(mat[,6]<max_na),]
  mat = mat[which(as.numeric(rownames(mat))<max_div),]
  #plot(exp(exp(as.numeric(rownames(mat)))-1),mat[,t],t="b")
  plot(as.numeric(rownames(mat)),mat[,t],t="b")
  mat[which(mat[,t]==Inf),t] = NaN
  s = which(is.na(mat[,t]))[1]
  if(is.na(s)) s = dim(mat)[1]+1
  if(raw) mat
  else trapz(as.numeric(rownames(mat))[1:(s-1)],mat[1:(s-1),t])
}
accSR<-function(model,signal,newdata=NULL,div=10,b=252){
  if(is.null(newdata)){
    yhat=predict(model,type='response') #should be changed to XTS, currently is not
  }
  else{
    yhat=predict(model,type='response',newdata)
  }
  z=cbind(yhat,signal)
  z = z[complete.cases(z),]
  z = coredata(z)
  colnames(z)=c('yhat','signal')
  for(i in 1:div) {
    zz=z[which(z$yhat>(i-1)/div & z$yhat<=i/div),]
    freq = round(dim(zz)[1]/dim(z)[1],3)
    print(c(freq,SR(zz$signal,b)))
  }
}
accSRlin<-function(model,signal,newdata=NULL,div=10,b=252){
  if(is.null(newdata)){
    yhat=logit(predict(model))
  }
  else{
    yhat=logit(predict(model,newdata))
  }
  z=cbind(yhat,signal)
  z = z[complete.cases(z),]
  colnames(z)=c('yhat','signal')
  for(i in 1:div) {
    zz=z[which(z$yhat>(i-1)/div & z$yhat<=i/div),]
    freq = round(dim(zz)[1]/dim(z)[1],3)
    print(c(freq,SR(zz$signal,b)))
  }
}
logit<-function(x){1/(1+exp(-x))}
nowe <- function(){
  library(MASS)
  lda(x, grouping, prior = proportions, tol = 1.0e-4,method, CV = FALSE, nu, ...)
  lda(formula, data, ..., subset, na.action)
}
antiGrid <- function(ret,k=2,req=F){
  n=dim(ret)[1]
  x=rep(NA,n)
  x[1]=ret[1]
  x[2]=ret[2]
  m=k
  for(i in 3:n){
    if(coredata(ret[i-2]>0)*coredata(ret[i-1]>0)) m=k*ifelse(req,m,1)
    else if(coredata(ret[i-2]<0)*coredata(ret[i-1]<0)) m=1/k*ifelse(req,m,1)
    else m=1*ifelse(req,m,1)
    x[i]=levret(m,ret[i])
  }
  xts(x,index(ret))
}
stoploss <- function(ret,ohlc,limit){
  n=dim(ret)[1]
  bar=log(ohlc[,4]/ohlc[,1])
  down = log(ohlc[,1]/ohlc[,3])
  up = log(ohlc[,2]/ohlc[,1])
  dir= ret*bar
  tmp = cbind(dir,up,down,limit)
  d = merge(ret,tmp,join="left")
  colnames(d) = c("ret","dir","up","down","limit")  
  ifelse(d$dir>0,ifelse(d$down>d$limit,-d$limit,d$ret),ifelse(d$dir<0,ifelse(d$up>d$limit,-d$limit,d$ret),0))
}
takeprofit <- function(ret,ohlc,limit,cost=0.00015){
  n=dim(ret)[1]
  bar=log(ohlc[,4]/ohlc[,1])
  down = log(ohlc[,1]/ohlc[,3])
  up = log(ohlc[,2]/ohlc[,1])
  dir= ret*bar
  tmp = cbind(dir,up,down,limit)
  d = merge(ret,tmp,join="left")
  colnames(d) = c("ret","dir","up","down","limit")
  ifelse(d$dir>0,ifelse(d$up>d$limit+cost,d$limit,d$ret),ifelse(d$dir<0,ifelse(d$down>d$limit+cost,d$limit,d$ret),0))
}
intra_time<- function(extr,o,limitcost,m=1){
  fo = as.numeric(coredata(o)[1])
  t=which(m*log(extr/fo)>limitcost)[1]
  out = as.numeric(index(extr)[t])
  if(is.na(out)) NaN#1/12*1/2
  else out
}
sltpTS <- function(ret,ohlc,mindata,tp,sl,cost=0.00015){
  n=dim(ret)[1]
  bar=log(ohlc[,4]/ohlc[,1])
  down = log(ohlc[,1]/ohlc[,3])
  up = log(ohlc[,2]/ohlc[,1])
  dir= ret*bar
  tmp = cbind(dir,up,down,tp,sl)
  d = merge(ret,tmp,join="left")
  colnames(d) = c("ret","dir","up","down","tp","sl")
  out_tp = coredata(ifelse(d$dir>0,ifelse(d$up>d$tp+cost,d$tp,d$ret),ifelse(d$dir<0,ifelse(d$down>d$tp+cost,d$tp,d$ret),0)))
  out_sl = coredata(ifelse(d$dir>0,ifelse(d$down>d$sl,-d$sl,d$ret),ifelse(d$dir<0,ifelse(d$up>d$sl,-d$sl,d$ret),0)))
  out=d$ret
  idx=index(d$dir)
  ret=coredata(ret)
  day = as.Date(as.character(mindata$d), format = "%Y%m%d")
  mindata$day = day
  for(i in 1:n){
    if(!is.na(ret[i])){
      if(ret[i]==out_tp[i] & ret[i]!=out_sl[i]){# no take profit, but stoploss
        out[i]=out_sl[i]
      }
      if(ret[i]==out_sl[i] & ret[i]!=out_tp[i]){# no stop loss, but take profit
        out[i]=out_tp[i]
      }
      if(ret[i]!=out_sl[i] & ret[i]!=out_tp[i]){# both stop loss and take profit
        tt = which(mindata$day==idx[i])
        if(d$dir[i]>0){
          timeTP = intra_time(mindata[tt,2],mindata[tt,1],tp+cost,1)
          timeSL = intra_time(mindata[tt,3],mindata[tt,1],sl+cost,-1)
        }
        if(d$dir[i]<0){
          timeTP = intra_time(mindata[tt,3],mindata[tt,1],tp+cost,-1)
          timeSL = intra_time(mindata[tt,2],mindata[tt,1],sl+cost,1)
        }
        else{
          timeTP=1
          timeSL=2
        }
        if(timeTP<timeSL) out[i]=out_tp[i]
        if(timeTP>timeSL) out[i]=out_sl[i]
        if(timeTP==timeSL) out[i]=out_sl[i]
      }
      #else#nothing happened - we already did it
      #  out[i]=d$ret[i]
    }
  }
  out
}
alpha <- function(x,y,b=252){
  d = cbind(x,y)
  colnames(d)=c("x","y")
  lm1=lm(x~y,data=d)
  s = summary(lm1)$coef
  s[1,1]/s[2,1]*b
}
treynor <- function(x,y,b=252){
  d = cbind(x,y)
  colnames(d)=c("x","y")
  lm1=lm(x~y,data=d)
  s = summary(lm1)$coef
  mean(x,na.rm=TRUE)*b/s[2,1]
}
invdotcom <- function(names){
  for (i in (1:length(names))){
    series = read.csv(paste0(data_folder,names[i],"_d.csv"),header=TRUE,sep="\t")
    series = series[,-(6:7)]
    series = series[,c(1,3,4,5,2)]
    series[,1] = as.Date(series[,1],format="%b %d, %Y")
    series[,2] = as.numeric(gsub(",", "", series[,2]))
    series[,3] = as.numeric(gsub(",", "", series[,3]))
    series[,4] = as.numeric(gsub(",", "", series[,4]))
    series[,5] = as.numeric(gsub(",", "", series[,5]))
    write.csv(series,paste0(data_folder,names[i],"_d.csv"),row.names = F, sep=",")
  }
}
vrpmat <- function(ind,ivol,dir=1,pos=1){
  vrp = ivol-(emavol(diff(log(ind)),21))
  vrpm = to.weekly(vrp)[,4]
  outcomes = as.data.frame(matrix(NaN,12,12))
  step=1
  for (i in 1:12) {
    par1=1+(i-1)*step
    smoo = SMA(vrpm,par1)
    rownames(outcomes)[i]=paste0("i",par1)
    for (j in 1:12) {
      par2=1+(j-1)*step
      colnames(outcomes)[j]=par2
      if(dir==1) tt = splotd(ind,pos*diff(smoo,lag=par2))
      else tt = splotS(ind,pos*diff(smoo,lag=par2))
      outcomes[i,j] = round(SR(diff(log(tt["2007-11/"])))[1],2)
    }
  }
  outcomes
}
ssum<-function(...){
  out = exp(diff(log(cbind(...))))-1
  ind = index(out)
  out = std2log(rowSums(out,na.rm=TRUE))
  xts(exp(cumsum(na20(out))),ind)
}
subper<-function(series,p1b,p1e,p2b,p2e){
  exp(cumsum(rbind(diff(log(series))[p1b][p1e],diff(log(series))[p2b][p2e])))
}
rowMax <- function(X) {
  n = dim(X)[1]
  y = numeric(n)
  for (i in 1:n) {
    y[i] = max(X[i,],na.rm=TRUE)
  }
  y
}
cftcdata <- function(name,begin=2017,end=2019,path='') {
  letter = substr(name,1,1)
  markets = strsplit(name,"_")[[1]][1]
  type = strsplit(name,"_")[[1]][2]
  if(type=="fin") {
    fname1 = paste0(toupper(letter),'_TFF_2006_2016.txt')
    fname2 = paste0(capitalize(type),capitalize(markets),'YY.txt')
    fname3 = paste0(type,'_',markets,'_txt')
  }
  else{
    fname1 = paste0(toupper(letter),'_Disagg06_16.txt')
    fname2 = paste0(letter,'_year.txt')
    fname3 = paste0(name,'_txt_hist')
  }
  out = read.table(unz(paste0(data_folder,fname3,'_2006_2016.zip'),fname1), sep=',', header=T)
  for(i in begin:end) {
    tmp = read.table(unz(paste0(path,name,'_txt_',i,'.zip'),fname2), sep=',', header=T)
    out=rbind(out,tmp)
  }
  out
}
take <- function(x_xts,date1='1970-01-01',date2='2020-12-31'){x_xts[paste0(date1,'/')][paste0('/',date2)]}
capitalize <- function(str){paste0(toupper(substr(str,1,1)),substr(str,2,nchar(str)))}
share <- function(a,b){a/(a+b)}
multp <- function(mat,k){
  for(i in 1:k) {
    if(i==1) out = mat
    else out = cbind(out,mat)
  }
  out
}
capcumsum <- function(x_xts){
  if(is.xts(x_xts))
    x = coredata(x_xts)
  else
    x = x_xts  
  if(is.null(dim(x))) x=as.matrix(x)
  n=dim(x)[1]
  y=rep(NA,n)
  y[1]=x[1]
  for(i in 2:n){
    y[i]=max(0,x[i]+y[i-1])
  }
  if(is.xts(x_xts))
    xts(y, order.by=index(x_xts))
  else
    y  
}
extcum <- function(x_xts,z_xts){
  ### x contains 1 or more for open position and 0 otherwise
  ### z contains spcific number for first 1 in x and has to be populated
  if(is.xts(x_xts) & is.xts(z_xts)){
    d = merge(x_xts,z_xts)
    colnames(d) = c("x","z")
    d = as.data.frame(coredata(d))
  }
  if(is.xts(x_xts))
    x = coredata(x_xts)
  else
    x = x_xts  
  if(is.xts(z_xts))
    z = coredata(z_xts)
  else
    z = z_xts      
  if(!(is.xts(x_xts) & is.xts(z_xts))){
    d = as.data.frame(cbind(x,z))
    colnames(d) = c("x","z")
  }
  n=dim(d)[1]
  y=rep(NA,n)
  y[1]=d$x[1]
  for(i in 2:n){
    if(d$x[i]>=1 & d$x[i-1]==0)
      y[i] = d$z[i]
    else if(d$x[i]>=1 & d$x[i-1]!=0)
      y[i] = y[i-1]
  }
  if(is.xts(x_xts))
    xts(y, order.by=index(x_xts))
  else
    y  
}
cumenum <- function(x_xts){
  ### x contains 1 or more for open position and 0 otherwise
  ### y (output) will contain number of iterations since first 1 in x
  if(is.xts(x_xts))
    x = coredata(x_xts)
  else
    x = x_xts  
  n=dim(x_xts)[1]
  y=rep(NA,n)
  y[1]=0
  for(i in 2:n){
    if(x[i]>=1 & x[i-1]==0) #first occurence of 1
      y[i] = 1
    else if(x[i]>=1 & x[i-1]!=0)
      y[i] = y[i-1]+1
  }
  if(is.xts(x_xts))
    xts(y, order.by=index(x_xts))
  else
    y  
}
## internal zigzag function returns same structure as ZigZag but instead of turning points it gives confirmation points
internalZigZag <- function(x,k,percent=TRUE,side="both"){
  zz = ZigZag(x,k,percent = percent)
  dirext = -diff(na20(sign(diff((lag(zz,-1))))))/2 #zigzag, all dates, transformed into -1 / 1 directions
  zigs = capcumsum(na20(lag(-dirext,1))) #only 1's for up zigzags times
  zags = -capcumsum(na20(lag(dirext,1))) #only -1's for down zigzags times
  if(percent){
    ppext1 = extcum(zigs,(x)*(1+k/100))
    ppext2 = extcum(-zags,(x)*(1-k/100))
  }
  else{
    ppext1 = extcum(zigs,(x)-sign(x)*k)
    ppext2 = extcum(-zags,(x)+sign(x)*k)
  }
  xx1=xts((x>ppext1),index(x)) ## 1's for up zig after confirmation
  fxx1 = floor((1+diff(ifelse(xx1,2,1)))/2) ## first occurence of xx1, zig confirmation
  xx2=xts((x<ppext2),index(x)) ## 1's for down zig after confirmation
  fxx2 = -floor((1+diff(ifelse(xx2,2,1)))/2) ## first occurence of xx2, zag confirmation
  ffxx1 = floor((1+diff(ifelse(capcumsum(na20(fxx1)-na20(ifelse(dirext==1,1,0))*999),2,1)))/2) #first occurrence of fxx1 in zig
  ffxx2 = -floor((1+diff(ifelse(capcumsum(-(na20(fxx2)-na20(ifelse(dirext==-1,-1,0))*999)),2,1)))/2) #first occurrence of fxx2 in zag
  both = xts(rowMeans(cbind(z2na(ffxx1),z2na(ffxx2)),na.rm=TRUE),index(cbind(ffxx1,ffxx2)))
  both = both[complete.cases(both),]
  if(side=="both") both
  else if(side==1) ffxx1 #index(ffxx1[complete.cases(z2na(ffxx1))])
  else ffxx2 #index(ffxx2[complete.cases(z2na(ffxx2)),])
}
#izz = internalZigZag(dd,0.2,percent=F,side=1)
#plot(dd)
#lines(zz,col=2)
#points(dd[izz],col=4)
synth <- function(open,close,o.type=1,c.type=1){
  #in type 1: open and close are path indicators and we make actions on first 0 crossing
  #in type 0: open and close are presence indicators and we make actions when positive
  if(o.type==1) xo = diff(sign(open)) else xo = open
  if(c.type==1) xc = diff(sign(close)) else xc = close
  xo = ifelse(xo>0,99,0)
  xc = ifelse(xc>0,-99999,0)
  y = capcumsum(na20(xo)+na20(xc))
  sign(y)
}
quickrec<-function(x,denom=1){cumsum(na20(diff(x)*-1/lag(x)^2*ilor(denom)))}
