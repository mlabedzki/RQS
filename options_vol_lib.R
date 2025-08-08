#globals list: tau,S,K,vols,k,vec,ttm,stm,F,rd,rn
vip_globals <- function(){
	tau <<- c(1/52,1/12,2/12,3/12,6/12,9/12,1,2)
	z <<- length(tau)
	k <<- 2
	vec <<- (k-1)*5+(1:5)
	ttm <<- tau[k]
	stm <<- floor(ttm*5/7*365)
}
jump=0.0005
impdistr <- function(i,lx,cum=0,type="volspline",cp=1,capped=0) {
  x = exp(lx)*as.numeric(Fwd[i,k])
  if(type=="volparabolic") {
	pvols = pmax(0.001,unname(parabolic(lx,log(K[i,vec]/as.numeric(Fwd[i,k])),as.numeric(vol[i,vec]))))
	prms = Black76(Fwd[i,k],x,pvols,ttm,rd[i,k],1)
  }
  if(type=="volspline") {
	pvols = splinefun(x=log(K[i,vec]/as.numeric(Fwd[i,k])), y=as.numeric(vol[i,vec]), method="natural",  ties = mean)
	if(capped==1) {
		pvolsCap = pvols(lx)
		xx = which(lx<log(K[i,1+(k-1)*5]/as.numeric(Fwd[i,k])))
		pvolsCap[xx] = pvols(log(K[i,1+(k-1)*5]/as.numeric(Fwd[i,k])))
		xx = which(lx>log(K[i,5+(k-1)*5]/as.numeric(Fwd[i,k])))
		pvolsCap[xx] = pvols(log(K[i,5+(k-1)*5]/as.numeric(Fwd[i,k])))
		pvols = pvolsCap
	}
	else{
		pvols = pvols(lx)
	}
	prms = Black76(Fwd[i,k],x,pvols,ttm,rd[i,k],1)
  }
  if(type=="sabr") {
	params=c(log(0.1),atanh(0),log(1))
	opt=optim(params,fn=SSE_SABRlim,sigma.Mkt=as.numeric(vol[i,vec]),f=as.numeric(Fwd[i,k]),K=K[i,vec],T=tau[k],beta=1)
	params = c(exp(opt$par[1]),tanh(opt$par[2]),exp(opt$par[3]))
	pvols = sigma_SABR(f=Fwd[i,k],K=x,T=tau[k],beta=1,params[1],params[2],params[3],ref = FALSE)
	prms = Black76(Fwd[i,k],x,pvols,ttm,rd[i,k],1)
  }
  cdf0 = c(NA,diff(prms)/jump)
  #cdf= cdf/Fwd[i,k]+1
  cdf=decrease_cap(norm_func(cdf0))
  pdf = c(NA,diff(cdf)/jump)
  fac=1
  if(capped==1) {#make smoother
  	m = length(pdf)
  	pdf2=rep(NA,m)
  	pdf2[1:3]=pdf[1:3]
  	#test = abs(c(NA,diff(c(NA,abs(diff(pdf)))))) #<0.75
  	test = c(NA,abs(diff(pdf))) #<0.2
  	for(j in 4:m){
	  	if(test[j]<0.2 & pdf[j]!=0) pdf2[j] = pdf[j] #
	  	else pdf2[j] = pdf2[j-1]
	}
	pdf=pdf2
	func = splinefun(x=lx, y=pdf, method="natural",  ties = mean)
  	fac = as.numeric(integrate(func,min(lx),max(lx))[1])
  	cdf = norm_func(cumsum(na20(pdf)/fac))
  }
  if(cum==2) {
  	if(type=="volparabolic") prms = Black76(Fwd[i,k],x,pvols,ttm,rd[i,k],cp)
  	if(type=="volspline") prms = Black76(Fwd[i,k],x,pvols(lx),ttm,rd[i,k],cp)
  	if(type=="sabr") prms = Black76(Fwd[i,k],x,pvols(lx),ttm,rd[i,k],cp)
  }
  if(cum==0) return(splinefun(x=lx, y=pdf/fac, method="natural",  ties = mean))
  else if(cum==1) return(approxfun(x=lx, y=cdf))
  else if(cum==2) return(prms)
  else if(cum==3) return(splinefun(x=lx, y=pvols, method="natural",  ties = mean))
  else if(cum==-1) return(approxfun(x=cdf, y=lx)) #inverse cdf
  else if(cum==-11) return(approxfun(x=cdf0/Fwd[i,k]+1, y=lx)) #inverse cdf
}
givemid<-function(pair,z=8,t="mid",raw=FALSE,format="%m/%d/%Y",v=1){
	newtable = givemidavg(pair,z=z,t=t,raw=raw,format=format)
	if(v==2) {
		tmp = newtable[,1:10]
		cntmp = colnames(newtable)[1:10]
		#tmp2 = newtable[,51:58]
		newtable[,1:40] = newtable[,11:50]
		colnames(newtable)[1:40] = colnames(newtable)[11:50]
		newtable[,41:50] = tmp
		colnames(newtable)[41:50] = cntmp
	}
	midtable = newtable
	if(!raw) for(i in 0:(z-1)){
		newtable[,1+i*5] = midtable[,1+i*5]+midtable[,5+i*5]-midtable[,4+i*5]/2
		newtable[,2+i*5] = midtable[,1+i*5]+midtable[,3+i*5]-midtable[,2+i*5]/2
		newtable[,3+i*5] = midtable[,1+i*5]
		newtable[,4+i*5] = midtable[,1+i*5]+midtable[,3+i*5]+midtable[,2+i*5]/2
		newtable[,5+i*5] = midtable[,1+i*5]+midtable[,5+i*5]+midtable[,4+i*5]/2
	}
	#newtable[,1:(5*i)] = newtable[,1:(5*i)]/100
	#newtable=newtable[rowSums(is.na(newtable)) != ncol(newtable)-1, ]
	#newtable=newtable[,colSums(is.na(newtable)) != nrow(newtable), ]
	newtable
	#write.table(cbind(idx,coredata(newtable)),"audmid.csv",sep=",",row.names=F)
}
givemidavg<-function(pair,z=8,t="mid",raw=FALSE,format="%m/%d/%Y",dq=TRUE){
	bidtable=read.table(paste0('/home/miklab/Downloads/marketdata/FX/new/',get_ccy(pair),'_bid_vols.csv'), header = TRUE, sep=',',dec='.')
	bidtable=bidtable[rowSums(is.na(bidtable)) != ncol(bidtable)-1, ]
	#bidtable=bidtable[,colSums(is.na(bidtable)) != nrow(bidtable), ]
	bidtable[,1]=as.Date(as.character(bidtable[,1]),format=format)#format="%m/%d/%Y" or #"%Y-%m-%d"
	bidtable=xts(bidtable[,-1],bidtable[,1])
	asktable=read.table(paste0('/home/miklab/Downloads/marketdata/FX/new/',get_ccy(pair),'_ask_vols.csv'), header = TRUE, sep=',',dec='.')
	asktable=asktable[rowSums(is.na(asktable)) != ncol(asktable)-1, ]
	#asktable=asktable[,colSums(is.na(asktable)) != nrow(asktable), ]
	asktable[,1]=as.Date(as.character(asktable[,1]),format=format)
	asktable=xts(asktable[,-1],asktable[,1])
	if(dim(bidtable)[2]>=dim(asktable)[2]){
		bidtable=bidtable[,1:dim(asktable)[2]]
	}
	if(dim(bidtable)[2]<=dim(asktable)[2]){
		asktable=asktable[,1:dim(bidtable)[2]]
	}
	if(t=="mid") 
		midtable = asktable/2+bidtable/2
	else if(t=="bid")
		midtable = bidtable
	else
		midtable = asktable
	newtable = midtable
	if(dq) newtable[which(is.na(newtable[,28])),11:50] = NA #due to DQ error in Reuters
	newtable
}
get_ccy=function(pair){
	a=substr(pair,1,3)
	b=substr(pair,4,6)
	if(a=="USD") return(b)
	if(b=="USD") return(a)
}
load_globals<-function(newtable,z,pair,noxts=TRUE){
	vip_globals() #tau, z, k, vec, ttm, stm
	foisusd = givemidavg("USDUSD",z)[,2:9]/100

	newtable <- (fillerxts(newtable))
	newtable=newtable['2007-01-26/']
	foisusd=foisusd['2007-01-26/']

	#foisusd = merge(newtable[,1],foisusd,join="left")[,-1]
	n <- dim(newtable)[1]
	idx <- index(newtable)
	newtable <- coredata(fillerxts(newtable))
	foisusd <- coredata(fillerxts(foisusd))
	S <- as.numeric(newtable[,z*5+1])
	vol <- newtable[,1:(z*5)]/100
	fwdpts <- newtable[,(z*5+2):(z*5+1+z)]/10^4
	if(substr(pair,4,6)=='USD'){
		rd = as.data.frame(-log(1/(1+foisusd[,1]*tau[1]))/tau[1]) #have to be defined externally
		for (i in 2:z) rd = cbind(rd,-log(1/(1+foisusd[,i]*tau[i]))/tau[i])
		rn = as.data.frame(-log((1+fwdpts[,1]/S)/exp(rd[,1]*tau[1]))/tau[1])
		for (i in 2:z) rn=cbind(rn,-log((1+fwdpts[,i]/S)/exp(rd[,i]*tau[i]))/tau[i])
		#fois <- newtable[,(z*6+3):(z*6+2+z)]/100
		#rn = -log(1/(1+fois[,1]*tau[1]))/tau[1]
		#for (i in 2:z) rn=cbind(rn,-log(1/(1+fois[,i]*tau[i]))/tau[i])
		#rd = log((1+fwdpts[,1]/S)*exp(rn[,1]*tau[1]))/tau[1]
		#for (i in 2:z) rd=cbind(rd,log((1+fwdpts[,i]/S)*exp(rn[,i]*tau[i]))/tau[i])
	}
	else{
		rn = -log(1/(1+foisusd[,1]*tau[1]))/tau[1] #have to defined externally
		for (i in 2:z) rn=cbind(rn,-log(1/(1+foisusd[,i]*tau[i]))/tau[i])
		rd = log((1+fwdpts[,1]/S)*exp(rn[,1]*tau[1]))/tau[1]
		for (i in 2:z) rd=cbind(rd,log((1+fwdpts[,i]/S)*exp(rn[,i]*tau[i]))/tau[i])
	}	
	Fwd <- S+fwdpts
	options = get_options_data(vol,Fwd,rd,rn,tau)
	K <- options[,1:(z*5)]
	mktPrice = options[,(z*5+1):(z*5*2)]
	vega <- options[,(z*5*2+1):(z*5*3)]
	mktPrice <- mktPrice*vega #due to function returning price per vega
	if(noxts) {rd<<-rd; rn<<-rn; S<<-S; Fwd<<-Fwd; K<<-K; vol<<-vol; mktPrice<<-mktPrice; vega<<-vega; idx<<-idx; n<<-n}
	else {rd<<-xts(rd,idx); rn<<-xts(rn,idx); S<<-xts(S,idx); Fwd<<-xts(Fwd,idx); K<<-xts(K,idx); vol<<-xts(vol,idx); mktPrice<<-xts(mktPrice,idx); vega<<-xts(vega,idx); idx<<-idx; n<<-n}
}
loadtable <- function(pair){
	vip_globals() #tau, z, k, vec, ttm, stm
	newtable = givemid(pair,z=8,t="mid",raw=FALSE,v=2)
	load_globals(newtable,z,pair,noxts=T)#rd,rn,S,Fwd,K,vol,mktPrice,vega
		
	rd<-xts(rd,idx); rn<-xts(rn,idx); S<-xts(S,idx); Fwd<-xts(Fwd,idx); K<-xts(K,idx); vol<-xts(vol,idx); mktPrice<-xts(mktPrice,idx); vega<-xts(vega,idx)

	ohlcret = log(local_load(pair,col=2)/local_load(pair,col=3))
	logret = diff(log(local_load((pair))))  
	hvol = emavol(logret,31)
	hlvol = emavol(ohlcret,31)
	#hvol = emavol(logret,63)
	hskew = smaskew(logret,31)
	hkurt = smaskew(logret,31)
	hvolp = emavolSem(logret,31,p=1,h=1)
	hvoln = emavolSem(logret,31,p=-1,h=1)
	dta = cbindname(hvol, hlvol, hvolp, hvoln, hskew,hkurt)
	newS = fillerxts(merge(S,dta,join="left"))
	sub = newS[paste0(idx[1],'/')][paste0('/',idx[n])]
	#wsk = c(SMA(vol[,3+(k-1)*5],45)-vol[,3+(k-1)*5])
	k=2 # row of 1M vols
	vol1watm = vol[,3+(k-2)*5]
	vol1matmLong = 1#SMA(vol[,3+(k-1)*5],252)
	vol1matmSMA2 = SMA(vol[,3+(k-1)*5],2)
	vol1mp10SMA2 = SMA(vol[,1+(k-1)*5],2)
	vol1mc10SMA2 = SMA(vol[,5+(k-1)*5],2)
	vol1matmEMA5 = EMA(vol[,3+(k-1)*5],1/5)
	vol1matmEMA15 = EMA(vol[,3+(k-1)*5],1/15)
	futS = lag(S,-stm)
	kVol = vol[,vec]
	colnames(kVol) = paste0('vol',tau[k]*12,'m',c("p10",'p25','atm','c25','c10'))
	kK = K[,vec]
	colnames(kK) = paste0('K',tau[k]*12,'m',c("p10",'p25','atm','c25','c10'))
	kPrice = mktPrice[,vec] #call only here, we have to modifiy first two cols by CP parity
	kPrice[,1] = kPrice[,1] - exp(-rd[,k]*ttm)*(Fwd[,k] - kK[,1])
	kPrice[,2] = kPrice[,2] - exp(-rd[,k]*ttm)*(Fwd[,k] - kK[,2])
	str10 = Black76(Fwd[,k],K[,1+(k-1)*5],vol[,1+(k-1)*5],tau[k],rd[,k],-1)+Black76(Fwd[,k],K[,5+(k-1)*5],vol[,5+(k-1)*5],tau[k],rd[,k],1)
	colnames(kPrice) = paste0('price',tau[k]*12,'m',c("p10",'p25','atm','c25','c10'))
	kVega = vega[,vec]
	colnames(kVega) = paste0('vega',tau[k]*12,'m',c("p10",'p25','atm','c25','c10'))
	colnames(Fwd)=paste0('F',substr(colnames(Fwd),4,5))
	gamma1mc10 = Gamma76(Fwd[,k],K[,5+(k-1)*5],vol[,5+(k-1)*5],tau[k],rd[,k])
	gamma1mp10 = Gamma76(Fwd[,k],K[,1+(k-1)*5],vol[,1+(k-1)*5],tau[k],rd[,k])
	tmp = cbindname(futS,vol1matmLong,vol1matmSMA2,vol1mp10SMA2,vol1mc10SMA2,vol1matmEMA5,vol1matmEMA15,vol1watm,gamma1mc10,gamma1mp10,str10)
	out = cbind(kVol,kK,kPrice,kVega,sub,Fwd[,k],tmp)
	out = cbind(idx,as.data.frame(out))
	out$pair = NA
	out$pair = pair
  
	divisor = ifelse(substr(out$pair,4,6)!='USD',out$S,1)
	futdivisor = ifelse(substr(out$pair,4,6)!='USD',out$futS,1)
	yy = -(pmax(out$futS-out$K1mc10,0)+pmax(out$K1mp10-out$futS,0))/futdivisor+(out$price1mc10+out$price1mp10)/divisor
	yyn = ifelse(substr(out$pair,4,6)!='USD',-(pmax(out$K1mp10-out$futS,0)/futdivisor)+(out$price1mp10)/divisor,-(pmax(out$futS-out$K1mc10,0)/futdivisor)+(out$price1mc10)/divisor)
	yyp = ifelse(substr(out$pair,4,6)!='USD',-(pmax(out$futS-out$K1mc10,0)/futdivisor)+(out$price1mc10)/divisor,-(pmax(out$K1mp10-out$futS,0)/futdivisor)+(out$price1mp10)/divisor)
	out = cbind(out,yy,yyn,yyp)
	out
}
loadlongtable <- function(){
	for(i in 1:length(ccypairs)){
		out = loadtable(ccypairs[i])
		if(i==1) dd=out
  		else dd=rbind(dd,out)
	}
	dd
}
loadspxtable<-function(){
	vip_globals() #tau, z, k, vec, ttm, stm
	#load vix as proxy
	vix_d = load_futures("CBOE_VX")/100
	vix_d = fillerxts(vix_d)
	vix_d2 = load_futures("CBOE_VX",2)/100
	vix_d2 = fillerxts(vix_d2)
	vix_d['/2007-03-23'] = vix_d['/2007-03-23']/10
	vix_d2['/2007-03-23'] = vix_d2['/2007-03-23']/10
	vix = ts2xts(read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=VIXCLS"))/100
	vix = fillerxts(vix)
	skew = read.csv("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/skewdailyprices.csv")
	skew=skew[,1:2]
	skew[,1]=as.character(skew[,1])
	skew[,2]=as.character(skew[,2])
	colnames(skew)=c(skew[1,])
	skew=skew[-1,]
	skew=ts2xts(skew,format="%m/%d/%Y")
  
	vol10p = vix*(1.1+0.05*(skew-120)/20) #modifier for 10d put
	vol10c = vix*(0.9-0.05*(skew-120)/20) #modifier for 10c put # not checked
	S = local_load("^spx")["1970-01-01/"]
	So = local_load("^spx",col=1)["1970-01-01/"]
	highs = local_load("^spx",col=2)["1970-01-01/"]
	lows = local_load("^spx",col=3)["1970-01-01/"]
	rd = ts2xts(read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=TB3MS"))/100#USD3MTD156N
  	rn = 0
  	#rn = usd_rate/usd_rate-1
  	## data cleaning
  	d = cbindname(S,vol10p,vol10c,vix,skew,rd,So,highs,lows,vix_d,vix_d2)
  	d[,2:6]=fillerxts(d[,2:6])
  	d=d[complete.cases(d[,2])]
  	d=d[complete.cases(d[,1])]
	#d=merge(d,vix_d,join="left")
  	#d=merge(d,vix_d2,join="left")
  	idx = index(d)
  	n=dim(d)[1]
  	##
  	logret = diff(log(d$S))
  	rvol = lag(smavol(logret,stm),-stm)
  	Fwd = d$S
  	futS = lag(d$S,-stm)
  	Kp = Fwd*exp(d$vol10p^2*tau[k]/2)*exp(d$vol10p*sqrt(tau[k])*qnorm(0.10/exp(-rn*tau[k])))
  	Kc = Fwd*exp(d$vol10c^2*tau[k]/2)*exp(-d$vol10c*sqrt(tau[k])*qnorm(0.10/exp(-rn*tau[k])))
  	put10 = Black76(Fwd,Kp,d$vol10p,tau[k],d$rd,-1)
  	put10r = Black76(Fwd,Kp,rvol,tau[k],d$rd,-1)
  	call10 = Black76(Fwd,Kc,d$vol10c,tau[k],d$rd,1)
  	pays = -pmax(Kp-futS,0)
  	paysC = -pmax(futS-Kc,0)
  	pays2 = -pmax(lag(Kp,stm)-d$S,0)
  	yyn = (put10+pays)/d$S
  	yyp = (call10+paysC)/d$S
  	yy = yyn + yyp
  	#strat2 = (put10+pays2)/lag(d$S,stm)

	ohlcret = log(highs/lows)
  	hvol = emavol(logret,31)
	hlvol = emavol(ohlcret,31)
	#hvol = emavol(logret,63)
	hskew = smaskew(logret,31)
	hkurt = smaskew(logret,31)
	hvolp = emavolSem(logret,31,p=1,h=1)
	hvoln = emavolSem(logret,31,p=-1,h=1)
	dta = cbindname(hvol, hlvol, hvolp, hvoln, hskew,hkurt)
	newS = fillerxts(merge(S,dta,join="left"))
	sub = newS[paste0(idx[1],'/')][paste0('/',idx[n])]
	vol1matmSMA2 = SMA(vix,2)
	vol1mp10SMA2 = SMA(vol,2)
	vol1mc10SMA2 = SMA(vol10c,2)
	vol1matmEMA5 = EMA(vix,1/5)
	vol1matmEMA15 = EMA(vix,1/15)

  	other = cbindname(rvol,futS,Kp,Kc,put10,call10,put10r,yyn,yyp,yy,vol1matmEMA5,vol1matmEMA15)

  	d = cbind(d,sub,other)
	d = d[complete.cases(d),]
  	d
}
lagged<-function(x){c(NA,x[-length(x)])}
get_Splines<-function(n){
	out = vector("list", n)
	for(i in 1:n){
		out[[i]] = splinefun(x=log(K[i,vec]/as.numeric(Fwd[i,k])), y=as.numeric(vol[i,vec]), method="natural",  ties = mean)
	}
	out
}
get_SABRs<-function(n){
	out = matrix(rep(NA,n*3),n,3)
	#params=c(log(0.1),atanh(0),log(1))
	for(i in 1:n){
		params = c(log(0.1),atanh(0),log(1))
		opt = optim(params,fn=SSE_SABRlim,sigma.Mkt=as.numeric(vol[i,vec]),f=as.numeric(Fwd[i,k]),K=K[i,vec],T=tau[k],beta=1)
		out[i,] = c(exp(opt$par[1]),tanh(opt$par[2]),exp(opt$par[3]))
		#params=out[i,] 
	}
	out
}
norm_func <- function(y){
	y = y-min(y,na.rm=T)
	y = y/max(y,na.rm=T)
	y = filler_vec(y)
	y = filler_vec(y[length(y):1])
	y[length(y):1]
}
decrease_cap <- function(y,begin=0){
	n = length(y)
	out = numeric(n)
	if(begin==1){
		out[1]=y[1]
		for (i in 2:n){
			if(y[i]<out[i-1])
				out[i] = y[i]
			else
				out[i] = out[i-1]
		}
	}
	else{
		out[n]=y[n]
		for (i in (n-1):1){
			if(y[i]<out[i+1])
				out[i] = y[i]
			else
				out[i] = out[i+1]
		}		
	}
	out
}
closest = function(x,mat,prev=1,id=1,growing=1){
	if(growing==1) y = which(mat-x>0)[1]
	else  y = which(mat-x<0)[1]
	if(id==1) y-prev
	else mat[y-prev]
}
empcdf <- function(emppdf){
	values = cumsum(emppdf)
	values/values[length(values)]
}
test_line <- function(i,ver=1,type="volspline",euro=0){
	#fret = log(S[i+stm]/S[i])
	subsample = logret[paste0(datatable[i,1]-45,'/')][paste0('/',datatable[i,1])]
	lx <- seq(0.99*min(log(K[i,vec]/Fwd[i,k])),1.01*max(log(K[i,vec]/Fwd[i,k])),jump)
	invcdf = impdistr(i,lx,cum=-1,type="volspline")
	original = invcdf(runif(10000))
	newsample = histMC((as.numeric(coredata(subsample))),ttm*5/7*365,100000)#+mean(original,na.rm=T)
	boundaries = vol[i,3+(k-1)*5]^2*ttm/2 + vol[i,3+(k-1)*5]*sqrt(ttm)*qnorm(c(0.001,0.999)/exp(-as.numeric(rn[i,k])*ttm))
	lx <- seq(min(c(boundaries,newsample),na.rm=T),max(c(boundaries,newsample),na.rm=T),jump)
	if(ver==2) lx <- seq(0.99*min(log(K[i,vec]/Fwd[i,k])),1.01*max(log(K[i,vec]/Fwd[i,k])),jump)
	x <- exp(lx)*Fwd[i,k]

	#prms = impdistr(i,lx,cum=2,type="volspline",cp=-1)
	pvols = impdistr(i,lx,cum=3,type=type,capped=1)
	prms = Black76(Fwd[i,k],x,pvols(lx),ttm,rd[i,k],-1)
	marprms = ifelse(x>Fwd[i,k],prms + exp(-rd[i,k]*ttm)*(Fwd[i,k] - x),prms)	

	if(ver==0) {
		d = density(newsample)
		d1 = approxfun(d$x, d$y, yleft=0, yright=0)
		ipdf = impdistr(i,lx,cum=0,type=type)
		#cipdf = function(x) ifelse(ipdf(x)<0,0,ipdf(x))		
		#t1=lx[which((cipdf(lx)-d1(lx))==closest(0,cipdf(lx)-d1(lx)))]
		t1=argmax(lx,d1(lx)-ipdf(lx))
		t2=argmax(lx,ipdf(lx)-d1(lx))
		#t1=lx[which(lx==t1)-5]
	}
	else if(ver==1) {
		cdf0 = stepfun(x=sort(newsample), y=(0:sum(!is.na(newsample)))/sum(!is.na(newsample)))
		cdf = impdistr(i,lx,cum=1,type=type)
		ccdf = function(x) ifelse(cdf(x)<0,0,cdf(x))
		t1=argmax(lx,cdf0(lx)-ccdf(lx))
		t2=argmax(lx,ccdf(lx)-cdf0(lx))
	}
	else if(ver==2) {
		newprices = cdf.price(S[i],x,ttm,newsample,rd=rd[i,k],rn=rn[i,k],type=1,upper=log(1.7))
		newprices = ifelse(x<Fwd[i,k],newprices - exp(-rd[i,k]*ttm)*(Fwd[i,k] - x),newprices)
		t1=argmax(lx,marprms-newprices)
		t2=argmax(lx,newprices-marprms)
		#t1=argmax(lx,log(round(marprms,4)/round(newprices,4)))
	}
	#p10d = closest(log(K[i,1]/S[i]),lx)
	cp = as.numeric(t1>0)*2-1
	cp2 = as.numeric(t2>0)*2-1
	#pvols = impdistr(i,lx,cum=3,type=type)
	d = Delta76(Fwd[i,k],exp(t1)*Fwd[i,k],pvols(t1),ttm,rd[i,k],cp)
	d2 = Delta76(Fwd[i,k],exp(t2)*Fwd[i,k],pvols(t2),ttm,rd[i,k],cp2)
	out=numeric(8)
	out[1] = marprms[which(lx==t1)]
	out[2] = -payme(i,cp,exp(t1)*Fwd[i,k],pvols(t1),euro) #- max(cp*(S[i+stm]-exp(t1)*S[i]),0)
	out[3] = d
	out[4] = t1
	out[5] = -marprms[which(lx==t2)]
	out[6] = payme(i,cp2,exp(t2)*Fwd[i,k],pvols(t2),euro) # max(cp*(S[i+stm]-exp(t2)*S[i]),0)
	out[7] = d2
	out[8] = t2
	vec = 1:stm#deltame(i,cp,exp(t1)*S[i],pvols(t1),cost=0)
	vec2 = 1:stm#deltame(i,cp2,exp(t2)*S[i],pvols(t2),cost=0)
	list(out,vec,vec2)
}
tester = function(n,ver=1,type="volspline",euro=0,loud=0){
	options(warn=-1)
	k = 1
	vec=k*(1:5)
	ttm = tau[k]
	stm = floor(ttm*5/7*365)
	out = matrix(rep(NA,(n+stm)*10),(n+stm),10)
	out[,9] = rep(0,(n+stm))
	out[,10] = rep(0,(n+stm))
	for(i in 1:n) {
		if(loud==1) if(i %% 10 == 0) print(i)
		check = try(test_line(i,ver,type,euro))
		if (!is.character(check)) {
			out[i,1:8] = check[[1]]
			for(j in 1:stm)
				out[i+j,9] = out[i+j,9] + check[[2]][j]
				out[i+j,10] = out[i+j,10] + check[[3]][j]
		}
	}
	options(warn=0)
	colnames(out)=c('premium','payout','delta','mon','premium2','payout2','delta2','mon2','hedge','hedge2')
	for(j in 1:8){
		if(j!=2 & j!=6)	out[,j] = c(out[(stm+1):n,j],rep(NA,stm*2))
	}
	out[,9] = c(out[(stm+1):(n+stm),9],rep(NA,stm))
	out[,10] = c(out[(stm+1):(n+stm),10],rep(NA,stm))
	out[1:n,]
}
opt1_line <- function(i,cp=-1,d=-0.1,type="volspline",means=0,euro=0,wsk=1){
	#simple strategy buing only 1 option with given delta
	options(warn=-1)
	#fret = log(S[i+stm]/S[i])
	subsample = logret[paste0(idx[i]-45,'/')][paste0('/',idx[i])]
	newsample = demean(as.numeric(coredata(subsample)))*sqrt(ttm*5/7*365)
	boundaries = vol[i,3+(k-1)*5]^2*ttm/2 + vol[i,3+(k-1)*5]*sqrt(ttm)*qnorm(c(0.001,0.999)/exp(-as.numeric(rn[i,k])*ttm))
	lx <- seq(min(c(boundaries,newsample),na.rm=T),max(c(boundaries,newsample),na.rm=T),jump)
	x <- exp(lx)*Fwd[i,k]

	#prms = impdistr(i,lx,cum=2,type=type)
	pvols = impdistr(i,lx,cum=3,type=type,capped=1)
	#cp = as.numeric(t1>Fwd[i,k])*2-1
	prms = Black76(Fwd[i,k],x,pvols(lx),ttm,rd[i,k],-1)
	marprms = ifelse(x>Fwd[i,k],prms + exp(-rd[i,k]*ttm)*(Fwd[i,k] - x),prms)

	if(length(means)!=1) cp=-sign(means)
	else cp = rep(cp,n)

	deltas = Delta76(Fwd[i,k],x,pvols(lx),ttm,rd[i,k],cp[i])
	place = closest(d,deltas,id=1,growing=cp[i])
	if (is.na(place)) place = length(deltas)
	if (place==0) place = 1
	t1 = lx[place]
	#print(paste(i,place,marprms[place]))
	out=numeric(8)
	if(wsk>0) {
		out[1] = marprms[place]
		out[2] = -payme2(i,cp[i],exp(t1)*Fwd[i,k],pvols(t1),euro) #- max(cp[i]*(S[i+stm]-exp(t1)*S[i]),0)
		out[3] = deltas[place]*(S[i+stm]-S[i]) #deltas[place] #*cp[i]*(S[i+stm]-exp(t1)*S[i])
		out[4] = t1
		vec = sum(deltame(i,cp[i],exp(t1)*S[i],pvols(t1),cost=0)) #out[9] = ??
		
		deltas = Delta76(Fwd[i,k],x,pvols(lx),ttm,rd[i,k],-cp[i])
		place = closest(-d,deltas,id=1,growing=cp[i])
		if (is.na(place)) place = length(deltas)	
		if (place==0) place = 1
		t1 = lx[place]		
		out[5] = marprms[place]
		out[6] = -payme2(i,-cp[i],exp(t1)*Fwd[i,k],pvols(t1),euro) #- max(-cp[i]*(S[i+stm]-exp(t1)*S[i]),0)
		out[7] = deltas[place]*(S[i+stm]-S[i]) #deltas[place] #*cp[i]*(S[i+stm]-exp(t1)*S[i])
		out[8] = t1
		vec2 = sum(deltame(i,cp[i],exp(t1)*S[i],pvols(t1),cost=0)) #out[10] = 
	}
	else{
		out = rep(NA,8)
		vec = NA
		vec2 = NA
	}
	options(warn=0)
	list(out,vec,vec2)
}
opt1tester <- function(n,cp=1,d=0.5,type="volspline",means=0,euro=0,wsk=1,shift=1,loud=0){
	#simple strategy buing only 1 option with given delta
	cp=sign(d)
	options(warn=-1)
	sink("opt_log.txt")
	out = matrix(rep(NA,(n+stm)*10),(n+stm),10)
	out[,9] = rep(0,(n+stm))
	out[,10] = rep(0,(n+stm))
	#k=1
	#vec=k*(1:5)
	#ttm = tau[k]
	#stm = floor(ttm*5/7*365)
	if(length(d)==1) d = rep(d,n)
	if(length(wsk)==1) wsk = rep(1,n)
	for(i in 1:n) {
		#if(loud==1) if(i %% 10 == 0) print(i)
		#check = try(opt1_line(i,cp,d[i],type,means,euro,wsk[i]))
		check = tryCatch({options(warn=-1);opt1_line(i,cp,d[i],type,means,euro,wsk[i])}, error = function(e) {print(paste('error:', e))})
		if (!is.character(check)) {
			out[i,1:8] = check[[1]]
			#for(j in 1:stm){
			#	out[i+j,9] = out[i+j,9] + check[[2]][j]
			#	out[i+j,10] = out[i+j,10] + check[[3]][j]
			#}
			out[i,9] = check[[2]]
			out[i,10] = check[[3]]
		}
		else
			print(i)
	}
	options(warn=0)
	sink(type = "message")
	sink()
	colnames(out)=c('premium','payout','delta','mon','premium2','payout2','delta2','mon2','hedge','hedge2')
	if(shift==1) shifter(out)
	else out #[1:n,]
}
shifter <- function(out){
	#out2 = matrix(rep(NA,(stm)*10),(stm),10)
	#out = rbind(out,out2)
	for(j in 1:8){
		if(!(j %in% c(2,3,6,7)))	out[,j] = c(out[(stm+1):n,j],rep(NA,stm*2))
		#if(j==2 & j==6) out[,j] = c(rep(NA,stm),out[1:n,j])
	}
	out[,9] = c(out[(stm+1):(n+stm),9],rep(NA,stm))
	out[,10] = c(out[(stm+1):(n+stm),10],rep(NA,stm))
	out[1:n,]	
	#What's the loss? Options from the 1:stm period are undefined because we don't know the payout that should accompany them for the previous options, which are unknown.
}
global_strat<-function(pair){
	logret = diff(log(local_load((pair))))
	hvol = emavol(logret,31)
	#hvol = emavol(logret,63)
	hskew = smaskew(logret,63)
	hvolp = emavolSem(logret,63,p=1,h=1)
	hvoln = emavolSem(logret,63,p=-1,h=1)
	newS = fillerxts(merge(xts(S,idx),hvol,hvolp,hvoln,hskew))
	sub = as.data.frame(coredata(newS[paste0(idx[1],'/')][paste0('/',idx[n])]))
	#wsk = c(SMA(vol[,3+(k-1)*5],45)-vol[,3+(k-1)*5])
	wsk = c(EMA(vol[,3+(k-1)*5],1/45)-as.numeric(sub$hvol)); wsk=c(wsk,rep(1,stm));	wsk[1:44] = rep(0,44)
	wskP = c(EMA(vol[,3+(k-1)*5],1/45)-as.numeric(sub$hvolp)); wskP=c(wskP,rep(1,stm)); wskP[1:44] = rep(0,44)
	wskN = c(EMA(vol[,3+(k-1)*5],1/45)-as.numeric(sub$hvoln)); wskN=c(wskN,rep(1,stm)); wskN[1:44] = rep(0,44)
	wsk2 = sub$hvolp/sub$hvoln-1

	divisor = 1
	if(substr(pair,1,3)=="USD") divisor = S
	simple1 = opt1tester(n,cp=-1,d=-0.1,means=0,euro=0,shift=0)
	simple2 = shifter(sweep(simple1,1,ifelse(wsk>0,0,NA),'+'))
	test  = bindingCF(simple2[,c(1:2,5:6)])/divisor
	testB = ifelse(wsk2>0,bindingCF(simple2[,1:2]),bindingCF(simple2[,5:6]))/divisor
	testS = ifelse(sub$hskew>0,bindingCF(simple2[,1:2]),bindingCF(simple2[,5:6]))/divisor
	simple2 = shifter(sweep(simple1,1,ifelse(wskN>0,0,NA),'+'))
	testN = bindingCF(simple2[,1:2])/divisor
	simple2 = shifter(sweep(simple1,1,ifelse(wskP>0,0,NA),'+'))
	testP = bindingCF(simple2[,5:6])/divisor
	as.data.frame(cbind(test,testB,testS,testN,testP))
}
deltame <- function(i,cp=1,strike,vol1,cost=0) {
  times = ttm-((0:(stm-1))+2*round((0:(stm-1))/5,0))/365
  spots = S[i:(i+stm)]
  fwdptss = spots[1:stm]*exp((rd[i,k]-rn[i,k])*times)
  deltas = Delta76(fwdptss,strike,vol1,times,rd[i,k],cp)
  #exp(logret[paste0(datatable[i,1],'/')][paste0('/',datatable[i,1])])
  deltas * diff(spots)
}
unwindme <- function(i,cost=0) {
  times = ttm-((0:(stm-1))+2*round((0:(stm-1))/5,0))/365
  spots = as.numeric(coredata(S[i:(i+stm)]))
  callK = as.numeric(coredata(K[i,5+(k-1)*5]))
  putK = as.numeric(coredata(K[i,1+(k-1)*5]))
  fwdptss = spots[1:stm]*exp((rd[i,k]-rn[i,k])*times)
  volsP = as.numeric(coredata(vol[i:(i+stm-1),1+(k-1)*5]))
  volsC = as.numeric(coredata(vol[i:(i+stm-1),5+(k-1)*5]))
  unwind = Black76(fwdptss,putK,volsP,times,rd[i,k],-1) + Black76(fwdptss,callK,volsC,times,rd[i,k],1)
  min(coredata(unwind))
}
payme <- function(i,cp=1,strike,vol1,euro=0) {
  vols1 = vol1 + diff(vol[i:(i+stm-1),3+(k-1)*5])
  times = ttm-((1:(stm-1))+2*round((1:(stm-1))/5,0))/365
  spots = S[(i+1):(i+stm-1)]
  fwdptss = spots*exp((rd[i,k]-rn[i,k])*times)
  prices = Black76(fwdptss,strike,vols1,times,rd[i,k],cp)
  pays = pmax(cp*(spots-strike),0)
  out = pays-prices*1
  if(length(which(out>0))==0 | euro==1) max(cp*(S[i+stm]-strike),0)
  else pays[which(out>0)[1]]
}
payme2 <- function(i,cp=1,strike,vol1,euro=0) {
  vols1 = vol1 + diff(vol[i:(i+stm-1),3+(k-1)*5])
  times = ttm-((1:(stm-1))+2*round((1:(stm-1))/5,0))/365
  spots = S[(i+1):(i+stm-1)]
  fwdptss = spots*exp((rd[i,k]-rn[i,k])*times)
  prices = Black76(fwdptss,strike,vols1,times,rd[i,k],cp)
  pays = pmax(cp*(spots-strike),0)
  if(euro==1) max(cp*(S[i+stm]-strike),0)
  else sum(pmax(pays-prices*1,0))+max(cp*(S[i+stm]-strike),0)
}
payme3 <- function(i,cp=1,strike,vol1,euro=0) {
  range = (i+1):(i+stm-1)
  times = ttm-((1:(stm))+2*round((1:(stm-1))/5,0))/365
  spots = S[range]
  fwdptss = spots*exp((rd[i,k]-rn[i,k])*times)
  vols1 = sigma_SABR(f=fwdptss,K=strike,T=times,beta=1,SABRs[range,1],SABRs[range,2],SABRs[range,3],ref = FALSE)  
  prices = Black76(fwdptss,strike,vols1,times,rd[i,k],cp)
  pays = pmax(cp*(spots-strike),0)
  if(euro==1) max(cp*(S[i+stm]-strike),0)
  else sum(pmax(pays-prices*1,0))+max(cp*(S[i+stm]-strike),0)
}
get_exmeans <- function(n,type="volspline",capped=0,p=0){
	out <- matrix(rep(NA, n*3),n,3)
	for(i in 1:n) {
		if(sum(is.na(vol[i,vec]))==0){
			lx <- seq((1-p)*log(min(K[i,vec],na.rm=T)/as.numeric(Fwd[i,k])),(1+p)*log(max(K[i,vec],na.rm=T)/as.numeric(Fwd[i,k])),jump)
			invcdf = try(impdistr(i,lx,cum=-1,type=type,capped=capped))
			if (!is.character(invcdf)) {
				original = invcdf(runif(10000)) #getSampleOnCDF(cdf,10000)
				out[i,] = c(mean(original,na.rm=T),skewness(original,na.rm=T),kurtosis(original,na.rm=T))
			}
		}
	}
	out
}
distraggregator <- function(n,type="volspline"){
	out <- vector("list", n)
	k = 1
	vec=k*(1:5)
	ttm = tau[k]
	stm = floor(ttm*5/7*365)
	for(i in 1:n) {	
		x <- seq(min(K[i,vec]),max(K[i,vec]),0.001)
		lx <- log(x/S[i])
		subsample = logret[paste0(datatable[i,1]-45,'/')][paste0('/',datatable[i,1])]
		invcdf = impdistr(i,lx,cum=-1)
		original = invcdf(runif(10000)) #getSampleOnCDF(cdf,10000)
		newsample = histMC(demean(as.numeric(coredata(subsample))),ttm*5/7*365,100000)+mean(original,na.rm=T)
		lx <- seq(min(newsample),max(newsample),0.001)
		x <- exp(lx)*S[i]

		d = density(newsample)
		#d1 = splinefun(x=d$x, y=d$y, method="natural",  ties = mean)
		d1 = approxfun(d$x, d$y, yleft=0, yright=0)
		ipdf = impdistr(lx,cum=0,type=type)
		#ipdf2 = impdistr(lx,cum=0,type="volparabolic")
		#cipdf = function(x) ifelse(ipdf(x)<0,0,ipdf(x))	
		out[[i]] = list("lx" = lx, "impl" = ipdf, "hist" = d1)	
	}
	out
}
supernun1 <- function(x,sets,cap=1){
	out = 0
	n = length(sets)
	for(i in 1:n){
		if(cap==0)
			out = out + sets[[i]]$impl(x)
		else{
			val = sets[[i]]$impl(x)
			val = ifelse(val<0,0,val)
			out = out + val
		}
	}
	out/n
}
supernun2 <- function(x,sets){
	out = 0
	n = length(sets)
	for(i in 1:n){
		out = out + sets[[i]]$hist(x)
	}
	out/n
}
superminmax <- function(sets,cap=1){
	min1 = sets[[1]]$lx[1]
	max1 = sets[[1]]$lx[1]
	n = length(sets)
	for(i in 1:n) {
		min1 = cap*min(c(min1,sets[[i]]$lx))
		max1 = cap*max(c(max1,sets[[i]]$lx))
	}
	seq(min1,max1,0.0001)
}
cpparity = function(call,Fwd,K,r,T){
	call-exp(-r*T)*(Fwd-K)
}
get_options_data = function(vol,Fwd,rd,rn,tau,noxts=FALSE){
	m = dim(vol)[1]
	n = length(tau)
	out = matrix(NA,m,z*5*3)
	for (k in 1:m) {
		volvec=t(matrix(as.numeric(vol[k,]),nrow = 5,ncol = n))
		K10p=as.numeric(Fwd[k,])*exp(volvec[,1]^2*tau/2)*exp(volvec[,1]*sqrt(tau)*qnorm(0.10/exp(-as.numeric(rn[k,])*tau)))
		K25p=as.numeric(Fwd[k,])*exp(volvec[,2]^2*tau/2)*exp(volvec[,2]*sqrt(tau)*qnorm(0.25/exp(-as.numeric(rn[k,])*tau)))
		Katmf=as.numeric(Fwd[k,])*exp(volvec[,3]^2*tau/2)
		K25c=as.numeric(Fwd[k,])*exp(volvec[,4]^2*tau/2)*exp(-volvec[,4]*sqrt(tau)*qnorm(0.25/exp(-as.numeric(rn[k,])*tau)))
		K10c=as.numeric(Fwd[k,])*exp(volvec[,5]^2*tau/2)*exp(-volvec[,5]*sqrt(tau)*qnorm(0.10/exp(-as.numeric(rn[k,])*tau)))
		K=t(rbind(K10p,K25p,Katmf,K25c,K10c))
		mktPrice=matrix(0,n,5)
		vega=matrix(0,n,5)
		for(i in 1:n){
			for(j in 1:5){
				d1 <- (log(as.numeric(Fwd[k,i]/K[i,j])) + (volvec[i,j]^2/2)*tau[i])/(volvec[i,j]*sqrt(tau[i]))
				d2 <- d1 - volvec[i,j]*sqrt(tau[i])
				vega[i,j] <- K[i,j]*exp(-as.numeric(rd[k,i])*tau[i])*sqrt(tau[i])*dnorm(d2)
				typeo = 1
				mktPrice[i,j] = (typeo * exp(-as.numeric(rd[k,i])*tau[i])*(as.numeric(Fwd[k,i])*pnorm(typeo*d1)-K[i,j]*pnorm(typeo*d2)))/vega[i,j]
			}
		}
		out[k,] = c(as.numeric(t(K)),as.numeric(t(mktPrice)),as.numeric(t(vega)))
	}
	if(noxts) xts(out,index(vol))
	else out
}
cdf.price<-function(S,K,T,returns,rd=0,rn=0,type=1,upper=7,method="lin") {
  #xy=density(log(S)+(returns+(rd-rn)/252-mean(returns))*sqrt(252*T))
  xy=density(log(S)+(rd-rn)*ttm+(returns))
  if(method=="lin") f<- approxfun(xy[1]$x, xy[2]$y, yleft=0, yright=0)
  else if(method=="spline") f<- spline(xy[1]$x, xy[2]$y, method="natural",  ties = mean)
  else {
    #dx <- d$x[2L] - d$x[1L]
    #C <- sum(d$y[d$x >= a]) * dx  ## sum(yy * dx)
  }
  n = length(K)
  #upper = min(upper,max(returns)+log(S))
  call = rep(NA,n)
  for(i in 1:n)
    call[i] = exp(-rd*T)*integrate(function(u) (exp((rd-rn)*T)*exp(u)-K[i])*f(u),log(K[i]),upper)$value
  if (type==1) 
    call
  else
    call - S + K*exp(-rd*T)
}
Black76 <- function(futures,strike,vol,T,rate,oType){
  d1=(log(futures/strike)+0.5*T*vol^2)/(sqrt(T)*vol)
  d2=d1-vol*sqrt(T)
  exp(-rate*T)*(oType*futures*pnorm(oType*d1)-oType*strike*pnorm(oType*d2))}
Delta76 <- function(Fwd,K,v,T,r,oType) {
  d1 = (log(Fwd / K) + (v ^ 2 / 2) * T) / (v * sqrt(T))
  oType*exp(-r * T) * pnorm(oType*d1)
}
Gamma76 <- function(Fwd,K,v,T,r,oType=1) {
  d2 = (log(Fwd / K) + (v ^ 2 / 2) * T) / (v * sqrt(T)) - v * sqrt(T)
  K*exp(-r*T)*dnorm(d2)/S^2/v/sqrt(T)
}
Vega76 <- function(Fwd,K,v,T,r,oType=1) {
  d1 = (log(Fwd / K) + (v ^ 2 / 2) * T) / (v * sqrt(T))
  dnorm(d1)*sqrt(T)*exp(-r*T)*Fwd
}
Speed76 <- function(Fwd,K,v,T,r,oType=1) {
  d1 = (log(Fwd / K) + (v ^ 2 / 2) * T) / (v * sqrt(T))
  g = Gamma76(Fwd,K,v,T,r)
  S = Fwd*exp((rn[,k]-r)*T)
  -g/S*(d1/v/sqrt(T)+1)
}
Volga76 <- function(Fwd,K,v,T,r,oType=1) {
  vv = Vega76(Fwd,K,v,T,r)
  d1 = (log(Fwd / K) + (v ^ 2 / 2) * T) / (v * sqrt(T))
  d2 = d1 - v * sqrt(T)
  vv*d1*d2/v
}
Vanna76 <- function(Fwd,K,v,T,r,oType=1) {
  vv = Vega76(Fwd,K,v,T,r)
  d1 = (log(Fwd / K) + (v ^ 2 / 2) * T) / (v * sqrt(T))
  d2 = d1 - v * sqrt(T)
  -dnorm(d1)*d2/v*exp(-rn[,k]*T)
}
delta <- function(futures,strike,vol,T,rate,oType,d=0.1){(Black76(futures+d,strike,vol,T,rate,oType)-Black76(futures-d,strike,vol,T,rate,oType))/(2*d)}
Black76ivol <- function(market,futures,strike,T,rate,oType) {
  sig <- 0.20
  sig.up <- 1
  sig.down <- 0.001
  count <- 0
  err <- Black76(futures,strike,sig,T,rate,oType) - market 
  
  ## repeat until error is sufficiently small or counter hits 1000
  while(abs(err) > 0.00001 & count<1000){
    if(err < 0){
      sig.down <- sig
      sig <- (sig.up + sig)/2
    }else{
      sig.up <- sig
      sig <- (sig.down + sig)/2
    }
    err <- Black76(futures,strike,sig,T,rate,oType) - market 
    count <- count + 1
  }
  
  ## return NA if counter hit 1000
  if(count==1000){
    return(NA)
  }else{
    return(sig)
  }
}
impdistr_eq <- function(vols,cum=0,type="volparabolic") {
  if(type=="volparabolic") {
    pvols = pmax(0.01,unname(parabolic(lx,log(otm$strike/spot),vols)))
    prms = Black76(spot,x,pvols,ttm,0,1)
  }
  if(type=="volspline") {
    pvols = splinefun(x=otm$strike, y=vols, method="fmm",  ties = mean)
    prms = Black76(spot,x,pmax(0.01,pvols(x)),ttm,0,1)
  }
  if(type=="prmparabolic") {
    prms = pmax(0.01,unname(parabolic(lx,log(otm$strike/spot),vols))) #here vols variable should contain prms
  }
  if(type=="prmspline") {
    prms = splinefun(x=otm$strike, y=vols, method="fmm",  ties = mean) #here vols variable should contain prms
    prms = pmax(0.01,prms(x))
  } 
  cdf = c(NA,diff(prms)/0.25)
  #cdf[1] = cdf[2]
  pdf = c(NA,diff(cdf)/0.25)
  func = splinefun(x=lx, y=pdf, method="natural",  ties = mean)
  fac = as.numeric(integrate(func,min(lx),max(lx))[1])
  if(cum==0) return(splinefun(x=lx, y=pdf/fac, method="natural",  ties = mean))
  else if(cum==1) return(splinefun(x=lx, y=1+cdf, method="natural",  ties = mean))
  else if(cum==-1) return(approxfun(x=1+cdf, y=lx))
  #return(stepfun(x=1+cdf[-1], y=lx))}
  #return(splinefun(x=1+cdf, y=lx, method="natural",  ties = mean))}
}