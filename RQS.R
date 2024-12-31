source("core_lib.R")
library(xts)
library(moments)

rolltrend <- function(y)  {
  n = dim(y)[1]
  deviation = rep(NA,n)
  s = sum(is.na(y))+1
  for(i in (s+24):n) {
    x = y[1:i]
    regset = cbind(lag(x,-24),x)
    colnames(regset) = c('h24','l0')
    lm1 = lm(h24~l0,data=regset)
    deviation[i]=last(residuals(lm1))
  }
  deviation
}
add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]
rollmarkovitz <- function(strats,start,h=24) {
  s = which(as.Date(index(a))==as.Date(start))
  n = dim(strats)[1]
  m = dim(strats)[2]
  mar = matrix(NA,n,m)
  k = 1#firstcompleteobs(strats)
  for(i in 1:s) {
    mar.covar=histcorrmat(strats[k:s,],63) #cor(strats[1:s,],use="complete.obs")
    mar[i,] = markovitz(mar.covar)
  }
  for(i in (s+1):n) {
    mar.covar= histcorrmat(strats[k:(i-1),],63) #cor(strats[(i-1-h):(i-1),],use="complete.obs")
    mar[i,] = markovitz(mar.covar)
  }
  mar
}
markovitz <- function(covar){
  k = dim(covar)[2]
  i=rep(1,k)
  b=c(rep(0,k),1)
  A = cbind(2*covar,i)
  A = rbind(A,c(i,0))
  A_inv=solve(A)
  z = A_inv%*%b
  z[1:k]
  #s_inv=solve(covar)
  #s_inv%*%i / as.numeric(i%*%s_inv%*%i)
}
markovitzrf <- function(covar,rf=0,target=0.15,mju=0){
  k = dim(covar)[2]
  i=rep(1,k)
  s_inv=solve(covar)
  if(mju==0) mjubis = rep(target,k) - rf
  else mjubis = mju - rf
  targetbis = target - rf
  #as.numerictargetbis * s_inv%*%mjubis / as.numeric(mjubis%*%s_inv%*%mjubis)) #optimal x
  as.numeric(s_inv%*%mjubis / as.numeric(i%*%s_inv%*%mjubis)) #tangent portfolio
}
markovitzCAPM <- function(volm,b,mju=0.15,rf=0){
  k = length(b)
  i=rep(1,k)
  sigma=b%*%t(b)*volm^2+diag(k)*0.001
  sigma_inv=solve(sigma)
  mjum=0
  xsr=rf*i+(mjum-rf)*b
  A=as.numeric(t(i)%*%sigma_inv%*%xsr)
  B=as.numeric(t(xsr)%*%sigma_inv%*%xsr)
  C=as.numeric(t(i)%*%sigma_inv%*%i)
  H=B-2*A*rf+C*rf^2
  w_star=(sigma_inv%*%(xsr-i*rf))*as.numeric((mju-rf)/H)
  rest=1-sum(w_star)
  
  list(rest,w_star)
  #sum(w_star*(p2/p0-1))+rest*rf/12
  #to overcome inverting of diffcult matrices
  #v=eigen(sigma)$vectors
  #d=eigen(sigma)$values
  #s2=v%*%diag(d)%*%solve(v)
  #s2inv=v%*%diag(d^-1)%*%solve(v)
  #solve(sigma)
}
#library(PortfolioAnalytics)
minvaropt <- function(returns) {
  # Start with the names of the assets
  port <- portfolio.spec(assets = colnames(returns))
  # Box
  port <- add.constraint(port, type = "box", min = 0.05, max = 0.8)
  # Leverage
  port <- add.constraint(portfolio = port, type = "full_investment")
  # Generate random portfolios
  rportfolios <- random_portfolios(port, permutations = 500000, rp_method = "sample")
  # Get minimum variance portfolio
  minvar.port <- add.objective(port, type = "risk", name = "var")
  # Optimize
  minvar.opt <- optimize.portfolio(returns.data, minvar.port, optimize_method = "random", rp = rportfolios)
  minvar.opt
}
maxretopt <- function(returns) {
  # Start with the names of the assets
  port <- portfolio.spec(assets = colnames(returns))
  # Box
  port <- add.constraint(port, type = "box", min = 0.05, max = 0.8)
  # Leverage
  port <- add.constraint(portfolio = port, type = "full_investment")  
  # Generate random portfolios
  rportfolios <- random_portfolios(port, permutations = 500000, rp_method = "sample")  
  # Generate maximum return portfolio
  maxret.port <- add.objective(port, type = "return", name = "mean")
  # Optimize
  maxret.opt <- optimize.portfolio(returns.data, maxret.port, optimize_method = "random", rp = rportfolios)
  maxret.opt
}
ranker <- function(returns0,criteria0,s,bpsspread,ycost,f) {
  #colnames(criteria0)=colnames(returns0)
  returns = coredata(returns0)
  criteria = coredata(criteria0)
  m = dim(returns)[1]
  performance=rep(NA,m)
  costs = bpsspread/10000+ycost/f
  for (i in s:m) {
    if(sum(as.numeric(is.na(criteria[i,])))==0) {
      criterialocal = criteria[i,]
      returnslocal = returns[i,]
      costslocal = costs
    } else {
      criterialocal = criteria[i,-which(is.na(criteria[i,]))]
      returnslocal = returns[i,-which(is.na(criteria[i,]))]
      costslocal = costs[-which(is.na(criteria[i,]))]
    }
    
    n = length(returnslocal)
    if(n>0){
      weighting = (median(1:n)-(1:n))/(ceiling(median(1:n))*(ceiling(median(1:n))-1)/4+floor(median(1:n))*(floor(median(1:n))-1)/4)
      sorted.returns=returnslocal[order(-criterialocal)]
      sorted.costs = costslocal[order(-criterialocal)]
      p1 = 1:floor(n/2)
      p2 = ceiling(n/2):n
      performance[i]=mean(sorted.returns*weighting,na.rm=T)-mean(sorted.costs*abs(weighting),na.rm=T)
    }
    else
      performance[i]=NA
  }
  #weighted.mean(as.numeric(sorted.returns[1:floor(n/2)]),w=weighting[1:floor(n/2)],na.rm=T)
  #-weighted.mean(as.numeric(sorted.returns[ceiling(n/2):n]),w=weighting[ceiling(n/2):n],na.rm=T)
  #performance=xts(performance, order.by=as.Date(as.character(gsub("-", "", rownames(returns[(t.start:t.end),]))),format="%Y%m%d"))
  performance = xts(performance,index(returns0))
  return(performance)
  #if (alloc==0) else return(alloc)
}
allocationRF <- function(criteria0,returns,s,k,direction,pos=0) {
  returns = coredata(merge(criteria0[,1], returns, join="left")[,-1])
  criteria = coredata(criteria0)
  m = dim(criteria)[2]
  n = dim(criteria)[1]
  vorder = returns
  for (i in 1:n) {
    vorder[i,] = returns[i,] %in% returns[i,order(-direction*returns[i,])][1:k]
  }
  for (i in s:n) {
    #criterialocal = direction*criteria[i,]
    pastcriteria1 =  as.numeric(t(direction*criteria[(i-3):(i-1),]))
    #pastcriteria2 =  as.numeric(t(direction*criteria[(i-4):(i-2),]))
    #pastcriteria3 =  as.numeric(t(direction*criteria[(i-5):(i-3),]))
    #pastcriteria4 =  as.numeric(t(direction*criteria[(i-6):(i-4),]))
    pastbin1 = as.numeric(t(vorder[(i-4):(i-2),]))==1
    #pastbin2 = as.numeric(t(vorder[(i-5):(i-3),]))==1
    #pastbin3 = as.numeric(t(vorder[(i-6):(i-4),]))==1
    #pastbin4 = as.numeric(t(vorder[(i-7):(i-5),]))==1
    binary = as.numeric(t(vorder[(i-3):(i-1),]))==1
    mydata = cbind(binary,pastcriteria1,pastbin1) #pastcriteria2,pastcriteria3,pastcriteria4,pastbin1,pastbin2,pastbin3,pastbin4)
    subset =  mydata[complete.cases(mydata), ]
    mymodel = randomForest(binary~.,data=subset)
    mydata =  t(matrix(as.numeric(direction*criteria[(i),]),1,m))
    mydata = cbind(mydata,t(matrix(as.numeric(vorder[(i-1),]),1,m))==1)
    colnames(mydata) = c("pastcriteria1","pastbin1") #"pastcriteria2","pastcriteria3","pastcriteria4","pastbin1","pastbin2","pastbin3","pastbin4")
    pred = predict(mymodel,mydata)
    mymean = mean(pred)
    allocationlocal = as.NA1(pred>0.5)
    #allocationlocal = as.NA1(pred>mymean)
    if(i>s) allocation = rbind(allocation,allocationlocal)
    else allocation = allocationlocal
  }
  xts(rbind(matrix(NA,s-1,m),allocation),index(criteria0))
}
allocation2s <- function(criteria,t.start,t.end,k) {
  allocation = allocation(criteria,t.start,t.end,k,1)
  allocation2 = allocation(criteria,t.start,t.end,k,-1)
  return(cbind(allocation,allocation2))
}
allocation <- function(criteria0,s,k0,direction,pos=0,tr=NULL) {
  criteria = coredata(criteria0)
  m = dim(criteria)[2]
  n = dim(criteria)[1]
  for (i in s:n) {
    criterialocal = direction*criteria[i,]
    k = k0 #round(k0/100*length(criterialocal),0)
    if(sum(is.na(criterialocal))==m) allocationlocal = rep(NA,m)
    else allocationlocal = as.NA1(criterialocal %in% criterialocal[order(-criterialocal)][1:k])
    #if(sum(as.numeric(criterialocal<0),na.rm=TRUE)!=0) {
    #  criterialocal[which(criterialocal<0)] = NA
    #}    
    if(sum(as.numeric(criterialocal<0),na.rm=T)!=0 & pos == 1)
      allocationlocal[which(criterialocal<=0)] = NA 
    if(!is.null(tr)) {
      if(sum(as.numeric(criterialocal<tr),na.rm=T)!=0)
        allocationlocal[which(abs(criterialocal)<tr)] = NA 
    }
    if(i>s) allocation = rbind(allocation,allocationlocal)
    else allocation = allocationlocal
  }
  alloc = xts(rbind(matrix(NA,s-1,m),allocation),index(criteria0))
  colnames(alloc)=colnames(criteria0)
  alloc
}
portfolio <- function(returns0,criteria0,s,k0,direction,pos=0,tr=NULL) {
  #performance = returns[,1]
  #performance=rep(NA,t.end)
  returns = coredata(returns0)
  criteria = coredata(criteria0)
  n = dim(returns)[1]
  performance=rep(NA,n)
  for (i in s:n) {
    #handle NA
    if(sum(as.numeric(is.na(criteria[i,])))==0) {
      criterialocal = criteria[i,]
      returnslocal = returns[i,]   
    } else {
      criterialocal = criteria[i,-which(is.na(criteria[i,]))]
      returnslocal = returns[i,-which(is.na(criteria[i,]))]
    }
    criterialocal = direction*criterialocal
    if(sum(as.numeric(criterialocal<0),na.rm=T)!=0 & pos == 1) {
      returnslocal = returnslocal[-which(criterialocal<0)]   
      criterialocal = criterialocal[-which(criterialocal<0)]
    }
    if(!is.null(tr)) {
      if(sum(as.numeric(criterialocal<tr),na.rm=T)!=0)
        returnslocal = returnslocal[-which(abs(criterialocal)<tr)]   
        criterialocal = criterialocal[-which(abs(criterialocal)<tr)]        
    }
    if (length(returnslocal)!=0) {
      sorted.returns=returnslocal[order(-criterialocal)]
    } else {
      sorted.returns=0
    }
    k = k0 #round(k0/100*length(criterialocal),0)
    if (k>1 & length(returnslocal)!=0) performance[i]=mean(sorted.returns[1:k],na.rm=TRUE)
    else performance[i]=sorted.returns[1]
  }
  performance = xts(performance,index(returns0))
  return(performance)
  #return((mean(performance)*12)/(sd(performance)*sqrt(12)))
}
portfoliodouble <- function(returns,criteria,criteria2,t.start,t.end,k,l,direction) {
  #performance = returns[,1]
  performance=rep(NA,t.end)
  for (i in t.start:t.end) {
    #handle NA
    if(sum(as.numeric(is.na(criteria[i,])))==0) {
      criterialocal = criteria[i,]
      criterialocal2 = criteria2[i,]
      returnslocal = returns[i,]   
    } else {
      criterialocal = criteria[i,-which(is.na(criteria[i,]))]
      criterialocal2 = criteria2[i,-which(is.na(criteria[i,]))]
      returnslocal = returns[i,-which(is.na(criteria[i,]))]
    }
    criterialocal = direction*criterialocal
    criterialocal2 = direction*criterialocal2
    if(sum(as.numeric(criterialocal<0))!=0) {
      #returnslocal = returnslocal[-which(criterialocal<0)]   
      #criterialocal = criterialocal[-which(criterialocal<0)]
    }
    if (length(returnslocal)!=0) {
      sorted.returns=returnslocal[order(-criterialocal)]
    } else {
      sorted.returns=0
    }
    if (length(criterialocal2)!=0) {
      sorted.criteria2=criterialocal2[order(-criterialocal)]
    } else {
      sorted.criteria2=rep(NA,dim(criteria)[2])
    }    
    
    top.returns = sorted.returns[1:k]
    top.criteria2 = sorted.criteria2[1:k]
    sorted.returns2 = top.returns[order(-top.criteria2)]
    
    if (l>1 & length(returnslocal)!=0) performance[i]=mean(sorted.returns2[1:l],na.rm=TRUE)
    else performance[i]=sorted.returns2[1]
  }
  return(performance)
  #return((mean(performance)*12)/(sd(performance)*sqrt(12)))
}
portfolio2x <- function(lreturns,sreturns,criteria,t.start,t.end,k) {
  returns = cbind(lreturns,sreturns)
  criteria2x = cbind(criteria,-criteria)
  portfolio(returns,criteria2x,t.start,t.end,k,1)
}
portfolio2s <- function(lreturns,sreturns,criteria,t.start,t.end,k) {
  performance = portfolio(lreturns,criteria,t.start,t.end,k,1)
  performance2 = portfolio(sreturns,criteria,t.start,t.end,k,-1)
  performance = rowMeans(cbind(performance,performance2),na.rm=TRUE)
  return(performance)
}
portfoliolin <- function(returns,costs,criteria,t.start,t.end) {
  performance=rep(0,t.end-t.start)
  for (i in t.start:t.end) {
    #handle NA
    if(sum(as.numeric(is.na(criteria[i,])))==0) {
      criterialocal = criteria[i,]
      returnslocal = returns[i,]  
      costslocal = costs
    } else {
      criterialocal = criteria[i,-which(is.na(criteria[i,]))]
      returnslocal = returns[i,-which(is.na(criteria[i,]))]
      costslocal = costs[-which(is.na(criteria[i,]))]
    }
    weights= (criterialocal-mean(criterialocal))
    weights = weights/sum(abs(weights))
    if (length(returnslocal)!=0) {
      sorted.returns = returnslocal*weights-costslocal/10000
    } else {
      sorted.returns=0
    }
    performance[i-t.start+1]=mean(sorted.returns,na.rm=TRUE)
  }
  return(performance)
  #return((mean(performance)*12)/(sd(performance)*sqrt(12)))
}
momentumHold <- function(prices_d,bpsspread,carry=0,ycost=0.01,dir=1,k=3,j=231,v=1,e=0) {
  criteria = diff(log(prices_d),lag=j)
  hold = 1
  criteria = lag(criteria,hold)
  index = index(criteria)
  criteria = criteria #coredata(criteria)[-(1:(j+hold)),]
  n = dim(criteria)[1]
  m = dim(criteria)[2]
  if(v==1) {
    vreturns = exp(diff(log(prices_d)))-1
    vols = vreturns
    for(i in 1:dim(prices_d)[2]) {
      vol = lag(emavol(vreturns[,i],13),hold)
      vol[vol==0] = 0.17
      vols[,i] = 0.17/vol
    }
  }
  s=j+1
  
  if (length(carry)==1) {
    lreturns_d = exp(diff(log(prices_d),lag=hold))-1-bpsspread/10000
    sreturns_d = exp(-diff(log(prices_d),lag=hold))-1-bpsspread/10000 
  }
  else {
    carry_d = lag(carry)
    carry_d = merge(ccys[,1], xts(coredata(carry_d), as.Date(index(carry_d))-1), join="left")[,-1]
    carry_d = fillerxts(carry_d)
    lreturns_d = exp(diff(log(prices_d),lag=hold))-1-bpsspread/10000+lag(carry_d)/51-ycost/51
    sreturns_d = exp(-diff(log(prices_d),lag=hold))-1-bpsspread/10000-lag(carry_d)/51 -ycost/51
  }
  
  if (dir==2) {
    alloc = rbind(matrix(NA,j+1,2*m),allocation2s(criteria,s,n,k))
    alloc = xts(alloc,index)*cbind(lreturns,sreturns)
    colnames(alloc) = c(colnames(prices),paste0("-",colnames(prices)))
    lreturns = coredata(lreturns)[-(1:(j+1)),]
    sreturns = coredata(sreturns)[-(1:(j+1)),]
    test = portfolio2s(lreturns,sreturns,criteria,s,k);
  }
  else {
    alloc = allocation(criteria,s,k,dir)
    colnames(alloc)=colnames(prices_d)
    if (dir==1) returns = lreturns_d #coredata(lreturns_d)[-(1:(j+hold)),]
    else returns = sreturns_d #coredata(sreturns_d)[-(1:(j+hold)),]
    #test = ranker(returns,criteria,s,n);
    test = portfolio(returns,criteria,s,k,dir)
    
    if (v==1) avgvol = xts(rowMeans(alloc*1/vols,na.rm=TRUE),index(vols))
    #avgvol = to.monthly(avgvol)[,4]   
  }
  
  #test = xts(c(rep(NA,j+hold),test),index(prices_d))
  #vol = emavol(test,63)
  if(v==1) test = test*0.17/avgvol
  if (e==0) SRS(test,51/hold,hold) #because 21d-returns are overlapping
  else {
    if (e==1) test #exp(cumsum(c(0,test)))
    else alloc
  }
}
carryHold <- function(prices_d,bpsspread,criteria0,carry=0,ycost=0.01,dir=1,k=3,s=1,e=0) {
  hold=21
  criteria = lag(-criteria0,hold)
  
  index = index(criteria)
  criteria = criteria #coredata(criteria)[-(1:(hold)),]
  n = dim(prices_d)[1]
  m = dim(criteria)[2]
  vreturns = exp(diff(log(prices_d)))-1
  vols = vreturns
  for(i in 1:dim(prices_d)[2]) {
    vol = lag(emavol(vreturns[,i],63),hold)
    vol[vol==0] = 0.17
    vols[,i] = 0.17/vol
  }
  s = 21
  
  if (length(carry)==1) {
    lreturns_d = exp(diff(log(prices_d),lag=hold))-1-bpsspread/10000
    sreturns_d = exp(-diff(log(prices_d),lag=hold))-1-bpsspread/10000 
  }
  else {
    carry_d = lag(carry)
    carry_d = merge(ccys[,1], xts(coredata(carry_d), as.Date(index(carry_d))-1), join="left")[,-1]
    carry_d = fillerxts(carry_d)
    lreturns_d = exp(diff(log(prices_d),lag=hold))-1-bpsspread/10000+lag(carry_d)/252-ycost/252
    sreturns_d = exp(-diff(log(prices_d),lag=hold))-1-bpsspread/10000-lag(carry_d)/252 -ycost/252
  }
  
  if (dir==2) {
    alloc = rbind(matrix(NA,j+1,2*m),allocation2s(criteria,s,n,k))
    alloc = xts(alloc,index)*cbind(lreturns,sreturns)
    colnames(alloc) = c(colnames(prices),paste0("-",colnames(prices)))
    lreturns = coredata(lreturns_d)[-(1:(hold)),]
    sreturns = coredata(sreturns_d)[-(1:(hold)),]
    test = portfolio2s(lreturns,sreturns,criteria,s,k);
  }
  else {
    alloc = allocation(criteria,s,k,dir)
    colnames(alloc)=colnames(prices_d)
    if (dir==1) returns = lreturns_d #coredata(lreturns_d)[-(1:(hold)),]
    else returns = sreturns_d #coredata(sreturns_d)[-(1:(hold)),]
    #test = ranker(returns,criteria,s,n);
    test = portfolio(returns,criteria,s,k,dir)
    
    avgvol = xts(rowMeans(alloc*1/vols,na.rm=TRUE),index(vols))
    #avgvol = to.monthly(avgvol)[,4]   
  }
  
  #test = xts(c(rep(NA,hold),test),index(prices_d))
  #vol = emavol(test,63)
  test = test*0.17/avgvol
  if (e==0) SRS(test,252/hold,hold) #because 21d-returns are overlapping
  else {
    if (e==1) test #exp(cumsum(c(0,test)))
    else alloc
  }
}
momentum12 <- function(prices_d,prices_do,bpsspread,carry=0,ycost=0.01,dir=1,k=3,s=1,j=12,v=1,e=0,f=12,skew=0,pos=1,p=1,h=1,sh=0,w=2,inv=0,highs=0,lows=0,limit=0.05,tp=1,tr=NULL) {
  if(f==12) {prices = mat.to.monthly(prices_d); period_days=21}
  else if(f==52) {prices = mat.to.weekly(prices_d); period_days=5}
  else {prices = prices_d; period_days=1}
  
  #vreturns = diff(log(prices_d))
  #vols = vreturns
  #p=1
  #for(i in 1:dim(vols)[2]) {
  #  tmp = emavol(vols[,i],63)^p
  #  vols[,i] = tmp
  #}
  #for(i in 1:dim(vols)[2]) {
  #  vols[which(vols[,i]==0),i] = rowMeans(vols,na.rm=TRUE)[which(vols[,i]==0)]
  #}
  #newprices_d=vreturns
  #for(i in 1:dim(vols)[2]) {
  #  tmp = xts(exp(cumsum(na20(vreturns[,i]*0.1^p/lag(vols[,i])^p))),index(vreturns))
  #  tmp[which(tmp==1)] = NA
  #  newprices_d[,i] = tmp
  #}
  #newprices = mat.to.monthly(newprices_d) 
  
  if (skew==0) criteria = -diff(log(prices),lag=j)-carry
  else {
    criteria = prices_d
    for(i in 1:dim(prices_d)[2]) criteria[,i] =  rollskew(diff(log(prices_d[,i])),21*j)
    criteria[1:(j*21),1]=99
    criteria = mat.to.monthly(criteria)
    criteria[1:j,1]=NA
    #criteria = xts(coredata(rbind(criteria)),index(prices))
  }
  #criteria_d = prices_d
  #for(i in 1:dim(prices_d)[2]) {
  #  criteria_d[,i] = log(prices_d[,i]) - log(SMA(prices_d[,i],j))
  #}
  #criteria = mat.to.monthly(criteria_d)
  carry12(prices_d,prices_do,bpsspread,criteria,carry=carry,ycost=ycost,dir=dir,k=k,s=s,v=v,e=e,f=f,pos=pos,p=p,h=h,sh=sh,w=w,inv=inv,highs=highs,lows=lows,limit=limit,tp=tp,tr=tr)
}
momentum12d <- function(prices_d,prices_do,bpsspread,carry=0,ycost=0.01,k=3,s=1,j=12,v=1,e=0,f=12,skew=0,pos=1,p=1,h=1,sh=0,w=2,use=0,inv=0,highs=0,lows=0,limit=0.05,tp=1,tr=NULL) {
  if(f==12) {prices = mat.to.monthly(prices_d); period_days=21}
  else if(f==52) {prices = mat.to.weekly(prices_d); period_days=5}
  else {prices = prices_d; period_days=1}
  if(use==1) usecarry = carry/f*j #diff(cumsum(mat.na20(carry/f)),lag=j)
  else usecarry = 0
  if (skew==0) criteria = -diff(log(prices),lag=j) - usecarry
  else {
    criteria = prices_d
    for(i in 1:dim(prices_d)[2]) criteria[,i] =  rollskew(diff(log(prices_d[,i])),period_days*j)
    criteria[1:(j*21),1]=99
    criteria = mat.to.monthly(criteria)
    criteria[1:j,1]=NA
    #criteria = xts(coredata(rbind(criteria)),index(prices))
  }
  carry12d(prices_d,prices_do,bpsspread,criteria,carry=carry,ycost=ycost,k=k,s=s,v=v,e=e,f=f,pos=pos,p=p,h=h,sh=sh,w=w,inv=inv,highs=highs,lows=lows,limit=limit,tp=tp,tr=tr)
}
carry12d<- function(prices_d,prices_do,bpsspread,criteria0,carry=0,ycost=0.01,k=3,s=1,v=0,e=0,f=12,pos=0,p=1,h=1,sh=0,w=2,inv=0,highs=0,lows=0,limit=0.05,tp=1,tr=NULL) {
  if(e==0 | e==1) {
    long  = carry12(prices_d,prices_do,bpsspread,criteria0,carry=carry,ycost=ycost,dir=1,k=k,s=s,v=v,e=1,f=f,pos=pos,p=p,h=h,sh=sh,w=w,inv=inv,highs=highs,lows=lows,limit=limit,tp=tp,tr=tr)
    short = carry12(prices_d,prices_do,bpsspread,criteria0,carry=carry,ycost=ycost,dir=-1,k=k,s=s,v=v,e=1,f=f,pos=pos,p=p,h=h,sh=sh,w=w,inv=inv,highs=highs,lows=lows,limit=limit,tp=tp,tr=tr)
    double = cbind(long,short)
  }
  else {
    long  = carry12(prices_d,prices_do,bpsspread,criteria0,carry=carry,ycost=ycost,dir=1,k=k,s=s,v=v,e=e,f=f,pos=pos,p=p,h=h,sh=sh,w=w,inv=inv,highs=highs,lows=lows,limit=limit,tp=tp,tr=tr)
    short = carry12(prices_d,prices_do,bpsspread,criteria0,carry=carry,ycost=ycost,dir=-1,k=k,s=s,v=v,e=e,f=f,pos=pos,p=p,h=h,sh=sh,w=w,inv=inv,highs=highs,lows=lows,limit=limit,tp=tp,tr=tr)
    double = cbind(long,short)
  }
  if (e==0) SR(xts(log(rowMeans(exp(double),na.rm=T)),index(double)),f)
  else if (e==1) xts(log(rowMeans(exp(double),na.rm=T)),index(double))
  else mat.na20(long)+mat.na20(short)
}
carry12hld<- function(prices_d,prices_do,prices_dh,prices_dl,bpsspread,criteria0,carry=0,ycost=0.01,k=3,s=1,v=0,e=0,f=12,pos=0,p=1,h=1,sh=0,w=2,inv=0,llimit=0.05,tp=1){
  carry12d(prices_d,prices_do,bpsspread,criteria0,carry=0,ycost=0.01,k=3,s=1,v=0,e=0,f=12,pos=0,p=1,h=1,sh=0,w=2,inv=0,highs=prices_dh,lows=prices_dl,limit=limit,tp=tp)
}
momentum12hld <- function(prices_d,prices_do,prices_dh,prices_dl,bpsspread,carry=0,ycost=0.01,k=3,s=1,j=12,v=1,e=0,f=12,skew=0,pos=1,p=1,h=1,sh=0,w=2,use=0,inv=0,limit=0.05,tp=1) {
  momentum12d(prices_d,prices_do,bpsspread,carry=0,ycost=0.01,k=3,s=1,j=12,v=1,e=0,f=12,skew=0,pos=1,p=1,h=1,sh=0,w=2,use=0,inv=0,highs=prices_dh,lows=prices_dl,limit=limit,tp=tp)
}
carry12hl <- function(prices_d,prices_do,prices_dh,prices_dl,bpsspread,criteria0,carry=0,ycost=0.01,dir=1,k=3,s=1,v=0,e=0,f=12,pos=0,p=1,h=1,sh=0,w=2,inv=0,limit=0.05,tp=1) {
  carry12(prices_d,prices_do,bpsspread,criteria0,carry=0,ycost=0.01,dir=1,k=3,s=1,v=0,e=0,f=12,pos=0,p=1,h=1,sh=0,w=2,inv=0,highs=prices_dh,lows=prices_dl,limit=limit,tp=tp)
}
momentum12hl <- function(prices_d,prices_do,prices_dh,prices_dl,bpsspread,carry=0,ycost=0.01,k=3,s=1,j=12,v=1,e=0,f=12,skew=0,pos=1,p=1,h=1,sh=0,w=2,use=0,inv=0,limit=0.05,tp=1) {
  momentum12(prices_d,prices_do,bpsspread,carry=0,ycost=0.01,k=3,s=1,j=12,v=1,e=0,f=12,skew=0,pos=1,p=1,h=1,sh=0,w=2,use=0,inv=0,highs=prices_dh,lows=prices_dl,limit=limit,tp=tp)
}
carry12 <- function(prices_d,prices_do,bpsspread,criteria0,carry=0,ycost=0.01,dir=1,k=3,s=1,v=0,e=0,f=12,pos=0,p=1,h=1,sh=0,w=2,inv=0,highs=0,lows=0,limit=0.05,tp=1,tr=NULL) {
  if(1==1){##preparation 
    if(sh>=h) {stop("Shift is greter than holding period. Respecify input.")}
    options(warn=-1)
    if(f==12) {prices = mat.to.monthly(prices_d,4); priceso = mat.to.monthly(prices_do,1)}
    else if (f==52) {prices = mat.to.weekly(prices_d,4); priceso = mat.to.weekly(prices_do,1)}
    else {prices =  prices_d; priceso =  prices_do}
    if(length(highs)>1 & length(lows)>1){
      if(f==12) {pricesh = mat.to.monthly(highs,2); pricesl = mat.to.monthly(lows,3)}
      else if (f==52) {pricesh = mat.to.weekly(highs,2); pricesl = mat.to.weekly(lows,3)}
      else {pricesh = highs; pricesl = lows}
    }
    
    vreturns = diff(log(prices_d)) #/2+prices_do/2))
    vols = vreturns
    volpar=63
    target=0.35
    
    skews = vreturns
    for(i in 1:dim(vols)[2]) {
      tmp = emavol(vols[,i],volpar)^p
      vols[,i] = tmp
      #skews[,i] = rollskew(vreturns[,i],1/63)
    }
    for(i in 1:dim(vols)[2]) {
      vols[which(vols[,i]==0),i] = rowMeans(vols,na.rm=TRUE)[which(vols[,i]==0)]
    }
    vols2 = vreturns
    for(i in 1:dim(vols)[2]) {
      tmp = emavol(vreturns[,i]/vols[,i],volpar)^p
      vols2[,i] = tmp
    }
    
    if(f==12) vols_l = mat.to.monthly(vols)
    else if (f==52) vols_l = mat.to.weekly(vols) 
    else vols_l = vols
    
    if (length(carry)==1) {
      l.carry=lag(carry)[1]
    }
    else {
      l.carry=lag(carry)
    }    
    #criteria0 = -roll_rates_ms
    returns_w = (prices/priceso)-1 + carry/f
    lreturns_w = sweep(returns_w,2,ycost/f + bpsspread/10000/4,'-')
    sreturns_w = sweep(-returns_w,2,ycost/f + bpsspread/10000/4,'-')
    if(h>1){
      prices = elim_shift(prices,h,sh)  
      priceso = lag(elim_shift(lag(priceso,-1),h,sh))
      criteria0 = elim_shift(criteria0,h,sh)  
    }
    
    range_limit = as.Date(min(last(index(prices)),last(index(criteria0))))
    prices=prices[paste0('/',range_limit)]
    priceso=priceso[paste0('/',range_limit)]
    if(length(highs)>1 & length(lows)>1){
      pricesh=pricesh[paste0('/',range_limit)]
      pricesl=pricesl[paste0('/',range_limit)]
    }    
    #carry=carry[paste0('/',range_limit)]
    criteria0=criteria0[paste0('/',range_limit)]
    prices=prices[paste0(as.Date(first(index(criteria0))),'/'),]
    
    returns = (prices/priceso)-1 + carry/f*h
    if(inv==1) {
      invprices_d = prices_d
      for(i in 1:dim(prices_d)[2]) invprices_d[,i] = quickrecip3(prices_d[,i])
      #invprices_do = prices_do
      #for(i in 1:dim(prices_d)[2]) invprices_do[,i] = quickrecip3(prices_do[,i])
      invprices_d = mat.12na(invprices_d)
      invprices_do = lag(invprices_d)*(1-(prices_do/lag(prices_d)-1))
      #invprices_do = mat.12na(invprices_do)
      if(f==12) {invprices = mat.to.monthly(invprices_d); invpriceso = mat.to.monthly(invprices_do,1)}
      else if (f==52) {invprices = mat.to.weekly(invprices_d); invpriceso = mat.to.weekly(invprices_do,1)}
      else {invprices =  invprices_d; invpriceso =  invprices_do}
      invreturns = (invprices/invpriceso)-1 - carry/f*h
    }
    else invreturns = -returns
    lreturns = sweep(returns,2,ycost/f*h + bpsspread/10000,'-')
    sreturns = sweep(invreturns,2,ycost/f*h + bpsspread/10000,'-')
    #sreturns = sweep(levret(-1,returns),2,log(ycost+1)/f*h + log(bpsspread/10000+1),'-')
    #sreturns = sweep((-1*returns),2,log(ycost+1)/f*h + log(bpsspread/10000+1),'-')
    
    criteria = lag(-criteria0,1)  
    
  }##preparation   
  
  if(dir==2) {
    lreturns = coredata(lreturns)[-(1),]
    sreturns = coredata(sreturns)[-(1),]
    test = portfolio2s(lreturns,sreturns,criteria,s,k);
  }
  if(dir==3) {
    criteriaC = (criteria-rowMeans(criteria,na.rm=T))
    alloc = -1*criteriaC #/rowSums(abs(criteriaC),na.rm=T)
    #test = ranker(returns,criteria,s,bpsspread,ycost,f)
    test = xts(rowMeans(alloc*returns,na.rm=T),index(returns))
  }
  if(dir==4) {
    #criteriaC = (criteria-rowMeans(criteria))
    alloc = criteria/rowSums(abs(criteria),na.rm=T)
    test = ranker(returns,criteria,s,bpsspread,ycost,f)
  }
  else {
    alloc = allocation(criteria,s,k,dir,pos,tr) ## MAIN function, gives 1 or NA, it is pure selector
    #alloc = allocationRF(criteria,diff(log(prices)),s,k,dir,pos)
    if (dir==1) returns = lreturns #coredata(lreturns)[-(1),]
    else returns = sreturns #coredata(sreturns)[-(1),]
    if(h==1) test = portfolio(returns,criteria,s,k,dir,pos,tr)
    #if (dir==1) returns = alloc*lreturns
    #else returns = alloc*sreturns    
    #test = xts(rowMeans(returns,na.rm=TRUE),index(returns))
    #alloc = elim_shift(alloc,h,sh)  
  }  
  
  if(v>=1) {
    if(f==12) {
      alloc_d = merge(prices_d[,1], xts(coredata(alloc), as.Date(index(alloc))))[,-1]
      alloc_d = fillerxtsM(alloc_d)
    }
    else if (f==52) {
      alloc_d = merge(prices_d[,1], xts(coredata(alloc), as.Date(index(alloc))))[,-1]
      alloc_d = fillerxtsW(alloc_d) #not tested yet
      if(h>1){
        alloc2 = lag(alloc,-1)
        alloc_w = merge(to.weekly(prices_d[,1])[,4], xts(coredata(alloc2), as.Date(index(alloc2))))[,-1]
        alloc_w = fillerxtsWh(alloc_w,h,sh) #not tested yet 
        if (dir==1) returns = alloc_w*lreturns_w
        else returns = alloc_w*sreturns_w 
        test = xts(rowMeans(returns,na.rm=TRUE),index(returns))
      }
      else{
        alloc_w = alloc
      }
    }
    else alloc_d = alloc
    colnames(alloc_d)=colnames(prices)
    
    if(w==1) vols=vols
    if(w==2) vols=vols2
    if(f==12 & h==1) vol = lag(mat.to.monthly(vols))
    else if (f==252 & h==1) vol = lag(vols)
    else if (f==52) vol = lag(mat.to.weekly(vols))
    
    if(h==1) avgvol = xts(rowMeans(abs(alloc)*vol,na.rm=TRUE),index(abs(alloc)*vol))
    else avgvol = xts(rowMeans(abs(alloc_w)*vol,na.rm=TRUE),index(abs(alloc_w)*vol)) ## hard to explain why NaNs are produced sometimes here
    avgvol = fillerxts(avgvol) ## to remove NAs and NaNs
    #avgskew = xts(rowMeans(abs(alloc_d)*skews,na.rm=TRUE),index(skews))
    avgvol_mat = xts(matrix(rep(avgvol,dim(prices_d)[2]),dim(avgvol)[1],dim(prices_d)[2]),index(avgvol))
    colnames(avgvol_mat)=colnames(prices_d)
    
    if(v==4) {
      avgvol_l = xts(rowMeans(abs(alloc_d)*vols_l,na.rm=TRUE),index(abs(alloc_d)*vols_l))
      if(f==12) avgvol_l = to.monthly(avgvol_l)[,4]
      else if (f==52) avgvol_l = to.weekly(avgvol_l)[,4]
    }
  }    
  
  #test = xts(c(NA,test),index(prices))
  positions = alloc*dir ## pure selector plus direction
  #test = xts(rowMeans(alloc*returns,na.rm=T),index(alloc*returns))
  if(v==0) mod2alloc=1
  if(v==1) {
    mod2alloc = target^p/avgvol_mat
    test = target^p/avgvol*test
    positions = positions*mod2alloc
  }
  else if(v==2) {
    meanswitch = (sign(cumsum(na20(test^3)))+1)/2
    meanswitch = lag(meanswitch)
    meanswitch[1:2] = 1
    test = (1-meanswitch) *  target^p/avgvol*test + (meanswitch)*test
  }
  else if(v==3) {
    #if (dir==1) returns = alloc_w*lreturns_w #levret(alloc,lreturns) #*0.15^p/lag(vols_l)^p
    if (dir==1) returns = alloc*lreturns #levret(alloc,lreturns) #*0.15^p/lag(vols_l)^p
    else returns = alloc*sreturns #levret(alloc,sreturns) #*0.15^p/lag(vols_l)^p
    test = xts(rowMeans(returns,na.rm=TRUE),index(returns)) #*0.10^p/lag(avgvol)
  }
  else if(v==4) {
    test = target^p/lag(avgvol_l)*test
  }
  options(warn=0)
  if (e==0) SR(std2log(test),f)
  else if (e==1) std2log(test) #exp(cumsum(c(0,test)))
  else {
    if (dir==1) returns = lreturns
    else returns = sreturns
    if(e==2) std2log(mod2alloc*alloc*returns) #mod2alloc*alloc = positions/dir
    else if(e==3) returns#positions #avgskew #0.15^p/lag(avgvol)
    else if(e==4) {
      #returns = (prices/priceso)-1
      capped_returns = sltp(dir,std2log(mod2alloc*alloc*returns),prices_do,highs,lows,prices_d,limit,cost=bpsspread/10000,tp,f,carry,ycost)
      std2log(xts(rowMeans(log2std(capped_returns[[1]]),na.rm=TRUE),index(mod2alloc*alloc*returns)))
    }
    else {#e==5
      #returns = (prices/priceso)-1
      capped_returns = sltp(dir*mod2alloc,std2log(mod2alloc*alloc*returns),prices_do,highs,lows,prices_d,limit,cost=bpsspread/10000,tp,f,carry,ycost)
      capped_returns2 = NA#levret(mod2alloc,sltp(dir,std2log(alloc*returns),prices_do,highs,lows,prices_d,limit,cost=0.00015,tp,f,carry,ycost)[[1]])
      list(capped_returns[[1]],std2log(mod2alloc*alloc*returns),mod2alloc,capped_returns[[2]],capped_returns2)
    }
  }
}
time_carry<- function(extr,o,limitcost,m=1){
  fo = as.numeric(coredata(o)[1])
  t=which(m*log(extr/fo)>limitcost)[1]
  out = as.numeric(index(extr)[t]-index(extr)[1]-1)/365
  if(is.na(out)) NaN#1/12*1/2
  else out
}
sltp <- function(dir,ret,o_d,h_d,l_d,c_d,limit,cost=0.00015,tp=1,f,carry,ycost){
  if(f==12) {c = mat.to.monthly(c_d,4); o = mat.to.monthly(o_d,1)}
  else if (f==52) {c = mat.to.weekly(c_d,4); o = mat.to.weekly(o_d,1)}
  else {c = c_d; o = o_d}
  if(f==12) {h = mat.to.monthly(h_d,2); l = mat.to.monthly(l_d,3)}
  else if (f==52) {h = mat.to.weekly(h_d,2); l = mat.to.weekly(l_d,3)}
  else {h = h_d; l = l_d}  
  n=dim(ret)[1]
  k=dim(ret)[2]
  if(length(cost)==1) cost=rep(cost,k)
  out = ret #+ sign(ret*bar)*carry/f
  times = ret*NA
  prop_carry = coredata(merge(ret[,1],carry,join="left")[,-1])
  idx=substr(as.Date(index(out)),0,7)
  c = coredata(merge(ret[,1],c,join="left")[,-1])
  o = coredata(merge(ret[,1],o,join="left")[,-1])
  h = coredata(merge(ret[,1],h,join="left")[,-1])
  l = coredata(merge(ret[,1],l,join="left")[,-1])
  if(length(dir)==1) dir = dir*ret/ret
  else dir = coredata(merge(ret[,1],dir,join="left")[,-1])
  bar=log(c/o)
  down = coredata(log(o/l)) #sweep(coredata(log(o/l)),2,cost,'-')
  up = coredata(log(h/o)) #sweep(coredata(log(h/o)),2,cost,'-')
  #dir= coredata(ret*bar)
  bar = coredata(bar)
  ret = coredata(ret)
  if(tp==1){
    for(i in 1:n){
      for(j in 1:k) {
        if(is.na(ret[i,j]))
          out[i,j] = NA
        else if (dir[i,j]>0) {#(dir[i,j]>0)
          if(abs(dir[i,j])*up[i,j]>limit+abs(dir[i,j])*cost[j]*2) { #(up[i,j]>limit+cost[j]*2) {
            times[i,j] = time_carry(h_d[idx[i]][,j],o_d[idx[i]][,j],limit+cost[j]*2,1)
            out[i,j] = limit+abs(dir[i,j])*(times[i,j]*(prop_carry[i,j]-ycost)) #abs(dir[i,j])*(limit+times[i,j]*(prop_carry[i,j]-ycost))
          }
          else
            out[i,j] = ret[i,j]
        }
        else if (dir[i,j]<0) {
          if(abs(dir[i,j])*down[i,j]>limit+abs(dir[i,j])*cost[j]*2){
            times[i,j] = time_carry(l_d[idx[i]][,j],o_d[idx[i]][,j],limit+cost[j]*2,-1)
            out[i,j] = limit+abs(dir[i,j])*(times[i,j]*(-prop_carry[i,j]-ycost))
          }
          else
            out[i,j] = ret[i,j]
        }
        else if (dir[i,j]==0)
          out[i,j] = 0
      }
    } 
  }
  else {
    for(i in 1:n){
      for(j in 1:k) {
        if(is.na(ret[i,j]))
          out[i,j] = NA
        else if (dir[i,j]>0) {#(dir[i,j]>0)
          if(down[i,j]>limit+cost[j]*2) {
            times[i,j] = time_carry(h_d[idx[i]][,j],o_d[idx[i]][,j],limit+cost[j]*2,1)
            out[i,j] = abs(dir[i,j])*(-limit+times[i,j]*(prop_carry[i,j]-ycost))
          }
          else
            out[i,j] = ret[i,j]
        }
        else if (dir[i,j]<0) {
          if(up[i,j]>limit+cost[j]*2){
            times[i,j] = time_carry(l_d[idx[i]][,j],o_d[idx[i]][,j],limit+cost[j]*2,-1)
            out[i,j] = abs(dir[i,j])*(-limit+times[i,j]*(-prop_carry[i,j]-ycost))
          }
          else
            out[i,j] = ret[i,j]
        }
        else if (dir[i,j]==0)
          out[i,j] = 0
      }
    }     
  }
  list(out,times)
}
carry12daily <- function(prices_d,prices_do,bpsspread,criteria0,carry=0,ycost=0.01,dir=1,k=3,s=1,v=0,e=0,f=12,pos=0,p=1,h=1,sh=0,w=2,inv=0) {
  monthlypos = carry12(prices_d,prices_do,bpsspread,criteria0,carry,ycost,dir,k,s,v,e=3,f,pos,p,h,sh,w,inv)  
  splotMat(prices_d,prices_do,monthlypos)
}
splotMat <- function(dailypricec,dailypriceo,monthlypos){
  #monthlypriceo = mat.to.monthly(dailypriceo,1)
  #monthlypricec = mat.to.monthly(dailypricec,4)
  #openval = monthlypriceo*monthlysizes*monthlypos
  #closeval = monthlypricec*monthlysizes*monthlypos
  
  monthlypriceo = lag(mat.to.monthly(dailypricec,4))
  k=dim(monthlypos)[2]
  monthlysizes = rowSums(monthlypriceo,na.rm=T)/monthlypriceo/k
  
  dailypos = merge(dailypricec[,1],monthlypos)
  dailypos = fillerxts(dailypos)[,-1]
  dailypos = dailypos[paste0(index(dailypricec)[1],"/")]
  dailysizes = merge(dailypricec[,1],monthlysizes)
  dailysizes = fillerxts(dailysizes)[,-1]  
  dailysizes = dailysizes[paste0(index(dailypricec)[1],"/")]
  dailyvalc = dailypricec*dailysizes*dailypos
  dailyvalc = xts(rowSums(dailyvalc,na.rm=T),index(dailyvalc))
  returns = diff(log(dailyvalc))
  dailyvalo = dailypriceo*dailysizes*dailypos
  dailyvalo = xts(rowSums(dailyvalo,na.rm=T),index(dailyvalo))
  returnsdays = log(dailyvalc/dailyvalo)
  monthchanges = xts(c(NA,diff(as.numeric(format(index(dailypricec),"%m")))),index(dailypricec))
  dane = cbind(monthchanges,returns,returnsdays)
  colnames(dane) = c('monthchanges','returns','returnsdays')
  out = ifelse(dane$monthchanges == 0, dane$returns, dane$returnsdays)
  out
}
carry12old <- function(prices_d,prices_do,bpsspread,criteria0,carry=0,ycost=0.01,dir=1,k=3,s=1,v=0,e=0,f=12,pos=0,p=1) {
  options(warn=-1)
  if(f==12) {prices = mat.to.monthly(prices_d); priceso = mat.to.monthly(prices_do,1)}
  else if (f==52) {prices = mat.to.weekly(prices_d); priceso = mat.to.weekly(prices_do,1)}
  else {prices =  prices_d; priceso =  prices_do}
  vreturns = diff(log(prices_d))
  vols = vreturns
  volpar=63
  skews = vreturns
  for(i in 1:dim(vols)[2]) {
    tmp = emavol(vols[,i],volpar)^p
    vols[,i] = tmp
    skews[,i] = rollskew(vreturns[,i],1/63)
  }
  for(i in 1:dim(vols)[2]) {
    vols[which(vols[,i]==0),i] = rowMeans(vols,na.rm=TRUE)[which(vols[,i]==0)]
  }
  vols2 = vreturns
  for(i in 1:dim(vols)[2]) {
    tmp = emavol(vreturns[,i]/vols[,i],volpar)^p
    vols2[,i] = tmp
  }
  
  if(f==12) vols_l = mat.to.monthly(vols)
  else if (f==52) vols_l = mat.to.weekly(vols) 
  else vols_l = vols
  vols_l = lag(vols_l)
  criteria = -lag(criteria0)
  
  #prices=prices[paste0(as.Date(first(index(criteria))),'/'),]
  limit = as.Date(min(last(index(prices)),last(index(criteria))))
  prices=prices[paste0('/',limit)]
  criteria=criteria[paste0('/',limit)]
  
  if (length(carry)==1) returns = log(prices/priceso)
  else returns = log(prices/priceso) + lag(carry)/f
  lreturns = sweep(returns,2,log(bpsspread/10000+1) + log(ycost+1)/f,'-')
  sreturns = sweep(-returns,2,log(bpsspread/10000+1) + log(ycost+1)/f,'-')
  
  index = index(criteria)  
  #criteria = coredata(criteria)[-(1),]
  n = dim(criteria)[1]
  m = dim(criteria)[2]
  
  if (dir==2) {
    #alloc = rbind(matrix(NA,1,2*m),allocation2s(criteria,s,n,k))
    #alloc = xts(alloc,index)*cbind(lreturns,sreturns)  
    #colnames(alloc) = c(colnames(prices),paste0("-",colnames(prices)))
    lreturns = coredata(lreturns)[-(1),]
    sreturns = coredata(sreturns)[-(1),]
    test = portfolio2s(lreturns,sreturns,criteria,s,k);
  }
  if(dir==3) {
    criteriaC = (criteria-rowMeans(criteria,na.rm=T))
    alloc = -1*criteriaC #/rowSums(abs(criteriaC),na.rm=T)
    #test = ranker(returns,criteria,s,bpsspread,ycost,f)
    test = xts(log(rowMeans(exp(levret(alloc,returns)),na.rm=T)),index(returns))
  }
  if(dir==4) {
    #criteriaC = (criteria-rowMeans(criteria))
    alloc = criteria/rowSums(abs(criteria),na.rm=T)
    test = ranker(returns,criteria,s,bpsspread,ycost,f)
  }
  else {
    #alloc = rbind(matrix(NA,1,m),allocation(criteria,s,n,k,dir))
    #alloc = xts(alloc,index) 
    alloc = allocation(criteria,s,k,dir,pos)
    #alloc = allocationRF(criteria,diff(log(prices)),s,k,dir,pos)
    if (dir==1) returns = lreturns #coredata(lreturns)[-(1),]
    else returns = sreturns #coredata(sreturns)[-(1),]
    test = portfolio(returns,criteria,s,k,dir,pos)
    
    #if (dir==1) returns = lreturns*alloc
    #else returns = sreturns*alloc    
    #go daily and calc vol
    #alloc = lag(alloc)
    #alloc = merge(prices[,1], xts(coredata(alloc), as.Date(index(alloc))-1))[,-1]
    #alloc = fillerxts(alloc)
    #meanret = xts(rowMeans(returns,na.rm=TRUE),index(returns))
    #vol = emavol(meanret,63)
    #vol = to.monthly(vol)[,4]
  }  
  
  if(v>=1) {
    if(f==12 | f==52) {
      alloc_d = merge(prices_d[,1], xts(coredata(alloc), as.Date(index(alloc))))[,-1]
      alloc_d = fillerxtsM(alloc_d)
    }
    else alloc_d = alloc
    colnames(alloc_d)=colnames(prices)
    avgvol = xts(rowMeans(abs(alloc_d)*vols2,na.rm=TRUE),index(vols2))
    avgskew = xts(rowMeans(abs(alloc_d)*skews,na.rm=TRUE),index(skews))
    if(f==12) avgvol = to.monthly(avgvol)[,4]   
    else if (f==52) avgvol = to.weekly(avgvol)[,4]
    else avgvol = avgvol
  }    
  
  #test = xts(c(NA,test),index(prices))
  if(v==1) test = levret(0.15^p/lag(avgvol),test)
  else if(v==2) {
    meanswitch = (sign(cumsum(na20(test^3)))+1)/2
    meanswitch = lag(meanswitch)
    meanswitch[1:2] = 1
    test = (1-meanswitch) *  levret(0.15^p/lag(avgvol),test) + (meanswitch)*test
  }
  else if(v==3) {
    if (dir==1) returns = alloc*lreturns #levret(alloc,lreturns) *0.15^p/vols_l^p
    else returns = alloc*sreturns #levret(alloc,sreturns) *0.15^p/vols_l^p
    test = xts(log(rowMeans(exp(returns),na.rm=TRUE)),index(returns)) #*0.10^p/lag(avgvol)
  }
  options(warn=0)
  if (e==0) SR(test,f)
  else {
    if (e==1) test #exp(cumsum(c(0,test)))
    else {
      if(e==2) mat.na20(alloc)*dir
      else avgskew #0.15^p/lag(avgvol)
    }
  }
}
carry12dold<- function(prices_d,prices_do,bpsspread,criteria0,carry=0,ycost=0.01,k=3,s=1,v=0,e=0,f=12,pos=0,p=1,h=1,sh=0) {
  if(e==0 | e==1) {
    long  = carry12old(prices_d,prices_do,bpsspread,criteria0,carry=carry,ycost=ycost,dir=1,k=k,s=s,v=v,e=1,f=f,pos=pos,p=p)
    short = carry12old(prices_d,prices_do,bpsspread,criteria0,carry=carry,ycost=ycost,dir=-1,k=k,s=s,v=v,e=1,f=f,pos=pos,p=p)
    double = cbind(long,short)
  }
  else {
    long  = carry12old(prices_d,prices_do,bpsspread,criteria0,carry=carry,ycost=ycost,dir=1,k=k,s=s,v=v,e=e,f=f,pos=pos,p=p)
    short = carry12old(prices_d,prices_do,bpsspread,criteria0,carry=carry,ycost=ycost,dir=-1,k=k,s=s,v=v,e=e,f=f,pos=pos,p=p)
    double = cbind(long,short)
  }
  if (e==0) SR(xts(log(rowMeans(exp(double),na.rm=T)),index(double)),f)
  else if (e==1) xts(log(rowMeans(exp(double),na.rm=T)),index(double))
  else xts(log(rowMeans(exp(double),na.rm=T)),index(double)) #mat.na20(long)+mat.na20(short)
}
carry12r <- function(prices_d,longoptim,shortoptim,bpsspread,criteria0,carry=0,ycost=0.01,dir=1,k=3,s=1,e=0) {
  prices = mat.to.monthly(prices_d) 
  
  criteria = -lag(criteria0)
  m = dim(prices)[2]
  #limit = min(last(index(prices)),last(index(criteria)))
  #prices=prices[paste0('/',limit)]
  #criteria=criteria[paste0('/',limit)]
  
  if (length(carry)==1) {
    #lreturns_d = exp(diff(log(prices_d)))-1-bpsspread/10000
    #sreturns_d = exp(-diff(log(prices_d)))-1-bpsspread/10000 
    lreturns = exp(diff(log(prices)))-1-bpsspread/10000-ycost/12
    sreturns = exp(-diff(log(prices)))-1-bpsspread/10000-ycost/12
    lreturns_d = longoptim-bpsspread/10000/21-ycost/252
    sreturns_d = shortoptim-bpsspread/10000/21-ycost/252
  }
  else {
    carry_d = (carry)
    carry_d = merge(ccys[,1], xts(coredata(carry_d), as.Date(index(carry_d))), join="left")[,-1]
    carry_d = fillerxts(carry_d)
    lreturns = exp(diff(log(prices)))-1-bpsspread/10000+lag(carry)/12-ycost/12
    sreturns = exp(-diff(log(prices)))-1-bpsspread/10000-lag(carry)/12 -ycost/12
    lreturns_d = longoptim-bpsspread/10000/21+lag(carry_d)/252-ycost/252
    sreturns_d = shortoptim-bpsspread/10000/21-lag(carry_d)/252 -ycost/252
    #lreturns_d = exp(diff(log(prices_d)))-1-bpsspread/10000/21+lag(carry_d)/252-ycost/252
    #sreturns_d = exp(-diff(log(prices_d)))-1-bpsspread/10000/21-lag(carry_d)/252 -ycost/252
  }
  
  index = index(criteria)  
  criteria = coredata(criteria)[-(1),]
  n = dim(criteria)[1]
  m = dim(criteria)[2]
  
  if (dir==2) {
    alloc = rbind(matrix(NA,1,2*m),allocation2s(criteria,s,n,k))
    alloc = xts(alloc,index)*cbind(lreturns,sreturns)  
    colnames(alloc) = c(colnames(prices),paste0("-",colnames(prices)))
    lreturns = coredata(lreturns)[-(1),]
    sreturns = coredata(sreturns)[-(1),]
    test = portfolio2s(lreturns,sreturns,criteria,s,n,k);
  }
  else {
    alloc = rbind(matrix(NA,1,m),allocation(criteria,s,n,k,dir))
    alloc = xts(alloc,index)  
    if (dir==1) returns = coredata(lreturns)[-(1),]
    else returns = coredata(sreturns)[-(1),]
    test = portfolio(returns,criteria,s,n,k,dir);
    
    if (dir==1) returns = lreturns*alloc
    else returns = sreturns*alloc    
    #go daily and calc vol
    #alloc = lag(alloc)
    alloc = merge(prices_d[,1], xts(coredata(alloc), as.Date(index(alloc))))[,-1]
    alloc = fillerxts(alloc)
    colnames(alloc)=colnames(prices_d)
    if (dir==1) returns_d = lreturns_d*alloc
    else returns_d = sreturns_d*alloc 
    meanret = xts(rowMeans(returns_d,na.rm=TRUE),index(returns_d))
    #vol = emavol(meanret,63)
    #vol = to.monthly(vol)[,4]    
  }
  
  #test = xts(c(NA,test),index(prices))
  #test = test*0.17/lag(vol)
  if (e==0) SR(meanret,252)
  else {
    if (e==1) meanret #exp(cumsum(c(0,test)))
    else {
      if(e==2) alloc
      else returns
    }
  }  
}
carrydouble <- function(prices,bpsspread,criteria0,criteria1,carry=0,ycost=0.01,dir=1,k=3,l=3,s=1,e=0) {
  
  criteria = -lag(criteria0)
  criteria2 = -lag(criteria1)
  
  if (length(carry)==1) {
    lreturns = exp(diff(log(prices)))-1-bpsspread/10000
    sreturns = exp(-diff(log(prices)))-1-bpsspread/10000    
  }
  else {
    lreturns = exp(diff(log(prices)))-1-bpsspread/10000+lag(carry)/12-ycost/12
    sreturns = exp(-diff(log(prices)))-1-bpsspread/10000-lag(carry)/12 -ycost/12 
  }
  
  index = index(criteria)  
  criteria = coredata(criteria)[-(1),]
  n = dim(criteria)[1]
  m = dim(criteria)[2]
  
  if (dir==2) {
    alloc = rbind(matrix(NA,1,2*m),allocation2s(criteria,s,n,k))
    alloc = xts(alloc,index)*cbind(lreturns,sreturns)  
    colnames(alloc) = c(colnames(prices),paste0("-",colnames(prices)))
    lreturns = coredata(lreturns)[-(1),]
    sreturns = coredata(sreturns)[-(1),]
    test = portfolio2s(lreturns,sreturns,criteria,s,n,k);
  }
  else {
    alloc = rbind(matrix(NA,1,m),allocation(criteria,s,n,k,dir))
    alloc = xts(alloc,index)  
    if (dir==1) returns = coredata(lreturns)[-(1),]
    else returns = coredata(sreturns)[-(1),]
    test = portfoliodouble(returns,criteria,criteria2,s,n,k,l,dir);
    
    if (dir==1) returns = lreturns*alloc
    else returns = sreturns*alloc    
  }
  
  test = xts(c(NA,test),index(prices))
  if (e==0) SR(test,12)
  else {
    if (e==1) test #exp(cumsum(c(0,test)))
    else {
      if(e==2) alloc
      else returns
    }
  }  
}
strategy <- function(returns,spreads,criteria,vols_d,dir=1,k=3,s=1,e=0,v=0,per=252){
  if (dir==2) {
    alloc = rbind(matrix(NA,j+1,2*m),allocation2s(criteria,s,n,k))
    alloc = xts(alloc,index)*cbind(lreturns,sreturns)
    colnames(alloc) = c(colnames(prices),paste0("-",colnames(prices)))
  }
  else {
    alloc = allocation(criteria,s,k,dir)
    colnames(alloc)=colnames(returns)
    if (dir==1) outcome = alloc*(exp(returns)-1-bpsspread/10000)
    else outcome = alloc*(exp(-returns)-1-bpsspread/10000)
    if (v==1){
      if(per==12) vols = lag(to.monthly(vols_d)[,4])
      else if(per==52) vols = lag(to.weekly(vols_d)[,4])
      else vols = lag(vols_d)
      avgvol = xts(rowMeans(alloc*vols,na.rm=TRUE),index(vols))
    }
  }
  test = rowMeans(outcome,na.rm=TRUE)
  if(v==1) test = test*0.17/avgvol
  if (e==0) SR(test,per)
  else {
    if (e==1) test
    else alloc
  }
}
tsavgmatrices <- function(indices,indiceso,n=232,cost,carry=0,carrycost0=0,t=0.17,dir="both"){
  xx=tsavgall(indices,n,cost,carry,carrycost0,dir=dir,e=1)
  xx=fillerxts(xx)
  xxe = tsmomenh(xx,t)
  xx_val = cumulate(xx)
  xxe_val = cumulate(xxe)
  xxdirs=tsavgall(indices,n,cost,carry,carrycost0,dir=dir,e=2)
  xx_valo = lag(xx_val)*(1+xxdirs*(indiceso/lag(indices)-1))
  xx_val=fillerxts(xx_val)
  xx_valo=fillerxts(xx_valo)
  xxe_valo = lag(xxe_val)*(1+xxdirs*(indiceso/lag(indices)-1))
  xxe_val=fillerxts(xxe_val)
  xxe_valo=fillerxts(xxe_valo)
  return(list(xx_val,xx_valo,xxe_val,xxe_valo))
}
coint <- function(indices,i){
  ind = indices[,i]['/1995-07']
  SR(diff(log(tt)),252)
  tt = as.numeric(ind[1])*cumulate(tsmomenh(diff(log(ind)),0.165,1))
  plot(log(tt))
  lines(log(ind),col=2)
  #SR(diff(log(tt)),252)
  plot((tt-indices[,i])['1995-07/'])
  dane=merge(tt,ind)
  colnames(dane) = c("tt","ind")
  summary(lm(tt~ind-1,data=dane))
  SR(diff(log((tt-indices[,1])['1995-07/'])),252)
  SR(vreturns['1995-07/'],252)
  tt = tt-indices[,i]
  t2 = tt['1995-01/']
  plot((t2))
  SR(diff(log(t2)),252)
  SR(diff(log(tt['1995-07/'])),252)
  SR(diff(log(indices[,1]['2016-01/'])),252)
}
pcaanalysis <- function(){
  tt=1/ccys['1997-01/']
  summary(tt)
  
  fxpca <- prcomp(tt,center = TRUE,scale. = TRUE)
  fxpcaret <- prcomp(diff(log(ccys['1997-01/']))[-1,],center = TRUE,scale. = TRUE)
  summary(fxpca)
  summary(fxpcaret)
  tt = xts(predict(fxpca),index(tt))
  tt2 = xts(predict(fxpcaret),index(tt)[-1])
  tt2 = cumulate(tt2)
  plot(tt[,1])
  plot(log(tt2[,1]))
  
  indpcaret <- prcomp(diff(log(indices['1997-01/']))[-1,-c(18)],center = TRUE,scale. = TRUE)
  summary(indpcaret)
  tt3 = xts(predict(indpcaret),index(indices['1997-01/'])[-1])
  tt3 = cumulate(tt3)
  plot(log(tt3[,1]))
  
  metpcaret <- prcomp(diff(log(fut_d['1997-01/']))[-1,],center = TRUE,scale. = TRUE)
  summary(metpcaret)
  tt4 = xts(predict(metpcaret),index(fut_d['1997-01/'])[-1])
  tt4 = cumulate(tt4)
  plot(log(tt4[,1]))
  
  cor(cbind(tt2[,1],tt3[,1],tt4[,1]),use="complete.obs")
  
  round(cor(cbind(-diff(log(ccys)),diff(log(indices[,-18])))['1997-01/'],use="complete.obs"),3)[1:19,20:42]
  
  ret = diff(log(indices['1997-01/'][,1]))
  fxret = diff(log(ccys['1997-01/']))
  fxret = merge(fxret,ret,join="left")
  
  summary(lm(X.spx~usdeur+usdjpy+usdcad+usdsek+usdaud+usdmxn+usdnzd-1,data=fxret))
  lm1=lm(X.spx~usdeur+usdjpy+usdcad+usdsek+usdaud+usdmxn+usdnzd-1,data=fxret)
  plot2(ts(cumsum(predict(lm1))),log(indices[,1]['1997-01/']))  
  
  rm(tt,tt2,tt3,tt4,lm1,ret,fxret,fxpca,fxpcaret,indpcaret,metpcaret)
}
mom4strat <- function(returns,dir=1,k=3,j=1,s=1,v=0,e=0,f=12,pos=0,p=1) {
  options(warn=-1)
  vols = returns
  volpar=3
  skews = returns
  sumreturns = returns
  for(i in 1:dim(vols)[2]) {
    sumreturns[,i] = cumsum(na20(returns[,i]))
    #tmp = emavol(vols[,i],volpar)^p
    #vols[,i] = tmp
    #skews[,i] = rollskew(returns[,i],1/3)
  }
  for(i in 1:dim(vols)[2]) {
    #vols[which(vols[,i]==0),i] = rowMeans(vols,na.rm=TRUE)[which(vols[,i]==0)]
  }
  vols2 = returns
  for(i in 1:dim(vols)[2]) {
    #tmp = emavol(returns[,i]/vols[,i],volpar)^p
    #vols2[,i] = tmp
  }
  
  criteria = lag(diff(sumreturns,lag=j))
  
  #prices=prices[paste0(as.Date(first(index(criteria))),'/'),]
  limit = as.Date(min(last(index(returns)),last(index(criteria))))
  returns=returns[paste0('/',limit)]
  criteria=criteria[paste0('/',limit)]
  
  index = index(criteria)  
  #criteria = coredata(criteria)[-(1),]
  n = dim(criteria)[1]
  m = dim(criteria)[2]
  
  alloc = allocation(criteria,s,k,dir,pos)
  if (dir!=1) returns = -returns #coredata(lreturns)[-(1),]
  test = portfolio(returns,criteria,s,k,dir,pos)
  
  if(v>=1) {
    #avgvol = xts(rowMeans(abs(alloc)*vols2,na.rm=TRUE),index(vols2))
    #avgskew = xts(rowMeans(abs(alloc)*skews,na.rm=TRUE),index(skews))
  }    
  
  if(v==1) test = test #levret(0.15^p/lag(avgvol),test)
  else if(v==2) {
    meanswitch = (sign(cumsum(na20(test^3)))+1)/2
    meanswitch = lag(meanswitch)
    meanswitch[1:2] = 1
    #test = (1-meanswitch) *  levret(0.15^p/lag(avgvol),test) + (meanswitch)*test
  }
  else if(v==3) {
    #if (dir==1) returns = levret(alloc,returns) *0.15^p/vols_l^p
    #else returns = levret(alloc,-returns) *0.15^p/vols_l^p
    #test = xts(log(rowMeans(exp(returns),na.rm=TRUE)),index(returns)) #*0.10^p/lag(avgvol)
  }
  options(warn=0)
  if (e==0) SR(test,f)
  else {
    if (e==1) test #exp(cumsum(c(0,test)))
    else {
      if(e==2) mat.na20(alloc)*dir
      else avgskew #0.15^p/lag(avgvol)
    }
  }
}
weeks <- function(){
  #date to week number
  library(lubridate)
  SPY$SPY.week <- week(index(SPY))
  #week number to date
  data <- 1:4
  weeks <- c('201501','201502','201552','201553')
  weeks_2 <- as.Date(weeks,format='%Y%w') 
  xts(data, order.by = weeks_2)
  
  test <- xts(data, order.by = weeks_2)
  index(test)
  
  weeks <- c('201501','201502','201552','201553')
  as.Date(paste0(weeks,'1'),format='%Y%W%w') # paste a dummy day
}
prod <- function(prices_d,prices_do,bpsspread,criteria0,carry=0,ycost=0.01,dir=1,k=3,s=1,v=0,e=0,f=12,pos=0,p=1,h=1,sh=0,w=2,date=0){
  if(sh>=h) {stop("Shift is greter than holding period. Respecify input.")}
  options(warn=-1)
  vreturns = diff(log(prices_d))
  vols = vreturns
  volpar=63
  target=0.35
  skews = vreturns
  for(i in 1:dim(vols)[2]) {
    tmp = emavol(vols[,i],volpar)^p
    vols[,i] = tmp
    #skews[,i] = rollskew(vreturns[,i],1/63)
  }
  for(i in 1:dim(vols)[2]) {
    vols[which(vols[,i]==0),i] = rowMeans(vols,na.rm=TRUE)[which(vols[,i]==0)]
  }
  vols2 = vreturns
  for(i in 1:dim(vols)[2]) {
    tmp = emavol(vreturns[,i]/vols[,i],volpar)^p
    vols2[,i] = tmp
  }
  
  if(f==12) vols_l = mat.to.monthly(vols)
  else if (f==52) vols_l = mat.to.weekly(vols) 
  else vols_l = vols
  
  if(h>1){
    criteria0 = elim_shift(criteria0,h,sh)  
  }
  criteria = -criteria0
  alloc = allocation(criteria,s,k,dir,pos)
  #alloc = elim_shift(alloc,h,sh)  
  
  if(v>=1) {
    if(f==12) {
      alloc_d = merge(prices_d[,1], xts(coredata(alloc), as.Date(index(alloc))))[,-1]
      alloc_d = fillerxtsM(alloc_d)
    }
    else if (f==52) {
      alloc_d = merge(prices_d[,1], xts(coredata(alloc), as.Date(index(alloc))))[,-1]
      alloc_d = fillerxtsW(alloc_d) #not tested yet
      if(h>1){
        alloc2 = lag(alloc,-1)
        alloc_w = merge(to.weekly(prices_d[,1])[,4], xts(coredata(alloc2), as.Date(index(alloc2))))[,-1]
        alloc_w = fillerxtsWh(alloc_w,h,sh) #not tested yet 
      }
      else{
        alloc_w = alloc
      }
    }
    else alloc_d = alloc
    colnames(alloc_d)=colnames(prices_d)
    
    if(w==1) vols=vols
    if(w==2) vols=vols2
    if(f==12 & h==1) vol = mat.to.monthly(vols)
    else if (f==252 & h==1) vol = vols
    else if (f==52) vol = mat.to.weekly(vols)
    
    #avgvol = xts(rowMeans(abs(alloc)*vol,na.rm=TRUE),index(vol))
    if(h==1) avgvol = xts(rowMeans(abs(alloc)*vol,na.rm=TRUE),index(abs(alloc)*vol))
    else avgvol = xts(rowMeans(abs(alloc_w)*vol,na.rm=TRUE),index(vol))
    #avgskew = xts(rowMeans(abs(alloc_d)*skews,na.rm=TRUE),index(skews))
    avgvol_mat = xts(matrix(rep(avgvol,dim(prices_d)[2]),dim(avgvol)[1],dim(prices_d)[2]),index(avgvol))
    
    if(v==4) {
      avgvol_l = xts(rowMeans(abs(alloc_d)*vols_l,na.rm=TRUE),index(vols_l))
      if(f==12) avgvol_l = to.monthly(avgvol_l)[,4]
      else if (f==52) avgvol_l = to.weekly(avgvol_l)[,4]
    }
  }    
  
  #test = xts(c(NA,test),index(prices))
  positions = alloc*dir
  if(v==1) {
    positions = positions*target^p/avgvol_mat
  }
  options(warn=0)
  positions = xts(coredata(positions),index(positions)+1/f)
  if (date==0) last(positions)
  else positions[date]
}
prodd <- function(prices_d,prices_do,bpsspread,criteria0,carry=0,ycost=0.01,k=3,s=1,v=0,f=12,pos=0,p=1,h=1,sh=0,w=2,date=0){
  long  = prod(prices_d,prices_do,bpsspread,criteria0,carry=carry,ycost=ycost,dir=1,k=k,s=s,v=v,f=f,pos=pos,p=p,h=h,sh=sh,w=w,date=date)
  short = prod(prices_d,prices_do,bpsspread,criteria0,carry=carry,ycost=ycost,dir=-1,k=k,s=s,v=v,f=f,pos=pos,p=p,h=h,sh=sh,w=w,date=date)
  mat.02na(mat.na20(long)+mat.na20(short))
}
validation <- function(limit,prices_d,prices_do,bpsspread,criteria0,carry=0,ycost=0.01,k=3,s=1,v=0,f=12,pos=0,p=1,h=1,sh=0){
  prices_d_=prices_d[paste0('/',limit)]
  prices_do_=prices_do[paste0('/',limit)]
  criteria0_=criteria0[paste0('/',limit)]
  tt=prodd(prices_d_,prices_do_,bpsspread,criteria0_,carry=carry,ycost=ycost,k=k,s=s,v=v,f=f,pos=pos,p=p,h=h,sh=sh)
  if(f==12) {prices = mat.to.monthly(prices_d); priceso = mat.to.monthly(prices_do,1)}
  else if (f==52) {prices = mat.to.weekly(prices_d); priceso = mat.to.weekly(prices_do,1)}
  else {prices =  prices_d; priceso =  prices_do}
  if (length(carry)==1) l.carry=lag(carry)[1]
  else l.carry=lag(carry)
  returns = (prices/priceso)-1 + l.carry/f*h
  prices_m_ = mat.to.monthly(prices_d_)
  ret = returns[last(index(prices_m_))+1/f]
  allcosts = ycost/f*h + bpsspread/10000
  #out=as.numeric(mat.levret(tt,ret))-std2log(pmax(as.numeric(tt),0)*log2std(allcosts))+std2log(pmin(as.numeric(tt),0)*log2std(allcosts))
  lret = sweep(ret,2,allcosts,'-')
  sret = sweep(-ret,2,allcosts,'-')
  out = as.numeric(pmax(as.numeric(tt),0)*lret) + as.numeric(abs(pmin(as.numeric(tt),0))*sret)
  names(out)=colnames(ret)
  std2log(out)
  #sweep(mat.levret(tt,ret),2,pmax(tt,0)*allcosts-pmin(tt,0)*allcosts,'-')
}
intraday1min15min<-function(name,dane){
  rownames(dane) = as.character(as.POSIXlt(strptime(as.character(dane[,1]),format="%Y%m%d %H%M%S")))
  colnames(dane)=c("Time","Open","High","Low","Close","Volume")
  dane15 = to.minutes15(dane[,-1])
  colnames(dane15)=c("Open","High","Low","Close","Volume")
  setwd(data_folder)
  write.table(dane15,paste0(name,"15M.csv"),sep=",")
}
carry12_1 <- function(prices_d,criteria0,dir=1,k=3,s=1,f=12,pos=0,h=1,sh=0,tr=NULL) {
  if(1==1){##preparation 
    if(sh>=h) {stop("Shift is greter than holding period. Respecify input.")}
    options(warn=-1)
    if(f==12) {prices = mat.to.monthly(prices_d,4)}
    else if (f==52) {prices = mat.to.weekly(prices_d,4)}
    else {prices =  prices_d}

    if(h>1){
      prices = elim_shift(prices,h,sh)  
      priceso = lag(elim_shift(lag(priceso,-1),h,sh))
      criteria0 = elim_shift(criteria0,h,sh)  
    }
    
    range_limit = as.Date(min(last(index(prices)),last(index(criteria0))))
    criteria0=criteria0[paste0('/',range_limit)]
    prices=prices[paste0(as.Date(first(index(criteria0))),'/'),]
    
    criteria = lag(-criteria0,1)  
    
  }##preparation   
  
  if(dir==3) {
    criteriaC = (criteria-rowMeans(criteria,na.rm=T))
    alloc = -1*criteriaC #/rowSums(abs(criteriaC),na.rm=T)
  }
  if(dir==4) {
    #criteriaC = (criteria-rowMeans(criteria))
    alloc = criteria/rowSums(abs(criteria),na.rm=T)
  }
  else {
    alloc = allocation(criteria,s,k,dir,pos,tr) ## MAIN function, gives 1 or NA, it is pure selector
    #alloc = allocationRF(criteria,diff(log(prices)),s,k,dir,pos)
    #alloc = elim_shift(alloc,h,sh)  
  }  
  
  positions = alloc*dir ## pure selector plus direction
  positions
}
carry12_2 <- function(prices_d,prices_do,bpsspread,criteria0,carry=0,ycost=0.01,dir=1,k=3,v=0,e=0,f=12,p=1,h=1,sh=0,w=2,inv=0,highs=0,lows=0,limit=0.05,tp=1) {
  if(1==1){##preparation 
    if(sh>=h) {stop("Shift is greter than holding period. Respecify input.")}
    options(warn=-1)
    if(f==12) {prices = mat.to.monthly(prices_d,4); priceso = mat.to.monthly(prices_do,1)}
    else if (f==52) {prices = mat.to.weekly(prices_d,4); priceso = mat.to.weekly(prices_do,1)}
    else {prices =  prices_d; priceso =  prices_do}
    if(length(highs)>1 & length(lows)>1){
      if(f==12) {pricesh = mat.to.monthly(highs,2); pricesl = mat.to.monthly(lows,3)}
      else if (f==52) {pricesh = mat.to.weekly(highs,2); pricesl = mat.to.weekly(lows,3)}
      else {pricesh = highs; pricesl = lows}
    }
    
    vreturns = diff(log(prices_d)) #/2+prices_do/2))
    vols = vreturns
    volpar=63
    target=0.15
    
    skews = vreturns
    for(i in 1:dim(vols)[2]) {
      tmp = emavol(vols[,i],volpar)^p
      vols[,i] = tmp
      #skews[,i] = rollskew(vreturns[,i],1/63)
    }
    for(i in 1:dim(vols)[2]) {
      vols[which(vols[,i]==0),i] = rowMeans(vols,na.rm=TRUE)[which(vols[,i]==0)]
    }
    vols2 = vreturns
    for(i in 1:dim(vols)[2]) {
      tmp = emavol(vreturns[,i]/vols[,i],volpar)^p
      vols2[,i] = tmp
    }
    
    if(f==12) vols_l = mat.to.monthly(vols)
    else if (f==52) vols_l = mat.to.weekly(vols) 
    else vols_l = vols
    
    if (length(carry)==1) {
      l.carry=lag(carry)[1]
    }
    else {
      l.carry=lag(carry)
    }    

    returns_w = (prices/priceso)-1 + carry/f
    lreturns_w = sweep(returns_w,2,ycost/f + bpsspread/10000/4,'-')
    sreturns_w = sweep(-returns_w,2,ycost/f + bpsspread/10000/4,'-')
    if(h>1){
      prices = elim_shift(prices,h,sh)  
      priceso = lag(elim_shift(lag(priceso,-1),h,sh))
      criteria0 = elim_shift(criteria0,h,sh)  
    }
    
    range_limit = as.Date(min(last(index(prices)),last(index(criteria0))))
    prices=prices[paste0('/',range_limit)]
    priceso=priceso[paste0('/',range_limit)]
    if(length(highs)>1 & length(lows)>1){
      pricesh=pricesh[paste0('/',range_limit)]
      pricesl=pricesl[paste0('/',range_limit)]
    }    
    #carry=carry[paste0('/',range_limit)]
    criteria0=criteria0[paste0('/',range_limit)]
    prices=prices[paste0(as.Date(first(index(criteria0))),'/'),]
    
    returns = (prices/priceso)-1 + carry/f*h
    if(inv==1) {
      invprices_d = prices_d
      for(i in 1:dim(prices_d)[2]) invprices_d[,i] = quickrecip3(prices_d[,i])
      #invprices_do = prices_do
      #for(i in 1:dim(prices_d)[2]) invprices_do[,i] = quickrecip3(prices_do[,i])
      invprices_d = mat.12na(invprices_d)
      invprices_do = lag(invprices_d)*(1-(prices_do/lag(prices_d)-1))
      #invprices_do = mat.12na(invprices_do)
      if(f==12) {invprices = mat.to.monthly(invprices_d); invpriceso = mat.to.monthly(invprices_do,1)}
      else if (f==52) {invprices = mat.to.weekly(invprices_d); invpriceso = mat.to.weekly(invprices_do,1)}
      else {invprices =  invprices_d; invpriceso =  invprices_do}
      invreturns = (invprices/invpriceso)-1 - carry/f*h
    }
    else invreturns = -returns
    lreturns = sweep(returns,2,ycost/f*h + bpsspread/10000,'-')
    sreturns = sweep(invreturns,2,ycost/f*h + bpsspread/10000,'-')
    #sreturns = sweep(levret(-1,returns),2,log(ycost+1)/f*h + log(bpsspread/10000+1),'-')
    #sreturns = sweep((-1*returns),2,log(ycost+1)/f*h + log(bpsspread/10000+1),'-')

    alloc = criteria0/dir
  }##preparation   
  
  if(v>=1) {
    if(f==12) {
      alloc_d = merge(prices_d[,1], xts(coredata(alloc), as.Date(index(alloc))))[,-1]
      alloc_d = fillerxtsM(alloc_d)
    }
    else if (f==52) {
      alloc_d = merge(prices_d[,1], xts(coredata(alloc), as.Date(index(alloc))))[,-1]
      alloc_d = fillerxtsW(alloc_d) #not tested yet
      if(h>1){
        alloc2 = lag(alloc,-1)
        alloc_w = merge(to.weekly(prices_d[,1])[,4], xts(coredata(alloc2), as.Date(index(alloc2))))[,-1]
        alloc_w = fillerxtsWh(alloc_w,h,sh) #not tested yet 
        if (dir==1) returns = alloc_w*lreturns_w
        else returns = alloc_w*sreturns_w 
        test = xts(rowMeans(returns,na.rm=TRUE),index(returns))
      }
      else{
        alloc_w = alloc
      }
    }
    else alloc_d = alloc
    colnames(alloc_d)=colnames(prices)
    
    if(w==1) vols=vols
    if(w==2) vols=vols2
    if(f==12 & h==1) vol = lag(mat.to.monthly(vols))
    else if (f==252 & h==1) vol = lag(vols)
    else if (f==52) vol = lag(mat.to.weekly(vols))
    
    if(h==1) avgvol = xts(rowMeans(abs(alloc/alloc)*vol,na.rm=TRUE),index(abs(alloc)*vol)) # alloc/alloc to get unity
    else avgvol = xts(rowMeans(abs(alloc_w/alloc_w)*vol,na.rm=TRUE),index(abs(alloc_w)*vol)) ## hard to explain why NaNs are produced sometimes here
    avgvol = fillerxts(avgvol) ## to remove NAs and NaNs
    #avgskew = xts(rowMeans(abs(alloc_d)*skews,na.rm=TRUE),index(skews))
    avgvol_mat = xts(matrix(rep(avgvol,dim(prices_d)[2]),dim(avgvol)[1],dim(prices_d)[2]),index(avgvol))
    colnames(avgvol_mat)=colnames(prices_d)
    
    if(v==4) {
      avgvol_l = xts(rowMeans(abs(alloc_d)*vols_l,na.rm=TRUE),index(abs(alloc_d)*vols_l))
      if(f==12) avgvol_l = to.monthly(avgvol_l)[,4]
      else if (f==52) avgvol_l = to.weekly(avgvol_l)[,4]
    }
  }    
  
  #test = xts(c(NA,test),index(prices))
  positions = alloc*dir ## pure selector plus direction
  if (dir==1) returns = lreturns
  else returns = sreturns
  test = xts(rowMeans(positions*returns,na.rm=T),index(positions*returns))
  
  if(v==0) mod2alloc=1
  if(v==1) {
    mod2alloc = target^p/avgvol_mat
    test = target^p/avgvol*test
    positions = positions*mod2alloc
    if(cap!=NULL) positions = mat.cens(positions,cap)
  }
  else if(v==2) {
    meanswitch = (sign(cumsum(na20(test^3)))+1)/2
    meanswitch = lag(meanswitch)
    meanswitch[1:2] = 1
    test = (1-meanswitch) *  target^p/avgvol*test + (meanswitch)*test
  }
  else if(v==3) {
    #if (dir==1) returns = alloc_w*lreturns_w #levret(alloc,lreturns) #*0.15^p/lag(vols_l)^p
    if (dir==1) returns = alloc*lreturns #levret(alloc,lreturns) #*0.15^p/lag(vols_l)^p
    else returns = alloc*sreturns #levret(alloc,sreturns) #*0.15^p/lag(vols_l)^p
    test = xts(rowMeans(returns,na.rm=TRUE),index(returns)) #*0.10^p/lag(avgvol)
  }
  else if(v==4) {
    test = target^p/lag(avgvol_l)*test
  }
  options(warn=0)
  if (e==0) SR(std2log(test),f)
  else if (e==1) std2log(test) #exp(cumsum(c(0,test)))
  else {
    if (dir==1) returns = lreturns
    else returns = sreturns
    if(e==2) std2log(mod2alloc*alloc*returns) #mod2alloc*alloc = positions/dir
    else if(e==3) positions #avgskew #0.15^p/lag(avgvol)
    else if(e==4) {
      #returns = (prices/priceso)-1
      capped_returns = sltp(dir,std2log(mod2alloc*alloc*returns),prices_do,highs,lows,prices_d,limit,cost=bpsspread/10000,tp,f,carry,ycost)
      std2log(xts(rowMeans(log2std(capped_returns[[1]]),na.rm=TRUE),index(mod2alloc*alloc*returns)))
    }
    else {#e==5
      #returns = (prices/priceso)-1
      capped_returns = sltp(dir*mod2alloc,std2log(mod2alloc*alloc*returns),prices_do,highs,lows,prices_d,limit,cost=bpsspread/10000,tp,f,carry,ycost)
      capped_returns2 = NA#levret(mod2alloc,sltp(dir,std2log(alloc*returns),prices_do,highs,lows,prices_d,limit,cost=0.00015,tp,f,carry,ycost)[[1]])
      list(capped_returns[[1]],std2log(mod2alloc*alloc*returns),mod2alloc,capped_returns[[2]],capped_returns2)
    }
  }
}
sumNoNA <- function(...){
  input = list(...)
  input_list <- as.list(substitute(list(...)))
  len = length(input_list)
  for (i in 2:len) {
    if(i==2) out = mat.na20(input[[i-1]])
    else out = out + mat.na20(input[[i-1]])
  }
  out
}
