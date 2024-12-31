source("RQS.R")
source("data_setup.R")

data_folder = "C:/temp/"
ccy_setup()
indices_setup()
comm_setup()

macrodays <- function(){
  futvalue_d = fut_d - lag(fut_dsmoo,5*252)
  
  fstartd=fstart*21+3907
  fd1=momentum12d(futenh_d,fut_spreads/4,0,futcost,2,fstartd,j=21*10,v=vcorr,e=1,f=252);SR(fd1['/2014-06'],252)
  #fw2=momentum12d(fut_d,fut_spreads/4,fut_carry_w,futcost,2,fstartw,j=4*5,v=vcorr,e=1,f=52);SR(fw2['/2014-06'],52)
  fd3=carry12d(fut_d,fut_spreads/4,-fut_carry_d,fut_carry_d,futcost,1,fstartd,v=vcorr,e=1,f=252);SR(fd3['/2014-06'],252)
  fd4=carry12d(fut_d,fut_spreads/4,futvalue_d,fut_carry_d,futcost,1,fstartd,v=vcorr,e=1,f=252);SR(fd4['/2014-06'],252)
  SR(rowMeans(cbind(fd1,fd3)['/2014-06'],na.rm=T),252)
  SR(rowMeans(cbind(fd1,fd3)['2014-07/'],na.rm=T),252)
  tt=cbind(fd1,fd3)
  pp=xts(cumsum(na20(rowMeans(tt,na.rm=TRUE))),index(tt))
  plot((pp))
  
  startd=start*21+126
  c2i_d = diff(log(ccys3_d),lag=21*13)
  i2c_d = diff(log(indices3_d),lag=21*10)
  td1=momentum12d(futccy_d,ccy_spreads/4,0,0.01,2,startd,j=21*1,v=vcorr,e=1,f=252);SR(td1['/2014-06'],252)
  td2=momentum12d(ccys,ccy_spreads/4,carry_d,0.01,2,startd,j=1,v=0,e=1,f=252);SR(td2['/2014-06'],252)
  td4=carry12d(ccys,ccy_spreads/4,-carry_d,carry_d,0.01,2,startd,v=vcorr,e=1,f=252);SR(td4['/2014-06'],252)
  #td4b=carry12(ccys,ccy_spreads/4,-carry_w,carry_w,0.01,-1,2,startd,v=vcorr,e=1,f=252);SR(td4b['/2014-06'],252)
  td10=carry12d(ccys3,ccy3_spreads/4,i2c_d,carry3_d,ycost=0.01,1,startd,v=vcorr,e=1,f=252);SR(td10['/2014-06'],252)
  td11=momentum12d(zze_val,ccy_spreads/4,0,0,2,startd,j=21*1,v=vcorr,e=1,f=252);SR(td11['/2014-06'],252)
  
  td3=carry12d(ccys['/2017-09'],ccy_spreads/4,lag(ycpi),carry_w,0.01,2,startd,v=0,e=1,f=52);SR(td3['/2014-06'],52)
  td5=carry12d(ccys['/2017-09'],ccy_spreads/4,lag(rccys),carry_w,0.01,2,startd,v=0,e=1,f=52);SR(td5['/2014-06'],52)
  td7=carry12d(ccys['/2017-09'],ccy_spreads/4,(lag(cindpot)),carry_w,0.01,2,startd,v=vcorr,e=1,f=52);SR(td7['/2014-06'],52)
  td8=carry12d(ccys['/2017-05'],ccy_spreads/4,lag(rm3),carry_w,0.01,2,startd,v=vcorr,e=1,f=52);SR(td8['/2014-06'],52)
  td9=carry12d(ccys['/2017-08'],ccy_spreads/4,rcind2,carry_w,0.01,2,startd,v=0,e=1,f=52);SR(td9['/2014-06'],52)
  
  SR(rowMeans(cbind(td4,td11)['/2014-06'],na.rm=T),252)
  SR(rowMeans(cbind(td4,td11)['2014-07/'],na.rm=T),252)
  SR(rowMeans(cbind(td4,td11,td10)['2014-07/'],na.rm=T),252)
  
  istartd=start*21+1102
  id1=momentum12d(indices,indices_spreads/4,-indices_rates_d,0.0175,4,startd,j=21*11,v=vcorr,e=1,f=252);SR(id1['/2014-06'],52)
  id2=momentum12d(futind_d,indices_spreads/4,0,0.0175,4,startd,j=21*11,v=vcorr,e=1,f=252);SR(id2['/2014-06'],252)
  id3=carry12d(indices3,ind3_spreads/4,c2i_d,0,ycost=0.0,2,startd,v=vcorr,e=1,f=252);SR(id3['/2014-06'],252)
  id3=carry12(indices3,ind3_spreads/4,c2i_d,0,ycost=0.0,1,2,startd,v=vcorr,e=1,f=252);SR(id3['/2014-06'],252)
  id5 = momentum12d(xxe_val,indices_spreads/4,0,0,2,startd,j=21*2,v=vcorr,e=1,f=252);SR(id5['/2014-06'],252)
  id4=carry12d(indices,indices_spreads/4,rind_d,-indices_rates,0.0175,1,2,startd,v=vcorr,e=1,f=252);SR(id4,252)
  
  SR(rowMeans(cbind(id1,id2,id3,id5)['/2014-06'],na.rm=T),252)
  SR(rowMeans(cbind(id1,id2,id3,id5)['2014-07/'],na.rm=T),252)
  SR(rowMeans(cbind(id1,id5)['/2014-06'],na.rm=T),252)
  SR(rowMeans(cbind(id1,id5)['2014-07/'],na.rm=T),252)
  
  ad=binding(cbind(td4,td11))
  bd=binding(cbind(fd1,fd3))
  cd=binding(cbind(id1,id5))
  mat.SR(cbind(ad,bd,cd)['/2014-06'],252)
  mat.SR(cbind(ad,bd,cd)['2014-07/'],252)
  SR(rowMeans(cbind(ad,cd)['/2014-06'],na.rm=T),252)
  SR(rowMeans(cbind(ad,cd)['2014-07/'],na.rm=T),252)  
  adm = day2monthret(ad)
  bdm = day2monthret(bd)
  cdm = day2monthret(cd)
  id5m = day2monthret(id5)
  td11m = day2monthret(td11)
  td4m = day2monthret(td4)
  SR(rowMeans(cbind(adm,cdm)['2014-07/'],na.rm=T),12)  
}#final days

macroweeks <- function(){
  fut_w = mat.to.weekly(fut_d)
  fut_carry_w = mat.to.weekly(fut_carry_d)
  fut_carry_wf = mat.to.weekly(fut_carry_df)
  fut_carry_wsmoo = mat.to.weekly(fut_carry_dsmoo)
  futvalue_w = fut_w - lag(mat.to.weekly(fut_dsmoo),5*52)
  futenh_w = mat.to.weekly(futenh_d)
  
  fstartw=fstart*4+87
  fw1=momentum12d(futenh_d,fut_spreads/4,0,futcost,2,fstartw,j=4*5,v=vcorr,e=1,f=52);SR(fw1['/2014-06'],52)
  #fw2=momentum12d(fut_d,fut_spreads/4,fut_carry_w,futcost,2,fstartw,j=4*5,v=vcorr,e=1,f=52);SR(fw2['/2014-06'],52)
  fw3=carry12d(fut_d,fut_spreads/4,-fut_carry_w,fut_carry_w,futcost,1,fstartw,v=vcorr,e=1,f=52);SR(fw3['/2014-06'],52)
  #fw4=carry12d(fut_d,fut_spreads/4,futvalue_w,fut_carry_w,futcost,1,fstartw,v=vcorr,e=1,f=52);SR(fw4['/2014-06'],52)
  SR(rowMeans(cbind(fw1,fw3)['/2014-06'],na.rm=T),52)
  SR(rowMeans(cbind(fw1,fw3)['2014-07/'],na.rm=T),52)
  tt=cbind(fw1,fw3)
  pp=xts(cumsum(na20(rowMeans(tt,na.rm=TRUE))),index(tt))
  plot((pp))
  
  startw=start*4+95
  c2i_w = diff(log(ccys3_w),lag=4*13)
  i2c_w = diff(log(indices3_w),lag=4*10)
  tw1=momentum12d(futccy_w,ccy_spreads/4,0,0.01,2,startw,j=4*3,v=vcorr,e=1,f=52);SR(tw1['/2014-06'],52)
  #tw2=momentum12d(ccys,ccy_spreads/4,carry_w,0.01,2,start,j=4*3,v=0,e=1,f=52);SR(tw2['/2014-06'],52)
  tw4=carry12d(ccys,ccy_spreads/4,-carry_w,carry_w,0.01,2,startw,v=vcorr,e=1,f=52);SR(tw4['/2014-06'],52)
  #tw4b=carry12(ccys,ccy_spreads/4,-carry_w,carry_w,0.01,-1,2,startw,v=vcorr,e=1,f=52);SR(tw4b['/2014-06'],52)
  tw10=carry12d(ccys3,ccy3_spreads/4,i2c_w,carry3_w,ycost=0.01,1,startw,v=vcorr,e=1,f=52);SR(tw10['/2014-06'],52)
  tw11=momentum12d(zze_val,ccy_spreads/4,0,0,2,startw,j=4*1,v=vcorr,e=1,f=52);SR(tw11['/2014-06'],52)
  
  tw3=carry12d(ccys['/2017-09'],ccy_spreads/4,lag(ycpi),carry_w,0.01,2,startw,v=0,e=1,f=52);SR(tw3['/2014-06'],52)
  tw5=carry12d(ccys['/2017-09'],ccy_spreads/4,lag(rccys),carry_w,0.01,2,startw,v=0,e=1,f=52);SR(tw5['/2014-06'],52)
  tw7=carry12d(ccys['/2017-09'],ccy_spreads/4,(lag(cindpot)),carry_w,0.01,2,startw,v=vcorr,e=1,f=52);SR(tw7['/2014-06'],52)
  tw8=carry12d(ccys['/2017-05'],ccy_spreads/4,lag(rm3),carry_w,0.01,2,startw,v=vcorr,e=1,f=52);SR(tw8['/2014-06'],52)
  tw9=carry12d(ccys['/2017-08'],ccy_spreads/4,rcind2,carry_w,0.01,2,startw,v=0,e=1,f=52);SR(tw9['/2014-06'],52)
  
  SR(rowMeans(cbind(tw4,tw11)['/2014-06'],na.rm=T),52)
  SR(rowMeans(cbind(tw4,tw11)['2014-07/'],na.rm=T),52)
  SR(rowMeans(cbind(tw4,tw11,tw10)['2014-07/'],na.rm=T),52)
  
  iw1=momentum12d(indices,indices_spreads/4,-indices_rates_w,0.0175,4,startw,j=4*11,v=vcorr,e=1,f=52);SR(iw1['/2014-06'],52)
  iw2=momentum12d(futind_d,indices_spreads/4,0,0.0175,4,startw,j=4*11,v=vcorr,e=1,f=52);SR(iw2['/2014-06'],52)
  iw3=carry12d(indices3,ind3_spreads/4,c2i_w,0,ycost=0.0,2,startw,v=vcorr,e=1,f=52);SR(iw3['/2014-06'],52)
  #iw3=carry12(indices3,ind3_spreads/4,c2i_w,0,ycost=0.0,1,2,startw,v=vcorr,e=1,f=52);SR(iw3['/2014-06'],52)
  iw5 = momentum12d(xxe_val,indices_spreads/4,0,0,2,startw,j=4*2,v=vcorr,e=1,f=52);SR(iw5['/2014-06'],52)
  #iw4=carry12d(indices,indices_spreads/4,rind_m,-indices_rates,0.0175,1,2,startw,v=vcorr,e=1,f=52);SR(iw4,52)
  
  SR(rowMeans(cbind(iw1,iw2,iw3,iw5)['/2014-06'],na.rm=T),52)
  SR(rowMeans(cbind(iw1,iw2,iw3,iw5)['2014-07/'],na.rm=T),52)
  SR(rowMeans(cbind(iw1,iw5)['/2014-06'],na.rm=T),52)
  SR(rowMeans(cbind(iw1,iw5)['2014-07/'],na.rm=T),52)
  
  aw=binding(cbind(tw4,tw11))
  bw=binding(cbind(fw1,fw3))
  cw=binding(cbind(iw1,iw5))
  mat.SR(cbind(aw,bw,cw)['/2014-06'],52)
  mat.SR(cbind(aw,bw,cw)['2014-07/'],52)
  SR(rowMeans(cbind(aw,cw)['/2014-06'],na.rm=T),52)
  SR(rowMeans(cbind(aw,cw)['2014-07/'],na.rm=T),52)  
  awm = week2monthret(aw)
  bwm = week2monthret(bw)
  cwm = week2monthret(cw)
  iw5m = week2monthret(iw5)
  tw11m = week2monthret(tw11)
  tw4m = week2monthret(tw4)
  SR(rowMeans(cbind(awm,cwm)['2014-07/'],na.rm=T),12)  

  #tw4=carry12(ccys,lag(ccys),ccy_spreads/4,-carry_w,carry_w,0.01,-1,2,5,v=1,e=1,f=52,h=4,sh=0);SR(tw4['1998-01/']['/2014-06'],52)
  #tw4=carry12(ccys,lag(ccys),ccy_spreads/4,-carry_m,carry_m,0.01,-1,2,5,v=1,e=1,f=12,h=1,sh=0);SR(tw4['1998-01/']['/2014-06'],12)
  #tw4=carry12old(ccys,ccy_spreads/4,-carry_m,carry_m,0.01,dir=-1,k=2,s=start,v=3,e=1,f=12,pos=0,p=1);SR(tw4['1998-01/']['/2014-06'],12)
  #tw4=carry12(ccys,lag(ccys),ccy_spreads/4,-carry_m,carry_m,0.01,-1,2,start,v=3,e=1,f=12,h=1,sh=0);SR(tw4['/2014-06'],12)
  #plot(cumsum(na20(tw4)))
}#final week  

macrodouble <- function(){
  f1d=momentum12d(futenh_d,fut_spreads/4,0,futcost,2,fstart,j=11,v=1,e=1);SR(f1d)
  f2d=momentum12d(fut_d,fut_spreads/4,fut_carry_m,futcost,2,fstart,j=11,v=1,e=1);SR(f2d)
  f3d=carry12d(fut_d,fut_spreads/4,-fut_carry_m,fut_carry_m,futcost,1,fstart,v=1,e=1);SR(f3d)
  f4d=carry12d(fut_d,fut_spreads/4,futvalue,fut_carry_m,futcost,1,fstart,v=1,e=1);SR(f4d)
  f5d = momentum12d(yye_val,fut_spreads/4,0,0,1,fstart,j=1,v=1,e=1);SR(f5d,12)
  i1d=momentum12d(indices,indices_spreads/4,-indices_rates,eqcost,4,start,j=11,v=vcorr,e=1);SR(i1d,12)
  i2d=momentum12d(futind_d,indices_spreads/4,0,eqcost,4,start,j=11,v=1,e=1);SR(i2d,12)
  i3d=carry12d(indices3,ind3_spreads/4,c2i,0,ycost=0.0,2,start,v=vcorr,e=1);SR(i3d,12)
  i4d=carry12d(indices,indices_spreads/4,rind_m,-indices_rates,eqcost,2,start,v=1,e=1);SR(i4d,12)
  i5d = momentum12d(xxe_val,indices_spreads/4,0,0,2,start,j=1,v=1,e=1);SR(i5d,12)
  i6d = carry12d(indices,indices_spreads/4,-indSR,-indices_rates,eqcost,2,start,v=1,e=1);SR(i6d,12)
  i7d = carry12d(xxe_val,indices_spreads/4,-indSR,-indices_rates,eqcost,2,start,v=1,e=1);SR(i7d,12)
  i8d = carry12d(indices,indices_spreads/4,indvol_m,-indices_rates,eqcost,2,start,v=1,e=1);SR(i8d,12)
  i9d = carry12d(indices,indices_spreads/4,indbreak_m,-indices_rates,eqcost,6,start,v=1,e=1);SR(i9d,12)
  t1d=momentum12d(futccy_d,ccy_spreads/4,0,fxcost,2,start,j=3,v=1,e=1);SR(t1d,12)
  t2d=momentum12d(ccys,ccy_spreads/4,carry_m,fxcost,2,start,j=3,v=1,e=1);SR(t2d,12)
  t4d=carry12d(ccys,ccy_spreads/4,-carry_m,carry_m,fxcost,2,start,v=vcorr,e=1);SR(t4d,12)
  t7d=carry12d(ccys,ccy_spreads/4,(lag(cindpot)),carry_m,fxcost,2,start,v=vcorr,e=1);SR(t7d,12)
  t8d=carry12d(ccys,ccy_spreads/4,lag(rm3),carry_m,fxcost,2,start,v=vcorr,e=1);SR(t8d,12)
  t9d=carry12d(ccys,ccy_spreads/4,rcind2,carry_m,fxcost,2,start,v=0,e=1);SR(t9d,12)
  t11d = momentum12d(zze_val,ccy_spreads/4,0,0,2,start,j=1,v=1,e=1);SR(t11d,12)
  t3d=carry12d(ccys,ccy_spreads/4,lag(ycpi),carry_m,fxcost,2,start,v=vcorr,e=1);SR(t3d,12) 
  t5d=carry12d(ccys,ccy_spreads/4,lag(rccys),carry_m,fxcost,2,start,v=vcorr,e=1);SR(t5d,12)
  t10d=carry12d(ccys3,ccy3_spreads/4,i2c,carry3,ycost=fxcost,1,start,v=1,e=1);SR(t10d,12)  
}

portfolios_all<-function(){
  start=412
  start=319
  vcorr=2
  indices_swaprates = sweep(indices_rates, 1, -indices_rates[,1], FUN = "+")
  
  fxcost=0.01
  eqcost=0.0175
  fxcost=0
  eqcost=0
  
  #vv=tsavgall(cashind,232,cashind_spreads,-cashind_rates_d,0.0175,e=1)
  xx=tsavgall(indices[,1:s],232,indices_spreads[1:s],-indices_rates_d[,1:s]/10,eqcost,e=1)
  yy=tsavgall(fut_d['1997-07/'],232,fut_spreads,roll_rates_d['1997-07/'],futcost,e=1)
  zz=tsavgall(ccys,63,ccy_spreads,carry_d,fxcost,e=1)
  xxe = tsmomenh(xx,0.17)
  xxe = tsmomenh(xx,1)
  yy=fillerxts(yy)
  yye = tsmomenh(yy,0.15)
  zze = tsmomenh(zz,0.1)
  #vve_val = cumulate(vv)
  xx_val = cumulate(xx)
  xxe_val = cumulate(xxe)
  zz_val = cumulate(zz)
  zze_val = cumulate(zze)
  yy_val = cumulate(yy)
  yye_val = cumulate(yye)
  #SR(xts(rowMeans(xx,na.rm=TRUE),index(xx)),252)
  #SR(xts(rowMeans(zz,na.rm=TRUE),index(zz)),252)
  #SR(xts(rowMeans(yy,na.rm=TRUE),index(yy)),252)
  xxdirs=tsavgall(indices[,1:s],232,indices_spreads[1:s],-indices_rates_d[,1:s]/10,eqcost,e=2)
  xx_valo = lag(xx_val)*(1+xxdirs*(indiceso[,1:s]/lag(indices[,1:s])-1))
  xx_val=fillerxts(xx_val)
  xx_valo=fillerxts(xx_valo)
  xxe_valo = lag(xxe_val)*(1+xxdirs*(indiceso[,1:s]/lag(indices[,1:s])-1))
  xxe_val=fillerxts(xxe_val)
  xxe_valo=fillerxts(xxe_valo)
  yydirs=tsavgall(fut_d['1997-07/'],232,fut_spreads,roll_rates_d['1997-07/'],futcost,e=2)
  yy_valo = lag(yy_val)*(1+yydirs*(fut_do/lag(fut_d)-1))
  yy_val=fillerxts(yy_val)
  yy_valo=fillerxts(yy_valo)
  yye_valo = lag(yye_val)*(1+yydirs*(fut_do/lag(fut_d)-1))
  yye_val=fillerxts(yye_val)
  yye_valo=fillerxts(yye_valo)
  
  i=1
  SR(diff(log(indices[,i])),252)
  tt = as.numeric(indices[1,i])*cumulate(tsmomenh(diff(log(indices[,i])),0.175,1))
  plot(log(tt))
  lines(log(indices[,i]),col=2)
  #SR(diff(log(tt)),252)
  plot((tt-indices[,i])['1995-07/'])
  SR(diff(log((tt-indices[,i])['1995-07/'])),252)
  SR(vreturns['1995-07/'],252)
  dd = tt-indices[,i]
  t4 = dd['1995-01/']
  plot((t3))
  SR(diff(log(t2)),252)
  SR(diff(log(tt['1995-07/'])),252)
  SR(diff(log(indices[,i]['1995-07/'])),252)
  cor(diff(log(cbind(t1,t2,indices[,i]))),use="complete.obs")
  cor(diff(log(cbind(t1,t2,t3,t4))),use="complete.obs")
  SR(diff(log(t1))+diff(log(t2))+diff(log(t3))+diff(log(t4)),252)
  plot(cumsum(na20(diff(log(indices[,i]))^3)))
  abline(h=0,col=2)
  skewness(indices[,i]['/1995-07'])
  
  vreturns=diff(log(tt))
  summary(emavol(vreturns,63))
  SR(diff(log(cumulate(tsmomenh(diff(log(tt)),0.18)))),252)
  plot(emavol(vreturns,63))
  skewness(diff(log(indices[,1]['1995-01/'])),na.rm=T)
  
  vreturns = diff(log(indices[,1]))
  SR(levret(0.18^2/lag(emavol(vreturns,63))^2,vreturns),252)
  
  vix = ts2xts(read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=VIXCLS"))/100
  rv = SMA(diff(log(indices[,1]))^2,252)*252 - SMA(diff(log(indices[,1])),252)^2*252
  vrp = lag(vix^2,252)-rv
  vrpsqr = lag(vix,63)-sqrt(rv)
  plot2(vix,tt)
  plot(cumsum(na20(vrp)))
  
  tt=emavol(diff(log(indices[,1])),63)
  tt= lag(tt-EMA(tt,1/63))
  tt2 = as.numeric(tt>0)
  ret = diff(log(indices[,1]))
  summary(lm(ret~(tt)+lag(tt)))#,family="binomial"))
  plot(tt)
  
  megaxx = cbind(xxe_val,zze_val,yye_val)  
  xx=tsavgall(indices,232,indices_spreads,-indices_rates_d,eqcost,e=1)
  xxe_val = cumulate(xx)
  
  mega = cbind(indices,ccys,fut_d['1971-01/'])
  megao = cbind(indiceso,lag(ccys),fut_do['1971-01/'])
  mega = fillerxts(mega)
  megao = fillerxts(megao)
  megaxx = fillerxts(megaxx)
  #megafut = cbind(futind_d,futccy_d,futenh_d['1971-01/'])
  mega_carry = cbind(-indices_rates,carry_m,roll_rates_m)
  mega_carry_inf = cbind(-indices_rates,carry_m,-roll_rates_m)
  mega_spreads = c(indices_spreads,ccy_spreads,fut_spreads)
  mega_costs = c(rep(eqcost,24),rep(fxcost,19),futcost)
  vcorr=1
  mega = cbind(fut_d['1971-01/'],ccys)
  megao = cbind(fut_do['1971-01/'],lag(ccys))
  mega_carry = cbind(roll_rates_m,carry_m)
  mega_carry_inf = cbind(-roll_rates_ms,carry_m)
  mega_spreads = c(ccy_spreads,fut_spreads)
  mega_costs = c(futcost,rep(fxcost,19))
  
  #m1d=momentum12d(megafut,mega_spreads/4,0,mega_costs,4,start,j=6,v=vcorr,e=1);SR(m1d['/2014-06'])
  m2d=momentum12d(mega,megao,mega_spreads/4,mega_carry,mega_costs,4,start,j=11,v=vcorr,e=1);SR(m2d['/2014-06'],12)
  m3d=carry12d(mega,megao,mega_spreads/4,-mega_carry,mega_carry,mega_costs,6,start,v=vcorr,e=1);SR(m3d['/2014-06'],12)
  m4d=momentum12d(megaxx,lag(megaxx),mega_spreads/4,0,0.01,4,start,j=1,v=vcorr,e=1);SR(m4d['/2014-06'],12)
  
  SR(binding(cbind(m2d,m3d,t2d,m4d)['2006-09/'])['/2014-06'],12)
  SR(binding(cbind(m3d)['2006-09/'])['2014-07/'],12)
  plot(cumsum(na20(m3d)['1997-09/']))
  SR(rowMeans(cbind(m1d+m4d,2*m3d)['2014-07/'],na.rm=T),12)
  tt = cbind(m1d+m4d,2*m3d)
  tt = xts(cumsum(na20(rowMeans(tt,na.rm=T))),index(tt))
  plot(tt['1997-07/'])
  
  vcorr=2
  fstart=300
  f1=momentum12(futenh_d,fut_spreads/4,0,futcost,1,2,fstart,j=11,v=vcorr,e=1);#SR(f1['/2014-06'])
  f1b=momentum12(futenh_d,fut_spreads/4,0,futcost,-1,2,fstart,j=11,v=vcorr,e=1);#SR(f1b['/2014-06'])
  f1l=momentum12(futenh_d,fut_spreads/4,0,futcost,3,2,fstart,j=11,v=0,e=1,pos=0);SR(f1l['/2014-06'])
  #SR(rowMeans(cbind(f1,f1b,f1l)['/2014-06'],na.rm=T))
  f2=momentum12(fut_d,fut_spreads/4,fut_carry_m,futcost,1,2,fstart,j=11,v=vcorr,e=1);#SR(f2)
  f2b=momentum12(fut_d,fut_spreads/4,fut_carry_m,futcost,-1,2,fstart,j=11,v=vcorr,e=1);#SR(f2b)
  f3=carry12(fut_d,fut_spreads/4,-fut_carry_m,fut_carry_m,futcost,1,1,fstart,v=vcorr,e=1);#SR(f3)
  f3b=carry12(fut_d,fut_spreads/4,-fut_carry_m,fut_carry_m,futcost,-1,1,fstart,v=vcorr,e=1);#SR(f3b)
  f4=carry12(fut_d,fut_spreads/4,futvalue,fut_carry_m,futcost,1,1,fstart,v=vcorr,e=1);#SR(f4)
  f4b=carry12(fut_d,fut_spreads/4,futvalue,fut_carry_m,futcost,-1,1,fstart,v=vcorr,e=1);#SR(f4b)
  
  #mat.SR(cbind(f1+f1b,f2+f2b,f3+f3b,f4+f4b)['/2014-06'],12)
  #SR(rowMeans(cbind(f1,f1b)['/2014-06'],na.rm=T),12)
  #SR(rowMeans(cbind(f1,f1b,f3,f3b,f4,f4b)['/2014-06'],na.rm=T),12)
  #SR(rowMeans(cbind(f1,f1b,f3,f3b,f4,f4b)['2014-07/'],na.rm=T),12)

  idx_n = length(indices_names)
  logrets = diff(log(indices_m))["1999-11-30/"]
  logrets_sqr = xts(as.data.frame(matrix(NA,dim(logrets)[1],idx_n*(idx_n-1)/2)),index(logrets))
  indices_spreads_sqr = numeric(idx_n*(idx_n-1)/2)
  l=0
  for(i in 1:idx_n){
    for(j in 1:idx_n) {
      if(i>j) {l=l+1;logrets_sqr[,l] = logrets[,i] - logrets[,j]; indices_spreads_sqr[l] = indices_spreads[i]/2 + indices_spreads[j]/2}
    }
  }
  indices_sqr = exp(cumsum(logrets_sqr))
  plot(indices_sqr[,231])
  i1=momentum12(indices_sqr,indices_sqr,indices_spreads/4,-indices_rates/10,0.00175,1,4,start,j=11,v=0,e=1);SR(i1,12)
  plot.cum(i1)

  vcorr=1
  plot.cum(i1b["2006-01/"])
  plot(log(indices_m$`^spx`["2006-01/"]))
  diff(log(indices_m))["2008"]
  tt=binding(cbind(i1,-i1b))
  SR(tt["2006-01/"],12)
  SR(i1["2006-01/"],12)
  SR(diff(log(indices_m$`^spx`))["2006-01/"],12)
  tail(i1,18)

  i1=momentum12(indices,indiceso,indices_spreads/4,-indices_rates/10,0.00175,1,4,start,j=11,v=vcorr,e=1);SR(i1,12)
  i1b=momentum12(indices,indiceso,indices_spreads/4,-indices_rates/10,0.00175,-1,4,start,j=1,v=vcorr,e=1);SR(i1b,12)
  i2=momentum12(futind_d,indices_spreads/4,0,0.0175,1,4,start,j=11,v=vcorr,e=1);SR(i2,12)
  i2b=momentum12(futind_d,indices_spreads/4,0,0.0175,-1,4,start,j=11,v=vcorr,e=1);SR(i2b,12)
  
  tt=momentum12(futind_d,indices_spreads/4,0,0.0175,1,2,start,j=11,v=3,e=1)
  tt=momentum12(futenh_d,fut_spreads/4,0,futcost,1,2,fstart,j=11,v=3,e=1);
  SR(tt,12)
  
  newind = indices[,1:22]["1999-01-01/"]
  newindo = indiceso[,1:22]["1999-01-01/"]
  indpot = lag(indpot["/2018-03-01"]["1999-01-01/"])
  rm(newind,newindo)
  i1=carry12(newind,newind,indices_spreads[1:23]/4,indpot,0,ycost=0.0,1,4,1,v=vcorr,e=1);SR(i1,12)
  i3=carry12(indccys,indccyso,indccys_spreads/4,c2i,0,ycost=0.0,1,2,start,v=vcorr,e=1);SR(i3,12)
  i3b=carry12(indccys,indccyso,indccys_spreads/4,c2i,0,ycost=0.0,-1,2,start,v=vcorr,e=1);SR(i3b,12)
  i3d=carry12d(indccys,indccyso,indccys_spreads/4,c2i,0,ycost=0.0,2,start,v=vcorr,e=1);SR(i3d,12)
  i4=carry12(indices,indiceso,indices_spreads/4,rind_m,-indices_rates,0.0,1,2,start,v=vcorr,e=1);SR(i4,12)
  i4b=carry12(indices,indiceso,indices_spreads/4,rind_m,-indices_rates,0.0,-1,2,start,v=vcorr,e=1);SR(i4b,12)
  i4d=carry12d(indices,indiceso,indices_spreads/4,rind_m,-indices_rates,0.0,2,start,v=vcorr,e=1);SR(i4d,12)
  i5 = momentum12(xxe_val,xxe_valo,indices_spreads/4,0,0,1,2,start,j=1,v=vcorr,e=1);SR(i5,12)
  i5b = momentum12(xxe_val,xxe_valo,indices_spreads/4,0,0,-1,2,start,j=1,v=vcorr,e=1);SR(i5b,12)
  i6d = carry12d(indices,indiceso,indices_spreads/4,-indSR,-indices_rates,0.0175,2,start,v=1,e=1);SR(i6d,12)
  i7d = carry12d(xxe_val,xxe_valo,indices_spreads/4,-xindSR,-indices_rates,0.0175,2,start,v=1,e=1);SR(i7d,12)
  i8d = carry12d(indices,indiceso,indices_spreads/4,indvol_m,-indices_rates,0.0175,2,start,v=1,e=1);SR(i8d,12)
  s=1:23
  i9d = carry12d(indices[,s],indiceso[,s],indices_spreads[s]/4,-inddd_m[,s],-indices_rates[,s]/10,0.00175,2,start,v=1,e=1);SR(i9d,12)
  i9 = carry12(indices[,s],indiceso[,s],indices_spreads[s]/4,inddd_m[,s],-indices_rates[,s]/10,0.00175,1,2,start,v=1,e=1);SR(i9,12)
   
  indmaxp = maxproximity(indices,indicesH,5*51);
  indmaxp = diff(log(indices)); for(i in 1:dim(indmaxp)[2]) indmaxp[,i] = xts((rolldrawdown(indmaxp[,i],diff(log(indicesH[,i])),k=5*51)),index(indmaxp[,i]))
  benchmark = exp(cum(binding(diff(log(indices)))))
  for(i in 1:dim(indices)[2]) {tmp=rollbeta(indices[,i],benchmark,k=252); if(i==1) indbet=tmp else indbet = cbind(indbet,tmp)}
  indbet_m = mat.to.monthly(indbet)
  indmaxp_m = mat.to.monthly(indmaxp)
  i10=carry12(indices,indiceso,indices_spreads/4,-indmaxp_m,0,eqcost,1,3,start,v=0,e=1);SR(i10,12)  
  i11=carry12(indices,indiceso,indices_spreads/4,indbet_m,0,eqcost,1,3,start,v=0,e=1);SR(i11,12)

  indvol = diff(log(indices_m))
  for(i in 1:dim(indvol)[2]) indvol[,i] = sqrt(SMA(indvol[,i]^2,11)*12) #smavolSem(indvol[,i],11,12)
  indSR = diff(log(indices_m),lag=11)*12/11 / indvol
  xxe_val_m = mat.to.monthly(xxe_val)
  xindvol = diff(log(xxe_val_m))
  for(i in 1:dim(xindvol)[2]) xindvol[,i] = sqrt(SMA(xindvol[,i]^2,11)*12)
  xindSR = diff(log(xxe_val_m),lag=11)*12/11 / xindvol
  
  xdindvol = diff(log(xxe_val))
  for(i in 1:dim(xdindvol)[2]) xdindvol[,i] = sqrt(SMA(xdindvol[,i]^2,1*21)*12*21)
  xdindSR = diff(log(xxe_val),lag=1*21)*12/1 / xdindvol
  xdindSR_m = mat.to.monthly(xdindSR)
  
  indvol = diff(log(indices))
  for(i in 1:dim(indvol)[2]) indvol[,i] = na20(sqrt(EMA(indvol[,i]^2,1/(3*21))*252))
  #for(i in 1:dim(indvol)[2]) indvol[,i] = na20(emavolSem(indvol[,i],3*21,p=-1))
  indvol_m = mat.to.monthly(indvol)
  indvol_w = mat.to.weekly(indvol)
  inddd = diff(log(indices))
  #for(i in 1:dim(inddd)[2]) inddd[,i] = xts(cummin(drawdown(inddd[,i])),index(inddd[,i]))
  for(i in 1:dim(inddd)[2]) inddd[,i] = xts(-rollmaxna(-drawdown(inddd[,i]),252),index(inddd[,i]))
  #inddd_m = mat.to.monthly(inddd)
  inddd_m = mat.02na(mat.to.monthly(mat.na20(inddd)))
  
  tt=c(1,2,3,4)
  tt[order(-c(11,3,55,44))]
  i6 = carry12(indices,indiceso,indices_spreads/4,-indSR,-indices_rates,0.00175,1,2,start,v=1,e=1);SR(i6,12)
  i6 = carry12(indices,indiceso,indices_spreads/4,-1/indvol,-indices_rates,0.00175,1,2,start,v=1,e=1);SR(i6,12)
  i7 = carry12(xxe_val,xxe_valo,indices_spreads/4,-xdindSR_m,0,0.00175,1,2,start,v=1,e=1);SR(i7,12)
  i2 = momentum12(indices,indiceso,indices_spreads/4,-indices_rates,0.0175,1,2,start,j=11,v=1,e=1);SR(i2,12)
  (diff(log(xxe_val_m)))
  plot(trailSR(diff(log(xxe_val))[,1],get=T))
  
  class(cummin(inddd[,1]))
  apply(cbind(i6d,i7d,i8d), 2, maxdrawdown) 
  
  c2cashind = diff(log(cashind_ccys_m),lag=11)
  ii1d=momentum12d(cashind,cashind_spreads/4,-cashind_rates,0.0175,4,start,j=11,v=vcorr,e=1);SR(ii1d,12)
  ii2d=momentum12d(futcashind_d,cashind_spreads/4,0,0.0175,4,start,j=11,v=vcorr,e=1);SR(ii2d,12)
  ii4d=carry12d(cashind,cashind_spreads/4,rind_m,-cashind_rates,0.0175,2,start,v=vcorr,e=1);SR(ii4d,12)
  ii5d = momentum12d(vve_val,cashind_spreads/4,0,0,2,start,j=1,v=vcorr,e=1);SR(ii5d,12)
  #ii3d=carry12d(cashind,cashind_spreads/4,c2cashind,0,ycost=0.0,2,start,v=vcorr, e=1);SR(ii3d,12)
  
  #mat.SR(cbind(i1+i1b,i2+i2b,i3+i3b,i4+i4b,i5+i5b)['/2014-06'],12)
  #mat.SR(cbind(i1+i1b,i2+i2b,i3+i3b,i4+i4b,i5+i5b)['2014-07/'],12)
  #SR(rowMeans(cbind(i2+i2b,i3,i4b,i5+i5b)['/2014-06'],na.rm=T),12)
  #SR(rowMeans(cbind(i2+i2b,i3,i4b,i5+i5b)['2014-07/'],na.rm=T),12)
  
  vcorr=1
  fxcost=0.01
  s=1:19
  s=c(1,3:8)
  t1=momentum12(futccy_d,ccy_spreads/4,0,0.01,1,2,start,j=3,v=vcorr,e=1);SR(t1['/2014-06'],12)
  t1b=momentum12(futccy_d,ccy_spreads/4,0,0.01,-1,2,start,j=3,v=vcorr,e=1);SR(t1b['/2014-06'],12)
  t2=momentum12(ccys,ccy_spreads/4,carry_m,0.01,1,2,start,j=3,v=vcorr,e=1);SR(t2['/2014-06'],12)
  t2b=momentum12(ccys,ccy_spreads/4,carry_m,0.01,-1,2,start,j=3,v=vcorr,e=1);SR(t2b['/2014-06'],12)
  t4=carry12(ccys,ccy_spreads/4,-carry_m,carry_m,0.01,1,2,start,v=vcorr,e=1);SR(t4['/2014-06'],12)
  t6=momentum12(ccys,ccy_spreads/4,carry_m,0.01,1,2,start,j=3,v=vcorr,e=1,skew=1);SR(t6['/2014-06'],12)
  t6b=momentum12(ccys,ccy_spreads/4,carry_m,0.01,-1,2,start,j=3,v=vcorr,e=1,skew=1);SR(t6b['/2014-06'],12)
  t7=carry12(ccys,ccy_spreads/4,(lag(cindpot)),carry_m,0.01,1,2,start,v=vcorr,e=1);SR(t7['/2014-06'],12)
  t7b=carry12(ccys,ccy_spreads/4,(lag(cindpot)),carry_m,0.01,-1,2,start,v=vcorr,e=1);SR(t7b['/2014-06'],12)
  t8b=carry12(ccys,ccy_spreads/4,lag(rm3),carry_m,0.01,-1,2,start,v=vcorr,e=1);SR(t8b['/2014-06'],12)
  t9=carry12(ccys,ccy_spreads/4,rcind2,carry_m,0.01,1,2,start,v=vcorr,e=1);SR(t9['/2014-06'],12)
  t9b=carry12(ccys,ccy_spreads/4,rcind2,carry_m,0.01,-1,2,start,v=1,e=1);SR(t9b['/2014-06'],12)
  t11 = momentum12(zze_val,ccy_spreads/4,0,0,1,2,start,j=1,v=vcorr,e=1);SR(t11['/2014-06'],12)
  t11b = momentum12(zze_val,ccy_spreads/4,0,0,-1,2,start,j=1,v=vcorr,e=1);SR(t11b['/2014-06'],12)
  t11l = momentum12(zze_val,ccy_spreads/4,0,0,3,2,start,j=1,v=0,e=1);SR(t11l['/2014-06'],12)
  #SR(rowMeans(cbind(t11,t11b,t11l),na.rm=T))
  t3=carry12(ccys,ccy_spreads/4,lag(ycpi),carry_m,0.01,1,2,start,v=vcorr,e=1);SR(t3['/2014-06'],12)
  t5=carry12(ccys,ccy_spreads/4,lag(rccys),carry_m,0.01,1,2,start,v=vcorr,e=1);SR(t5['/2014-06'],12)
  t5b=carry12(ccys,ccy_spreads/4,lag(rccys),carry_m,0.01,-1,2,start,v=vcorr,e=1);SR(t5b['/2014-06'],12)
  t10=carry12(ccysind,ccysind_spreads/4,i2c,ccysind_carry_m,ycost=0.01,1,1,start,v=vcorr,e=1);SR(t10['/2014-06'],12)
  t10b=carry12(ccysind,ccysind_spreads/4,i2c,ccysind_carry_m,ycost=0.01,-1,1,start,v=vcorr,e=1);SR(t10b['/2014-06'],12)
  
  t8=carry12(ccys,ccy_spreads/4,lag(rm3),carry_m,0.01,1,2,start,v=vcorr,e=1);SR(t8,12)
  t4b=carry12(ccys,ccy_spreads/4,-carry_m,carry_m,0.01,-1,2,start,v=vcorr,e=1);SR(t4b,12)
  t3b=carry12(ccys,ccy_spreads/4,lag(ycpi),carry_m,0.01,-1,2,start,v=vcorr,e=1);SR(t3b,12) 
  i3=carry12(indices3,ind3_spreads/4,c2i,0,ycost=0.0,1,2,start,v=vcorr,e=1);SR(i3,12)
  i4b=carry12(indices,indices_spreads/4,rind_m,-indices_rates,0.0175,-1,2,start,v=vcorr,e=1);SR(i4b,12)
  
  SR(log(rowMeans(exp(cbind(t11d,t10d,t2d)['/2014-06']),na.rm=T)))
  SR(log(rowMeans(exp(cbind(t3b,t4b,t7b)['/2014-06']),na.rm=T)))
  SR(log(rowMeans(exp(cbind(binding(cbind(t11d,t10d,t2d)), binding(cbind(t3b,t4b,t7b))))['/2014-06'],na.rm=T)),12)
  SR(log(rowMeans(exp(cbind(t11d,t10d,t2d)['2014-07/']),na.rm=T)))
  SR(log(rowMeans(exp(cbind(binding(cbind(t11d,t10d,t2d)), binding(cbind(t3b,t4b,t7b))))['2014-07/'],na.rm=T)),12)
  
  mat.SR(cbind(t2,t4,t7,t8,t9,t11,t3,t5,t10)['/2014-06'],12)
  mat.SR(cbind(t2b,t4b,t7b,t8b,t9b,t11b,t3b,t5b,t10b)['2014-07/'],12)
  mat.SR(cbind(t2d,t4d,t7d,t8d,t9d,t11d,t3d,t5d,t10d)['/2014-06'],12)
  abs(round(cor(merge(t11/2,t11b/2,t8/2,t4b/2,t7b/2,t7/2,t3b/2,t10/2)['/2014-06'],use="complete.obs"),2)-round(cor(merge(t11/2,t11b/2,t8/2,t4b/2,t7b/2,t7/2,t3b/2,t10/2)['2014-07/'],use="complete.obs"),2))
  
  mar.covar=cov(merge(t11/2,t11b/2,t8/2,t4b/2,t7b/2,t7/2,t3b/2,t10/2)['/2014-06'],use="complete.obs")
  mar.rf=as.numeric(ts$ukousd1w['2014-06-30'])
  mar = markovitz(mar.covar)
  mar = markovitzrf(mar.covar,rf=0.1)
  SR(rowMeans(t(mar*t(cbind(t11/2,t11b/2,t8/2,t4b/2,t7b/2,t7/2,t3b/2,t10/2)['/2014-06'])),na.rm=T),12)
  SR(rowMeans(t(mar*t(cbind(t11/2,t11b/2,t8/2,t4b/2,t7b/2,t7/2,t3b/2,t10/2)['2014-07/'])),na.rm=T),12)
  
  #old
  #SR(rowMeans(cbind(1.5*t2,t4b/2,t7b/2,t9/2,t8/2,t8b/2,t11/2,t11b/2)['2000-04/']),12)
  #SR(rowMeans(cbind(t2,t9/2,t4b/2,t7b/2,t8/2,t8b/2,t11/2,t11b/2)['2000-04/']),12)
  
  #new, with in-outof-sample
  SR(rowMeans(cbind(t2/2,t2b/2,t4/2,t4b/2,t7/2,t7b/2,t8/2,t8b/2,t9/2,t9b/2,t11/2,t11b/2,t3/2,t3b/2,t5/2,t5b/2,t10/2,t10b/2)['/2014-06'],na.rm=T),12)
  SR(rowMeans(cbind(t2/2,t2b/2,t4/2,t4b/2,t7/2,t7b/2,t8/2,t8b/2,t9/2,t9b/2,t11/2,t11b/2,t3/2,t3b/2,t5/2,t5b/2,t10/2,t10b/2)['2014-07/'],na.rm=T),12)
  
  #t2b/2,t4b/2,t7b/2,t8/2*3,t9b/2,t11/2*3,t3b/2,t5b/2,t10/2,t10b/2
  #t11/2,t11b/2,t8/2,t4b/2,t7b/2,t7/2,t3b/2,t10/2
  SR(rowMeans(cbind(t11*1.5,t11b,t10,t10b/2,t8b/2,t4b/2)['/2014-06'],na.rm=T),12)
  SR(rowMeans(cbind(t11*1.5,t11b,t10,t10b/2,t8b/2,t4b/2)['2014-07/'],na.rm=T),12)
  SR(rowMeans(cbind(t11,t11b,t10/2,t10b/2,t8*2,t3b,t4b)['/2014-06'],na.rm=T),12)
  SR(rowMeans(cbind(t11,t11b,t10/2,t10b/2,t8*2,t3b,t4b)['2014-07/'],na.rm=T),12)
  SR(rowMeans(cbind(t11,t11b,t10/2,t10b/2,t8,t3b)['/2014-06'],na.rm=T),12)
  SR(rowMeans(cbind(t11,t11b,t10/2,t10b/2,t8,t3b)['2014-07/'],na.rm=T),12)
  round(cor(merge(t11+t11b,t2b+t2,t4b,t4,t7b,t7,t8b,t8,t9b,t9,t3,t3b,t5,t5b,t10,t10b)['/2014-06'],use="complete.obs"),2)
  
  #with metals and indices
  SR(rowMeans(cbind(t11,t11b,t10/2,t10b/2,t8*2,t3b,t4b,f1,f1b,f3,f3b,f4,f4b,i2+i2b,i3,i4b,i5+i5b)['/2014-06'],na.rm=T),12)
  SR(rowMeans(cbind(t11,t11b,t10/2,t10b/2,t8*2,t3b,t4b,f1,f1b,f3,f3b,f4,f4b,i2+i2b,i3,i4b,i5+i5b)['2014-07/'],na.rm=T),12)
  mat.SR(cbind(t11,t11b,t10/2,t10b/2,t8*2,t3b,t4b,f1,f1b,f3,f3b,f4,f4b,i2+i2b,i3,i4b,i5+i5b)['/2014-06'],12)
  mat.SR(cbind(t2d,t11d,f1d,i2d)['/2014-06'])
  
  SR(rowMeans(cbind(binding(cbind(t1d,t10d,t4d,t3d,t8)), binding(cbind(i2d,i5d,i3,i4d)), binding(cbind(f1d,f3d,f5d)))['/2014-06'],na.rm=T),12)
  SR(rowMeans(cbind(binding(cbind(t1d,t10d,t4d,t3d,t8)), binding(cbind(i2d,i5d,i3,i4d)), binding(cbind(f1d,f3d,f5d)))['2014-07/'],na.rm=T),12)
  SR(rowMeans(cbind(t10d,i2d,i5d,f1d,f5d)['/2014-06'],na.rm=T),12)
  SR(rowMeans(cbind(t4d,t3d,t8,i3,i4d,f3d)['/2014-06'],na.rm=T),12)
  SR(rowMeans(cbind(binding(cbind(t1d,t10d,i2d,i5d,f1d,f5d)), binding(cbind(t3d,t8,i3,i4d,t4d,f3d)) )['/2014-06'],na.rm=T),12)
  SR(rowMeans(cbind(binding(cbind(t1d,t10d,i2d,i5d,f1d,f5d)), binding(cbind(t3d,t8,i3,i4d,t4d,f3d)) )['2014-07/'],na.rm=T),12)
  SR(rowMeans(cbind(binding(cbind(t1d,t10d,i2d,i5d,f1d,f5d)), binding(cbind(i2,i3,i5)), binding(cbind(t3d,t8,i4d,t4d,f3d)) )['/2014-06'],na.rm=T),12)
  SR(rowMeans(cbind(binding(cbind(t1d,t10d,t4d,t3d,t8)), binding(cbind(i3,i2d,i5d,i4d)), binding(cbind(f1d,f3d,f5d)))['/2014-06'],na.rm=T),12)
  
  mat.SR(merge(i1,i2,i3,i4,i5,i1b,i2b,i3b,i4b,i5b)['/2014-06'])
  mat.SR(merge(i1d,i2d,i3d,i4d,i5d)['/2014-06'])
  tt=merge(t1d,t2d,t3d,t4d,t5d,t7d,t8d,t9d,t10d,t11d,f1d,f2d,f3d,f4d,f5d,i1d,i2d,i3d,i4d,i5d)['/2014-06']
  tt=merge(t2,t2b,t4,t4b,t7,t7b,t8,t8b,t9,t9b,t11,t11b,t3,t3b,t5,t5b,t10,t10b)['/2014-06']
  tt=merge(t2,t4,t7,t8,t9,t11,t3,t5,t10)['/2014-06']
  sub=which(mat.SR(tt)[,1]>0.45)
  
  xxe_val = cumulate(xxe)
  tt = mat.to.monthly(xxe_val)
  tt = tsmomenh(diff(log(indices)),0.17)
  tt = diff(log(indices))
  tt = cumulate(tt)
  tt = mat.to.monthly(tt)
  tt = xts(rowMeans(diff(log(tt))-indices_rates/12-log(1.0175)/12,na.rm=T),index(tt))
  SR(tt['2014-07/'],12)
  plot(indices[,1])
  
  SR(rowMeans(cbind(i4d,i5d,tt)['/2014-06'],na.rm=T),12)
  SR(rowMeans(cbind(i4d,i5d)['2014-07/'],na.rm=T),12)
  SR(rowMeans(cbind(t7d*2,t11b,t10,binding(cbind(t2b,t3b,t5b)))['/2014-06'],na.rm=T),12)
  SR(rowMeans(cbind(t7d*2,t11b,t10,binding(cbind(t2b,t3b,t5b)))['2014-07/'],na.rm=T),12)
  SR(rowMeans(cbind(binding(cbind(t2b,t7b)), binding(cbind(t11b)), binding(cbind(t8b,t3b,t5b)))['/2014-06'],na.rm=T),12)
  SR(rowMeans(cbind(binding(cbind(t2b,t7b)), binding(cbind(t11b)), binding(cbind(t8b,t3b,t5b)))['2014-07/'],na.rm=T),12)
  
  strategies=cor(tt[,sub],use="complete.obs")
  hc = hclust(dist(strategies))
  plot(hc)
  SR(rowMeans(cbind(t4d)['2018-06/'],na.rm=T))
  SR(rowMeans(cbind(binding(cbind(i4d,t5d,t4d,t3d,t8d)), binding(cbind(t9d,f4d,i3d,f3d,f2d,f1d,f5d)), binding(cbind(t11d,t1d,t2d,i5d,i1d,i2d,t7d,t10d)))['/2014-06'],na.rm=T),12)
  SR(rowMeans(cbind(binding(cbind(i4d,t5d,t4d,t3d,t8d)), binding(cbind(t9d,f4d,i3d,f3d,f2d,f1d,f5d)), binding(cbind(t11d,t1d,t2d,i5d,i1d,i2d,t7d,t10d)))['2014-07/'],na.rm=T),12)
  SR(rowMeans(cbind(binding(cbind(t5d,t4d,t8d,i4d,f3d,f5d)), binding(cbind(t9d,f1d,i5d,i1d,i2d)), binding(cbind(t11d,t1d,t7d,t10d)))['/2014-06'],na.rm=T),12)
  SR(rowMeans(cbind(binding(cbind(t5d,t4d,t8d,i4d,f3d,f5d)), binding(cbind(t9d,f1d,i5d,i1d,i2d)), binding(cbind(t11d,t1d,t7d,t10d)))['2014-07/'],na.rm=T),12)
  SR(rowMeans(cbind(ii2d,ii4d,ii5d)['/2014-06'],na.rm=T))
  SR(f3d['2014-07/'])
  
  test= momentum12d(xxe_val,indices_spreads/4,0,0,2,start,j=1,v=1,e=2)
  colnames(test) = indices_names
  colSums(abs(test))
  
  s=c(5:22,24)
  s=c(1:24,25,26,27)
  s=c(1:21,23,24)
  eqcost=0.0175
  eqcost=0.0
  s=c(2,9:19)
  s=1:19
  s=1:13
  s=1:24
  fxcost=0.01
  xx=tsavgall(indices,232,indices_spreads,-indices_rates_d/10,eqcost,e=1)
  xxe_val=cumulate(xx)
  dirs=tsavgall(indices,232,indices_spreads,-indices_rates_d/10,eqcost,e=2)
  on.returns = indiceso/lag(indices)-1
  xxe_valo = lag(xxe_val)*(1+dirs*(indiceso/lag(indices)-1))
  plot.cum(f3d)
  plot(xts(rowMax(-roll_rates_ms[,s]),index(-roll_rates_ms[,s])),t="l")
  xxe_val=fillerxts(xxe_val)
  summary(xts(rowMax(-roll_rates_ms[-(1:2),s]),index(-roll_rates_ms[-(1:2),s])))
  plot(-rowMax(roll_rates_ms[-(1:2),s]),t="l")
  
  
  i5d = momentum12d(xxe_val[,s],xxe_valo[,s],indices_spreads[s]/4,0,0,2,start,j=1,v=1,e=1,p=2);SR(i5d,12)
  #i6d = carry12d(indices,indiceso,indices_spreads/4,-indSR,-indices_rates/10,eqcost,2,start,v=1,e=1);SR(i6d,12)
  i8d = carry12d(indices[,s],indiceso[,s],indices_spreads[s]/4,-indvol_m[,s],-indices_rates[,s]/10,eqcost,2,start+60,v=1,e=1,p=2);SR(i8d,12)
  i10d = carry12d(indices[,s],indiceso[,s],indices_spreads[s]/4,indbreak_m[,s],-indices_rates[,s]/10,eqcost,6,start,v=1,e=1,p=2);SR(i10d,12)
  f1d=momentum12d(fut_d[,s],(fut_do[,s]),fut_spreads[s]/4,roll_rates_m[,s],futcost[s],2,futstart,j=11,v=1,e=1,use=1,p=1);SR(f1d['2007-01/'],12)
  f3d=carry12d(fut_d[,s],(fut_do[,s]),fut_spreads[s]/4,-roll_rates_ms[,s],roll_rates_m[,s],futcost[s],1,futstart,v=vcorr,e=1);SR(f3d['2007-01/'],12)
  f4d=carry12d(fut_d[,s],(fut_do[,s]),fut_spreads/4,-futvalue,roll_rates_m[,s],futcost,1,futstart,v=vcorr,e=1);SR(f4d['2007-01/'],12)
  f5d = momentum12d(yy_val[,s],lag(yy_val[,s]),fut_spreads[s]/4,0,0,3,2,j=6,v=vcorr,e=1);SR(f5d['2007-01/'],12)
  
  t2d=momentum12d(ccys[,s],lag(ccys[,s]),ccy_spreads[s]/4,carry_m[,s],fxcost,2,start,j=3,v=1,e=1,p=2);SR(t2d,12)
  t4d=carry12d(ccys[,s],lag(ccys[,s]),ccy_spreads[s]/4,-carry_m[,s],carry_m[,s],fxcost,2,start,v=vcorr,e=1,p=2);SR(levret(1,t4d),12)
  t11d = momentum12d(zze_val[,s],lag(zze_val[,s]),ccy_spreads[s]/4,0,0,2,start,j=2,v=1,e=1,p=2);SR(t11d,12)
  s=c(1,3:8);t10d=carry12d(ccysind[,s],lag(ccysind[,s]),ccysind_spreads[s]/4,i2c[,s],ccysind_carry_m[,s],ycost=fxcost,1,start,v=1,e=1,p=2);SR(t10d,12)  
  t7d=carry12d(ccys[,s],lag(ccys[,s]),ccy_spreads[s]/4,(lag(cindpot[,s])),carry_m[,s],fxcost,2,start,v=vcorr,e=1);SR(t7d,12)
  SR(binding(cbind(t2d,t11d,t4d,t10d)['1998-01/'])['/2014-07'],12)
  SR(binding(cbind(i5d,i10d,i8d)['1998-01/'])['/2014-07'],12)
  SR(binding(cbind(f1d,f3d,f5d,f4d)['1998-01/'])['/2014-07'],12)
  SR(binding(cbind(f1d,f3d,f5d,f4d)['1998-01/'])['2007-01/'],12)
  #wniosek ccy3 warto na g7, a carry na non-G7, a na ALL dac mom i cycle, 
  
  ss = c(1:9,11,14,15,16) #CME ccy futures
  tt=cbind(ccys[,s],fut_d[,1:5])
  tt=fillerxts(tt)
  t2d=momentum12d(tt,fillerxts(cbind(lag(ccys[,s]),fut_do[,1:5])),c(ccy_spreads[s],fut_spreads[1:5])/4,cbind(carry_m[,s],roll_rates_m[,1:5]),c(rep(fxcost,19),futcost[1:5]),2,start+130,j=3,v=1,e=1);SR(t2d,12)
  s=1:19
  f1d=momentum12d(fut_d[,s],(fut_do[,s]),fut_spreads[s]/4,roll_rates_m[,s],futcost[s],2,futstart,j=11,v=1,e=1);SR(f1d,12)
  tt=momentum12d(fut_d[,s],(fut_do[,s]),fut_spreads[s]/4,roll_rates_m[,s],futcost[s],2,futstart,j=11,v=1,e=2)
  tt=tt*(log(fut_d/fut_do) + lag(roll_rates_m)/12)
  plot(cumsum(na20(tt$CME_NG+tt$CME_CL+tt$CME_NG/2+tt$CME_CL/2)))
  SR(tt$CME_NG+tt$CME_CL+tt2$CME_NG/2+tt2$CME_CL/2,12)
  tt2=carry12d(fut_d[,s],(fut_do[,s]),fut_spreads[s]/4,-roll_rates_ms[,s],roll_rates_m[,s],futcost[s],1,futstart,v=vcorr,e=2)
  tt2=tt2*(log(fut_d/fut_do) + lag(roll_rates_m)/12)
  colnames(tt2)=fut_names
  
  i5dw = momentum12d(xxe_val[,s],lag(xxe_val[,s]),indices_spreads[s]/4,0,0,2,start,j=1*5,v=1,e=1,f=52);SR(i5dw,52)
  t11dw = momentum12(zze_val[,s],lag(zze_val[,s]),ccy_spreads[s]/4,0,0,-1,2,start,j=1*3,v=1,e=1,f=52);SR(t11dw,52)
  f11dw = momentum12d(yye_val[,s],lag(yye_val[,s]),fut_spreads[s]/4,0,0,2,start,j=1*8,v=1,e=1,f=52);SR(f11dw,52)
  i2w = momentum12(indices[,s],indiceso[,s],indices_spreads[s]/4,0,0,1,2,start,j=13*4,v=1,e=1,f=52);SR(i2w["1998-01-01/"],52)
  plot.cum(t11dw["1998-01-01/"])
  SR(binding(cbind(t11dw,i2w))['2014-06/'],52)
  plot.cum(binding(cbind(t11dw)['2012-06/']))
  t2dw=momentum12d(ccys[,s],lag(ccys[,s]),ccy_spreads[s]/4,carry_w[,s],fxcost,2,start,j=3*4,v=1,e=1,f=52);SR(t2dw,52)
  t10dw=carry12d(ccysind[,s],lag(ccysind[,s]),ccysind_spreads[s]/4,i2c_w[,s],ccysind_carry_m_w[,s],ycost=fxcost,1,start*4,v=1,e=1,f=52);SR(t10dw,52)  
  f5dw = momentum12d(yye_val[,s],lag(yye_val[,s]),fut_spreads[s]/4,0,0,3,2,j=10,v=vcorr,e=1,f=52);SR(f5dw,52)
  GDPC1/GDPPOT
  SR(binding(cbind(t10d,t11d,t2d,t4d,f1d*2,f3d/2,f5d/2,i10d,i5d,i8d)['2006-09/'])['/2014-06'],12)
  SR(binding(cbind( binding(cbind(t11d,t2d,f1d*2,f5d/2,i5d)), binding(cbind(t4d,f3d/2)), binding(cbind(t10d,i10d,i8d)) )['2006-09/'])['2014-06/'],12)
  
  
  mat.SR(cbind(t11/2,t11b/2,t8/2,t4b/2,t7b/2,t7/2,t3b/2,t10/2)['/2014-06'],12)
  SR(rowMeans(cbind(t11/2,t11b/2)['/2014-06'],na.rm=T),12)
  SR(t4b['2014-06/'],12)
  SR(i5dw['2014-06/'],12)
  
  m2=momentum12(ccys,ccy_spreads/4,carry_m,0.01,1,2,start,j=3,e=2)
  m4b=carry12(ccys_m,ccy_spreads/4,-carry_m,carry_m,0.01,-1,1,start,e=2)
  m7b=carry12(ccys_m['/2017-09'],ccy_spreads/4,(lag(cindpot2)),carry_m,0.01,-1,3,start,e=2)
  m9=carry12(ccys_m['/2017-08'],ccy_spreads/4,rcind,carry_m,0.01,1,2,start,e=2)
  m8=carry12(ccys_m['/2017-05'],ccy_spreads/4,-lag(rm3),carry,0.01,1,2,start,e=2)
  m8b=carry12(ccys_m['/2017-05'],ccy_spreads/4,-lag(rm3),carry,0.01,-1,2,start,e=2)
  m11=momentum12(xxe_val,0,0,0,1,2,start,j=1,e=2)
  m11b=momentum12(xxe_val,0,0,0,-1,2,start,j=1,e=2)
  
  mm = 2*(1.5*m2+m4b/2+m7b/2+m9/2+m8/2+m8b/2+m11/2+m11b/2)
  colnames(mm) = colnames(ccys)
  colMeans(mm)
  ml=mm;ms=mm
  for(i in 1:19) ml[,i] = pmax(0,ml[,i])
  colMeans(ml)
  for(i in 1:19) ms[,i] = pmin(0,ms[,i])
  colMeans(ms)
  
  ml = 2*(1.5*m2+m8/2+m11/2+m9/2)
  ms = 2*(m4b/2+m7b/2+m8b/2+m11b/2)
  colnames(ml) = colnames(ccys)
  colMeans(ml)
  colnames(ms) = colnames(ccys)
  colMeans(ms)
  
  t9c=carry12(ccys_m['/2017-08'],ccy_spreads/4,rcind2,carry_m,0.01,-1,2,start,e=1);SR(t9,12)
  t9d=carry12(ccys_m['/2017-08'],ccy_spreads/4,ycip,carry_m,0.01,-1,2,start,e=1);SR(t9,12)
  t9e=carry12(ccys_m['/2017-08'],ccy_spreads/4,ycpi,carry_m,0.01,-1,2,start,e=1);SR(t9,12)
  
  ccys[substr(as.Date(index(ccys_m)[10]),0,7)][,2]
  substr(as.Date(index(ccys_m)),0,7)
  range_limit = paste0('/',)
  prices=prices[paste0('/',range_limit)]
  
  f1d=momentum12d(futenh_d,lag(futenh_d),fut_spreads/4,0,futcost,2,fstart,j=11,v=vcorr,e=1);SR(f1d,12)
  f2d=momentum12d(fut_d,fut_do,fut_spreads/4,fut_carry_m,futcost,2,fstart,j=11,v=vcorr,e=1);SR(f2d,12)
  f3d=carry12d(fut_d,fut_do,fut_spreads/4,-fut_carry_m,fut_carry_m,futcost,1,fstart,v=vcorr,e=1);SR(f3d,12)
  f4d=carry12d(fut_d,fut_do,fut_spreads/4,futvalue,fut_carry_m,futcost,1,fstart,v=vcorr,e=1);SR(f4d,12)
  f5d = momentum12d(yye_val,lag(yye_val),fut_spreads/4,0,0,1,fstart,j=1,v=vcorr,e=1);SR(f5d,12)
  
  i1d=momentum12d(indices[,1:s],indiceso[,1:s],indices_spreads[1:s]/4,-indices_rates[,1:s],0.0175,20,start,j=11,v=vcorr,e=1);SR(i1d,12)
  i2d=momentum12d(futind_d,lag(futind_d),indices_spreads/4,0,0.0175,20,start,j=11,v=vcorr,e=1);SR(i2d,12)
  i3d=carry12d(indices3,ind3_spreads/4,c2i,0,ycost=0.0,10,start,v=vcorr,e=1);SR(i3d,12)
  i4d=carry12d(indices[,1:s],indiceso[,1:s],indices_spreads[1:s]/4,rind_m[,1:s],-indices_rates[,1:s],0.0175,10,start,v=vcorr,e=1);SR(i4d,12)
  i5d = momentum12d(xxe_val,xxe_valo,indices_spreads/4,0,0,10,start,j=1,v=vcorr,e=1);SR(i5d,12)
  
  vcorr=1
  t1d=momentum12d(futccy_d,lag(futccy_d),ccy_spreads/4,0,0.01,2,start,j=3,v=vcorr,e=1);SR(t1d,12)
  t2d=momentum12d(ccys,lag(ccys),ccy_spreads/4,carry_m,0.01,2,start,j=3,v=vcorr,e=1);SR(t2d,12)
  t4d=carry12d(ccys,lag(ccys),ccy_spreads/4,-carry_m,carry_m,0.01,2,start,v=vcorr,e=1);SR(t4d,12)
  t7d=carry12d(ccys,lag(ccys),ccy_spreads/4,(lag(cindpot)),carry_m,0.01,2,start,v=vcorr,e=1);SR(t7d,12)
  t8d=carry12d(ccys,lag(ccys),ccy_spreads/4,lag(rm3),carry_m,0.01,2,start,v=vcorr,e=1);SR(t8d,12)
  t9d=carry12d(ccys,lag(ccys),ccy_spreads/4,rcind2,carry_m,0.01,2,start,v=0,e=1);SR(t9d,12)
  t3d=carry12d(ccys,lag(ccys),ccy_spreads/4,lag(ycpi),carry_m,0.01,2,start,v=vcorr,e=1);SR(t3d,12) 
  t5d=carry12d(ccys,lag(ccys),ccy_spreads/4,lag(rccys),carry_m,0.01,2,start,v=vcorr,e=1);SR(t5d,12)
  t10d=carry12d(ccysind,lag(ccysind),ccysind_spreads/4,i2c,ccysind_carry_m,ycost=0.01,1,start,v=vcorr,e=1);SR(t10d,12)  
  t11d = momentum12d(zze_val,lag(zze_val),ccy_spreads/4,0,0,2,start,j=1,v=vcorr,e=1);SR(t11d,12)
  SR(binding(cbind(t10d,t11d,t2d,t4d,t7d)['1998-01/'])['2014-07/'],12)
  
  fstart=50
  f1l=momentum12(futenh_d,lag(futenh_d),fut_spreads/4,0,futcost,3,1,fstart,j=11,v=0,e=1,pos=0);SR(f1l)
  f2l=momentum12(fut_d,lag(fut_d),fut_spreads/4,fut_carry_m,futcost,3,1,fstart,j=11,v=0,e=1);SR(f2l)
  f3l=carry12(fut_d,lag(fut_d),fut_spreads/4,-fut_carry_m,fut_carry_m,futcost,3,1,fstart,v=0,e=1);SR(f3l)
  f4l=carry12(fut_d,lag(fut_d),fut_spreads/4,futvalue,fut_carry_m,futcost,3,1,fstart,v=0,e=1);SR(f4l)
  f5l= momentum12(yye_val,lag(yye_val),fut_spreads/4,0,0,3,1,fstart,j=1,v=0,e=1);SR(f5l,12)
  
  i1l=momentum12(indices,indiceso,indices_spreads/4,-indices_rates,0.0175,3,1,start,j=11,v=0,e=1);SR(i1l,12)
  i2l=momentum12(futind_d,lag(futind_d),indices_spreads/4,0,0.0175,3,1,start,j=11,v=0,e=1);SR(i2l,12)
  i3l=carry12(indices3,ind3_spreads/4,c2i,0,ycost=0.0,3,1,start,v=0,e=1);SR(i3l,12)
  i4l=carry12(indices,indiceso,indices_spreads/4,rind_m,-indices_rates,0.0175,3,1,start,v=0,e=1);SR(i4l,12)
  i5d = momentum12(xxe_val,xxe_valo,indices_spreads/4,0,0,3,1,start,j=1,v=0,e=1);SR(i5l,12)
  
  t1l=momentum12(futccy_d,ccy_spreads/4,0,0.01,3,1,start,j=3,v=0,e=1);SR(t1l,12)
  t2l=momentum12(ccys,ccy_spreads/4,carry_m,0.01,3,1,start,j=3,v=0,e=1);SR(t2l,12)
  t4l=carry12(ccys,ccy_spreads/4,-carry_m,carry_m,0.01,3,1,start,v=0,e=1);SR(t4l,12)
  t7l=carry12(ccys,ccy_spreads/4,(lag(cindpot)),carry_m,0.01,3,1,start,v=0,e=1);SR(t7l,12)
  t8l=carry12(ccys,ccy_spreads/4,lag(rm3),carry_m,0.01,3,1,start,v=0,e=1);SR(t8l,12)
  t9l=carry12(ccys,ccy_spreads/4,rcind2,carry_m,0.01,3,1,start,v=0,e=1);SR(t9l,12)
  t11d = momentum12(zze_val,ccy_spreads/4,0,0,3,1,start,j=1,v=0,e=1);SR(t11l,12)
  t3l=carry12(ccys,ccy_spreads/4,lag(ycpi),carry_m,0.01,3,1,start,v=0,e=1);SR(t3l,12) 
  t5l=carry12(ccys,ccy_spreads/4,lag(rccys),carry_m,0.01,3,1,start,v=0,e=1);SR(t5l,12)
  t10l=carry12(ccysind,ccysind_spreads/4,i2c,ccysind_carry_m,ycost=0.01,3,1,start,v=0,e=1);SR(t10l,12)  
  
  #volbreaks tests  
  indicesH = load_indices_loc((indices_names),col=2)
  indicesL = load_indices_loc((indices_names),col=3)
  indicesH=fillerxts(indicesH)
  indicesH=fillerxtsNON0(indicesH)
  indicesL=fillerxts(indicesL)
  indicesL=fillerxtsNON0(indicesL)
  #test = tsvolbreak(indices[,1],indicesH[,1],indicesL[,1],5,indices_spreads[1],indices_rates_d[,1])
  #test = xts(exp(cumsum(na20(test))),index(indices[,1]))
  #plot(test)
  cvolbr = crossvolbreak(indices,indicesH,indicesL,5)
  indbreak_m = mat.to.monthly(cvolbr)
  indbreak_w = mat.to.weekly(cvolbr)
  i10d=carry12d(indices,indiceso,indices_spreads/4,indbreak_m,-indices_rates,0.00175,5,start,v=1,e=1);SR(i10d['/2014-06'],12)
  i10d=carry12d(indices['1997-07/'],indices_spreads/4,indbreak_w['1997-07/'],-indices_rates_w['1997-07/'],0.00175,4,3,v=1,e=1,f=52);SR(i10d['/2014-06'],52)
  i10d=carry12d(indices['1997-07/'][,],indices_spreads[]/4,test['1997-07/'][,],-indices_rates_d['1997-07/'][,],0.00175,2,3,v=1,e=1,f=252);SR(i10d['/2014-06'],252)
  i10d=carry12d(indices['1997-07/'][,c(1:7,17,23:24)],indices_spreads[c(1:7,17,23:24)]/4,test['1997-07/'][,c(1:7,17,23:24)],-indices_rates_d['1997-07/'][,c(1:7,17,23:24)],0.0175,2,3,v=1,e=1,f=252);SR(i10d['/2014-06'],252)
  i10a=carry12(indices['1997-07/'][,c(1:7,17,23:24)],indices_spreads[c(1:7,17,23:24)]/4,test['1997-07/'][,c(1:7,17,23:24)],-indices_rates_d['1997-07/'][,c(1:7,17,23:24)],0.0175,1,2,3,v=1,e=1,f=252);SR(i10a['/2014-06'],252)
  i10b=carry12(indices['1997-07/'][,c(1:7,17,23:24)],indices_spreads[c(1:7,17,23:24)]/4,test['1997-07/'][,c(1:7,17,23:24)],-indices_rates_d['1997-07/'][,c(1:7,17,23:24)],0.0175,-1,2,3,v=1,e=1,f=252);SR(i10b['/2014-06'],252)
  SR(i9d['2014-07/'],252)
  plot(exp(cumsum(na20(i9b)))) 
  plot(exp(cumsum(na20(i9b)))) # traci na malym vol
  plot(vix['1997-07/'])
  plot(exp(cumsum(na20(i9d['2014-07/']))))
  plot(exp(cumsum(na20(i9a['2014-07/']))))
  plot(exp(cumsum(na20(i9b['2014-07/']))))
  plot(fin_d['1997-07/'])
  indices_w[start*4,1]
  tt=carry12d(indices['1997-07/'],indices_spreads/4,test['1997-07/'],-indices_rates_d['1997-07/'],0.00175,2,3,v=1,e=2,f=252)
  colnames(tt)=colnames(indices)
  so=colMeans(abs(tt))
  so[order(so)]
  
  ccysH = load_indices_loc(tolower(ccy_names),col=2)
  ccysL = load_indices_loc(tolower(ccy_names),col=3)
  ccysH=fillerxts(ccysH)
  ccysL=fillerxts(ccysL)
  ccybreak = crossvolbreak(ccys,ccysH,ccysL,5)
  ccybreak_m = mat.to.monthly(ccybreak)
  t12d=carry12d(ccys['1997-07/'],ccy_spreads/4,-ccybreak_m['1997-07/'],carry_m['1997-07/'],0.01,4,start,v=1,e=1);SR(t12d,12)
  ccybreak_w = mat.to.weekly(ccybreak)
  t12d=carry12d(ccys['1997-07/'],ccy_spreads/4,-ccybreak_w['1997-07/'],carry_w['1997-07/'],0.01,4,start,v=1,e=1,f=52);SR(t12d,52)
  t12d=carry12d(ccys['1997-07/'],ccy_spreads/4,-ccybreak['1997-07/'],carry_d['1997-07/'],0.01,4,3,v=1,e=1,f=252);SR(t12d,252)
  
  fut_dH = load_futures(fut_names,col=2)
  fut_dL = load_futures(fut_names,col=3)
  fut_dH=fillerxts(fut_dH)
  fut_dL=fillerxts(fut_dL)
  fut_dH=fut_dH['1989-03-01/']
  fut_dL=fut_dL['1989-03-01/']
  tt = crossvolbreak(fut_d,fut_dH,fut_dL,5)
  futbreak_m = mat.to.monthly(tt)
  f6d=carry12d(fut_d,fut_spreads/4,-futbreak_m,roll_rates_m,futcost,2,start,v=1,e=1);SR(f9d,12)
  
}#final

stops<-function(){
  #take profit and stop loss tests
  t7=carry12(ccys,lag(ccys),ccy_spreads/4,(lag(cindpot)),carry_m,0.01,-1,2,start,v=vcorr,e=4,highs=ccysh,lows=ccysl,limit=0.025,tp=1);SR(t7,12)
  t1=momentum12(ccys,lag(ccys),ccy_spreads/4,carry_m,0.01,1,2,start,j=1,v=vcorr,e=5,highs=ccysh,lows=ccysl,limit=0.09,tp=-1);SR(t1[[1]],12)
  #indicesh=indicesH
  #indicesl=indicesL
  indiceso <- load_indices_loc((indices_names),1)
  indicesh <- load_indices_loc((indices_names),2)
  indicesl <- load_indices_loc((indices_names),3)
  indices <- load_indices_loc((indices_names))
  indiceso = mat.02na(indiceso)
  indicesh = mat.02na(indicesh)
  indicesl = mat.02na(indicesl)
  indices = mat.02na(indices)
  indices <- fillerxts(indices)
  indicesh <- fillerxts(indicesh,indices)
  indicesl <- fillerxts(indicesl,indices)
  indiceso <- fillerxts(indiceso,indices)
  i1=momentum12(indices,indiceso,indices_spreads/2,-indices_rates,0.0175,1,4,start,j=11,v=vcorr,e=5,highs=indicesh,lows=indicesl,limit=0.025,tp=1,w=1,p=2);
  SR(i1[[1]],12)
  colSums(is.na(i1[[2]]))-colSums(is.na(i1[[1]]))
  tail(i1[[2]])
  SR(i1['2008-01-01/'],12)
  plot.cum(i1b['2008-01-01/'])
  rr = c(1:13,15:21,23:27)
  rr = c(1:13,15:27)
  rr = 1:27
  
  i1b=momentum12(indices[,rr],indiceso[,rr],indices_spreads[rr]/2,-indices_rates[,rr],0.0175,-1,4,start,j=11,v=vcorr,e=5,highs=indicesh[,rr],lows=indicesl[,rr],limit=0.025,tp=1,w=2,p=2);
  SR(i1b[[1]],12)
  SR(i1b[[2]],12)
  SR(i1b[[5]],12)
  tail(i1b[[1]])
  tail(i1b[[4]])
  tail(i1b[[2]])
  colSums(is.na(i1b[[2]]))-colSums(is.na(i1b[[1]]/i1b[[2]]))
  colSums(is.na(i1b[[2]]))-colSums(is.na(i1b[[1]]))
  SR(i1b[[2]],12)
  which(is.na(i1b[[1]]$'X.psi20') & !is.na(i1b[[2]]$'X.psi20'))
  cc[564,]/oo[564,]-1
  time_carry(indicesh[idx[i]][,j],indiceso[idx[i]][,j],limit+cost[j],1)
  tt=momentum12test(indices,indiceso,indices_spreads/2,-indices_rates,0.0175,1,4,start,j=11,v=vcorr,e=5,highs=indicesh,lows=indicesl,limit=0.025,tp=1,w=1);
  tail(tt)
  
  oo = mat.to.monthly(indiceso,1)
  hh = mat.to.monthly(indicesh,2)
  ll = mat.to.monthly(indicesl,3)
  cc = mat.to.monthly(indices,4)
  down = (log(oo/ll))
  up = (log(hh/oo))
  tail(down)
  indices_rates$'^ipc'["2017-11"]
  
  test=binding(cbind(i1b,i1))
  SR(i1,12)
  trailSR(test,b=12,w=30)
  plot(cumsum(na20(i1b["1997-03-01/"])))
  
  #jakie skladniki indices przewaznie?
  i1=momentum12(indices[,cc],indiceso[,cc],indices_spreads[cc]/4,-indices_rates[,cc],0.0175,1,4,start,j=11,v=vcorr,e=4,highs=indicesh[,cc],lows=indicesl[,cc],limit=0.025,tp=1)
  test=colSums(i1,na.rm=T);test[order(test)]
  indices_names
  cc=c(-11,-12)
  
  #stop loss na short indices 
}#stoplosses

rer<-function(){
  vcorr=1
  t1d=momentum12d(futccy_d,lag(futccy_d),ccy_spreads/4,0,0.005,2,start,j=3,v=vcorr,e=1);SR(t1d,12)
  t2d=momentum12d(ccys,lag(ccys),ccy_spreads/4,carry_m,0.005,2,start,j=3,v=vcorr,e=1,p=2,w=2,use=0,tr=0.015);SR(t2d,12)
  plot.cum(t8d)
  #meanrev
  t2d=carry12d(ccys,lag(ccys),ccy_spreads/4,log(ccys_m/mat.SMA(ccys_m,9)),carry_m,0.005,2,start,v=vcorr,e=1,p=2,w=2,tr=0.015);SR(t2d,12)
  t8d=carry12d(ccys,lag(ccys),ccy_spreads/4,lag(rm3),carry_m,0.01,2,start,v=vcorr,e=1,tr=-0.003);SR(t8d,12)
  plot(xts((rowMax(rm3)+rowMax(-rm3))/rowMeans(rm3),index(rm3)),t="l")
  summary(rowMeans(rm3))
  
  t4d=carry12d(ccys,lag(ccys),ccy_spreads/4,-carry_m,carry_m,0.01,2,start,v=vcorr,e=1,p=2,w=2);SR(t4d,12)
  t7d=carry12d(ccys,lag(ccys),ccy_spreads/4,(lag(cindpot)),carry_m,0.01,2,start,v=vcorr,e=1);SR(t7d,12)
  t8d=carry12d(ccys,lag(ccys),ccy_spreads/4,lag(rm3),carry_m,0.01,2,start,v=vcorr,e=1);SR(t8d,12)
  t9d=carry12d(ccys,lag(ccys),ccy_spreads/4,rcind2,carry_m,0.01,2,start,v=0,e=1);SR(t9d,12)
  
  t3d=carry12d(ccys,lag(ccys),ccy_spreads/4,lag(ycpi),carry_m,0.01,2,start,v=vcorr,e=1,p=2,w=2);SR(t3d,12) 
  t5d=carry12d(ccys,lag(ccys),ccy_spreads/4,lag(rccys),carry_m,0.01,2,start,v=vcorr,e=1,p=2,w=2);SR(t5d,12)
  t3d=carry12d(ccys,lag(ccys),ccy_spreads/4,-ppp,carry_m,0.01,2,start,v=vcorr,e=1,p=2,w=2);SR(t3d,12) 
  t5d=carry12d(ccys,lag(ccys),ccy_spreads/4,-realccys,carry_m,0.01,2,start,v=vcorr,e=1,p=2,w=2);SR(t5d,12)
  t3d=carry12d(ccys,lag(ccys),ccy_spreads/4,diff(log(ppp),6),carry_m,0.01,2,start,v=vcorr,e=1,p=2,w=2);SR(t3d,12) 
  t5d=carry12d(ccys,lag(ccys),ccy_spreads/4,diff(log(realccys),6),carry_m,0.01,2,start,v=vcorr,e=1,p=2,w=2);SR(t5d,12)
  
  t3=carry12(ccys,lag(ccys),ccy_spreads/4,ppp,carry_m,0.01,1,2,start,v=vcorr,e=1,p=2,w=2);SR(t3,12) 
  t5=carry12(ccys,lag(ccys),ccy_spreads/4,realccys,carry_m,0.01,1,2,start,v=vcorr,e=1,p=2,w=2);SR(t5,12)
  t3b=carry12(ccys,lag(ccys),ccy_spreads/4,-ppp,carry_m,0.01,-1,2,start,v=vcorr,e=1,p=2,w=2);SR(t3b,12) 
  t5b=carry12(ccys,lag(ccys),ccy_spreads/4,1/realccys,carry_m,0.01,-1,2,start,v=vcorr,e=1,p=2,w=2);SR(t5b,12)  
  
  
  t10d=carry12d(ccysind,lag(ccysind),ccysind_spreads/4,i2c,ccysind_carry_m,ycost=0.01,1,start,v=vcorr,e=1);SR(t10d,12)  
  t11d = momentum12d(zze_val,lag(zze_val),ccy_spreads/4,0,0,2,start,j=1,v=vcorr,e=1);SR(t11d,12)
  SR(binding(cbind(t10d,t11d,t2d,t4d,t7d)['1998-01/'])['2014-07/'],12)
  
  m3 <- load_rates(m3_names)
  m3 <- m3[,-1]
  rm3 <- lag(diff(log(m3),lag=12))
  cpi <- load_rates(cpi_names)
  cpi <- fillerxts(cpi)
  #rcpi <- lag(diff(log(cpi[,-1]),lag=12))
  cpi <- sweep(cpi,2,cpi['2017-3'],"/")
  lcpi <- log(cpi)
  ycpi <- lag(diff(lcpi[,-1],lag=12)) #annual inflation
  rcpi <- exp(sweep(lcpi[,-1], 1, -lcpi[,1], FUN = "+")) # inflation ratio vs US
  ppp <- lag(sweep(rcpi,2,(ppp1),'*'))
  realccys <- ccys_m*ppp
  plot(realccys[,4]['1996-01-01/'])
  plot(ppp[,4]['1996-01-01/'])
  summary(realccys)
  
  #rcpi <- sweep(rcpi,2,ccy_type, FUN = "*")
  rccys <- log(sweep(ccys_m,2,ccys_m['1996-1'],"/"))+lag(rcpi)
  fundrev <- diff(log(ccys_m),lag=12)+lag(diff(rcpi,lag=12))
  meanrev <- -(ccys/mat.SMA(ccys,5*252)-1)/5
  meanrev_m <- mat.to.monthly(meanrev)
  
  #RER for usdeur: RER=e*P^/P, P^ is price level in EZ
  #same as in ccys_names, IMF data for 12-Apr-2017
  ppp1 = c(0.8711,0.4725,0.9124,0.9013,1.3142,0.9367,1.1165,1.0797,1.0680,1.1414,1.0324,0.5792,0.4915,0.3928,0.4610,0.4783,0.3983,1.1110,0.6128) 
  #same as in ccys_names, WorldB data for 1-Jul-2017
  ppp1 = c(0.8539,0.4715,0.9053,0.8758,1.2336,0.9710,1.1435,1.0495,1.2382,1.1141,1.0673,0.5517,0.5013,0.4207,0.4564,0.4906,0.3776,1.0483,0.6372)
  
}

sectors<-function(){
  sec_names = c("bi.c","bk.c","bl.c","bm.c","bn.c","bs.c","ja.c","jb.c","je.c","jg.c")
  sectors = load_indices_loc(sec_names)
  usd_rate = local_load("ukousd3m")/100
  sector_rates = cbind(usd_rate,usd_rate,usd_rate,usd_rate,usd_rate)
  sector_rates = cbind(sector_rates,sector_rates)
  sstart=1
  sector_spreads=rep(10,10)
  s1=momentum12(sectors,lag(sectors),sector_spreads/4,-sector_rates*0.0,0.0000175,1,6,sstart,j=11,v=0,e=1);SR(s1,12)
  spx = local_load("^spx")
  SR(diff(log(spx))["2012-02-01/"])
  tt=cbind(sectors,spx)
  write.table(diff(log(mat.to.monthly(tt))),paste0(data_folder,"sectors.csv"))
}#s&p industry sectors

mixing<-function(){
  SR(c,12)
  SR(binding(cbind(a,c)),12)
  plot(cumsum(na20(c)))
  SR(c['2002-03/'],12)
  
  ret = diff(log(indices_m[,1]))
  w = (1-vix_m/100)/2
  SR(binding(cbind(a,b)))
  SR(log(rowMeans(exp(cbind(levret(1-lag(w),binding(cbind(a,b))),levret(lag(w),c))['2014-07/']),na.rm=T)),12)
  SR(log(rowMeans(exp(cbind(binding(cbind(a,b)),c)['/2014-06']),na.rm=T)),12)
  
  
  a=binding(cbind(t1d,t11d,t10d,t8*2,t3b,t4b));SR(a['/2014-06'],12) #++ 2*tw4m,2*tw11m
  #a=binding(cbind(t1d,t8d,t3d));SR(a['/2014-06'],12) #++ 2*tw4m,2*tw11m
  
  a=binding(cbind(t11d,t2d,t4d,t10d))['1997-07/'];SR(a['/2014-06'],12)
  b=binding(cbind(f1d*2,f3d/2,f5d/2))['1997-07/'];SR(b['/2014-06'],12) #deceased
  #c=binding(cbind(i2d,i3,i5d))
  c=binding(cbind(i8d,i6d,i5d,i10d))['1997-07/'];SR(c['/2014-06'],12)
  c=binding(cbind(i10d,i5d,i8d))['1997-07/'];SR(c['/2014-06'],12)
  d=binding(cbind(m1d,m4d,m3d))
  #c=binding(cbind(ii2d,ii4d,ii5d))
  SR(rowMeans(cbind(t1d,t8d,t3d,f1d,f3d,i2d,i4d,i5)['/2014-06'],na.rm=T),12)
  SR(binding(cbind(a,c))['/2014-06'],12)
  
  
  SR(diff(log(indices_m[,1]))['2014-07/'],12)
  
  SR(log(rowMeans(exp(cbind(i9d,i5d,i6d))['/2014-06'],na.rm=T)),12)
  
  mat.SR(merge(t11d,t10d,t2d,t4d,f1d,f3d,i8d,i6d,i5d,i9d)['/2014-06'])
  SR(log(rowMeans(exp(cbind(t10d,f1d,i5d,i9d))['/2014-06'],na.rm=T)),12)
  SR(log(rowMeans(exp(cbind(t10d,f1d,i5d,i9d))['2014-07/'],na.rm=T)),12)
  SR(log(rowMeans(exp(cbind(a,c))['/2014-06'],na.rm=T)),12)
  SR(log(rowMeans(exp(cbind(a,c))['2014-07/'],na.rm=T)),12)
  SR(rowMeans(cbind(a/2+awm/2,b,c)['/2014-06'],na.rm=T),12)
  SR(rowMeans(cbind(a/2+awm/2,b,c)['2014-07/'],na.rm=T),12)
  mar.covar=cor(cbind(a,b,c)['/2014-06'],use="complete.obs")
  cor(cbind(a,b,c)['2014-07/'],use="complete.obs")
  cor(cbind(a,b,c),use="complete.obs")
  mar = markovitz(mar.covar)
  SR(rowMeans(t(mar*t(cbind(a,b,c)['2014-07/'])),na.rm=T),12)
  mar = rollmarkovitz(cbind(a,b,c),"2014-06-01",12*16)
  SR(rowMeans((mar*cbind(a,b,c))['2014-07/'],na.rm=T),12)
  
  tt=log(rowMeans(exp(cbind(a,c))))
  tt=xts(cumsum(na20(tt)),index(a))
  plot2(tt,t10y3m)
  
  tt=cbind(t11,t11b,t10/2,t10b/2,t8*2,t3b,t4b,f1,f1b,f3,f3b,f4,f4b,i2+i2b,i3,i4b,i5+i5b)['1997-07/']
  tt=cbind(a,b,c)['1997-07/']
  tt=cbind(a,b,c)['2016-01/']
  pp=xts(cumsum(na20(rowMeans(tt,na.rm=TRUE))),index(tt))
  #5 x long-short
  #in this 4 x long from moms of both types and 1 short from ts-mom
  plot(exp(pp*8))
  
  plot2(pp,t10y3m-1.5)
  plot2(pp,edc_m)
  plot2(pp,log(ts_m$ukousd6m))
  plot2(pp,ts_m$ukousd6m-ts_m$ukousd1w)
  plot2(pp,t10y)
  plot2(pp,(t10y3m-1.5)*vix_m)
  
  #maxdrawdown(diff(log(c(1,2,3,2,4))))
  maxdrawdown(8*rowMeans(cbind(t11,t11b,t10/2,t10b/2,t8*2,t3b,t4b,f1,f1b,f3,f3b,f4,f4b,i2+i2b,i3,i4b,i5+i5b)['1997-07/'],na.rm=T))
  maxdrawdown(8*rowMeans(cbind(a,b,c)['1997-07/'],na.rm=T))
  maxdrawdown(diff(coredata(pp)*8))
  as.numeric(last(exp(pp*8)))/as.numeric(first(exp(pp*8)))
  
  SR(binding(cbind(a,b,c)['2006-09/'])['/2014-06'],12)
  SR(binding(cbind(f1d)['2006-09/'])['2014-07/'],12)
  #SR(binding(cbind(a,b,c,c4a/12)['2006-09/'])['2014-07/'],12)
  plot(cumsum(na20(binding(cbind(b,c)['2006-09/']))))
  
}# mixing

oilspread<-function(){
  #1. WTI/BRENT Spread/ A 20-day moving average of WTI/Brent spread is calculated each day. If the current spread value is above SMA 20 then we enter a short position in the spread on close (betting that the spread will decrease to fair value represented by SMA 20). The trade is closed at the close of the trading day when the spread crosses below fair value. If the current spread value is below SMA 20 then we enter a long position betting that the spread will increase and the trade is closed at the close of the trading day when the spread crosses above fair value.
  cl = local_load("cl.f")#cme_cl
  cb = local_load("cb.f")#ice_cb
  sc = local_load("sc.f")#cme_sc
  oilspread = local_load("eurdkk")
  summary(abs((rates[,"ukoeur3m"]-rates[,"IR3TIB01DKM156N"])["2006-09-01/"]))
  a1=splot(-diff(log(oilspread)),lag(oilspread-SMA(oilspread,5)));SR(a1["2006-09-01/"]-0.011/365,252)
  oilspread = cb-cl
  oilspread = sc-cl
  oilspreada = 73+cumsum(na20(diff(oilspread)))
  plot(oilspread-SMA(oilspread,20))
  plot(oilspreada)
  a1=splot(-diff(log(oilspreada)),lag(oilspread-SMA(oilspread,5)));SR(a1,252)
  a2=splot(-diff(log(oilspreada)),lag(oilspread-SMA(oilspread,10)));SR(a2,252)
  a3=splot(-diff(log(oilspreada)),lag(oilspread-SMA(oilspread,15)));SR(a3,252)
  a4=splot(-diff(log(oilspreada)),lag(oilspread-SMA(oilspread,20)));SR(a4,252)
  SR(binding(cbind(a1,a2))["2009-01-01/"],252)
  plot.cum(binding(cbind(a1,a2,a3)))
  plot.cum(a1["2006-09-01/"])
  #2. Payday Anomaly. The investment universe consists of S&P500 index. Simply, buy and hold the index during the 16th day in the month during each month of the year.
  spx = local_load("^spx")
  spxo = local_load("^spx",col=1)
  es = local_load("es.f")
  idx=index(spx["2006-09-01/"])
  cal1= xts(ifelse(week(idx) == 1,1,0),idx)
  cal1 = xts(ifelse(day(idx) == 16,1,0),idx)
  cal2 = xts(ifelse(day(idx) == 18,1,0),idx)
  a2 = splotL(diff(log(spx)),cal1+cal2);SR(a2["2006-09-01/"])
  plot.cum(a2["2006-09-01/"])
  spec1 = sapply(z,function(x) SR(splotL(diff(log(spx)),xts(ifelse((day(idx) %in% c((x %% 100)-1,x %% 100)*5 ) & wday(idx) == x %/% 100,1,0),idx))["2006-09-01/"],252))
  plot(z,spec1[1,],t="b");grid()
  z=numeric(0)
  for(i in 1:6) for (j in 1:5) z=c(z,j*100+i)
  #3. FED Model. Each month, the investor conducts a 1 month predictive regression (using all available data up to that date) predicting excess stock market returns using the yield gap as independent variable. The "Yield gap" is calculated as YG = EY - y, with earnings yield EY = ln (1 + E/P) and y = ln (1 + Y) is the log 10 year Treasury bond yield. Then, the strategy allocates 100% in the risky asset if the forecasted excess returns are positive and otherwise it invests 100% in the risk-free rate.
  ygap = 1/pe10-us10d
  d=rollalphabeta(diff(log(spx),21),lag(ygap,21),k=252)
  fits = d[,1]+d[,2]*ygap
  a3=splotL(log(lag(spx,-21)/spx),fits);SR(a3["2006-09-01/"],252/21,overlap=21)
  #4. Skewness Effect in Commodities. The investment universe consists of 27 futures contracts on commodities. Each month, investor calculates skewness (3rd moment of returns) from daily returns from data going 12 months into the past for all futures. Commodities are then sorted into quintiles and investor goes long quintile containing the commodities with the 20% lowest total skewness and short quintile containing the commodities with the 20% highest total skewness (over a ranking period of 12 months). Resultant portfolio is equally weighted and rebalanced each month.
  skews = diff(log(fut_d))
  for(i in 1:dim(skews)[2]) skews[,i] = rollskew(skews[,i],1/252)
  skews_m = mat.to.monthly(skews)
  a4=carry12(fut_d,fut_do,fut_spreads/4,skews_m,fut_carry_m,futcost,dir=1,k=2,s=2,v=0,e=1);SR(a4['2006-09-01/'],b=12)
  #5. Asset Class Trend Following. Use 5 ETFs (SPY - US stocks, EFA - foreign stocks, BND - bonds, VNQ - REITs, GSG - commodities), equal weight the portfolio. Hold asset class ETF only when it is over its 10 month Simple Moving Average, otherwise stay in cash.
  world_m = cbind(global_tot$`^spx`,global_tot$fstocks,global_tot$gc.f,global_tot$ty.f)
  a5=tsavgall(world_m,10,0,0,eqcost,e=1,dir=1);SR(a5["2006-09-01/"],12)
  SR(binding(diff(log(world_m)))["2006-09-01/"],12)
  usdhkd = local_load('usdhkd')
  xau = local_load('gc.f')
  fstocks = cumsum(na20(binding(diff(log(cbind(indices$`^dax`*1/ccys$usdeur,indices$`^ukx`*1/ccys$usdgbp,indices$`^nkx`*1/ccys$usdjpy,indices$`^aor`*1/ccys$usdaud,indices$`^hsi`*1/usdhkd))))))
  world = cbind(spx,fstocks,xau)
  world = world[complete.cases(world),]
  a5=tsavgall(world,10*21,0,0,eqcost,e=1,dir=1);SR(a5["2006-09-01/"],12*21)
  #6. Asset Class Momentum - Rotational System. Use 5 ETFs (SPY - US stocks, EFA - foreign stocks, BND - bonds, VNQ - REITs, GSG - commodities). Pick 3 ETFs with strongest 12 month momentum into your portfolio and weight them equally. Hold for 1 month and then rebalance.
  a6=momentum12(world,lag(world),0,0,0,1,2,2,j=12,v=1,e=1);SR(a6["2006-09-01/"],12)
  a6=momentum12(world_m,lag(world_m),0,0,0,1,2,2,j=12,v=1,e=1);SR(a6["2006-09-01/"],12)
  #7. Dollar Carry Trade. The investment universe consists of currencies from developed countries (the Euro area, Australia, Canada, Denmark, Japan, New Zealand, Norway, Sweden, Switzerland, and the United Kingdom). The average forward discount (AFD) is calculated for this basket of currencies (each currency has an equal weight). The average 3-month rate could be used instead of the AFD in the calculation. The AFD is then compared to the 3-month US Treasury rate. The investor goes long on the US dollar and short on the basket of currencies if the 3-month US Treasury rate is higher than the AFD. The investor goes short on the US dollar and long on the basket of currencies if the 3-month US Treasury rate is higher than the AFD. The portfolio is rebalanced monthly.
  #c("usdeur","usdgbp", "usdjpy", "usdchf" ,"usdcad", "usdaud" ,"usdnzd" ,"usdnok", "usddkk", "usdsek")
  colnames(ccys)[c(1,3:11)]
  ccysavg = xts(rowMeans(diff(log(ccys[,c(1,3:11)])),na.rm=T),index(ccys))
  carryavg = xts(rowMeans(carry_d[,c(1,3:11)],na.rm=T),index(carry_d))
  a7=splot(ccysavg+carryavg/365,-carryavg);SR(a7["2006-09-01/"],252)
  #8. Short Term Reversal with Futures. The investment universe consists of 24 types of US futures contracts (4 currencies, 5 financials, 8 agricultural, 7 commodities). A weekly time frame is used - a Wednesday- Wednesday interval. The contract closest to expiration is used, except within the delivery month, in which the second-nearest contract is used. Rolling into the second nearest contract is done at the beginning of the delivery month. The contract is defined as the high- (low-) volume contract if the contract's volume changes between period from t-1 to t and period from t-2 to t-1 is above (below) the median volume change of all contracts (weekly trading volume is detrended by dividing the trading volume by its sample mean to make the volume measure comparable across markets).
  #8. CD. All contracts are also assigned to either high-open interest (top 50% of changes in open interest) or low-open interest groups (bottom 50% of changes in open interest) based on lagged changes in open interest between period from t-1 to t and period from t-2 to t-1. The investor goes long (short) on futures from the high-volume, low-open interest group with the lowest (greatest) returns in the previous week. The weight of each contract is proportional to the difference between the return of the contract over the past 1 week and the equal-weighted average of returns on the N (number of contracts in group) contracts during that period.
  #9. Short Term Reversal in Stocks/ The investment universe consists of the 100 biggest companies by market capitalization. The investor goes long on the 10 stocks with the lowest performance in the previous month and goes short on the 10 stocks with the greatest performance from the previous month. The portfolio is rebalanced weekly.
  #10. PPP Strategy. Create an investment universe consisting of several currencies (10-20). Use the latest OECD Purchasing Power Parity figure to assess fair value of each currency versus USD in the month of publishing and then use monthly CPI changes and exchange rate changes to create fair PPP value for the month prior to the current month. Go long 3 currencies which are the most undervalued (lowest PPP fair value figure) and go short 3 currencies which are the most overvalued (highest PPP fair value figure). Invest cash not used as margin on overnight rates. Rebalance quarterly or monthly.
  #11. FX Carry Trade. Create an investment universe consisting of several currencies (10-20). Go long 3 currencies with highest central bank prime rates and go short 3 currencies with lowest central bank prime rates. The cash not used as margin is invested on overnight rates. The strategy is rebalanced monthly.
  #12. Currency Momentum Factor. Create an investment universe consisting of several currencies (10-20). Go long 3 currencies with strongest 12 month momentum against USD and go short 3 currencies with lowest 12 month momentum against USD. Cash not used as margin invest on overnight rates. Rebalance monthly.
  #13. Momentum Effect in Commodities. Create a universe of tradable commodity futures. Rank futures performance for each commodity for the last 12 months and divide them into quintiles. Go long on the quintile with highest momentum and go short on the quintile with lowest momentum. Rebalance each month.
  #14. Pairs Trading with Stocks. Cumulative total return index is then created for each stock (dividends included) and starting price during formation period is set to $1 (price normalization). Pairs are formed over a twelve-month period (formation period) and are then traded in next six-month period (trading period). The matching partner for each stock is found by looking for the security that minimizes the sum of squared deviations between two normalized price series. Top 20 pairs with the smallest historical distance measure are then traded and long-short position is opened when pair prices have diverged by two standard deviations and the position is closed when prices revert back.
  #16. Option-Expiration Week Effect. Option-expiration week is a week before options expiration (Friday before each 3rd Saturday in each month). Investors choose stocks from S&P 100 index as his/her investment universe (stocks could be easily tracked via ETF or index fund). He/she then goes long S&P 100 stocks during option expiration week and stays in cash during other days.
  #17. Volatility Risk Premium Effect. Each month, at-the-money straddle, with one month until maturity, is sold at the bid price with a 5% option premium, and an offsetting 15% out-of-the-money puts are bought (at the ask price) as insurance against a market crash. The remaining cash and received option premium is invested in the index. The strategy is rebalanced monthly.
  #18. Asset Growth Effect. The investment universe consists of all non-financial U.S. stocks listed on NYSE, AMEX, and NASDAQ. Stocks are then sorted each year at the end of June into ten equal groups based on the percentage change in total assets for the previous year. The investor goes long decile with low asset growth firms and short decile with high asset growth firms. The portfolio is weighted equally and rebalanced every year.
  #19. PutCall Ratio.
  #http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/totalpc.csv
  #http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/equitypc.csv
  #http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/indexpc.csv
  #http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/etppc.csv
  #http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/vixpc.csv
  #http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/totalpcarchive.csv
  #http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/pcratioarchive.csv
  
  pcratio = read.csv("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/indexpc.csv")
  pcratio=pcratio[-1,]
  pcratio[] <- lapply(pcratio, as.character)
  colnames(pcratio)=pcratio[1,]
  pcratio=pcratio[-1,]
  pcratio[,2:5] <- lapply(pcratio[,2:5], as.numeric)
  ts2xts<-function(tmp,format="%Y-%m-%d",col=1){
    xts((tmp[,1+col]),as.Date(as.character(tmp[,1]),format=format))
  }
  pcratio=ts2xts(pcratio,format="%m/%d/%Y",col=(1:4))
  
  summary(diff(pcratio[,4],10))
  a8=splotL(log(lag(spx,-10)/spx),pcratio[,4]<0.9);SR(a8["2006-09-01/"],252/10,overlap=10)
  a8=splotL(log(lag(spx,-10)/spx),pcratio[,4]>1.1);SR(a8["2006-09-01/"],252/10,overlap=10)
  a8=splotL(log(lag(spx,-10)/spx),abs(pcratio[,4]-1.1)<0.3);SR(a8["2006-09-01/"],252/10,overlap=10)
  a8=splotL(log(lag(spx,-10)/spx),diff(pcratio[,4],10)<0.001);SR(a8["2006-09-01/"],252/10,overlap=10)
  a8=splotL(log(lag(spx,-10)/spx),diff(pcratio[,4],10)>0.001);SR(a8["2006-09-01/"],252/10,overlap=10)
  
  #larry williams 
  #buy/sell when market closes near high/low
  #if price EOD is in the upper 65% of days range what would happen if we bought on close and exited 5, 10, 15 or 20 days later?
  spxh = local_load("^spx",col=2)
  spxl = local_load("^spx",col=3)
  logret = diff(log(spx))
  logrange = log(spxh/spxl)
  a8=splotL(log(lag(spx,-10)/spx),log(spx)-(0.35*log(spxl)+0.65*log(spxh)));SR(a8["2006-09-01/"],252/10,overlap=10)
  #buy new highs sell new lows
  #number of days of breakout (breaking rolling high/low)
  a8=splotL(log(lag(spx,-10)/spx),(spx)>lag(rollmaxna(spxh,15)));SR(a8["2006-09-01/"],252/10,overlap=10)
  a8=splotL(log(lag(spx,-10)/spx),(spx)<lag(-rollmaxna(-spxl,15)));SR(a8["2006-09-01/"],252/10,overlap=10)
  #buy week/monday if fri-mon gap is positive
  #we only make money on large-range days, these are usually close at or near the high/low in up/low day
  #instead of TP hold to the close of day
  #volatility breakout
  #difference in range size for different days
  #buy on the opening of the bias day + x% of previous days range, hold from 1d to 3d
  #test buy spx pn the open of the first trade day of every month with an exit on yhr first profitable opening
  #are some months better?
  #4 years phenomenom and other uneven cycles, 8 years
  #amazing october
  #bonds rise above volatility stop - buy spx
  #long only high yield stocks
  
  spx = local_load("^dji")
  spxo = local_load("^dji",col=1)
  spx = Fdax$Close
  spxo = Fdax$Open
  spx2 = tA[,4]
  spx2o =tA[,1]
  #mr x quintile conditions
  luka = log(spxo/lag(spx))
  luka2 = log(spx2o/lag(spx2))
  logretd = log(spx2/spx2o)
  logretoo = diff(log(spxo))
  hvold = emavol(logretd,63)
  hvoll = emavol(luka,63)
  #vix_d = ts2xts(read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=VIXCLS"))
  summary(logretd["/2006-09-01"])
  summary(luka["/2006-09-01"])
  q75 = quantile(luka["1962-01-01/"]["/2006-09-01"],probs=0.75,na.rm=TRUE)
  q25 = quantile(luka["1962-01-01/"]["/2006-09-01"],probs=0.25,na.rm=TRUE)
  q75 = quantile(luka["2000-01-01/"]["/2009-12-01"],probs=0.75,na.rm=TRUE)
  q25 = quantile(luka["2000-01-01/"]["/2009-12-01"],probs=0.25,na.rm=TRUE)
  a8=splotL(logretd,(luka-q75));SR(a8["2010-11-15/"],252)
  a8=splotL(logretd,(luka));SR(a8["2010-11-15/"],252)
  a8b=splotS(logretd,(luka-q25));SR(a8b["2006-09-01/"],252)
  quantile(logretd["1962-01-01/"]["/2006-09-01"],probs=0.75,na.rm=T)
  quantile(logretd["1962-01-01/"]["/2006-09-01"],probs=0.25,na.rm=T)
  a8=splotL(luka,-(lag(logretd)-0.0008));SR(a8["2006-09-01/"],252)
  a8=splotL(luka,-(lag(logretd)-0.00486));SR(a8["2006-09-01/"],252)
  a8=splotS(luka,-(lag(logretd)+0.0001));SR(a8["2006-09-01/"],252)
  a8=splotS(luka,-(lag(logretd)+0.0043));SR(a8["2006-09-01/"],252)
  quantile(logretoo["1962-01-01/"]["/2006-09-01"],probs=0.75,na.rm=T)
  quantile(logretoo["1962-01-01/"]["/2006-09-01"],probs=0.25,na.rm=T)
  a8=splotL(logretoo,(lag(logretoo)-0.005));SR(a8["2006-09-01/"],252)
  a8=splotS(logretoo,(lag(logretoo)+0.00435));SR(a8["2006-09-01/"],252)  
  
  SR(binding(cbind(a8,a8b))["2006-09-01/"],252)
  plot.cum(a8["2006-09-01/"])
  plot.cum(a8b["2006-09-01/"])
  plot.cum(binding(cbind(a8,a8b))["2006-09-01/"])
  plot(hvoll["2006-09-01/"])
  plot(hvoll/hvold["2006-09-01/"])
  # po zamianie M1 na M15:
  # zamkniecie minuty 14 (==open min. 15) z M1 przechodzi na zamkniecie minuty 14 z M15
  # open minuty 29 w M15 (==open min. 14) pochodzi z [zamkniecie min 14 w M1 == open min 15 w M1] 
  # minuta 29 jest zatem zakonczeniem periodu 15-29
  # close min 29 w M15 jest jak w M1

  summary(abs(spxo/opens-1))  
  spxusd = read.posix.table("SPXUSD15M.csv")[,-5]
  opens = subhourmin(spxusd,h='9',m='29')[,4]
  closes4NA = subhourmin(spxusd,h='15',m='14')[,4]
  closes = subhourmin(spxusd,h='15',m='59')[,4]
  d=cbindname(closes,closes4NA)
  closes = xts(ifelse(is.na(d$closes),d$closes4NA,d$closes),index(d))
  d=cbindname(spx,spxo,opens,closes)
  d=d[complete.cases(d)]
  luka2 = log(d$opens/lag(d$closes))
  logretd2 = log(d$closes/d$opens)
  logretd = log(d$spx/d$spxo)
  luka = log(d$spxo/lag(d$spx))
  
  summary(abs(luka2-luka))
  summary(abs(logretd2-logretd))
  a8=splotL(logretd2,(luka2-0.004));SR(a8["2006-09-01/"],252)
  a8=splotL(levret(-1,logret),lag(logret-0.004));SR(a8["2006-09-01/"],252)
  a8=splotL(logretd2,(luka2-0.006));SR(a8["2006-09-01/"],252)
  a8=splotL(logretd2,(luka));SR(a8["2006-09-01/"],252)

  correct = quantile(normalize(logret),probs=0.01,na.rm=T) / qnorm(0.01)
  var1 = quantile((logret),probs=0.01,na.rm=T)
  var1
  SRvar = function(x){mean(x,na.rm=T)/quantile(x,probs=0.01,na.rm=T)}
  1/SRvar(logret)
  # var_sums = 0.5d * var1 + 1.5d * var1 + ... ~ var1*(mean_delta)*N
  vv1=(ifelse(war1,put10,NA)+lag(ifelse(war1,pays,NA),stm))/d$S;SR(vv1["2006-09-01/"],12,overlap=21)
  put10delta = -Delta76(Fwd,K,d$vol,tau[k],d$rd,-1)
  put10gamma = -Gamma76(Fwd,K,d$vol,tau[k],d$rd,-1)
  deltas = -hedge_series(d$S,d$vol,K,d$rd,rn)
  vv1put10delta*var1 + 0.5 * put10gamma*var1^2  
  quantile((logret),probs=0.01,na.rm=T)*sqrt(252)
  quantile((pays+put10)/d$S,probs=0.01,na.rm=T)*sqrt(12)
  mean(put10delta*var1 + 0.5 * put10gamma*var1^2)*sqrt(252)
  mean(put10delta*var1)*sqrt(252)
  mean(deltas/20*var1)*sqrt(252)

  d0 = '1997-09'
  d1 = '2006-09'
  d2 = '2018-09'
  take <- function(x_xts,date1='1970-01-01',date2='2020-12-31'){x_xts[paste0(date1,'/')][paste0('/',date2)]}
  
  vv1=(ifelse(war1,put10,NA)+lag(ifelse(war1,pays,NA),stm))/d$S;
  v4= splotL(diff(rollvix)/200,lag(d$CBOE_VX-vix));
  SR(vv1["2006-09-01/"]*3,12,overlap=21)
  SR(i9d["2006-09-01/"]*10,12)
  SR(v4["2006-09-01/"],252) #0.785
  SR(binding(cbind(i9d*10,downsample(v4),downsample(a2)))['2006-09/'],12)
  plot.cum(binding(cbind(i9d*10,downsample(v4),downsample(a2)))['2006-09/'])
  
  SR(take(binding(cbind(i9d*10,downsample(v4),downsample(a2))),d0,d1),12)
  SR(take(binding(cbind(downsample(vv1*3,overlap=21),downsample(v4),downsample(a2))),d1,d2),12)
  SR(take(binding(cbind(i9d*10,downsample(v4),downsample(a2))),d2),12)
  
  #jakie indeksy, jak wyglada ich srednia
  s=1:24
  i9d = carry12d(indices[,s],indiceso[,s],indices_spreads[s]/4,inddd_m[,s],-indices_rates[,s]/10,0.00175,2,start,v=1,e=1);SR(i9d,12)
  plot.cum(take(binding(diff(log(indices_m))),d0,d1))
  head(indices[,1])
  head(inddd[,1])
  plot((drawdown(diff(log(indices_m))[,1])))

  #odsezonowac levele w kursie tak jak robi TRAMO-SEATS i sprawdzic bledy prognozy
  
}#quantpedia

volopt<-function(){
  aa = 1/ccys['1995-09/']
  aa = indices['1995-09/'][,-18]
  vvind = aa
  for(i in 1:dim(aa)[2]) vvind[,i] = vvolind(aa[,i],0.02)
  volind = aa
  volind2 = aa
  for(i in 1:dim(aa)[2]) volind[,i] = emavol(diff(log(aa[,i])),63)
  for(i in 1:dim(aa)[2]) volind2[,i] = emavol(diff(log(aa[,i])),252)
  for(i in 1:dim(aa)[2]) aa[,i] = quickvs(aa[,i],1)
  y=numeric(dim(aa)[2]); for(i in 1:dim(aa)[2]) y[i]=w2l(aa[,i])
  mean(y)
  aav = aa
  for(i in 1:dim(aa)[2]) aav[,i] = splot(aa[,i],lag(vvind[,i],4))
  
  SR(diff(log(quickvsloop(indices['2009-09/'][,1],5))))
  SR(diff(log(indices['2009-09/'][,1])))
  SR(diff(log(indices['1995-09/'][,1]/fnna(indices['1995-09/'][,1])+quickvsloop(indices['1995-09/'][,1],5)/10)))
  SR(diff(log(exp(cumsum(na20(tsmomenh(diff(log(indices['2009-09/'][,1]))))))+quickvsloop(indices['2009-09/'][,1],5)/10)))
  plot(indices['1995-09/'][,1]/fnna(indices['1995-09/'][,1]))
  plot(quickvsloop(indices['1995-09/'][,1],5)/10)
  plot(quickvsloop(indices['2002-01/']['/2011-09'][,1],5)/10)
  plot(vix['2002-01/']['/2011-09']/10,col=2)
  plot(splot(quickvsloop(indices[,1],5)/10,lag(vvind[,1],4))['2008-09/']['/2009-01'],type="b")
  plot(exp(cumsum(na20(tsmomenh(diff(log(indices['1995-09/'][,1]))))))+indices['1995-09/'][,1]/fnna(indices['1995-09/'][,1])+quickvsloop(indices['1995-09/'][,1],5)/10)
  SR(tsmomenh(diff(log(indices['2009-09/'][,1]))))
  plot(exp(cumsum(na20(tsmomenh(diff(log(indices['1995-09/'][,1])))))))
  plot(exp(cumsum(na20(tsmomenh(diff(log(indices['1995-09/'][,1]))))))+quickvsloop(indices['1995-09/'][,1],5)/10)
  SR(diff(log(splot(quickvsloop(indices['1995-09/'][,1],5)/10,lag(vvind[,1],4)))))
  
  plot(indices[,1])
  
  for(i in 1:dim(aa)[2]) print(paste(colnames(aa)[i],SR(diff((aa[,i])),252)))
  for(i in 1:dim(aa)[2]) print(paste(colnames(aa)[i],SRc(diff(log(aav[,i])),252)))
  for(i in 1:dim(aa)[2]) print(paste(colnames(aa)[i],w2l(aa[,i])))
  SR(xts(log(1+rowMeans(diff(aa[,c(1:4,7,9)])/lag(aa[,c(1:4,7,9)]),na.rm=TRUE)),index(aa)),252)
  SR(xts(rowMeans(diff(aa),na.rm=TRUE),index(aa)),252)
  w2l(aa)
  SR(diff(log(aa[,1])),252)
  SR(diff((aa[,7])),252)
  
  SR(diff(aa[,1]),252)
  SRdl(aa[,3],252)
  plot(EMA(diff(aa[,1]),1/21))
  plot(aav[,3])
  plot(emavol(diff(log(indices))['1995-01/'][,3],63))
  vvind = vvolind(indices['1995-01/'][,3],0.03)
  plot(vvind)
  plot(splot(aa[,3],lag(vvind,5)))
  plot(splot(aa[,3],lag(emavol(diff(log(indices))['1995-09/'][,3],63)-0.2,5)))
  SR(diff(log(aa[,3])),252)
  SR(diff(log(aav[,3])),252)
  SR(diff(log(splot(aa[,3],lag(emavol(diff(log(indices))['1995-09/'][,3],63)-0.1,5)))))
  
  as.numeric(indices[1,1])^2*diff(1/lag(indices[1:10,1],2),2)
  
  plot(aav[,9])
  lines(aav[,1],col=2)
  lines(aav[,4],col=3)
  lines(aav[,3],col=4)
  plot(xts(exp(cumsum(na20(log(1+rowMeans(diff(aa[,c(1:4,7,9)])/lag(aa[,c(1:4,7,9)]),na.rm=TRUE))))),index(aa)))
  
  plot(log(aa[,1]))
  plot(volind[,1])
  plot(volind[,1]-volind2[,1])
  lines(volind2[,1],col=2)
  abline(h=0.2,col=3)
  SR(diff(log(aa[,1])),252)
  SR(diff(log(splot(aa[,1],-volind[,1]+0.15))),252)
  SR(diff(log(splot2(aa[,1],-volind[,1]+0.15))),252)
  SR(diff(log(splot(aa[,1],-volind2[,1]+0.15))),252)
  SR(diff(log(splot(aa[,1],volind2[,1]-volind[,1]+0.1))),252)
  SR(diff(log(splot2(aa[,1],volind2[,1]-volind[,1]+0.1))),252)
  plot(log(splot(aa[,1],-volind[,1]+0.15)))
  SR(diff(log(splot(aa[,1],vvind[,1]))),252)
  plot(log(splot(aa[,1],vvind[,1])))
  SR(tsavg(aa[,1],63,0),252)
  SR(tsmom(aa[,1],230,0,0),252)
  plot(cumsum(na20(tsavg(aa[,1],300,0))))  
  
  for(i in 1:dim(aa)[2]) print(SR(diff(log(aa[,i])),252))
  for(i in 1:dim(aa)[2]) print(SR(diff(log(splot(aa[,i],-volind[,i]+0.15))),252))
  for(i in 1:dim(aa)[2]) print(SR(diff(log(splot(aa[,i],volind2[,i]-volind[,i]+0.1))),252))
  for(i in 1:dim(aa)[2]) print(SR(diff(log(splot(aa[,i],vvind[,i]))),252))
  for(i in 1:dim(aa)[2]) print(paste(colnames(aa)[i],SR(diff(log(splot(aa[,i],-volind[,i]+0.15))),252)))
  plot(log(splot(aa[,19],-volind[,19]+0.15)))
  
  for(i in 1:dim(xxe)[2]) print(SR(diff(log(xxe[,i]))))
}#var swap without options

crypto<-function(){
  crypto_names =  c("BTC-USD","BCH-USD","ETH-USD","LTC-USD","XRP-USD","ADA-USD","NEO-USD","XEM-USD","EOS-USD","XLM-USD","XMR-USD","IOT-USD","DASH-USD")
  crypto_spreads = c(0.01,0.1,0.1,0.5,rep(15,9),0.01)
  crypto = load_indices_loc(crypto_names,f="")
  crypto = crypto['2015-01-29/']
  crypto = crypto[,1:5]
  crypto = fillerxts(crypto)
  crypto_w = mat.to.weekly(crypto)
  crypto_m = mat.to.monthly(crypto)
  cryptoLS = cbind(crypto,1/crypto[,1])
  
  cc=tsavgall(crypto,10,crypto_spreads,0,0,e=1)
  cc_val = cumulate(yy)
  SR(xts(rowMeans(cc,na.rm=TRUE),index(cc)),252)
  i=3
  cc=tsavg(crypto[,i],21,crypto_spreads[i],0,0,dir=1);SR(cc)
  cc=tsavg(crypto[,i],15,crypto_spreads[i],0,0,dir=1);SR(cc)
  cc=tsavg(crypto[,i],10,crypto_spreads[i],0,0,dir=1);SR(cc)
  cc=tsavg(crypto[,i],7,crypto_spreads[i],0,0,dir=1);SR(cc)
  cc=tsavg(crypto[,i],5,crypto_spreads[i],0,0,dir=1);SR(cc)
  SR(binding(cbind(cc1,cc2,cc3)),252)
  plot(exp(cumsum(na20(binding(cbind(cc1,cc2,cc3))))))
  
  cstart=3
  c1d=momentum12d(crypto,lag(crypto),crypto_spreads,0,0,1,start,j=21,v=1,e=1,f=252);SR(c1d['/2017-06'])
  c2d=momentum12d(crypto,lag(crypto),crypto_spreads,0,0,1,2,j=13,v=1,e=1,f=52);SR(c2d['/2017-06'],52)
  c3d=momentum12d(crypto,lag(crypto),crypto_spreads,0,0,1,2,j=3,v=1,e=1,f=12);SR(c3d['/2017-06'],12)
  c1a=momentum12(crypto,lag(crypto),crypto_spreads,0,0,1,2,start,j=21,v=1,e=1,f=252);SR(c1a['/2017-06'])
  c2a=momentum12(crypto,lag(crypto),crypto_spreads,0,0,1,2,2,j=1,v=1,e=1,f=52);SR(c2a['/2017-06'],52)
  c3a=momentum12(crypto,lag(crypto),crypto_spreads,0,0,1,2,2,j=1,v=1,e=1,f=12);SR(c3a['/2017-06'],12)
  c4a=momentum12(cryptoLS,lag(cryptoLS),crypto_spreads,0,0,1,2,2,j=1,v=1,e=1,f=12);SR(c4a['/2017-06'],12)
  SR(c1d['2017-07/'])
  SR(c2d['2017-07/'],52)
  SR(c3d['2017-07/'],12)
  SR(c1a['2017-07/'])
  SR(c2a['2017-07/'],52)
  SR(c3a['2017-07/'],12)
  SR(c1a)
  
  plot(cumsum(na20(c4a)))
  
  diff(log(crypto))['2017-12-29']
  crypto['2017-12-28']
  plot(log(crypto[,"ETH-USD"]))
  
  SR(diff(log(crypto[,"ETH-USD"])))
  SR(diff(log(crypto[,"BCH-USD"])))
}#crypto

stocks<-function(){
  data_folder = "C:/temp/stocks/"
  #for wig20+wig40+wig80 as of 14-may-2019
  stocks_name = tolower(c("ALR","CCC","CDR","CPS","DNP","JSW","KGH","LPP","LTS","MBK","OPL","PEO","PGE","PGN","PKN","PKO","PLY","PZU","SPL","TPE","11B","ACP","AMC","ATT","BDX","BFT","BHW","BRS","CAR","CIE","CIG","CMR","EAT","ECH","ENA","ENG","EUR","FMF","FTE","GPW","GTC","GTN","ING","KER","KRU","KTY","LCC","LVC","LWB","MAB","MIL","ORB","PKP","PLW","PXM","STP","TRK","VST","WPL","WWL","1AT","ABC","ABE","ABS","ACG","AGO","ALI","AMB","AML","APR","APT","ASB","ASE","AST","ATG","BAH","BIO","BKM","BNP","BOS","CLN","CMP","COG","CPG","CRM","DBC","DOM","ELB","EMT","ENT","FRO","GCN","IDA","IMC","INK","IPX","KAN","KGN","LTX","MCI","MDG","MGT","MLG","MNC","MON","NET","NEU","NWG","OPN","OVO","PBX","PCE","PCM","PCR","PEP","PFL","PHN","PND","PSW","PWX","QRS","R22","RBW","RFK","SKA","SKH","SLV","SNK","STX","TEN","TIM","TOA","TOR","VGO","VOX","VVD","WLT","WSE","ZEP"))
  stockspe_name = paste0(tolower(stocks_name),"_pe")
  stockspb_name = paste0(tolower(stocks_name),"_pb")
  stocksmv_name = paste0(tolower(stocks_name),"_mv")
  #download_stooq(stocksmv_name[-c(1:24)])
  stocks_spreads = rep(0.0025,139)
  rate = multp(to.monthly(local_load("plopln1m"))[,4]/100,139)
  rate_d = multp((local_load("plopln1m"))/100,139)
  benchmark1 = local_load("mwig40")
  benchmark2 = local_load("wig20")
  benchmark = exp(cum(binding(diff(log(cbind(benchmark1,benchmark2))))))
  plot(benchmark["2008-07-01/"])
  plot(wigpe[,1]["2008-07-01/"])
  stocks = load_indices_loc(stocks_name)
  stocks_m = mat.to.monthly(stocks)
  stocks = fillerxts(stocks)
  stocksH = load_indices_loc(stocks_name,col=2)
  stocksL = load_indices_loc(stocks_name,col=3)
  stocksH=fillerxts(stocksH)
  stocksH=fillerxtsNON0(stocksH)
  stocksL=fillerxts(stocksL)
  stocksL=fillerxtsNON0(stocksL)
  stockspe = load_indices_loc(stockspe_name)
  stockspe = fillerxts(stockspe)
  stockspe_m = mat.to.monthly(stockspe)
  stockspb = load_indices_loc(stockspb_name)
  stockspb = fillerxts(stockspb)
  stockspb_m = mat.to.monthly(stockspb)
  start = 12
  s=1:60
  eqcost=0.00175
  vcorr=1
  s1d=momentum12d(stocks[,s],lag(stocks[,s]),stocks_spreads[s]/4,-rate[,s]/1,eqcost,3,start,j=11,v=vcorr,e=1);SR(s1d,12)  
  s2d=carry12d(stocks[,s],lag(stocks[,s]),stocks_spreads[s]/4,-stockspe_m[,s],-rate[,s]/1,eqcost,3,start,v=vcorr,e=1);SR(s2d,12)
  s3d=carry12d(stocks[,s],lag(stocks[,s]),stocks_spreads[s]/4,-stockspb_m[,s],-rate[,s]/1,eqcost,3,start,v=vcorr,e=1);SR(s3d,12)
  SR(binding(cbind(s1d,s2d,s3d))["2008-07-01/"],12)
  s1=momentum12(stocks[,s],lag(stocks[,s]),stocks_spreads[s]/4,-rate[,s]/1,eqcost,1,3,start,j=11,v=vcorr,e=1);SR(s1,12)  
  s2=carry12(stocks[,s],lag(stocks[,s]),stocks_spreads[s]/4,stockspe_m[,s],-rate[,s]/1,eqcost,1,3,start,v=vcorr,e=1);SR(s2,12)
  s3=carry12(stocks[,s],lag(stocks[,s]),stocks_spreads[s]/4,-stockspb_m[,s],-rate[,s]/1,eqcost,1,3,start,v=vcorr,e=1);SR(s3,12)
  SR(binding(cbind(s1,s3))["2008-07-01/"],12)
  SR(diff(log(benchmark["2008-07-01/"])),12)
  plot.cum(s3d)
  plot.cum(binding(cbind(s1d,s2d,s3d))["2008-07-01/"])
  
  tt=tsavgmatrices(stocks[,s],lag(stocks[,s]),n=232,stocks_spreads,-rate_d[,s]/1,eqcost,t=0.17)
  ss_val=tt[[1]]
  ss_valo=tt[[2]]
  sse_val=tt[[3]]
  sse_valo=tt[[4]]
  SR(binding(diff(log(ss_val)))["2008-07-01/"],12)
  SR(binding(diff(log(sse_val)))["2008-07-01/"],12)
  
  stocksvol = diff(log(stocks)); for(i in 1:dim(stocksvol)[2]) stocksvol[,i] = sqrt(SMA(stocksvol[,i]^2,11*21)*12*21) #smavolSem(indvol[,i],11,12)
  stocksSR = diff(log(stocks),lag=11*21)*12/11 / stocksvol
  stocksvol = diff(log(stocks)); for(i in 1:dim(stocksvol)[2]) stocksvol[,i] = (sqrt(EMA(stocksvol[,i]^2,1/(3*21))*252))
  #for(i in 1:dim(indvol)[2]) indvol[,i] = na20(emavolSem(indvol[,i],3*21,p=-1))
  stocksdd = diff(log(stocks)); for(i in 1:dim(stocksdd)[2]) stocksdd[,i] = xts(cummin(drawdown(stocksdd[,i])),index(stocksdd[,i]))
  for(i in 1:dim(stocks)[2]) {tmp=rollbeta(stocks[,i],benchmark,k=252); if(i==1) stocksbet=tmp else stocksbet = cbind(stocksbet,tmp)}
  
  stocksSR_m = mat.to.monthly(stocksSR)
  stocksvol_m = mat.to.monthly(stocksvol)
  stocksdd_m = mat.to.monthly(stocksdd)
  stocksbet_m = mat.to.monthly(stocksbet)
  eps = stocks/stockspe
  epsgrowth = diff(log(eps),9*22)
  epsgrowth_m = mat.to.monthly(epsgrowth)
  #volbreaks tests  
  #test = tsvolbreak(indices[,1],indicesH[,1],indicesL[,1],5,indices_spreads[1],indices_rates_d[,1])
  #test = xts(exp(cumsum(na20(test))),index(indices[,1]))
  #plot(test)
  cvolbr = crossvolbreak(stocks,stocksH,stocksL,5); stocksbreak_m = mat.to.monthly(cvolbr)
  stocksmaxp = maxproximity(stocks,stocksH,5*51); stocksmaxp_m = mat.to.monthly(stocksmaxp)
  stocksmaxp = diff(log(stocks)); for(i in 1:dim(stocksmaxp)[2]) stocksmaxp[,i] = xts((rolldrawdown(stocksmaxp[,i],diff(log(stocksH[,i])),k=5*51)),index(stocksmaxp[,i]))
  stocksmaxp_m = mat.to.monthly(stocksmaxp)
  tt = rollbeta(stocks[,2],benchmark,252)
  #,index(stocksbet[,i]))
  stocksbet=out
  summary(index(stocksbet))
  plot(61-xts(rowSums(is.na(stocks_m[,s])),index(stocks_m)))
  
  s4=carry12(stocks[,s],lag(stocks[,s]),stocks_spreads[s]/4,stocksvol_m[,s],-rate[,s]/1,eqcost,1,3,start,v=vcorr,e=1);SR(s4,12)
  #this is bad #s5=carry12(stocks[,s],lag(stocks[,s]),stocks_spreads[s]/4,-stocksSR_m[,s],-rate[,s]/1,eqcost,1,3,start,v=vcorr,e=1);SR(s5,12)
  s6=carry12(stocks[,s],lag(stocks[,s]),stocks_spreads[s]/4,-stocksdd_m[,s],-rate[,s]/1,eqcost,1,3,start,v=vcorr,e=1);SR(s6,12)
  s7=carry12(stocks[,s],lag(stocks[,s]),stocks_spreads[s]/4,-stocksbreak_m[,s],-rate[,s]/1,eqcost,1,3,start,v=vcorr,e=1);SR(s7,12)
  s8=carry12(stocks[,s],lag(stocks[,s]),stocks_spreads[s]/4,-stocksmaxp_m[,s],-rate[,s]/1,eqcost,1,3,start,v=vcorr,e=1);SR(s8,12)
  s9=carry12(stocks[,s],lag(stocks[,s]),stocks_spreads[s]/4,stocksbet_m[,s],-rate[,s]/1,eqcost,1,3,start,v=vcorr,e=1);SR(s9,12)
  s10=carry12(stocks[,s],lag(stocks[,s]),stocks_spreads[s]/4,-epsgrowth_m[,s],-rate[,s]/1,eqcost,1,3,start,v=vcorr,e=1);SR(s10,12)
  SR(binding(cbind(s3,s1,s10,s8,s9))["2008-07-01/"],12)
  dim(stocksmaxp)
  stocksmaxp_m = mat.to.monthly(stocksmaxp)
  stocksmaxp_m[200,1]
  index(stocksmaxp_m)[1:30]
  plot.cum(binding(cbind(s3,s1,s10,s8,s9))["2008-07-01/"])
  
  s3_a = carry12_1(stocks[,s],-stockspb_m[,s],1,3,start)
  s6_a = carry12_1(stocks[,s],-stocksdd_m[,s],1,3,start)
  s7_a = carry12_1(stocks[,s],-stocksbreak_m[,s],1,3,start)
  s8_a = carry12_1(stocks[,s],-stocksmaxp_m[,s],1,3,start)
  s10_a = carry12_1(stocks[,s],-epsgrowth_m[,s],1,3,start)
  mix=mat.02na(sumNoNA(s3_a,s8_a,s10_a))/(3*1)*1.5
  summary(rowSums(mix,na.rm=T))
  #mix=mat.cens(mat.02na(sumNoNA(s3_a,s8_a,s10_a)),2)/(5*3)
  s3m = carry12_2(stocks[,s],lag(stocks[,s]),stocks_spreads[s]/4,mix,-rate[,s]/1,eqcost,1,v=vcorr,e=1);#SR(s3m,12)
  summary(rowSums(s3m,na.rm=T))
  plot(ts(rowSums(s3m,na.rm=T)))
  s3m = carry12(stocks[,s],lag(stocks[,s]),stocks_spreads[s]/4,mix,-rate[,s]/1,eqcost,1,8,start,v=vcorr,e=1);SR(s3m,12)
  SR(binding(cbind(s3,s10,s8))["2008-07-01/"],12)
  SR(s3m["2008-07-01/"],12)
  
  #for sp500 as of 14-may-2019
  l1 = tolower(c("A.US","AAL.US","AAP.US","AAPL.US","ABBV.US","ABC.US","ABT.US","ACN.US","ADBE.US","ADI.US","ADM.US","ADP.US","ADS.US","ADSK.US","AEE.US","AEP.US","AES.US","AFL.US","AGN.US","AIG.US","AIV.US","AIZ.US","AJG.US","AKAM.US","ALB.US","ALGN.US","ALK.US","ALL.US","ALLE.US","ALXN.US","AMAT.US","AMD.US","AME.US","AMG.US","AMGN.US","AMP.US","AMT.US","AMZN.US","ANSS.US","ANTM.US","AON.US","AOS.US","APA.US","APC.US","APD.US","APH.US","APTV.US","ARE.US","ARNC.US","ATVI.US","AVB.US","AVGO.US","AVY.US","AWK.US","AXP.US","AYI.US","AZO.US","BA.US","BAC.US","BAX.US","BBT.US","BBY.US","BDX.US","BEN.US","BHF.US","BHGE.US","BIIB.US","BK.US","BKNG.US","BLK.US","BLL.US","BMY.US","BSX.US","BWA.US","BXP.US","C.US","CAG.US","CAH.US","CAT.US","CB.US","CBOE.US","CBRE.US","CBS.US","CCI.US","CCL.US","CDNS.US","CELG.US","CERN.US","CF.US","CFG.US","CHD.US","CHRW.US","CHTR.US","CI.US","CINF.US","CL.US","CLX.US","CMA.US","CMCSA.US","CME.US","CMG.US","CMI.US","CMS.US","CNC.US","CNP.US","COF.US","COG.US","COO.US","COP.US","COST.US","COTY.US","CPB.US","CRM.US","CSCO.US","CSX.US","CTAS.US","CTL.US","CTSH.US","CTXS.US","CVS.US","CVX.US","CXO.US","D.US","DAL.US","DE.US","DFS.US","DG.US","DGX.US","DHI.US","DHR.US","DIS.US","DISCA.US","DISCK.US","DISH.US","DLR.US","DLTR.US","DOV.US","DRE.US","DRI.US","DTE.US","DUK.US","DVA.US","DVN.US","DWDP.US","DXC.US","EA.US","EBAY.US","ECL.US","ED.US","EFX.US","EIX.US","EL.US","EMN.US","EMR.US","EOG.US","EQIX.US","EQR.US","EQT.US","ES.US","ESS.US","ETFC.US","ETN.US","ETR.US","EW.US","EXC.US","EXPD.US","EXPE.US","EXR.US","F.US","FAST.US","FB.US","FBHS.US","FCX.US","FDX.US","FE.US","FFIV.US","FIS.US","FISV.US","FITB.US","FL.US","FLIR.US","FLR.US","FLS.US","FMC.US","FOX.US","FOXA.US","FRT.US","FTI.US","FTV.US","GD.US","GE.US","GILD.US","GIS.US","GLW.US","GM.US","GOOG.US","GOOGL.US","GPC.US","GPN.US","GPS.US","GRMN.US","GS.US","GT.US","GWW.US","HAL.US","HAS.US","HBAN.US","HBI.US","HCA.US","HCP.US","HD.US","HES.US","HIG.US","HII.US","HLT.US","HOG.US","HOLX.US"))
  l2 = tolower(c("HON.US","HP.US","HPE.US","HPQ.US","HRB.US","HRL.US","HRS.US","HSIC.US","HST.US","HSY.US","HUM.US","IBM.US","ICE.US","IDXX.US","IFF.US","ILMN.US","INCY.US","INFO.US","INTC.US","INTU.US","IP.US","IPG.US","IPGP.US","IQV.US","IR.US","IRM.US","ISRG.US","IT.US","ITW.US","IVZ.US","JBHT.US","JCI.US","JEC.US","JNJ.US","JNPR.US","JPM.US","JWN.US","K.US","KEY.US","KHC.US","KIM.US","KLAC.US","KMB.US","KMI.US","KMX.US","KO.US","KR.US","KSS.US","KSU.US","L.US","LB.US","LEG.US","LEN.US","LH.US","LKQ.US","LLL.US","LLY.US","LMT.US","LNC.US","LNT.US","LOW.US","LRCX.US","LUV.US","LYB.US","M.US","MA.US","MAA.US","MAC.US","MAR.US","MAS.US","MAT.US","MCD.US","MCHP.US","MCK.US","MCO.US","MDLZ.US","MDT.US","MET.US","MGM.US","MHK.US","MKC.US","MLM.US","MMC.US","MMM.US","MNST.US","MO.US","MOS.US","MPC.US","MRK.US","MRO.US","MS.US","MSCI.US","MSFT.US","MSI.US","MTB.US","MTD.US","MU.US","MYL.US","NAVI.US","NBL.US","NCLH.US","NDAQ.US","NEE.US","NEM.US","NFLX.US","NI.US","NKE.US","NKTR.US","NLSN.US","NOC.US","NOV.US","NRG.US","NSC.US","NTAP.US","NTRS.US","NUE.US","NVDA.US","NWL.US","NWS.US","NWSA.US","O.US","OKE.US","OMC.US","ORCL.US","ORLY.US","OXY.US","PAYX.US","PBCT.US","PCAR.US","PCG.US","PEG.US","PEP.US","PFE.US","PFG.US","PG.US","PGR.US","PH.US","PHM.US","PKG.US","PKI.US","PLD.US","PM.US","PNC.US","PNR.US","PNW.US","PPG.US","PPL.US","PRGO.US","PRU.US","PSA.US","PSX.US","PVH.US","PWR.US","PXD.US","PYPL.US","QCOM.US","QRVO.US","RCL.US","RE.US","REG.US","REGN.US","RF.US","RHI.US","RHT.US","RJF.US","RL.US","RMD.US","ROK.US","ROP.US","ROST.US","RRC.US","RSG.US","RTN.US","SBAC.US","SBUX.US","SCHW.US","SEE.US","SHW.US","SIVB.US","SJM.US","SLB.US","SLG.US","SNA.US","SNPS.US","SO.US","SPG.US","SPGI.US","SRCL.US","SRE.US","STI.US","STT.US","STX.US","STZ.US","SWK.US","SWKS.US","SYF.US","SYK.US","SYMC.US","SYY.US","T.US","TAP.US","TDG.US","TEL.US","TGT.US","TIF.US","TJX.US","TMK.US","TMO.US","TPR.US","TRIP.US","TROW.US","TRV.US","TSCO.US","TSN.US","TSS.US","TTWO.US","TXN.US","TXT.US","UA.US","UAA.US","UAL.US","UDR.US","UHS.US","ULTA.US","UNH.US","UNM.US","UNP.US","UPS.US","URI.US","USB.US","UTX.US","V.US","VAR.US","VFC.US","VIAB.US","VLO.US","VMC.US","VNO.US","VRSK.US","VRSN.US","VRTX.US","VTR.US","VZ.US","WAT.US","WBA.US","WDC.US","WEC.US","WELL.US","WFC.US","WHR.US","WLTW.US","WM.US","WMB.US","WMT.US","WRK.US","WU.US","WY.US","WYNN.US","XEC.US","XEL.US","XLNX.US","XOM.US","XRAY.US","XRX.US","XYL.US","YUM.US","ZBH.US","ZION.US","ZTS.US"))
  spx_name = c(l1,l2); rm(l1,l2)
  download_stooq(spx_name)
  
  #wig indeksy
  wig_name = tolower(c("WIG20","MWIG40","SWIG80","WIG_BANKI","WIG_BUDOW","WIG_CEE","WIG_CHEMIA","WIG_ENERG","WIG_GAMES","WIG_GORNIC","WIG_INFO","WIG_LEKI","WIG_MEDIA","WIG_MOTO","WIG_MS_BAS","WIG_MS_FIN","WIG_MS_PET","WIG_NRCHOM","WIG_ODZIEZ","WIG_PALIWA","WIG_POLAND","WIG_SPOZYW","WIG_TELKOM","WIG_UKRAIN"))
  wigpe_name = paste0(tolower(wig_name),"_pe")
  wigpb_name = paste0(tolower(wig_name),"_pb")
  wigmv_name = paste0(tolower(wig_name),"_mv")
  wigdy_name = paste0(tolower(wig_name),"_dy")
  wigbv_name = paste0(tolower(wig_name),"_bv")
  wigpe = load_indices_loc(wigpe_name)
  wigpe = fillerxts(wigpe)
  wigpb = load_indices_loc(wigpb_name)
  wigpb = fillerxts(wigpb)  
  #download_stooq(wig_name)
  #download_stooq(wigpb_name)
  #download_stooq(wigpe_name)
  #download_stooq(wigmv_name)
  #download_stooq(wigdy_name)
  #download_stooq(wigbv_name)
}#stocks

  index_fundamental<-function(){
  pe1 = read.csv(paste0(data_folder,"/pe1.csv"),sep="\t")
  pe10 = read.csv(paste0(data_folder,"/pe10.csv"),sep="\t") #https://www.quandl.com/data/MULTPL/SHILLER_PE_RATIO_MONTH
  pe10$Date=as.Date(as.character(pe10$Date),format="%m/%d/%Y");pe10=ts2xts(pe10)
  pe1$Date=as.Date(as.character(pe1$Date),format="%d/%m/%Y");pe1=ts2xts(pe1)
  vix_m = to.monthly(ts2xts(read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=VIXCLS")))[,4]
  fin_d = ts2xts(read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=T10Y3M"))
  t10y3m = to.monthly(fin_d)[,4]
  t10y2y = to.monthly(ts2xts(read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=T10Y2Y")))[,4]
  t10y =  to.monthly(ts2xts(read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=DGS10")))[,4]
  t2y =  to.monthly(ts2xts(read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=DGS2")))[,4]
  ty_d = local_load('zn_f')
  ty = to.monthly(137-local_load('zn_f'))[,4]
  ed_d = local_load('ge_f')
  ed = to.monthly(100-local_load('ge_f'))[,4]
  ts = load_indices_loc(c('ukousdon','ukousd1w','ukousd1m','ukousd2m','ukousd3m','ukousd6m'))/100
  fwd3m = 1/(4.5/12 - 1.5/12) * ( (1+(ts$ukousd6m/2+ts$ukousd3m/2)*4.5/12)/(1+(ts$ukousd2m/2+ts$ukousd1m/2)*1.5/12) -1)
  edcost = fwd3m - ts$ukousd1w
  ed1 = load_futures(c('CME_ED'),1)
  ed2 = load_futures(c('CME_ED'),2)
  summary(abs(ed1-ed_d))
  ts_m = mat.to.monthly(ts)
  
  tyc = diff(log(ty_d)) - log(1+fin_d/100)/252
  tyc = xts(exp(cumsum(na20(tyc))),index(tyc))
  tyc[which(tyc==1)] = NA
  tyc_m = mat.to.monthly(tyc)
  tyc_mret = diff(log(tyc_m))
  
  edc = diff(log(ed_d)) - log(1+edcost)/252
  edc = xts(exp(cumsum(na20(edc))),index(edc))
  edc[which(edc==1)] = NA
  edc_m = mat.to.monthly(edc)
  edc_mret = diff(log(edc_m))
  
  #fut_carry_d = -log(fut_d2/fut_d)*fut_series
  
  set = cbind(t10y2y,t10y3m,out,diff(out),t10y,ty,ed,tyc_m,tyc_mret,edc_m,edc_mret,edcost,ts$ukousd1w)
  colnames(set) = c('tsn','tsb','logeq','ret','t10y','ty','ed','tyc','tycret','edc','edcret','edcost','libor')
  set = set['1997-01-01/']
  lm1 = lm(logeq~tsb+tsn+ty+ed+tyc,data=set); summary(lm1)
  lm1 = lm(logeq~ty+tyc,data=set); summary(lm1)
  lm2 = lm(ret~tycret+edcret,data=set); summary(lm2)
  
  round(cor(cbind(set$logeq,set$tsb,set$tsn,set$tyc,set$edc,set$edcost,set$t10y,set$libor),use="complete.obs"),3)
  round(cor(cbind(set$ret,set$tycret,set$edcret),use="complete.obs"),3)
  
  summary(set$logeq)
  plot(out['1998-01-01/']/abs(as.numeric(out['1998-01-01'])))
  plot((exp(out['1998-01-01/'])+0.2)*2/3+tyc_m['1998-01-01/']/3)
  tescik = (exp(out['1998-01-01/'])+0.2)*4/5+edc_m['1998-01-01/']/5
  plot(tescik)
  SR(diff(log(tescik)))
  SR(diff(out['1998-01-01/']))
  
  
  plot(ts(cumsum(set$tsb-0.5)))
  plot(set$ty-set$ed)
  plot(set$tsb)
  plot(-set$ty)
  plot(-set$t10y)
  plot(edc_m['1998-01-01/'])
  plot(ed['1998-01-01/'])
  plot(tyc_m['1998-01-01/'])
  plot(set$tsn['1998-01-01/'])
  plot(fwd3m-ts$ukousd3m)
  plot(edcost['1998-01-01/'])
  
  #plot
  #l6 = .0163171
  #l3 = .0143567
  #l2 = .0137944
  #l1 = .0128267
  #lon = .0118278 
  #l4_5 = l6/2+l3/2
  #l1_5 = l2/2+l1/2
  #l3x6 = 1/(0.5-0.25) * ( (1+l6*0.5)/(1+l3*0.25) -1)
  #l1_5x4_5
  #1/(4.5/12 - 1.5/12) * ( (1+l4_5*4.5/12)/(1+l1_5*1.5/12) -1) - lon
  
  #why does fx carry does not work sometimes
  
  plot(cumsum(na20(tt)))
  plot(ts(cumsum(na20(rowMeans(indices_rates['1997-01-01/'])))))
  plot(ts((na20(rowMeans(-carry_m['1997-01-01/'])))))
  SR(tt)
  
  tt=cbind(a,b,i5d)
  tt=xts(log(rowMeans(exp(tt),na.rm=T)),index(tt))
  
  meanCarry = xts(rowMeans(-carry_m,na.rm=T),index(carry_m))
  set = cbind(cumsum(na20(tt)),tt,meanCarry,t10y2y,t10y3m,t10y,ty,ed,tyc_m,tyc_mret,edc_m,edc_mret,edcost,ts$ukousd1w)
  colnames(set) = c('logeq','ret','mc','tsn','tsb','t10y','ty','ed','tyc','tycret','edc','edcret','edcost','libor')
  set = set['1997-01-01/']
  lm1 = lm(ret~t10y+mc-1,data=set); summary(lm1)
  
  plot(histcorr(t2d,t4d,3))
  SR(pmax(t2d,levret(2,t4d)),12)
  SR(pmax(levret(2,a),c),12)
  SR(c/2+a,12)
  plot(cumsum(na20(pmax(levret(2.2,a),c))))
  z = (lag(c - levret(2,a),-1))>0
  lz = (c - levret(2,a))>0
  lzval = c - levret(2,a)
  usindpro= indpro[,1]
  uscpi = diff(log(indcpi[,1]),12)
  plot(usindpro)
  usindpot=indpot[,1]
  usrate = indices_rates[,1]
  reces = indpot[,1]<0
  sa = a>0
  sc = c>0
  
  set = cbindname(z,lz,lzval,sa,sc,t10y,t10y3m,t10y2y,t2y,vix_m,usindpot,usindpro,uscpi,ed,ty,usrate)
  summary(glm(z~t2y+ed-1,data=set,family="binomial"))
  tt = 0.27*ed + 13.11*usindpro/ + 0.67*t10y2y - 10.6
  plot(ed-t2y)
  SR(simpledominance(t2d,levret(2,t4d),!lag(st2d)),12)
  SR(t2d+levret(2,t4d),12)
  SR(ifelse(lag(t2y-ed)<0,c,levret(2,a)),12)
}#index_fundamentals testing
  
