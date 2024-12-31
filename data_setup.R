source("core_lib.R")

old_setup <- function(){
  rate_names=c("USD3MTD156N","EUR3MTD156N","GBP3MTD156N","IR3TIB01CAM156N","IR3TIB01AUM156N","IR3TIB01NZM156N","JPY3MTD156N","CHF3MTD156N","IR3TIB01NOM156N","IR3TIB01SEM156N","IR3TIB01DKM156N","IR3TIB01PLM156N","IR3TIB01CZM156N","IR3TIB01HUM156N","IR3TIB01RUM156N","IR3TIB01ZAM156N","IR3TIB01MXM156N","IR3TIB01TRM156N","IR3TIB01CNM156N","IR3TIB01CLM156N","IR3TIB01ILM156N","INTDSRBRM193N","INTDSRINM193N","IR3TIB01KRM156N","IR3TIB01IDM156N")
  indices_names=c("^spx","^cac","^ukx","^tsx","^aor","^nz50","^nkx","^smi","^oseax","^omxs","^OMXC20","wig20","^px","^bux","^rts","^JSE","^ipc","^xu100","^shc","^ipsa","T25.TA","^bvp","^snx","^kospi","^jci")
  gdp_names=c("USAGDPNQDSMEI","NAEXCP01EZQ189S","GBRGDPNQDSMEI","CANGDPNQDSMEI","AUSGDPNQDSMEI","NZLGDPNQDSMEI","JPNGDPNQDSMEI","CHEGDPNQDSMEI","NORGDPNQDSMEI","SWEGDPNQDSMEI","DNKGDPNQDSMEI","POLGDPNQDSMEI","CZEGDPNQDSMEI","HUNGDPNQDSMEI","RUSGDPNQDSMEI","ZAFGDPNQDSMEI","MEXGDPNQDSMEI","TURGDPNQDSMEI","CHNGDPNQDSMEI","CHLGDPNQDSMEI","ISRGDPNQDSMEI","BRAGDPNQDSMEI","INDGDPNQDSMEI","KORGDPNQDSMEI","IDNGDPNQDSMEI")
  m3_names=c("MABMM301USM189S","MYAGM3EZM196N","MABMM301GBM189S","MABMM301CAM189S","MABMM301AUM189S","MABMM301NZM189S","MABMM301JPM189S","MABMM301CHM189S","MABMM301NOM189S","MABMM301SEM189S","MABMM301DKM189S","MABMM301PLM189S","MABMM301CZM189S","MABMM301HUM189S","MABMM301RUM189S","MABMM301ZAM189S","MABMM301MXM189S","MABMM301TRM189S","MABMM301CNM189S","MABMM301CLM189S","MABMM301ILM189S","MABMM301BRM189S","MABMM301INM189S","MABMM301KRM189S","MABMM301IDM189S")
  gdpMultipl=c(10^9,1,rep(10^9,22),10^12,10^12)
  gdp1998Q4=rbind(gdp1998Q4,c(606.069/4,"PLN"))
  gdp1998Q4=rbind(gdp1998Q4,c(2629.6/4,"RUB"))
  tmp$index = as.numeric(as.character(tmp$index)) / as.numeric(as.character(tmp$index[1])) * subset(gdp1998Q4,ccy==ccys[i])[,1]*subset(gdpMultipl,ccy==ccys[i])[,1] #normalisation by first value then by gdp for YE1998
  all.nonxts$factor = all.nonxts$gdp/(all.nonxts$cpi*all.nonxts$m3)
  #^spx"\tDTB3\tUSAGDPNQDSMEI"\tMABMM301USM189S
  #"^dax"\tEUR3MTD156N\tNAEXCP01EZQ189S\tMYAGM3EZM196N
  #"^ukx"\tGBP3MTD156N\tGBRGDPNQDSMEI\tMABMM301GBM189S
  #^aor\tIR3TIB01AUM156N\tAUSGDPNQDSMEI\tMABMM301AUM189S
  #^nkx\tJPY3MTD156N\tJPNGDPNQDSMEI\tMABMM301JPM189S
  #^smi\tCHF3MTD156N\tCHEGDPNQDSMEI\tMABMM301CHM189S
  #"^omxs"\tIR3TIB01SEM156N\tSWEGDPNQDSMEI\tMABMM301SEM189S
  #wig20\tIR3TIB01PLM156N\tPOLGDPNQDSMEI\tMABMM301PLM189S
  #^px\tIR3TIB01CZM156N\tCZEGDPNQDSMEI\tMABMM301CZM189S
  #^bux\tIR3TIB01HUM156N\tHUNGDPNQDSMEI\tMABMM301HUM189S
  #^rts\tIR3TIB01RUM156N\tRUSGDPNQDSMEI\tMABMM301RUM189S
  #^ipc\tIR3TIB01MXM156N\tMEXGDPNQDSMEI\tMABMM301MXM189S
  #^bvp\tINTDSRBRM193N\tBRAGDPNQDSMEI\tMABMM301BRM189S
  #^kospi\tIR3TIB01KRM156N\tKORGDPNQDSMEI\tMABMM301KRM189S
  #^tsx\tIR3TIB01CAM156N\tCANGDPNQDSMEI\tMABMM301CAM189S
  #indices_names0 = c("^spx", "^STOXX50E", "^dax", "^cac", "^ukx","^aor","^nkx","^smi","OBX","^omxs","^OMXC20","wig20","^px","^bux","^rts","^ipc","^bvp","^NSEI","^kospi","^hsi", "^HSCE", "^fmib", "^aex", "^psi20", "^ibex","^tsx","FTNFIX","^BFX","^dji","^ndq","RU2000PR")
  #bpsspread0=c(3,7,1,2,3,9,16,6,41,50,52,7,24,18,15,32,21,6,2,9,20,8,5,31,7,50,50,50,2,2,2) #spread in bps
  #rates_names0=c("DTB3","EUR3MTD156N","EUR3MTD156N","EUR3MTD156N","GBP3MTD156N","IR3TIB01AUM156N","JPY3MTD156N","CHF3MTD156N","IR3TIB01NOM156N","IR3TIB01SEM156N","IR3TIB01DKM156N","IR3TIB01PLM156N","IR3TIB01CZM156N","IR3TIB01HUM156N","IR3TIB01RUM156N","IR3TIB01MXM156N","INTDSRBRM193N","INTDSRINM193N","IR3TIB01KRM156N","HKD_IR","HKD_IR","EUR3MTD156N","EUR3MTD156N","EUR3MTD156N","EUR3MTD156N","IR3TIB01CAM156N","EUR3MTD156N","EUR3MTD156N","USD3MTD156N","USD3MTD156N","USD3MTD156N")
  #ccys_names0 =  c("USD",  "EUR",  "EUR", "EUR", "GBP", "AUD",  "JPY",  "CHF", "NOK",  "SEK",  "DKK", "PLN",  "CZK",  "HUF",  "RUB",  "MXN", "BRL", "INR", "KRW", "HKD", "HKD", "EUR","EUR","EUR","EUR","CAD","EUR","EUR","USD","USD","USD")
}#old
indices_setup <- function(){
  #indices_names <<- c("^AXJO","^ATX","^bvp","^HSCE","^px","^dax","feu50","^cac","^hsi","^fmib","^nkx","KOSP200","^ipc","^aex","^rts","^ibex","^smi","^ukx","^ndx","^RUT","^dji","^spx","wig20")
  #indices_spreads <<- c(5.6,58.2,9.1,18.0,24.2,0.6,3.9,1.7,5.6,4.4,7.3,6.9,23.2,3.3,15.5,6.7,3.4,2.4,0.7,1.8,0.6,1.2,7.3)

  indices_names <<- c("^spx", "^dax","^cac", "^ukx","^aor","^nkx","^smi","^oseax","^omxs","wig20","^px","^bux","^rts","^ipc","^bvp","^kospi","^hsi","^fmib", "^aex", "^psi20", "^ibex","MX_SXF","^dji","^ndq","rj.f","fx.f","^HSCE")
  indices_spreads <<- c(3,1,2,3,9,16,6,41,50,7,24,18,15,32,21,2,9,8,5,31,7,50,2,2,4,9)#,16)
  #EUR3MTD156N
  indices_rates_names <<- c("DTB3","ukoeur3m","ukoeur3m","GBP3MTD156N","IR3TIB01AUM156N","JPY3MTD156N","CHF3MTD156N","IR3TIB01NOM156N","IR3TIB01SEM156N","IR3TIB01PLM156N","IR3TIB01CZM156N","IR3TIB01HUM156N","IR3TIB01RUM156N","IR3TIB01MXM156N","INTDSRBRM193N","IR3TIB01KRM156N","HKD_IR","ukoeur3m","ukoeur3m","ukoeur3m","ukoeur3m","IR3TIB01CAM156N","USD3MTD156N","USD3MTD156N","USD3MTD156N","ukoeur3m")#,"HKD_IR")
  ip_names <<- c("INDPRO","DEUPROINDMISMEI","FRAPROINDMISMEI","IPIUKM","AUSPROINDQISMEI","JPNPROINDMISMEI", "CHEPROINDQISMEI","NORPROINDMISMEI","SWEPROINDMISMEI","POLPROINDMISMEI","CZEPROINDMISMEI","HUNPROINDMISMEI", "RUSPROINDMISMEI","PRINTO02MXQ661S","BRAPROINDMISMEI","KORPROINDMISMEI","null","ITAPROINDMISMEI","NLDPROINDMISMEI","PRTPROINDMISMEI","ESPPROINDMISMEI","CANPROINDMISMEI","null","null","null","null")#,"null")
  cpi_names <<- c("CPALTT01USM661S","DEUCPIALLMINMEI","FRACPIALLMINMEI","GBRCPIALLMINMEI","AUSCPIALLQINMEI","JPNCPIALLMINMEI","CHECPIALLMINMEI","NORCPIALLMINMEI","SWECPIALLMINMEI","POLCPIALLMINMEI","CZECPIALLMINMEI","HUNCPIALLMINMEI","RUSCPIALLMINMEI","MEXCPIALLMINMEI","BRACPIALLMINMEI","KORCPIALLMINMEI","null","ITACPIALLMINMEI","NLDCPIALLMINMEI","PRTCPIALLMINMEI","ESPCPIALLMINMEI","CANCPIALLMINMEI","null","null","null","null")#,"null")
  
  pb = c('.AEX','.BUX .BVSP .DJI  .FTSE .GDAXI  .HSCE .HSI  .IBEX .IRTS .KS200  .N225 .NDX  .NSEI .SPX  .SSEA .SSMI .STOXX50E .WIG20')
  #download_stooq(indices_names)
  #download_quandl("MX_SXF")
  indices <<- load_indices_loc((indices_names))
  indices <<- fillerxts(indices)
  indices <<- fillerxtsNON0(indices)
  indiceso <<- load_indices_loc((indices_names),col=1)
  #indiceso <<- lag(indiceso,-1)
  indiceso <<- fillerxts(indiceso)
  indiceso <<- fillerxtsNON0(indiceso)
  indices_w <<- mat.to.weekly(indices)
  indiceso_w <<- mat.to.weekly(indiceso)
  indices_m <<- mat.to.monthly(indices)
  indicesSMA <<- indices
  for(i in 1:dim(indices)[2]) indicesSMA[,i] <<- SMA(indices[,i],10)
  indicesSMA_m <<- mat.to.monthly(indicesSMA)
  
  indices_rates <<- load_rates(indices_rates_names)
  colnames(indices_rates) <- indices_names
  indices_rates <<- fillerxts(indices_rates)
  indices_rates_d <<- lag(indices_rates)
  indices_rates_d <<- merge(indices[,1], xts(coredata(indices_rates_d), as.Date(index(indices_rates_d))-1), join="left")[,-1]
  indices_rates_d <<- fillerxts(indices_rates_d)
  indices_rates_w <<- mat.to.weekly(indices_rates_d)
  
  ind_carry <<- xts(matrix(rep(-rates[,1],dim(indices)[2]),dim(rates)[1],dim(indices)[2]),index(rates))#+0.015
  indices_rates_d <<- dailization(indices_rates,indices)
  
  futind_d <<- add_daily_swaps(indices,-indices_rates_d)
  futind_m <<- mat.to.monthly(futind_d)
  
  indcpi <<- load_rates(cpi_names)
  indcpi <<- fillerxts(indcpi)
  indcpi[which(indcpi[,15]<0.000001),15] <<- NA
  indcpi_d <<- lag(dailization(indcpi,indices),21)
  
  indpro <<- load_rates(ip_names[1:22])
  indpro <<- fillerxts(indpro)
  #indpro = indpro[,1:22]["1993-01-01/"]
  indpro_d <<- lag(dailization(indpro,indices),21)
  realindpro <<- sweep(indpro,2,indpro['2010-6'],"/")/sweep(indcpi,2,indcpi['2010-6'],"/")[,1:22]
  indpot <- indpro
  realpot <<- realindpro
  for(i in 1:22) {
    #indpot[,i] <<- avg2Filter(indpro[,i],24,96)
    if(i!=17) indpot[,i] <- log(indpro[,i]) - SMA(log(indpro[,i]),6*12)
    #realpot[,i] <<- avg2Filter(realindpro[,i],24,96)
  }
  
  rind_m <<- log(sweep(indices_m,2,indices_m['2010-6'],"/"))[,1:22]-log(lag(lag(sweep(indpro,2,indpro['2010-6'],"/"))))
  rind_d <<- log(sweep(indices,2,indices['2010-06-01'],"/"))-log(sweep(indpro_d,2,indpro_d['2010-06-01'],"/"))
  
  realind_m <<- rind_m + log(lag(lag(sweep(indcpi,2,indcpi['2010-6'],"/"))))
  realind_d <<- rind_d + log(sweep(indcpi_d,2,indcpi_d['2010-06-01'],"/"))
  profit <<- lag(lag(diff(indpro)))
  profit12 <<- lag(lag(diff(indpro,lag=12)))
  #carry12(inds2_m,ind2_spreads/4,lag(indpro),0,1,3,360)
  #carry12(inds2_m,ind2_spreads/4,rind,0,1,3,360)
  cashind_names = c("^aor","^bel20","^STOXX50E","^OMXH25","^SBF120","^cac","^dax","^mdax","^tdxp","FTSE.AT","^aex","^hsi","^fmib","^nkx","wig20","^ibex","^smi","^ukx","^ftm","rj.c","^dji","^ndq","^spx")
  cashind_spreadpoints = c(2.41,6.71,1.61,7.33,4.71,1.34,1.64,12.77,4.06,1.96,0.36,19.61,11.7,8.39,2.24,5.44,3.07,1.72,19.08,1.14,1.92,1.42,1.08)
  cashind_rates_names = c("IR3TIB01AUM156N","ukoeur3m","ukoeur3m","IR3TIB01FIM156N","ukoeur3m","ukoeur3m","ukoeur3m","ukoeur3m","ukoeur3m","ukoeur3m","ukoeur3m","HKD_IR","ukoeur3m","JPY3MTD156N","IR3TIB01PLM156N","ukoeur3m","CHF3MTD156N","GBP3MTD156N","GBP3MTD156N","USD3MTD156N","USD3MTD156N","USD3MTD156N","USD3MTD156N")  
  cashind_ccy_names = c("USDAUD","USDEUR","USDEUR","null","USDEUR","USDEUR","USDEUR","USDEUR","USDEUR","USDEUR","null","USDHKD","USDEUR","USDJPY","USDPLN","null","USDCHF","USDGBP","USDGBP","null","null","null","null")
  cashind_ccy_names = c("USDAUD","null","USDEUR","null","null","null","null","null","null","null","null","USDHKD","null","USDJPY","USDPLN","null","USDCHF","null","USDGBP","null","null","null","null")
  cashindip_names = c("AUSPROINDQISMEI","BELPROINDMISMEI","null","FINPROINDMISMEI","FRAPROINDMISMEI","FRAPROINDMISMEI","DEUPROINDMISMEI","DEUPROINDMISMEI","null","GRCPROINDMISMEI","NLDPROINDMISMEI","null","ITAPROINDMISMEI","JPNPROINDMISMEI", "POLPROINDMISMEI","ESPPROINDMISMEI","CHEPROINDQISMEI","IPIUKM","IPIUKM","INDPRO","INDPRO","null","INDPRO")
}#indices
ccy_setup <- function(){
  ccy_names <<- c("USDEUR","USDPLN","USDGBP","USDJPY","USDCHF","USDCAD","USDAUD","USDNZD","USDNOK","USDDKK","USDSEK","USDCZK","USDHUF","USDRUB","USDZAR","USDMXN","USDTRY","USDILS","USDCLP")
  ccy_spreads <<- c(1,7,2,1,3,2,2,3,5,6,2,13,14,7,15,11,6,9,30)
  ccy_rates <<- c("DTB3","ukoeur3m","IR3TIB01PLM156N","GBP3MTD156N","JPY3MTD156N","CHF3MTD156N","IR3TIB01CAM156N","IR3TIB01AUM156N","IR3TIB01NZM156N","IR3TIB01NOM156N","IR3TIB01DKM156N","IR3TIB01SEM156N","IR3TIB01CZM156N","IR3TIB01HUM156N","IR3TIB01RUM156N","IR3TIB01ZAM156N","IR3TIB01MXM156N","IR3TIB01TRM156N","IR3TIB01ILM156N","IR3TIB01CLM156N")
  #ccy_type <<- c(-1,1,-1,1,1,1,-1,-1,1,1,1,1,1,1,1,1,1,1,1)
  cpi_names <<- c("CPALTT01USM661S","CPHPTT01EZM661N","POLCPIALLMINMEI","GBRCPIALLMINMEI","JPNCPIALLMINMEI","CHECPIALLMINMEI","CANCPIALLMINMEI","AUSCPIALLQINMEI","NZLCPIALLQINMEI","NORCPIALLMINMEI","DNKCPIALLMINMEI","SWECPIALLMINMEI","CZECPIALLMINMEI","HUNCPIALLMINMEI","RUSCPIALLMINMEI","ZAFCPIALLMINMEI","MEXCPIALLMINMEI","TURCPIALLMINMEI","ISRCPIALLMINMEI","CHLCPIALLMINMEI")
  cpi_names <<- c("CPALTT01USM661S","CP0000EZ19M086NEST","POLCPIALLMINMEI","GBRCPIALLMINMEI","JPNCPIALLMINMEI","CHECPIALLMINMEI","CANCPIALLMINMEI","AUSCPIALLQINMEI","NZLCPIALLQINMEI","NORCPIALLMINMEI","DNKCPIALLMINMEI","SWECPIALLMINMEI","CZECPIALLMINMEI","HUNCPIALLMINMEI","RUSCPIALLMINMEI","ZAFCPIALLMINMEI","MEXCPIALLMINMEI","TURCPIALLMINMEI","ISRCPIALLMINMEI","CHLCPIALLMINMEI")
  m3_names <<- c("MABMM301USM189S","MYAGM3EZM196N","MABMM301PLM189S","MABMM301GBM189S","MABMM301JPM189S","MABMM301CHM189S","MABMM301CAM189S","MABMM301AUM189S","MABMM301NZM189S","MABMM301NOM189S","MABMM301DKM189S","MABMM301SEM189S","MABMM301CZM189S","MABMM301HUM189S","MABMM301RUM189S","MABMM301ZAM189S","MABMM301MXM189S","MABMM301TRM189S","MABMM301ILM189S","MABMM301CLM189S")
  gdp_names <<- c("USAGDPNQDSMEI","NAEXCP01EZQ189S","GBRGDPNQDSMEI","AUSGDPNQDSMEI","JPNGDPNQDSMEI","CHEGDPNQDSMEI","SWEGDPNQDSMEI","POLGDPNQDSMEI","CZEGDPNQDSMEI","HUNGDPNQDSMEI","RUSGDPNQDSMEI","MEXGDPNQDSMEI","BRAGDPNQDSMEI","KORGDPNQDSMEI","CANGDPNQDSMEI")
  cip_names <<- c("INDPRO","DEUPROINDMISMEI","POLPROINDMISMEI","IPIUKM","JPNPROINDMISMEI","CHEPROINDQISMEI","CANPROINDMISMEI","AUSPROINDQISMEI","NZLPROINDQISMEI","NORPROINDMISMEI","DNKPROINDMISMEI","SWEPROINDMISMEI","CZEPROINDMISMEI","HUNPROINDMISMEI","RUSPROINDMISMEI","null","PRINTO02MXQ661S","TURPROINDMISMEI","ISRPROINDMISMEI","CHLPROINDMISMEI")
  
  #ccy2_names <<- c("dx.f","USDPLN","EURUSD","GBPUSD","USDJPY","USDCHF","USDCAD","AUDUSD","NZDUSD","USDNOK","USDDKK","USDSEK","USDCZK","USDHUF","USDRUB","USDMXN","USDTRY","USDILS","USDCLP")
  #ccy2_spreads <<- c(3,7,1,2,1,3,2,2,3,5,6,2,13,14,7,11,6,9,30)
  #ccy2_type <<- c(-1,1,-1,-1,1,1,1,-1,-1,1,1,1,1,1,1,1,1,1,1)
  
  download_stooq(ccy_names)
  download_stooq("ukoeur3m")
  ccys <<- load_indices_loc(tolower(ccy_names))
  ccyso <<- load_indices_loc(tolower(ccy_names),1)
  ccysh <<- load_indices_loc(tolower(ccy_names),2)
  ccysl <<- load_indices_loc(tolower(ccy_names),3)
  ccys <<- fillerxts(ccys)
  ccysh <<- fillerxts(ccysh,ccys)
  ccysl <<- fillerxts(ccysl,ccys)
  ccyso <<- fillerxts(ccyso,ccys)
  ccys_m <<- mat.to.monthly(ccys)
  ccys_w <<- mat.to.weekly(ccys)
  m3 <<- load_rates(m3_names)
  m3 <<- m3[,-1]
  rm3 <<- lag(diff(log(m3),lag=12))
  
  cpi <<- load_rates(cpi_names)
  cpi <<- fillerxts(cpi)
  #rcpi <<- lag(diff(log(cpi[,-1]),lag=12))
  cpi <<- sweep(cpi,2,cpi['1996-1'],"/")
  lcpi <<- log(cpi)
  ycpi <<- lag(diff(lcpi[,-1],lag=12))
  rcpi <<- sweep(lcpi[,-1], 1, -lcpi[,1], FUN = "+")
  #rcpi <<- sweep(rcpi,2,ccy_type, FUN = "*")
  rccys <<- log(sweep(ccys_m,2,ccys_m['1996-1'],"/"))+lag(rcpi)
  fundrev <<- diff(log(ccys_m),lag=12)+lag(diff(rcpi,lag=12))
  meanrev <<- -(ccys/mat.SMA(ccys,5*252)-1)/5
  meanrev_m <<- mat.to.monthly(meanrev)
  
  rates <<- load_rates(ccy_rates)
  rates <<- fillerxts(rates)
  
  carry_m <<- sweep(-rates[,-1], 1, rates[,1], FUN = "+")
  colnames(carry_m) <<- ccy_names
  #carry_m <<- sweep(carry_m,2,ccy_type, FUN = "*")
  carry_d <<- lag(carry_m)
  carry_d <<- merge(ccys[,1], xts(coredata(carry_d), as.Date(index(carry_d))-1), join="left")[,-1]
  carry_d <<- fillerxts(carry_d)
  carry_w <<- mat.to.weekly(carry_d)
  
  is.carry <<- (abs(carry_m/12)-0.01/12-ccy_spreads/10000) > 0
  cor.carry <<- carry_m
  for(i in 1:19) cor.carry[which(!is.carry[,i]),i] <- NA
  cor.carry <<- cor.carry
  crit.carry <<- carry_m/12-sign(carry_m)*ccy_spreads/10000
  
  futccy_d <<- diff(log(ccys)) + carry_d/252 #- indices_rates_d*2.4/252-indices_spreads*12/252
  for(i in 1:dim(ccys)[2]) futccy_d[,i] <- xts(exp(cumsum(na20(futccy_d[,i]))),index(ccys))
  for(i in 1:dim(ccys)[2]) futccy_d[which(futccy_d[,i]==1),i] <- NA
  futccy_d <<- futccy_d
  futccy_m <<- mat.to.monthly(futccy_d)
  futccy_w <<- mat.to.weekly(futccy_d)
  
  ccys2 <<- load_indices_loc(tolower(ccy2_names))
  ccys2 <<- fillerxts(ccys2)
  ccys2_m <<- mat.to.monthly(ccys2)
  carry2 <<- cbind(rep(0,dim(carry)[1]),carry[,-15])
  colnames(carry2) <<- ccy2_names
  
  cindpro <<- load_rates(cip_names)
  cindpro <<- fillerxts(cindpro)
  ycip <<- lag(diff(log(cindpro[,-1]),lag=12))
  rcip <<- sweep(log(cindpro)[,-1], 1, -log(cindpro)[,1], FUN = "+")
  #rcip <<- sweep(rcpi,2,ccy_type, FUN = "*")
  rcind <<- log(sweep(ccys_m,2,ccys_m['2010-6'],"/"))-log((lag(cindpro[,-1])))
  rcind2 <<- log(sweep(ccys_m,2,ccys_m['1996-1'],"/"))+lag(rcip)
  # cyclical component of a trending series, defined as the difference between the value at date
  # t + h and the value that we would have expected based on its behavior through date t. Thus,
  # he suggests estimating by OLS a regression of output at time t + h on a constant and four lags
  # of output as available at time t and then using the residuals to remove the trend; we follow
  # Hamilton (2016) and set h equal to 24
  cindpot <<- cindpro[,-1]
  for(i in 2:20) {
    if(colnames(cindpot)[i-1]!="null") cindpot[,i-1] <- rolltrend(cindpro[,i])
  }
  
  cindpot2 <<- cindpro[,-1]
  for(i in 2:20) {
    if(colnames(cindpot2)[i-1]!="null") cindpot2[,i-1] <- avg2Filter(cindpro[,i],2,60)
    else  cindpot2[,i] = rep(NA,dim(cindpot2)[1])
  }
  
  ccysind_names <<- c("USDEUR","USDPLN","USDGBP","USDJPY","USDCHF","USDCAD","USDAUD","USDSEK","USDCZK","USDHUF","USDRUB","USDMXN","USDNZD")
  ccysind_nums <<- c(1:8,11:14,16)
  indccys_spreads <<- c(1,7,3,16,6,50,9,50,24,18,15,32,20)
  indccys_names <<- c("^dax","wig20","^ukx","^nkx","^smi","^tsx","^aor","^omxs","^px","^bux","^rts","^ipc","^nz50")
  #download_stooq(unique(c(indccys_names,indices_names,findices_names)))
  #download_stooq(ccy_names[ccysind_nums])
  indccys <<- load_indices_loc(tolower(indccys_names))
  indccys <<- fillerxts(indccys)
  indccys_m <<- mat.to.monthly(indccys)
  indccys_w <<- mat.to.weekly(indccys)
  indccyso <<- load_indices_loc(tolower(indccys_names),1)
  indccyso <<- fillerxts(indccyso)
  #ccysind <<- ccys[,ccysind_nums]
  ccysind <<-load_indices_loc(tolower(ccy_names[ccysind_nums]))
  ccysind_carry_m <<- carry_m[,ccysind_nums]
  ccysind_spreads <<- ccy_spreads[ccysind_nums]
  c2i <<- diff(log(ccysind),lag=13)
  i2c <<- diff(log(indccys),lag=12)
}#ccys
indfut_setup<-function(){
  #futures on indices
  ind_names = c('CME_ES','CME_DJ','CME_ND','EUREX_FDAX','LIFFE_FCE','LIFFE_Z','CME_NK','HKEX_HSI','LIFFE_FTI','LIFFE_PSI','MX_SXF','EUREX_FESX')
  ind_spreads=c(3,2,2,1,2,3,16,9,5,31,50,1)
  ind_letters = character(12)
  names(ind_letters) = ind_names
  ind_letters['CME_ES'] = "H;M;U;Z"
  ind_letters['CME_DJ'] = "H;M;U;Z"
  ind_dates['CME_ND'] = "H;M;U;Z"
  ind_dates['EUREX_FDAX'] = "H;M;U;Z"
  ind_dates['LIFFE_FCE'] = "F;G;H;J;K;M;N;Q;U;V;X;Z"
  ind_dates['LIFFE_Z'] = "H;M;U;Z"
  ind_dates['CME_NK'] = "H;M;U;Z"
  ind_dates['HKEX_HSI'] = "H;M;U;Z"
  ind_dates['LIFFE_FTI'] = "F;G;H;J;K;M;N;Q;U;V;X;Z"
  #ind_dates['LIFFE_PSI'] = "H;M;U;Z"
  #ind_dates['MX_SXF'] = "H;M;U;Z"
  ind_dates['EUREX_FESX'] = "H;M;U;Z"
  ind_dates['CME_ASX'] = "H;M;U;Z"
  ind_dates['ICE_TF'] = "H;M;U;Z"
  
  #ind_dates['EUREX_FESC'] = "H;M;U;Z"
  ind_dates = character(12)
  names(ind_dates) = ind_names
  ind_dates['CME_ES'] = rolldates('CME_ES',ind_letters,1997,3,0) #SP for big-contract
  #ind_dates['CME_SP'] = rolldates('CME_SP',ind_letters,1982,2,0) #ES for e-mini
  ind_dates['CME_DJ'] = rolldates('CME_DJ',ind_letters,1998,1,0) #YM for e-mini
  #ind_dates['CME_YM'] = rolldates('CME_YM',ind_letters,2012,1,0) #DJ for big-contract
  ind_dates['CME_NQ'] = rolldates('CME_ES',ind_letters,1999,3,0) #ND for big-contract
  #ind_dates['CME_ND'] = rolldates('CME_ND',ind_letters,1998,1,0) #NQ for e-mini
  ind_dates['EUREX_FDAX'] = rolldates('EUREX_FDAX',ind_letters,1997,1,0)
  ind_dates['LIFFE_FCE'] = rolldates('LIFFE_FCE',ind_letters,1999,3,1)
  ind_dates['LIFFE_Z'] = rolldates('LIFFE_Z',ind_letters,1984,2,0)
  ind_dates['CME_NK'] = rolldates('CME_NK',ind_letters,1990,4,0)
  ind_dates['HKEX_HSI'] = rolldates('HKEX_HSI',ind_letters,1997,9,1)
  ind_dates['LIFFE_FTI'] = rolldates('LIFFE_FTI',ind_letters,2013,10,1)
  ind_dates['LIFFE_PSI'] = rolldates('LIFFE_PSI',ind_letters,2013,4,0)
  ind_dates['MX_SXF'] = rolldates('MX_SXF',ind_letters,2011,1,0)
  ind_dates['EUREX_FESX'] = rolldates('EUREX_FESX',ind_letters,1998,3,0)
  #ind_dates['EUREX_FESC'] = rolldates('EUREX_FESC',ind_letters,2016,1,0) #STOXX Banks
  ind_dates['ASX'] = rolldates('ASX',ind_letters,2013,4,0)
  ind_dates['TF'] = rolldates('TF',ind_letters,2007,1,0)
  ind_dates['FSMI'] = rolldates('FSMI',ind_letters,2013,3,0)
  
  #BR.69 - 14 - 12 - 14 - 16 - 17 - 13
  #CH.20 - - 15 - - 14 - - 13 - - 13
  #CN.40 23 26 27 24 23 26 24 28 26 23 27 20
  #ES.35 18 15 15 19 17 14 19 16 13 18 15 13
  #IT.40 - - 15 - - 14 - - 13 - - 13
  #PL.20 - - 15 - - 14 - - 13 - - 13
  #SE.30 18 15 15 19 17 14 19 16 13 18 15 13
  
  #CHRIS/EUREX_FSMI1
  #BITAL/FTSEMU2018
  
}#indices futures
comm_setup<-function(){
  fut_names = c('CME_GC','CME_SI','CME_PL','CME_PA','ca_','ah_','ni_','zs_','CME_CL','CME_NG','ICE_CC','ICE_KC','CME_C','ICE_CT','CME_S','ICE_SB','CME_W','CME_RR','CME_NG','CME_BZ')
  fut_spreads=c(4,26,59,64,40,58,56,47,5,56,15,8,28,8,35,14,47,36,5)
  fut_series=c(7,6,4,5,4,4,4,4,12,4,5,5,4,7,4,5,6,12,12)  
  
  fut_names = c('CME_GC','CME_SI','CME_PL','CME_PA','ca_','ah_','ni_','zs_','CME_BZ','CME_CL','CME_HH','ICE_CC','ICE_KC','CME_C','ICE_CT','CME_S','ICE_SB','CME_W','CME_RR')
  fut_spreads=c(4,26,59,64,40,58,56,47,5,5,36,56,15,8,28,8,35,14,47)
  fut_series=c(7,6,4,5,4,4,4,4,12,12,12,4,5,5,4,7,4,5,6)  
  
  #XTB setup, high carrycost!
  fut_names = c('CME_GC','CME_SI','CME_PL','CME_PA','ca_','ah_','ni_','zs_')
  fut_spreads=c(4,26,59,64,40,58,56,47)
  fut_series=c(7,6,4,5,4,4,4,4)
  futcost = 0.05
  
  #bossa setup, ok
  fut_names = c('CME_GC','CME_SI','CME_PL','CME_PA','CME_HG','CME_CL','ICE_CC','ICE_KC','CME_C','ICE_CT','CME_S','ICE_SB','CME_W','CME_RR','CME_TY','EUREX_FGBS','EUREX_FGBL','CME_NG','CBOE_VX')
  futspot_names = c('xauusd','xagusd','xptusd','xpdusd','ca_c.f')
  
  fut_spreads=c(4,26,59,64,40,5,56,15,8,28,8,35,14,47,3,3,3,56,31)
  fut_series=c(6,6,4,4,12,12,5,5,5,5,7,4,5,6,4,4,4,12,12)
  futcost = c(0,rep(0.01,3),rep(0,15))
  fut_costs = fut_spreads/10^4*fut_d['2017-01-03']
  fut_dirs = c(rep(1,17),-1,-1)
  
  #download_quandl(fut_names,1)
  #download_quandl(fut_names,2)
  fut_d = load_futures_loc(fut_names)
  fut_d = fillerxts(fut_d)
  fut_do = load_futures_loc(fut_names,1,1)
  fut_do = fillerxts(fut_do)
  fut_d2 = load_futures_loc(fut_names,2)
  fut_d2['/2006-03-30',2] = NA
  fut_d2['/2009-02-09',3] = NA
  fut_d2['/2006-03-30',4] = NA
  fut_d[,'CBOE_VX']['/2007-03-23'] = fut_d[,'CBOE_VX']['/2007-03-23']/10
  fut_do[,'CBOE_VX']['/2007-03-23'] = fut_do[,'CBOE_VX']['/2007-03-23']/10
  fut_d2[,'CBOE_VX']['/2007-03-23'] = fut_d2[,'CBOE_VX']['/2007-03-23']/10
  fut_d[,'CME_HG']['/2015-06'] = NA
  fut_d2[,'CME_HG']['/2015-06'] = NA
  fut_do[,'CME_HG']['/2015-06'] = NA
  fut_d2[,'CME_PA']['/2010-05'] = NA
  fut_d[,'CME_PA']['/2010-05'] = NA
  fut_do[,'CME_PA']['/2010-05'] = NA
  fut_d=fut_d[-dim(fut_d)[1],]
  fut_do=fut_do[-dim(fut_do)[1],]
  
  fut_d0 = fut_d
  fut_d20 = fut_d2
  fut_d = fut_d0[,c(1:6)]
  fut_d2 = fut_d20[,c(1:6)]
  fut_series=fut_series[c(1:6)]
  fut_spreads=fut_spreads[c(1:6)]
  futcost=futcost[c(1:6)]
  
  fut_letters = character(20)
  names(fut_letters) = c(fut_names,"EUREX_FVS")
  fut_letters["CME_GC"] = "G;J;M;Q;V;Z"
  fut_letters["CME_SI"] = "F;H;K;N;U;Z"
  fut_letters["CME_PL"] = "F;J;N;V"
  fut_letters["CME_PA"] = "H;M;U;Z"
  fut_letters["CME_HG"] = "F;G;H;J;K;M;N;Q;U;V;X;Z"
  fut_letters["CME_CL"] = "F;G;H;J;K;M;N;Q;U;V;X;Z"
  fut_letters["CME_C"] = "H;K;N;U;Z"
  fut_letters["CME_S"] = "F;H;K;N;Q;U;X"
  fut_letters["CME_W"] = "H;K;N;U;Z"
  fut_letters["ICE_CC"] = "H;K;N;U;Z"
  fut_letters["ICE_KC"] = "H;K;N;U;Z"
  fut_letters["ICE_CT"] = "H;K;N;V;Z"
  fut_letters["ICE_SB"] = "H;K;N;V"
  fut_letters["CME_RR"] = "F;H;K;N;U;X"
  fut_letters["CME_TY"] = "H;M;U;Z"
  fut_letters["EUREX_FGBS"] = "H;M;U;Z"
  fut_letters["EUREX_FGBL"] = "H;M;U;Z"
  fut_letters["CME_NG"] = "F;G;H;J;K;M;N;Q;U;V;X;Z"
  fut_letters["CBOE_VX"] = "F;G;H;J;K;M;N;Q;U;V;X;Z"
  fut_letters["EUREX_FVS"] = "F;G;H;J;K;M;N;Q;U;V;X;Z"
  fut_series=c(6,6,4,5,12,12,4,5,5,4,7,4,5,6,4,4,4,12,12,12)
  
  fut_dates = character(20)
  names(fut_dates) = c(fut_names,"EUREX_FVS")
  fut_dates["CME_GC"] = rolldates("CME_GC",fut_letters,1975,1,0)
  fut_dates["CME_SI"] = rolldates("CME_SI",fut_letters,1964,2,1)
  fut_dates["CME_PL"] = rolldates("CME_PL",fut_letters,1970,1,1)
  fut_dates["CME_PA"] = rolldates("CME_PA",fut_letters,1977,1,0)
  fut_dates["CME_HG"] = rolldates("CME_HG",fut_letters,1994,11,1)
  fut_dates["CME_CL"] = rolldates("CME_CL",fut_letters,1983,6,1)
  
  fut_dates["CME_C"] = rolldates("CME_C",fut_letters,1960,1,0)#''
  fut_dates["CME_S"] = rolldates("CME_S",fut_letters,1970,1,1)#''
  fut_dates["CME_W"] = rolldates("CME_W",fut_letters,1959,5,0)#''
  fut_dates["ICE_CC"] = rolldates("ICE_CC",fut_letters,1970,1,0)#''
  fut_dates["ICE_KC"] = rolldates("ICE_KC",fut_letters,1973,5,0)#''
  fut_dates["ICE_CT"] = rolldates("ICE_CT",fut_letters,1972,1,0)#''
  fut_dates["ICE_SB"] = rolldates("ICE_SB",fut_letters,1964,1,0)#''
  
  fut_dates["CME_RR"] = rolldates("CME_RR",fut_letters,1986,6,1)#''
  fut_dates["CME_TY"] = rolldates("CME_TY",fut_letters,1982,2,0)
  fut_dates["EUREX_FGBS"] = rolldates("EUREX_FGBS",fut_letters,1999,1,0)
  fut_dates["EUREX_FGBL"] = rolldates("EUREX_FGBL",fut_letters,2013,1,0)
  fut_dates["CME_NG"] = rolldates("CME_NG",fut_letters,1990,6,1)
  fut_dates["CBOE_VX"] = rolldates("CBOE_VX",fut_letters,2004,5,1)  
  fut_dates["EUREX_FVS"] = rolldates("EUREX_FVS",fut_letters,2013,9,7)  
  
  #USD.INDEX
  #GILT
  #OIL.BRENT 23 20 20 17 22 19 24 21 18 23 20 20
  #HEATINGOIL 18 15 20 17 17 19 18 16 18 18 15 19
  #GASOLINE 17 15 20 17 17 19 17 16 18 17 15 18
  
  #CATTLE 11 - 20 - 17 - 18 - 20 - 13 -
  #LEANHOGS 11 - 20 - 17 - 18 - 20 - 13 -
  #SOYMEAL 2 22 - 18 - 28 - - - - 19 -
  #SOYOIL 2 15 - 19 - 28 - - - - 19 -
  #SUGAR - 15 - 18 - 14 - - 18 - - -
  
  setwd("C:\\\\Users\\\\ml50928\\\\Downloads\\\\futures\\\\")
  setwd("/home/miklab/Downloads/")
  #write.csv(fut_dates,"fut_dates.csv")
  fut_dates = read.csv("fut_dates.csv", stringsAsFactors=FALSE, row.names = 1)
  
  newfut = allfutures(fut_names,fut_dates,fut_d,fut_d2,rep(0,19),rep(1,19))
  newfut = fillerxts(newfut)
  newfut3 = futaddrate(newfut,rates[,1])
  
  roll_costs = newfut - fut_d
  roll_rates = diff(roll_costs)/lag(fut_d)
  roll_rates_m = sweep(sign(mat.to.monthly(roll_rates,2)+mat.to.monthly(roll_rates,3))*mat.to.monthly(abs(roll_rates),2),2,fut_series,'*')
  roll_rates_ms = mat.SMA(mat.na20(roll_rates_m),3)
  roll_rates_w = sweep(sign(mat.to.weekly(roll_rates,2)+mat.to.weekly(roll_rates,3))*mat.to.weekly(abs(roll_rates),2),2,fut_series*4.33,'*')
  roll_rates_ws = mat.SMA(mat.na20(roll_rates_w),13)
  roll_rates_d = sweep(roll_rates,2,fut_series*21,'*') #lag(roll_rates*252,-1)
  
  fut_m = mat.to.monthly(fut_d)
  fut_d2f = fillerxts(fut_d2)
  fut_dsmoo = mat.SMA(fut_d,10)
  fut_d2smoo= mat.SMA(fillerxts(fut_d2),10)
  fut_carry_d = sweep(-log(fut_d2/fut_d),2,fut_series,'*')
  fut_carry_df = sweep(-log(fut_d2f/fut_d),2,fut_series,'*')
  fut_carry_dsmoo = sweep(-log(fut_d2smoo/fut_dsmoo),2,fut_series,'*')
  fut_carry_d = fut_carry_d[rowSums(is.na(fut_carry_d))!=ncol(fut_carry_d),]
  for(i in 1:dim(fut_carry_d)[2]) fut_carry_d[which(fut_carry_d[,i]==0),i] = NA
  fut_carry_d = fillerxts(fut_carry_d)
  fut_carry_m = mat.to.monthly(fut_carry_d)
  fut_carry_mf = mat.to.monthly(fut_carry_df)
  fut_carry_msmoo = mat.to.monthly(fut_carry_dsmoo)
  futvalue = fut_m - lag(mat.to.monthly(fut_dsmoo),60)
  
  futspot = load_indices_loc(futspot_names)
  futspot = fillerxts(futspot)
  futspot = cbind(futspot,fut_d[,6:14]) 
  futfwd = load_indices_loc('ca_3.f')
  tt = -log(futfwd[,1]/futspot[,5])*4
  fut_carry_d = cbind(fut_carry_d[,1:4],tt,fut_carry_d[,6:19])
  fut_carry_d = fillerxts(fut_carry_d)
  
  fut_carry_ds = mat.SMA(fut_carry_d,42)
  fut_carry_m = mat.to.monthly(fut_carry_ds)
  
  for (i in 1:dim(fut_d)[2]) print(c(fut_names[i],skewness(diff(log(newfut2[,i])),na.rm=T)[1]))
  newfut['1990-04-03']
  yy=tsavgall(fut_d['1997-07/'],232,fut_spreads,roll_rates_d['1997-07/'],futcost,e=1)
  yye = tsmomenh(yy,0.15)
  yye_val = fillerxts(cumulate(yy))
  SR(xts(rowMeans(yy,na.rm=TRUE),index(yy)),252)
  
  vcorr=1
  futstart = 3
  s=1:19
  fut_d0=fut_d
  fut_do0=fut_do
  roll_rates_m0=roll_rates_m
  roll_rates_ms0=roll_rates_ms
  fut_d = cbind(fut_d,v1s)
  fut_d=fillerxts(fut_d)
  fut_do = cbind(fut_do,lag(v1s))
  fut_do=fillerxts(fut_do)
  roll_rates_m=cbind(roll_rates_m,0)
  roll_rates_ms=cbind(roll_rates_ms,-roll_rates_ms[,19])
  fut_d=fut_d0
  fut_do=fut_do0
  roll_rates_m=roll_rates_m0
  roll_rates_ms=roll_rates_ms0
  rm(fut_d0,fut_do0,roll_rates_m0,roll_rates_ms0,v1s)
  
  
  f1d=momentum12d(fut_d[,s],(fut_do[,s]),fut_spreads[s]/4,roll_rates_m[,s],futcost[s],2,futstart,j=11,v=1,e=1);SR(f1d,12)
  f3d=carry12d(fut_d[,s],(fut_do[,s]),fut_spreads[s]/4,-roll_rates_ms[,s],roll_rates_m[,s],futcost[s],1,futstart,v=vcorr,e=1);SR(f3d,12)
  f3d=carry12d(futenh_d[,s],lag(futenh_d[,s]),fut_spreads[s]/4,fut_carry_m[,s],0,futcost[s],1,futstart,v=vcorr,e=1);SR(f3d,12)
  f4d=carry12d(newfut2,lag(newfut2),fut_spreads/4,futvalue,0,futcost,1,futstart,v=vcorr,e=1);SR(f4d,12)
  f5d = momentum12d(yye_val[,s],yye_valo[,s],fut_spreads[s]/4,0,0,3,2,j=2,v=vcorr,e=1);SR(f5d,12)
  SR(binding(cbind(f1d*2,f3d/2,f5d/2)),12)
  plot(cumsum(na20(binding(cbind(f1d*2,f3d)))))
  #wniosek: nie dodawac mod. strat. vix
  SR(levret(0.15,f1d['1998-01/']),12)
  
  colnames(f1d)=colnames(fut_d);  colnames(f3d)=colnames(fut_d)
  tt=abs(mat.02na(f1d['1998-01/']*2+f3d['1998-01/']))*fillerxts(roll_rates_m/roll_rates_m)
  tt=colMeans(mat.na20(tt),na.rm=T)
  tt[order(-tt)]/3
  
  tt=abs(mat.02na(i5d['1998-01/']+i10d['1998-01/']))
  colnames(tt)=indices_names
  tt=colMeans(mat.na20(tt),na.rm=T)
  tt[order(-tt)]/2
  
  momentum12(fut_d,fut_spreads/4,fut_carry_m,0,-1,3,start,j=11,e=0)
  momentum12(fut_d,fut_spreads/4,fut_carry_mf,0,-1,3,start,j=11,e=0)
  momentum12(fut_d,fut_spreads/4,fut_carry_msmoo,0,-1,3,start,j=11,e=0)
  momentum12(futenh_d,fut_spreads/4,0,0,-1,3,start,j=11,e=0)
  carry12(fut_d,fut_spreads/4,-fut_carry_m,fut_carry_m,0,-1,2,start,e=0)
  
  futstart=457
  futcost=0
  futcost=0.01
  futcost=fut_spreads*12/10000
  
  tt=cbind(f1,f1b,f3,f3b,f4,f4b)
  pp=xts(cumsum(na20(rowMeans(tt,na.rm=TRUE))),index(tt))
  plot(pp)  
  
  #SR(rowMeans(cbind(f1b,f2b,f3,f4),na.rm=TRUE),12)
  out = xts(cumsum(na20(rowMeans(cbind(f2b,f1b,f5),na.rm=TRUE))),index(f5))
  SR(diff(log(out['2010-01/'])))
  out2 = xts(cumsum(na20(rowMeans(cbind(t1,t2,t4b,t7b,t5b),na.rm=TRUE))),index(t4))
  SR(diff(log(out2['2010-01/'])))
  SR(rowMeans(cbind(diff(log(out['2010-01/'])),diff(log(out2['2010-01/']))),na.rm=TRUE))
  out = xts(cumsum(na20(rowMeans(cbind(t1,t2,t4b,t5b,f1,f4),na.rm=TRUE))),index(f4))
  SR(diff(log(out['2010-01/'])))  
  plot(out)
  plot(out['2010-01/'],t="b")
  SR(rowMeans(cbind(f2b)['2010-01/'],na.rm=TRUE))
}#comm futures
gpw_setup<-function(){
  gpw_names = c("FLTS","FPKN","FKGH","FPZU","FJSW","FW40","FW20","FCDR","FPKO","FPGE","FPEO","FTPE","FPGN","FGPW","FENG","FCIE","FALR","FMBK","FENA","FMIL","FOPL","FPLY","FPXM","FACP","FEUH","FBZW","FKER","FLWB","FATT","FGTC","FBRS","FCCC")
  gpw_cost = c(1.5,3.8,4.0,4.2,5.0,8.1,11.8,20.6,21.7,22.4,31.1,54.2,57.8,63.6,69.2,74.9,103.3,106.5,116.9,118.3,155.6,179.5,189.8,194.8,205.1,222.8,286.7,287.9,294.2,297.3,298.8,316.3)
  gpw_names = c("FPKN","FKGH","FW40","FLTS","FJSW","FW20","FPZU","FCDR","FPEO","FPKO","FPGN","FPGE","FTPE","FGPW","FCIE","FMBK","FALR","FENG","FMIL","FENA","FPLY","FACP","FEUH","FBZW","FOPL")
  gpw_cost = c(8.4,8.9,9.3,11.3,10.9,14.9,16.9,25.6,35.7,37.9,66.5,67.3,70.5,76.4,84.1,107.9,111.7,115.4,126.3,158.4,195.7,207.9,220.7,224.5,273.3)
  gpw_cost = c(3.8,4.0,8.1,1.5,5.0,11.8,4.2,20.6,31.1,21.7,57.8,22.4,54.2,63.6,74.9,106.5,103.3,69.2,118.3,116.9,179.5,194.8,205.1,222.8,155.6)
  unit_cost = c(0.06,0.06,1.6,0.06,0.06,0.8,0.06,0.06,0.06,0.06,0.006,0.06,0.006,0.06,0.06,0.06,0.06,0.06,0.006,0.06,0.06,0.06,0.06,0.06,0.06)
  gpw_names = c("FPKN","FKGH","FW40","FLTS","FJSW","FW20","FPZU","FCDR","FPEO","FPKO","FPGN","FPGE","FTPE","FGPW","FCIE","FMBK","FALR","FENG","FMIL","FENA","FPLY","FACP","FBZW","FOPL")
  gpw_cost = c(8.4,8.9,9.3,11.3,10.9,14.9,16.9,25.6,35.7,37.9,66.5,67.3,70.5,76.4,84.1,107.9,111.7,115.4,126.3,158.4,195.7,207.9,224.5,273.3)
  #download_stooq(gpw_pe_names)
  download_stooq(gpw_pe_names[21:25])
  gpw_pe_names = sapply(gpw_names, function(x) paste0(substring(x,2),"_PE"))
  gpw_pb_names = sapply(gpw_names, function(x) paste0(substring(x,2),"_PB"))
  gpw_pe_names['FW20'] = 'WIG20_PE'
  gpw_pb_names['FW20'] = 'WIG20_PB'
  gpw_pe_names['FW40'] = 'MWIG40_PE'
  gpw_pb_names['FW40'] = 'MWIG40_PB'
  gpw = load_indices_loc(gpw_names)
  gpw=fillerxts(gpw)
  gpwpe = load_indices_loc(gpw_pe_names[1:19])
  gpwpe=fillerxts(gpwpe)
  gpwpb = load_indices_loc(gpw_pb_names)
  gpwpb=fillerxts(gpwpb)
  
  i2 = momentum12(gpw,lag(gpw),gpw_cost/4,0,eqcost,1,2,2,j=11,v=1,e=1);SR(i2['2001-10/'],12)
  i3 = carry12(gpw,lag(gpw),gpw_cost/4,gpwpb,0,eqcost,1,2,2,v=1,e=1);SR(i3['2001-10/'],12)
  i3 = carry12(gpw[,s],lag(gpw[,s]),gpw_cost[s]/4,gpwpe,0,eqcost,1,2,2,v=1,e=1);SR(i3['2001-10/'],12)
  SR(binding(cbind(i2,i3)['2001-10/']),12)
  plot(cumsum(na20(i2)))
  ww=tsavgall(gpw[,c(1:20,22:25)],232,gpw_cost[c(1:20,22:25)],0,eqcost,e=1)
  wwe = tsmomenh(ww,0.17)
  ww_val = cumulate(ww)
  wwe_val = cumulate(wwe)
  i2 = momentum12(ww_val,lag(ww_val),gpw_cost[c(1:20,22:25)]/4,0,eqcost,1,2,2,j=1,v=1,e=1,p=2);SR(i2['2001-10/'],12)
  
}# GPW setup