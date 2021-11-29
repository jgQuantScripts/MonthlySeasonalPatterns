require("quantmod");require("data.table");require("pbapply"); require("PerformanceAnalytics")
# Finding Monthly Seasonal Patterns
# International ETFs:
# EWA  - Australia | EIS  - Israel      | EEM  - Emerging  | EWJ  - Japan    | EPOL - Poland       | EZA - S. Africa
# EWK  - Belgium   | EIDO - Indonesia   | EWQ  - France    | EWM  - Malaysia | EPHE - Philippines  | EWP - Spain
# EWZ  - Brazil    | ENZL - New Zealand | EWG  - Germany   | EWW  - Mexico   | ICOL - Colombia     | EWD - Sweden
# ECH  - Chile     | EPU  - Peru        | EWH  - Hong Kong | THD  - Thailand | EWN  - Netherlands  | EWL - Switzerland
# MCHI - China     | ERUS - Russia      | EWI  - Italy     | TUR  - Turkey   | EWS  - Singapore    | EWU - UK
# EIRL - Ireland   | ENOR - Norway      | EDEN - Denmark   | INDA - India    | KSA  - Saudi Arabia | QAT - Qatar

# US Based
# SPY - S&P 500    | DIA - Dow Jones    | QQQ - Nasdaq-100 | MDY - MidCap    | IWM - Rusell 2000   | OEF - S&P 100

# Sector based
# XLC - Communications          | XLI  - Industrials
# XLY - Consumer Discretionary  | XLB  - Materials
# XLP - Consumer Staples        | XLRE - Real Estate
# XLE - Energy                  | XLK  - Technology 
# XLF - Financials              | XLU  - Utilities
# XLV - Health Care             | 

# Misc 
# VXX - S&P 500 Short-Term VIX  | USO  - US Oil       | TQQQ - 3x Long QQQ
# TLT - 20 Plus Year T-Bond     | UUP  - US Dollar    | SPXU - 3x Short S&P 500
# GLD - Gold                    | UGA  - Gasoline     | UPRO - 3x Long S&P 500
# SLV - Silver                  | SQQQ - 3x Short QQQ | TTT  - 3x Short 20 Plus Year T-Bond 

# ETFS
tickers = c("SPY","DIA","QQQ","MDY","IWM","OEF","EWA","EWK","EWZ","ECH","MCHI","EIRL","EIS","EIDO","ENZL",
            "EPU","ERUS","ENOR","EEM","EWQ","EWG","EWH","EWI","EDEN","EWJ","EWM","THD","TUR","INDA","EPOL",
            "EPHE","ICOL","EWN","EWS","KSA","EZA","EWP","EWD","EWL","EWU","QAT","XLC","XLY","XLP","XLE","XLF",
            "XLV","XLI","XLB","XLRE","XLK","XLU","VXX","TLT","GLD","SLV","USO","UUP","UGA","SQQQ","TQQQ","SPXU",
            "UPRO","TTT")
# get data
e <- new.env()
getSymbols(tickers, from="2000-01-01",env=e)
# put them all together
ALL = do.call(merge,eapply(e,Ad))
# format column names
colnames(ALL) = gsub(".Adjusted","",names(ALL))
# NA handling
ALL <- na.locf(ALL)
# save to reload data
# save.image("moRanks.RData")
# load("moRanks.RData")
# ************************************************************************
#                             Monthly Ranks
# ************************************************************************
# stk      = ticker name
# rankType = byReturn (ranks by Monthly % Return)
#            byGain   (ranks by % wins each month)
#            byDD     (ranks by average Drawdown) 
#            bySharpe (ranks by Sharpe Ratio)
getMonthlyRanks = function(stk,rankType){
  # subset stock
  tmp = na.omit(ALL[,stk])
  # get montly returns for each
  tmp = monthlyReturn(tmp)
  # selects rank monthly by type
  if(rankType == "byReturn"){
    # extract by month + calculate averages
    JAN = tmp[.indexmon(tmp) == 0] %>% mean() %>% round(digits = 4)
    FEB = tmp[.indexmon(tmp) == 1] %>% mean() %>% round(digits = 4)
    MAR = tmp[.indexmon(tmp) == 2] %>% mean() %>% round(digits = 4)
    APR = tmp[.indexmon(tmp) == 3] %>% mean() %>% round(digits = 4)
    MAY = tmp[.indexmon(tmp) == 4] %>% mean() %>% round(digits = 4)
    JUN = tmp[.indexmon(tmp) == 5] %>% mean() %>% round(digits = 4)
    JUL = tmp[.indexmon(tmp) == 6] %>% mean() %>% round(digits = 4)
    AUG = tmp[.indexmon(tmp) == 7] %>% mean() %>% round(digits = 4)
    SEP = tmp[.indexmon(tmp) == 8] %>% mean() %>% round(digits = 4)
    OCT = tmp[.indexmon(tmp) == 9] %>% mean() %>% round(digits = 4)
    NOV = tmp[.indexmon(tmp) == 10]%>% mean() %>% round(digits = 4)
    DEC = tmp[.indexmon(tmp) == 11]%>% mean() %>% round(digits = 4)
  }
  if(rankType == "byGain"){
  # extract by month + calculate averages
  JAN = tmp[.indexmon(tmp) == 0]
  FEB = tmp[.indexmon(tmp) == 1]
  MAR = tmp[.indexmon(tmp) == 2]
  APR = tmp[.indexmon(tmp) == 3]
  MAY = tmp[.indexmon(tmp) == 4]
  JUN = tmp[.indexmon(tmp) == 5]
  JUL = tmp[.indexmon(tmp) == 6]
  AUG = tmp[.indexmon(tmp) == 7]
  SEP = tmp[.indexmon(tmp) == 8]
  OCT = tmp[.indexmon(tmp) == 9]
  NOV = tmp[.indexmon(tmp) == 10]
  DEC = tmp[.indexmon(tmp) == 11]
  
  JAN =round(nrow(JAN[JAN$monthly.returns > 0,])/nrow(JAN),4)
  FEB =round(nrow(FEB[FEB$monthly.returns > 0,])/nrow(FEB),4)
  MAR =round(nrow(MAR[MAR$monthly.returns > 0,])/nrow(MAR),4)
  APR =round(nrow(APR[APR$monthly.returns > 0,])/nrow(APR),4)
  MAY =round(nrow(MAY[MAY$monthly.returns > 0,])/nrow(MAY),4)
  JUN =round(nrow(JUN[JUN$monthly.returns > 0,])/nrow(JUN),4)
  JUL =round(nrow(JUL[JUL$monthly.returns > 0,])/nrow(JUL),4)
  AUG =round(nrow(AUG[AUG$monthly.returns > 0,])/nrow(AUG),4)
  SEP =round(nrow(SEP[SEP$monthly.returns > 0,])/nrow(SEP),4)
  OCT =round(nrow(OCT[OCT$monthly.returns > 0,])/nrow(OCT),4)
  NOV =round(nrow(NOV[NOV$monthly.returns > 0,])/nrow(NOV),4)
  DEC =round(nrow(DEC[DEC$monthly.returns > 0,])/nrow(DEC),4)
  }
  if(rankType == "byDD"){
    # extract by month + calculate averages
    JAN = tmp[.indexmon(tmp) == 0] %>% AverageDrawdown() %>% round(digits = 4) %>% as.numeric()*-1
    FEB = tmp[.indexmon(tmp) == 1] %>% AverageDrawdown() %>% round(digits = 4) %>% as.numeric()*-1
    MAR = tmp[.indexmon(tmp) == 2] %>% AverageDrawdown() %>% round(digits = 4) %>% as.numeric()*-1
    APR = tmp[.indexmon(tmp) == 3] %>% AverageDrawdown() %>% round(digits = 4) %>% as.numeric()*-1
    MAY = tmp[.indexmon(tmp) == 4] %>% AverageDrawdown() %>% round(digits = 4) %>% as.numeric()*-1
    JUN = tmp[.indexmon(tmp) == 5] %>% AverageDrawdown() %>% round(digits = 4) %>% as.numeric()*-1
    JUL = tmp[.indexmon(tmp) == 6] %>% AverageDrawdown() %>% round(digits = 4) %>% as.numeric()*-1
    AUG = tmp[.indexmon(tmp) == 7] %>% AverageDrawdown() %>% round(digits = 4) %>% as.numeric()*-1
    SEP = tmp[.indexmon(tmp) == 8] %>% AverageDrawdown() %>% round(digits = 4) %>% as.numeric()*-1
    OCT = tmp[.indexmon(tmp) == 9] %>% AverageDrawdown() %>% round(digits = 4) %>% as.numeric()*-1
    NOV = tmp[.indexmon(tmp) == 10]%>% AverageDrawdown() %>% round(digits = 4) %>% as.numeric()*-1
    DEC = tmp[.indexmon(tmp) == 11]%>% AverageDrawdown() %>% round(digits = 4) %>% as.numeric()*-1
  }
  if(rankType == "bySharpe"){
    # extract by month + calculate averages
    JAN = tmp[.indexmon(tmp) == 0]  
    FEB = tmp[.indexmon(tmp) == 1] 
    MAR = tmp[.indexmon(tmp) == 2] 
    APR = tmp[.indexmon(tmp) == 3] 
    MAY = tmp[.indexmon(tmp) == 4] 
    JUN = tmp[.indexmon(tmp) == 5] 
    JUL = tmp[.indexmon(tmp) == 6] 
    AUG = tmp[.indexmon(tmp) == 7] 
    SEP = tmp[.indexmon(tmp) == 8] 
    OCT = tmp[.indexmon(tmp) == 9]  
    NOV = tmp[.indexmon(tmp) == 10] 
    DEC = tmp[.indexmon(tmp) == 11] 
    
    JAN = round(mean(JAN)/sd(JAN),4)
    FEB = round(mean(FEB)/sd(FEB),4)
    MAR = round(mean(MAR)/sd(MAR),4) 
    APR = round(mean(APR)/sd(APR),4) 
    MAY = round(mean(MAY)/sd(MAY),4) 
    JUN = round(mean(JUN)/sd(JUN),4) 
    JUL = round(mean(JUL)/sd(JUL),4) 
    AUG = round(mean(AUG)/sd(AUG),4) 
    SEP = round(mean(SEP)/sd(SEP),4) 
    OCT = round(mean(OCT)/sd(OCT),4) 
    NOV = round(mean(NOV)/sd(NOV),4) 
    DEC = round(mean(DEC)/sd(DEC),4) 
  
  }
  # combine all months
  as.data.frame(cbind(stk,JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC))
}

MO1  = pblapply(as.list(names(ALL)),function(x){
  getMonthlyRanks(stk=x, rankType = "byReturn")
})
MO2  = pblapply(as.list(names(ALL)),function(x){
  getMonthlyRanks(stk=x, rankType = "byGain")
})
MO3  = pblapply(as.list(names(ALL)),function(x){
  getMonthlyRanks(stk=x, rankType = "byDD")
})
MO4  = pblapply(as.list(names(ALL)),function(x){
  getMonthlyRanks(stk=x, rankType = "bySharpe")
})
# row bind results: 
MO1 = rbindlist(MO1,use.names = TRUE,fill = TRUE) # byReturn
MO2 = rbindlist(MO2,use.names = TRUE,fill = TRUE) # byGain
MO3 = rbindlist(MO3,use.names = TRUE,fill = TRUE) # byDD
MO4 = rbindlist(MO4,use.names = TRUE,fill = TRUE) # bySharpe

# get monthly returns for each stock
moRet = pblapply(as.list(names(ALL)), function(stk){
  # subset stock
  tmp = na.omit(ALL[,stk])
  # get montly returns for each
  tmp = monthlyReturn(tmp)
  # assign ticker name
  colnames(tmp) = stk
  # return tmp
  tmp
})
moRet = do.call(merge,moRet)

# create function to long the top 4 best performing ETFs each month - Equalt Wt
monthlyN = function(MO,moRet,N,short){
  # apply the following for each month
  LONG = lapply(as.list(0:11), function(ii){
    # which month
    MONTH = names(MO)[ii+2]
    # best performing this month
    BEST = MO[order(as.numeric(MO[[MONTH]]),decreasing = TRUE),][1:N]
    # extract ETF names
    BEST = BEST[["stk"]]
    # get monthly returns 
    BEST = moRet[,BEST]
    # subset to month returns only
    BEST = BEST[.indexmon(BEST) == ii]
    # equally weighted == rowMeans on ETFs that were available that year
    BEST = reclass(rowMeans(BEST,na.rm = TRUE),match.to = BEST)
    # NA & NAN handling
    BEST[is.na(BEST)] <-0
    BEST[is.nan(BEST)] <-0
    # add column name
    colnames(BEST) = "Strat"
    # return XTS
    BEST
  })
  # row bind XTS - it will sort itself by date
  jd = do.call(rbind,LONG)
  # ************************************************************************
  # ************************************************************************
  if(short==TRUE){
  # apply the following for each month
  SHORT = lapply(as.list(0:11), function(ii){
    # which month
    MONTH = names(MO)[ii+2]
    # best performing this month
    BEST = MO[order(as.numeric(MO[[MONTH]]),decreasing = FALSE),][1:N]
    # extract ETF names
    BEST = BEST[["stk"]]
    # get monthly returns 
    BEST = moRet[,BEST]
    # subset to month returns only
    BEST = BEST[.indexmon(BEST) == ii]
    # equally weighted == rowMeans on ETFs that were available that year
    BEST = reclass(rowMeans(BEST,na.rm = TRUE)*-1,match.to = BEST)
    # NA & NAN handling
    BEST[is.na(BEST)] <-0
    BEST[is.nan(BEST)] <-0
    # add column name
    colnames(BEST) = "Strat"
    # return XTS
    BEST
  })
  # row bind XTS - it will sort itself by date
  SHORT = do.call(rbind,SHORT)
  # combine with LONGS
  jd = reclass(coredata(jd) + coredata(SHORT), match.to = jd)
  }
  # ************************************************************************
  # ************************************************************************
  # return jd (Jan - Dec)
  jd
}

# get cumulative returns by type
byReturn = monthlyN(MO=MO1,moRet = moRet, N=4,short=FALSE); colnames(byReturn) = "byReturn"
byGain   = monthlyN(MO=MO2,moRet = moRet, N=4,short=FALSE); colnames(byGain) = "byGain" 
byDD     = monthlyN(MO=MO3,moRet = moRet, N=4,short=FALSE); colnames(byDD) = "byDD"
bySharpe = monthlyN(MO=MO4,moRet = moRet, N=4,short=FALSE); colnames(bySharpe) = "bySharpe"
# with SHORTS
byReturnS = monthlyN(MO=MO1,moRet = moRet, N=4,short=TRUE); colnames(byReturnS) = "byReturnS"
byGainS   = monthlyN(MO=MO2,moRet = moRet, N=4,short=TRUE); colnames(byGainS) = "byGainS" 
byDDS     = monthlyN(MO=MO3,moRet = moRet, N=4,short=TRUE); colnames(byDDS) = "byDDS"
bySharpeS = monthlyN(MO=MO4,moRet = moRet, N=4,short=TRUE); colnames(bySharpeS) = "bySharpeS"

# merge with SPY to compare returns
LONG  = merge(byReturn,byGain,byDD,bySharpe,moRet[,"SPY"])
SHORT = merge(byReturnS,byGainS,byDDS,bySharpeS,moRet[,"SPY"])
COMP = merge(LONG[,1:4],SHORT[,1:4],moRet[,"SPY"])
             
# plot returns
charts.PerformanceSummary(LONG,geometric = FALSE,cex.legend=0.5)
charts.PerformanceSummary(SHORT,geometric = FALSE,cex.legend=0.5)
charts.PerformanceSummary(COMP,geometric = FALSE,cex.legend=0.35, col=tim10equal)
# compare long vs short
charts.PerformanceSummary(merge(LONG$byReturn,SHORT$byReturnS),geometric = FALSE,cex.legend=0.5)
charts.PerformanceSummary(merge(LONG$byGain,SHORT$byGainS),geometric = FALSE,cex.legend=0.5)
charts.PerformanceSummary(merge(LONG$byDD,SHORT$byDDS),geometric = FALSE,cex.legend=0.5)
charts.PerformanceSummary(merge(LONG$bySharpe,SHORT$bySharpeS),geometric = FALSE,cex.legend=0.5)

# this year
charts.PerformanceSummary(LONG["2021"],geometric = FALSE)
charts.PerformanceSummary(SHORT["2021"],geometric = FALSE)
charts.PerformanceSummary(COMP["2021"],geometric = FALSE,cex.legend=0.35, col=tim10equal)
# charts.PerformanceSummary(COMP$bySharpe,geometric = FALSE)
# get Stats
View(table.Stats(COMP))
# correlation
View(cor(COMP,COMP$SPY))
# compare annual reutrns
View(round(Return.annualized(COMP,scale = 12),4))
# Drawdowns - LONG
table.Drawdowns(COMP$byReturn,geometric = FALSE)
table.Drawdowns(COMP$byGain,geometric = FALSE)
table.Drawdowns(COMP$byDD,geometric = FALSE)
table.Drawdowns(COMP$bySharpe,geometric = FALSE)
table.Drawdowns(COMP$SPY,geometric = FALSE)
# Drawdowns - SHORTS
table.Drawdowns(COMP$byReturnS,geometric = FALSE)
table.Drawdowns(COMP$byGainS,geometric = FALSE)
table.Drawdowns(COMP$byDDS,geometric = FALSE)
table.Drawdowns(COMP$bySharpeS,geometric = FALSE)

# function to get the top tickers for any given month
getTopTickers = function(MONTH,MO,topN,short)
{
  tmp = MO[order(as.numeric(MO[[paste(MONTH)]]), decreasing = TRUE),]
  tmp = tmp[["stk"]][1:topN]
  if(short==TRUE)
  {
    tmp2 = MO[order(as.numeric(MO[[paste(MONTH)]]), decreasing = FALSE),]
    tmp2 = tmp2[["stk"]][1:topN]
    tmp = c(tmp,tmp2)
  }
  tmp
}
thisMO = getTopTickers(MONTH="DEC",MO=MO4,topN=4,short = TRUE);thisMO
perf <- moRet[,c(thisMO)] # performance for those tickers
perf <- perf[.indexmon(perf) == 11] # MANUALLY CHANGE INT (11 == DECEMBER)
perf_c = reclass(rowMeans(perf,na.rm = TRUE), match.to = perf)
perf[is.na(perf)]<-0
charts.PerformanceSummary(perf,geometric = FALSE, cex.legend=0.35) # individual
charts.PerformanceSummary(perf_c,geometric = FALSE) # combined
# *************************************************************************************
# Compare to picking the top N ETFs: top 1, top 4, top 5, top 10
RANKs = MO4 # MO1 (byReturn), MO2 (byGain), MO3 (byDD), MO4 (bySharpe)
jd1  = monthlyN(MO=RANKs,moRet = moRet, N=1,short = TRUE)
jd4  = monthlyN(MO=RANKs,moRet = moRet, N=4,short = TRUE)
jd5  = monthlyN(MO=RANKs,moRet = moRet, N=5,short = TRUE)
jd10 = monthlyN(MO=RANKs,moRet = moRet, N=10,short = TRUE)
# merge with SPY to compare returns
COMP = merge(jd1,jd4,jd5,jd10,moRet[,"SPY"])
colnames(COMP)=c("strat1","strat4","strat5","strat10","SPY")
charts.PerformanceSummary(COMP,geometric = FALSE)
table.Stats(COMP)
# *************************************************************************************


