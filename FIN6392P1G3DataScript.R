##### FIN 6392 Project 1 Group 3 #####

### Required Packages for Trading Data and Macroeconomic Data Extraction ###
  require(quantmod)

### Function that returns factor values from stock "..." ###
  factorValueGeneratorAndAppender = function(...) {
    globalFactors =
      merge(..., ## Where stock-specific adjusted prices will go ##
        RSI(...), # Relative Strength Index
        MACD(...), # MACD 
        SMA(..., 200), # 200SMA
        SMA(..., 100), # 100SMA
        SMA(..., 50), # 50SMA
        SMA(..., 25), # 20SMA
        EMA(..., 14), # 14EMA
        EMA(..., 10), # 10EMA
        EMA(..., 5), # 5EMA
        BBands(..., sd=2.0), # Bollinger Bands
        WMA(...,), # Weighted MA
        TRIX(...,), # Triple Smoothed Exponential Oscillator
        DEMA(...,), # Double Exponential MA 
        CCI(...), # Commodity Channel Index
        CMO(...), # Chande Momentum Oscillator
        DPO(...), # Detrended Price Oscillator
        ROC(...), # Rate of Change
        SMI(...), # Stocastic Momentum Index
        WPR(...), # Williams %R
        CTI(...), # Ehler's Correlation Trend Indicator
        DVI(..., 100), # DV Intermediate Oscillator
        KST(...), # Know Sure Thing
        TDI(...), # Trend Detection Index
        VHF(...), # Vertical Horizonal Filter
        
      all = TRUE
    )
    return (globalFactors)
  }

### Macroeconomic Data ###
  MACRO_ENV = new.env()
  setDefaults(getSymbols, src='FRED')
  
  getSymbols('UNRATE', env = MACRO_ENV)
  getSymbols('CPIAUCSL', env = MACRO_ENV)
  getSymbols('GDPC1', env = MACRO_ENV)
  getSymbols('INDPRO', env = MACRO_ENV)
  getSymbols('RRSFS', env = MACRO_ENV)
  getSymbols('ALTSALES', env = MACRO_ENV)
  getSymbols('PCEC96', env = MACRO_ENV)
  getSymbols('BUSINV', env = MACRO_ENV)
  getSymbols('ISRATIO', env = MACRO_ENV)
  getSymbols('TOTALSL', env = MACRO_ENV)


  macroData = merge(
    MACRO_ENV$UNRATE["2019-02-25/2022-02-25"],
    MACRO_ENV$CPIAUCSL["2019-02-25/2022-02-25"],
    MACRO_ENV$GDPC1["2019-02-25/2022-02-25"],
    MACRO_ENV$INDPRO["2019-02-25/2022-02-25"],
    MACRO_ENV$RRSFS["2019-02-25/2022-02-25"],
    MACRO_ENV$ALTSALES["2019-02-25/2022-02-25"],
    MACRO_ENV$PCEC96["2019-02-25/2022-02-25"],
    MACRO_ENV$BUSINV["2019-02-25/2022-02-25"],
    MACRO_ENV$ISRATIO["2019-02-25/2022-02-25"],
    MACRO_ENV$TOTALSL["2019-02-25/2022-02-25"]
  )
  macroData <- as.data.frame(macroData)


### SBUX ###
  
  #SBUXfinancials
  # Extracts stock's factor values from the function in line 5 and appends them to stock's adjusted prices, returns xts object with correct time frame
    sbuxData = merge(factorValueGenerator(Ad(
      getSymbols(
        c("SBUX"),
        src = "yahoo",
        from = as.Date("2019-2-25"),
        to = as.Date("2022-2-25"),
        periodicity = "daily",
        auto.assign = FALSE
      )
    )), all = TRUE)[253:758, ]

  sbuxData <- as.data.frame(sbuxData)
  colnames(sbuxData)[4] <- "MACDsignal"
  colnames(sbuxData)[18] <- "TRIXsignal"
  colnames(sbuxData)[22] <- "DPO"
  colnames(sbuxData)[23] <- "ROC"
  colnames(sbuxData)[25] <- "SMIsignal"
  colnames(sbuxData)[26] <- "WPR"
  colnames(sbuxData)[32] <- "KSTsignal"

#sbuxsentiment
  sbuxSentiment <- read.csv("SBUXSentimentDataFinal.csv")
  sbuxSentiment<- aggregate(sbuxSentiment, by=list(sbuxSentiment$threadComments_SBUX.date), FUN = mean, na.rm=TRUE)
  sbuxSentiment <- subset(sbuxSentiment, select = -c(X,threadComments_SBUX.date))
  sbuxDataDaily <- merge(sbuxData, sbuxSentiment,by.x = 0, by.y=1, all.x= TRUE)
  sbuxDataDaily <- data.frame(sbuxDataDaily[,-1], row.names = sbuxDataDaily[,1])
  names(sbuxDataDaily)[names(sbuxDataDaily) == 'SBUX.Adjusted'] <- 'Adjusted'
  names(sbuxDataDaily)[names(sbuxDataDaily) == 'SBUXSentimentDataFinal.SentimentGI'] <- 'Sentiment'
  names(sbuxDataDaily)[names(sbuxDataDaily) == 'SBUXSentimentDataFinal.WordCount'] <- 'WordCount'
  sbuxDataDaily$ticker <- "SBUX"
  sbuxDataMonthly <- merge(sbuxDataDaily, macroData ,by.x = 0, by.y=0)
  sbuxDataMonthly <- data.frame(sbuxDataMonthly[,-1], row.names = sbuxDataMonthly[,1])
  sbuxDataMonthly <- sbuxDataMonthly[, -c(2:37)] 
  names(sbuxDataMonthly)[names(sbuxDataMonthly) == 'SBUX.Adjusted'] <- 'Adjusted'
  names(sbuxDataMonthly)[names(sbuxDataMonthly) == 'SBUXSentimentDataFinal.SentimentGI'] <- 'Sentiment'
  names(sbuxDataMonthly)[names(sbuxDataMonthly) == 'SBUXSentimentDataFinal.WordCount'] <- 'WordCount'
  sbuxDataMonthly$ticker <- "SBUX"
  sbuxDataDaily$returns <- Delt(sbuxDataDaily$Adjusted) 
  sbuxDataMonthly$returns <- Delt(sbuxDataMonthly$Adjusted)


### AMZN ###
  
  #AMZNfinancials
  # Extracts stock's factor values from the function in line 5 and appends them to stock's adjusted prices, returns xts object with correct time frame
    amznData = merge(factorValueGenerator(Ad(
      getSymbols(
        c("AMZN"),
        src = "yahoo",
        from = as.Date("2019-2-25"),
        to = as.Date("2022-2-25"),
        periodicity = "daily",
        auto.assign = FALSE
      )
    )), all = TRUE)[253:758, ]
  
    amznData <- as.data.frame(amznData)
    colnames(amznData)[4] <- "MACDsignal"
    colnames(amznData)[18] <- "TRIXsignal"
    colnames(amznData)[22] <- "DPO"
    colnames(amznData)[23] <- "ROC"
    colnames(amznData)[25] <- "SMIsignal"
    colnames(amznData)[26] <- "WPR"
    colnames(amznData)[32] <- "KSTsignal"
  
  #AMZNsentiment
    amznSentiment <- read.csv("AMZNSentimentDataFinal.csv")
    amznSentiment<- aggregate(amznSentiment, by=list(amznSentiment$threadComments_AMZN.date), FUN = mean, na.rm=TRUE)
    amznSentiment <- subset(amznSentiment, select = -c(X,threadComments_AMZN.date))
    amznDataDaily <- merge(amznData, amznSentiment ,by.x = 0, by.y=1)
    amznDataDaily <- data.frame(amznDataDaily[,-1], row.names = amznDataDaily[,1])
    names(amznDataDaily)[names(amznDataDaily) == 'AMZN.Adjusted'] <- 'Adjusted'
    names(amznDataDaily)[names(amznDataDaily) == 'amznSentiment.SentimentGI'] <- 'Sentiment'
    names(amznDataDaily)[names(amznDataDaily) == 'amznSentiment.WordCount'] <- 'WordCount'
    amznDataDaily$ticker <- "AMZN"
    amznDataMonthly <- merge(amznDataDaily, macroData ,by.x = 0, by.y=0)
    amznDataMonthly <- data.frame(amznDataMonthly[,-1], row.names = amznDataMonthly[,1])
    amznDataMonthly <- amznDataMonthly[, -c(2:37)] # delete columns 2 through 37
    names(amznDataMonthly)[names(amznDataMonthly) == 'AMZN.Adjusted'] <- 'Adjusted'
    names(amznDataMonthly)[names(amznDataMonthly) == 'amznSentiment.SentimentGI'] <- 'Sentiment'
    names(amznDataMonthly)[names(amznDataMonthly) == 'amznSentiment.WordCount'] <- 'WordCount'
    amznDataMonthly$ticker <- "AMZN"
    amznDataDaily$returns <- Delt(amznDataDaily$Adjusted) 
    amznDataMonthly$returns <- Delt(amznDataMonthly$Adjusted)

### TSLA ###
    
# Extracts stock's factor values from the function in line 5 and appends them to stock's adjusted prices, returns xts object with correct time frame
    tslaData = merge(factorValueGenerator(Ad(
      getSymbols(
        c("TSLA"),
        src = "yahoo",
        from = as.Date("2019-2-25"),
        to = as.Date("2022-2-25"),
        periodicity = "daily",
        auto.assign = FALSE
      )
    )), all = TRUE)[253:758, ]

  tslaData <- as.data.frame(tslaData)
  colnames(tslaData)[4] <- "MACDsignal"
  colnames(tslaData)[18] <- "TRIXsignal"
  colnames(tslaData)[22] <- "DPO"
  colnames(tslaData)[23] <- "ROC"
  colnames(tslaData)[25] <- "SMIsignal"
  colnames(tslaData)[26] <- "WPR"
  colnames(tslaData)[32] <- "KSTsignal"
  
#tslasentiment
  tslaSentiment <- read.csv("TSLASentimentDataFinal.csv")
  tslaSentiment<- aggregate(tslaSentiment, by=list(tslaSentiment$threadComments_TSLA.date), FUN = mean, na.rm=TRUE)
  tslaSentiment <- subset(tslaSentiment, select = -c(X,threadComments_TSLA.date))
  tslaDataDaily <- merge(tslaData, tslaSentiment ,by.x = 0, by.y=1)
  tslaDataDaily <- data.frame(tslaDataDaily[,-1], row.names = tslaDataDaily[,1])
  names(tslaDataDaily)[names(tslaDataDaily) == 'TSLA.Adjusted'] <- 'Adjusted'
  names(tslaDataDaily)[names(tslaDataDaily) == 'TSLASentiment.SentimentGI'] <- 'Sentiment'
  names(tslaDataDaily)[names(tslaDataDaily) == 'TSLASentiment.WordCount'] <- 'WordCount'
  tslaDataDaily$ticker <- "TSLA"
  tslaDataMonthly <- merge(tslaDataDaily, macroData ,by.x = 0, by.y=0)
  tslaDataMonthly <- data.frame(tslaDataMonthly[,-1], row.names = tslaDataMonthly[,1])
  tslaDataMonthly <- tslaDataMonthly[, -c(2:37)]
  names(tslaDataMonthly)[names(tslaDataMonthly) == 'TSLA.Adjusted'] <- 'Adjusted'
  names(tslaDataMonthly)[names(tslaDataMonthly) == 'TSLASentiment.SentimentGI'] <- 'Sentiment'
  names(tslaDataMonthly)[names(tslaDataMonthly) == 'TSLASentiment.WordCount'] <- 'WordCount'
  tslaDataMonthly$ticker <- "TSLA"
  tslaDataDaily$returns <- Delt(tslaDataDaily$Adjusted)
  tslaDataMonthly$returns <- Delt(tslaDataMonthly$Adjusted)

  allData <- rbind(tslaDataDaily, amznDataDaily, sbuxDataDaily)
  allMonthly <- rbind(tslaDataMonthly, amznDataMonthly, sbuxDataMonthly)


#Split data into train/test sets
  set.seed(12345)
  train.rows <- sample(rownames(allData), dim(allData)[1]*0.75)
  valid.rows <- sample(setdiff(rownames(allData), train.rows), dim(allData)[1]*0.3)
  test.rows <- setdiff(rownames(allData), train.rows)
  
  train.data <- allData[train.rows, ]
  train.data <- na.omit(train.data)
  valid.data <- allData[valid.rows, ]
  test.data <- allData[test.rows, ]

#initial model
  initialmodel <- lm(returns ~ . -ticker+factor(ticker)-1, data=train.data)
  summary(initialmodel)
  
  set.seed(123)
  
  train.control = trainControl(method = "cv", number = 10)
  step.model = train(returns ~., data = na.omit(allData), method = "leapSeq", tuneGrid = data.frame(nvmax = 39), trControl = train.control)
  step.model$results
  
  summary(step.model$finalModel)

#Check for multicolinearity
  numData <- allData[, sapply(allData, is.numeric)]
  heatmap(cor(numData), Rowv = NA, Colv = NA)
  round(cor(numData),2)
  
#vif(initialmodel)>10

#Build model
  cleanedmodel <- lm(returns ~ . -ticker-Adjusted-SMA.1-SMA.2-SMA.3-EMA.1-EMA.2-SMIsignal-KSTsignal-TRIXsignal-MACDsignal-dvi.mag-dvi.str-up-dn-mavg-up-WMA.......-DEMA-rsi-SMA-EMA-dn-mavg-pctB-cci+factor(ticker)-1, data=train.data)
  summary(cleanedmodel)
  plot(cleanedmodel)
  confint(cleanedmodel, level = .95)

#Confirm colinearity is gone
  vif(cleanedmodel)>10 #all variables have less than 10 VIF!

#Test the model on new data
  valid.predict <- predict(cleanedmodel, valid.data)
  accuracy(valid.predict, valid.data$returns)
  test.predict <- predict(cleanedmodel, test.data)
  accuracy(test.predict, test.data$returns)

#Sort stocks based on chosen factors
  sortedData <- allData[order(-allData$ROC, -allData$macd, allData$kst) , ]
  top10percent <- sortedData[1:150, ]
  bottom10percent <- sortedData[1358:1508, ]

#Performance Analysis
  install.packages("PerformanceAnalytics")
  library(PerformanceAnalytics)

  amznReturnsOnly <- amznDataDaily[39]
  tslaReturnsOnly <- tslaDataDaily[39]
  sbuxReturnsOnly <- sbuxDataDaily[39]
  allReturnsOnly <- allData[39]
  top10percentReturnsOnly <- top10percent[39]
  bottom10percentReturnsOnly <- bottom10percent[39]

  Return.annualized(top10percentReturnsOnly)
  Return.annualized(bottom10percentReturnsOnly)
  Return.annualized(sbuxReturnsOnly)
  Return.annualized(tslaReturnsOnly)
  Return.annualized(amznReturnsOnly)

  Return.annualized(allReturnsOnly)

  maxDrawdown(allReturnsOnly) #most u may lose at one point in time
  
#charts.PerformanceSummary(returns$AMZN.ret,methods="GaussianES")
  SharpeRatio.annualized(allReturnsOnly)

#SharpeRatio(amznReturnsOnly)
  VaR(allReturnsOnly, 0.05,method="gaussian")
  chart.VaRSensitivity(allReturnsOnly) #chart includes VaR, ES

  charts.PerformanceSummary(tslaReturnsOnly)
  charts.PerformanceSummary(sbuxReturnsOnly)
  charts.PerformanceSummary(amznReturnsOnly)
