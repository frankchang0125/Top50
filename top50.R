library(quantmod)

Top50 <- get(getSymbols("0050.tw"))
TSE <- get(getSymbols("^TWII"))

# Limit time interval of data
Top50 <- Top50["2009-01-01::"]
TSE <- TSE["2009-01-01::"]

# Rename column names
colnames(Top50) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(TSE) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")

tseHigh <- TSE$High
tseLow <- TSE$Low
tseClose <- TSE$Close

date <- index(tseClose)
numDate <- length(date)

# Calculate RSV
tsePeriodHigh <- xts(rep(0, numDate - 8), order.by = date[-(1:8)])
tsePeriodLow <- xts(rep(0, numDate - 8), order.by = date[-(1:8)])
tseRSV <- xts(rep(0, numDate - 8), order.by = date[-(1:8)])

for (j in (9:numDate)) {
  period <- date[(j-8): j]
  i <- date[j]
  tsePeriodHigh[i] <- max(tseHigh[period])
  tsePeriodLow[i] <- min(tseLow[period])
  tseRSV[i] <- 100*(tseClose[i] - tsePeriodLow[i]) / (tsePeriodHigh[i] - tsePeriodLow[i])
}

# Calculate K/D values
rsv <- c(50, 50, as.numeric(tseRSV))
RSV1 <- xts(rsv, order.by=date[7:numDate])

tseKValues <- EMA(RSV1, n = 2, ratio = 1/3)
names(tseKValues) <- "TSE K Values"

tseKValues[1] <- 50
tseDValues <- EMA(tseKValues, n = 2, ratio = 1/3)
names(tseDValues) <- "TSE D Values"

tseKValues <- tseKValues[-(1:2)]
tseDValues <- tseDValues[-(1:2)]

# KD indicator
tseKDindicator <- merge(tseRSV, tseKValues, tseDValues)

# Calculate profit
money <- 1000000
totalSpendMoney <- 0
holdShares <- 0
profits <- data.frame(matrix(ncol = 10, nrow = 0))
names(profits) <- c("Buy Date", "Buy Price", "Buy K Value", "TSE Index",
                    "Sell Date", "Sell Price", "Sell K Value", "TSE Index",
                    "Volume", "Profit")
i <- 1

for (tseKValue in tseKValues) {
  # if (tseKValue < 20) {
  #   maxBuyShares <- 1000000000
  if (tseKValue < 80) {
    if (tseKValue < 20) {
      # Buy at most 5000 shares
      maxBuyShares <- 5000
    } else if (tseKValue >= 20 && tseKValue < 40) {
      # Buy at most 4000 shares
      maxBuyShares <- 4000
    } else if (tseKValue >= 40 && tseKValue < 60) {
      # Buy at most 3000 shares
      maxBuyShares <- 3000
    } else if (tseKValue >= 60 && tseKValue < 80) {
      # Buy at most 2000 shares
      maxBuyShares <- 2000
    }
    
    buyDate <- as.character(index(tseKValues[i]))
    buyPrice <- as.numeric(Top50[buyDate]$Close)
    buyIndex <- as.numeric(TSE[buyDate]$Close)
    canBuyShares <- as.integer(money / buyPrice)
    
    if (!length(buyPrice)) {
      print(paste0("Error buy date: ", buyDate))
      i <- i + 1
      next
    }
    
    buyShares <- if (canBuyShares < maxBuyShares) canBuyShares else maxBuyShares
    
    if (buyShares > 0 && money >= buyPrice * buyShares) {
      buyKValue <- tseKValue
      holdShares <- holdShares + buyShares
      spendMoney <- (buyPrice * buyShares)
      totalSpendMoney <- totalSpendMoney + spendMoney
      money <- money - spendMoney
      profits[nrow(profits) + 1,] <- c(buyDate, buyPrice, buyKValue, buyIndex,
                                       NA, NA, NA, NA,
                                       buyShares, NA)
    }
  } else if (tseKValue >= 80 && holdShares > 0) {
    # Sell all shares
    sellDate <- as.character(index(tseKValues[i]))
    sellPrice <- as.numeric(Top50[sellDate]$Close)
    sellIndex <- as.numeric(TSE[sellDate]$Close)
    
    if (!length(sellPrice)) {
      print(paste0("Error sell date: ", sellDate))
      i <- i + 1
      next
    }
    
    sellKValue <- tseKValue
    profit <- (sellPrice * holdShares) - totalSpendMoney
    profits[nrow(profits) + 1,] <- c(NA, NA, NA, NA,
                                     sellDate, sellPrice, sellKValue, sellIndex,
                                     holdShares, profit)
    money <- money + (sellPrice * holdShares)
    totalSpendMoney <- 0
    holdShares <- 0
  }
  
  i <- i + 1
}

if (holdShares > 0) {
  # Game over, sell all the remaining shares
  sellDate <- as.character(index(tseKValues[length(tseKValues)]))
  sellPrice <- as.numeric(Top50[sellDate]$Close)
  sellIndex <- as.numeric(TSE[sellDate]$Close)
  sellKValue <- tseKValue
  profit <- (sellPrice * holdShares) - totalSpendMoney
  profits[nrow(profits) + 1,] <- c(NA, NA, NA, NA,
                                   sellDate, sellPrice, sellKValue, sellIndex,
                                   holdShares, profit)
  money <- money + (sellPrice * holdShares)
  totalSpendMoney <- 0
  holdShares <- 0
}

totalProfits <- rowSums(rbind(as.numeric(profits$Profit)), na.rm = TRUE)
print(paste0("Total profits: ", totalProfits))

print("Exporting Profits.csv...")
profitsCSV <- "~/Desktop/Profits.csv"
write.table(profits, profitsCSV, na = "", col.names = TRUE, row.names = FALSE, sep = ",")

summary <- data.frame(matrix(ncol = 10, nrow = 0))
summary[1,] <- c('Total Profits', NA, NA, NA, NA, NA, NA, NA, NA, totalProfits)
write.table(summary, profitsCSV, na = "", col.names = FALSE, row.names = FALSE, append = TRUE, sep = ",")
print("Done!!")
