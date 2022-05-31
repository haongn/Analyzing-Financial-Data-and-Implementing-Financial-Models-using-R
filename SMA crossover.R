library(xts)
library(quantmod)


data.AMZN = read.csv("AMZN Yahoo.csv", header=TRUE)
date = as.Date(data.AMZN$Date, format="%Y-%m-%d")
data.AMZN = cbind(date, data.AMZN[, -1])
data.AMZN = data.AMZN[order(data.AMZN$date), ]
data.AMZN = xts(data.AMZN[, 2:7], order.by = data.AMZN[, 1])
names(data.AMZN) = paste(c("AMZN.Open", "AMZN.High", "AMZN.Low", "AMZN.Close", "AMZN.Adjusted", "AMZN.Volume"))
print(data.AMZN[c(1:3, nrow(data.AMZN)), ])

AMZN.sma = data.AMZN[, 4]
print(AMZN.sma[c(1:3, nrow(AMZN.sma)), ])

# add two more columns
AMZN.sma$sma50 = rollmeanr(AMZN.sma$AMZN.Close, k = 50)
AMZN.sma$sma200 = rollmeanr(AMZN.sma$AMZN.Close, k = 200)
print(AMZN.sma[48:52, ])
print(AMZN.sma[198:202, ])

# subset the year 2012 
AMZN.sma2012 = subset(AMZN.sma, index(AMZN.sma) >= "2012-01-01")
print(AMZN.sma2012[c(1:3, nrow(AMZN.sma2012)), ])

# identify the range of y-axis 
y.range = range(AMZN.sma2012, na.rm = TRUE)
print(y.range)

# plot 
par(mfrow = c(1, 1))
plot(x = index(AMZN.sma2012), 
     xlab = "Date", 
     y = AMZN.sma2012$AMZN.Close, 
     ylim = y.range, 
     ylab = "Price ($)", 
     type = "l",      # line chart  
     col = "green",   # color 
     lwd = 2,         # line width 
     main = "Amazon - Simple Moving Average 
     January 1, 2012 - December 31, 2013")
lines(x = index(AMZN.sma2012), y = AMZN.sma2012$sma50)
lines(x = index(AMZN.sma2012), y = AMZN.sma2012$sma200, lty = 2)
legend("topleft", 
       c("Amazon Price", "50-Day Moving Average", "200-Day Moving Average"),
       col = c("green", "black", "black"),
       lty = c(1, 1, 2), 
       lwd = c(2, 1, 1))
































































