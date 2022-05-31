library(xts)
library(quantmod)

data.AMZN = read.csv("AMZN Yahoo.csv", header=TRUE)
date = as.Date(data.AMZN$Date, format="%Y-%m-%d")
data.AMZN = cbind(date, data.AMZN[, -1])
data.AMZN = data.AMZN[order(data.AMZN$date), ]
data.AMZN = xts(data.AMZN[, 2:7], order.by = data.AMZN[, 1])
names(data.AMZN) = paste(c("AMZN.Open", "AMZN.High", "AMZN.Low", "AMZN.Close", "AMZN.Adjusted", "AMZN.Volume"))
print(data.AMZN[c(1:3, nrow(data.AMZN)), ])

# Step 1: Obtain Closing Prices for Amazon.com Stock 
AMZN.RSI = data.AMZN[, 4]
AMZN.RSI$delta = diff(AMZN.RSI$AMZN.Close)   # lay hieu today voi yesterday 
print(AMZN.RSI[c(1:3, nrow(AMZN.RSI)), ])

# Step 2: Create Dummy Variables to Indicate Whether Price Went Up or Price
# Went Down
AMZN.RSI$up = ifelse(AMZN.RSI$delta > 0, 1, 0)
AMZN.RSI$down = ifelse(AMZN.RSI$delta < 0, 1, 0)
print(AMZN.RSI[c(1:3, nrow(AMZN.RSI)), ])

# Step 3: Calculate Prices (Differences) for Up Days and Prices for Down Days
AMZN.RSI$up.val = AMZN.RSI$delta * AMZN.RSI$up 
AMZN.RSI$down.val = -AMZN.RSI$delta * AMZN.RSI$down
AMZN.RSI = AMZN.RSI[-1, ]   # xoa di dong dau tien
print(AMZN.RSI[c(1:3, nrow(AMZN.RSI)), ])

# Step 4: Calculate Initial Up and Down 14-Day Averages
AMZN.RSI$up.first.avg = rollapply(AMZN.RSI$up.val, 
                                  width = 14, FUN = mean, fill = NA, na.rm = TRUE)
AMZN.RSI$down.first.avg = rollapply(AMZN.RSI$down.val, 
                                  width = 14, FUN = mean, fill = NA, na.rm = TRUE)
print(AMZN.RSI[c(1:15, nrow(AMZN.RSI)), ])

# Step 5: Calculate the Wilder Exponential MovingAverage to Calculate Final Up
# and Down 14-DayAverages
up.val = as.numeric(AMZN.RSI$up.val)
down.val = as.numeric(AMZN.RSI$down.val)

AMZN.RSI$up.avg = AMZN.RSI$up.first.avg
for (i in 15:nrow(AMZN.RSI))
{
  AMZN.RSI$up.avg[i] = ((AMZN.RSI$up.avg[i - 1] * 13 + up.val[i]) / 14)
}
AMZN.RSI$down.avg = AMZN.RSI$down.first.avg 
for (i in 15:nrow(AMZN.RSI))
{
  AMZN.RSI$down.avg[i] = ((AMZN.RSI$down.avg[i - 1] * 13 + down.val[i]) / 14)
}
print(AMZN.RSI[c(1:20, nrow(AMZN.RSI)), ])

# Step 6: Calculate the RSI
AMZN.RSI$RS = AMZN.RSI$up.avg / AMZN.RSI$down.avg
AMZN.RSI$RSI = 100 - (100 / (1 + AMZN.RSI$RS))
print(AMZN.RSI[c(14:20, nrow(AMZN.RSI)), ])
 
# Step 7: Subset to Show Only 2012 and 2013 Data
AMZN.RSI2012 = subset(AMZN.RSI[, ncol(AMZN.RSI)], 
                      index(AMZN.RSI) >= "2012-01-01")
print(AMZN.RSI2012[c(1:3, nrow(AMZN.RSI2012)), ])

# Step 8: Plot the RSI
title1 = "Amazon - Relative Strength Index"
title2 = "January 2012 - December 2013"
plot(x = index(AMZN.RSI2012), 
     xlab = "Date", 
     y = AMZN.RSI2012$RSI, 
     ylab = "RSI (14 Day Moving Average)",
     ylim = c(0, 100), 
     type = "l", 
     main = paste(title1, "\n", title2))
abline(h = c(30, 70), lty = 2)






























