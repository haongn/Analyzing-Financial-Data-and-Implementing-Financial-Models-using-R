library(xts)
library(quantmod)

data.AMZN = read.csv("AMZN Yahoo.csv", header = TRUE)
date = as.Date(data.AMZN$Date, format = "%Y-%m-%d")
data.AMZN = cbind(date, data.AMZN[, -1])
data.AMZN = data.AMZN[order(data.AMZN$date), ]
data.AMZN = xts(data.AMZN[, 2:7], order.by = data.AMZN[, 1])
names(data.AMZN) = paste(c("AMZN.Open", "AMZN.High", "AMZN.Low", "AMZN.Close", "AMZN.Adjusted", "AMZN.Volume"))
print(data.AMZN[c(1:3, nrow(data.AMZN)), ])
print(class(data.AMZN))

mo = data.AMZN
print(mo[c(1:3, nrow(mo)), ])

# Step 2: Convert Daily Data to Monthly Data.
AMZN.monthly = to.monthly(mo, OHLC = FALSE)
print(AMZN.monthly[c(1:3, nrow(AMZN.monthly)), ])

# Step 3: Clean up Data to Include Only Adjusted Closing Prices for the End of Each Month 
AMZN.monthly = AMZN.monthly[, 5]
names(AMZN.monthly) = paste("mo.Adjusted")
print(AMZN.monthly[c(1:3, nrow(AMZN.monthly)), ])

# Step 4: Calculate Monthly Returns
AMZN.monthly$Ret = Delt(AMZN.monthly$mo.Adjusted)
print(AMZN.monthly[c(1:3, nrow(AMZN.monthly)), ])

# Step 5: Cleanup Monthly Data Object
AMZN.monthly = AMZN.monthly[-1, 2]
print(AMZN.monthly[c(1:3, nrow(AMZN.monthly)), ])



















