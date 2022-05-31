# As data.AMZN is an xts object, we can easily change the daily
# data to weekly using the to.weekly command.
data.AMZN = read.csv("AMZN Yahoo.csv", header = TRUE)
date = as.Date(data.AMZN$Date, format = "%Y-%m-%d")
data.AMZN = cbind(date, data.AMZN[, -1])
data.AMZN = data.AMZN[order(data.AMZN$date), ]
data.AMZN = xts(data.AMZN[, 2:7], order.by = data.AMZN[, 1])
names(data.AMZN) = paste(c("AMZN.Open", "AMZN.High", "AMZN.Low", "AMZN.Close", "AMZN.Adjusted", "AMZN.Volume"))
print(data.AMZN[c(1:3, nrow(data.AMZN)), ])

wk = data.AMZN

# Step 2: Convert to Daily Data Data to Weekly Data: using to.weekly function  
AMZN.weekly = to.weekly(wk, OHLC = FALSE)              ## problem solved 
print(AMZN.weekly[c(1:3, nrow(AMZN.weekly)), ])

# Step 3: Clean up Weekly Data to Keep Only the Adjusted Closing Prices at the End of Each Week
AMZN.weekly = AMZN.weekly[, 5]
names(AMZN.weekly) = paste("wk.Adjusted")
print(AMZN.weekly[c(1:3, nrow(AMZN.weekly)), ])

# Step 4: Calculate Weekly Returns
# what we calculate is Friday-to-Friday total return.
# Note that if Friday is a
# non-trading day, the weekly return will be calculated based on the price of the last
# trading day of this week relative to the price of the last trading day of the prior week.
AMZN.weekly$Ret = Delt(AMZN.weekly$wk.Adjusted)
print(AMZN.weekly[c(1:3, nrow(AMZN.weekly)), ])
# Note that the December 31, 2013 return is not a full week's return. The last Friday
# prior to the end of the year is on December 27, 2013.

# Step 5: Cleanup Weekly Returns Data by Deleting the First Observation
AMZN.weekly = AMZN.weekly[-1, 2]
print(AMZN.weekly[c(1:3, nrow(AMZN.weekly)), ])















