library(xts)
library(quantmod)

data.IBM = read.csv("IBM Yahoo.csv", header = TRUE)
date = as.Date(data.IBM$Date, format = "%Y-%m-%d")
data.IBM = cbind(date, data.IBM[, -1])
data.IBM = data.IBM[order(data.IBM$date), ]
data.IBM = xts(data.IBM[, 2:7], order.by= data.IBM[, 1])
names(data.IBM) = paste(c("IBM.Open", "IBM.High", "IBM.Low", "IBM.Close", "IBM.Adjusted", "IBM.Volume"))

# calculate arithmetic return 
IBM.ret = data.IBM[, 5]   # adjusted price
IBM.ret$IBM.tot.ret = Delt(IBM.ret$IBM.Adjusted)
IBM.ret = IBM.ret[, 2]
print(IBM.ret[c(1:3,nrow(IBM.ret)), ])


# Step 2: Calculate log returns: use "diff" + "log" commands
IBM.log.ret = data.IBM[, 5]
IBM.log.ret$IBM.log.ret = diff(log(IBM.log.ret$IBM.Adjusted))
print(IBM.log.ret[c(1:3, nrow(IBM.log.ret)), ])

# Step 3: Clean up the data 
# we keep only the logarithmic return column 
options(digits = 3)
IBM.log.ret = IBM.log.ret[, 2]
print(IBM.log.ret[c(1:3, nrow(IBM.log.ret)), ])

# Compare Log Returns with Arithmetic Returns
options(digits = 3, scipen = 100)
tot.rets = cbind(IBM.ret, IBM.log.ret)   # ket hop hai cot lai tao thanh mot xts zoo moi
class(tot.rets)
tot.rets[c(1:3, nrow(tot.rets)), ]

print(max(abs(tot.rets$IBM.tot.ret - tot.rets$IBM.log.ret), na.rm = TRUE))
print(min(abs(tot.rets[, 1] - tot.rets[, 2]), na.rm = TRUE))

options(digits = 7, scipen = 0)













