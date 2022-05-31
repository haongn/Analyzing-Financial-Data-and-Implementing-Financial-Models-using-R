library(xts)
library(quantmod)

data.IBM = read.csv("IBM Yahoo.csv", header = TRUE)
date = as.Date(data.IBM$Date, format = "%Y-%m-%d")
data.IBM = cbind(date, data.IBM[, -1])
data.IBM = data.IBM[order(data.IBM$date), ]
data.IBM = xts(data.IBM[, 2:7], order.by= data.IBM[, 1])
names(data.IBM) = paste(c("IBM.Open", "IBM.High", "IBM.Low", "IBM.Close", "IBM.Adjusted", "IBM.Volume"))
print(data.IBM[715:720, ])


IBM.ret = data.IBM[, 5]
print(IBM.ret[c(1:3, nrow(IBM.ret)), ])

# Step 2: Calculate Total Return
IBM.ret$IBM.tot.ret = Delt(IBM.ret$IBM.Adjusted)   # add one more column from IBM.Adjusted column
print(IBM.ret[c(1:3, nrow(IBM.ret)), ])

# Step 3: Clean up the Data
options(digits = 3)
IBM.log.ret = IBM.ret[, 2]
print(IBM.log.ret[c(1:3, nrow(IBM.log.ret)), ])

