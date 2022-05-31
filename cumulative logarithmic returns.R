library(xts)
library(quantmod)

data.IBM = read.csv("IBM Yahoo.csv", header = TRUE)
date = as.Date(data.IBM$Date, format = "%Y-%m-%d")
data.IBM = cbind(date, data.IBM[, -1])
data.IBM = data.IBM[order(data.IBM$date), ]
data.IBM = xts(data.IBM[, 2:7], order.by= data.IBM[, 1])
names(data.IBM) = paste(c("IBM.Open", "IBM.High", "IBM.Low", "IBM.Close", "IBM.Adjusted", "IBM.Volume"))

# Step 2: Calculate log returns: use "diff" + "log" commands
IBM.log.ret = data.IBM[, 5]
IBM.log.ret$IBM.log.ret = diff(log(IBM.log.ret$IBM.Adjusted))
IBM.log.ret = IBM.log.ret[, 2]
print(IBM.log.ret[c(1:3, nrow(IBM.log.ret)), ])

IBM.logcum = IBM.log.ret
print(IBM.logcum[c(1:3, nrow(IBM.logcum)), ])


# Step 2: Set the First Log Return to Zero
IBM.logcum[1, 1] = 0 
print(IBM.logcum[c(1:3, nrow(IBM.logcum)), ])

# Step 3: Take the Sum of all Logarithmic Returns During the Investment Period
logcumret = sum(IBM.logcum$IBM.log.ret)
print(logcumret)

# Step 4: Convert Log Return Back to Arithmetic Return
cumret = exp(logcumret) - 1
print(cumret)

# more: show daily cumulative returns: sd ham cumsum  
IBM.logcum$IBM.cum.log.ret = cumsum(IBM.logcum$IBM.log.ret)
print(IBM.logcum[c(1:3, nrow(IBM.logcum)), ])

# convert to cumulative arithmetic return 
IBM.logcum$cum.arth.ret = exp(IBM.logcum$IBM.cum.log.ret) - 1
print(IBM.logcum[c(1:3, nrow(IBM.logcum)), ])




















