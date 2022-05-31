library(xts)
library(quantmod)

data.AMZN = read.csv("AMZN Yahoo.csv", header=TRUE)
date = as.Date(data.AMZN$Date, format="%Y-%m-%d")
data.AMZN = cbind(date, data.AMZN[, -1])
data.AMZN = data.AMZN[order(data.AMZN$date), ]
data.AMZN = xts(data.AMZN[, 2:7], order.by = data.AMZN[, 1])
names(data.AMZN) = paste(c("AMZN.Open", "AMZN.High", "AMZN.Low", "AMZN.Close", "AMZN.Adjusted", "AMZN.Volume"))
print(data.AMZN[c(1:3, nrow(data.AMZN)), ])

AMZN.bb = data.AMZN[, 4]
print(AMZN.bb[c(1:3, nrow(AMZN.bb)), ])

# Step 2: Calculate Rolling 20-Day Mean and Standard Deviation 
AMZN.bb$avg = rollmeanr(AMZN.bb$AMZN.Close, k = 20)
AMZN.bb$sd = rollapply(AMZN.bb$AMZN.Close, width = 20, FUN = sd, fill = NA)
print(AMZN.bb[c(1:3, nrow(AMZN.bb)), ])
print(AMZN.bb[18:22, ])

# Step 3: Subset to only show 2013 data 
AMZN.bb2013 = subset(AMZN.bb, index(AMZN.bb) >= "2013-01-01")
print(AMZN.bb2013[c(1:3, nrow(AMZN.bb2013)), ])

# Step 4: Calculate the Bollinger Bands 
AMZN.bb2013$sd2up = AMZN.bb2013$avg + 2 * AMZN.bb2013$sd
AMZN.bb2013$sd2down = AMZN.bb2013$avg - 2 * AMZN.bb2013$sd
print(AMZN.bb2013[c(1:3, nrow(AMZN.bb2013)), ])

# Step 5: Plot the Bollinger Bands 
y.range = range(AMZN.bb2013[, -3], na.rm = TRUE)
print(y.range)
plot(x = index(AMZN.bb2013), 
     xlab = "Date", 
     y = AMZN.bb2013$AMZN.Close, 
     ylim = y.range, 
     ylab = "Price ($)", 
     type = "l", 
     lwd = 3, 
     main = "Amazon - Bollinger Bands (20 days, 2 deviations)
     January 1, 2013 - December 31, 2013")
lines(x = index(AMZN.bb2013), y = AMZN.bb2013$avg, lty = 2)
lines(x = index(AMZN.bb2013), y = AMZN.bb2013$sd2up, col = "green")
lines(x = index(AMZN.bb2013), y = AMZN.bb2013$sd2down, col = "red")
legend("topleft", 
       c("Amazon Price", "20-Day Moving Average", "Upper Band", "Lower Band"), 
       lty = c(1, 2, 1, 1), 
       lwd = c(3, 1, 1, 1), 
       col = c("black", "black", "green", "red"))





