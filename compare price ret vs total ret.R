library(xts)
library(quantmod)

data.IBM = read.csv("IBM Yahoo.csv", header = TRUE)
date = as.Date(data.IBM$Date, format = "%Y-%m-%d")
data.IBM = cbind(date, data.IBM[, -1])
data.IBM = data.IBM[order(data.IBM$date), ]
data.IBM = xts(data.IBM[, 2:7], order.by= data.IBM[, 1])
names(data.IBM) = paste(c("IBM.Open", "IBM.High", "IBM.Low", "IBM.Close", "IBM.Adjusted", "IBM.Volume"))

IBM.ret = data.IBM[, 4:5]
IBM.ret$IBM.tot.ret = Delt(IBM.ret$IBM.Adjusted)   # add one more column from IBM.Adjusted column
IBM.ret$IBM.prc.ret = Delt(IBM.ret$IBM.Close)
print(IBM.ret[c(1:3, nrow(IBM.ret)), ])

IBM.Ret = cbind(IBM.ret$IBM.prc.ret, IBM.ret$IBM.tot.ret)
names(IBM.Ret) = c("prc.ret", "tot.ret")           # doi lai ten cot 
print(IBM.Ret[c(1:3, nrow(IBM.Ret)), ])

# Step 2: Set First Returns to Zero
IBM.Ret$prc.ret[1] = 0 
IBM.Ret$tot.ret[1] = 0 
print(IBM.Ret[c(1:3, nrow(IBM.Ret)), ])

# Step 3: Calculate Gross Returns
IBM.Ret$gross.prc = 1 + IBM.Ret$prc.ret
IBM.Ret$gross.tot = 1 + IBM.Ret$tot.ret
print(IBM.Ret[c(1:3, nrow(IBM.Ret)), ])

# Step 4: Cumulate the Gross Returns
# su dung lenh cumprod 
IBM.Ret$cum.prc = cumprod(IBM.Ret$gross.prc)
IBM.Ret$cum.tot = cumprod(IBM.Ret$gross.tot)
print(IBM.Ret[c(1:3, nrow(IBM.Ret)), ])

# Step 5: Plot the Two Return Series 
# plot cum.prc and cum.tot variables 
y.range = range(IBM.Ret[, 5:6])
print(y.range)
plot(x = index(IBM.Ret),                     ## doan nay code chay co van de?? cu phap dung nhung hien thi thieu thong tin.
     y = IBM.Ret$cum.tot,                    ## thieu legend va duong thang h = 1.
     type = "l", 
     auto.grid = FALSE, 
     xlab = "Date", 
     ylab = "Value of Investment ($)", 
     ylim = y.range, 
     minor.ticks = FALSE, 
     main = "IBM Stock Performance Based on 
     Total Returns and Price Returns 
     December 31, 2010 - December 31, 2013")
lines(IBM.Ret$cum.prc, 
      type = "l", 
      lty = 3)
abline(h = 1.0, col = "black")
legend("bottomright", 
       col = c("black", "black"), 
       lty = c(1, 3), 
       c("Value Based on Total Return", 
         "Value Based on Price Return"))




















