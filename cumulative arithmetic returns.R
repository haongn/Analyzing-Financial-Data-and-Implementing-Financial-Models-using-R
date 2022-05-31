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

IBM.acum = IBM.ret 
print(IBM.acum[c(1:3, nrow(IBM.acum)), ])

# Step 2: Set First Day Total Return Value to Zero
IBM.acum[1, 1] = 0 
print(IBM.acum[c(1:3, nrow(IBM.acum)), ])

# Step 3: Calculate Gross Daily Returns: tinh tong loi nhuan cua tung ngay  
IBM.acum$GrossRet = 1 + IBM.acum$IBM.tot.ret
print(IBM.acum[c(1:3, nrow(IBM.acum)), ])

# Step 4: Calculate Cumulative Gross Returns: tinh tong loi nhuan tich luy hang ngay 
IBM.acum$GrossCum = cumprod(IBM.acum$GrossRet)  # su dung ham cumprod 
print(IBM.acum[c(1:3, nrow(IBM.acum)), ])

# Step 5: Convert Cumulative Gross Returns to Cumulative Net Returns
IBM.acum$NetCum = IBM.acum$GrossCum - 1
print(IBM.acum[c(1:10, nrow(IBM.acum)), ])


























