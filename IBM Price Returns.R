library(quantmod)
library(xts)

data.IBM = read.csv("IBM Yahoo.csv", header = TRUE)  # doc file csv
date = as.Date(data.IBM$Date, format = "%Y-%m-%d")   # doi format cot ngay
data.IBM= cbind(date, data.IBM[, -1])                # ket hop cot date vao
data.IBM = data.IBM[order(data.IBM$date), ]          # sap xep lai ngay 
data.IBM = xts(data.IBM[, 2:7], order.by = data.IBM[, 1])
names(data.IBM) = paste(c("IBM.Open", "IBM.High", "IBM.Low", "IBM.Close", 
                          "IBM.Adjusted", "IBM.Volume"))
print(data.IBM[c(1:3, nrow(data.IBM)), ])

# Step 2: Subset the Data to Only Include the Closing Price
IBM.prc.ret = data.IBM[, 4]
print(IBM.prc.ret[c(1:3, nrow(IBM.prc.ret)), ])

# Step 3: Calculate IBM's Price Return
IBM.prc.ret$IBM.prc.ret = Delt(IBM.prc.ret$IBM.Close)   # create a new column 
print(IBM.prc.ret[c(1:3, nrow(IBM.prc.ret)), ])

# Step 4: Clean up Data Object
options(digits = 3)
IBM.prc.ret = IBM.prc.ret[-1, 2]
print(IBM.prc.ret[c(1:3, nrow(IBM.prc.ret)), ])
options(digits = 7)









