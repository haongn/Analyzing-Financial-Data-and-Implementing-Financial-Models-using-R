# Step 1: Convert the Data into an xts Object
library(xts)
library(quantmod)
port.xts = xts(port.val[, 2:3], order.by = port.val[, 1])
print(port.xts[c(1:3, nrow(port.xts)), ])

port.xts$Lag.VW = Lag(port.xts$VW.cum, k = 1)
port.xts$Lag.EW = Lag(port.xts$EW.cum, k = 1)
print(port.xts[c(1:3, nrow(port.xts)), ])

# Step 2: Create EW and VW Returns (The lag values allows us to easily calculate the returns on each day.)
port.xts$VW.ret = port.xts$VW.cum / port.xts$Lag.VW - 1
port.xts$EW.ret = port.xts$EW.cum / port.xts$Lag.EW - 1
print(port.xts[c(1:3,nrow(port.xts)),])

# Step 3: Clean up the Data
# (only keep the portfolio values (VW.cum and EW.cum) and the portfolio returns (VW.ret and EW.ret).)
Port.Ret = port.xts[, c(1,2,5,6)]
print(Port.Ret[c(1:3,nrow(Port.Ret)), ])

# Step 4: Create a Date Variable in the Data Object
csv.port = cbind(data.frame(index(Port.Ret)), data.frame(Port.Ret))
names(csv.port)[1] = paste('date')
print(csv.port[c(1:3, nrow(csv.port)), ])

# Step 5: Replace Index with Observation Numbers
rownames(csv.port) = seq(1:nrow(csv.port))
print(csv.port[c(1:3, nrow(csv.port)), ])

# Step 6: Save the Data to a CSV File
write.csv(csv.port, 'Hypothetical Portfolio (Daily).csv')


















