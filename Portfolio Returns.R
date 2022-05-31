library(xts)
library(quantmod)

# Step 1: Importing Price Data
data.AMZN = read.csv("AMZN Yahoo.csv", header = TRUE)
date = as.Date(data.AMZN$Date, format = "%Y-%m-%d")
data.AMZN = cbind(date, data.AMZN[, -1])
data.AMZN = data.AMZN[order(data.AMZN$date), ]
data.AMZN = xts(data.AMZN[, 2:7], order.by = data.AMZN[, 1])
names(data.AMZN) = paste(c("AMZN.Open", "AMZN.High", "AMZN.Low", "AMZN.Close", "AMZN.Adjust", "AMZN.Volume"))
print(data.AMZN[c(1:3, nrow(data.AMZN)), ])

data.IBM = read.csv("IBM Yahoo.csv", header = TRUE)
date = as.Date(data.IBM$Date, format = "%Y-%m-%d")    # doi ngay 
data.IBM = cbind(date, data.IBM[, -1])                # ket hop data frame
data.IBM = data.IBM[order(data.IBM$date), ]           # sap xep lai ngay 
data.IBM = xts(data.IBM[, 2:7], order.by = data.IBM[, 1])   # doi dataframe thanh xts object 
names(data.IBM) = paste(c("IBM.Open", "IBM.High", "IBM.Low", "IBM.Close", "IBM.Adjusted", "IBM.Volume"))
print(data.IBM[c(1:3, nrow(data.IBM)), ])

# DL cua S&P500 khong download duoc, phai cop bang tay 
data.GSPC = read.csv("GSPC Yahoo1.csv", header=TRUE)
date = as.Date(data.GSPC$Date, format="%d-%m-%Y")
data.GSPC = cbind(date, data.GSPC[, -1])
data.GSPC = data.GSPC[-nrow(data.GSPC), ]
data.GSPC = data.GSPC[order(data.GSPC$date), ]
data.GSPC = xts(data.GSPC[, 2:7], order.by = data.GSPC[, 1])
names(data.GSPC) = paste(c("GSPC.Open", "GSPC.High", "GSPC.Low", "GSPC.Close", "GSPC.Adjusted", "GSPC.Volume"))
print(data.GSPC[c(1:3, nrow(data.GSPC)), ])

# MSFT 
data.MSFT = read.csv("MSFT Yahoo.csv", header=TRUE)
date = as.Date(data.MSFT$Date, format="%Y-%m-%d")
data.MSFT = cbind(date, data.MSFT[, -1])
data.MSFT = data.MSFT[order(data.MSFT$date), ]
data.MSFT = xts(data.MSFT[, 2:7], order.by = data.MSFT[, 1])
names(data.MSFT) = paste(c("MSFT.Open", "MSFT.High", "MSFT.Low", "MSFT.Close", "MSFT.Adjusted", "MSFT.Volume"))
print(data.MSFT[c(1:3, nrow(data.MSFT)), ])

# step 2: combining data: we hold only adjusted closing price for the four securities 
multi = data.AMZN[, 5]
multi = merge(multi, data.GSPC[, 5])
multi = merge(multi, data.MSFT[, 5])
multi = merge(multi, data.IBM[, 5])
print(multi[c(1:3, nrow(multi)), ])

multi.df = cbind(data.frame(index(multi)), 
                 data.frame(multi))
print(class(multi.df))
names(multi.df) = paste(c("date", "AMZN", "GSPC", "MSFT", "IBM"))    ## the cot dau tien mac dinh la cot index???
print(multi.df[c(1:3, nrow(multi.df)), ])

# Step 4: Constructing Normalized Values for Each Security
multi.df$AMZN.idx = multi.df$AMZN / multi.df$AMZN[1]
multi.df$GSPC.idx = multi.df$GSPC / multi.df$GSPC[1]
multi.df$MSFT.idx = multi.df$MSFT / multi.df$MSFT[1]
multi.df$IBM.idx = multi.df$IBM / multi.df$IBM[1]
multi.df[c(1:3,nrow(multi.df)), 6:9]
print(multi.df[c(1:3, nrow(multi.df)), ])
# we invested $ 50,000 in AMZN, $ 10,000 in GSPC, $ 30,000 in MSFT, and $ 10,000 in IBM. How much would
# our portfolio return be as of December 31, 2013 if we made the investment in those securities on December 31, 2010?

# Step 1: Find First and Last Adjusted Closing Price for Each Security Over the Investment Period
period.ret = multi[c(1, nrow(multi))]
print(period.ret)

# Step 2: Calculate Returns for Each Security Over the Investment Period
rets = lapply(period.ret, Delt)   # net total return 
print(rets)
class(rets)

# Step 3: Convert to a data.frame and Clean up Data
rets = data.frame(rets)
rets

rets = rets[2, ] * 100
names(rets) = paste(c("AMZN", "GSPC", "MSFT", "IBM"))
print(rets)

# Step 4: Calculate Weight of Each Security in the Portfolio
i.AMZN = 50000
i.GSPC = 10000
i.MSFT = 30000
i.IBM = 10000
w.AMZN = i.AMZN / (i.AMZN + i.GSPC + i.MSFT + i.IBM)
w.GSPC = i.GSPC / (i.AMZN + i.GSPC + i.MSFT + i.IBM)
w.MSFT = i.MSFT / (i.AMZN + i.GSPC + i.MSFT + i.IBM)
w.IBM = i.IBM / (i.AMZN + i.GSPC + i.MSFT + i.IBM)
print(w.AMZN)
print(w.GSPC)
print(w.MSFT)
print(w.IBM)

# Step 5: Calculate Portfolio Return
port.ret.4asset = w.AMZN * rets$AMZN + w.GSPC * rets$GSPC + w.MSFT * rets$MSFT + w.IBM * rets$IBM
print(port.ret.4asset)

# C2: Matrix Algebra: nhan hai ma tran hoac nhan vo huong hai vector deu duoc. 



































