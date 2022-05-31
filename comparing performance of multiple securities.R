# AMZN and YHOO have not paid dividends, but IBM has. 
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

# Step 3: Converting Data into a data.frame Object: for a workable date variable and shortened names for the
#                                                   four adjusted closing price variables
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

# Step 5: Plotting the Index Values of Each Security
y.range = range(multi.df[, 6:9])
print(y.range)
par(mfrow = c(1, 1))
plot(x = multi.df$date, 
     xlab = "Date", 
     y = multi.df$GSPC.idx, 
     ylab = "Value of $1 Investment ($)", 
     ylim = y.range, 
     type = "l", 
     col = "black", 
     lty = 1, 
     lwd = 2, 
     main = "Value of $1 Investment in AMZN, IBM, MSFT, and the S&P 500 Index Based on Total Returns 
             December 31, 2010 - December 31, 2013")
lines(x = multi.df$date,                                   # add one more line to the above plot 
      y = multi.df$AMZN.idx, 
      col = "black", 
      lty = 2, 
      lwd = 1)
lines(x = multi.df$date, 
      y = multi.df$IBM.idx, 
      col = "gray40", 
      lty = 1, 
      lwd = 2)
lines(x = multi.df$date, 
      y = multi.df$MSFT.idx, 
      col = "gray60", 
      lty = 1, 
      lwd = 1)
abline(h = 1, lty = 1, col = "black")
legend("topleft", 
       c("AMZN", "IBM", "MSFT", "S&P 500 Index"), 
       col = c("black", "gray40", "gray60", "black"), 
       lty = c(2, 1, 1, 1), 
       lwd = c(1, 2, 1, 2))














