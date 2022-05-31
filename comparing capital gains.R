library(xts)
library(quantmod)

data.IBM = read.csv("IBM Yahoo.csv", header=TRUE)
date = as.Date(data.IBM$Date, format="%Y-%m-%d")
data.IBM = cbind(date, data.IBM[, -1])
data.IBM = data.IBM[order(data.IBM$date), ]
data.IBM = xts(data.IBM[, 2:7], order.by = data.IBM[, 1])
names(data.IBM) = paste(c("IBM.Open", "IBM.High", "IBM.Low", "IBM.Close", "IBM.Adjusted", "IBM.Volume"))
print(data.IBM[c(1:3, nrow(data.IBM)), ])

data.AMZN = read.csv("AMZN Yahoo.csv", header=TRUE)
date = as.Date(data.AMZN$Date, format="%Y-%m-%d")
data.AMZN = cbind(date, data.AMZN[, -1])
data.AMZN = data.AMZN[order(data.AMZN$date), ]
data.AMZN = xts(data.AMZN[, 2:7], order.by = data.AMZN[, 1])
names(data.AMZN) = paste(c("AMZN.Open", "AMZN.High", "AMZN.Low", "AMZN.Close", "AMZN.Adjusted", "AMZN.Volume"))
print(data.AMZN[c(1:3, nrow(data.AMZN)), ])

# DL cua S&P500 khong download duoc, phai cop bang tay 
data.GSPC = read.csv("GSPC Yahoo1.csv", header=TRUE)
date = as.Date(data.GSPC$Date, format="%d-%m-%Y")
data.GSPC = cbind(date, data.GSPC[, -1])
data.GSPC = data.GSPC[-nrow(data.GSPC), ]
data.GSPC = data.GSPC[order(data.GSPC$date), ]
data.GSPC = xts(data.GSPC[, 2:7], order.by = data.GSPC[, 1])
names(data.GSPC) = paste(c("GSPC.Open", "GSPC.High", "GSPC.Low", "GSPC.Close", "GSPC.Adjusted", "GSPC.Volume"))
print(data.GSPC[c(1:3, nrow(data.GSPC)), ])

# STEP 2: Combine data into one data object 

Close.Prices = data.AMZN$AMZN.Close
Close.Prices = cbind(Close.Prices, data.GSPC$GSPC.Close, data.IBM$IBM.Close)
Close.Prices = Close.Prices[-2, ]
print(Close.Prices[c(1:3, nrow(Close.Prices)), ])


# STEP 3: Convert data into a data.frame
multi.df = cbind(index(Close.Prices), data.frame(Close.Prices))
names(multi.df) = paste(c("date","AMZN","GSPC","IBM"))
rownames(multi.df) = seq(1,nrow(multi.df), 1)
multi.df[c(1:3, nrow(multi.df)), ]

# STEP 4: Calculate normalized values for each security 
multi.df$AMZN.idx = multi.df$AMZN / multi.df$AMZN[1]
multi.df$GSPC.idx = multi.df$GSPC / multi.df$GSPC[1]
multi.df$IBM.idx = multi.df$IBM / multi.df$IBM[1]
options(digits=5)
multi.df[c(1:3,nrow(multi.df)), ]
options(digits = 7)

# Step 5: Plot the Capital Appreciation of Each Security
plot(x=multi.df$date, y=multi.df$GSPC.idx, 
     type="l", 
     xlab="Date", 
     ylab="Value of Investment ($)", 
     col="black", 
     lty=1, 
     lwd=2, 
     main="Value of $1 Investment in AMZN, IBM, and the S&P500 Index 
December 31, 2010 - December 31, 2013")

# add more lines: 
lines(x=multi.df$date, 
      y=multi.df$AMZN.idx, 
      col="black", 
      lty=2, 
      lwd=1)
lines(x=multi.df$date, 
      y=multi.df$IBM.idx, 
      col="gray", 
      lty=2, 
      lwd=1)

abline(h=1, lty=1, col="black")

# add legend: 
legend("topleft", 
       c("AMZN", "IBM", "S&P 500 Index"), 
       col=c("black", "gray", "black"), 
       lty=c(2, 2, 1), 
       lwd=c(1, 1, 2))


# Step 6: Fix the y-axis to Encompass Range of Normalized Values for All Securities
y.range = c(0.6668671, 2.4564041)
plot(x=multi.df$date,
     y=multi.df$GSPC.idx,
     type="l",
     xlab="Date",
     ylim=y.range,
     ylab="Value of Investment ($)",
     col="black",
     lty=1,
     lwd=2,
     main="Value of $1 Investment in AMZN, IBM, YHOO, and the S&P 500 Index
December 31, 2010 - December 31, 2013")

lines(x=multi.df$date,
        y=multi.df$AMZN.idx,
        col="black",
        lty=2,
        lwd=1)
lines(x=multi.df$date,
        y=multi.df$IBM.idx,
        col="gray",
        lty=2,
        lwd=1)

abline(h=1,lty=1,col="black")

legend("topleft",c("AMZN","IBM","S&P 500 Index"),
        col=c("black","gray","gray","black"),
        lty=c(2,2,1),
        lwd=c(1,1,2))

# Alternative presentation
par(oma=c(0, 0, 3, 0))
par(mfrow=c(2, 2))

plot(x=multi.df$date,
     xlab="",
     y=multi.df$AMZN.idx,
     ylim=y.range,
     ylab="",
     type="l",
     col="gray",
     main="Amazon Stock")
lines(x=multi.df$date,y=multi.df$GSPC.idx,col="gray")
lines(x=multi.df$date,y=multi.df$IBM.idx,col="gray")
lines(x=multi.df$date,y=multi.df$AMZN.idx,col="black",lwd=2)
abline(h=1)

plot(x=multi.df$date,
    xlab="",
      y=multi.df$IBM.idx,
      ylim=y.range,
      ylab="",
      type="l",
      col="gray",
      main="IBM Stock")

lines(x=multi.df$date,y=multi.df$AMZN.idx,col="gray")
lines(x=multi.df$date,y=multi.df$GSPC.idx,col="gray")
lines(x=multi.df$date,y=multi.df$IBM.idx,col="black",lwd=2)
abline(h=1)

plot(x=multi.df$date,
       xlab="",
       y=multi.df$GSPC.idx,
       ylim=y.range,
       ylab="",
       type="l",
       col="gray",
       main="GSPC Stock")
lines(x=multi.df$date,y=multi.df$AMZN.idx,col="gray")
lines(x=multi.df$date,y=multi.df$IBM.idx,col="gray")
lines(x=multi.df$date,y=multi.df$GSPC.idx,col="black",lwd=2)
abline(h=1)

title1="Value of $1 Invested in Amazon, IBM, Yahoo, and the Market"
title2="December 31, 2010 - December 31, 2013"
title(main=paste(title1,"\n",title2),outer=T)





























