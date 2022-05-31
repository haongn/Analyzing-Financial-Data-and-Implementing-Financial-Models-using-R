library(xts)
library(quantmod)

data.AMZN = read.csv("AMZN Yahoo.csv", header=TRUE)
date = as.Date(data.AMZN$Date, format="%Y-%m-%d")
data.AMZN = cbind(date, data.AMZN[, -1])
data.AMZN = data.AMZN[order(data.AMZN$date), ]
data.AMZN = xts(data.AMZN[, 2:7], order.by = data.AMZN[, 1])
names(data.AMZN) = paste(c("AMZN.Open", "AMZN.High", "AMZN.Low", "AMZN.Close", "AMZN.Adjusted", "AMZN.Volume"))
print(data.AMZN[c(1:3, nrow(data.AMZN)), ])

chartSeries(data.AMZN[,4],
          theme="white.mono",
            TA=c(addSMA(n=c(50,200))))
zoomChart("2012::2013")