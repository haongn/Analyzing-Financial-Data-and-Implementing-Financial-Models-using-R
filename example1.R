data.AMZN = read.csv("AMZN Yahoo.csv", header=TRUE)
head(data.AMZN)
tail(data.AMZN)

class(data.AMZN$Date)
date = as.Date(data.AMZN$Date, format="%Y-%m-%d")
head(date)
tail(date)

# data.AMZN$Date = as.Date(data.AMZN$Date, format="%Y-%m-%d") (co the gan truc
# tep nhu nay)
data.AMZN = cbind(date, data.AMZN[, -1])
head(data.AMZN)
tail(data.AMZN)

# data.AMZN[, -1]: xoa cot 1 cua dataframe

# sap xep theo cot $date theo thu tu tang dan ve thoi gian
data.AMZN = data.AMZN[order(data.AMZN$date), ]

class(data.AMZN)

library(xts)
data.AMZN = xts(data.AMZN[, 2:7], order.by=data.AMZN[, 1])
head(data.AMZN)
tail(data.AMZN)
class(data.AMZN)
names(data.AMZN)  # ten cac bien (cot) cua dataframe 

# dat lai ten cot (ten bien)
names(data.AMZN) = paste(c("AMZN.Open", "AMZN.High", "AMZN.Low", "AMZN.Close", "AMZN.Adjusted", "AMZN.Volume"))
head(data.AMZN)

# ve do thi de check 
plot(data.AMZN$AMZN.Close)

data.missing = data.AMZN[-400:-500, ]
plot(data.missing$AMZN.Close)

dim(data.AMZN)

summary(data.AMZN)

AMZN.onlyFirst = data.AMZN[1, ]
AMZN.onlyFirst

AMZN.delFirst = data.AMZN[-1, ]
head(AMZN.delFirst)

# lay hang 1 va hang cuoi: sd dau c()
data.AMZN[c(1, nrow(data.AMZN)), ]


# lay tuan dau tien
AMZN.first.week = data.AMZN[2:6, ]
AMZN.first.week

names(data.AMZN) = paste(c("AMZN.Open", "AMZN.High", "AMZN.Low", "AMZN.Close", "AMZN.Ahjusted", "AMZN.Volume"))

# lay 30 ngay cuoi cung 
AMZN.last30 = data.AMZN[(nrow(data.AMZN) - 29):nrow(data.AMZN), ]
AMZN.last30
nrow(AMZN.last30)



# show 3 hang dau + hang cuoi 
data.AMZN[c(1:3, nrow(data.AMZN)), ]

# chon rieng mot cot 
AMZN.onlyPrice = data.AMZN[, 4]
AMZN.onlyPrice[c(1:3, nrow(AMZN.onlyPrice)), ]

AMZN.onlyPrice2 = data.AMZN$AMZN.Close
AMZN.onlyPrice2[c(1:3, nrow(AMZN.onlyPrice)), ]

# xoa cot 
AMZN.delAdjPrice = data.AMZN[, -5]
AMZN.delAdjPrice[c(1:3, nrow(AMZN.delAdjPrice)), ]

# chon hai cot khong lien tiep 
AMZN.OpenClose = data.AMZN[, c(1, 4)]
AMZN.OpenClose[c(1:3, nrow(AMZN.OpenClose)), ]

# chon hai cot lien tiep 
AMZN.PriceVol = data.AMZN[, 4:5]
AMZN.PriceVol[c(1:3, nrow(AMZN.PriceVol)), ]


# subsetting using dates 

# data is an xts object 
class(data.AMZN)
xts.2012= subset(data.AMZN[, 4], index(data.AMZN) >= "2012-01-01" & index(data.AMZN) <= "2012-12-31")
xts.2012[c(1:3, nrow(xts.2012)), ]

# data is an data-frame object
AMZN.2012 = cbind(index(data.AMZN), data.frame(data.AMZN[, 4]))
AMZN.2012[c(1:3, nrow(AMZN.2012)), ]
class(AMZN.2012)
names(AMZN.2012)
names(AMZN.2012)[1] = paste("date")
rownames(AMZN.2012) = seq(1, nrow(AMZN.2012), 1)
AMZN.2012[c(1:3, nrow(AMZN.2012)), ]

AMZN.2012 = subset(AMZN.2012, AMZN.2012$date >= "2012-01-01" & AMZN.2012$date <= "2012-12-31")
AMZN.2012[c(1:3, nrow(AMZN.2012)), ]

# converting to weekly prices 
class(data.AMZN)
wk = data.AMZN
data.weekly = to.weekly(wk)
data.weekly[c(1:3, nrow(data.weekly)), ]
data.AMZN[2:6, ]
sum(data.AMZN[2:6, 6])

# converting to monthly prices 
mo = data.AMZN
data.monthly = to.monthly(mo)
data.monthly[c(1:3, nrow(data.monthly)), ]

# plotting a candlestick chart using monthly data 
library(quantmod)
OHLC = data.monthly[-1, -6]
AMZN.ohlc = as.quantmod.OHLC(OHLC, col.names=c("Open", "High", "Low", "Close", "Volume"))
class(AMZN.ohlc)
AMZN.ohlc[c(1:3, nrow(AMZN.ohlc)), ]
chartSeries(AMZN.ohlc, theme="white.mono", name="AMZN OHLC")

































