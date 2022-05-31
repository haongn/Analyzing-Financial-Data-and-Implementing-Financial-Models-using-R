library(xts)
library(quantmod)

# Step 1: Importing Price Data
data.AMZN = read.csv("AMZN Yahoo.csv", header = TRUE)
date = as.Date(data.AMZN$Date, format = "%Y-%m-%d")
data.AMZN = cbind(date, data.AMZN[, -1])
data.AMZN = data.AMZN[order(data.AMZN$date), ]
data.AMZN = xts(data.AMZN[, 2:7], order.by = data.AMZN[, 1])
names(data.AMZN) = paste(c("AMZN.Open", "AMZN.High", "AMZN.Low", "AMZN.Close", "AMZN.Adjusted", "AMZN.Volume"))
print(data.AMZN[c(1:3, nrow(data.AMZN)), ])

data.IBM = read.csv("IBM Yahoo.csv", header = TRUE)
date = as.Date(data.IBM$Date, format = "%Y-%m-%d")    # doi ngay 
data.IBM = cbind(date, data.IBM[, -1])                # ket hop data frame
data.IBM = data.IBM[order(data.IBM$date), ]           # sap xep lai ngay 
data.IBM = xts(data.IBM[, 2:7], order.by = data.IBM[, 1])   # doi dataframe thanh xts object 
names(data.IBM) = paste(c("IBM.Open", "IBM.High", "IBM.Low", "IBM.Close", "IBM.Adjusted", "IBM.Volume"))
print(data.IBM[c(1:3, nrow(data.IBM)), ])

# MSFT 
data.MSFT = read.csv("MSFT Yahoo.csv", header=TRUE)
date = as.Date(data.MSFT$Date, format="%Y-%m-%d")
data.MSFT = cbind(date, data.MSFT[, -1])
data.MSFT = data.MSFT[order(data.MSFT$date), ]
data.MSFT = xts(data.MSFT[, 2:7], order.by = data.MSFT[, 1])
names(data.MSFT) = paste(c("MSFT.Open", "MSFT.High", "MSFT.Low", "MSFT.Close", "MSFT.Adjusted", "MSFT.Volume"))
print(data.MSFT[c(1:3, nrow(data.MSFT)), ])

# Step 2: Create Object with Only the Relevant Data: adjusted and closing prices of 3 securities 
port = data.AMZN[, 4:5]
port = merge(port, data.MSFT[, 4:5])
port = merge(port, data.IBM[, 4:5])
print(port[c(1:3, nrow(port)), ])

# Step 3: Calculate Returns of Each Security: using adjusted close prices to calculate total returns
port$AMZN.ret = Delt(port$AMZN.Adjusted)
port$MSFT.ret = Delt(port$MSFT.Adjusted)
port$IBM.ret = Delt(port$IBM.Adjusted)
print(port[c(1:3, nrow(port)), ])

# Step 4: Convert to data.frame Object and Subset Data
port = cbind(data.frame(index(port)), 
             data.frame(port))
names(port)[1] = paste("date")
print(port[c(1:3, nrow(port)), ])

port = subset(port, 
              port$date >= "2012-12-31" & port$date <= "2013-12-31")
print(port[c(1:3, nrow(port)), ])

# Step 1: Keep Only Variables We Need to Construct VW Portfolio
vwport = port[, c(1, 2, 4, 6, 8:10)]
print(vwport[c(1:3, nrow(ewport)), ])
print(vwport[c(1:3, nrow(vwport)), ])

# change the index of vwport to an indicator for the observation number 
rownames(vwport) = seq(1: nrow(vwport))
print(vwport[c(1:3, nrow(vwport)), ])

# Step 2: Converting Net Returns to Gross Returns: we overwrite the net returns with gross returns.
vwport$AMZN.ret = 1 + vwport$AMZN.ret
vwport$MSFT.ret = 1 + vwport$MSFT.ret
vwport$IBM.ret = 1 + vwport$IBM.ret
print(vwport[c(1:3,nrow(vwport)), ])

# Step 3: Calculate the Market Capitalization of Each Security in the Portfolio
# Construct Series of Calendar Days
date = seq(as.Date("2012-12-31"), as.Date("2013-12-31"), by = 1)
date = data.frame(date)
print(date[c(1:3, nrow(date)), ])

# Create Data Object With Daily Prices, Filling in Last Available Price on Non-trading Days
PRICE.qtr = vwport[, c(1, 2, 3, 4)]
print(PRICE.qtr[c(1:3, nrow(PRICE.qtr)), ])

PRICE.qtr = na.locf(merge(x = date, y = PRICE.qtr, by = "date", all.x = TRUE))
print(PRICE.qtr[c(1:3, nrow(PRICE.qtr)), ])

# Keep Only Prices at the End of Each Calendar Quarter
PRICE.qtr = subset(PRICE.qtr, 
                   PRICE.qtr$date == as.Date("2012-12-31") |
                   PRICE.qtr$date == as.Date("2013-03-31") |
                   PRICE.qtr$date == as.Date("2013-06-30") |
                   PRICE.qtr$date == as.Date("2013-09-30"))
print(PRICE.qtr)

# Obtain Shares Outstanding Data from SEC Filings
PRICE.qtr$AMZN.shout = c(454000000, 455000000, 457000000, 458000000)
PRICE.qtr$MSFT.shout = c(1115233000,1084766000,1065046000,1013059000)   ## this is make-up 
PRICE.qtr$IBM.shout = c(1117367676, 1108794396, 1095425823, 1085854383)
print(PRICE.qtr)

# Calculate Market Capitalization of Each Security
print(str(PRICE.qtr))
weights = PRICE.qtr
weights$AMZN.mcap = weights$AMZN.Close * weights$AMZN.shout 
weights$MSFT.mcap = weights$MSFT.Close * weights$MSFT.shout
weights$IBM.mcap = weights$IBM.Close * weights$IBM.shout
print(weights)

# Calculate Quarter-end Aggregate Market Capitalization
weights$tot.mcap = rowSums(weights[8:10])
print(weights)

# Step 4: Calculate Quarter-end Weights of Each Security in the Portfolio
weights$AMZN.wgt = weights$AMZN.mcap / weights$tot.mcap
weights$MSFT.wgt = weights$MSFT.mcap / weights$tot.mcap
weights$IBM.wgt = weights$IBM.mcap / weights$tot.mcap
print(weights)

# keeping only the date and the three weight variables in weights.
WEIGHT = weights[, c(1, 12:14)]
print(WEIGHT)

# add one to all dates
WEIGHT$date = WEIGHT$date + 1
print(WEIGHT)

# Step 5: Calculating the Quarterly VW Portfolio Values
# we calculate the gross returns for each quarter and then apply to it the allocated value of each security
# based on its quarter-end weights
date = seq(as.Date("2012-12-31"), as.Date("2013-12-31"), by = 1)
date = data.frame(date)
print(date[c(1:3, nrow(date)), ])

# We then merge in the WEIGHT into date using a combination of the na.locf command and merge command.
vwret = na.locf(merge(date, WEIGHT, by = "date", all.x = TRUE))
print(vwret[c(1:3, nrow(vwret)), ])

# Next, we extract the beginning of the quarter weights from vwret
q1.vw.wgt = subset(vwret, vwret$date == as.Date("2013-01-01"))
q2.vw.wgt = subset(vwret, vwret$date == as.Date("2013-04-01"))
q3.vw.wgt = subset(vwret, vwret$date == as.Date("2013-07-01"))
q4.vw.wgt = subset(vwret, vwret$date == as.Date("2013-10-01"))

print(q1.vw.wgt)
print(q2.vw.wgt)
print(q3.vw.wgt) 
print(q4.vw.wgt) 

# Step 6: Create Pie Charts of the Weights
par(mfrow = c(2, 2))
Q1.pie.values = as.numeric(q1.vw.wgt[, -1])  # chi can 3 cot phan tram la duoc => bo cot date
Q1.pie.labels = names(q1.vw.wgt[, -1])
pie(Q1.pie.values, 
    labels = Q1.pie.labels, 
    col = c("black", "gray", "gray40"), 
    main = "Q1 Value Weighting")

Q2.pie.values = as.numeric(q2.vw.wgt[,-1])
Q2.pie.labels = names(q1.vw.wgt[,-1])
pie(Q2.pie.values,
    labels = Q2.pie.labels,
    col = c("black","gray","gray40"),
    main = "Q2 Value Weighting")

Q3.pie.values = as.numeric(q3.vw.wgt[,-1])
Q3.pie.labels = c("Amazon","Yahoo","IBM")
pct = round(Q3.pie.values * 100)
Q3.pie.labels = paste(Q3.pie.labels,pct)             # Add Pct to Labels
Q3.pie.labels = paste(Q3.pie.labels,"%",sep="")      # Add % Sign
pie(Q3.pie.values,
    labels = Q3.pie.labels,
    col = c("black","gray","gray40"),
    main = "Q3 Value Weighting")

Q4.pie.values = as.numeric(q4.vw.wgt[,-1])
Q4.pie.labels = c("Amazon","Yahoo","IBM")
pct = round(Q4.pie.values * 100)
Q4.pie.labels = paste(Q4.pie.labels,pct)              # Add Pct to Labels
Q4.pie.labels = paste(Q4.pie.labels,"%",sep="")       # Add % Sign
pie(Q4.pie.values,
    labels = Q4.pie.labels,
    col = c("black","white","gray40"),
    main = "Q4 Value Weighting")

# Step7: Calculating VW Portfolio Values for 1Q 2013
vw.q1 = subset(vwport[, c(1, 5:7)], 
               vwport$date >= as.Date("2012-12-31") & 
               vwport$date <= as.Date("2013-03-31"))
print(vw.q1[c(1:3, nrow(vw.q1)), ])

names(vw.q1) = paste(c("date", "AMZN", "MSFT", "IBM"))
vw.q1[c(1:3, nrow(vw.q1)), ]

# calculate the cumulative gross return for each security
vw.q1[1, 2:4] = 1
vw.q1$AMZN = cumprod(vw.q1$AMZN)
vw.q1$MSFT = cumprod(vw.q1$MSFT)
vw.q1$IBM = cumprod(vw.q1$IBM)
print(vw.q1[c(1:3, nrow(vw.q1)), ])

vw.q1$AMZN.idx = (1 * q1.vw.wgt$AMZN.wgt) * vw.q1$AMZN
vw.q1$MSFT.idx = (1 * q1.vw.wgt$MSFT.wgt) * vw.q1$MSFT
vw.q1$IBM.idx = (1 * q1.vw.wgt$IBM.wgt) * vw.q1$IBM
print(vw.q1[c(1:3,nrow(vw.q1)),])

# calculate the daily VW portfolio values by summing the daily index values of the three securities
q1.vw.val = data.frame(rowSums(vw.q1[, 5:7]))
print(q1.vw.val[c(1:3, nrow(q1.vw.val)), ])

names(q1.vw.val) = paste("port.val")
q1.vw.val$date = vw.q1$date                # add a date column 
print(q1.vw.val[c(1:3,nrow(q1.vw.val)),])

q2.vw.inv = q1.vw.val[nrow(q1.vw.val), 1]  # gia tri cua danh muc dau tu o quy 1
print(q2.vw.inv)

# Step 8: Calculating VW Portfolio Values for 2Q 2013
vw.q2 = subset(vwport[, c(1,5:7)],
              vwport$date >= as.Date("2013-04-01") &
              vwport$date <= as.Date("2013-06-30"))
print(vw.q2[c(1:3,nrow(vw.q2)),])

names(vw.q2) = paste(c("date", "AMZN", "MSFT", "IBM"))
print(vw.q2[c(1:3,nrow(vw.q2)),])

vw.q2$AMZN = cumprod(vw.q2$AMZN)
vw.q2$MSFT = cumprod(vw.q2$MSFT)
vw.q2$IBM = cumprod(vw.q2$IBM)
print(vw.q2[c(1:3, nrow(vw.q2)), ])

vw.q2$AMZN.idx = (q2.vw.inv * q2.vw.wgt$AMZN.wgt) * vw.q2$AMZN
vw.q2$MSFT.idx = (q2.vw.inv * q2.vw.wgt$MSFT.wgt) * vw.q2$MSFT
vw.q2$IBM.idx = (q2.vw.inv * q2.vw.wgt$IBM.wgt) * vw.q2$IBM
print(vw.q2[c(1:3,nrow(vw.q2)), ])

q2.vw.val = data.frame(rowSums(vw.q2[,5:7]))
print(q2.vw.val[c(1:3,nrow(q2.vw.val)), ])

names(q2.vw.val) = paste("port.val")
q2.vw.val$date = vw.q2$date
print(q2.vw.val[c(1:3,nrow(q2.vw.val)), ])

q3.vw.inv<-q2.vw.val[nrow(q2.vw.val), 1]
print(q3.vw.inv)

# Step 9: Calculating VW Portfolio Values for 3Q 2013
vw.q3 = subset(vwport[,c(1, 5:7)],
              vwport$date >= as.Date("2013-07-01") &
              vwport$date <= as.Date("2013-09-30"))
print(vw.q3[c(1:3,nrow(vw.q3)),])

names(vw.q3) = paste(c("date", "AMZN", "MSFT", "IBM"))
print(vw.q3[c(1:3,nrow(vw.q3)),])

vw.q3$AMZN = cumprod(vw.q3$AMZN)
vw.q3$MSFT = cumprod(vw.q3$MSFT)
vw.q3$IBM = cumprod(vw.q3$IBM)
print(vw.q3[c(1:3, nrow(vw.q3)), ])

vw.q3$AMZN.idx = (q3.vw.inv * q3.vw.wgt$AMZN.wgt) * vw.q3$AMZN
vw.q3$MSFT.idx = (q3.vw.inv * q3.vw.wgt$MSFT.wgt) * vw.q3$MSFT
vw.q3$IBM.idx = (q3.vw.inv * q3.vw.wgt$IBM.wgt) * vw.q3$IBM
print(vw.q3[c(1:3,nrow(vw.q3)), ])

q3.vw.val = data.frame(rowSums(vw.q3[,5:7]))
print(q3.vw.val[c(1:3, nrow(q3.vw.val)), ])

names(q3.vw.val) = paste("port.val")
q3.vw.val$date = vw.q3$date
print(q3.vw.val[c(1:3,nrow(q3.vw.val)), ])

q4.vw.inv = q3.vw.val[nrow(q3.vw.val), 1]
print(q4.vw.inv)

# Step 10: Calculating VW Portfolio Values for 4Q 2013
vw.q4 = subset(vwport[,c(1, 5:7)],
               vwport$date >= as.Date("2013-10-01") &
               vwport$date <= as.Date("2013-12-31"))
print(vw.q4[c(1:3, nrow(vw.q4)),])

names(vw.q4) = paste(c("date", "AMZN", "MSFT", "IBM"))
print(vw.q4[c(1:3,nrow(vw.q4)),])

vw.q4$AMZN = cumprod(vw.q4$AMZN)
vw.q4$MSFT = cumprod(vw.q4$MSFT)
vw.q4$IBM = cumprod(vw.q4$IBM)
print(vw.q4[c(1:3, nrow(vw.q4)), ])

vw.q4$AMZN.idx = (q4.vw.inv * q4.vw.wgt$AMZN.wgt) * vw.q4$AMZN
vw.q4$MSFT.idx = (q4.vw.inv * q4.vw.wgt$MSFT.wgt) * vw.q4$MSFT
vw.q4$IBM.idx = (q4.vw.inv * q4.vw.wgt$IBM.wgt) * vw.q4$IBM
print(vw.q4[c(1:3, nrow(vw.q4)), ])

q4.vw.val = data.frame(rowSums(vw.q4[,5:7]))
print(q4.vw.val[c(1:3, nrow(q4.vw.val)), ])

names(q4.vw.val) = paste("port.val")
q4.vw.val$date = vw.q4$date
print(q4.vw.val[c(1:3,nrow(q4.vw.val)), ])

# Step 11: Combining Quarterly VW Portfolio Values into One Data Object 
vw.portval = rbind(q1.vw.val,q2.vw.val,q3.vw.val,q4.vw.val)
print(vw.portval[c(1:3,nrow(vw.portval)), ])













