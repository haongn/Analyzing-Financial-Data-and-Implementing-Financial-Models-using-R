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

# Step 1: Keep Only Variables We Need to Construct EW Portfolio
ewport = port[, c(1, 8:10)]
print(ewport[c(1:3, nrow(ewport)), ])

names(ewport) = paste(c("date", "AMZN", "MSFT", "IBM"))
rownames(ewport) = seq(1:nrow(ewport))
print(ewport[c(1:3, nrow(ewport)), ])

# Step 2: Converting Net Returns to Gross Returns
ewport$AMZN = 1 + ewport$AMZN
ewport$MSFT = 1 + ewport$MSFT
ewport$IBM = 1+ ewport$IBM
print(ewport[c(1:3, nrow(ewport)), ])

# Step 3: Calculate EW Portfolio Values for 1Q 2013
# trich xuat du lieu cua quy 1: tu thang 1 - het thang 3
ew.q1 = subset(ewport, 
               ewport$date >= as.Date("2012-12-31") & ewport$date <= as.Date("2013-03-31"))
print(ew.q1[c(1:3, nrow(ew.q1)), ])

# overwrite the gross return values for each security on December 31, 2013 to 1.00.
ew.q1[1, 2:4] = 1
# calculate cumulative gross returns for each security during the quarter 
ew.q1$AMZN = cumprod(ew.q1$AMZN)
ew.q1$MSFT = cumprod(ew.q1$MSFT)
ew.q1$IBM = cumprod(ew.q1$IBM)
print(ew.q1[c(1:3, nrow(ew.q1)), ])

# set up a variable denoting the number of securities in the portfolio 
num.sec = 3

# get the index value for each security during the quarter. 
ew.q1$AMZN.idx = (1 / num.sec) * ew.q1$AMZN
ew.q1$MSFT.idx = (1 / num.sec) * ew.q1$MSFT
ew.q1$IBM.idx = (1 / num.sec) * ew.q1$IBM
print(ew.q1[c(1:3, nrow(ew.q1)), ])

# calculate the value of the portfolio 
q1.val = data.frame(rowSums(ew.q1[, 5:7]))
print(q1.val[c(1:3, nrow(q1.val)), ])

names(q1.val) = paste("port.val")
q1.val$date = ew.q1$date
print(q1.val[c(1:3, nrow(q1.val)), ])

q2.inv = q1.val[nrow(q1.val), 1]
print(q2.inv)

# Step 4: Calculate EW Portfolio Values for 2Q 2013
ew.q2 = subset(ewport, 
               ewport$date >= as.Date("2013-04-01") & 
               ewport$date <= as.Date("2013-06-30"))
print(ew.q2[c(1:3, nrow(ew.q2)), ])

# calculate cumulative total returns 
ew.q2$AMZN = cumprod(ew.q2$AMZN)
ew.q2$MSFT = cumprod(ew.q2$MSFT)
ew.q2$IBM = cumprod(ew.q2$IBM)
print(ew.q2[c(1:3, nrow(ew.q2)),])

# calculate index value for each securities 
ew.q2$AMZN.idx = (q2.inv / num.sec) * ew.q2$AMZN
ew.q2$MSFT.idx = (q2.inv / num.sec) * ew.q2$MSFT
ew.q2$IBM.idx = (q2.inv / num.sec) * ew.q2$IBM
print(ew.q2[c(1:3, nrow(ew.q2)), ])

# calculate daily portfolio returns 
q2.val = data.frame(rowSums(ew.q2[, 5:7]))
print(q2.val[c(1:3, nrow(q2.val)), ])
names(q2.val) = paste("port.val")
q2.val$date = ew.q2$date
print(q2.val[c(1:3, nrow(q2.val)), ])

# calculate portfolio index value for the beginning of 3th quarter: 
q3.inv = q2.val[nrow(q2.val), 1]
print(q3.inv)

# => the portfolio has grown from $1.086834 at the end of the first quarter to $1.144049 at the end of the second quarter.
# We then use $1.144049 as the portfolio value that will be allocated evenly across the
# three securities at the start of the third quarter.

# Step 5: Calculate EW Portfolio Values for 3Q 2013
ew.q3 = subset(ewport, 
               ewport$date >= as.Date("2013-07-01") & 
               ewport$date <= as.Date("2013-09-30"))
print(ew.q3[c(1:3, nrow(ew.q3)), ])

# calculate cumulative total returns 
ew.q3$AMZN = cumprod(ew.q3$AMZN)
ew.q3$MSFT = cumprod(ew.q3$MSFT)
ew.q3$IBM = cumprod(ew.q3$IBM)
print(ew.q3[c(1:3, nrow(ew.q3)),])

# calculate index value for each securities 
ew.q3$AMZN.idx = (q3.inv / num.sec) * ew.q3$AMZN
ew.q3$MSFT.idx = (q3.inv / num.sec) * ew.q3$MSFT
ew.q3$IBM.idx = (q3.inv / num.sec) * ew.q3$IBM
print(ew.q3[c(1:3, nrow(ew.q3)), ])

# calculate daily portfolio returns 
q3.val = data.frame(rowSums(ew.q3[, 5:7]))
print(q3.val[c(1:3, nrow(q3.val)), ])
names(q3.val) = paste("port.val")
q3.val$date = ew.q3$date
print(q3.val[c(1:3, nrow(q3.val)), ])

# calculate portfolio index value for the beginning of 3th quarter: 
q4.inv = q3.val[nrow(q3.val), 1]
print(q4.inv)

# Step 6: Calculate EW Portfolio Values for 4Q 2013
ew.q4 = subset(ewport, 
               ewport$date >= as.Date("2013-10-01") & 
               ewport$date <= as.Date("2013-12-30"))
print(ew.q4[c(1:3, nrow(ew.q4)), ])

# calculate cumulative total returns 
ew.q4$AMZN = cumprod(ew.q4$AMZN)
ew.q4$MSFT = cumprod(ew.q4$MSFT)
ew.q4$IBM = cumprod(ew.q4$IBM)
print(ew.q4[c(1:3, nrow(ew.q4)),])

# calculate index value for each securities 
ew.q4$AMZN.idx = (q4.inv / num.sec) * ew.q4$AMZN
ew.q4$MSFT.idx = (q4.inv / num.sec) * ew.q4$MSFT
ew.q4$IBM.idx = (q4.inv / num.sec) * ew.q4$IBM
print(ew.q4[c(1:3, nrow(ew.q4)), ])

# calculate daily portfolio returns 
q4.val = data.frame(rowSums(ew.q4[, 5:7]))
print(q4.val[c(1:3, nrow(q4.val)), ])
names(q4.val) = paste("port.val")
q4.val$date = ew.q4$date
print(q4.val[c(1:3, nrow(q4.val)), ])

# Step 7: Combine Quarterly EW Portfolio Values into One Data Object
ew.portval= rbind(q1.val, q2.val, q3.val, q4.val)  # ket hop 4 cot lai, chong len nhau 
print(ew.portval[c(1:3, nrow(ew.portval)), ])




















































