# calculate the variance and standard deviation of AMZN's returns. 
# Note: the variance and standard deviation calculations change depending on the time period we choose. 
library(xts)
library(quantmod)

# Step 1: Import AMZN Data from Yahoo Finance
data.AMZN = read.csv('AMZN Yahoo.csv', header = TRUE)
date = as.Date(data.AMZN$Date, format = '%Y-%m-%d')
data.AMZN = cbind(date, data.AMZN[, -1])
data.AMZN = data.AMZN[order(data.AMZN$date), ]

data.AMZN = xts(data.AMZN[, 2:7], order.by = data.AMZN[, 1])
names(data.AMZN) = paste(c('AMZN.Open', 'AMZN.High', 'AMZN.Low', 'AMZN.Close', 'AMZN.Adjusted', 'AMZN.Volume'))
print(data.AMZN[c(1:3, nrow(data.AMZN)), ])

# Step 2: Calculate Returns (using 'Delt' command)
AMZN.ret = data.AMZN[, 5]     # extract' AMZN.Adjusted' column
AMZN.ret$Return = Delt(AMZN.ret$AMZN.Adjusted)    # tru lien tiep hang sau cho hang truoc 
print(AMZN.ret[c(1:3, nrow(AMZN.ret)), ])         
AMZN.ret = AMZN.ret[-1, 2]                        # lay moi cot return, xoa hang dau tien di. 
print(AMZN.ret[c(1:3, nrow(AMZN.ret)), ])

# Step 3: Calculate Full Period (2011-2013) Variance and Standard Deviation
# using 'var' command and 'sd' command, respectively. 
AMZN.var.full = var(AMZN.ret$Return)
print(AMZN.var.full)

AMZN.sd.full = sd(AMZN.ret$Return)
print(AMZN.sd.full)

# Step 4: Calculate Variance and Standard Deviation for 2011
AMZN.2011 = subset(AMZN.ret, 
                   index(AMZN.ret) >= '2011-01-01' & 
                   index(AMZN.ret) <= '2011-12-31')
AMZN.2011[c(1:3, nrow(AMZN.2011)), ]

AMZN.var.2011 = var(AMZN.2011)
AMZN.var.2011

AMZN.sd.2011 = sd(AMZN.2011)
AMZN.sd.2011

# Step 5: Calculate Variance and Standard Deviation for 2012 and 2013
AMZN.2012 = subset(AMZN.ret, 
                   index(AMZN.ret) >= '2012-01-01' & 
                   index(AMZN.ret) <= '2012-12-31')
AMZN.2012[c(1:3, nrow(AMZN.2012)), ]

AMZN.var.2012 = var(AMZN.2012)
AMZN.var.2012

AMZN.sd.2012 = sd(AMZN.2012)
AMZN.sd.2012


AMZN.2013 = subset(AMZN.ret, 
                   index(AMZN.ret) >= '2013-01-01' & 
                   index(AMZN.ret) <= '2013-12-31')
AMZN.2013[c(1:3, nrow(AMZN.2013)), ]

AMZN.var.2013 = var(AMZN.2013)
AMZN.var.2013

AMZN.sd.2013 = sd(AMZN.2013)
AMZN.sd.2013

# Step 6: Calculate Average Return for the Full Period and Each of the Subperiods
# calculate the mean for each of the four time periods above. 
mean.ret.full = mean(AMZN.ret)
mean.ret.full

mean.ret.2011 = mean(AMZN.2011)
mean.ret.2011

mean.ret.2012 = mean(AMZN.2012)
mean.ret.2012

mean.ret.2013 = mean(AMZN.2013)
mean.ret.2013

# Step 7: Combine All Data
AMZN.risk = rbind()





































































