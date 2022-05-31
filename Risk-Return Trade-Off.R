# first take a look at how stocks and bonds have performed over the last 50 years.
library(xts)
library(quantmod)

# Step 1: Import Fama-French Data from Professor Kenneth French's Websites
FF.raw = read.fwf(file = 'F-F_Research_Data_Factors.txt', widths = c(6, 8, 8, 8, 8), skip = 4)
print(head(FF.raw))
print(tail(FF.raw))

# Step 2: Clean up Data
FF.raw = FF.raw[-1051:-1244, ]
names(FF.raw) = paste(c('text.date', 'RmxRf', 'SMB', 'HML', 'Rf'))
print(head(FF.raw))

print(str(FF.raw))
FF.raw = FF.raw[, c(-1, -3, -4)]
print(FF.raw[c(1:3, nrow(FF.raw)), ])
print(str(FF.raw))
FF.raw$RmxRf = as.numeric(as.character(FF.raw$RmxRf)) / 100   ## co ve khong can as.character
print(FF.raw[c(1:3, nrow(FF.raw)), ])
FF.raw$Rf = as.numeric(as.character(FF.raw$Rf)) / 100
print(FF.raw[c(1:3, nrow(FF.raw)), ])
FF.raw$date = seq(as.Date('1926-07-01'), as.Date('2013-12-31'), by = 'months')
print(FF.raw[c(1:3, nrow(FF.raw)), ])
FF.raw$date = as.yearmon(FF.raw$date, '%Y-%m-%d')
print(FF.raw[c(1:3, nrow(FF.raw)), ])

# Step 3: Calculate Raw Market Return Variable
FF.raw$Rm = FF.raw$RmxRf + FF.raw$Rf
print(FF.raw[c(1:3, nrow(FF.raw)), ])

# Step 4: Subset Data from December 1963 to December 2013
# to get 50 years of returns we would assume an investment made at the end of December 1963
FF = subset(FF.raw, 
            FF.raw$date >= '1963-12-01' & 
            FF.raw$date <= '2013-12-31')
print(FF[c(1:3, nrow(FF)), ])

# Step 5: Calculate Gross Returns for the Market and Risk-free Rate
FF$Gross.Rm = 1 + FF$Rm
FF$Gross.Rm[1] = 1
FF$Gross.Rf = 1 + FF$Rf 
FF$Gross.Rf[1] = 1 
print(FF[c(1:3, nrow(FF)), ])

# Step 6: Calculate Cumulative Returns for the Market and Risk-free Rate
FF$cum.Rm = cumprod(FF$Gross.Rm)
FF$cum.Rf = cumprod(FF$Gross.Rf)
print(FF[c(1:3, nrow(FF)), ])

# Step 7: Plot the Data
y.range = range(FF$cum.Rf, FF$cum.Rm)
print(y.range)

title1 = 'Stock vs. Bond Returns'
title2 = '1964 to 2013'
plot(x = FF$date, 
     y = FF$cum.Rm, 
     type = 'l', 
     xlab = 'Date', 
     ylab = 'Value of $1 Invesment ($)', 
     ylim = y.range,
     main = paste(title1,"\n", title2))
lines(x = FF$date, y = FF$cum.Rf, lty = 2)
legend('topleft', 
       c('Stock (2013 Ending Value: $124.89)', 
         'Bond (2013 Ending Value: $12.10)'), 
       lty = c(1, 2))

# Step 8: Plot Stock and Bond Returns
y.range = range(FF$Rm, FF$Rf)
print(y.range)

title1 = 'Volatility of Stock vs. Bond Returns'
title2 = '1964 to 2013'
plot(x = FF$date, 
     y = FF$Rm, 
     type = 'l', 
     xlab = 'Date', 
     ylab = 'Returns (%)', 
     ylim = y.range, 
     col = 'gray50', 
     main = paste(title1,'\n', title2))
lines(x = FF$date, y = FF$Rf)
abline(h = 0)
legend('topleft', 
       c('Stocks', 'Bonds'), 
       lty = c(1, 2))


















