# muc dich la so sanh performance cua EW va VW 
# file nay can chay sau khi da chay hai file EW Portfo va VW portfo
# Step 1: Combine the Data
port.val = merge(vw.portval, ew.portval, by = "date")
print(port.val[c(1:3, nrow(port.val)), ])

# Step 2: Rename the Variables
names(port.val) = paste(c("date", "VW.cum", "EW.cum"))
print(port.val[c(1:3, nrow(port.val)), ])

# Step 3: Plot the Data
par(mfrow = c(1, 1))
y.range = range(port.val[, 2:3])
print(y.range)

plot(port.val$EW.cum, 
     type = "l", 
     xlab = 'date', 
     ylab = 'Value of Investment', 
     ylim = y.range, 
     lty = 1, 
     main = "Value of $1 Investment in Equal-Weighted and 
             Value-Weighted Portfolios of AMZN, MSFT and IBM 
             December 31, 2012 - December 31, 2013")
lines(port.val$VW.cum, 
      lty = 2)
abline(h= 1, lty = 1)
legend('topleft', 
       c('Equal-Weighted Portfolio', 'Value-Weighted Portfolio'), 
       lty = c(1, 2))