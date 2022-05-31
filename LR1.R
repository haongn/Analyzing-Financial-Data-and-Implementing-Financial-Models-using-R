# library(ISLR) 
data(Auto)
View(Auto)

# regeression
m1 = lm(mpg ~ horsepower, data = Auto)
print(summary(m1))

# prediction 
newdata = data.frame(horsepower = 90)
predict(m1, newdata)

newdata = data.frame(horsepower = c(85, 90, 93, 98))
predict(m1, newdata)

# plot regression line
plot(x = Auto$horsepower, 
     y = Auto$mpg, 
     xlab = "Horsepower", 
     ylab = "mpg")
abline(m1)

# multiple regression 
m2 = lm(mpg ~ horsepower + weight + year, data = Auto)
print(summary(m2))

m5 = lm(mpg ~ . - name, data = Auto)
print(summary(m5))







