library(AER)
data("Journals")

str(Journals)
dim(Journals)
names(Journals)      # gia tri cot index 

# we are interested in the relation between the demand for economics journals 
# and their price 
# a suitable measure of the price for scientific journals is the 
# price per citation. 
attach(Journals)
plot(log(subs) ~ log(price / citations), data = Journals)
plot(subs ~ price / citations, data = Journals)

# linear regression model by OLS 
j_lm = lm(log(subs) ~ log(price/citations), data= Journals)
abline(j_lm)

# detailed summary of the fitted model 
summary(j_lm)

# chap3: 

journals = Journals[, c('subs', 'price')]
journals$citeprice = Journals$price/Journals$citations
head(journals)
summary(journals)

# because the variable is highly skewed, it is useful to take logarithm
# the goal is to estimate the effect of the price per citation on the number of 
# library subscriptions. 
attach(journals)
plot(log(subs) ~ log(citeprice), data = journals)
jour_lm = lm(log(subs) ~ log(citeprice), data = journals)  # return fitted-model object
abline(jour_lm)  # extracts the coefficients of the fitted model and adds the corresponding regression line to the plot 

class(jour_lm)  # check the datatype of jour_lm 
names(jour_lm)  # 12 components
coef(jour_lm)
print(jour_lm)  # simple printed display 
summary(jour_lm)  # standard regression output 
str(jour_lm)      # more detailed view 

# all components are directly accessible using $
jour_lm$rank    # the rank of the model matrix 
jour_lm$residuals

# generic functions
coef(jour_lm)
jour_lm$coefficients    # so sanh: giong het nhau 
confint(jour_lm)

# look what summary() method returns 
jour_slm = summary(jour_lm)
class(jour_slm)
names(jour_slm)
jour_slm$coefficients
str(jour_slm)   # complete overview 











