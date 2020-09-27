
# Toronto housing prices

#load data

load("housing.rda")

#determine variable names

names(housing)

attach(housing)

#mean and SD of price

mean(price)

sd(price)

#plot price and sqft living

plot(price, sqft_living, cex.axis=0.8, cex=0.8)

#does price increase with the number of bedrooms?

tapply(price, bedrooms, mean)

#plot avg price and bedrooms

barplot(tapply(price, bedrooms, mean),ylim=c(0,1200000))

#see the outliers
table(bedrooms)

#subset bedrooms by removing units with more than 7 bedrooms or 
#less than 0 bedrooms.

housing$new.beds=bedrooms
housing$new.beds[bedrooms==0] <- NA
housing$new.beds[bedrooms>7]<- NA

table(housing$new.beds)

attach(housing)

barplot(tapply(price, new.beds, mean), ylim = c(0,1200000),
        main = "average price of bedrooms",
        xlab = "Number of bedrooms",
        ylab = "Average price")

install.packages("tableone")
library(tableone)

my.vars=c("price","bedrooms","bathrooms","sqft_living","sqft_lot",
          "sqft_above","sqft_basement","grade")

CreateTableOne(my.vars, data = housing)

#transform categorical variables

housing$f.cond= factor(condition)
housing$f.view= factor(view)
housing$f.water=factor(waterfront, labels = c("no waterfront","waterfront"))

names(housing)

#revised tableone with categorical variables
my.vars=c("price","bedrooms","bathrooms","sqft_living","sqft_lot",
          "sqft_above","sqft_basement","grade","f.view","f.cond","f.water")
CreateTableOne(my.vars,data = housing)

attach(housing)

#prices of homes with and without waterfront
tapply(price, f.water, mean)

#create a t-test

t.test(price~f.water, var.equal=T)

#regression models

mod.1=lm(price~f.water,data = housing)
summary(mod.1)


install.packages("stargazer")
library(stargazer)

#price dependent on bedrooms
mod.2= lm(price~bedrooms, data = housing)
stargazer(mod.2, type="text",no.space = T, digits=2)

#price dependent on bathrooms
mod.3=lm(price~bathrooms, data = housing)
stargazer(mod.3, type="text",no.space = T, digits = 2)

# price dependent on bedrooms and bathrooms

mod.4= lm(price~ bedrooms+bathrooms, data=housing)
stargazer(mod.4, type = "text", no.space = T, digits=2)

#price dependent on bedrooms, bathrooms, sqft_lot

mod.5= lm(price~ bedrooms+bathrooms+sqft_lot, data=housing)
stargazer(mod.5, type = "text", no.space = T, digits=2)

#price dependent on bedrooms, bathrooms, sqft_lot and sqft_living

mod.6= lm(price~ bedrooms+bathrooms+sqft_lot+sqft_living, data=housing)
stargazer(mod.6, type = "text", no.space = T, digits=2)

#price dependent on bathrooms and sqft_living

mod.7= lm(price~ bedrooms+sqft_living, data=housing)
stargazer(mod.7, type = "text", no.space = T, digits=2)

#year built- non-linear relationship

data=as.data.frame(aggregate(price, by=list(yr_built),FUN=mean))

plot(tapply(price,yr_built,mean))

plot(data,ylab="price in dollars")


#set decimal point display

options(scipen=9)

plot(data,pch=19, main="Price as a function of year of renovation",
     ylab="price in dollars",
     xlab="Year renovated")
abline(lm(price~yr_built),col="red") #regression line
