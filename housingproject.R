
# Toronto housing prices

#load data

load("housing.rda")

#determine variable names

names(housing)

# [1] "id"            "date"          "price"         "bedrooms"      "bathrooms"    
# [6] "sqft_living"   "sqft_lot"      "floors"        "waterfront"    "view"         
# [11] "condition"     "grade"         "sqft_above"    "sqft_basement" "yr_built"     
# [16] "yr_renovated"  "zipcode"       "lat"           "long"          "sqft_living15"
# [21] "sqft_lot15"

attach(housing)

#mean and SD of price

mean(price)
# 
# 540088.1

sd(price)

# 367127.2

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
        main = "average price of houses",
        xlab = "Number of bedrooms",
        ylab = "Average price")

install.packages("tableone")
library(tableone)

my.vars=c("price","bedrooms","bathrooms","sqft_living","sqft_lot",
          "sqft_above","sqft_basement","grade")

CreateTableOne(my.vars, data = housing)


                        # Overall              
# n                             21613            
# price (mean (sd))         540088.14 (367127.20)
# bedrooms (mean (sd))           3.37 (0.93)     
# bathrooms (mean (sd))          2.11 (0.77)     
# sqft_living (mean (sd))     2079.90 (918.44)   
# sqft_lot (mean (sd))       15106.97 (41420.51) 
# sqft_above (mean (sd))      1788.39 (828.09)   
# sqft_basement (mean (sd))    291.51 (442.58)   
# grade (mean (sd))              7.66 (1.18)  
#transform categorical variables

attach(housing)
housing$f.cond= factor(condition)
housing$f.view= factor(view)
housing$f.water=factor(waterfront, labels = c("no waterfront","waterfront"))

names(housing)

#revised tableone with categorical variables
my.vars=c("price","bedrooms","bathrooms","sqft_living","sqft_lot",
          "sqft_above","sqft_basement","grade","f.view","f.cond","f.water")
CreateTableOne(my.vars,data = housing)


#                               Overall              
# n                             21613            
# price (mean (sd))         540088.14 (367127.20)
# bedrooms (mean (sd))           3.37 (0.93)     
# bathrooms (mean (sd))          2.11 (0.77)     
# sqft_living (mean (sd))     2079.90 (918.44)   
# sqft_lot (mean (sd))       15106.97 (41420.51) 
# sqft_above (mean (sd))      1788.39 (828.09)   
# sqft_basement (mean (sd))    291.51 (442.58)   
# grade (mean (sd))              7.66 (1.18)     
# f.view (%)                                     
# 0                          19489 (90.2)     
# 1                            332 ( 1.5)     
# 2                            963 ( 4.5)     
# 3                            510 ( 2.4)     
# 4                            319 ( 1.5)     
# f.cond (%)                                     
# 1                             30 ( 0.1)     
# 2                            172 ( 0.8)     
# 3                          14031 (64.9)     
# 4                           5679 (26.3)     
# 5                           1701 ( 7.9)     
# f.water = waterfront (%)        163 ( 0.8)     

attach(housing)

#prices of homes with and without waterfront
tapply(price, f.water, mean)

# no waterfront    waterfront 
# 531563.6     1661876.0 

#create a t-test

t.test(price~f.water, var.equal=T)


# Two Sample t-test
# 
# data:  price by f.water
# t = -40.626, df = 21611, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#         -1184847 -1075778
# sample estimates:
#         mean in group no waterfront    mean in group waterfront 
#                    531563.6                   1661876.0 


#regression models

mod.1=lm(price~f.water,data = housing)
summary(mod.1)


Call:
        lm(formula = price ~ f.water, data = housing)

# Residuals:
#         Min       1Q   Median       3Q      Max 
#       -1376876  -211564   -81564   108436  7168436 
# 
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        531564       2416  220.00   <2e-16 ***
# f.waterwaterfront  1130312      27823   40.63   <2e-16 ***
#         ---
#         Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 353900 on 21611 degrees of freedom
# Multiple R-squared:  0.07095,	Adjusted R-squared:  0.07091 
# F-statistic:  1650 on 1 and 21611 DF,  p-value: < 2.2e-16

install.packages("stargazer")
library(stargazer)

#price dependent on bedrooms
mod.2= lm(price~bedrooms, data = housing)
stargazer(mod.2, type="text",no.space = T, digits=2)

#                  Dependent variable:    
#         ---------------------------
#                     price           
# -----------------------------------------------
#bedrooms                   121,716.10***       
#                            (2,554.30)         
# Constant                   129,802.40***       
#                             (8,931.87)         
# -----------------------------------------------
# Observations                  21,613           
# R2                             0.10            
# Adjusted R2                    0.10            
# Residual Std. Error   349,246.30 (df = 21611)  
# F Statistic         2,270.66*** (df = 1; 21611)
# ===============================================
#         Note:               *p<0.1; **p<0.05; ***p<0.01


#price dependent on bathrooms
mod.3=lm(price~bathrooms, data = housing)
stargazer(mod.3, type="text",no.space = T, digits = 2)

#                          Dependent variable:    
#         ---------------------------
#                                price           
# -----------------------------------------------
#bathrooms                  250,326.50***       
#                            (2,759.53)         
# Constant                    10,708.31*         
#                             (6,210.67)         
# ---------------------------------------------
#Observations                  21,613           
# R2                             0.28            
# Adjusted R2                    0.28            
# Residual Std. Error   312,438.90 (df = 21611)  
# F Statistic         8,228.94*** (df = 1; 21611)
# ===============================================
#         Note:               *p<0.1; **p<0.05; ***p<0.01


# price dependent on bedrooms and bathrooms

mod.4= lm(price~ bedrooms+bathrooms, data=housing)
stargazer(mod.4, type = "text", no.space = T, digits=2)

#                         Dependent variable:    
#         ---------------------------
#                               price           
# -----------------------------------------------
#bedrooms                   20,138.27***        
#                           (2,664.00)         
# bathrooms                  237,780.60***       
#                            (3,217.09)         
# Constant                   -30,643.00***       
#                             (8,270.15)         
# -----------------------------------------------
# Observations                  21,613           
# R2                             0.28            
# Adjusted R2                    0.28            
# Residual Std. Error   312,033.80 (df = 21610)  
# F Statistic         4,153.73*** (df = 2; 21610)
# ===============================================
#         Note:               *p<0.1; **p<0.05; ***p<0.01

#price dependent on bedrooms, bathrooms, sqft_lot

mod.5= lm(price~ bedrooms+bathrooms+sqft_lot, data=housing)
stargazer(mod.5, type = "text", no.space = T, digits=2)

#                          Dependent variable:    
#         ---------------------------
#                              price           
# -----------------------------------------------
#bedrooms                   20,463.80***        
#                           (2,660.75)         
# bathrooms                  235,711.10***       
#                            (3,223.99)         
# sqft_lot                      0.40***          
#                              (0.05)           
# Constant                   -33,340.01***       
#                            (8,266.44)         
# -----------------------------------------------
#         Observations                  21,613           
# R2                             0.28            
# Adjusted R2                    0.28            
# Residual Std. Error   311,613.90 (df = 21609)  
# F Statistic         2,796.38*** (df = 3; 21609)
# ===============================================
#         Note:               *p<0.1; **p<0.05; ***p<0.01


#price dependent on bedrooms, bathrooms, sqft_lot and sqft_living

mod.6= lm(price~ bedrooms+bathrooms+sqft_lot+sqft_living, data=housing)
stargazer(mod.6, type = "text", no.space = T, digits=2)

#                               Dependent variable:    
#         ---------------------------
#                                      price           
# -----------------------------------------------
#bedrooms                   -59,406.81***       
#                             (2,337.21)         
# bathrooms                    6,268.66*         
#                             (3,509.57)         
# sqft_lot                     -0.38***          
#                               (0.04)           
# sqft_living                  314.29***         
#                                (3.13)           
# Constant                   79,092.32***        
#                            (6,918.60)         
# -----------------------------------------------
# Observations                  21,613           
# R2                             0.51            
# Adjusted R2                    0.51            
# Residual Std. Error   257,363.10 (df = 21608)  
# F Statistic         5,592.50*** (df = 4; 21608)
# ===============================================
#         Note:               *p<0.1; **p<0.05; ***p<0.01


#price dependent on bathrooms and sqft_living

mod.7= lm(price~ bedrooms+sqft_living, data=housing)
stargazer(mod.7, type = "text", no.space = T, digits=2)

#year built- non-linear relationship

data=as.data.frame(aggregate(price, by=list(yr_built),FUN=mean))

plot(tapply(price,yr_built,mean)

plot(data,ylab="price in dollars",
     main="Mean price of homes as per the year built" )


#set decimal point display

options(scipen=9)

plot(data,pch=19, main="Price as a function of year of renovation",
     ylab="price in dollars",
     xlab="Year renovated")
abline(lm(price~yr_built),col="red") #regression line
