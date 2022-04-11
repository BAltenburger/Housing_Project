setwd("C:/Users/betsy/OneDrive/Documents/Data_Science/Housing")
### Packages ###
library(car)
library(forecast)
library(sandwich)
library(lmtest)
library(MASS)
### Data Cleaning ###
index <- read.csv("All-Transactions-City.csv") # Quarterly, from 1983 to 2021
income <- read.csv("Cville_Income.csv") # Yearly, from 1993 to 2019
#listing <- read.csv("Listing_Price.csv")
pop <- read.csv("Resident Population in Charlottesville.csv") # Yearly, from 2000 to 2020
gdp <- read.csv("Total Gross Domestic Product for Charlottesville.csv") # Yearly, from 1993 to 2020
unemployment <- read.csv("Unemployment Rate in Charlottesville.csv") #Monthly, from 1990 to 2021
wage <- read.csv("Weekly-Wage.csv") # Monthly, from 2007 to 2021

#### Should use data from Jan 2007- Dec 2019, should be monthly (so 156 rows)
# Subset yearly indexed, and interpolate using cubic spline
pop_new <- spline(pop[7:19,2], n = 156)$y
gdp_new <- spline(gdp[7:19,2], n = 156)$y

# Subset quarterly index and interpolate using cubic spline
index_new <- spline(index[93:145, 2], n = 156)$y

# Subset Monthly Data to be from Jan 2007 to Dec 2019
unemployment_new <- unemployment[204:359, 2]
wage_new <- wage[1:156, 2]


newdataset <- cbind(index_new, pop_new, gdp_new, unemployment_new, wage_new)

#newdataset <- merge(index, income, by = "DATE")
#newdataset <- merge(index, pop, by = "DATE")
#newdataset <- merge(newdataset, gdp, by = "DATE")
#newdataset <- merge(newdataset, unemployment, by = "DATE")
#newdataset <- merge(newdataset, wage, by = "DATE")

# data <- lm(newdataset$INDEX~newdataset$WAGE+newdataset$POPULATION+newdataset$UNEMPLOYMENT+newdataset$GDP)
model_raw <- lm((index_new) ~ (pop_new) + (gdp_new) + (unemployment_new) + (wage_new), na.action=na.exclude)
qqnorm(model_raw$residuals)
qqline(model_raw$residuals)
summary(model_raw)
par(mfrow=c(2,2))
plot(model_raw) # Non-Constant Errors
bc <- boxcox(model_raw) 
durbinWatsonTest(model_raw) # Serial Correlation
plot(model_raw$residuals)
lines(model_raw$residuals)
coeftest(model_raw, vcov = NeweyWest(model_raw, lag = 12))

### Boxcox Transformation Model
optimal_transform <- bc$x[which.max(bc$y)]
optimal_transform
model_transform <- lm((index_new)**optimal_transform ~ (pop_new) + (gdp_new) + (unemployment_new) + (wage_new), na.action=na.exclude)
summary(model_transform)
plot(model_transform)
durbinWatsonTest(model_transform) # Serial Correlation
coeftest(model_transform, vcov = NeweyWest(model_transform, lag = 12))
