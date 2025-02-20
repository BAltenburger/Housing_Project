index <- read.csv("All-Transactions-City.csv")
income <- read.csv("Cville_Income.csv")
#listing <- read.csv("Listing_Price.csv")
pop <- read.csv("Resident Population in Charlottesville.csv")
gdp <- read.csv("Total Gross Domestic Product for Charlottesville.csv")
unemployment <- read.csv("Unemployment Rate in Charlottesville.csv")
wage <- read.csv("Weekly-Wage.csv")

#newdataset <- merge(index, income, by = "DATE")
newdataset <- merge(index, pop, by = "DATE")
newdataset <- merge(newdataset, gdp, by = "DATE")
newdataset <- merge(newdataset, unemployment, by = "DATE")
newdataset <- merge(newdataset, wage, by = "DATE")

data <- lm(newdataset$INDEX~newdataset$WAGE+newdataset$POPULATION+newdataset$UNEMPLOYMENT+newdataset$GDP)
summary(data)