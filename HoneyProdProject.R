library(tidyverse)
library(dplyr)
library(ggplot2)
library(mosaic)

#data was downloaded in 2023, the dataset covers 1995-2021.
HoneyProd<- read.csv("https://raw.githubusercontent.com/lukew69/Honey-Production/master/US_honey_dataset_updated.csv")
View(HoneyProd)

#plots for the US
plot(HoneyProd$year, HoneyProd$colonies_number, xlab="Year", ylab="Amount of Producing Colonies")
plot(HoneyProd$year, HoneyProd$production, xlab="Year", ylab="Honey Produced (pounds)")
plot(HoneyProd$year, HoneyProd$value_of_production, xlab="Year", ylab="Value (dollars)")

plot(HoneyProd$colonies_number, HoneyProd$production, xlab="Amount of Producing Colonies", ylab="Honey Produced (pounds)")
plot(HoneyProd$colonies_number, HoneyProd$value_of_production, xlab="Amount of Producing Colonies", ylab="Value (dollars)")

plot(HoneyProd$production, HoneyProd$value_of_production, xlab="Honey Produced (pounds)", ylab="Value (dollars)")

#correlation for the linear models for the US
cor(HoneyProd$year, HoneyProd$colonies_number)
cor(HoneyProd$year, HoneyProd$production)
cor(HoneyProd$year, HoneyProd$value_of_production)

cor(HoneyProd$colonies_number, HoneyProd$production) #good
cor(HoneyProd$colonies_number, HoneyProd$value_of_production) #good

cor(HoneyProd$production, HoneyProd$value_of_production)

#linear regression models for the adequate plots
USreg1=lm(HoneyProd$colonies_number~HoneyProd$production )
plot(HoneyProd$colonies_number, HoneyProd$production, xlab="Amount of Producing Colonies", ylab="Honey Produced (pounds)")
abline(USreg1)
summary(USreg1)

USreg2=lm(HoneyProd$colonies_number~HoneyProd$value_of_production )
plot(HoneyProd$colonies_number, HoneyProd$value_of_production, xlab="Amount of Producing Colonies", ylab="Value (dollars)")
abline(USreg2)
summary(USreg2)

#apply a natural log transformation
#creating new columns in the data set for easier use later

HoneyProd <- HoneyProd %>%
  mutate(
    colonies_number.ln = log(colonies_number)
  )
HoneyProd <- HoneyProd %>%
  mutate(
    production.ln = log(production)
  )
HoneyProd <- HoneyProd %>%
  mutate(
    value_of_production.ln = log(value_of_production)
  )
View(HoneyProd)

#run them again with the transformation
cor(HoneyProd$colonies_number.ln, HoneyProd$production.ln)
USreg1=lm(HoneyProd$colonies_number.ln~HoneyProd$production.ln )
plot(HoneyProd$colonies_number.ln, HoneyProd$production.ln, xlab="Amount of Producing Colonies", ylab="Honey Produced (pounds)")
abline(USreg1)
summary(USreg1)

cor(HoneyProd$colonies_number.ln, HoneyProd$value_of_production.ln)
USreg2=lm(HoneyProd$colonies_number.ln~HoneyProd$value_of_production.ln )
plot(HoneyProd$colonies_number.ln, HoneyProd$value_of_production.ln, xlab="Amount of Producing Colonies", ylab="Value (dollars)")
abline(USreg2)
summary(USreg2)

##honey production for specific states
#This creates new data sets
HoneyProdAZ = filter(HoneyProd, state == "Arizona")
View(HoneyProdAZ) #because I live in Arizona

HoneyProdCA = filter(HoneyProd, state == "California")
View(HoneyProdCA) #because California is a big state

HoneyProdND = filter(HoneyProd, state == "NorthDakota")
View(HoneyProdND) #because North Dakota has the highest number of colonies

HoneyProdMD = filter(HoneyProd, state == "Maryland")
View(HoneyProdMD) #because Maryland has the fewest colonies

#plots for AZ. Colonies~Value is no good
cor(HoneyProdAZ$colonies_number.ln, HoneyProdAZ$production.ln)
AZreg1=lm(HoneyProdAZ$colonies_number.ln~HoneyProdAZ$production.ln )
plot(HoneyProdAZ$colonies_number.ln, HoneyProdAZ$production.ln, xlab="Amount of Producing Colonies", ylab="Honey Produced (pounds)")
abline(AZreg1)
summary(AZreg1)

#plots for CA. Colonies~Value is no good
cor(HoneyProdCA$colonies_number.ln, HoneyProdCA$production.ln)
CAreg1=lm(HoneyProdCA$colonies_number.ln~HoneyProdCA$production.ln )
plot(HoneyProdCA$colonies_number.ln, HoneyProdCA$production.ln, xlab="Amount of Producing Colonies", ylab="Honey Produced (pounds)")
abline(CAreg1)
summary(CAreg1)

#plots for ND. Colonies~Production is iffy
cor(HoneyProdND$colonies_number.ln, HoneyProdND$production.ln)
NDreg1=lm(HoneyProdND$colonies_number.ln~HoneyProdND$production.ln )
plot(HoneyProdND$colonies_number.ln, HoneyProdND$production.ln, xlab="Amount of Producing Colonies", ylab="Honey Produced (pounds)")
abline(NDreg1)
summary(NDreg1)

cor(HoneyProdND$colonies_number.ln, HoneyProdND$value_of_production.ln)
NDreg2=lm(HoneyProdND$colonies_number.ln~HoneyProdND$value_of_production.ln )
plot(HoneyProdND$colonies_number.ln, HoneyProdND$value_of_production.ln, xlab="Amount of Producing Colonies", ylab="Value (dollars)")
abline(NDreg2)
summary(NDreg2)

#plots for MD. Small amount of data points, run it anyways
cor(HoneyProdMD$colonies_number.ln, HoneyProdMD$production.ln)
MDreg1=lm(HoneyProdMD$colonies_number.ln~HoneyProdMD$production.ln )
plot(HoneyProdMD$colonies_number.ln, HoneyProdMD$production.ln, xlab="Amount of Producing Colonies", ylab="Honey Produced (pounds)")
abline(MDreg1)
summary(MDreg1)

cor(HoneyProdMD$colonies_number.ln, HoneyProdMD$value_of_production.ln)
MDreg2=lm(HoneyProdMD$colonies_number.ln~HoneyProdMD$value_of_production.ln )
plot(HoneyProdMD$colonies_number.ln, HoneyProdMD$value_of_production.ln, xlab="Amount of Producing Colonies", ylab="Value (dollars)")
abline(MDreg2)
summary(MDreg2)
