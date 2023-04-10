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

cor(HoneyProd$colonies_number, HoneyProd$production)#
cor(HoneyProd$colonies_number, HoneyProd$value_of_production)#

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