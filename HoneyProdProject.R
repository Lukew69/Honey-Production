library(tidyverse)
library(dplyr)
library(ggplot2)
library(mosaic)
library(lme4)
#heat maps
library(usmap)

#data was downloaded in 2023, the data set covers 1995-2021.
HoneyProd<- read.csv("https://raw.githubusercontent.com/lukew69/Honey-Production/master/US_honey_dataset_updated.csv")
View(HoneyProd)

HoneyProd[HoneyProd=="NewMexico"] <- "New Mexico"
HoneyProd[HoneyProd=="NewJersey"] <- "New Jersey"
HoneyProd[HoneyProd=="NewYork"] <- "New York"
HoneyProd[HoneyProd=="NorthCarolina"] <- "North Carolina"
HoneyProd[HoneyProd=="NorthDakota"] <- "North Dakota"
HoneyProd[HoneyProd=="SouthCarolina"] <- "South Carolina"
HoneyProd[HoneyProd=="SouthDakota"] <- "South Dakota"
HoneyProd[HoneyProd=="WestVirginia"] <- "West Virginia"


#taking the average of each state to make independence
StateProd = HoneyProd %>%
  group_by(state) %>%
    summarise(
      mean_prod=mean(production),
      mean_value=mean(value_of_production),
      mean_size=mean(colonies_number)
    )
View(StateProd)

#heat maps
plot_usmap(data=StateProd, values="mean_prod", color ="red") + 
  scale_fill_continuous(low="white", high="Dark Orange",name="Average Honey Produced 1995-2021 (pounds)", label = scales::comma)+ 
  theme(legend.position = "right")

plot_usmap(data=StateProd, values="mean_value", color ="red") + 
  scale_fill_continuous(low="white", high="dark green", name="Average Value 1995-2021", label = scales::comma)+ 
  theme(legend.position = "right")

plot_usmap(data=StateProd, values="mean_size", color ="red") + 
  scale_fill_continuous(low="white", high="red", name="Average Colony Size 1995-2021", label = scales::comma)+ 
  theme(legend.position = "right")


#plots for the entire US, the year is not a factor
#linear regression models
cor(StateProd$mean_size, StateProd$mean_prod)
USreg1=lm(StateProd$mean_prod~StateProd$mean_size)
plot(StateProd$mean_size, StateProd$mean_prod, xlab="Number of Producing Colonies", ylab="Honey Produced (pounds)")
abline(USreg1)
summary(USreg1)

cor(StateProd$mean_size, StateProd$mean_value)
USreg2=lm(StateProd$mean_value~StateProd$mean_size)
plot(StateProd$mean_size, StateProd$mean_value, xlab="Number of Producing Colonies", ylab="Value (dollars)")
abline(USreg2)
summary(USreg2)

cor(StateProd$mean_prod, StateProd$mean_value)
#taking the average of each year to make independence
YearProd = HoneyProd %>%
  group_by(year) %>%
    summarise(
      mean_prod=mean(production),
      mean_value=mean(value_of_production),
      mean_size=mean(colonies_number)
  )
View(YearProd)

#plots for each year, the state is not a factor
#linear regression models
cor(YearProd$year, YearProd$mean_size)
Yreg1=lm(YearProd$mean_size~YearProd$year)
plot(YearProd$year, YearProd$mean_size, xlab="Year", ylab="Number of Producing Colonies")
abline(Yreg1)
summary(Yreg1)

cor(YearProd$year, YearProd$mean_value)
Yreg2=lm(YearProd$mean_value~YearProd$year)
plot(YearProd$year, YearProd$mean_value, xlab="Year", ylab="Value (dollars)")
abline(Yreg2)
summary(Yreg2)

cor(YearProd$year, YearProd$mean_prod)
Yreg3=lm(YearProd$mean_prod~YearProd$year)
plot(YearProd$year, YearProd$mean_prod, xlab="Year", ylab="Honey Produced (pounds)")
abline(Yreg3)
summary(Yreg3)
