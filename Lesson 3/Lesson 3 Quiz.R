library(ggplot2)
data(diamonds)
str(diamonds)
dim(diamonds)

qplot(diamonds$price)

by(diamonds$price,diamonds$cut, summary)

qplot(diamonds$carat, binwidth= 0.01)+
  scale_x_continuous(breaks = seq(0,2,0.05), limits = c(0,2))+
  coord_cartesian(ylim = c(2000,3000))
  

summary(diamonds$price)

dim(subset(diamonds, diamonds$price<500))
dim(subset(diamonds, diamonds$price<250))
dim(subset(diamonds, diamonds$price>=15000))

qplot(x = price, data = diamonds) + facet_wrap(~cut, scales="free_y")

qplot(x = log10(price/carat), data = diamonds) + facet_wrap(~cut, scales="free_y")

qplot(x = cut, y= price, data= diamonds, geom = 'boxplot')+
  coord_cartesian(ylim = c(0,7000))

summary(subset(diamonds,diamonds$color == 'D')$price)
summary(subset(diamonds,diamonds$color == 'J')$price)

IQR(subset(diamonds,diamonds$color == 'D')$price)
IQR(subset(diamonds,diamonds$color == 'J')$price)

qplot(x = color , y= (price/carat), data= diamonds, geom = 'boxplot')+
  coord_cartesian(ylim = c(0,6000))

install.packages("tidyr")
devtools::install_github("rstudio/EDAWR")

library(EDAWR)
emp<-read.csv(file = "gapminder_above15_employ.csv")

str(emp)
library(tidyr)

emp_tb <- gather(emp, "Year", "n", 2:18,na.rm = TRUE)


emp_tb$Year <- factor(emp_tb$Year)
str(emp_tb_f)
dim(emp_tb_f)

emp_tb_f <- subset(emp_tb, select = c(Year,n,ï..Total.above.15.employment.to.population....))
colnames(emp_tb_f)[3] <- "country"

emp_tb_f$Year <- factor(emp_tb_f$Year)
emp_tb_f$country <- factor(emp_tb_f$country)

by(emp_sub_ausa$n, emp_sub_ausa$country, summary)

table(emp_sub_ausa)

emp_sub_ausa <- subset(emp_tb_f, ((emp_tb_f$country == "Afghanistan" | 
                                    emp_tb_f$country == "United States"|emp_tb_f$country == "India")&
                       !is.na(emp_tb_f)))

emp_sub_ausa <- subset(emp_sub_ausa, emp_sub_ausa$n > 1)




write.csv(emp_sub_ausa,"t.csv")

library(ggplot2)

qplot(x= Year, y= n, colour = emp_sub_ausa$country, data = emp_sub_ausa)+
  geom_smooth()

ggplot(aes(x= emp_sub_ausa$Year, y= emp_sub_ausa$n), data = emp_sub_ausa)+
  geom_freqploy(aes(color = emp_sub_ausa$country))



db <- read.csv("Birthdays_hotkeats@gmail.com.csv")

head(db)







