library(ggplot2)
library(plyr)
detach("package:plyr", unload=TRUE)
library(dplyr)
data(diamonds)

#Creating new volumnes varibale in diamonds
diamonds$volume = diamonds$x*diamonds$y*diamonds$z

ggplot(data= diamonds, aes(x= diamonds$price, y= x))+
  geom_point()+
  coord_cartesian(ylim= c(3,quantile(diamonds$x,0.95)))+
  geom_smooth()


#correlation x,y,z

cor.test(diamonds$price,diamonds$x)
cor.test(diamonds$price,diamonds$y)
cor.test(diamonds$price,diamonds$z)

#Diamond price vs depth
ggplot(data= diamonds, aes(x= diamonds$price, y= depth))+
  geom_point()+
  coord_cartesian(ylim= c(55,quantile(diamonds$depth,0.99)))+
  scale_x_continuous(breaks = seq(0,15000,3000), limits = c(0,15000))

#Diamond price vs depth at alpha = 1/100 or 0.01
ggplot(data= diamonds, aes(x= diamonds$price, y= depth))+
  geom_point(alpha = 0.01, color= 'blue')+
  coord_cartesian(ylim= c(55,quantile(diamonds$depth,0.99)))+
  scale_y_continuous(breaks = seq(57,67,2))+
  scale_x_continuous(breaks = seq(0,15000,1000), limits = c(0,15000))

#Correlation between depth and price
cor.test(diamonds$price,diamonds$depth)

#Diamond price vs carat
ggplot(data= diamonds, aes(x= diamonds$price, y= diamonds$carat))+
  geom_point()+
  scale_y_continuous(limits = c(0,quantile(diamonds$carat,0.99)))+
  scale_x_continuous(limits = c(0,quantile(diamonds$price,0.99)))

#Diamond price vs volume
ggplot(data= diamonds, aes(x= diamonds$price, y= diamonds$volume))+
  geom_point()

#Diamond price vs volume with limits
ggplot(data= diamonds, aes(x= diamonds$price, y= diamonds$volume))+
  geom_point()+
  scale_x_continuous(limits = c(0,quantile(diamonds$price,0.99)))+
  scale_y_continuous(limits = c(0,500))


#number of diamonds with 0 Volume
vol_0 <- subset(diamonds$volume,diamonds$volume == 0)
count(diamonds$volume == 0)

#correlation between volume(0,800) and price
vol_0_800<- subset(vol_0,vol_0 < 800)
cor.test(subset(diamonds$price,diamonds$volume>1&diamonds$volume<800),
         subset(diamonds$volume,diamonds$volume>1&diamonds$volume<800))

dia_0_800<-subset(diamonds,diamonds$volume>1&diamonds$volume<800)

#dataplot with the volume(0,800) and price
ggplot(data= dia_0_800,
       aes(x= dia_0_800$price, y= dia_0_800$volume))+
  geom_point()+
  geom_smooth(color='white')+
  coord_cartesian(ylim = c(0,400))

diamondsByClarity <- diamonds %>%
  group_by(clarity)%>%
  summarise(mean_price= mean(price), median_price = median(price),
            min_price = min(price),max_price = max(price), n = n())

head(diamondsByClarity,6)

#sample groups crated by quiz
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

head(diamonds_mp_by_clarity)
head(diamonds_mp_by_color)

p1= ggplot(data=diamonds_mp_by_clarity, aes(clarity,mean_price))+geom_bar(stat = 'identity')
p2= ggplot(data=diamonds_mp_by_color, aes(color,mean_price))+geom_bar(stat = 'identity')

grid.arrange(p1,p2,ncol=1)



