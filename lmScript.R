#Libraries
library(ggplot2)
library(dplyr)


#Read data
bike <- read.csv('bikeshare.csv')
print(head(bike))


#Format dates & feature engineering
bike$datetime <- as.POSIXct(bike$datetime)

bike$hour <- sapply(bike$datetime, function(x){
  format(x,'%H')
}
) 
bike$hour <- sapply(bike$hour, as.numeric)

#Exploration - Temp, Month, Season
ggplot(bike, aes(temp, count)) + geom_point(alpha = 0.3, aes(color=temp)) + 
  theme_bw() +
  labs(x = 'Temperature (in Cesius)', y = 'Number of Bikes Rented')

pl <- ggplot(bike, aes(datetime, count)) + geom_point(aes(color = temp), alpha = 0.5) + 
  scale_color_continuous(low = '#55D8CE', high = '#FF6E2E') + 
  theme_bw() +
  labs(x = 'Date', y = 'Number of Bikes Rented')

cor(bike[,c('temp','count')])

pl2 <- ggplot(bike, aes(factor(season), count)) + 
  geom_boxplot(aes(color = factor(season))) + 
  theme_bw() +
  labs(x = 'Season', y = 'Number of Bikes Rented')


pl3 <- bike %>%
  filter(workingday == 1) %>%
  ggplot(.,aes(hour, count)) +
  geom_point(position = position_jitter(w=1, h=0),aes(color = temp), alpha = 0.5) 

pl3 + 
  scale_color_gradientn(colors = c('dark blue', 'blue','light blue','light green','yellow','orange','red')) +
  theme_bw()

#Model
temp_model <- lm(count ~ temp, bike)
print(summary(temp_model))

multi_model <- lm(count ~ . -casual - registered - datetime - atemp, bike)
print(summary(multi_model))

#25 degrees celsius bike rental estimate based on temp_model = mx + b = 9.1705*25 + 6.0462
9.1705*25 + 6.0462
