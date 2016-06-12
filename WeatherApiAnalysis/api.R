# loading libraries 
require(jsonlite)
library(plyr)
library("ggplot2")

#setting api key
key<-"23f5472576a0a566737e2c28f9118724"

# for any number smaller than 10 , returns the number padded with zero from the left
twoDigit <- function(arg){
  if (arg<10){
    arg<-paste0("0",arg)
  }
  return(arg)
}

# initilizing empty list
dataList<-c()
# querying the api for 3 days a month for any month between years 1999-2016
# this should return a the weather in tel aviv for every 10 days in 1999-2016
for(j in 1999:2016){
year<-twoDigit(j)
  for (k in 1:12) {
   month = twoDigit(k); 
   # querying 3 days a month: 1,10,20
    for (i in c(1,10,20)) {
      day<-twoDigit(i);  
      # link contains the forecast api url with parameters 
      link<-paste0("https://api.forecast.io/forecast/",key,"/32.083328,34.799999,",year,"-",month,"-",day,"T15:00:00?units=si")
      # sending http request and converting response data from json format to data frame
      jsonData <- fromJSON(link)
      # adding date column 
      jsonData$daily$data$date<-paste0(year,"-",month,"-",day)
      dataList<-c(dataList,jsonData$daily)
    }
  }
}

# create dataframe
df <- rbind.fill(dataList)

# choosing the interresting columns
df2 = df[,c("humidity","windSpeed","temperatureMin","temperatureMax","cloudCover","date")]
# showing data format and the few first rows 
head(df2)

# showing summary
summary(df2)

# showing min tempreature of the day histogram.
hist(df2$temperatureMin,col="blue")
abline(v=median(df2$temperatureMin),lwd=4,col="red")
abline(v=mean(df2$temperatureMin),lwd=4,col="yellow")

# showing max tempreature of the day histogram.
hist(df2$temperatureMax,col="blue")
abline(v=median(df2$temperatureMax),lwd=4,col="red")
abline(v=mean(df2$temperatureMax),lwd=4,col="yellow")

#showing average temperature histogram
hist((df2$temperatureMax+df2$temperatureMin)/2,col="blue")
abline(v=median((df2$temperatureMax+df2$temperatureMin)/2),lwd=4,col="red")
abline(v=mean((df2$temperatureMax+df2$temperatureMin)/2),lwd=4,col="yellow")

# filtering summer months
df3 = subset(df2, substr(date,6,7) %in% c("07","08","09"))

# comparing summer months to check if global warming is detected in tel aviv
ggplot(data=df3,aes(x=date,y=temperatureMax,group=1))+geom_line(data=df3,stat="identity")

#comparing winter months for the same goal
df4 = subset(df2, substr(date,6,7) %in% c("12","01","02"))
ggplot(data=df4,aes(x=date,y=temperatureMax,group=1))+geom_line(data=df4,stat="identity")

# checking for connection between cloud cover and humidity
p = ggplot(df2, aes(humidity,cloudCover))
p+geom_point()

#checking for more correlations between parameters
cordf = cor(df2[,c("humidity" ,"windSpeed" ,"temperatureMin", "temperatureMax", "cloudCover")])
cordf

