# Fiji islands earthquake data analysis
## data consisting of 1000 earthquake documentations we are analyzing earthquake magnitude distribution, specific dangerous areas and more.

####loading required packages
library("ggplot2")
library("ggmap")
library("caret")
####loading life 
df = read.csv("c:\\dev\\data\\quakes.csv",TRUE)
#### showing data format and the few first rows 
head(df)
#### showing summary
summary(df)
#### showing magnitude distribution
boxplot(df$mag,col="red")

####create magnitude histogram to visualize frequency of different magnitude quakes. red line shows median. yellow line shows mean
hist(df$mag,col="blue")
abline(v=median(df$mag),lwd=4,col="red")
abline(v=mean(df$mag),lwd=4,col="yellow")

####scatter plot of quakes to visualize dangerous areas by latitude
with(df,plot(lat,mag))

####scatter plot of quakes to visualize dangerous areas by longtitude
with(df,plot(mag,long))

####scatter plot of both latitude and longtitude. magnitude is shown by color axis.
qplot(long,lat,data=df,col=mag)

map = get_map (location = c(lon=mean(df$long),lat=mean(df$lat)),zoom=5,maptype="satellite",scale=2)
####showing quake distibution on map
ggmap(map) + geom_point(data = df, aes(x = long, y = lat), size = 1,color="red") +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

####showing the most dangerous places on fiji
ggmap(map) + geom_point(data = df[df$mag>5.8,], aes(x = long, y = lat), size = 2,color="red") +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

####check for correlation between depth and magnitude
cordf = cor(df[,c("mag","stations")])
cordf[2,1]
#### we can see there is a high correlation between number of stations detecting the quake and its magnitude. thus, we can assume that this feature is redundant. moreover, high correlation also implies that one parameter can predict the other. This means that if a sesmologic noise is caught on many stations it could predict a quake of big magnitude. this can also be shown be a line graph
ggplot(data=df,aes(x=mag,y=stations,group=1))+geom_line(data=df,stat="identity")
