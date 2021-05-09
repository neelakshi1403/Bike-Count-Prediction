library(ggplot2)
library(ggthemes)
library(dplyr )
library(caTools)

# Load the dataset
df<-read.csv('bikeshare.csv',sep=',')
#Set the data as data frame
bike<-as.data.frame(df)
print(head(bike))
str(bike)

# Check for null values
is.na(bike)

#Create a scatter plot of count vs temp
ggplot(bike, aes(x=temp, y=count)) + geom_point(alpha=0.2, aes(color=temp)) +  geom_smooth(method=lm)
       
#Plot count versus datetime as a scatterplot with a color gradient based on temperature. You'll need to convert the datetime column into POSIXct before plotting.

bike$datetime<-as.POSIXct(bike$datetime)
bike$datetime
ggplot(bike,aes(datetime,count)) + geom_point(aes(color=temp),alpha=0.5)  + scale_color_continuous(low='#55D8CE',high='#FF6E2E') +theme_bw()

#correlation between temp and count

cor_data<- cor(bike[,c('temp','count')])
print(cor_data)

ggplot(bike,aes(factor(season),count)) + geom_boxplot(aes(color=factor(season))) +theme_bw()

#Feature Engineering


bike$hour1 <- sapply(bike$datetime,function(x){format(x,"%H")})
bike

#plot a scatterplot betwwn count and hour

pl <- ggplot(filter(bike,workingday==1),aes(hour1,count)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()
print(pl)

dl <- ggplot(filter(bike,workingday==0),aes(hour1,count)) 
dl <- dl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)
dl <- dl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
dl + theme_bw()
print(pl)


#Building the model based on temp feature
temp.model<-lm(count~temp,bike)
summary(temp.model)
#Building the model based on all feature 
# how many bikes rental count for temp=25
#6.0462 +9.1705     *25 = 235.3087
temp.test <- data.frame(temp=c(25))
predict(temp.model,temp.test)
bike$hour1 <- sapply(bike$hour1,as.numeric)
model <- lm(count ~ . -casual - registered -datetime -atemp,bike )
summary(model)




