#required packages

require(devtools)
install_github("ramnathv/rCharts")

Sys.setenv(LANG = "en")
install.packages(c("twitterR","ROAuth","RCurl","tm","wordcloud","SnowballC"))
install.packages("SnowballC")
library(SnowballC)
library(twitteR)
library(ROAuth)
library(RCurl)
library(tm)
library(wordcloud)

#twitter authentication
consumerKey <- "tT7e2vSIAv8mzOFjKnedhau0D"
consumerSecret <- "PWgfDoG24U7VUiFCHnLarNz0RmQbjPEofywxxOHcp1eOI6dotI"
accessToken <- "2608779027-8NQT6mu3Kf8hwKZTAuPr9ua3wpS43cs02UqeMWZ"
accessTokenSecret <- "V3jfYIVnbztp9OlbLlDUOTz2bRNJrOMBLe3D3Old6JVpb"

twitteR::setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)

#retrive tweets from twitterhttp://thinktostart.files.wordpress.com/2014/05/bildschirmfoto-2014-05-13-um-11-05-06.png?w=388

#get the twitter data

user <- getUser("loraliin") #Set the username
View(user)

userFriends <- user$getFriends()
userFollowers <- user$getFollowers()
View(userFollowers)
userNeighbors <- union(userFollowers, userFriends) #merge followers and friends
userNeighbors.df = twListToDF(userNeighbors) #create the dataframe
plot(userNeighbors.df$followersCount,userNeighbors.df$friendsCount,xlab="followers",ylab="friends")


#log transformation
userNeighbors.df[userNeighbors.df==0]=1
userNeighbors.df$logFoloowersCount=log(userNeighbors.df$followersCount)
userNeighbors.df$logFriendsCount=log(userNeighbors.df$friendsCount)

#we extract the relevant columns out of our dataframe and create a new one for our algorithm.
kObject.log=data.frame(userNeighbors.df$logFoloowersCount,userNeighbors.df$logFriendsCount)
a=apply(kObject.log,2,var)
View(a)
#Before we can use the k-means algorithm we have to decide how many clusters we want to have in the end.
#Therefore we can use the so called elbow method. 
#It runs the k-means algorithm with different numbers of clusters
#and shows the results. Based on this we can decide how many clusters we should choose.

mydata=kObject.log
wss=(nrow(mydata)-1)*sum(apply(mydata,2, var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#The best number of clusters it now at the “elbow” of the graph. 
#In this case the best number of clusters would we something around 4.
#So we will try to find 4 clusters in our data.
#Run the K Means algorithm, specifying 4 centers
user2Means.log <- kmeans(kObject.log, centers=4, iter.max=10, nstart=100)

#Add the vector of specified clusters back to the original vector as a factor

userNeighbors.df$cluster <- factor(user2Means.log$cluster)
# now that we have our data we can plot it with the rCharts library
library(rCharts)
p2 <- nPlot(logFoloowersCount ~ logFriendsCount, group = 'cluster', data = userNeighbors.df, type = 'scatterChart')

p2$xAxis(axisLabel = 'Followers Count')

p2$yAxis(axisLabel = 'Friends Count')

p2$chart(tooltipContent = "#! function(key, x, y, e){
         return e.point.screenName + ' Followers: ' + e.point.followersCount +' Friends: ' + e.point.friendsCount
         } !#")

p2

