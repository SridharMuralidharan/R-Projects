setwd("C:/Users/ashle/OneDrive/Desktop/Quant Methods 5030/Project/Project 3/YouTube2013Attention")

######################################################        Revenue            #############################################

folderdata=function(paths){
  dat=NULL
  for (i in 1:length(paths)){
    this=read.csv(paths[i],header=TRUE)
    this=cbind(this,paths[i])
    dat=rbind(dat,this)
  }
  return (dat)
}

base="C:/Users/ashle/OneDrive/Desktop/Quant Methods 5030/Project/Project 3/YouTube2013Attention/Revenues"
yt_paths = list.files(base,full.names = TRUE)
revs=folderdata(yt_paths)
names(revs)<-c("DATE","Total.earn","AFV.earn","YT.earn","Transactions","video.id")
revs$video.id <- gsub('C:/Users/ashle/OneDrive/Desktop/Quant Methods 5030/Project/Project 3/YouTube2013Attention/Revenues/Revenue',
                      '', revs$video.id)
revs$video.id <- gsub('.csv','', revs$video.id)

revs$DATE<-as.character(revs$DATE)                      
revs$DATE <- as.Date(revs$DATE, format='%b %d, %Y') 


##############      Vids


folderdata=function(paths){
  dat=NULL
  for (i in 1:length(paths)){
    this=read.csv(paths[i],header=TRUE)
    this=cbind(this,paths[i])
    dat=rbind(dat,this)
  }
  return (dat)
}


base="C:/Users/ashle/OneDrive/Desktop/Quant Methods 5030/Project/Project 3/YouTube2013Attention/Videos"
yt_paths=list.files(base,full.names=TRUE)
vids=folderdata(yt_paths)
names(vids)<-c("DATE","Views","Estimated.min.watched","Avg.view.duration","Unique.cookies","video.id")
vids$video.id <- gsub('C:/Users/ashle/OneDrive/Desktop/Quant Methods 5030/Project/Project 3/YouTube2013Attention/Videos/Video',
                      '', vids$video.id)
vids$video.id <- gsub('.csv','', vids$video.id)

vids$DATE<-as.character(vids$DATE)

a <- as.Date(vids$DATE,format="%b %d, %Y") # Produces NA when format is not "%m/%d/%Y"
b <- as.Date(vids$DATE,format="%d-%b-%y") # Produces NA when format is not "%d.%m.%Y"
a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
vids$DATE <- a # Put it back in your dataframe

#Merge Dataframes 

alldata <- merge(vids, revs, on = c(video.id, DATE), all = TRUE)
summary(alldata$Total.earn)

names(alldata) <-c("DATE","video.id","views","est.min.watched","average_watch_time","unique_cookies", "daily.earnings","AFV.earn","YouTube.earn","transactions")
alldata$daily.earnings<-as.numeric(as.character(gsub("\\$", "", alldata$daily.earnings)))
alldata$YouTube.earn <-as.numeric(as.character(gsub("\\$", "", alldata$YouTube.earn)))
alldata$AFV.earn <-as.numeric(as.character(gsub("\\$", "", alldata$AFV.earn)))
alldata$transactions <-as.numeric(as.character(gsub("\\$", "", alldata$transactions)))
alldata$earnings_per_view<-alldata$daily.earnings/alldata$views

##add in adinstream data
adinstream<-read.csv("~/Desktop/adinstream.csv",stringsAsFactors = FALSE,header=TRUE)

adinstream[5,1]<-"Reg1"
adinstream[6,1]<-"Reg2"
adinstream[7,1]<-"Reg2"
adinstream[65,1]<-"WhatIs"
adinstream[adinstream$video.id=="e11",1]<-"E11"
adinstream[adinstream$video.id=="e12",1]<-"E12"
adinstream[adinstream$video.id=="e13",1]<-"E13"
adinstream[adinstream$video.id=="e14",1]<-"E14"
adinstream[adinstream$video.id=="e15",1]<-"E15"
adinstream[adinstream$video.id=="e16",1]<-"E16"
adinstream[adinstream$video.id=="e17",1]<-"E17"
adinstream[adinstream$video.id=="e18",1]<-"E18"
adinstream[adinstream$video.id=="e19",1]<-"E19"
adinstream[adinstream$video.id=="e20",1]<-"E20"
adinstream[adinstream$video.id=="e21",1]<-"E21"
adinstream[adinstream$video.id=="e110",1]<-"E110"
adinstream[adinstream$video.id=="e111",1]<-"E111"
adinstream[adinstream$video.id=="r1",1]<-"R1"
adinstream[adinstream$video.id=="r2",1]<-"R2"
adinstream[adinstream$video.id=="r3",1]<-"R3"
adinstream[adinstream$video.id=="r4",1]<-"R4"
adinstream[adinstream$video.id=="r5",1]<-"R5"
adinstream[adinstream$video.id=="r6",1]<-"R6"
adinstream[adinstream$video.id=="r7",1]<-"R7"
adinstream[adinstream$video.id=="r8",1]<-"R8"
adinstream[adinstream$video.id=="r9",1]<-"R9"
adinstream[adinstream$video.id=="r10",1]<-"R10"
adinstream[adinstream$video.id=="r11",1]<-"R11"
adinstream[adinstream$video.id=="r12",1]<-"R12"
adinstream[adinstream$video.id=="r13",1]<-"R13"
adinstream[adinstream$video.id=="r14",1]<-"R14"
adinstream[adinstream$video.id=="r15",1]<-"R15"


finaldf<-merge(alldata, adinstream, on = c(video.id,DATE), all = FALSE)

finaldf<-finaldf[,c(1,2,3,5,7,11,12,13,14)] 

finaldf<-finaldf[complete.cases(finaldf[,5:6]),]

## We chose to remove rows with NA's that occur in daily earnings and earnings per view from the data frame because we have no idea if it was missing data or a
#zero earnings value. 




###########################################  Part 2 Question 1  Non Annoying adds vs Annoying adds   ##################################
#1a During the pre-period (before Feb 5th) Is average watch time different for videos that have pre-roll ads versus not?
##not annoying adds is 0 annoyingadds is 1

histogram(awtanoy)
histogram(awtnoadd)

awtanoy<-finaldf$average_watch_time[finaldf$annoy_before ==1 & finaldf$DATE < '2013-02-05']
awtnotanoy<-finaldf$average_watch_time[finaldf$annoy_before ==0 & finaldf$DATE < '2013-02-05']

t.test(awtanoy,awtnotanoy) 



#Because the confidence interval doese not include zero we can be 95% confident that the mean difference in watch time between annoying adds and non annoying adds in the pre watch
#period is not zero. In other words that there is a difference in mean watchtime. From the polarity of the CI we can be confident in saying that the average whatch time
#for videos with the annoying adds is lower than the average watch time for the non annoying adds. 

##1 b Is the number of views different for videos that have pre-roll ads versus not?

viewanoyadd<-finaldf$views[finaldf$annoy_before ==1 & finaldf$DATE < '2013-02-05']
viewnotanoyadd<-finaldf$views[finaldf$annoy_before ==0 & finaldf$DATE < '2013-02-05'] 

t.test(viewanoyadd,viewnotanoyadd)


##Because the confidence interval doese not include zero we can be 95% confident that the difference in mean views between videos with annying adds and non annoying 
#adds in the pre watch period is not zero. In other words that there is a difference in average number of views 


######################################################           Question 2   Full Sample         ##############################################
#2a. In the full sample, How does attention (measured by average watch time) relate to daily earnings on a video? 
awt<-finaldf$average_watch_time
de<-finaldf$daily.earnings
plot(awt,de,pch=16,col="lightblue")
cor(awt,de)
(cor(awt,de))^2
summary(lm(de~awt))
awt
.0059*98 #money per day additioanl for every one min gain of avg watch time 

#Is this relationship statistically significant? 
    # Yes, since the p-value is less than .05 and the slope is not equal to zero we can conclude that there is a relationship between average watch time and 
#dailyearnings and it is statistically signifigant. However, the slope tells us that forevery miniute of average watch time you increase, the daily earnins by $.0059 
#which is a very difficult task for so small a gain. If you increase the average watch time one minuite it would only equate to an additional $.5782 per day
 

#How do views relate to daily earnings on a video?  
#Is this relationship statistically significant? 

views<-finaldf$views
cor(views,de)
(cor(de,views))^2
summary(lm(de~views))
plot(views,de)
mean(finaldf$views)*.00466*98 


#Yes. since the p-Value is less than .05  and the slope is greater than zerowe can conclued that the relationship is statisticaly signifigant, 
#meaning there is a relationship between views and daily earnings. However, the practical signifigance is in question. 
#For every view you get an additional $.00466 in daily earnings. If you take the mean number of views for each video and multiplied it our would come 
#out to around an additional $10 a day 
 

#How does attention relate to views on a video? 
#Is this relationship statistically significant?

plot(awt,views)
cor(awt,views)
summary(lm(views~awt))

##Yes, since the p-value is less than .05 and the slope is greater than zero we can conclude that the relationship between average watch time and dailyearnings 
#is statistically signifigant. For every one view added you gain .4402 min of average watch time. for every minuite watch time gained we add .44 views 

######################################################       3     #######################################################################

#Define a date-level variable (named "post") that equals whether the date is during the post period (Feb 5 or later, = 1) or during the pre period 
#(Feb 4 or earlier, =0). Using this variable and regression tools, estimate the specification: For your estimate of ????2, is this estimate statistically significant
# Interpret this estimate in the context of the problem.

finaldf$post[finaldf$DATE < '2013-02-05']<-0
finaldf$post[finaldf$DATE >= '2013-02-05']<-1

sum(finaldf$post) 

plot(lm(finaldf$daily.earnings~finaldf$post))

summary(lm(finaldf$daily.earnings~finaldf$post))
lm(finaldf$daily.earnings~finaldf$post)

##winzorize daily earnings due to massive heteroskadistic values 

hi <- quantile(finaldf$daily.earnings, 0.99) 
hi
finaldf$daily.earnings <- ifelse(finaldf$daily.earnings>hi,hi, finaldf$daily.earnings)

## daily earnings on average went down in the post feb5 time frame. 

##hertoskasticity determines when one is more spreadout than the other, OUTLIERS AND INTERPERTTING intercept meaning etc 


########################################################       4         ####################################################
#Define a video-level variable (named "annoy_later") that equals whether a video went from no
#pre-roll advertisements to pre-roll advertisements in the post period (=1 if no pre-roll à pre-roll, =0 otherwise).
#Using this variable and regression tools, estimate the specification:
#annoy later is 1 when the vide was not annoying at first then moved to annoyoing adds

finaldf$annoy_later<-0
finaldf$annoy_later[finaldf$annoy_after==1 & finaldf$annoy_before==0]<-1

lm(finaldf$daily.earnings~finaldf$annoy_later)
summary(lm(finaldf$daily.earnings~finaldf$annoy_later)) 

#it looks like the moving from not annoying adds to annoying adds increased daily earnings by .052 per video per day. 


###################################################         5           #######################################################
#Putting the previous two parts together, estimate the following multiple regression equation
#specification: For your estimate of B3 ????8, is this estimate statistically significant
# Interpret this estimate in the context of the problem.


lm(finaldf$daily.earnings~finaldf$annoy_later*finaldf$post)
 
summary(lm(finaldf$daily.earnings~finaldf$annoy_later*finaldf$post)) 


## annoy later is one when it has an annoying add after feb 5th but not before 
## intercept is the base expected revenue when before feb 5th  






