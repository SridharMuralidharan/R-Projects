##load in CSV Files
ump<-read.csv("~/Desktop/Unemployment.csv",stringsAsFactors = FALSE,header=TRUE)
gdp<-read.csv("~/Desktop/RGDP.csv",stringsAsFactors = FALSE,header=TRUE) 
govspend<-read.csv("~/Desktop/Govspending.csv",stringsAsFactors = FALSE,header=TRUE) 


##Merge Data Sets

proj<-merge(govspend,ump,by="DATE")
data<-merge(proj,gdp,by="DATE")

##coerce date into a DATE field

data$DATE<-as.Date(data$DATE,format = "%m/%d/%Y")

##sort the df
data<-data[order(data$DATE),]

#######################################################2 Plot Taylors Chart#######################################################
##Get percent GDP column
data$pgdp<-(data$GovSpending/data$GDP)*100

#Subset Data
plot1<-data[data$DATE>"1989-10-01" & data$DATE<"2011-01-01",]
#set axis limits and ;labels
xax<-seq(from=17,to=22,by=.5) 
yax<-c(3:12)

##Trendline for Original Plot
modelorig<-lm(Unemployment.Rate~pgdp, data = plot1)

##plot first graph
plot(plot1$pgdp,plot1$Unemployment.Rate,main="Taylor's Original Plot", xlim=c(17,21.5), ylim=c(3,11), xaxt="n",yaxt="n",pch=16,col="blue",
     xlab="Government Purchases as a Percent of GDP",ylab="Unemployment Rate") 
axis(1,xax,labels=xax)
axis(2,yax,labels=yax)



######################################################### 3 plot second graph of all data #########################################################

plot2l<-data[data$DATE<"1989-10-01",] 
plot2u<-data[data$DATE>"2011-01-01",]
xax2<-seq(from=14.5,to=26,by=.5) 
yax2<-c(0:13)

##Model for all data
modelall<-lm(Unemployment.Rate~pgdp, data = data)

plot(plot1$pgdp,plot1$Unemployment.Rate,main="Plot of All Available Data",pch=16,col="blue",xlim=c(14.5,25.5), ylim=c(2,10), xaxt="n",yaxt="n",
     xlab="Government Purchases as a Percent of GDP",ylab="Unemployment Rate") 
points(plot2l$pgdp,plot2l$Unemployment.Rate,pch=16, col="red")
points(plot2u$pgdp,plot2u$Unemployment.Rate,pch=16, col="red")
axis(1,xax2,labels=xax2)
axis(2,yax2,labels=yax2)
abline(modelall, col = "red")
abline(modelorig, col = "blue3")
legend(14.5,10,legend= c("Trendlind for Taylors Plot","Trendline for All Data"), box.lty = 0, fill=c("blue2","red"),col=c("Blue2","red"))

#################################################    Part 4     ############################################################################################

deccor<-as.data.frame(rep(1,243))
colnames(deccor)<-"Correlation"
deccor$Date<-data$DATE[1:243]
deccor$Year<-as.numeric(format(as.Date(deccor$Date,'%Y,%m,%d'),'%Y'))
deccor$Decade<-deccor$Year-deccor$Year%%10
for (i in 1:length(deccor$Year)){
  deccor$Correlation[i] <- cor(data$pgdp[i:(39 +i)], data$Unemployment.Rate[i:(39+i)])
}

yearvec<-deccor$Year

plot(deccor$Correlation, type= "l",main="Rolling Decade Correlation of Goverment Investment as a percent of GDP and Unemployment Rate", 
     xlab="Year",ylab="Correlation",ylim = c(-1,1),lwd=2,xaxt="n")
axis(side=1,at=c(1:243),labels = yearvec, tick=F)
abline(v=168,col="red")
legend(1,1.05,legend= c("When Taylor's Plot Started"), box.lty = 0, fill="red",col="red")


##aggregate to plot 

meanRy<-aggregate(Correlation ~ Decade, FUN=mean, data=deccor)  
Decadelist<-c(1940,1950,1960,1970,1980,1990,2000)
DecName<-c("40's","50's","60's","70's","80's","90's","00's")

plot(meanRy$Decade,meanRy$Correlation,main ='Average Correlation between Unemployement Rate and Goverment Spending as a % of GDP',pch=16,type ="b",
     ylab = 'Correlation', xlab = 'Decade', col = 'blue2') 
#################################################     Part 5      ################################################################################



test<-subset(data,DATE>=1954-01-01)  
test2<-data[25:849,]
test3.1<-test2$Unemployment.Rate
test3.1<-test3.1[1:256]
test3.2<-test2$pgdp
test3.2<-test3.2[1:256]

num_samples<-256
Umean=mean(test3.1)
Pmean=mean(test3.2)
Uvar=var(test3.1)
Pvar=var(test3.2)

x = rnorm(256,Umean,sqrt(Uvar))   
y = rnorm(256, Pmean,sqrt(Pvar)) 


out_stor=NA

for(i in 1:256){
  x = rnorm(i,Umean,sqrt(Uvar))   
  y = rnorm(i,Pmean,sqrt(Pvar)) 
  out_stor[i] = cor(x,y)
  
}
Final5.a<-out_stor[60:256]
max(Final5.a)
min(Final5.a)

max_corr<-NA
for(j in 1:1000)
{
  for(i in 1:256){
    x = rnorm(256,Umean,sqrt(Uvar))   
    y = rnorm(256,Pmean,sqrt(Pvar)) 
    
    out_stor[i] = cor(x,y)
    
  }
  max_corr[j]<-max(out_stor)
}
mean(max_corr)

hist(max_corr,main="Histogram of Cherry Picked Data",col="red")
box()

hist(a,col="blue3",main="Histogram of Non Cherry Picked Data")
box()

##summary data
summary(max_corr)
summary(a)

NC_corr<-NA
a<-NA

for(j in 1:1000)
{
  for(i in 1:256){
    x = rnorm(256,Umean,sqrt(Uvar))   
    
    y = rnorm(256,Pmean,sqrt(Pvar))
    
    NC_corr[i] = cor(x,y)
    
  }
  a[j]=mean(NC_corr)
  
}

##a is non cherry picked cor from generated random var. 
##Max_corr is random var generated with max value stored


Taylor_Ttest<-t.test(max_corr,a)
Taylor_Ttest


##over the date ranges Taylor plotted the cor is .8
cor(plot1$pgdp,plot1$Unemployment.Rate)
## if it should come from the same distribution of cherry picked data
t.test(max_corr,mu=.8)



freqcor<-deccor$Correlation[data$DATE>"1989-10-01" & data$DATE<"2011-01-01"]

hist(deccor$Correlation,main="Frequency of Correlations within all Data",col="yellow",xlab="Correlation")
hist(freqcor,main="Frequency of Correlations within Taylors Plot",col="blue3",xlab="Correlation")










