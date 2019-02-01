##load in Data
g<-read.csv("~/Desktop/sf_gestationdata.csv",header=TRUE) 
column<-c(1,6,8,9,12,17,18,19,22)
g.1<-g[,column]

##change duedatebase to a factor 
g.1$duedatebase<-as.factor(g.1$duedatebase)

##create a new column to convert bornonday to bornonweeks 
g.1$bornonweeks<-g.1$bornonday/7

##Summary of the variables in the Data Frame
summary(g.1)

#create a normilized curve
x = 140:400/7 
d = dnorm(x, mean=(40), sd=(10/7))
d2<-d/sum(d)

##induced vs spontaneous 
xlim<-c(35,45)
ylim1<-c(0,.06)

# creating a vector we can use the length function on to get the graph to plot in frequencies (PMF)
spont<-g.1$bornonweeks[g.1$induced == 0 & g.1$exclude==0]
indu<-g.1$bornonweeks[g.1$induced == 1 & g.1$exclude==0]

#Plotting Spontaneous Births vs Induced Births

plot(((table(g.1$bornonweeks[g.1$induced == 0 & g.1$exclude==0]))/length(spont)),type = "o",pch=16,col="purple",xaxt = "n",main="Gestation Periods for Spontaneous vs. Induced Births",xlim=xlim,ylim=ylim1,xlab="Weeks",ylab="") 
lines(((table(g.1$bornonweeks[g.1$induced == 1 & g.1$exclude==0]))/length(indu)),col="red",type = "o",pch = 16)
axis(1, at=35:45)
legend(41.6, 0.05,legend= c("Spontaneous","Induced","Normal Gestation Curve"), box.lty = 0, fill=c("purple","red","black"),col=c("purple","red"))
abline(v=40,lwd=2)
grid(nx=8,ny=8) 
lines(x, d2,lwd=2,lty="solid",col="black") 

##Plotting CDF for Spontaneous Births vs Induced Births 

x<-ecdf(g.1$bornonweeks[g.1$induced == 0 & g.1$exclude==0])
I<-ecdf(g.1$bornonweeks[g.1$induced == 1 & g.1$exclude==0])

plot(x,xlim=xlim,pch=16,col="purple",main="Probability of pregnancy term by a certain week for Spontaneous Birth",ylab = "Probability",xlab="Weeks")
lines(I,col="red",pch = 16)
abline(v=40)
legend(35, 0.7,legend= c("Spontaneous","Induced"), box.lty = 0, fill=c("purple","red"),col=c("purple","red"))




######################################################## Plotting First Child Graph vs. 2+####################################################################### 
#Setting the y limits
try3<-c(.0,.065)

#creating a vector we can use the length function on to get the graph to plot in frequencies (PMF)

firstchild<-g.1$bornonweeks[g.1$previousbirths == 0 & g.1$exclude==0]
twoplus<-g.1$bornonweeks[g.1$previousbirths > 1 & g.1$exclude==0&g.1$previousbirths<10]

#create a normilized curve
x = 140:400/7 
d = dnorm(x, mean=(40), sd=(10/7))
d2<-d/sum(d)

##plot script

plot(((table(g.1$bornonweeks[g.1$previousbirths == 0 & g.1$exclude==0]))/length(firstchild)),main="Gestation Periods of First Children vs. Latter Births",type = "o",pch=16,col="purple",xaxt = "n",xlim=xlim,ylim=try3,xlab="Weeks",ylab="") 
lines(((table(g.1$bornonweeks[g.1$previousbirths > 1 & g.1$exclude==0 & g.1$previousbirths<10]))/length(twoplus)),col="red",type = "o",pch = 16)
axis(1, at=35:45)
legend(40.2, 0.065,legend= c("First Child","Children After First","Normal Gestaiton Curve"), box.lty = 0, fill=c("purple","red","black"),col=c("purple","red"))
abline(v=40)
grid(nx=8,ny=8) 
lines(x, d2,lwd=2,lty="solid",col="black")

##Plotting CDF for First Child Graph vs. 2+ 
FC<-ecdf(g.1$bornonweeks[g.1$previousbirths == 0 & g.1$exclude==0])
SC<-ecdf(g.1$bornonweeks[g.1$previousbirths > 1 & g.1$exclude==0 & g.1$previousbirths<10])

plot(FC,xlim=xlim,pch=16,col="purple",main="Probability of pregnancy term by First or Latter Children",ylab = "Probability",xlab="Weeks")
lines(SC,col="red",pch = 16)
abline(v=40)
legend(35, 0.7,legend= c("First Child","Latter Children"), box.lty = 0, fill=c("purple","red"),col=c("purple","red"))


################################################ Ultrasound vs LMP vs Ovulation Plot##############################################################################

#resetting variables for loops
ultrasound <- 0
ovulation <- 0
LMP <- 0 
##looping to get lengths for ploting frequencies

for(i in 1:nrow(g.1)) {
  if(g.1$duedatebase[i] == "ultrasound") {
    ultrasound <- ultrasound + 1 
  }	
}

for(i in 1:nrow(g.1)) {
  if(g.1$duedatebase[i] == "LMP") {
    LMP <- LMP + 1
    
  }	}

for(i in 1:nrow(g.1)) {
  if(g.1$duedatebase[i] == "ovulation" ){
    ovulation <- ovulation + 1
    
  }	}
#setting the y parameter of the plot '

ylim3 <- c(0,0.07)

#create a normilized curve
x = 140:400/7 
d = dnorm(x, mean=(40), sd=(10/7))
d2<-d/sum(d)

#plotting the points 
plot(((table(g.1$bornonweeks[g.1$duedatebase == "ultrasound" & g.1$exclude==0]))/ultrasound),main="Gestation Period by Detection Method",type = "o",pch=16,col="purple",xaxt = "n",xlim=xlim,ylim=ylim3,xlab="Weeks",ylab="") 
lines(((table(g.1$bornonweeks[g.1$duedatebase == "LMP" & g.1$exclude==0]))/LMP),col="red",type = "o",pch = 16)
lines(((table(g.1$bornonweeks[g.1$duedatebase == "ovulation" & g.1$exclude==0]))/ovulation),col="green",type = "o",pch = 16)
axis(1, at=34:45)
legend(41, 0.07,legend= c("ultrasound","LMP","ovulation","Normal Gestation Curve"), box.lty = 0, fill=c("purple","red", "green","black"),col=c("purple","red", "green"))
abline(v=40,lwd=2)
grid(nx=8,ny=8)
lines(x, d2,lwd=2,lty="solid",col="black")


##Plotting the CDF for detection method
U<-ecdf(g.1$bornonweeks[g.1$duedatebase == "ultrasound" & g.1$exclude==0])
L<-ecdf(g.1$bornonweeks[g.1$duedatebase == "LMP" & g.1$exclude==0])
O<-ecdf(g.1$bornonweeks[g.1$duedatebase == "ovulation" & g.1$exclude==0])

plot(U,xlim=xlim,pch=16,col="purple",main="Probability of pregnancy term by Detection Method",ylab = "Probability",xlab="Weeks")
lines(L,col="red",pch = 16)
lines(O,col="green",pch = 16)
abline(v=40)
legend(35, 0.7,legend= c("Ultrasound","LMP","Ovulation"), box.lty = 0, fill=c("purple","red","green"),col=c("purple","red","green"))

######################################################## Baby Weight Plot #######################################################################################

#creating a vector we can use the length function on to get the graph to plot in frequencies (PMF)

light<-g.1$bornonweeks[g.1$pounds < 7 & g.1$exclude==0]
heavy<-g.1$bornonweeks[g.1$pounds >=7 & g.1$exclude==0] 

#create a normilized curve
x = 140:400/7 
d = dnorm(x, mean=(40), sd=(10/7))
d2<-d/sum(d)

##plotting heavy vs lighter babies 

plot(((table(g.1$bornonweeks[g.1$pounds >=7 & g.1$exclude==0]))/length(heavy)),type = "o", main="Gestation Period by Baby Weight",pch=16,col="purple",xaxt = "n",xlim=xlim,xlab="Weeks",ylab="") 
lines(((table(g.1$bornonweeks[g.1$pounds < 7 & g.1$exclude==0]))/length(light)),col="red",type = "o",pch = 16)
axis(1, at=35:45)
legend(42, 0.056,legend= c("7lbs or over","less than 7 lbs","Normal Gestation Curve"), box.lty = 0, fill=c("purple","red","black"),col=c("purple","red"))
grid(nx=8,ny=8) 
abline(v=40,lwd=2)
lines(x, d2,lwd=2,lty="solid",col="black")

##Plotting CDF for Baby Weight

lightc<-ecdf(g.1$bornonweeks[g.1$pounds < 7 & g.1$exclude==0])
heavyc<-ecdf(g.1$bornonweeks[g.1$pounds >=7 & g.1$exclude==0])

plot(lightc,xlim=xlim,pch=16,col="purple",main="Probability of pregnancy term by Baby Weight",ylab = "Probability",xlab="Weeks")
lines(heavyc,col="red",pch = 16)
abline(v=40)
legend(35, 0.8,legend= c("7Lbs or more","Less than 7 Lbs"), box.lty = 0, fill=c("purple","red"),col=c("purple","red"))



############################################################### Mother Birth Year Plot ########################################################################## 

#creating a vector we can use the length function on to get the graph to plot in frequencies (PMF)

seventies<-g.1$bornonweeks[g.1$motherbirthyear>=1970 & g.1$motherbirthyear<=1979 & g.1$exclude==0]
eighties<-g.1$bornonweeks[g.1$motherbirthyear >= 1980 & g.1$exclude==0]

#setting a limit on the y axis of the plot 
ylim4<-c(0,.06)


#create a normilized curve
x = 140:400/7 
d = dnorm(x, mean=(40), sd=(10/7))
d2<-d/sum(d)
#plotting the points 

plot(((table(g.1$bornonweeks[g.1$motherbirthyear>=1970 & g.1$motherbirthyear<=1979 & g.1$exclude==0]))/length(seventies)),main="Gestation Period by Age of Mother",type = "o",pch=16,col="purple",xaxt = "n",xlim=xlim,ylim=ylim4,xlab="Weeks",ylab="") 
lines(((table(g.1$bornonweeks[g.1$motherbirthyear >= 1980 & g.1$exclude==0]))/length(eighties)),col="red",type = "o",pch = 16)
axis(1, at=35:45)
legend(41.5, 0.056,legend= c("Mothers age 48-58","Mothers age <= 38","Normal Gestation Curve"), box.lty = 0, fill=c("purple","red","black"),col=c("purple","red"))
grid(nx=8,ny=8)  
abline(v=40,lwd=2)
lines(x, d2,lwd=2,lty="solid",col="black")

##Plotting CDF of Mother birth year

seventiesc<-ecdf(g.1$bornonweeks[g.1$motherbirthyear>=1970 & g.1$motherbirthyear<=1979 & g.1$exclude==0])
eightiesc<-ecdf(g.1$bornonweeks[g.1$motherbirthyear >= 1980 & g.1$exclude==0])


plot(seventiesc,xlim=xlim,pch=16,col="purple",main="Probability of pregnancy term by Mothers Age",ylab = "Probability",xlab="Weeks")
lines(eightiesc,col="red",pch = 16)
abline(v=40)
legend(39.5, .25,legend= c("Mothers born in the 1970s","Mothers Born after 1979"), box.lty = 0, fill=c("purple","red"),col=c("purple","red"))

