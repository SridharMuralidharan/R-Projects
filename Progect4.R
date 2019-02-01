basic<-read.csv("~/Desktop/LoanStats3c.csv",header=TRUE,stringsAsFactors = FALSE) #skip=1 for removing top row 

names<-c('zip_code','verification_status','grade', 'sub_grade','term', 'loan_amnt','annual_inc', 'purpose', 'tax_liens',
         'pct_tl_nvr_dlq', 'int_rate', 'loan_status','home_ownership')

loan<-as.data.frame(basic[,names])
loan$int_rate<-gsub('%','',loan$int_rate)
loan$zip_code<-gsub('xx','',loan$zip_code)
loan$int_rate<-as.numeric(loan$int_rate) 


loan$perpaid <- as.numeric(loan$loan_status == 'Fully Paid')
loan$lscurrent<-as.numeric(loan$loan_status=='Current')
loan$lslate<-as.numeric(loan$loan_status=='Late (31-120 days)'|loan$loan_status=='Late (16-30 days)'|loan$loan_status=='In Grace Period')
loan$defaulted<-as.numeric(loan$loan_status=='Default')
loan$chargedoff<-as.numeric(loan$loan_status=='Charged Off')
loan$percons <- as.numeric(loan$purpose == 'debt_consolidation')
loan$perhown <- as.numeric(loan$home_ownership == 'OWN'|loan$home_ownership == 'MORTGAUGE')
loan$term60<-as.numeric(loan$term ==' 60 months')
loan<- loan[complete.cases(loan[,c(1:14)]),]


###############################################      Packages    #####################################################
install.packages("devtools")
library(devtools)
install_github('arilamstein/choroplethrZip@v1.5.0')
require(choroplethrZip)
install.packages("sqldf")
require(sqldf)
install.packages("jtools")
require(jtools)
install.packages("ggplot2")
require (ggplot2)
###############################################################################################################

hist(loan$loan_amnt) ##right Skewed
hist(loan$annual_inc,breaks = 2000, xlim = c(0,250000)) ##right skewed
hist(loan$int_rate) ##right skewed
hist(loan$pct_tl_nvr_dlq) ##heavily left skewed


##################################################################   aggregates    ###################################################

# ZIP x Verification Status  

zipver <- sqldf('SELECT zip_code, verification_status, term, sub_grade, AVG(Int_rate) as AVGinterestRate, 
  AVG(loan_amnt) as avgLoanAmnt, AVG(annual_inc) as avgAnnualInc, 
  AVG(tax_liens) as avgTaxLiens, AVG(pct_tl_nvr_dlq) AS avgPercNeverDlq,AVG(term60) as PerTerm60,
  AVG(perpaid) as perPaidLoans, (1-AVG(perpaid)) as perUnPaidLoans, AVG(percons) as avgPctConsolidated, AVG(perhown) as PerHomeOwners
               FROM loan
      GROUP BY zip_code, verification_status')


#	ZIP x Verification Status x Term x Subgrade. 
zipvertermsub <- sqldf('SELECT zip_code, verification_status, term60, sub_grade, AVG(Int_rate) as AVGinterestRate, 
  AVG(loan_amnt) as avgLoanAmnt, AVG(annual_inc) as avgAnnualInc,avg(perpaid) as avgloansPaidOff, avg(defaulted) as avgdefault, avg(lscurrent) as avgCurrentLoans, avg(lslate) as avgLatetLoans,
  avg(chargedoff)as avgLoansChargedOff, AVG(tax_liens) as avgTaxLiens, AVG(pct_tl_nvr_dlq) AS avgPercNeverDlq, 
  AVG(perpaid) as perPaidLoans, (1-AVG(perpaid)) as perUnPaidLoans, AVG(percons) as avgPctConsolidated, AVG(perhown) as PerHomeOwners
               FROM loan
               GROUP BY zip_code, verification_status, term, sub_grade')

#	ZIP x Verification Status x Term x Subgrade X loanstatus 

zipvertermsubstat <- sqldf('SELECT zip_code, verification_status,term, sub_grade, loan_status,AVG(Int_rate) as AVGinterestRate, 
                       AVG(loan_amnt) as avgLoanAmnt, AVG(annual_inc) as avgAnnualInc,avg(perpaid) as avgloansPaidOff, avg(defaulted) as avgdefault,avg(lscurrent) as avgCurrentLoans, avg(lslate) as avgLatetLoans,
                        avg(chargedoff)as avgLoansChargedOff, AVG(tax_liens) as avgTaxLiens, AVG(pct_tl_nvr_dlq) AS avgPercNeverDlq, 
                       AVG(perpaid) as perPaidLoans, (1-AVG(perpaid)) as perUnPaidLoans, AVG(percons) as avgPctConsolidated, AVG(perhown) as PerHomeOwners
                       FROM loan
                       GROUP BY zip_code, verification_status, term, sub_grade,loan_status')

#	ZIP x Verification Status x Term x Subgrade X Purpose
zipvertermsubpurp <- sqldf('SELECT zip_code, verification_status,term, sub_grade, purpose,AVG(Int_rate) as AVGinterestRate, 
                           AVG(loan_amnt) as avgLoanAmnt, AVG(annual_inc) as avgAnnualInc,avg(perpaid) as avgloansPaidOff, avg(defaulted) as avgdefault,avg(lscurrent) as avgCurrentLoans, avg(lslate) as avgLatetLoans,
                           avg(chargedoff)as avgLoansChargedOff, AVG(tax_liens) as avgTaxLiens, AVG(pct_tl_nvr_dlq) AS avgPercNeverDlq, 
                           AVG(perpaid) as perPaidLoans, (1-AVG(perpaid)) as perUnPaidLoans, AVG(percons) as avgPctConsolidated, AVG(perhown) as PerHomeOwners
                           FROM loan
                           GROUP BY zip_code, verification_status, term, sub_grade,purpose')

##################################################### Part A Q.4 ###################################################

##interest rate goes up as subgrade goes down
plot(zipvertermsub$sub_grade,zipvertermsub$AVGinterestRate,xlab="SubGrade", ylab="Interest Rate",
     main="Subgrade by Average Interest rate")

##investigaing interest rate and subgrade by loan status 
reg<-lm(int_rate~sub_grade+loan_status,data=loan)
ggplot(reg,aes(loan$sub_grade,loan$int_rate,color=factor(loan$loan_status),xlab="Sub Grade",ylab="Interest Rate"))+stat_smooth(method="lm")+geom_point()

summary(reg)

ggplot(reg,aes(loan$sub_grade,loan$int_rate,color=factor(loan$loan_status)))+stat_smooth(method="lm")+geom_point()

##investigating interest rate by annual income - tells us that on average as annual income goes up the int rate goes down 

reg<-lm(zipvertermsub$AVGinterestRate[zipvertermsub$avgAnnualInc<90000]~zipvertermsub$avgAnnualInc[zipvertermsub$avgAnnualInc<90000])

plot(zipvertermsub$avgAnnualInc[zipvertermsub$avgAnnualInc<90000],zipvertermsub$AVGinterestRate[zipvertermsub$avgAnnualInc<90000],
     xlab="Annual Income",ylab="Interest Rate",pch=20,col = alpha("darkgreen", 0.4),main = "Figure2")
abline(reg,col="red",lwd=2)
summary(reg)

##Investigating previous delinquencies 
reg<-lm(zipvertermsub$AVGinterestRate~zipvertermsub$avgPercNeverDlq)

plot(zipvertermsub$avgPercNeverDlq,zipvertermsub$AVGinterestRate,
     xlab="Average Percent loans Without Previous Delinquencies",ylab="Interest Rate",pch=20,col = alpha("darkblue", 0.4),main = "Figure4")
abline(reg,col="red",lwd=2)
summary(reg)

##same but for tax liens
reg<-lm(zipvertermsub$AVGinterestRate~zipvertermsub$avgTaxLiens)

plot(zipvertermsub$avgTaxLiens,zipvertermsub$AVGinterestRate,
     xlab="Average Number of Tax Liens",ylab="Interest Rate",pch=20,col = alpha("purple", 0.4),main = "Figure 5")
abline(reg,col="red",lwd=2)

##are you more likely to pay off your lown if you have a higher annual income 
reg<-lm(zipvertermsub$avgloansPaidOff[zipvertermsub$avgAnnualInc<90000]~zipvertermsub$avgAnnualInc[zipvertermsub$avgAnnualInc<90000])
summary(reg)

plot(zipvertermsub$avgloansPaidOff[zipvertermsub$avgAnnualInc<90000]~zipvertermsub$avgAnnualInc[zipvertermsub$avgAnnualInc<90000],
     xlab="Annual Income",ylab="percentage of loans paid off")
abline(reg,col="red",lwd=2)

## As the subgrade goes down the average percentage of the ploans that are used for debt consolidation goes up
plot(zipvertermsub$sub_grade,zipvertermsub$avgPctConsolidated,xlab="SubGrade",
     main="Subgrade by Percentage of Loans used for Debt Consolidation")

## The size of the loan does effect the subgrade of the loan but very subtely but them more pronounced as you move towords the lower subgrades
plot(zipvertermsub$sub_grade,zipvertermsub$avgLoanAmnt,xlab="SubGrade",
     main="Subgrade by Average Loan Amount")

## There is very little correlation between loan amount and interest rate
plot(zipvertermsub$AVGinterestRate,zipvertermsub$avgLoanAmnt,xlab="interest rate",ylab=("Average Loan Amount"),
     main="Interest Rate by Average Loan Amount")

summary(lm(zipvertermsub$AVGinterestRate~zipvertermsub$avgLoanAmnt))

##The lower the subgrade the more stringint the verification procedure 
plot(zipvertermsub$sub_grade,zipvertermsub$verification_status,xlab="SubGrade",main="Subgrade by Verification Type")

##the lower the average percentage of loans that never went delinquent, the lower the subgrade on average 
plot(zipvertermsub$sub_grade,zipvertermsub$avgPercNeverDlq,xlab="SubGrade",ylab="Percentage",
     main="Subgrade by Percentage of Loans without Previous Delinquencies")
summary(lm(zipvertermsub$AVGinterestRate~zipvertermsub$avgPercNeverDlq))

summary(lm(loan$int_rate~loan$annual_inc))
summary(lm(zipvertermsub$AVGinterestRate~zipvertermsub$avgAnnualInc))
summary(lm(zipvertermsub$AVGinterestRate~zipvertermsub$avgTaxLiens))
summary(lm(zipvertermsub$AVGinterestRate~zipvertermsub$avgPercNeverDlq))

#################################################### Question 5 for Part A #################################################
## if p is less than .05 it is statistically signifigant

#	Does the term of the loan predict the status of the loan?  Think about what this comparison tells you about the structure of the data.
summary(lm(zipvertermsub$term~zipvertermsub$avgloansPaidOff+zipvertermsub$avgLoansChargedOff+zipvertermsub$avgCurrentLoans))
reg<-lm(zipvertermsub$term~zipvertermsub$avgloansPaidOff+zipvertermsub$avgLoansChargedOff+zipvertermsub$avgCurrentLoans)


plot(zipvertermsub$term~zipvertermsub$avgLoansChargedOff)
abline(reg)
par(mfrow=c(1,2))
ggplot(reg,aes(zipvertermsub$term,zipvertermsub$avgloansPaidOff))+stat_smooth(method="lm")+
  geom_point()+labs(x="Term",y="Percentage",title="Percentage of Loans Paid Off by Term")+
  scale_x_continuous(labels=c("0.00" = "36 Months", "0.25" = "","0.50" = "","0.75"="","1.00"= "60 Months"))

ggplot(reg,aes(zipvertermsub$term,zipvertermsub$avgLoansChargedOff))+stat_smooth(method="lm")+
  geom_point()+labs(x="Term",y="Percentage",title="Percentage of Loans Charged Off by Term")+
  scale_x_continuous(labels=c("0.00" = "36 Months", "0.25" = "","0.50" = "","0.75"="","1.00"= "60 Months"))

summary(reg)

##the term does have a minor predictive effect on if the loan is fully paid off or Charged off but not if the loan is current 
 
summary(lm(zipvertermsub$term~zipvertermsub$avgloansPaidOff+zipvertermsub$avgLoansChargedOff+zipvertermsub$avgCurrentLoans))

summary(lm(zipvertermsubstat$term~zipvertermsubstat$avgloansPaidOff+zipvertermsubstat$avgLoansChargedOff+zipvertermsubstat$avgCurrentLoans))
str(zipvertermsubstat)

## term and subgrade
reg<-lm(zipvertermsub$term~zipvertermsub$sub_grade)




################
#	Does annual income matter for the loan amount and/or its interest rate? 
#Is the relationship any different if the income is verified, source verified, or not verified? 
modelline<-lm(zipvertermsub$avgAnnualInc~zipvertermsub$avgLoanAmnt)
summary(modelline)
plot(zipvertermsub$avgAnnualInc~zipvertermsub$avgLoanAmnt,ylim=c(0,300000),ylab= "Average Annual Income", xlab="Average Loan Amount")
abline(modelline,col="red")
summary(lm(zipvertermsub$avgLoanAmnt~zipvertermsub$avgAnnualInc*zipvertermsub$verification_status))

reg<-(lm(avgAnnualInc~avgLoanAmnt+verification_status,data=zipvertermsub))
interact_plot(reg,pred="avgLoanAmnt", modx = "verification_status")

# same thing but now interest rate 
modelline<-lm(zipvertermsub$avgAnnualInc~zipvertermsub$AVGinterestRate)
plot(zipvertermsub$avgAnnualInc~zipvertermsub$AVGinterestRate,ylim=c(0,300000),ylab= "Average Annual Income", xlab="Average Interest Rate")
abline(modelline,col="red")

reg<-(lm(avgAnnualInc~AVGinterestRate*verification_status,data=zipvertermsub))
interact_plot(reg,pred="AVGinterestRate", modx = "verification_status")

# yes the annual income does matter for the loan amount. No loans where given below a certain threshold and the slope o fthe model line is positive. 
#If you look at the summary of the lminear model line it shows 
#that for every additional dollar of annual income you the average loan amount goes up 2.38 dollars 

###########

#Is the subgrade assigned to the loan useful for predicting the likelihood of defaulting on the loan ("Charged Off" means the borrower defaulted)?  
#As you think about this question, think about whether examining 36-month term versus 60-month term would tell you different information.
summary(lm(zipvertermsub$avgLoansChargedOff~zipvertermsub$sub_grade))
summary(lm(zipvertermsub$avgLoansChargedOff~zipvertermsub$sub_grade+zipvertermsub$term60))  

reg<-(lm(avgLoansChargedOff~sub_grade+term60,data=zipvertermsub))

cat_plot(reg,pred = "sub_grade",modx="term60",modx.labels = c("36 Month Term","60 Month Term"),main.title = "Subgrade Vs. Loan Dafualt")


#A Yes it is. Every subgrade you move down from A1 has a higher chance of having the loan be chared off

##########

#	Conditional on the loan's subgrade, are borrower credit characteristics (i.e., tax liens, previous delinquencies) 
#important for whether the borrower will default on the loan? 
#As you think about this question, think about whether examining 36-month term versus 60-month term would tell you different information.
reg<-(lm(avgLoansChargedOff~sub_grade+avgTaxLiens+avgPercNeverDlq+term60 ,data=zipvertermsub)) 

cat_plot(reg,pred = "sub_grade",modx="term60",modx.labels = c("36 Month Term","60 Month Term"),main.title = "Credit Characteristics and Subgrade vs. Loan Default")
summary(reg) 

#	Conditional on the loan's subgrade, are borrower credit characteristics (i.e., tax liens, previous delinquencies) important 
#for the interest rate or the loan amount?

reg<-(lm(AVGinterestRate~sub_grade+avgTaxLiens+avgPercNeverDlq+term60 ,data=zipvertermsub)) 

cat_plot(reg,pred = "sub_grade",modx="term60",modx.labels = c("36 Month Term","60 Month Term"),main.title = "Credit Characteristics and Subgrade vs. Avg Interest Rate")
summary(reg) 
reg<-(lm(avgLoanAmnt~sub_grade+avgTaxLiens+avgPercNeverDlq+term60 ,data=zipvertermsub)) 

#No, there are no notable signifigant interactions between tax liens, previous delinquencies, and subgrade in the data. 
avgPercNeverDlq+term60
summary(lm(AVGinterestRate~avgTaxLiens,data=zipvertermsub)) 
summary(lm(AVGinterestRate~avgPercNeverDlq,data=zipvertermsub)) 
summary(lm(AVGinterestRate~term60,data=zipvertermsub)) 

summary(lm(avgLoanAmnt~avgTaxLiens+avgPercNeverDlq+term60,data=zipvertermsub)) 
summary(lm(AVGinterestRate~avgPercNeverDlq,data=zipvertermsub)) 
summary(lm(AVGinterestRate~term60,data=zipvertermsub)) 
#	Does the purpose of the loan or the borrower's homeownership status matter for the likelihood of defaulting on the loan?
reg<-lm(avgLoansChargedOff~purpose+PerHomeOwners,data=zipvertermsubpurp)
summary(reg)
reg<-lm(avgLoansChargedOff~PerHomeOwners,data=zipvertermsubpurp)

##No, it dosnt fr home ownership but it does for purpose

################################################################# Part B ###########################################################

Partb<-read.csv("~/Desktop/14zpallnoagi.csv",header=TRUE,stringsAsFactors = FALSE)
names<-c("ZIPCODE", "N02650","A02650")
zc<-as.data.frame(Partb[,names])

##ZIPCODE is the 5-digit ZIP code (unit of observation for the data set), 
#N02650 is the number of tax returns from the ZIP code that report a total income, 
#and A02650 is total amount of total income reported by taxpayers in that ZIP.  
zc<-sqldf("SELECT ZIPCODE, N02650,A02650,A02650/N02650 as ati from zc" )
zc<-zc[(2:27787),]
map<-zc[,c(1,4)]
map$region<-map$ZIPCODE
map$value<-map$ati
map<-map[,3:4]

map<-map[map$region!=0,]
map<-map[map$region!=99999,]
map$charc<-nchar(sub('^0+','',sub('\\.','',map$region)))
map$region<-ifelse(map$charc==4,paste0("0", map$region),map$region)

states<-c("alabama", "arizona", "arkansas", "california","colorado", "connecticut", "delaware", "florida", "georgia","idaho","illinois","indiana","iowa", 
"kansas", "kentucky", "louisiana", "maine", "maryland", "massachusetts", "michigan", "minnesota", "mississippi","missouri", "montana","nebraska","nevada", 
"new hampshire", "new jersey", "new mexico","new york","north carolina", "north dakota", "ohio", "oklahoma", "oregon", "pennsylvania","rhode island", 
"south carolina", "south dakota", "tennessee","texas", "utah","vermont", "virginia","washington", "west virginia", "wisconsin", "wyoming")

choro = choroplethrZip::ZipChoropleth$new(map)
choro$prepare_map()

data(zip.regions)
choro$legend = "Average Taxpayer Income (in Thousands)"
states_zips = zip.regions[zip.regions$state.name %in% states, "region"]
states_df   = choro$choropleth.df[choro$choropleth.df$region%in% states_zips, ]
states_plot = choro$render_helper(states_df, "", choro$theme_clean()) + 
  ggtitle("2014 Average Taxpayer Income by Zip Code")

states_plot + coord_map() 
################################################# Merge ##########################################################

zc<-sqldf("SELECT ZIPCODE, N02650,A02650,A02650/N02650 as ati from zc" )
zc<-zc[(2:27787),]


hist(zc$ati,breaks = 2000, xlim = c(0,10),main="Distribution of Average Taxpayer Income in 2014 in thousands",xlab="Income by Taxpayer")
zc<-zc[zc$ZIPCODE!=0,]
zc<-zc[zc$ZIPCODE!=99999,]


hist(zc$ati,breaks = 2000, xlim = c(0,1000),main="Distribution of Average Taxpayer Income in 2014 in thousands",xlab="Income by Taxpayer")

zc$charc<-nchar(sub('^0+','',sub('\\.','',zc$ZIPCODE)))
zc$ZIPCODE<-ifelse(zc$charc==4,paste0("0", zc$ZIPCODE),zc$ZIPCODE)

mergezc<-sqldf("SELECT leftstr(ZIPCODE,3) as zip_code, avg(N02650) as Avgnumtaxreturn, avg(A02650) as avgincomperzip, (avg(A02650)/avg(N02650))as AverageTaxpayerIncome
               from zc GROUP BY zip_code")  

fdf<-sqldf("select * from zipver z
            join mergezc m on m.zip_code=z.zip_code")
fdf$AverageTaxpayerIncome<-(fdf$AverageTaxpayerIncome*1000)


reg<-summary(lm(fdf$avgAnnualInc~fdf$AverageTaxpayerIncome))
plot(fdf$avgAnnualInc~fdf$AverageTaxpayerIncome)  
abline(reg,col="red")


##Verified
reg1<-summary(lm(fdf$avgAnnualInc[fdf$verification_status=="Verified"]~fdf$AverageTaxpayerIncome[fdf$verification_status=="Verified"]))


##source Verified
reg2<-summary(lm(fdf$avgAnnualInc[fdf$verification_status=="Source Verified"]~fdf$AverageTaxpayerIncome[fdf$verification_status=="Source Verified"]))


##not verufied
reg3<-summary(lm(fdf$avgAnnualInc[fdf$verification_status=="Not Verified"]~fdf$AverageTaxpayerIncome[fdf$verification_status=="Not Verified"],main="Not Verifieid"))
reg1
reg2
reg3


par(mfrow=c(1,1))
plot(fdf$avgAnnualInc[fdf$verification_status=="Not Verified"],fdf$AverageTaxpayerIncome[fdf$verification_status=="Not Verified"],pch=16,main="Average Annual Income Reported vs.IRS by Verification",xlab="Reported to Loaner",ylab="IRS Average for Zip Code",col="black")
points(fdf$avgAnnualInc[fdf$verification_status=="Verified"],fdf$AverageTaxpayerIncome[fdf$verification_status=="Verified"],col="red",pch=20)
points(fdf$avgAnnualInc[fdf$verification_status=="Source Verified"],fdf$AverageTaxpayerIncome[fdf$verification_status=="Source Verified"],col="blue",pch=20)
abline(reg1,col="red",lwd=2)
abline(reg2,col="blue",lwd=2)
abline(reg3,col="Black",lwd=2)
abline(0,1,col="green",lwd=3)
legend("topleft",legend= c("Not Verified Income","Verified Income","Source Verified Income","Reporting Income at ZIP Code Average")
       , fill=c("black","red","blue","green"),col=c("black","red","blue","green"))

par(mfrow=c(1,3))
plot(fdf$avgAnnualInc[fdf$verification_status=="Not Verified"],fdf$AverageTaxpayerIncome[fdf$verification_status=="Not Verified"],pch=16,main="Average Annual Income Reported vs.IRS by Verification",xlab="Reported to Loaner",ylab="IRS Average for Zip Code",col="black")
points(fdf$avgAnnualInc[fdf$verification_status=="Verified"],fdf$AverageTaxpayerIncome[fdf$verification_status=="Verified"],col="red",pch=20)
points(fdf$avgAnnualInc[fdf$verification_status=="Source Verified"],fdf$AverageTaxpayerIncome[fdf$verification_status=="Source Verified"],col="blue",pch=20)
abline(reg1,col="red",lwd=2)
abline(reg2,col="blue",lwd=2)
abline(reg3,col="Black",lwd=2)
abline(0,1,col="green",lwd=3)
legend("topleft",legend= c("Not Verified Income","Verified Income","Source Verified Income","Reporting Income at ZIP Code Average")
       , fill=c("black","red","blue","green"),col=c("black","red","blue","green"))

par(mfrow=c(1,3))
plot(fdf$avgAnnualInc[fdf$verification_status=="Not Verified"],fdf$AverageTaxpayerIncome[fdf$verification_status=="Not Verified"],pch=16,main="Non Verifieid Loans",xlab="Reported to Loaner",ylab="IRS Average for Zip Code",col="black")
abline(reg3,col="red",lwd=2)

plot(fdf$avgAnnualInc[fdf$verification_status=="Verified"],fdf$AverageTaxpayerIncome[fdf$verification_status=="Verified"],pch=16,main="Verifid Loans",xlab="Reported to Loaner",ylab="IRS Average for Zip Code",col="black")
abline(reg1,col="red",lwd=2)

plot(fdf$avgAnnualInc[fdf$verification_status=="Source Verified"],fdf$AverageTaxpayerIncome[fdf$verification_status=="Source Verified"],pch=16,main="Source Verifid Loans",xlab="Reported to Loaner",ylab="IRS Average for Zip Code",col="black")
abline(reg2,col="red",lwd=2)

print(reg1) 
print(reg2)
print(reg3)





#######################################     4 Questions     ######################################################

##a.	Is there evidence that the incomes of Lending Club (LC) borrowers and IRS taxpayers are drawn from distributions with the same average income?  

t.test(fdf$AverageTaxpayerIncome,fdf$avgAnnualInc)
summary(lm(fdf$avgAnnualInc~fdf$AverageTaxpayerIncome))
par(mfrow=c(1,1))

hist((fdf$AverageTaxpayerIncome),breaks = 200, xlim = c(0,250000),fill=TRUE,col="blue",main="Average Income by Zipcode with Means Plotted")
hist(fdf$avgAnnualInc,breaks = 200, xlim = c(0,250000),fill=TRUE,col="red",main = "Reported Annual AVGIncome by Zipcode with mean plotted",add=T)
legend("topright",inset=c(-0.15,0),legend= c("Reported AVGIncome at ZIP Code","IRS AVG Income by ZIP Code")
       , fill=c("red","blue"),col=c("red","blue"),box.lty = 0)
abline(v=mean(fdf$AverageTaxpayerIncome),col="darkblue",lwd=2)
abline(v=mean(fdf$avgAnnualInc),col="darkred",lwd=2)

#No, there is evidence that the means are not the same. 

#Do LC borrower income and IRS borrower income have a similar spread? 

par(mfrow=c(1,2))
boxplot(fdf$AverageTaxpayerIncome,ylim=c(0,200000),main = "IRS Average income by Zipcode")
boxplot(fdf$avgAnnualInc,ylim=c(0,200000),main = "Average Reported Annual Income by Zipcode")

#The spreads of the data are pretty simmilar. with IRS being a bit longer

#How are these conclusions affected by the choices you made when you aggregated the data and merged?

boxplot(loan$annual_inc,main="Reported Annual Income",ylim=c(0,800000))
boxplot(fdf$avgAnnualInc,ylim=c(0,800000),main = "Average Reported Annual Income by Zipcode")

t.test(loan$annual_inc,fdf$avgAnnualInc)
zc$ati<-zc$ati*1000
t.test(loan$annual_inc,zc$ati) #t test between both unagreated data sets - means are differnt 

hist((loan$annual_inc),breaks = 2000, xlim = c(0,250000),fill=TRUE,col="red",main="Income Non-Aggregated")
hist(zc$actualati,breaks = 2000, xlim = c(0,250000),fill=TRUE,col="blue",main = "Reported Annual Income",add=T)
legend("topright",legend= c("Reported Income at ZIP Code","IRS Income by ZIP Code")
       ,fill=c("red","blue"),col=c("red","blue"),box.lty = 0, inset=c(-0.15,0))

##unaggregated the data is much more spread out and heavily left skewed. Aggregating is taking away some of the left skew away from the data
## means of non aggregated data are not the same but the original conclusion remains the same. 
#agregeating takes some of the skew out of the distributions but it shifts the means left. 

##They are not taken from the same distribution and aggregating shiftes the mean higher but gets rid of left skew
##data becomes more correlated the further you aggregeate

######################    Q2
#b.	Does verification strengthen the relationship between Lending Club income and verified IRS reported income?

reg<-lm(avgAnnualInc~AverageTaxpayerIncome*verification_status,data=fdf)
interact_plot(reg,pred = AverageTaxpayerIncome,modx = verification_status) 
reg1<-lm(avgAnnualInc~AverageTaxpayerIncome,data=fdf)
summary(reg)
summary(reg1) 

#Yes, it does. 


################# Q3
#c.	Construct a binned IRS income variable to help with this one. 
#Does IRS income exhibit a stronger relationship with LC income for different parts of the distribution of income
#(e.g., the top quartile versus the bottom quartile)?

reg<-lm(avgAnnualInc~AverageTaxpayerIncome*verification_status,data=fdf)
summary(reg) #r2=.4283
reg<-lm(avgAnnualInc~AverageTaxpayerIncome,data=fdf)
summary(reg) #r2=.283

fdf$abovemean<-ifelse(fdf$avgAnnualInc>fdf$AverageTaxpayerIncome,1,0)
reg<-lm(avgAnnualInc[abovemean==1]~AverageTaxpayerIncome[abovemean==1]*verification_status[abovemean==1],data=fdf)
summary(reg)##r2=.4433
reg1<-lm(avgAnnualInc[abovemean==0]~AverageTaxpayerIncome[abovemean==0]*verification_status[abovemean==0],data=fdf)
summary(reg1) #r2=.6131



##yes for when the reported income is less than the zip code mean the correalation goes up to .613 from .28 when broken up by above/below mean IRS
q1<-quantile(fdf$avgAnnualInc,.25)
q2<-quantile(fdf$avgAnnualInc,.5)
q3<-quantile(fdf$avgAnnualInc,.75)


regq1<-lm(avgAnnualInc[avgAnnualInc<=q1]~AverageTaxpayerIncome[avgAnnualInc<=q1]*verification_status[avgAnnualInc<=q1],data=fdf)
regq2<-lm(avgAnnualInc[avgAnnualInc>=q1 & avgAnnualInc<=q2]~AverageTaxpayerIncome[avgAnnualInc>=q1 & avgAnnualInc<=q2]*verification_status[avgAnnualInc>=q1 & avgAnnualInc<=q2],data=fdf)
regq3<-lm(avgAnnualInc[avgAnnualInc>=q2 & avgAnnualInc<=q3]~AverageTaxpayerIncome[avgAnnualInc>=q2 & avgAnnualInc<=q3]*verification_status[avgAnnualInc>=q2 & avgAnnualInc<=q3],data=fdf)
regq4<-lm(avgAnnualInc[avgAnnualInc>=q3]~AverageTaxpayerIncome[avgAnnualInc>=q3]*verification_status[avgAnnualInc>=q3],data=fdf)

summary(regq1)##r2=.06995
summary(regq2)##r2= .02826
summary(regq3)##r2=.04759 
summary(regq4)##r2= .3278 


###### summary
#disagregated data is a much 

