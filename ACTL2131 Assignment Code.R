# // Title: Pennsylvania Analysis
# // Author: Jason Khu
# // Student ID: 5254974

#Setting up of working directory ----------------------------------------------->

getwd()
#set up working directory
setwd("/Users/JasonKhu/Desktop/ACTL2131")
getwd()

#Data preprocessing stage ----------------------------------------------->

# Input the dataset
dataset <- read.csv("Pennsylvania.csv", header=TRUE)
typeof(dataset) #list
dataset <- as.data.frame(dataset)
typeof(dataset) #double
# Create vectors for all variables including time
time <- as.character(dataset[,1])
time <- as.Date.character(time, "%m/%d/%Y") #making time readable by R
initialClaims <- as.numeric(dataset[,2]) #... (red)
firstPayments <- as.numeric(dataset[,3]) #... (blue)
weeksClaimed <- as.numeric(dataset[,4]) #... (green)
weeksCompensated <- as.numeric(dataset[,5]) #... (pink)
avgWklyBenefit <- as.numeric(dataset[,6]) #... (purple)
benefitsPaid <- as.numeric(dataset[,7]) #... (orange)
finalPayments <- as.numeric(dataset[,8]) #... (black)

#Questions and Solutions ----------------------------------------------->

#Question 1a

# Show summary statistics of all the variables
basicStats <- summary(dataset) #basic statistics for all variables
basicStats
# More on variance, skewness, kurtosis
install.packages('moments')
library(moments)
  #skewness
skewness(finalPayments) #replace column to get value for variable
  #kurtosis
kurtosis(finalPayments)
  #variance
var(finalPayments)

# Inspect some correlations between variables
datasetCor <- as.matrix(dataset[,c(2,3,4,5,6,7,8)])
corMatrix <- cor(datasetCor, method="pearson")
corMatrix <- as.data.frame(corMatrix)
View(corMatrix)

#Question 1b

# Present exploratory analysis on all variables
  # Do this by presenting time series
library(ggplot2)
timeSeriesFunction <- function(variable, colour){
  value <- variable
  col <- colour
  ggplot(data = data.frame(time, value), aes(x = time, y = value))+
    geom_line(color = col, size = 0.5) + xlab("Date") + ylab("Size")
}
initialClaimsTS <- timeSeriesFunction(initialClaims, "red") #time series plot for Initial Claims
firstPaymentsTS <- timeSeriesFunction(firstPayments, "blue") #time series plot for First Payments
weeksClaimedTS <- timeSeriesFunction(weeksClaimed, "green") #time series plot for Weeks Claimed
weeksCompensatedTS <- timeSeriesFunction(weeksCompensated, "pink") #time series plot for Weeks Compensated
avgWklyBenefitTS <- timeSeriesFunction(avgWklyBenefit, "purple") #time series plot for Avg Wkly Benefit 
benefitsPaidTS <- timeSeriesFunction(benefitsPaid, "orange") #time series plot for Benefits Paid
finalPaymentsTS <- timeSeriesFunction(finalPayments, "black") #time series plot for Final Payments
# Type down timeseries name to get plot
initialClaimsTS

# Also present some joint plots
pairs(dataset[,c(2,3,4,5,6,7,8)],pch=1, cex=0.5,lower.panel=NULL)

#Question 2

# Perform normality test on log(Weeks compensated) using normal QQplot
  # Create vector for log(Weeks compensated)
logWeeksCompensated <- log(weeksCompensated, base=exp(1))
  # Generate QQPlot against the normal distribution
qqnorm(logWeeksCompensated, main= "log(Weeks Compensated) Q-Q Plot", col="pink")
# Perform normality test on log(Benefits paid) using normal QQplot
  # Create vector for log(Benefits paid)
logBenefitsPaid <- log(benefitsPaid, base=exp(1))
  # Generate QQPlot against the normal distribution
qqnorm(logBenefitsPaid, main="log(Benefits Paid) Q-Q Plot", col="orange")

#Question 3

# Graphing of histogram for log(Weeks compensated)
ggplot(as.data.frame(logWeeksCompensated),aes(logWeeksCompensated)) + geom_histogram(binwidth=0.1, col='black', fill='pink') +
  labs(title="log(Weeks Compensated) Histogram", x ="log(Weeks Compensated) Size", y = "Frequency")
# Graphing of ECDF for log(Weeks compensated)
logWeeksCompensatedECDF <- ggplot(as.data.frame(logWeeksCompensated),aes(logWeeksCompensated)) + stat_ecdf(geom="step") +
  labs(title="ECDF of log(Weeks Compensated)", x="log(Benefits Paid) Size", y="Cumulative Density")
logWeeksCompensatedECDF
# Graphing of histogram for log(Benefits paid)
ggplot(as.data.frame(logBenefitsPaid),aes(logBenefitsPaid)) + geom_histogram(binwidth=0.1, col='black', fill='orange') +
  labs(title="log(Weeks Compensated) Histogram",
       x ="log(Weeks Compensated) Size", y = "Frequency")
# Graphing of ECDF for log(Benefits paid)
logBenefitsPaidECDF <- ggplot(as.data.frame(logBenefitsPaid),aes(logBenefitsPaid)) + stat_ecdf(geom="step") +
  labs(title="ECDF of log(Benefits Paid)", x="log(Benefits Paid) Size", y="Cumulative Density")
logBenefitsPaidECDF

#Question 4

logWeeksClaimed <- log(weeksClaimed)
logWeeksClaimedECDF <- ggplot(as.data.frame(logWeeksClaimed),aes(logWeeksClaimed)) + stat_ecdf(geom="step") +
  labs(title="ECDF of log(Weeks Claimed)", x="log(Benefits Paid) Size", y="Cumulative Density")
logWeeksClaimedECDF
meanLogWeeksCompensated <- mean(logWeeksCompensated)
meanLogWeeksCompensated #13.38521
meanLogWeeksClaimed <- mean(logWeeksClaimed)
meanLogWeeksClaimed #13.51343 
  #Null hypothesis: The mean of logWeeksCompensated = mean of logBenefitsPaid
  #Alternative hypothesis: The mean of logWeeksCompensated != mean of logBenefitsPaid
  #Test statistic to use: the distribution of mean for logWeeksCompensated, and we will use a two tail test
  #Working out space:
length(logWeeksCompensated) #589
ztest = (meanLogWeeksClaimed-meanLogWeeksCompensated)/(sd(logWeeksClaimed)/sqrt(length(logWeeksClaimed)))
ztest
p1 = pnorm(-ztest)
p1 #p-value is very small - less than 0.05 --> reject
      
#Question 5

# Examine the plot again:
meanLogBenefitsPaid = mean(logBenefitsPaid)
meanLogBenefitsPaid #18.6068
logBenefitsPaidECDF 
logBenefitsPaidECDF + ylim(0.54,0.56) #55% quantile is 18.75
  #Null hypothesis: The mean of logBenefitsPaid > 18.75
  #Alternative hypothesis: The mean of logBenefitsPaid < 18.75
  #Test statistic to use: the distribution of mean for logBenefitsPaid, and we will use a one sided test
  #Working out space:
z2test = (meanLogBenefitsPaid-18.75)/(sd(logBenefitsPaid)/sqrt(length(logBenefitsPaid)))
z2test
p1 = pnorm(z2test)
p1 #8.351946e-09 < 0.05 --> reject

#Question 6a

  #Take a look at how large the dataset is first
head(dataset) #can see the dataset starts from the January of 1971
  #Check where to split the dataset
dataset[dataset$Date=="01/31/2011",] #can see that index is 481 --> training set is top 480 rows
  #Split the variables into a training set and testing set - weeksCompensated and weeksClaimed
datasetSLRTrain <- dataset[1:480,c(4,5)]
datasetSLRTest <- dataset[481:nrow(dataset),c(4,5)]
datasetSLRTest
  #Write code for the creation of a regression model
SLregressor = lm(formula = Weeks.Compensated ~ Weeks.Claimed, data = datasetSLRTrain)
summary(SLregressor) #computation of summary statistics

#Question 6b

  #Make predictions using the simple linear regression model created in part a
weeksCompensatedPred = predict(SLregressor, newdata=datasetSLRTest)
  #Compute the mean squared prediction error between the predicted value and the actual values
SSR = mean((datasetSLRTest$Weeks.Compensated - weeksCompensatedPred) ^ 2)
SSR #10196915024
SST = mean((datasetSLRTest$Weeks.Compensated-mean(datasetSLRTest$Weeks.Compensated))^2)
SST #31925047416
RsquaredSLR = 1-(SSR/SST)
RsquaredSLR #0.6805983 #decently good model
  #See this in perspective --> let's produce a plot for this
  #Compile x values, predictions and actual values
predDataSLR <- as.data.frame(cbind(datasetSLRTest$Weeks.Claimed,datasetSLRTest$Weeks.Compensated,weeksCompensatedPred))
predDataSLR
  #Set column names for easier reference
colnames(predDataSLR) = c("WeeksClaimed", "Actual","Predicted")
colnames(predDataSLR)
  #Generate plot
SLRActVSPred <- ggplot(data = predDataSLR, aes(x=WeeksClaimed)) +
  geom_line(aes(y = Actual), color = "darkred") + 
  geom_line(aes(y = Predicted), color="steelblue", linetype="twodash") +
  xlab("Weeks Claimed") + ylab("Weeks Compensated") + labs(title="SLR Predicted(Blue) vs. Actual(Red)")
SLRActVSPred #production of plot

#Question 7

  #In this question, decided to train with the whole dataset
datasetMLRTrain <- dataset[,2:8]
MLregressor = lm(formula = Benefits.Paid ~ ., data = datasetMLRTrain)
summary(MLregressor)
