#NYCLU Schools Analysis 2007-2017

#WD
setwd("C:/Users/ethan/Desktop/Swarthmore/Spring 2019/Economics Research Seminar/Data Analysis/NYCLU Analysis")

#Install packages required for analysis
#install.packages("lattice") ; install.packages("lmtest") ; install.packages("readxl")
#install.packages("sandwich"); install.packages("car") ; install.packages("rgl")

library(readxl)

#Import Dataset
NYCLUSchools <- read_excel("Final Test Data 2009 2017.xlsx")

#Intervention Variable (Before Treatment, After Treatment)
NYCLUSchools$BTAT <- ifelse(NYCLUSchools$Year<=2015,0,1)

#Variable recording the time elapased since beginning of study
StartYear <- 2007
NYCLUSchools$Timeelapsed <- NYCLUSchools$Year - StartYear

#Variable documentation
#Year - Year in which exam was taken
#Exam - Regents Exam Type - Global History/Geography, Living Environment (Biology)
#Percent Passing - Percentage of students passing Regents Exam at each school for each year
#BEDS - Unique School ID
#FSM - Percentage of students who are eligible for free lunch at each school for each year
#Total tested - Number of students who took the exam at each school for each year
#Demographic - Population of students - All students less special education students
#Timeelapased - time elapsed since beginning of study at each school for each year
#BTAT - Indicator variable for before treatment, after treatment

#Exploratory Data Analysis

#Histogram - Exam passing rate by school 2007-2017
hist(NYCLUSchools$`Percent Passing`,
     main = "Exam Passing Rate Distribution by School",
     xlab="Exam Passing Rate by School",
     ylab="Frequency")

#NPP - Exam passing rate by school 2007-2017
qqnorm(NYCLUSchools$`Percent Passing`)
qqline(NYCLUSchools$`Percent Passing`)

#Scatterplot - Year and Percent Passing
plot(NYCLUSchools$Year, NYCLUSchools$`Percent Passing`,
     ylim = c(0,100))

#Scatterplot - Year and Percent Passing (Linearity check)
plot(NYCLUSchools$Year, NYCLUSchools$`Percent Passing`,
     ylim = c(-100,200))

#Scatterplot - FSM and Percent Passing
plot(NYCLUSchools$FSM,NYCLUSchools$`Percent Passing`)

#Boxplot Percent Passing ~ School
par(mfrow = c(1,1))
boxplot(NYCLUSchools$`Percent Passing` ~ NYCLUSchools$School)

#Panel (School and exam) effects graphic 
library(lattice)
xyplot(NYCLUSchools$`Percent Passing` ~ NYCLUSchools$Year | Exam, data = NYCLUSchools, groups = NYCLUSchools$BEDS,
       type = "o", panel = panel.superpose)

#Exploratory Modeling

#T-test with groups 2007-2015, 2016-2017
t.test(NYCLUSchools$`Percent Passing` ~ NYCLUSchools$BTAT)
boxplot(NYCLUSchools$`Percent Passing` ~ NYCLUSchools$BTAT)

#Anova School ~ Percent Passing
fit13 <- lm(NYCLUSchools$`Percent Passing` ~ NYCLUSchools$School)
anova(fit13)

#Simple linear regression model
plot(NYCLUSchools$Year, NYCLUSchools$`Percent Passing`)
fit1 <- lm(NYCLUSchools$`Percent Passing` ~ NYCLUSchools$Year)
abline(fit1)
summary(fit1)

#Loess model
fit2 <- loess(NYCLUSchools$`Percent Passing` ~ NYCLUSchools$Year, span=.5)
plot(NYCLUSchools$Year, NYCLUSchools$`Percent Passing`,
     main = "Local Polynomial Regression",
     xlab="Time",
     ylab="Exam Passing Rate by School")
lines(NYCLUSchools$Year, predict(fit2,newdata = NYCLUSchools$Year), col="red")

#Interrupted Time Series Analysis

#ITSA model
fit3 <- lm(formula = NYCLUSchools$`Percent Passing` ~ NYCLUSchools$Timeelapsed + NYCLUSchools$BTAT + NYCLUSchools$Timeelapsed*NYCLUSchools$BTAT)
summary(fit3)

#ITSA model Visualization

plot(NYCLUSchools$Year, NYCLUSchools$`Percent Passing`,
     main="Interrupted Time Series",
     xlab="Time",
     ylab="Exam Passing Rate by School")

clip(0,2018,-20,110)
abline(v=2015.67)

clip(0,2015.67,0,100)
abline(-2770.9453, 1.4056,col="blue", lwd=2)

clip(2015.67,2018,0,100)
abline(-2770.9453 + 8650.9847, 1.4056 - 4.2943, col="red", lwd=2)

clip(2015.67,2018,0,100)
abline(-2770.9453, 1.4056, col="blue", lwd=2, lty=2)

#Newey-West HAC robust standard errors (fit 3)
library(sandwich)
library(lmtest)
HACfit3 <- vcovHAC(fit3)
coeftest(fit3, HACfit3 = vcovHAC(fit3))

#AVP (Added-Variable Plot)
library(car)
avPlots(fit3)

#Check Assumptions

#Visual Assessment - Linearity & Homoscedasticity
par(mfrow = c(2,2))
plot(fit3)

#Conditional Normality
par(mfrow = c(1,1))
qqnorm(resid(fit3))
qqline(resid(fit3))

#Residual Analysis
plot(fitted(fit3), resid(fit3))

#Breusch-Pagan Test
lmtest::bptest(fit3)

#3d visualization

#load/install packages
install.packages(c("rgl", "car"))
library("car")
library("rgl")

#3D Scatterplot - Year, Percent Passing, and Total tested
scatter3d(NYCLUSchools$Year, NYCLUSchools$`Percent Passing`, NYCLUSchools$`Total tested`)
