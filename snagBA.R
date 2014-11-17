#This script is to analyze changes in basal area of snag data
#from three different sites: Delta Junction, Alaska (K. Manies),
#Thompson, Manitoba (B. Bond-Lamberty), and a wetland chrono-
#sequence in Canada (M. Wotton).

#Have installed the following libraries:
#ggplot2, gridExtra

snagba<-read.csv("BAoverTimev3.csv", header=TRUE)

#Put output into a file so can refer to it later
sink(file="snagBA.txt")

#Remove weird data point: more post-fire than initially
snagba<-subset(snagba,PercentInitial<=1)

#Why do I have 100% at year 1? Shouldn't these ages by 0 (since
#it is our starting point? Resetting these to zero.
snagba$Age[snagba$Age==1] <- 0

snagba$logPercentInitial<-log(snagba$PercentInitial)

#Run a linear model on PercentInitial
snagba.lm<-lm(data=snagba, PercentInitial~Age)
snagba.res = resid(snagba.lm)
summary(snagba.lm)

#Run a linear model on the log of PercentInitial
logsnagba.lm<-lm(data=snagba, logPercentInitial~Age)
logsnagba.res = resid(logsnagba.lm)
summary(logsnagba.lm)

#Graph data and residuals
library(ggplot2)
scatter1 <- qplot(Age,PercentInitial, data=snagba, color=Source)
scatter2 <- qplot(Age,logPercentInitial, data=snagba, color=Drainage)
residplot1 <- qplot(snagba$Age, snagba.res)
residplot2 <- qplot(snagba$Age, logsnagba.res)

library(gridExtra)
grid.arrange(scatter1, scatter2, residplot1, residplot2, ncol=2)

#What are exponential fit parameters?
expo1 <- nls(PercentInitial ~ a * exp(b * Age), start=list(a=1, b=-0.06))
summary(expo1)

