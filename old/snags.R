

# Read and plot the data
df <- read.csv("snags.csv")
df <- subset(df,PercentInitial<=1)  # filter out crazy value
df$FractionFallen <- 1-df$PercentInitial # we want cumulative failure
library(ggplot2)
qplot(Age,FractionFallen,color=Source,data=df)

# Look at Weibull distribution to make a guess at parameters
curve(pweibull(x, scale=10, shape=4.5),from=0, to=15, main="Weibull distribution")
plot(1-exp(-(seq(0,20,.1)/10)^4.5))  # the underlying formula-same thing

# Now fit a nonlinear least squares model
m <- nls(FractionFallen~1-exp(-(Age/scale)^shape),data=df,start=list(shape=4.5,scale=10))
print( m )
df$pred <- predict(m)
qplot(Age,FractionFallen,color=Source,data=df)+geom_line(aes(y=pred,group=1),color='black',linetype=2)

# Add some fake data around 30 years
fake <- data.frame(Age=seq(30,40),FractionFallen=1,Source='fake')
library(reshape)
df1 <- rbind.fill(df,fake)
m1 <- nls(FractionFallen~1-exp(-(Age/scale)^shape),data=df1,start=list(shape=4.5,scale=10))
print( m1 )
df1$pred <- predict(m1)
qplot(Age,FractionFallen,color=Source,data=df1)+geom_line(aes(y=pred,group=1),color='black',linetype=2)
