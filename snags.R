# This script is to analyze changes in basal area of snag data
# from three different sites: Delta Junction, Alaska (K. Manies),
# Thompson, Manitoba (B. Bond-Lamberty), and a wetland chrono-
# sequence in Canada (M. Wotton).


SCRIPTNAME    	<- "snags.R"
OUTPUT_DIR		<- "outputs/"
LOG_DIR			<- "logs/"
SEPARATOR		<- "-------------------"

source("support_functions.R")   # support functions, in another file to keep tidy

# -----------------------------------------------------------------------------
# MAIN

# Preliminaries
library(ggplot2)
theme_set(theme_bw())
library(gridExtra)
library(plyr)

sink(paste(LOG_DIR, paste0(SCRIPTNAME, ".txt"), sep="/"), split=T)

printlog("Welcome to", SCRIPTNAME)


# -----------------------------------------------------------------------------
# Load data and normalize

# Read main data file
snag<-read_csv("StdgDeadOverTimev4.csv")

# Calculate BA PercentInitial
printlog("Computing basal area PercentInitial...")

# Because the sites from 'DG' have different site names, but are treated as a chronosequence,
# we assign a new 'Group' field
snag$Group <- ifelse(snag$Source=="DG", "SourceDG", snag$Site)

# We want to sort by group, ensuring that largest values come first if any ties
snag <- snag[order(snag$BA, decreasing=TRUE),]
snag <- snag[order(snag$Group, snag$Drainage, snag$Age),]

# To compare these sites, calculate 'PercentInitial' for each Group*Drainage combination
# Because of the sort above, we're guaranteed that the youngest, biggest BA is first
snag <- ddply(snag, .(Group, Drainage), transform, 
                BA.PercentInitial=BA/BA[1] * 100)

# Calculate density PercentInitial
printlog("Computing density PercentInitial...")

# We want to sort by group, ensuring that largest values come first if any ties
snag <- snag[order(snag$Density, decreasing=TRUE),]
snag <- snag[order(snag$Group, snag$Drainage, snag$Age),]

# To compare these sites, calculate 'PercentInitial' for each Group*Drainage combination
# Because of the sort above, we're guaranteed that the youngest, biggest density is first
snag <- ddply(snag, .(Group, Drainage), transform, 
                density.PercentInitial=Density/Density[1] * 100)

# -----------------------------------------------------------------------------
# QC data, looking for any problems
printlog("Doing QC...")

# Since this is such a short dataset I printed the data out. Best way for me to check the data.
print(snag)

# Making a graph is also useful!
p <- ggplot(snag, aes(Age, BA.PercentInitial, color=paste(Site,Drainage))) + geom_point() + geom_line() + ggtitle("Basal Area")
print(p)
readline("[RETURN]")

saveplot("BA.basicplot")
t <- ggplot(snag, aes(Age, density.PercentInitial, color=paste(Site,Drainage))) + geom_point() + geom_line() + ggtitle("Density")
print(t)
saveplot("density.basicplot")

if(any(snag$density.PercentInitial > 100)) {
     printlog("WARNING: removing data points with more post-fire density than initially:")
     print(subset(snag, density.PercentInitial > 100))
     snag <- subset(snag, density.PercentInitial <= 100)
 }

readline("[RETURN]")

# -----------------------------------------------------------------------------
# Fit and evaluate models for basal area

# Run a linear model on PercentInitial
printlog("First, basal area (BA) data:")
printlog("Computing linear model...")
snag.lm <- lm(BA.PercentInitial~Age, data=snag)
snag.res <- resid(snag.lm)
snag$lm <- predict(snag.lm)
print(summary(snag.lm))
printlog("BA linear AIC =", AIC(snag.lm))

par(mfrow=c(2,2)) # plot into a 2x2 grid
plot(snag.lm)
readline("[RETURN]")

# Graph base plot
scatter0 <- ggplot(snag, aes(Age, BA.PercentInitial, color=paste(Drainage))) + geom_point()
scatter0 <- scatter0 + xlab("Time since disturbance (years)") +
    ylab("Basal area (% of initial)")

# Graph linear data and residuals; note this does not fit well
printlog("Making linear plots...")
print(scatter0 + geom_line(data=snag, aes(y=lm), linetype=2, size=1, color='black') + ggtitle("Basal Area-linear"))
saveplot("basal_area-linear")
readline("[RETURN]")

# Let's try fitting a sigmoid-style model
# In this model, "Age_mid" gives the time of 50% loss
printlog("Computing sigmoidal model...")
sigmoid <- nls(BA.PercentInitial ~ 100 / (1 + exp(-slope * (Age-Age_mid))), data=snag, 
               start=list(slope=-2, Age_mid=10))
print(summary(sigmoid))
printlog("BA sigmoid AIC =", AIC(sigmoid))
snag$sigmoid <- predict(sigmoid)
print(scatter0 + geom_line(data=snag, aes(y=sigmoid), linetype=2, size=1, color='black') + ggtitle("Basal Area-sigmoid"))
saveplot("basal_area-sigmoid")
readline("[RETURN]")

# What are exponential fit parameters?
printlog("Fitting nonlinear exponential model...")
expo1 <- nls(BA.PercentInitial ~ a * exp(b * Age), data=snag, 
             start=list(a=100, b=-0.06))
print(summary(expo1))
printlog("expo AIC =", AIC(expo1))
snag$expo1 <- predict(expo1)
print(scatter0 + geom_line(data=snag, aes(y=expo1), linetype=2, size=1, color='black') + ggtitle("Basal Area-exponential"))
saveplot("basal_area-expo")
readline("[RETURN]")

# Run a linear model on the log of BA.PercentInitial
# printlog("Computing log model...")
# logsnag.lm <- lm(log(PercentInitial) ~ Age, data=snag)
# logsnag.res <- resid(logsnag.lm)
# snag$loglm <- predict(logsnag.lm)
# print(summary(logsnag.lm))
# par(mfrow=c(2,2))
# plot(logsnag.lm)
# 
# printlog("Saving log plot diagnostics")
# pdf("logsnag.lm.pdf")
# par(mfrow=c(2,2)) # plot into a 2x2 grid
# plot(logsnag.lm)
# dev.off()
# readline("[RETURN]")
#
# printlog("log-scale plot...")
# scatter2 <- ggplot(snag, aes(Age, log(PercentInitial), color=paste(Drainage))) +
#     geom_point() +
#     scale_color_discrete("Site, drainage") +
#     geom_line(data=snag, aes(y=loglm), linetype=2, size=1, color='black')
# 
# ggsave("scatter2-log.pdf")
# readline("[RETURN]")
# 

# -----------------------------------------------------------------------------
# Fit and evaluate models for density

# Run a linear model on PercentInitial
printlog("Now, working with density.")
printlog("Computing linear model...")
snagd.lm <- lm(density.PercentInitial~Age, data=snag)
snagd.res <- resid(snagd.lm)
snag$d_lm <- predict(snagd.lm)
print(summary(snagd.lm))
printlog("density AIC =", AIC(snagd.lm))

par(mfrow=c(2,2)) # plot into a 2x2 grid
plot(snagd.lm)
readline("[RETURN]")

# Graph base plot
scatter10 <- ggplot(snag, aes(Age, density.PercentInitial, color=paste(Drainage))) + geom_point()
scatter10 <- scatter10 + xlab("Time since disturbance (years)") +
    ylab("Density (% of initial)") +

# Graph linear data and residuals; does this also not fit well?
printlog("Making linear plots...")
print(scatter10 + geom_line(data=snag, aes(y=lm), linetype=2, size=1, color='black') + ggtitle("Density-linear"))
saveplot("density-linear")
readline("[RETURN]")

# Let's try fitting a sigmoid-style model
# In this model, "Age_mid" gives the time of 50% loss
printlog("Computing sigmoidal model...")
sigmoid_d <- nls(density.PercentInitial ~ 100 / (1 + exp(-slope * (Age-Age_mid))), data=snag, 
               start=list(slope=-2, Age_mid=10))
print(summary(sigmoid_d))
printlog("AIC =", AIC(sigmoid_d))
snag$sigmoid <- predict(sigmoid_d)
print(scatter10 + geom_line(data=snag, aes(y=sigmoid), linetype=2, size=1, color='black') + ggtitle("Density-linear"))
saveplot("density-sigmoid")
readline("[RETURN]")

# What are exponential fit parameters?
printlog("Fitting nonlinear exponential model...")
expo2 <- nls(density.PercentInitial ~ a * exp(b * Age), data=snag, 
             start=list(a=100, b=-0.06))
print(summary(expo2))
printlog("expo AIC =", AIC(expo2))
snag$expo2 <- predict(expo2)
print(scatter0 + geom_line(data=snag, aes(y=expo2), linetype=2, size=1, color='black') + ggtitle("Density-exponential"))
saveplot("density-expo")

# Print the version of R, packages used, etc.
printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink()
