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
snagba<-read_csv("BAoverTimev3.csv")

# Calculate PercentInitial
printlog("Computing PercentInitial...")

# Because the sites from 'DG' have different site names, but are treated as a chronosequence,
# we assign a new 'Group' field
snagba$Group <- ifelse(snagba$Source=="DG", "SourceDG", snagba$Site)

# We want to sort by group, ensuring that largest values come first if any ties
snagba <- snagba[order(snagba$BA, decreasing=TRUE),]
snagba <- snagba[order(snagba$Group, snagba$Drainage, snagba$Age),]

# To compare these sites, calculate 'PercentInitial' for each Group*Drainage combination
# Because of the sort above, we're guaranteed that the youngest, biggest BA is first
snagba <- ddply(snagba, .(Group, Drainage), transform, 
                PercentInitial=BA/BA[1] * 100)

# QC data, looking for any problems
printlog("Doing QC...")

# Since this is such a short dataset I printed the data out. Best way for me to check the data.
print(snagba)

# Making a graph is also useful!
p <- ggplot(snagba, aes(Age, PercentInitial, color=paste(Site,Drainage))) + geom_point() + geom_line()
print(p)
saveplot("basicplot")

# if(any(snagba$PercentInitial > 100)) {
#     printlog("WARNING: removing data points with more post-fire BA than initially:")
#     print(subset(snagba, PercentInitial > 100))
#     snagba <- subset(snagba, PercentInitial <= 100)
# }

readline("[RETURN]")


# -----------------------------------------------------------------------------
# Fit and evaluate models

# Run a linear model on PercentInitial
printlog("Computing linear model...")
snagba.lm <- lm(PercentInitial~Age, data=snagba)
snagba.res <- resid(snagba.lm)
snagba$lm <- predict(snagba.lm)
print(summary(snagba.lm))
printlog("AIC =", AIC(snagba.lm))

par(mfrow=c(2,2)) # plot into a 2x2 grid
plot(snagba.lm)

# printlog("Saving plot diagnostics")
# pdf("snagba.lm.pdf")
# par(mfrow=c(2,2)) # plot into a 2x2 grid
# plot(snagba.lm)
# dev.off()
readline("[RETURN]")

# Graph base plot
scatter0 <- ggplot(snagba, aes(Age, PercentInitial, color=paste(Drainage))) + geom_point()
scatter0 <- scatter0 + xlab("Time since disturbance (years)") +
    ylab("Basal area (% of initial)")

# Graph linear data and residuals; note this does not fit well
printlog("Making linear plots...")
print(scatter0 + geom_line(data=snagba, aes(y=lm), linetype=2, size=1, color='black'))
saveplot("scatter1-linear")
readline("[RETURN]")


# Let's try fitting a sigmoid-style model
# In this model, "Age_mid" gives the time of 50% loss
printlog("Computing sigmoidal model...")
sigmoid <- nls(PercentInitial ~ 100 / (1 + exp(-slope * (Age-Age_mid))), data=snagba, 
               start=list(slope=-2, Age_mid=10))
print(summary(sigmoid))
printlog("AIC =", AIC(sigmoid))
snagba$sigmoid <- predict(sigmoid)
print(scatter0 + geom_line(data=snagba, aes(y=sigmoid), linetype=2, size=1, color='black'))
saveplot("scatter2-sigmoid")
readline("[RETURN]")


# Run a linear model on the log of PercentInitial

# Putting this code back in because the log transform you had in scatter3 didn't work
# (My scatter 2 & 3 looked the same). Also now that data are corrected linear
# and, likely, the log fit are decent fits.

# printlog("Computing log model...")
# logsnagba.lm <- lm(log(PercentInitial) ~ Age, data=snagba)
# logsnagba.res <- resid(logsnagba.lm)
# snagba$loglm <- predict(logsnagba.lm)
# print(summary(logsnagba.lm))
# par(mfrow=c(2,2))
# plot(logsnagba.lm)
# 
# printlog("Saving log plot diagnostics")
# pdf("logsnagba.lm.pdf")
# par(mfrow=c(2,2)) # plot into a 2x2 grid
# plot(logsnagba.lm)
# dev.off()
# readline("[RETURN]")

# Took out smoothing graph now that data have been corrected.
# Also redid how log graph is done as your code wasn't working for me
# (both linear and log plots looked the same)

# printlog("log-scale plot...")
# scatter2 <- ggplot(snagba, aes(Age, log(PercentInitial), color=paste(Drainage))) +
#     geom_point() +
#     scale_color_discrete("Site, drainage") +
#     geom_line(data=snagba, aes(y=loglm), linetype=2, size=1, color='black')
# 
# ggsave("scatter2-log.pdf")
# readline("[RETURN]")
# 
# printlog("Everything on one page...")
# grid.arrange(scatter1 + guides(color=F), 
#              scatter2 + guides(color=F), 
#              ncol=1)
# readline("[RETURN]")

# What are exponential fit parameters?
printlog("Fitting nonlinear exponential model...")
expo1 <- nls(PercentInitial ~ a * exp(b * Age), data=snagba, 
             start=list(a=100, b=-0.06))
print(summary(expo1))
printlog("AIC =", AIC(expo1))
snagba$expo1 <- predict(expo1)
print(scatter0 + geom_line(data=snagba, aes(y=expo1), linetype=2, size=1, color='black'))
saveplot("scatter3-expo")
readline("[RETURN]")


# Print the version of R, packages used, etc.
printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink()
