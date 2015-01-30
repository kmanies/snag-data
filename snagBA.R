# This script is to analyze changes in basal area of snag data
# from three different sites: Delta Junction, Alaska (K. Manies),
# Thompson, Manitoba (B. Bond-Lamberty), and a wetland chrono-
# sequence in Canada (M. Wotton).

# Have installed the following libraries:
# ggplot2, gridExtra

# -----------------------------------------------------------------------------
# Time-stamped output function
printlog <- function( msg="", ..., ts=TRUE, cr=TRUE ) {
    if( ts ) cat( date(), " " )
    cat( msg, ... )
    if( cr ) cat( "\n")
} # printlog

# -----------------------------------------------------------------------------
# MAIN

library(ggplot2)
theme_set(theme_bw())
library(gridExtra)
library(plyr)

# Put output into a file so can refer to it later
sink(file="snagBA.txt", split=T)

# Read main data file
fn <- "BAoverTimev3.csv"
printlog("Reading", fn, "...")
snagba<-read.csv(fn, header=TRUE, stringsAsFactors=FALSE)

# Calculate PercentInitial
printlog("Computing PercentInitial...")

# Because the sites from 'DG' have different site names, but are treated as a chronosequence,
# we assign a new 'Group' field
snagba$Group <- ifelse(snagba$Source=="DG", "SourceDG", snagba$Site)
snagba <- snagba[order(snagba$Site, snagba$Drainage, snagba$Age),]

# By using the highest BA for this calculation you end up using the 4 yr old site for poorly drained
# sites. Is this the best way? Is it better to do it on the youngest sites, which is what I did
# in Excel? But then what do we do for poor sites which has two t=0 sites. In Excel I averaged them.
# This results in one being >100%, another <100%. We should talk about best way to deal with this
snagba <- ddply(snagba, .(Group, Drainage), transform, 
                PercentInitial=BA/BA[1] * 100)

# QC data, looking for any problems
printlog("Doing QC...")

# Since this is such a short dataset I printed the data out. Best way for me to check the data.
print(snagba)

if(any(snagba$PercentInitial > 100)) {
    printlog("WARNING: removing data points with more post-fire BA than initially:")
    print(subset(snagba, PercentInitial > 100))
    snagba <- subset(snagba, PercentInitial <= 100)
}

# Deleted code here since was due to wrong PercentInitial calculation for poorly drained sites.

readline("[RETURN]")

# Run a linear model on PercentInitial
printlog("Computing linear model...")
snagba.lm <- lm(PercentInitial~Age, data=snagba)
snagba.res <- resid(snagba.lm)
snagba$lm <- predict(snagba.lm)
print(summary(snagba.lm))
par(mfrow=c(2,2)) # plot into a 2x2 grid
plot(snagba.lm)

printlog("Saving plot diagnostics")
pdf("snagba.lm.pdf")
par(mfrow=c(2,2)) # plot into a 2x2 grid
plot(snagba.lm)
dev.off()
readline("[RETURN]")

# Graph base plot
scatter0 <- ggplot(snagba, aes(Age, PercentInitial, color=paste(Drainage)))

# Graph linear data and residuals
printlog("Making linear plots...")
scatter1 <- scatter0 +
    geom_point() +
    xlab("Time since disturbance (years)") +
    ylab("Initial basal area (%)") +
    geom_line(data=snagba, aes(y=lm), linetype=2, size=1, color='black')

ggsave("scatter1-linear.pdf")
readline("[RETURN]")

# Run a linear model on the log of PercentInitial

# Putting this code back in because the log transform you had in scatter3 didn't work
# (My scatter 2 & 3 looked the same). Also now that data are corrected linear
# and, likely, the log fit are decent fits.

printlog("Computing log model...")
logsnagba.lm <- lm(log(PercentInitial) ~ Age, data=snagba)
logsnagba.res <- resid(logsnagba.lm)
snagba$loglm <- predict(logsnagba.lm)
print(summary(logsnagba.lm))
par(mfrow=c(2,2))
plot(logsnagba.lm)

printlog("Saving log plot diagnostics")
pdf("logsnagba.lm.pdf")
par(mfrow=c(2,2)) # plot into a 2x2 grid
plot(logsnagba.lm)
dev.off()
readline("[RETURN]")

# Took out smoothing graph now that data have been corrected.
# Also redid how log graph is done as your code wasn't working for me
# (both linear and log plots looked the same)

printlog("log-scale plot...")
scatter2 <- ggplot(snagba, aes(Age, log(PercentInitial), color=paste(Drainage))) +
    geom_point() +
    scale_color_discrete("Site, drainage") +
    geom_line(data=snagba, aes(y=loglm), linetype=2, size=1, color='black')

ggsave("scatter2-log.pdf")
readline("[RETURN]")

printlog("Everything on one page...")
grid.arrange(scatter1 + guides(color=F), 
             scatter2 + guides(color=F), 
             ncol=1)
readline("[RETURN]")

# What are exponential fit parameters?
printlog("Fitting nonlinear exponential model...")
expo1 <- nls(PercentInitial ~ a * exp(b * Age), data=snagba, 
             start=list(a=100, b=-0.06))
print(summary(expo1))
snagba$expo1 <- predict(expo1)
scatter3 <- scatter0 +
  geom_point() +
  geom_line(data=snagba, aes(y=expo1), linetype=2, size=1, color='black')
print(scatter3)
ggsave("scatter3-expo.pdf")
readline("[RETURN]")

# Print the version of R, packages used, etc.
print( sessionInfo() )
printlog("All done.")
sink()
