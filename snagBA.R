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
snagba<-read.csv(fn, header=TRUE)

# Calculate PercentInitial
printlog("Computing PercentInitial...")
snagba <- snagba[order(snagba$Site, snagba$Drainage, snagba$Age),]
snagba <- ddply(snagba, .(Site, Drainage), transform, 
                PercentInitial=BA/BA[1] * 100)

# QC data, looking for any problems
printlog("Doing QC...")
if(any(snagba$PercentInitial > 100)) {
    printlog("WARNING: removing data points with more post-fire BA than initially:")
    print(subset(snagba, PercentInitial > 100))
    snagba <- subset(snagba, PercentInitial <= 100)
}
if(any(snagba$Age > 20 & snagba$PercentInitial == 100)) {
    printlog("WARNING: removing data points > 20 years and no loss:")
    print(subset(snagba, snagba$Age > 20 & snagba$PercentInitial == 100))
    snagba <- subset(snagba, snagba$Age <= 20 | snagba$PercentInitial < 100)
}
readline("[RETURN]")

#snagba$logPercentInitial <- log(snagba$PercentInitial)

# Run a linear model on PercentInitial
printlog("Computing (a clearly inappropriate) linear model...")
snagba.lm <- lm(PercentInitial~Age, data=snagba)
snagba.res <- resid(snagba.lm)
print(summary(snagba.lm))
par(mfrow=c(2,2)) # plot into a 2x2 grid
plot(snagba.lm)

printlog("Saving plot diagnostics")
pdf("snagba.lm.pdf")
par(mfrow=c(2,2)) # plot into a 2x2 grid
plot(snagba.lm)
dev.off()
readline("[RETURN]")

# Run a linear model on the log of PercentInitial
#logsnagba.lm <- lm(log(PercentInitial) ~ Age, data=snagba)
#logsnagba.res <- resid(logsnagba.lm)
#summary(logsnagba.lm)

# Graph data and residuals
printlog("Making plots...")
scatter0 <- ggplot(snagba, aes(Age, PercentInitial, color=paste(Site, Drainage))) +
    geom_point() +
    xlab("Time since disturbance (years)") +
    ylab("Initial basal area (%)")

printlog("Points with a smoother applied...")
scatter1 <- scatter0 +
    geom_smooth(group=1, method='loess', color='black')
print(scatter1)
ggsave("scatter1.pdf")
readline("[RETURN]")

printlog("Linear-scale plot...")
scatter2 <- scatter0 +
    geom_point() +
    geom_line() +
    scale_color_discrete("Site, drainage")
print(scatter2)
ggsave("scatter2.pdf")
readline("[RETURN]")

printlog("Log-scale plot...")
scatter3 <- scatter2 + 
    coord_trans(y="log2")
print(scatter3)
ggsave("scatter3.pdf")
readline("[RETURN]")

printlog("Everything on one page...")
grid.arrange(scatter1 + guides(color=F), 
             scatter2 + guides(color=F), 
             scatter3, 
             ncol=1)
readline("[RETURN]")

# What are exponential fit parameters?
printlog("Fitting nonlinear exponential model...")
expo1 <- nls(PercentInitial ~ a * exp(b * Age), data=snagba, 
             start=list(a=1, b=-0.06))
print(summary(expo1))
snagba$expo1 <- predict(expo1)
scatter4 <- scatter0 + 
    geom_line(data=snagba, aes(y=expo1), linetype=2, size=1, color='black')
print(scatter4)
ggsave("scatter4-expo1.pdf")
readline("[RETURN]")

# Print the version of R, packages used, etc.
print( sessionInfo() )
printlog("All done.")
sink()
