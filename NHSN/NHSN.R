################################################################################
#                                 set up                                       #
################################################################################
#This is my R package, ham
library(ham)

#HAI Types

#Definitions
# Central line-associated bloodstream infections (CLABSI)
# Catheter-associated urinary tract infections (CAUTI)
# Ventilator-associated events (VAE)
# Surgical site infections (SSI) following colon surgery in adults, ≥ 18years


################################################################################
#                                 Get data                                     #
################################################################################

load("C:/temp/ham/clabsi.rdata")
load("C:/temp/ham/cauti.rdata")
load("C:/temp/ham/vae.rdata")
load("C:/temp/ham/ssi.rdata")


################################################################################
#                                 Analyses                                     #
################################################################################

###########
# Study 1 #
###########

## CLABSI

# Was there an impact of covid-19 on CLABSI? Was there an improvement in 2022?
# US Statehood as an intervention for the CLABSI outcome
# 50 states vs. 4 non-states (D.C., Guam, Puerto Rico, Virgin Islands)

# We'll do both, Differences-in-Differences and Interrupted Time Series

# The model code
dm1cl <- assess(formula= SIR ~ ., data=clabsi_df, intervention = "US.State",
              int.time="year", treatment= 5, did="two",  #DID specific code
              its="two", interrupt= c(5, 7),             #ITS specific code
              newdata=TRUE )                             #stores new variables

# DID #
summary(dm1cl$DID)                                       #'DID' is the main variable
interpret(dm1cl)$did                                     #get interpretations

# variable importance
x11(width=10, height=8)
par(mar=c(4.2, 2, 3.5, 3))
par(oma = c(0, 0, 0, 3))
plot(importance(dm1cl$DID), cex=1.5)

#start with the basic plot and zoom from R studio
plot(dm1cl, "DID")

# graph with more detail
x11(width=12, height=8)
par(mar=c(4, 5, 3.5, 3))
plot(dm1cl, "DID", conf.int=TRUE, add.legend="topleft", lwd=5,
     cfact=TRUE, cex.legend=2, legend=c("US States", "Non-States"),
     main="DID: CLABSI SIRs by statehood", ylim=c(.7, 1.4), xlim=c(-.15, 1.15),
     cex.axis=2, cex.lab=2, xlab="Pre/Post", cex.main=3,
     coefs=TRUE, cex.text=3, adj.alpha=.2, pos.text=list("DID"=3))

# ITS #
summary(dm1cl$ITS)
interpret(dm1cl)$its
# “Summary 1" and “Summary 2". each value represents the change per unit of time
# in that period. For example, in “Summary 2" there is a significant difference
# between both groups of -0.328 for each year

# graph importance
x11(width=10, height=9)
par(mar=c(4.2, 2, 3.5, 3))
par(oma = c(0, 0, 0, 3))
plot(importance(dm1cl$ITS), cex=1.5, color="green")     #Change to green

# graph model results
x11(width=16, height=10)
par(mar=c(4, 5, 3.5, 3))
plot(dm1cl, "ITS", conf.int=TRUE, add.means=TRUE, x.axis=2016:2024,
     lwd=5, cfact=TRUE, cex.legend=1.75, add.legend="topleft", ylim=c(0.5,1.75),
     main="ITS: US CLABSI SIRs by year", cex.axis=2, cex.lab=2, cex=4,
     xlab="Year", cex.main=3, coefs=TRUE, cex.text=1.5, adj.alpha=.2,
     legend= c("US.States","US.Non-states"),
     pos.text=list("txp5"=3, "txp7"=1, "ixp7"=3, "Intercept"=4))

###############
# Conclusions #
###############
# 1. The trends flipped in 2020 and 2021 and then returned in 2022.
# 2. DID & ITS explain the covid effect in 2020, only ITS explains the 2022 effects.
# 3. Both approaches can help answer different questions. ITS is more helpful with
#    more complex questions over time but DID provides an easier interpretation in
#    results and graphing.
# 4. ITS lets us see the most dramatic changes in 2022, compared to main covid years.
# 5. A simple pre-and-post-test would suggest there was no difference before and
#    after covid for the US states. But by adding a comparison group to help us
#    understand "what might have been", we see that maintaining a steady level
#    in the face of great change, is success.
# 6. We can use the same analyses to identify false success, a pre-and-post test
#    may wrongly show program success because the control group is also experiencing
#    a rapid SIR decrease if coming out of a high rate and variation during 2020-2021.
#    E.g., imagine a CAUTI intervention starting in 2022, high 2020 rates due to
#    covid could falsely imply program success if the rates dramatically decreased.

###########
# Study 2 #
###########
# Did covid-19 impact CAUTIs, CLABSIs, SSIs, & VAEs in the 50 United States?
# Was there an improvement in 2022?
# No control group, all 50 states experienced covid.
# We can answer if things have gotten back to normal and make a better comparison
# with 2022, using a more relevant 2020 instead of 2016.
# We can see if all infections changed the same way in 2020 and 2021.

## One group ITS
# Because covid-19 had an impact on all states, we don't have a control group
# when examining just the data of the 50 US states (only CLABSI had significant
# US state vs non-US State differences).
# The analysis option we have with no control group is Interrupted Time Series.

############
## CLABSI ##
############

im12cl <- assess(formula=SIR ~ ., data=clabsi, intervention = "US.State",
               int.time="year", interrupt= c(5, 7), its="one")
summary(im12cl$ITS)

x11(width=16, height=10)
par(mar=c(4, 5, 3.5, 3))
plot(im12cl, "ITS", conf.int=TRUE, add.means=TRUE, x.axis=2016:2024,
     lwd=5, cfact=TRUE, cex.legend=2.25, add.legend="bottomleft", ylim=c(0.5,.95),
     main="ITS: US CLABSI SIRs by year", cex.axis=2, cex.lab=2, cex=4,
     xlab="Year", cex.main=3, coefs=TRUE, cex.text=1.5, adj.alpha=.2, legend="SIR Trend",
     col="seagreen2", pos.text=list("txp5"=3, "txp7"=1, "Intercept"=4))


###########
## CAUTI ##
###########

#One group ITS
im12ca <- assess(formula=SIR ~ ., data=cauti, intervention = "US.State",
               int.time="year", interrupt= c(5, 7), its="one")
summary(im12ca$ITS)

x11(width=16, height=10)
par(mar=c(4, 5, 3.5, 3))
plot(im12ca, "ITS", conf.int=TRUE, add.means=TRUE, x.axis=2016:2024,
     lwd=5, cfact=TRUE, cex.legend=2.25, add.legend="bottomleft", ylim=c(0.55, 1),
     main="ITS: US CAUTI SIRs by year", cex.axis=2, cex.lab=2, cex=4,
     xlab="Year", cex.main=3, coefs=TRUE, cex.text=1.5, adj.alpha=.2,legend="SIR Trend",
     col="salmon", pos.text=list("txp5"=3, "txp7"=1, "Intercept"=4, "post7"=3))

#########
## VAE ##
#########

#One group ITS
im12v <- assess(formula=SIR ~ ., data=vae, intervention = "US.State",
               int.time="year", interrupt= c(5, 7), its="one")
summary(im12v$ITS)

x11(width=16, height=10)
par(mar=c(4, 5, 3.5, 3))
plot(im12v, "ITS", conf.int=TRUE, add.means=TRUE, x.axis=2016:2024,
     lwd=5, cfact=TRUE, cex.legend=2.25, add.legend="topleft", ylim=c(0.9,1.4),
     main="ITS: US VAE SIRs by year", cex.axis=2, cex.lab=2, cex=4,tcol= "orange",
     xlab="Year", cex.main=3, coefs=TRUE, cex.text=1.5, adj.alpha=.2,legend="SIR Trend",
     col="slategray", pos.text=list("txp5"=3, "txp7"=1, "Intercept"=4))

#########
## SSI ##
#########

#One group ITS
im12s <- assess(formula=SIR ~ ., data=ssi, intervention = "US.State",
               int.time="year", interrupt= c(5, 7), its="one")
summary(im12s$ITS)

x11(width=16, height=10)
par(mar=c(4, 5, 3.5, 3))
plot(im12s, "ITS", conf.int=TRUE, add.means=TRUE, x.axis=2016:2024,
     lwd=5, cfact=T, cex.legend=2.25, add.legend="bottomleft", ylim=c(0.8,1.05),
     main="ITS: US SSI SIRs by year", cex.axis=2, cex.lab=2, cex=4,legend="SIR Trend",
     xlab="Year", cex.main=3, coefs=TRUE, cex.text=1.5, adj.alpha=.2,
     col="dodgerblue", pos.text=list("txp5"=3, "txp7"=1, "Intercept"=4))

###############
# Conclusions #
###############
# 1. We see things looked differently in SIRs during 2020 and 2021.
# 2. For some infections, SIRs seem to be back on track while others need more time.
# 3. We see which things are more susceptible to change and maybe some insight to
#    the potential impact of seasonality in general terms.

################################################################################
#                                   Graphs                                     #
################################################################################

#All 4 ITS
x11(width=16, height=10)
par(mar=c(4, 5, 3.5, 3))
par(mfrow=c(2,2))
plot(im12cl, "ITS", conf.int=TRUE, add.means=TRUE, x.axis=2016:2024,
     lwd=5, cfact=TRUE, cex.legend=1.75, add.legend="bottomleft", ylim=c(0.5,.95),
     main="ITS: US CLABSI SIRs by year", cex.axis=2, cex.lab=2, cex=4,
     xlab="Year", cex.main=3, coefs=TRUE, cex.text=1.5, adj.alpha=.2, legend="SIR Trend",
     col="seagreen2", pos.text=list("txp5"=3, "txp7"=1, "Intercept"=4))
plot(im12ca, "ITS", conf.int=TRUE, add.means=TRUE, x.axis=2016:2024,
     lwd=5, cfact=TRUE, cex.legend=1.75, add.legend="bottomleft", ylim=c(0.55, 1),
     main="ITS: US CAUTI SIRs by year", cex.axis=2, cex.lab=2, cex=4,
     xlab="Year", cex.main=3, coefs=TRUE, cex.text=1.5, adj.alpha=.2,legend="SIR Trend",
     col="salmon", pos.text=list("txp5"=3, "txp7"=1, "Intercept"=4, "post7"=3))
plot(im12v, "ITS", conf.int=TRUE, add.means=TRUE, x.axis=2016:2024,
     lwd=5, cfact=TRUE, cex.legend=1.75, add.legend="topleft", ylim=c(0.9,1.4),
     main="ITS: US VAE SIRs by year", cex.axis=2, cex.lab=2, cex=4,tcol= "orange",
     xlab="Year", cex.main=3, coefs=TRUE, cex.text=1.5, adj.alpha=.2,legend="SIR Trend",
     col="slategray", pos.text=list("txp5"=3, "txp7"=1, "Intercept"=4))
plot(im12s, "ITS", conf.int=TRUE, add.means=TRUE, x.axis=2016:2024,
     lwd=5, cfact=T, cex.legend=1.75, add.legend="bottomleft", ylim=c(0.8,1.05),
     main="ITS: US SSI SIRs by year", cex.axis=2, cex.lab=2, cex=4,legend="SIR Trend",
     xlab="Year", cex.main=3, coefs=TRUE, cex.text=1.5, adj.alpha=.2,
     col="dodgerblue", pos.text=list("txp5"=3, "txp7"=1, "Intercept"=4))
par(mfrow=c(1,1))


################################################################################
#                                 Save data                                    #
################################################################################

save(clabsi, clabsi_df, dm1cl, im12cl, im22cl,
     file="C:/temp/ham/clabsi.rdata")
save(cauti, cauti_df,dm1ca, im12ca, im22ca,
     file="C:/temp/ham/cauti.rdata")
save(vae, vae_df,dm1v, im12v, im22v,
     file="C:/temp/ham/vae.rdata")
save(ssi, ssi_df,dm1s, im12s, im22s,
     file="C:/temp/ham/ssi.rdata")


