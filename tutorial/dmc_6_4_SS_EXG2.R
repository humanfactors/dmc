##################  DMC Lesson 6: More Models

### Lesson 6.4: ExGaussian stop-signal model with 1 stop & 1 go accumulator (BEESTS)

# An EXG stop-signal example with TRIGGER and GO FAILURE and CONTEXT 
# INDEPENDENT parametrization (i.e., separate stop accumulator parameters).
# 2 accumulator race suitable for high accuracy data like BEESTS. 

rm(list=ls())
# Current working directory must be set to the top-level folder  
# containing the dmc and tutorial subfolders 
source ("dmc/dmc.R")
load_model ("EXG-SS","exgSS.R")

# load_data("dmc_SS_EXG2.Rdata")

# We will look at the most realistic scenario:
is.tf <- TRUE
is.gf <- TRUE
use.staircase <- TRUE

if (!is.tf & !is.gf) {
  
  model <- model.dmc(
    # SS stands for trial type (GO or Stop-signal [SS]):
    factors=list(S=c("s1","s2"),SS=c("GO","SS")), 
    # NR stands for "No response", i.e., go omission & successful inhibitions:
    responses=c("NR","r1","r2"), 
    # Match scores correct responses for each GO stimulus as usual, and scores 
    # the "correct" stimulus corresponding to an NR response, but the latter has
    # no effect (it just avoids a standard check making sure that each response is scored): 
    match.map=list(M=list(s1="r1",s2="r2",s1="NR")), 
    p.map=list(mu="M",sigma="M",tau="M",tf="1",muS="1",sigmaS="1",tauS="1",gf="1"),
    # No errors, no trigger failures, and no go failures: 
    constants=c(mu.false=1e6,sigma.false=.001,tau.false=.001,tf=0,gf=0),
    type="exgss")
  
  # This gives mean GoRT of .5 + .08 = .58 and SD Go RT of sqrt(.05^2+.08^2) = 0.09 
  # and mean SSRT of .2+.05 = 0.25 and SD SSRT of sqrt(.03^2+.05^2) = 0.06:
  p.vector  <- c(mu.true=.5,muS=.2,sigma.true=.05,sigmaS=.03,tau.true=.08,tauS=.05) 
}

if (is.tf & !is.gf) {
  
  model <- model.dmc(
    # SS stands for trial type (GO or Stop-signal [SS]):
    factors=list(S=c("s1","s2"),SS=c("GO","SS")),
    # NR stands for "No response", i.e., go omission & successful inhibitions:
    responses=c("NR","r1","r2"),
    # Match scores correct responses for each GO stimulus as usual, and scores 
    # the "correct" stimulus corresponding to an NR response, but the latter has
    # no effect (it just avoids a standard check making sure that each response is scored): 
    match.map=list(M=list(s1="r1",s2="r2",s1="NR")),
    p.map=list(mu="M",sigma="M",tau="M",tf="1",muS="1",sigmaS="1",tauS="1",gf="1"),
    # No errors and no go failures:
    constants=c(mu.false=1e6,sigma.false=.001,tau.false=.001,gf=0),
    type="exgss")
  
  # This gives mean GoRT of .5 + .08 = .58 and SD Go RT of sqrt(.05^2+.08^2) = 0.09 
  # and mean SSRT of .2+.05 = 0.25 and SD SSRT of sqrt(.03^2+.05^2) = 0.06:
  p.vector  <- c(mu.true=.5,muS=.2,sigma.true=.05,sigmaS=.03,tau.true=.08,tauS=.05,tf=.1) 
}

if (!is.tf & is.gf) {
  
  model <- model.dmc(
    # SS stands for trial type (GO or Stop-signal [SS]):
    factors=list(S=c("s1","s2"),SS=c("GO","SS")),
    # NR stands for "No response", i.e., go omission & successful inhibitions:
    responses=c("NR","r1","r2"),
    # Match scores correct responses for each GO stimulus as usual, and also scores 
    # the "correct" stimulus corresponding to an NR response, but the latter has
    # no effect (it just avoids a standard check making sure that each response is scored): 
    match.map=list(M=list(s1="r1",s2="r2",s1="NR")),
    p.map=list(mu="M",sigma="M",tau="M",tf="1",muS="1",sigmaS="1",tauS="1",gf="1"),
    # No errors and no trigger failures:
    constants=c(mu.false=1e6,sigma.false=.001,tau.false=.001,tf=0),
    type="exgss")
  
  # This gives mean GoRT of .5 + .08 = .58 and SD Go RT of sqrt(.05^2+.08^2) = 0.09 
  # and mean SSRT of .2+.05 = 0.25 and SD SSRT of sqrt(.03^2+.05^2) = 0.06:
  p.vector  <- c(mu.true=.5,muS=.2,sigma.true=.05,sigmaS=.03,tau.true=.08,tauS=.05,gf=.1) 
}

if (is.tf & is.gf) {
  
  model <- model.dmc(
    # SS stands for trial type (GO or Stop-signal [SS]):
    factors=list(S=c("s1","s2"),SS=c("GO","SS")),
    # NR stands for "No response", i.e., go omission & successful inhibitions:
    responses=c("NR","r1","r2"),
    # Match scores correct responses for each GO stimulus as usual, and scores 
    # the "correct" stimulus corresponding to an NR response, but the latter has
    # no effect (it just avoids a standard check making sure that each response is scored): 
    match.map=list(M=list(s1="r1",s2="r2",s1="NR")),
    p.map=list(mu="M",sigma="M",tau="M",tf="1",muS="1",sigmaS="1",tauS="1",gf="1"),
    # No errors:
    constants=c(mu.false=1e6,sigma.false=.001,tau.false=.001),
    type="exgss")
  
  # This gives mean GoRT of .5 + .08 = .58 and SD Go RT of sqrt(.05^2+.08^2) = 0.09 
  # and mean SSRT of .2+.05 = 0.25 and SD SSRT of sqrt(.03^2+.05^2) = 0.06:
  p.vector  <- c(mu.true=.5,muS=.2,sigma.true=.05,sigmaS=.03,tau.true=.08,tauS=.05,tf=.1,gf=.1) 
}

check.p.vector(p.vector,model)

# This's how (toy) stop-signal data look like with fixed SSDs:
n <- 6
# SSD must be a scalar, or a vector the same length as the number of cells, 
# or the same length as the data and have Inf in all go cells.
# SSD = .32 will result in response rate of about 50%, but only for THIS particular parameter setting:
data_fixed <- data.model.dmc(simulate.dmc(p.vector,model,n=n,SSD=c(Inf,Inf,.32,.32)),model)
data_fixed

# This's how (toy) stop-signal data look like with staircase tracking:
# SSD gives start of the tracking algorithm, then moves stop-signal delay 
# up und down by .05ms contingent on performance:
data_stair <- data.model.dmc(simulate.dmc(p.vector,model,n=n,staircase=.05, SSD=c(Inf,Inf,.25,.25)),model)
data_stair

# Make more realistic data:
n <- c(375,375,125,125)
data <- data.model.dmc(simulate.dmc(p.vector,model,staircase=.05,n=n,SSD=c(Inf,Inf,.25,.25)),model)

# Plot go RT distribution;
# P(NA) = Probability of go omission;
# Accuracy is computed as correct go/(all go - go omissions):
correct <- as.numeric(data$S)==(as.numeric(data$R)-1)
layout(1)
plot.cell.density(data[data$SS=="GO",],
                  C=correct[data$SS=="GO"],
                  ymax=5,main="Go RTs")

# Overall accuracy on go task (i.e., proportion go omissions in this case; remember, no errors!):
tapply(as.numeric(data$S)==(as.numeric(data$R)-1),data$SS,mean,na.rm=TRUE)["GO"]
# GO 
# 0.884

# Show the different SSDs:
sort(tapply(as.character(data$SSD),data[,c("SS")],unique)$SS)
# "0.1"  "0.15" "0.2"  "0.25" "0.3"  "0.35" "0.4"  "0.45" "0.5"  

# Show the number of trials for each SSD:
Ns = tapply(data$RT,data$SSD,length)
Ns
# 0.1 0.15  0.2 0.25  0.3 0.35  0.4 0.45  0.5  Inf 
#   1    6   18   46   76   67   28    7    1  750

# Show response rate:
tapply(!is.na(data$RT),data[,c("SS")],mean)
#    GO    SS 
# 0.884 0.492  

# Response rate broken down by SSD & corresponding inhibition function:
tapply(!is.na(data$RT),data[,c("SS","SSD")],mean)
plot_SS_if.dmc(data)  #P(Respond) increases as a function of SSD, as it should

# Plot median signal-respond RT per SSD:
tapply(data$RT,data$SSD,median,na.rm=TRUE) 
# 0.1      0.15       0.2      0.25       0.3      0.35       0.4 
#  NA 0.5252663 0.4763587 0.4985006 0.5159737 0.5229232 0.5733439 
# 0.45       0.5       Inf 
# 0.5547632 0.5441759 0.5627012 

# Plot median signal-respond RT per SSD:
plot_SS_srrt.dmc(data) # Median SRRT increases as a function of SSD, as it should

# Show number of signal-respond RTs per SSD:
Nr = tapply(!is.na(data$RT),data[,c("SS","SSD")],sum)[2,]
Nr
# 0.1 0.15  0.2 0.25  0.3 0.35  0.4 0.45  0.5  Inf 
#  0    1    5   13   31   44   22    6    1   NA 

# Signal-respond RTs should be faster than go RTs:
hist(data$RT[data$SS=="GO"],breaks="fd",main="SRRT vs. Go RT",freq=F,ylim=c(0,8))
lines(density(data$RT[data$SS=="SS"],na.rm=T),col="red",lwd=2)

###----------------------------------------- Let's start fitting

# Uniform (scaled beta) priors:
p1 <- p.vector; p1[1:length(p1)] <- 1; p1
p.prior <- prior.p.dmc(
  dists = rep("beta",length(p1)),p1=p1,p2=rep(1,length(p1)), # Uniform(0,1)
  lower=rep(0,length(p1)),upper=c(rep(2,2),rep(.5,4),rep(1,2)) # Scale to Uniform(lower,upper)
)
par(mfcol=c(2,4)); for (i in names(p.prior)) plot.prior(i,p.prior,ylim = c(0,4))

# Start sampling
samples <- samples.dmc(nmc=100,p.prior,data)
samples <- run.unstuck.dmc(samples,report = 10,cores=4,p.migrate=.05,verbose=TRUE)
layout(1)
plot.dmc(samples,pll.chain=TRUE)

samples2 <- run.converge.dmc(samples.dmc(samples=samples,nmc=50,thin=5),
                             report=10, cores=4,cut=1.1,verbose=TRUE,nmc=50,minN=500,max.try=20)

# Posterior loglikelihood looks nice:
layout(1)
plot.dmc(samples2,pll.chain=TRUE)
# Parameter chains look like fat hairy caterpillars:
plot.dmc(samples2,layout=c(2,4))

# Rhat looks good:
gelman.diag.dmc(samples2)
# Potential scale reduction factors:
#  
#  Point est. Upper C.I.
# mu.true          1.02       1.03
# sigma.true       1.02       1.04
# tau.true         1.03       1.05
# tf               1.02       1.03
# muS              1.02       1.04
# sigmaS           1.01       1.02
# tauS             1.01       1.02
# gf               1.01       1.02
#
# Multivariate psrf
#
# 1.05

# Good parameter recovery:
summary.dmc(samples2)
p.vector
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#  
#               Mean       SD  Naive SE Time-series SE
# mu.true    0.50025 0.004052 6.754e-05      0.0001826
# sigma.true 0.04744 0.002928 4.880e-05      0.0001257
# tau.true   0.07804 0.004951 8.252e-05      0.0002108
# tf         0.09748 0.052408 8.735e-04      0.0023456
# muS        0.21920 0.020341 3.390e-04      0.0009324
# sigmaS     0.02362 0.016345 2.724e-04      0.0007542
# tauS       0.04176 0.024714 4.119e-04      0.0011471
# gf         0.11496 0.011592 1.932e-04      0.0005035
#
# 2. Quantiles for each variable:
#  
#               2.5%     25%     50%     75%   97.5%
# mu.true    0.492298 0.49752 0.50025 0.50291 0.50805
# sigma.true 0.042017 0.04545 0.04744 0.04933 0.05337
# tau.true   0.068773 0.07454 0.07797 0.08123 0.08782
# tf         0.010134 0.05965 0.09427 0.12900 0.21407
# muS        0.180101 0.20601 0.21836 0.23292 0.25877
# sigmaS     0.001216 0.01015 0.02134 0.03417 0.06073
# tauS       0.002640 0.02379 0.03932 0.05679 0.09634
# gf         0.093316 0.10721 0.11485 0.12254 0.13905
#
# mu.true    muS    sigma.true     sigmaS   tau.true       tauS         tf         gf 
# 0.50       0.20       0.05       0.03       0.08       0.05       0.10       0.10 

# Parameter recovery can be summarized as follows:
check.recovery.dmc(samples2,p.vector)
#                mu.true  muS sigma.true sigmaS tau.true  tauS    tf   gf
# True              0.50 0.20       0.05   0.03     0.08  0.05  0.10 0.10
# 2.5% Estimate     0.49 0.18       0.04   0.00     0.07  0.00  0.01 0.09
# 50% Estimate      0.50 0.22       0.05   0.02     0.08  0.04  0.09 0.11
# 97.5% Estimate    0.51 0.26       0.05   0.06     0.09  0.10  0.21 0.14
# Median-True       0.00 0.02       0.00  -0.01     0.00 -0.01 -0.01 0.01

# Priors are nicely updated
# Go parameters are typically better constrained than stop parameters (more data = less uncertainty)
plot.dmc(samples2,layout=c(2,4),p.prior=p.prior,show.obs=FALSE)

# Good fits, as expected for simulated data.
# NB: For Stop-Signal fits need to save off the raw simulations!
pp <- post.predict.dmc(samples2,n.post=200,save.simulation=TRUE)

# Compare observed (histogram) and predicted (gray density lines) go RT distributions:
layout(1)
plot_SS_go.dmc(data=samples2$data,sim=pp)

# Next compare observed (red) and predicted (grey violins) response rates on the 
# different SSDs using posterior-predictive simulations.

# Also print the number of trials/SSD (n) and corresponding posterior-predictive 
# p value (p = mean(predicted response rate >= observed response rate)
plot_SS_if.dmc(data=samples2$data,sim=pp)
# SSD  n     p
# 0.10  1    NA
# 0.15  6 0.560
# 0.20 18 0.215
# 0.25 46 0.525
# 0.30 76 0.755
# 0.35 67 0.300
# 0.40 28 0.385
# 0.45  7 0.565
# 0.50  1    NA

# Note that this function also provides plots of inhibition functions averaged
# over subjects, in which case the data argument should be a samples object for
# multiple subjects. By default the averaging is of percentiles (i.e., for each 
# subject get the percentile cut points of their SSD distribution, then average 
# particiapnts' probabilty of inhibition within each percentile range). This 
# default choice was guided by the observation that individual differences cause 
# averages on absolute SSD values to be flat, unlike the inhibition functions 
# for any individual.

# Finally compare observed (red) and predicted (grey violins) median signal-respond 
# RTs using posterior-predictive simulations. 

# For each SSD for which the median signal-respond RT can be computed,
# also print the number of trials/SSD (n), the number of observed signal-respond RTs/SSD (nrt),
# the corresponding posterior-predictive p value (p), and the number of simulated 
# datasets for which median signal-respond RT can be computed:
plot_SS_srrt.dmc(data=samples2$data,sim=pp)
# SSD  n nrt         p n.sim
# 0.15  6   1 0.6428571   112
# 0.20 18   5 0.8359788   189
# 0.25 46  13 0.7450000   200
# 0.30 76  31 0.7600000   200
# 0.35 67  44 0.8850000   200
# 0.40 28  22 0.0700000   200
# 0.45  7   6 0.5000000   200
# 0.50  1   1        NA   168

# In this case the default for averaging is to pool all of the SSDs across 
# subjects then get the cut points between which to calculate each participant’s
# SSRTs from the percentiles of that pooled SSD distribution. The percentile.av
# method used for the inhibition function is also available here (as it the 
# default method used here for the inhibition function when percentile.av=FALSE)
# but tends to produce SSRT functions that tend to be flatter than observed for
# any indvidual.

###----------------------------------------------------------------------In-class exercise 1:

# Compare the observed vs. predicted omission rates on Go trials.

pp.go <- subset(pp,SS=="GO")
data.go <- subset(samples2$data,SS=="GO")
# Predicted:
pp.omission <- with(pp.go,tapply(RT,reps,function(x) mean(is.na(x))))
# Observed
omission <- mean(is.na(data.go$RT))
# Plot
layout(1)
hist(pp.omission)
points(omission,1,col="red",pch=16,cex=2)
# Posterior predictive p-value:
mean(pp.omission>=omission)

###----------------------------------------------------------------------In class excercise 2:

# Fit the data from sample2 with a misspecified model that does not feature trigger failures.
# How does this effect the parameter estimates?

# Specify the model without trigger failures:
modelMis <- model.dmc(
  factors=list(S=c("s1","s2"),SS=c("GO","SS")),
  responses=c("NR","r1","r2"),
  match.map=list(M=list(s1="r1",s2="r2",s1="NR")),
  p.map=list(mu="M",sigma="M",tau="M",tf="1",muS="1",sigmaS="1",tauS="1",gf="1"),
  constants=c(mu.false=1e6,sigma.false=.001,tau.false=.001,tf=0), #No trigger failures
  type="exgss")

p.vectorMis <- c(mu.true=NA,muS=NA,sigma.true=NA,sigmaS=NA,tau.true=NA,tauS=NA,gf=NA) 

# Specify the data:
dataMis <- data.model.dmc(samples2$data,modelMis)

# As before, uniform (scaled beta) priors, but now we don't need prior for tf:
p1Mis <- p.vectorMis; p1Mis[1:length(p1Mis)] <- 1; p1Mis
p.priorMis <- prior.p.dmc(
  dists = rep("beta",length(p1Mis)),p1=p1Mis,p2=rep(1,length(p1Mis)), # Uniform(0,1)
  lower=rep(0,length(p1Mis)),upper=c(rep(2,2),rep(.5,4),1) # Scale to Uniform(lower,upper)
)
par(mfcol=c(2,4)); for (i in names(p.priorMis)) plot.prior(i,p.priorMis,ylim = c(0,4))

# Start sampling:
samples3 <- samples.dmc(nmc=100,p.priorMis,dataMis)
samples3 <- run.unstuck.dmc(samples3,report = 10,cores=4,p.migrate=.05,verbose=TRUE)
layout(1)
plot.dmc(samples3,pll.chain=TRUE,density=FALSE,smooth=FALSE)

samples4 <- run.converge.dmc(samples.dmc(samples=samples3,nmc=50,thin=5),report=10, cores=4,cut=1.1,verbose=TRUE,nmc=50,minN=500,max.try=20)

# Posterior loglikelihood looks nice:
layout(1)
plot.dmc(samples4,pll.chain=TRUE,density=FALSE,smooth=FALSE)
# Parameter chains look like fat hairy catapillars:
plot.dmc(samples4,layout=c(2,4),smooth=FALSE,density=FALSE)

# Rhat looks good:
gelman.diag.dmc(samples4)

# Biased stop estimates:
summary.dmc(samples4)
p.vector

# Priors are updated but posteriors don't recover true values very well:
plot.dmc(samples4,layout=c(2,4),p.prior=p.priorMis)
p.vector

# Compare to true model:
par(mfcol=c(2,4))
for(i in names(p.vectorMis)){
  hist(samples2$theta[,i,],main=i,freq=F,breaks="fd")
  lines(density(samples4$theta[,i,]),col="red",lwd=2)
}

ppMis <- post.predict.dmc(samples4,n.post=200,save.simulation=TRUE)

# Similar predictions for Go RT distributions (no effect on go parameters):
par(mfcol=c(1,2))
plot_SS_go.dmc(data=samples2$data,sim=ppMis)
plot_SS_go.dmc(data=samples4$data,sim=pp)

# Misspecified model often produces more variable predictions:
# and more extreme p values (but not always...)
par(mfcol=c(1,2))
plot_SS_if.dmc(data=samples4$data,sim=ppMis)
plot_SS_if.dmc(data=samples2$data,sim=pp)

par(mfcol=c(1,2))
plot_SS_srrt.dmc(data=samples4$data,sim=ppMis,ylim=c(.3,.8))
plot_SS_srrt.dmc(data=samples2$data,sim=pp,ylim=c(.3,.8))

# save_data(data,samples,samples2,pp,dataMis,samples3,samples4,ppMis,file="dmc_SS_EXG2.RData")


