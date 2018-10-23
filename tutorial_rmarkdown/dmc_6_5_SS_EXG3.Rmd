##################  DMC Lesson 6: More Models

### Lesson 6.5: ExGaussian stop-signal model with 1 stop & 2 go accumulators 


### An EX-Gaussian stop-signal example with TRIGGER and GO FAILURE and 
# CONTEXT INDEPENDENT parametrization (i.e., separate stop accumulator parameters).
# This example uses a 3-accumulator race suitable for high error rates on the go task.
# It also uses a probit scale for the tf and gf parameters, and truncated normal
# priors (this could also be done with the 2 accumulator modle), which work best 
# with hierarchical models (although these are not fit here).
rm(list=ls())
# Current working directory must be set to the top-level folder  
# containing the dmc and tutorial subfolders 
source ("dmc/dmc.R")
load_model ("EXG-SS","exgSSprobit.R")
# load_data("dmc_6_5.RData")

# We will look at the most realistic scenario:
is.tf <- TRUE
is.gf <- TRUE
use.staircase <- TRUE

# Note that in cases where we set trigger failure and go failure to zero on the
# probit scale we set it to -6 (pnorm(-6)=9.865876e-10)
if (!is.tf & !is.gf) {
  
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
    # No trigger failures and no go failures:            
    constants=c(tf=-6,gf=-6),
    type="exgss")
  
  # This parameter setting will produce about 25% (relatively slow) errors:
  p.vector  <- c(mu.true=.5,mu.false=.60,muS=.2,
                 sigma.true=.05,sigma.false=.030,sigmaS=.03,
                 tau.true=.08,tau.false=.04,tauS=.05)
}

if (is.tf & !is.gf) {
  
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
    # No go failures: 
    constants=c(gf=-6),
    type="exgss")
  
  # This parameter setting will produce about 25% (relatively slow) errors:
  p.vector  <- c(mu.true=.5,mu.false=.60,muS=.2,
                 sigma.true=.05,sigma.false=.030,sigmaS=.03,
                 tau.true=.08,tau.false=.04,tauS=.05,
                 tf=qnorm(.1))
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
    # No trigger failures: 
    constants=c(tf=-6),
    type="exgss")
  
  # This parameter setting will produce about 25% (relatively slow) errors:
  p.vector  <- c(mu.true=.5,mu.false=.60,muS=.2,
                 sigma.true=.05,sigma.false=.030,sigmaS=.03,
                 tau.true=.08,tau.false=.04,tauS=.05,
                 gf=qnorm(.1))
}

if (is.tf & is.gf) {
  
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
    type="exgss")
  
  # This parameter setting will produce about 25% (relatively slow) errors:
  p.vector  <- c(mu.true=.5,mu.false=.60,muS=.2,
                 sigma.true=.05,sigma.false=.030,sigmaS=.03,
                 tau.true=.08,tau.false=.04,tauS=.05,
                 tf=qnorm(.1),gf=qnorm(.1))
}

check.p.vector(p.vector,model)

# This's how (toy) stop-signal data look like with fixed and staircase SSDs;
# Now we have errors:
n <- 6
data_fixed <- data.model.dmc(simulate.dmc(p.vector,model,n=n,SSD=c(Inf,Inf,.32,.32)),model)
data_fixed

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
                  xlim=c(0,5),ymax=5,main="Go RTs")

# Overall accuracy (i.e., all trials-(errors + omission)
tapply(as.numeric(data$S)==(as.numeric(data$R)-1),data$SS,mean,na.rm=TRUE)["GO"]
#  GO 
# 0.676  

# Show the different SSDs:
sort(tapply(as.character(data$SSD),data[,c("SS")],unique)$SS)
# "0.15" "0.2"  "0.25" "0.3"  "0.35" "0.4"  "0.45"

# Show the number of trials for each SSD:
Ns = tapply(data$RT,data$SSD,length)
Ns
# 0.15  0.2 0.25  0.3 0.35  0.4 0.45  Inf 
#    1   10   43   79   77   35    5  750 

# Show response rate:
tapply(!is.na(data$RT),data[,c("SS")],mean)
#   GO    SS 
# 0.896 0.500 

# Response rate broken down by SSD & corresponding inhibition function:
tapply(!is.na(data$RT),data[,c("SS","SSD")],mean)
layout(1)
plot_SS_if.dmc(data)  # P(Respond) increases as a function of SSD, as it should
Ns

# Median signal-respond RT per SSD:
tapply(data$RT,data$SSD,median,na.rm=TRUE)
# 0.15  0.2      0.25       0.3      0.35       0.4      0.45        Inf 
# NA  0.4169316 0.5367189 0.5176544 0.5497371 0.5468397 0.5735215 0.5588533

# Plot median signal-respond RT per SSD:
plot_SS_srrt.dmc(data) # Median SRRT increases as a function of SSD, as it should

# Show number of signal-respond RT per SSD:
Nr = tapply(!is.na(data$RT),data[,c("SS","SSD")],sum)[2,]
Nr
# 0.15  0.2 0.25  0.3 0.35  0.4 0.45  Inf 
#   0    1   10   32   47   30    5   NA 

# Signal-respond RTs should be faster than go RTs:
hist(data$RT[data$SS=="GO"],breaks="fd",main="SRRT vs. Go RT",freq=F,ylim=c(0,8))
lines(density(data$RT[data$SS=="SS"],na.rm=T),col="red",lwd=2)

###----------------------------------------- Let's start fitting

# Uniform (scaled beta) priors:
p1 <- p.vector; p1[1:length(p1)] <- 1; p1[10:11] <- 3
p.prior <- prior.p.dmc(
  dists = rep("tnorm",length(p1)),p1=p.vector,p2=p1, 
  lower=c(rep(0,length(p1)-2),-6,-6),upper=c(rep(2,3),rep(.5,6),rep(6,2)) 
)
par(mfcol=c(2,6)); for (i in names(p.prior)) plot.prior(i,p.prior)

# Start sampling
samples <- samples.dmc(nmc=100,p.prior,data)
samples <- run.unstuck.dmc(samples,report = 10,cores=33,p.migrate=.05,verbose=TRUE)
layout(1)
plot.dmc(samples,pll.chain=TRUE)

samples2 <- run.converge.dmc(samples.dmc(samples=samples,nmc=50,thin=5),
                             report=10, cores=33,cut=1.1,verbose=TRUE,nmc=50,minN=500,max.try=20)

# Posterior loglikelihood looks nice:
layout(1)
plot.dmc(samples2,pll.chain=TRUE,density=FALSE,smooth=FALSE)
# Parameter chains look like fat hairy catapillars:
plot.dmc(samples2,layout=c(2,6))

# Rhat looks good:
gelman.diag.dmc(samples2)
# Potential scale reduction factors:
#  
#  Point est. Upper C.I.
# mu.true           1.04       1.06
# mu.false          1.04       1.05
# sigma.true        1.03       1.04
# sigma.false       1.03       1.05
# tau.true          1.04       1.06
# tau.false         1.04       1.05
# tf                1.02       1.03
# muS               1.03       1.05
# sigmaS            1.03       1.04
# tauS              1.03       1.04
# gf                1.03       1.04
#
# Multivariate psrf
#
# 1.06

# Good parameter recovery:
summary.dmc(samples2)
p.vector
# 1. Empirical mean and standard deviation for each variable,
# plus standard error of the mean:
#  
#  Mean       SD  Naive SE Time-series SE
# mu.true     0.49622 0.005200 7.391e-05      0.0002151
# mu.false    0.60454 0.009558 1.358e-04      0.0004291
# sigma.true  0.04455 0.003376 4.799e-05      0.0001487
# sigma.false 0.03489 0.003966 5.637e-05      0.0001696
# tau.true    0.08402 0.007667 1.090e-04      0.0003308
# tau.false   0.03220 0.011183 1.590e-04      0.0004908
# tf          0.07216 0.055877 7.942e-04      0.0027383
# muS         0.20660 0.025018 3.556e-04      0.0011174
# sigmaS      0.04280 0.021732 3.089e-04      0.0009477
# tauS        0.04450 0.028782 4.091e-04      0.0013183
# gf          0.10416 0.010917 1.552e-04      0.0004594
#
# 2. Quantiles for each variable:
#  
#  2.5%     25%     50%     75%   97.5%
# mu.true     0.486009 0.49268 0.49618 0.49978 0.50659
# mu.false    0.588910 0.59805 0.60319 0.60957 0.62756
# sigma.true  0.038319 0.04227 0.04431 0.04686 0.05128
# sigma.false 0.027526 0.03212 0.03472 0.03751 0.04299
# tau.true    0.069964 0.07857 0.08405 0.08923 0.09946
# tau.false   0.007361 0.02523 0.03321 0.03980 0.05300
# tf          0.003173 0.02878 0.06038 0.10487 0.20575
# muS         0.152122 0.19031 0.20789 0.22519 0.25026
# sigmaS      0.003310 0.02714 0.04337 0.05777 0.08474
# tauS        0.002178 0.02094 0.04115 0.06382 0.10759
# gf          0.084133 0.09653 0.10385 0.11140 0.12616

# mu.true    mu.false         muS  sigma.true sigma.false 
# 0.50        0.60        0.20        0.05        0.03 
# sigmaS    tau.true   tau.false      tauS          tf 
# 0.03        0.08        0.04        0.05        0.10 
# gf 
# 0.10 

# Check parameter recovery:
check.recovery.dmc(samples2,p.vector)
#               mu.true    mu.false muS sigma.true sigma.false sigmaS
# True              0.50     0.60 0.20       0.05        0.03   0.03
# 2.5% Estimate     0.49     0.59 0.15       0.04        0.03   0.00
# 50% Estimate      0.50     0.60 0.21       0.04        0.03   0.04
# 97.5% Estimate    0.51     0.63 0.25       0.05        0.04   0.08
# Median-True       0.00     0.00 0.01      -0.01        0.00   0.01
#
#                 tau.true tau.false tauS    tf   gf
# True               0.08      0.04  0.05  0.10 0.10
# 2.5% Estimate      0.07      0.01  0.00  0.00 0.08
# 50% Estimate       0.08      0.03  0.04  0.06 0.10
# 97.5% Estimate     0.10      0.05  0.11  0.21 0.13
# Median-True        0.00     -0.01 -0.01 -0.04 0.00

# Priors are nicely updated;
# Note that for given n, EXG3 typically results in wider posteriors than EXG2, 
# because EXG3 estimates three extra parameters:
plot.dmc(samples2,layout=c(2,6), p.prior=p.prior, show.obs=FALSE)

# Good fits, as expected for simulated data: 
pp <- post.predict.dmc(samples2,n.post=200,save.simulation=TRUE)

# Compare observed (histogram) and predicted (gray density lines) go RT distributions:
plot_SS_go.dmc(data=samples2$data,sim=pp)

# Compare observed (red) and predicted (gray violins) response rates on the different SSDs
# using posterior-predictive simulations. 
# Aslo print the number of trials/SSD (n) and corresponding posterior-predictive p value 
# (p = mean(predicted response rate >= observed response rate)
layout(1)
plot_SS_if.dmc(data=samples2$data,sim=pp)
# SSD  n     p
# 1 0.15  1    NA
# 2 0.20 10 0.805
# 3 0.25 43 0.695
# 4 0.30 79 0.705
# 5 0.35 77 0.550
# 6 0.40 35 0.190
# 7 0.45  5 0.515

# Compare observed (red) and predicted (gray violins) median signal-respond RTs using
# posterior-predictive simulations. 
# For each SSD for which the median signal-respond RT can be computed,
# also print the number of trials/SSD (n), the number of observed signal-respond RTs/SSD (nrt),
# the corresponding posterior-predictive p value (p), and the number of simulated 
# datasets for which median signal-respond RT can be computed:
layout(1)
plot_SS_srrt.dmc(data=samples2$data,sim=pp)
# SSD  n nrt     p n.sim
# 1 0.20 10   1 1.000   161
# 2 0.25 43  10 0.175   200
# 3 0.30 79  32 0.700   200
# 4 0.35 77  47 0.120   200
# 5 0.40 35  30 0.535   200
# 6 0.45  5   5 0.350   200

###----------------------------------------------------------------------In class exercise 1

# Compare the observed vs. predicted error rates on Go trials.
pp.go <- subset(pp,SS=="GO")
data.go <- subset(samples2$data,SS=="GO")

# Predicted:
pp.error <- with(pp.go,tapply((as.numeric(S)!=(as.numeric(R)-1) & !is.na(RT)),reps,function(x) mean(x)))
# Observed:
error <- mean(as.numeric(data.go$S)!=(as.numeric(data.go$R)-1) & !is.na(data.go$RT))
# Plot
layout(1)
hist(pp.error)
points(error,1,col="red",pch=16,cex=2)
# Posterior predictive p-value:
mean(pp.error>=error)

###----------------------------------------------------------------------In class exercise 2

# Fit the data from sample2 with a misspecified model that does not account for errors (see previous lesson).
# How does this effect the parameter estimates?

# Specify the model without errors:
modelMis <- model.dmc(
  factors=list(S=c("s1","s2"),SS=c("GO","SS")),
  responses=c("NR","r1","r2"),
  match.map=list(M=list(s1="r1",s2="r2",s1="NR")),
  p.map=list(mu="M",sigma="M",tau="M",tf="1",muS="1",sigmaS="1",tauS="1",gf="1"),
  # No errors:
  constants=c(mu.false=1e6,sigma.false=.001,tau.false=.001),
  type="exgss")

p.vectorMis  <- c(mu.true=NA,muS=NA,sigma.true=NA,sigmaS=NA,tau.true=NA,tauS=NA,tf=NA,gf=NA) 

# Remove error RTs:
remove <- as.numeric(samples2$data$S)!=(as.numeric(samples2$data$R)-1) & !is.na(samples2$data$RT)
dataMis <- data.model.dmc(samples2$data[!remove,],modelMis)

# Sanity checks;
# Plot go RT distribution:
correctMis <- as.numeric(dataMis$S)==(as.numeric(dataMis$R)-1)
layout(1)
plot.cell.density(dataMis[dataMis$SS=="GO",],
                  C=correctMis[dataMis$SS=="GO"],
                  ymax=6,main="Go RTs")

# Overall accuracy on go task (i.e., proportion go omissions in this case; remember, no errors!):
tapply(as.numeric(dataMis$S)==(as.numeric(dataMis$R)-1),dataMis$SS,mean,na.rm=TRUE)["GO"]

# Show response rate;
# Go omission rate changes simply because number of Go trials changes;
# Stop response rate decreases because we remove only the signal-respond RTs:
tapply(!is.na(dataMis$RT),dataMis[,c("SS")],mean)
tapply(!is.na(data$RT),data[,c("SS")],mean)

# As before, uniform (scaled beta) priors, but now we don't need prior for mu.false, sigma.false, and tau.false:
p1Mis <- p.vectorMis; p1Mis[1:length(p1Mis)] <- 1; p1Mis
p.priorMis <- prior.p.dmc(
  dists = rep("beta",length(p1Mis)),p1=p1Mis,p2=rep(1,length(p1Mis)), # Uniform(0,1)
  lower=rep(0,length(p1Mis)),upper=c(rep(2,2),rep(.5,4),rep(1,2)) # Scale to Uniform(lower,upper)
)
par(mfcol=c(2,4)); for (i in names(p.priorMis)) plot.prior(i,p.priorMis,ylim = c(0,4))

# Start sampling:
samples3 <- samples.dmc(nmc=100,p.priorMis,dataMis)
samples3 <- run.unstuck.dmc(samples3,report = 10,cores=4,p.migrate=.05,verbose=TRUE)
layout(1)
plot.dmc(samples3,pll.chain=TRUE,density=FALSE,smooth=FALSE)

samples4 <- run.converge.dmc(samples.dmc(samples=samples3,nmc=50,thin=5),report=10, cores=4,cut=1.1,verbose=TRUE,nmc=50,minN=500,max.try=20)

# Posterior loglikelihood looks nice:
plot.dmc(samples4,pll.chain=TRUE,density=FALSE,smooth=FALSE)
# Parameter chains look like fat hairy catapillars:
plot.dmc(samples4,layout=c(2,4),smooth=FALSE,density=FALSE)

# Rhat looks good:
gelman.diag.dmc(samples4)

# Biased estimates:
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

# Can't really see differences in predictions for Go RT distribution; 
# strong trade-off between mu.true and tau.true...
layout(1)
plot_SS_go.dmc(data=samples4$data,sim=ppMis)
plot_SS_go.dmc(data=samples2$data,sim=pp)

# Misspecified model often produces more variable predictions:
# and more extreme p values (but not always...)
par(mfcol=c(1,2))
plot_SS_if.dmc(data=samples4$data,sim=ppMis)
plot_SS_if.dmc(data=samples2$data,sim=pp)

par(mfcol=c(1,2))
plot_SS_srrt.dmc(data=samples4$data,sim=ppMis,ylim=c(.3,.8))
plot_SS_srrt.dmc(data=samples2$data,sim=pp,ylim=c(.3,.8))

# save_data(data,samples,samples2,pp,dataMis,samples3,samples4,ppMis,
#           file="dmc_6_5.RData")
