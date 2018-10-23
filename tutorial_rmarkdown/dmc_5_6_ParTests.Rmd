##################  DMC Lesson 5: Advanced Multiple Subjects


### Lesson 5.6:  Testing parameter effects.
rm(list=ls())
source ("dmc/dmc.R")

# Parameter or posterior inference is a useful complement to model selection,
# where differences are allowed in estimates across conditions then tests are
# made to determine the probability of obtaining a difference of the observed
# magnitude (i.e., a posterior p value). This requires calculating the difference
# for each sampled posterior parameter and calculating the probability that the
# distribution of differences is greater than zero.
# 
# The procedure can be applied to individual subject fits, fixed effects fits
# and hierarchical fits, and in the latter case can also be applied to 
# hyper-parameters. For groups of subjects they can be applied to average 
# differences over subjects (i.e., the distribution if of group average 
# differences or to hyper-parameters. However, in the latter case they will not
# reflect correlations inherit in a with-subject contrast as this structure is
# not presently built in to DMC's hierarchical fitting (it is planned for the
# future). So, it is recommended that tests of within-subject factors are 
# done on averages of individual subject parameters, as they do capture the 
# correlation. However, such tests provide only fixed effects inference (about
# the group of subjects you measured not future subjects). Between subject 
# tests can be done on the hypers.

# We will use some fits of a Wald model with Go Failure (see dmc_6_2_Wald_GF.R)
# as an example, with samples (from a hierarchical fit) contained in an 
# object called "hsamples".
load_data("dmc_5_6.RData")

# The experiment had 20 subjects who had to indicate which of two stimuli 
# occurred (factor S=stimulus: low or high). They could do this either while 
# counting backwards by 3s or not (factor L=load: none vs. 3s). Here is the 
# model that was fit.

modelC <- model.dmc(p.map=list(A="1",B=c("L","R"),v=c("S","L","M"),t0="L",gf="L"),
                    match.map=list(M=list(high="HIGH",low="LOW")),
                    factors=list(L = c("none","3s"),S=c("high","low")),
                    constants=c(A=0),
                    responses=c(high="HIGH",low="LOW"),
                    type="wald")

# Parameter vector names are: ( see attr(,"p.vector") )
# [1] "B.none.HIGH"       "B.3s.HIGH"         "B.none.LOW"       
# [4] "B.3s.LOW"          "v.high.none.true"  "v.low.none.true"  
# [7] "v.high.3s.true"    "v.low.3s.true"     "v.high.none.false"
# [10] "v.low.none.false"  "v.high.3s.false"   "v.low.3s.false"   
# [13] "t0.none"           "t0.3s"             "gf.none"          
# [16] "gf.3s"            


# By default the compare.p function performs a test appropriate for a within-
# subjects effect, averaging over participant differences for two parameters
# specified with the pnames argument (second name - first name). It prints out
# a table of the 95% CI for each component of the difference and the difference
# ("contrast") followed by the probability that the difference is greater than
# zero. The names of the components can be controlled with the "pretty"
# argument. The width of the CI can be controlled by the lo.p (= .025) and 
# hi.p (= .975) arguments and significant digits with the digits argument.
# The test of the t0 parameter show participants were faster for 3s than none 
# by 45ms. 

compare.p(hsamples,pnames=c("t0.none","t0.3s"),pretty=c("none","3s"))
#        none    3s contrast
# 2.5%  0.234 0.188    0.025
# 50%   0.247 0.202    0.045
# 97.5% 0.261 0.216    0.064
# p.gt.0 
#      1

# Here we do the same test on the first subject by passing only their data
# (note the use of [] not [[]] to do this). We also ask for a plot of the
# posterior difference distribution (note that the table output can be turned
# off with show.table=FALSE). Default line0=TRUE shows a vertical line at zero.
# The legend can be turned off (show.legend=FALSE) and its position controlled
# (lpos="topleft" by defualt).
compare.p(hsamples[1],pnames=c("t0.none","t0.3s"),pretty=c("none","3s"),
          show.plot=TRUE,xlab="Non-decison time (s)")
#        none    3s contrast
# 2.5%  0.072 0.072   -0.091
# 50%   0.122 0.135   -0.013
# 97.5% 0.166 0.195    0.066
# p.gt.0 
#  0.373

# From here we will return to focusing on group inference.

# A function can be used to calculate the difference. Here it first converts
# the gf estimates to the probability scale (they are sampled on the probit)
# scale) before taking the difference. Note the function must return a 3-vector
# containing each element of the difference and the difference itself. The 
# results show 2.1% higher go failure in 3s than none.
compare.p(hsamples,show.plot=TRUE,main="Go Failure",pretty=c("none","3s"),
          fun=function(x){x<-pnorm(x[c("gf.none","gf.3s")]);c(x,-diff(x))},xlab="p(GF)")
#        none    3s contrast
# 2.5%  0.010 0.028   -0.027
# 50%   0.013 0.034   -0.021
# 97.5% 0.016 0.040   -0.014
# p.gt.0 
#      0

# Note that further arguments can be passed to fun through ... arguments. For 
# examples suppose we wished to test if go failure in the 3s condition was 
# greater than some given value.
fun=function(x,gf){x<-c(pnorm(x["gf.3s"]),gf);c(x,-diff(x))}

# We can now make the test passing whatever value of the values (gf) we like.
# Here we compare to a 2% rate.
compare.p(hsamples,pretty=c("3s","Fixed"),fun=fun,gf=.02)
#          3s Fixed contrast
# 2.5%  0.028  0.02    0.008
# 50%   0.034  0.02    0.014
# 97.5% 0.040  0.02    0.020
# p.gt.0 
#      1

# We can make the same test at the hyper level using hpar=TRUE. By defualt this
# this test is done on the location hyper. Estimates are a little different and
# CIs wider as between-subject variance is not partialed out of the contrast.
compare.p(hsamples,hyper=TRUE,
          show.plot=TRUE,main="Go Failure",pretty=c("none","3s"),
          fun=function(x){x<-pnorm(x[c("gf.none","gf.3s")]);c(x,-diff(x))},xlab="p(GF)")
#        none    3s contrast
# 2.5%  0.006 0.010   -0.030
# 50%   0.010 0.021   -0.010
# 97.5% 0.016 0.039    0.002
# p.gt.0 
#  0.053

# The same test can be made on the scale hyper (hpar=1 is the default for 
# testing location), showing greater individual differences in go failures under
# load.
compare.p(hsamples,hyper=TRUE,hpar=2,
          show.plot=TRUE,main="Go Failure",pretty=c("none","3s"),
          fun=function(x){x<-pnorm(x[c("gf.none","gf.3s")]);c(x,-diff(x))},xlab="p(GF)")
#        none    3s contrast
# 2.5%  0.592 0.655   -0.177
# 50%   0.636 0.717   -0.080
# 97.5% 0.701 0.802    0.012
# p.gt.0 
#  0.044

# Returning to test of averages over individual parameters, here we use a 
# function on four parameters, first averaging over thresholds for the
# two accumulators (HIGH and LOW) then taking a difference to see if average
# thresholds differ with load, finding an average B higher in 3s than none.

fun <- function(x){
  none <- mean(x[c("B.none.HIGH","B.none.LOW")])             
  s3 <- mean(x[c("B.3s.LOW","B.3s.HIGH")])  
  c(s3,none,s3-none)
}
compare.p(hsamples,show.plot=TRUE,pretty=c("3s","none"),fun=fun,xlab="Average Threshold")
#          3s  none contrast
# 2.5%  2.125 1.785    0.259
# 50%   2.172 1.841    0.331
# 97.5% 2.215 1.898    0.400
# p.gt.0 
#      1

# This function test the difference between 3s and none in the true-false rate
# differences averaged over high and low stimuli, finding them to be larger in 
# none than 3s.
fun <- function(x){
  hi.none.D <- diff(x[c("v.high.none.false","v.high.none.true")])             
  lo.none.D <- diff(x[c("v.low.none.false","v.low.none.true")])             
  hi.3s.D <- diff(x[c("v.high.3s.false","v.high.3s.true")])             
  lo.3s.D <- diff(x[c("v.low.3s.false","v.low.3s.true")])             
  none <- (hi.none.D+lo.none.D)/2
  s3 <- (hi.3s.D+lo.3s.D)/2
  c(none,s3,none-s3)
}
compare.p(hsamples,show.plot=TRUE,pretty=c("none","3s"),fun=fun,xlab="Average Rate")
#        none    3s contrast
# 2.5%  1.160 0.907    0.160
# 50%   1.233 0.973    0.261
# 97.5% 1.308 1.039    0.362
# p.gt.0 
#      1 

### BETWEEN SUBJECT TESTS

# Here we use separate LBA fits to two groups to illustrate testing between
# subject effects. The design had only a single stimulus factor, with levels 
# "new" and "old". The model fit was:

model <- model.dmc(
  p.map=list(A="1",B="R",mean_v=c("S","M"),sd_v=c("S","M"),t0="1",st0="1"), 
  match.map = list(M=list(new="NEW",old="OLD")),
  factors=list(S=c("new","old")),
  constants = c(sd_v.new.false=1,st0=0),responses = c("NEW","OLD"),type="norm")
#  [1] "A"                "B.NEW"            "B.OLD"           
#  [4] "mean_v.new.true"  "mean_v.old.true"  "mean_v.new.false"
#  [7] "mean_v.old.false" "sd_v.new.true"    "sd_v.old.true"   
# [10] "sd_v.old.false"   "t0"              
# Constants are (see attr(,"constants") ):
# sd_v.new.false            st0 
#              1              0 
# 
# Model type = norm (posdrift= TRUE )

# The fit to each groups of 24 participants was hierarchical with normal hyper prior
p1=c(A=1,B.NEW=2,B.OLD=2,
     mean_v.new.true=2,mean_v.old.true=2,mean_v.new.false=1,mean_v.old.false=1,
     sd_v.new.true=1,sd_v.old.true=1,sd_v.old.false=1,
     t0=.2)
m.prior <- prior.p.dmc(dists = rep("tnorm",11),
                       p1=ps,                           
                       p2=c(1,1,1,2,2,2,2,1,1,1,1),
                       lower=c(rep(0,3),rep(NA,4),rep(0,3),.1),upper=c(rep(NA,10),1)
)
p1[1:length(p1)] <- 1
s.prior <- prior.p.dmc(p1=p1,p2=p1,
                       dists=rep("gamma",length(p1)))
pp.prior=list(m.prior,s.prior)


# We first put the two fit objects into a list. If the list has names these
# will be used in output comparisons, otherwise they will be called G1 and G2.
fits=list(A1=hA1,A2=hA2)

# The first test compares the t0 parameter, specified using the within.pnames
# argument, at the hyper level. The arguments controlling outputs for the
# within subject case by compare.p, like "main" in this example, are also used 
# in the between subject version.
compare.ps(fits=fits,within.pnames="t0",main="t0",hyper=TRUE)
#          A1    A2 contrast
# 2.5%  0.508 0.114    0.252
# 50%   0.558 0.221    0.339
# 97.5% 0.609 0.288    0.455
# p.gt.0 
#      1 

# As shown the output is also the same as the with subjects version. The test
# can also be done on averages of participant level parameters rather than 
# hyper parameters, but here there is no advantage as there are no within-subject
# correlations to be take account of. The result is similar in this case.
compare.ps(fits=fits,within.pnames="t0",main="t0",hyper=FALSE)
#          A1    A2 contrast
# 2.5%  0.547 0.237    0.277
# 50%   0.558 0.258    0.300
# 97.5% 0.568 0.279    0.323
# p.gt.0 
#      1 

# From here we focus on the hyper parameters, which is the default setting. The
# next example shows how to use a function to combine parameters within each 
# group as input to the between subjects test using the within.fun argument. 
# Note that this function must return a single number, here the average of the
# thresholds for two accumulators. The results indicate a much higher average
# threshold in A2.
compare.ps(fits=fits,main="B.mean",within.fun=function(x){mean(x[c("B.OLD","B.NEW")])})
#          A1    A2 contrast
# 2.5%  0.531 2.759   -2.694
# 50%   0.697 3.047   -2.351
# 97.5% 0.859 3.344   -2.008
# p.gt.0 
#      0 

# A comparison of bias (here defined as NEW - OLD threshold) indicates a bias
# to old (i.e., threshold NEW > OLD) is stronger in the second group. 
compare.ps(fits=fits,main="B.mean",within.fun=function(x){diff(x[c("B.OLD","B.NEW")])})
#          A1    A2 contrast
# 2.5%  0.846 3.454   -3.573
# 50%   1.197 4.056   -2.869
# 97.5% 1.498 4.665   -2.160
# p.gt.0 
#      0     

