##################  DMC Lesson 5: Advanced Multiple Subjects


### Lesson 5.4:  Population covariates and plausible values.

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
#    THIS LESSON ALSO REQUIRES   # 
#        PACKAGE hypergeo        #
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #


# Demonstrates the use of "plausible values", a fully Bayesian way to test 
# correlations between subject-level covariate and hierarchical parameter
# estimates from models estimated without fitting the covariates, see 
# Marsman, M., Maris, G., Bechger, T. & G. Maris, T. M. Bechger & Glas, C.A.W. 
# (in press). What Can We Learn From Plausible Values? Psychometrika. 
# maartenmarsman.com/wp-content/uploads/2016/04/MarsmanMarisBechgerGlas2015.pdf

rm(list=ls())

# Current working directory must be set to the top-level folder  
# containing the dmc and tutorial subfolders 
source ("dmc/dmc.R")
load_model ("LNR","lnrPP.R")

# load_data ("dmc_5_4.RData")

# The first example below shows how to simulate data where a parameter is a
# a linear function of a covariate that varies among subjects. The second 
# example (".nr") has the same structure but no covariate.

# Data model
model <- model.dmc(p.map=list(meanlog="M",sdlog="M",t0="1",st0="1"),
                   match.map=list(M=list(s1=1,s2=2)),constants=c(st0=0),
                   factors=list(S=c("s1","s2")),responses=c("r1","r2"),type="lnr")

# Population distribution (used to simulate data not as a prior)
p.mu  <- c(meanlog.true=-1,meanlog.false=0,        # natural scale
           sdlog.true=log(1),sdlog.false=log(1),t0=log(.2)) # log scale

# Fairly tight distributions
p.sigma <- c(.2,.2,.2,.2,.1); names(p.sigma) <- names(p.mu)
p.prior <- prior.p.dmc(p1=p.mu,p2=p.sigma,
                       untrans=c(sdlog.true="exp",sdlog.false="exp",t0="exp"))

# plot population distributions
par(mfcol=c(2,3)); for (i in names(p.prior)) plot.prior(i,p.prior)

# Create a subject covariate data frame, using column names to indicate which 
# paramter is affected. Each parameter's covariate is simply added to the 
# "intercept" parameter from the standard model (so e.g., if the real covariate)
# was X and it had slope coefficients then the data frame would contain the
# the product sX. 
subject.cv <- data.frame(meanlog.true=rnorm(100,0,.2))

# subject.cv is added to the named parameter(s) in simulated data.
raw.data <- h.simulate.dmc(model,p.prior=p.prior,n=100,ns=100,
                           subject.cv=subject.cv)


# The sample correlation is
cor(attributes(raw.data)$parameters[,"meanlog.true"],subject.cv$meanlog.true)
# [1] 0.7397503


# Fit the hierarchical model
data.model <- data.model.dmc(raw.data,model)

# Location prior
p.mu.mu <- c(0,0,log(1),log(1),log(0.2)); names(p.mu.mu) <- names(p.mu)
p.mu.sigma <- c(3,3,1,1,1); names(p.mu.sigma) <- names(p.mu)
mu.prior <- prior.p.dmc(p1=p.mu.mu,p2=p.mu.sigma,
                        untrans=c(sdlog.true="exp",sdlog.false="exp",t0="exp"))
# Scale prior
p.sigma.shape <- rep(1,5); names(p.sigma.shape) <- names(p.sigma)
p.sigma.scale <- c(1,1,.5,.5,.2)
sigma.prior <- prior.p.dmc(p1=p.sigma.shape,p2=p.sigma.scale,
                           dists=rep("gamma",length(p.sigma)))

# Make a hyper-prior list
pp.prior=list(mu.prior,sigma.prior)


# Make a new samples object from the same 40 subject data as in the last lesson
hsamples <- h.samples.dmc(nmc=50,p.prior,data.model,thin=10,pp.prior=pp.prior)

hsamples <- h.run.dmc(hsamples,cores=12,report=1,p.migrate=0.05,h.p.migrate=0.05)

# Convergence is rapid, results look stable after 25
plot.dmc(hsamples,hyper=TRUE,pll.chain = TRUE,smooth=FALSE,density=FALSE,start=25)

# Turn off migration, get 500 samples for correlation testing
hsamples1 <- h.run.dmc(h.samples.dmc(samples=hsamples,nmc=100),cores=12,report=1)

# Stable and converged
plot.dmc(hsamples1,hyper=TRUE,pll.chain = TRUE,smooth=FALSE,density=FALSE)
plot.dmc(hsamples1,hyper=TRUE,layout=c(2,5),smooth=FALSE,density=FALSE)

gelman.diag.dmc(hsamples1,hyper=TRUE)
# Multivariate psrf
# 1.03
gelman.diag.dmc(hsamples1)
# Mean
# [1] 1.02

# Good effective number of samples per participant. 
apply(do.call(rbind,effectiveSize.dmc(hsamples1)),2,mean)
#  meanlog.true meanlog.false    sdlog.true   sdlog.false            t0 
#        844.88        858.11        855.46        877.92        808.23 

# Save parameters for each subject
stats.r <- summary.dmc(hsamples1)

### Example with no correlation

raw.data.nc <- h.simulate.dmc(model,p.prior=p.prior,n=100,ns=100)
# Sample correlation 
cor(attributes(raw.data.nc)$parameters[,"meanlog.true"],subject.cv$meanlog.true)
# [1] 0.05210581

# Fit the hierarchical model
data.model.nc <- data.model.dmc(raw.data.nc,model)

hsamples.nc <- h.samples.dmc(nmc=50,p.prior,data.model.nc,thin=10,pp.prior=pp.prior)

hsamples.nc <- h.run.dmc(hsamples.nc,cores=12,report=1,p.migrate=0.05,h.p.migrate=0.05)

# Convergence is rapid, results look stable after 25
plot.dmc(hsamples.nc,hyper=TRUE,pll.chain = TRUE,smooth=FALSE,density=FALSE,start=25)

# Turn off migration, get 500 samples for correlation testing
hsamples1.nc <- h.run.dmc(h.samples.dmc(samples=hsamples.nc,nmc=100),cores=12,report=1)

# Stable and converged
plot.dmc(hsamples1.nc,hyper=TRUE,pll.chain = TRUE,smooth=FALSE,density=FALSE)
plot.dmc(hsamples1.nc,hyper=TRUE,layout=c(2,5),smooth=FALSE,density=FALSE)

gelman.diag.dmc(hsamples1.nc,hyper=TRUE)
# Multivariate psrf
# 1.05
gelman.diag.dmc(hsamples1.nc)
# Mean
# [1] 1.02

# Good effective number of samples per participant. 
apply(do.call(rbind,effectiveSize.dmc(hsamples1.nc)),2,mean)
#  meanlog.true meanlog.false    sdlog.true   sdlog.false            t0 
#        859.23        853.51        855.39        837.27        805.36 

# Save parameters for each subject
stats <- summary.dmc(hsamples1.nc)


# Traditional correlation tests between the covariate and the posterior mean. 
# The parameter estimates are not independent, violating test assumptions and
# inflating Type 1 error. See Boehm, U., Marsmann, M., Matzke, D., & 
# Wagenmakers, E.-J. (submitted). On the Importance of Avoiding Shortcuts in 
# modelling hierarchical data.

# The true correlation .707
cor.test(subject.cv$meanlog.true,
         unlist(lapply(stats.r,function(x){x$statistics["meanlog.true","Mean"]})))
# t = 9.0512, df = 98, p-value = 1.377e-14
#       cor 
# 0.6747771 

# True zero correlation
cor.test(subject.cv$meanlog.true,
         unlist(lapply(stats,function(x){x$statistics["meanlog.true","Mean"]})))
# t = 0.77462, df = 98, p-value = 0.4404
#        cor 
# 0.07800997 


# Plausible values are based on the correlation between the covariate and each
# posterior sample. The distribution of these correlations provides a credible
# interval on the true correlation value. 

# True correlation .707.
# cor.plausible invisibly returns the vector of correlations. When plot=TRUE it
# can also plot a density estimate of the plausible value 
# distribution.
cor.r <- cor.plausible(hsamples1,p.name="meanlog.true",cv=subject.cv,plot=TRUE)
mean(cor.r)
# [1] 0.6424909

# Posterior inference on whether the correlation is bigger than zero might be
# carried out as follows. 
mean(cor.r<0)
# [1] 0

# Similarly for the case where the true correlation is zero. Graphical
# parameters such as xlim can be passed to the plot.
cor.r0 <- cor.plausible(hsamples1.nc,p.name="meanlog.true",cv=subject.cv,
                        plot=TRUE,xlim = c(-.5,.5))
mean(cor.r0)
# [1] 0.07186854
mean(cor.r0<0)
# [1] 0.03666667


# Population plausible values.

# The previous approach is appropriate for making inference about the particular
# sample of subjects (subjects as "fixed effect") but does not take into account
# uncertainty from subjects being samples from a population (subjects as 
# "random effects") and so is not suitable for population inference. 
# The following demonstrates how to obtain population plausible values based on
# the result in Ly, A., Marsman, M., &  Wagenmakers, E.-J. (2016). Analytic 
# posteriors for Pearson's correlation coefficient. Manuscript submitted for 
# publication. http://arxiv.org/abs/1510.01188

# The value of kappa determines the prior for the population distribution, by
# default set to 1 implying a uniform prior.
kappa <- 1 # Uniform prior

# To calculate density values from -1 to 1 at regular intervals given
# by spacing argument (e.g., seq(-1,1, spacing)). Finer spacings may be required
# for very peaked priors, which can also cause some numerical issues. n argument
# specifies the sample size. The output is a vector of density estimates at each
# point on the grid with attributes for n and kappa. For finely spaced grids it
# can be slow to run.

dens.r <- postRav(n=100, r=cor.r,spacing=.01,kappa=kappa) # True correlation
dens.r0 <- postRav(n=100, r=cor.r0,kappa=kappa)           # No correlation

# Results can be plotted by converting the output into an object of class 
# density useing the postRav.Density function.
plot(postRav.Density(dens.r),xlim=c(0.4,.9),main="True=.707")
plot(postRav.Density(dens.r0),xlim=c(-.4,.4),main="True=0")

# These graphs show the effect of taking population uncertainty into account
par(mfrow=c(1,2))
cor.plausible(hsamples1,p.name="meanlog.true",cv=subject.cv,
              plot=TRUE,xlim=c(0.4,.9),main="True=.707")
lines(postRav.Density(dens.r),lty=2)
cor.plausible(hsamples1.nc,p.name="meanlog.true",cv=subject.cv,
              plot=TRUE,xlim=c(-.4,.4),main="True=0")
lines(postRav.Density(dens.r0),lty=2)

# Population mean estimate
postRav.mean(dens.r)
# [1] 0.6669078
postRav.mean(dens.r0)
# [1] 0.04052986

# Probability < 0 
postRav.p(dens.r,upper=0)
# [1] 1.374167e-13
postRav.p(dens.r0,upper=0)
# [1] 0.3675319

# Can also specify lower (or both, note the limits are inclusive)
postRav.p(dens.r0,lower=0)

# Credible intervals can be obtained as follows (here the default 95% intervals)
postRav.ci(dens.r,interval=c(.025,.975))
#     0.025     0.975 
# 0.4836657 0.7391183 

postRav.ci(dens.r0)
#      0.025      0.975 
# -0.1446104  0.2685964 

#NB: Accuracy depends on the spacing argument, and that can interact with how 
#    peaked the distribution, with the requested values being estimated via 
#    linear interpolation. Here we check that by using a higher resolution. 
dens.r.hires <- postRav(n=100, r=cor.r,spacing=.001,kappa=kappa) 

# In this case there is not much change.
postRav.ci(dens.r.hires)
#     0.025     0.975 
# 0.4883547 0.7436174

# Testing Correlation Differences: here we compare the true correlation and 
# no correlation cases as if e.g., they were results for two groups.

# In the sample case we simply use the distribution of differences. The
# following function can plot the differences and returns the probability that
# the first correlation argument is greater than the second.
compare.r(cor.r,cor.r0,plot=TRUE,xlim=c(-2,2),main="")
# [1] 1

# The population case requires using the save argument to postRav, which store
# the un-normalized pdfs (attribute "updfs") as linear interpolation objects
# (see ?approxfun). These are used to take n (=1 by default) samples from the 
# population distribution for each sampled correlation, which are then treated
# like the sampled correlation inputs in the sample difference case above.
dens.r <- postRav(n=100, r=cor.r,spacing=.01,kappa=kappa,save = TRUE) # True correlation
dens.r0 <- postRav(n=100, r=cor.r0,kappa=kappa,save = TRUE)           # No correlation

# Here we use the add argument to superimpose on the sample plot
compare.r(dens.r,dens.r0,add=TRUE,lty=2,main="Solid=Sample, Dashed=Population")
# [1] 1


save_data(subject.cv,raw.data,raw.data.nc,hsamples,hsamples1,stats.r,
          hsamples.nc,hsamples1.nc,stats,dens.r, dens.r0,file="dmc_5_4.RData")

