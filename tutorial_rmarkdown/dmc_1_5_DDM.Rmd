##################  DMC Lesson 1 Models and Data
#
# Note: Before running these scripts, the current working directory must be set 
# to the top-level folder containing the dmc and tutorial subfolders 

### Lesson 1.5 Exploring the DDM model

# This lesson is about familiarising yourself with the DDM model and its 
# parameters, and how you can create different scenarios with different 
# parameter values. See Ratcliff, R., & McKoon, G. (2008). The diffusion 
# decision model: Theory and data for two-choice decision tasks. Neural 
# Computation, 20(4), 873–922.

rm(list=ls()); par(mfrow = c(1,1))
source ("dmc/dmc.R")

load_model ("DDM","ddm.R") 

# Start with a simple design with a binary stimulus factor, S 
# (e.g, left vs. right motion) and corresponding responses.  
factors=list(S=c("left","right"))
responses=c("LEFT","RIGHT")
match.map=list(M=list(left="LEFT",right="RIGHT"))

# NB1: See ?ddiffusion for rtdists help on parameters
# NB2: Like the LNR, for the DDM there is no need to fix a parameter (by
#      default moment-to-moment variability s=1 is assumed, see rtdists). 
#      Constants are set here just to focus on the computationally faster 
#      version with no non-decision variability (st0=0) and with no difference
#      in non-decision time between responses (d=0). When st0>0 it gets slow!
# NB3: Because there is only one accumulator the M and R factors cannot
#      appear in p.map
# NB4: Negative v is mapped to the error boundary and positive to correct 
#      boundary, so estimates are generally positive.
# NB5: Note that the left response corresponds to the bottom boundary (at 0) 
#      and the right response to the top boundary (at a)

# Simple no factor effects model
model <- model.dmc(constants=c(st0=0,d=0),type="rd",
                   p.map=list(a="1",v="1",z="1",d="1",
                              sz="1",sv="1",t0="1",st0="1"),
                   match.map=list(M=list(left="LEFT",right="RIGHT")),
                   factors=list(S=c("left","right")),
                   responses=c("LEFT","RIGHT"))

# 1) In this example accuracy is a bit above 75%
p.vector  <- c(a=2,v=1,z=0.5,sv=1,sz=0.2,t0=.2)
data.model <- data.model.dmc(simulate.dmc(p.vector,model,n=1e4),model)
plot.score.dmc (data.model)

# 2) Decreasing the threshold (a) decreases accuracy but increases speed
p.vector  <- c(a=1,v=1,z=0.5,sv=1,sz=0.2,t0=.2)
data.model <- data.model.dmc(simulate.dmc(p.vector,model,n=1e4),model)
plot.score.dmc (data.model)

# 3) Increasing start-point noise (sz) also decreases accuracy and increases 
# speed (NOTE: sz must always be < a/2, otherwise an error occurs)
p.vector  <- c(a=2,v=1,z=0.5,sv=1,sz=.9,t0=.2)
data.model <- data.model.dmc(simulate.dmc(p.vector,model,n=1e4),model)
plot.score.dmc (data.model)

# 4) Decreasing v decreases accuracy and speed 
# (mainly for correct responses in this case) 
p.vector  <- c(a=2,v=0.5,z=0.5,sv=1,sz=0.2,t0=.2)
data.model <- data.model.dmc(simulate.dmc(p.vector,model,n=1e4),model)
plot.score.dmc (data.model)

# 5) Increasing sv mainly decreases accuracy (also a little RT)  
p.vector  <- c(a=2,v=1,z=0.5,sv=1.5,sz=0.2,t0=.2)
data.model <- data.model.dmc(simulate.dmc(p.vector,model,n=1e4),model)
plot.score.dmc (data.model)

# 6) Increasing t0 decreases speed but has no effect on accuracy  
p.vector  <- c(a=2,v=1,z=0.5,sv=1,sz=0.2,t0=.4)
data.model <- data.model.dmc(simulate.dmc(p.vector,model,n=1e4),model)
plot.score.dmc (data.model)


# Now lets look at the effects of non-decision noise, so st0 is no longer a 
# constant. Just as in LBA t0 is the lower bound of non-decision time and 
# t0+st0 is the upper bound (so mean non-decision time is t0 + st0/2). 
model <- model.dmc(constants=c(d=0),type="rd",
                   p.map=list(a="1",v="1",z="1",d="1",
                              sz="1",sv="1",t0="1",st0="S"),
                   match.map=list(M=list(left="LEFT",right="RIGHT")),
                   factors=list(S=c("left","right")),
                   responses=c("LEFT","RIGHT"))
# Parameter vector names are: ( see attr(,"p.vector") )
# [1] "a"       "v"       "z"       "sz"      "sv"      "t0"      "st0.left" 
# [8] "st0.right"

# Accuracy is unaffected by variability increases.
p.vector  <- c(a=2,v=1,z=0.5,sv=1,sz=0.2,t0=0.1,st0.left=0,st0.right=1)
data.model <- data.model.dmc(simulate.dmc(p.vector,model,n=1e4),model)
# Score & plot
correct <- data.model$S==tolower(data.model$R)
# No effect on accuracy
round(tapply(correct,data.model$S,mean),2)                        
# Mean RT is increased by st0/2 
round(tapply(data.model$RT,data.model$S,mean),2)   
# Variability for left stimuli less than for right
round(tapply(data.model$RT,data.model$S,IQR),2)   
par(mfrow=c(1,2))
plot.cell.density(data.model[data.model$S=="left",],C=correct[data.model$S=="left"],
                  xlim=c(0,5),main="left")
plot.cell.density(data.model[data.model$S=="right",],C=correct[data.model$S=="right"],
                  xlim=c(0,5),main="right")
