# System functions for the DMC (Dynamic Models of Choice)
#    Usually user does not need to edit
#
# This script loads in all required files for DMC
#

require(msm)  # For truncated normal priors 
require(coda) # Sampling output analysis
require(loo) # For WAIC and looaic calculation
require(hypergeo) # For population plausible values
require(statmod) # Wald model
require(pracma)  # For gng and stop signal robust integration
require(numDeriv) # Prior transformations
require(vioplot) # Stop signal graphs
if(try(require(ggplot2))) require(ggplot2) else cat(
'No ggplot2, ggplot.RP.dmc and ggplot.RT.dmc will not work')  #Extra plots

# Pull in the file utilities so load_model can be called from dmc_hierarchical 
temp_wd <- getwd (); setwd(file.path (temp_wd, "dmc")) 

source ("file_utils.R")

# Load in all the DMC modules
source ("dmc_model.R")
source ("dmc_sampling.R")
source ("dmc_hierarchical.R")
source ("dmc_plotting.R")
source ("dmc_analysis.R")

setwd(temp_wd)


