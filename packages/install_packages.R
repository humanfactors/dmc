### Run this on first use of DMC to install requried packages. Run from RStudio
### in order to get help with dependencies.

# Standard packages from CRAN
install.packages("loo") # For WAIC and looaic calculation
install.packages("hypergeo") # For population plausible values
install.packages("statmod") # Wald model
install.packages("rtdists") # For standard model distirbution functions
install.packages("pracma")  # For gng and stop signal robust integration
install.packages("snowfall") # Parallel processing
install.packages("rlecuyer") # Parallel processing
install.packages("numDeriv") # Prior transformations
install.packages("vioplot") # Stop signal graphs
install.packages("ggplot2") # For fancy graphs

# Note: Before running commands scripts, the current working directory must be set 
# to the top-level folder containing the dmc and tutorial subfolders 

# Modifed packages distributed with DMC.

## Note that on some systems you will need to have admin rights to install 
## the followingl packages sucessfully and/or have your PATH set correctly. 
## It is also sometimes necessary to launch RStudio from an admin account.

# This modificaiton of coda allows for plotting of priors with plot.dmc
install.packages("packages/coda_0.19-2.tar.gz",repos=NULL,type="source")
## -- Installation Note
## On RStudio, you may use the "Install" button in the "Packages" tab.
## From its drop-down menu, choose "Package Archive File (.tar.gz)" to
## browser the location you store the source file "coda_0.19-2.tar.gz"
## Note that you may have to restart RStudio for the new version to take effect.