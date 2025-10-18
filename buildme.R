# buildme.R

# A script to build your package

# set working directory to package root
setwd("/cloud/project")

# Unload your package and uninstall it first.
unloadNamespace("teamATE_solarPanel_tool"); remove.packages("teamATE_solarPanel_tool")

# Auto-document your package, turning roxygen comments into manuals in the `/man` folder
devtools::document(".")
# Load your package temporarily!
devtools::load_all(".")

# Test out our functions
teamATE_solarPanel_tool::plus_one(x = 1) #CHANGE THIS--------

# When finished, remember to unload the package
unloadNamespace("teamATE_solarPanel_tool")

# Then, when ready, document, unload, build, and install the package!
# For speedy build, use binary = FALSE and vignettes = FALSE
devtools::document("."); # document the package
unloadNamespace("teamATE_solarPanel_tool"); # unload the package

# Build the package
devtools::build(pkg = ".", path = getwd(), binary = FALSE, vignettes = FALSE)


# Restart R
rstudioapi::restartSession()

# Install your package from a local build file
# such as 
# install.packages("nameofyourpackagefile.tar.gz", type = "source")
# or in our case:
install.packages("demotool_1.0.tar.gz", type = "source") #CHANGE THIS--------


# Load your package!
library("teamATE_solarPanel_tool")


# When finished, remember to unload the package
unloadNamespace("teamATE_solarPanel_tool"); remove.packages("teamATE_solarPanel_tool")

# Always a good idea to clear your environment and cache
rm(list = ls()); gc()
