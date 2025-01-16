# This is the script that will be used for the MCDA procedures

# Installing relevant packages
install.packages("librarian")
librarian::shelf(tidyverse,
                 dplyr,
                 ggpubr,
                 rstudioapi,
                 readr,
                 devtools,
                 data.tree,
                 update_all = FALSE)


#installing and loading the ahp package from github
devtools::install_github("gluc/ahp", build_vignettes = F, force = T)
library(ahp)


# Setting the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# select the AHP file 
criteria_ahp <- ahp::Load("x.yml")

# ahpFile <- system.file("extdata", "AHP/criteria.ahp.yml", package="ahp")
# cropAhp <- Load(ahpFile)




Calculate(criteria_ahp)

print(criteria_ahp, "weight")
print(criteria_ahp, priority = function(x) x$parent$priority["Total", x$name])


Visualize(criteria_ahp)
Analyze(criteria_ahp)

