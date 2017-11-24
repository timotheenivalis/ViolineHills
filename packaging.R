setwd(dir = "~/Documents/GitHub/MyPackages/ViolineHills/")

library(devtools)
library(roxygen2)
# initial package creation
package.skeleton(name="ViolineHills", code_files="GraphicFunctions.R")

# to up-date
load_all("ViolineHills/")

# to document the manual
document(pkg = "ViolineHills/")
roxygenise("ViolineHills/", clean = TRUE)

# Then RUN
# R CMD build ViolineHills 
# And maybe
#  R CMD check ViolineHills 

# Then install
install.packages("ViolineHills_1.0.tar.gz")
