#setwd(dir = "~/Documents/GitHub/MyPackages/ViolineHills/")

library(devtools)
library(roxygen2)
# initial package creation
#package.skeleton(name="ViolineHills", code_files="GraphicFunctions.R")

# to up-date
load_all("ViolineHills/")

# to document the manual
document(pkg = "ViolineHills/")
roxygenise("ViolineHills/", clean = TRUE)

build(pkg = "ViolineHills", manual = TRUE)
check("ViolineHills", manual = TRUE )

# Then RUN
# R CMD build ViolineHills 
#system(command = "R CMD build ViolineHills")
# And maybe
#system(command = "R CMD check ViolineHills")

# Then install
install(pkg = "ViolineHills/")
library(ViolineHills)

#install.packages("~/Documents/GitHub/MyPackages/ViolineHills/ViolineHills_1.0.tar.gz")
plotdensities

install_github("timotheenivalis/ViolineHills/ViolineHills")
library(ViolineHills)

