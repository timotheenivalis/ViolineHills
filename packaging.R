setwd(dir = "~/Documents/GitHub/MyPackages/ViolineHills/")

library(devtools)
library(roxygen2)
package.skeleton(name="ViolineHills", code_files="GraphicFunctions.R")

load_all("ViolineHills/")

