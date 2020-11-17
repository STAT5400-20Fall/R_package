setwd("~/Dropbox/Teaching/STAT 5400/5400_20F/Lecture Notes/Tech Guide/R Package/ridgereg/")
library(devtools)
document()

setwd("~/Dropbox/Teaching/STAT 5400/5400_20F/Lecture Notes/Tech Guide/R Package/")
system("R CMD build ridgereg")
system("R CMD INSTALL ridgereg_0.0.1.tar.gz")

system("R CMD check ridgereg_0.0.1.tar.gz")

system("R CMD check --as-cran ridgereg_0.0.1.tar.gz")
