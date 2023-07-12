try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))

##############
## Roxygen2 ##
##############
library(roxygen2)
roxygenize(package.dir = ".")

##############
## testthat ##
##############
library(testthat)
test_check("SMChelpR", path = "./tests")

###################
## build install ##
###################
setwd("..")
system("R CMD build SMChelpR --resave-data")

# verify current version
system("R CMD check SMChelpR_0.0-6.tar.gz --as-cran")

###############################################
## Install locally to test for hidden errors ##
###############################################
devtools::install("SMChelpR")


#########################
## Install from github ##
#########################
setwd("./SMChelpR")
readRenviron(".env")
devtools::install_github("sciencemediacenter/SMChelpR", ref = "master", auth_token = Sys.getenv("token"))
library(SMChelpR)

