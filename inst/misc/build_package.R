#----------------------------------------------------
# Build and Tests
#----------------------------------------------------

library(usethis)
library(testthat)
library(rhub)
library(devtools)
library(qpdf)
library(kableExtra)
library(testthat)

# Loading unfinished package to memory...
rm(list = ls())
devtools::load_all()
devtools::document()
devtools::test()  # Run tests - all passed Sept 11 2026
devtools::check() # Operating system test

remove.packages("ashgswuw")
# install.packages(getwd(), repos = NULL, type = "source")
# devtools::install_github("mattjbayly/ashgswuw")

library(ashgswuw)
