###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "map_city", with_test = FALSE) # Name of the module
golem::add_module(name = "", with_test = TRUE) # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("arrow_connect", with_test = TRUE)
golem::add_utils("collect_data", with_test = F)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_css_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
