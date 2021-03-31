# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package('bs4Dash')
usethis::use_package('bslib')
usethis::use_package('dplyr')
usethis::use_package('data.table')
usethis::use_package('ggalluvial')
usethis::use_package('ggplot2')
usethis::use_package('ggrepel')
usethis::use_package('janitor')
usethis::use_package('lmtest')
usethis::use_package('magrittr')
usethis::use_package('maps')
usethis::use_package('readr')
usethis::use_package('readxl')
usethis::use_package('sandwich')
usethis::use_package('shinipsum')
usethis::use_package('stringr')
usethis::use_package('stringi')
usethis::use_package('tidyfast')
usethis::use_package('tibble')
usethis::use_package('treemapify')
usethis::use_pipe()

## Add modules ----
modules <- c(
  '01_welcome',
  '02_selection',
  '03_demographics',
  '04_experience',
  '05_attachment',
  '06_return',
  '07_senseofus',
  '08_compare',
  '09_acknowledgement'
)

## Create a module infrastructure in R/
lapply(modules, golem::add_module)

## Add helper functions ----
## Creates ftc_* and utils_*
lapply(modules,function(x){golem::add_fct('helpers', module = x, open = FALSE)})
golem::add_utils('helpers')

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file('script')
golem::add_js_handler('handlers')
golem::add_css_file('custom')

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = 'survey_data', open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test('app')
usethis::use_coverage()

# Documentation

## Vignette ----
usethis::use_vignette('DiasporaSurveyResults')
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile('dev/03_deploy.R')

