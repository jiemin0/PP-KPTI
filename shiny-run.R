
# This script is used to run the application defined in app.R in the background
library(shiny)
setwd("~/Work/Project/Patients Profile/Code/Shiny-PatientProfile")
options(shiny.autoreload = TRUE)
shiny::runApp()
getwd()

