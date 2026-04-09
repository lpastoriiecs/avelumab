library(shiny)
library(bslib)
library(waiter)
library(ggplot2)
library(shinyWidgets)
library(shinyFeedback)
library(markdown)
source("login.R")
source("global.R")
source("db.R")
source("server_code.R")
source("ui_code.R")



shinyApp(server = server, ui = ui)
