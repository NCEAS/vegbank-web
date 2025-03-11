library(shiny)
library(bslib)
library(magrittr)
library(markdown)
library(DT)
library(leaflet)
library(htmltools)
library(jsonlite)

# Define custom theme and common rules
custom_theme <- bs_theme(
  bg = "#FFFFFF",
  fg = "#19201d",
  info = "#2c5443",
  primary = "#72b9a2",
  secondary = "#d8fb5a",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
) %>%
  bs_add_rules(
    ".card-header { background-color: #2c5443; color: #FFFFFF; font-weight: bold; }
     .navbar-brand { color: #2c5443 !important; font-weight: bold; }
     .navbar-brand img { height: 30px; margin-right: 10px; }"
  )

source("ui.R")
source("server.R")
shinyApp(ui, server, enableBookmarking = "url")
