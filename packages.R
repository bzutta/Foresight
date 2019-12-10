################### begin load package ################
# Cargando todos los paquetes a usar, en el caso de no estar instalado
# el paquete automaticamente se va a instalar.
# ref https://www.rdocumentation.org/packages/bs4Dash/versions/0.2.0
packages <- c("bs4Dash", "curl", "dismo", "dplyr", "DT", "fasterize", 
              "foreign", "ggplot2", "httr", "jsonlite", "leaflet", 
              "leaflet.extras", "lubridate", "maptools", "raster", 
              "rasterVis", "RColorBrewer", "RCurl", "readr", "rgdal", 
              "rgeos", "sf", "shiny", "shinydashboard", "shinythemes", 
              "tidyverse","filesstrings","shinyWidgets","shinyalert",
              "rJava")
source("functions.R")

ipak(packages)
################### end load package ################
#####################################################
