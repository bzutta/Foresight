workdir <- "G:/My Drive/Working/USAID/src/MaxEntApp_ORG"
setwd(workdir)
# getwd()
source("global.R")
source("packages.R")

tmp_dir <- "D:/R/tmp/"
rasterOptions(tmpdir = tmp_dir)
# bootstrapPage(
    navbarPage(
        div(tags$b("Foresight"),
            # Foresight (Forest Loss and Wildfire Risk Assessment System)
            style = "color:#3474A7"), windowTitle = "MaxEnt APP",
        # useShinyalert(),
        ########################################################################
        ########################################################################
        ########################################################################
        ########################################################################
        ########################################################################
        ########################################################################
        # menu superior 01
        tabPanel("Deforestación",
                 ###############################################################
                 ###############################################################
                 sidebarLayout(
                     position = "right", #
                     sidebarPanel(width=5,
                                  # h1(inputId = "tit_gen" ,"Condiciones Iniciales",
                                     # align = "center", style = "color:#3474A7"),
                                  fluidRow(
                                      column(8,
                                             fluidRow(
                                                 column(7,
                                                        selectInput(inputId = "area_estudio",
                                                                    label = h4("Área de Estudio"),
                                                                    choices = list(
                                                                        # "Toda la Amazonia" = "all", #por falta de recursos
                                                                        "Amazonia Norte" = "Norte",
                                                                        "Amazonia Centro" = "Centro",
                                                                        "Amazonia Sur" = "Sur"),
                                                                    selected = "Sur"
                                                        )
                                                 ),
                                                 column(5,
                                                        sliderInput(inputId = "inter_deforesta",
                                                                    label = h4("Interacciones"),
                                                                    min = 2,
                                                                    max = 10,
                                                                    value = 5
                                                        )
                                                 )
                                             ),
                                             # area variable
                                             checkboxGroupInput(inputId = "variables",
                                                                label = h4("Seleccionar Variables"),
                                                                choices =c(
                                                                    "Distancia Areas Naturales Protegidas"= "anp",
                                                                    "Distancia a No Bosques del 2000"= "nb00",
                                                                    "Distancia Comunidades Nativas"= "ccnn",
                                                                    "Distancia a Centros Problados"= "ccpp",
                                                                    "Distancia Agricultura del 2000"= "agri00",
                                                                    "Distancia a Áreas Mineras"= "min00",
                                                                    "Distancia a Pastizales"= "past00",
                                                                    "Mapa de Elevación"= "elev",
                                                                    "Mapa de Pendiente"= "slope",
                                                                    "Distancia Red vial"= "vias",
                                                                    "Distancia a Rios"= "hidro"
                                                                    # "Distancia viasproy"= "viasproy",
                                                                    )
                                             )
                                             # actionLink("selectall","Seleccionar Todo"),
                                      ),
                                      column(4,
                                             checkboxGroupInput(inputId="year_input", label=h4("Años de Estudio"),
                                                                choices=c(
                                                                    "2001"="1",
                                                                    "2002"="2",
                                                                    "2003"="3",
                                                                    "2004"="4",
                                                                    "2005"="5",
                                                                    "2006"="6",
                                                                    "2007"="7",
                                                                    "2008"="8",
                                                                    "2009"="9",
                                                                    "2010"="10",
                                                                    "2011"="11",
                                                                    "2012"="12",
                                                                    "2013"="13",
                                                                    "2014"="14",
                                                                    "2015"="15",
                                                                    "2016"="16",
                                                                    "2017"="17"
                                                                )
                                             )
                                      )
                                  ),

                                  actionButton(inputId = "runbtn",
                                               label = h4(tags$b("Ejecutar"),
                                                          style = "color: red"),
                                               width = '120px',
                                               icon = icon("cog", "fa-3x")
                                  ),

                                  actionButton(inputId = "verrasterplot",
                                               label = h4(tags$b("Raster Plot"),
                                                          style = "color: blue"),
                                               width = '120px',
                                               icon = icon("globe-americas","fa-3x")
                                               # icon = icon("cloud-download-alt","fa-3x")
                                               ),
                                  actionButton(inputId = "vertabla",
                                               label = h4(tags$b("Ver Tabla"),
                                                          style = "color: blue"),
                                               width = '120px',
                                               icon = icon("table","fa-3x")
                                               # icon = icon("cloud-download-alt","fa-3x")
                                  ),
                                  actionButton(inputId = "descargar",
                                               label = h4(tags$b("Descargar"),
                                                          style = "color: green"),
                                               width = '120px',
                                               icon = icon("cloud-download-alt","fa-3x")
                                  )

                     ),
                     ###########################################################
                     # Show a plot of the generated distribution
                     mainPanel(width=7,
                               tags$head(tags$style("#myMap{height:90vh !important;}")),
                               leafletOutput("myMap"),
                               textOutput("ejecutarapp"),
                               plotOutput("verresultado"),
                               tableOutput("vertabla")
                               # textOutput("descargarsalida")
                     )
                 )
                 ###############################################################
                 ###############################################################
        ),

        ########################################################################
        ########################################################################
        ########################################################################
        ########################################################################
        ########################################################################
        ########################################################################
        # menu superior 02
        tabPanel("Incendios",
                 ###############################################################
                 ###############################################################
                 sidebarLayout(
                     position = "right", #
                     sidebarPanel(width=5,
                                  # h1(inputId = "tit_gen" ,"Condiciones Iniciales",
                                  # align = "center", style = "color:#3474A7"),
                                  fluidRow(
                                      column(8,
                                             fluidRow(
                                                 column(7,
                                                        selectInput(inputId = "area_estudioincen",
                                                                    label = h4("Área de Estudio"),
                                                                    choices = list(
                                                                        # "Toda la Amazonia" = "all", #por falta de recursos
                                                                        "Amazonia Norte" = "Norte",
                                                                        "Amazonia Centro" = "Centro",
                                                                        "Amazonia Sur" = "Sur"),
                                                                    selected = "Sur"
                                                        )
                                                 ),
                                                 column(5,
                                                        sliderInput(inputId = "inter_deforestaincen",
                                                                    label = h4("Interacciones"),
                                                                    min = 2,
                                                                    max = 10,
                                                                    value = 5
                                                        )
                                                 )
                                             ),
                                             # area variable
                                             checkboxGroupInput(inputId = "variablesincen",
                                                                label = h4("Seleccionar Variables"),
                                                                choices =c(
                                                                    "Distancia Areas Naturales Protegidas"= "anp",
                                                                    "Distancia a No Bosques del 2000"= "nb00",
                                                                    "Distancia Comunidades Nativas"= "ccnn",
                                                                    "Distancia a Centros Problados"= "ccpp",
                                                                    "Distancia Agricultura del 2000"= "agri00",
                                                                    "Distancia a Áreas Mineras"= "min00",
                                                                    "Distancia a Pastizales"= "past00",
                                                                    "Mapa de Elevación"= "elev",
                                                                    "Mapa de Pendiente"= "slope",
                                                                    "Distancia Red vial"= "vias",
                                                                    "Distancia a Rios"= "hidro"
                                                                    # "Distancia viasproy"= "viasproy",
                                                                )
                                             )
                                             # actionLink("selectall","Seleccionar Todo"),
                                      ),
                                      column(4,
                                             checkboxGroupInput(inputId="year_inputincen", label=h4("Años de Estudio"),
                                                                choices=c(
                                                                    "2001"="1",
                                                                    "2002"="2",
                                                                    "2003"="3",
                                                                    "2004"="4",
                                                                    "2005"="5",
                                                                    "2006"="6",
                                                                    "2007"="7",
                                                                    "2008"="8",
                                                                    "2009"="9",
                                                                    "2010"="10",
                                                                    "2011"="11",
                                                                    "2012"="12",
                                                                    "2013"="13",
                                                                    "2014"="14",
                                                                    "2015"="15",
                                                                    "2016"="16",
                                                                    "2017"="17",
                                                                    "Ultimos 7 días"="sietedias"
                                                                )
                                             )
                                      )
                                  ),

                                  actionButton(inputId = "runbtnincen",
                                               label = h4(tags$b("Ejecutar"),
                                                          style = "color: red"),
                                               width = '120px',
                                               icon = icon("cog", "fa-3x")
                                  ),

                                  actionButton(inputId = "verrasterplotincen",
                                               label = h4(tags$b("Raster Plot"),
                                                          style = "color: blue"),
                                               width = '120px',
                                               icon = icon("globe-americas","fa-3x")
                                               # icon = icon("cloud-download-alt","fa-3x")
                                  ),
                                  actionButton(inputId = "vertablaincen",
                                               label = h4(tags$b("Ver Tabla"),
                                                          style = "color: blue"),
                                               width = '120px',
                                               icon = icon("table","fa-3x")
                                               # icon = icon("cloud-download-alt","fa-3x")
                                  ),
                                  actionButton(inputId = "descargarincen",
                                               label = h4(tags$b("Ver Tabla"),
                                                          style = "color: green"),
                                               width = '120px',
                                               icon = icon("cloud-download-alt","fa-3x")
                                  )

                     ),
                     ###########################################################
                     # Show a plot of the generated distribution
                     mainPanel(width=7,
                               tags$head(tags$style("#myMapincen{height:90vh !important;}")),
                               leafletOutput("myMapincen"),
                               textOutput("ejecutarappincen"),
                               plotOutput("verresultadoincen"),
                               tableOutput("vertablaincen")
                               # textOutput("descargarsalida")
                     )
                 )


                 ###############################################################
                 ###############################################################
        ),
        ########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
######################################################################## # menu superior 03
        tabPanel("Alertas Tempranas",
                 ###############################################################
                 ###############################################################
                 sidebarLayout(
                     position = "right", #
                     sidebarPanel(width=5,
                                  # h1(inputId = "tit_gen" ,"Condiciones Iniciales",
                                  # align = "center", style = "color:#3474A7"),
                                  fluidRow(
                                      column(8,
                                             fluidRow(
                                                 column(7,
                                                        selectInput(inputId = "area_estudioalerta",
                                                                    label = h4("Área de Estudio"),
                                                                    choices = list(
                                                                        # "Toda la Amazonia" = "all", #por falta de recursos
                                                                        "Amazonia Norte" = "Norte",
                                                                        "Amazonia Centro" = "Centro",
                                                                        "Amazonia Sur" = "Sur"),
                                                                    selected = "Sur"
                                                        )
                                                 ),
                                                 column(5,
                                                        sliderInput(inputId = "inter_deforestaalerta",
                                                                    label = h4("Interacciones"),
                                                                    min = 2,
                                                                    max = 10,
                                                                    value = 5
                                                        )
                                                 )
                                             ),
                                             # area variable
                                             checkboxGroupInput(inputId = "variablesalerta",
                                                                label = h4("Seleccionar Variables"),
                                                                choices =c(
                                                                    "Distancia Areas Naturales Protegidas"= "anp",
                                                                    "Distancia a No Bosques del 2000"= "nb00",
                                                                    "Distancia Comunidades Nativas"= "ccnn",
                                                                    "Distancia a Centros Problados"= "ccpp",
                                                                    "Distancia Agricultura del 2000"= "agri00",
                                                                    "Distancia a Áreas Mineras"= "min00",
                                                                    "Distancia a Pastizales"= "past00",
                                                                    "Mapa de Elevación"= "elev",
                                                                    "Mapa de Pendiente"= "slope",
                                                                    "Distancia Red vial"= "vias",
                                                                    "Distancia a Rios"= "hidro"
                                                                    # "Distancia viasproy"= "viasproy",
                                                                )
                                             )
                                             # actionLink("selectall","Seleccionar Todo"),
                                      ),
                                      column(4,
                                             checkboxGroupInput(inputId="year_inputalerta", label=h4("Años de Estudio"),
                                                                choices=c(
                                                                    "01/18",
                                                                    "02/18",
                                                                    "03/18",
                                                                    "04/18",
                                                                    "05/18",
                                                                    "06/18",
                                                                    "07/18",
                                                                    "08/18",
                                                                    "09/18",
                                                                    "10/18",
                                                                    "11/18",
                                                                    "12/18",
                                                                    "01/19",
                                                                    "02/19",
                                                                    "03/19"
                                                                )
                                             )
                                      )
                                  ),

                                  actionButton(inputId = "runbtnalerta",
                                               label = h4(tags$b("Ejecutar"),
                                                          style = "color: red"),
                                               width = '120px',
                                               icon = icon("cog", "fa-3x")
                                  ),

                                  actionButton(inputId = "verrasterplotalerta",
                                               label = h4(tags$b("Raster Plot"),
                                                          style = "color: blue"),
                                               width = '120px',
                                               icon = icon("globe-americas","fa-3x")
                                               # icon = icon("cloud-download-alt","fa-3x")
                                  ),
                                  actionButton(inputId = "vertablaalerta",
                                               label = h4(tags$b("Ver Tabla"),
                                                          style = "color: blue"),
                                               width = '120px',
                                               icon = icon("table","fa-3x")
                                               # icon = icon("cloud-download-alt","fa-3x")
                                  ),
                                  actionButton(inputId = "descargaralerta",
                                               label = h4(tags$b("Ver Tabla"),
                                                          style = "color: green"),
                                               width = '120px',
                                               icon = icon("cloud-download-alt","fa-3x")
                                  )

                     ),
                     ###########################################################
                     # Show a plot of the generated distribution
                     mainPanel(width=7,
                               tags$head(tags$style("#myMapalerta{height:90vh !important;}")),
                               leafletOutput("myMapalerta"),
                               textOutput("ejecutarappalerta"),
                               plotOutput("verresultadoalerta"),
                               tableOutput("vertablaalerta")
                               # textOutput("descargarsalida")
                     )
                 )

        )
        ########################################################################
        ########################################################################
        ########################################################################
        ########################################################################
        ########################################################################
        ########################################################################
    )


# )


