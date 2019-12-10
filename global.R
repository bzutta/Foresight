#########################################################
################# begin maxent parameters ###############
# Sys.setenv(NOAWT=TRUE)
# memory.limit(memory.limit() * 2^30)
# set.tempdir("data/tmp/corriendo/")
# se indica el path de java, tener en cuenta que esto
# cambia segun el ordenador a usar.


Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_202/')

# Opcion par aindicar cuantos GB va a usar java en
# el proceso de la ejecucion
options(java.parameters = "-Xmx2g")

# path maxent bim
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')


mxnt.args<-c(
    # "nowarnings=false",
    # "environmentallayers=data/tmp/entrada",
    "projectionlayers=data/tmp/proyeccion",
    # "samplesfile=data/tmp/csv/samples.csv",
    # "outputdirectory=data/tmp/output",
    "replicatetype=bootstrap",
    "randomtestpoints=60", ## ojo con el random points
    "skipifexists=true",
    "randomseed=true",
    "quadratic=true",
    "hinge=true",
    "linear=true",
    "plots=false", # ojo con este punto
    "product=true",
    "pictures=true",
    "threshold=true",
    "jackknife=true",
    "outputfiletype=asc",
    "responsecurves=true",
    "outputformat=logistic"
)
################# end maxent parameters #################
#########################################################

########################################################
################# begin directory path #################

# variables_datos <- c(
#     "Distancia Areas Naturales Protegidas"= "anp",
#     "Distancia a No Bosques del 2000"= "nb00",
#     "Distancia Comunidades Nativas"= "ccnn",
#     "Distancia a Centros Problados"= "ccpp",
#     "Distancia Agricultura del 2000"= "agri00",
#     "Distancia a Áreas Mineras"= "min00",
#     "Distancia a Pastizales"= "past00",
#     "Mapa de Elevación"= "elev",
#     "Mapa de Pendiente"= "slope",
#     "Distancia Red vial"= "vias",
#     "Distancia a Rios"= "hidro")
################### en directory path ##################
########################################################


########################################################################
########################################################################

########################################################################
########################################################################
################### begin variables ################

load("G:/My Drive/Working/USAID/MaxEntApp/data/input/shp/shapeFiles.RData") #shapeFiles

load("G:/My Drive/Working/USAID/MaxEntApp/data/input/csv/puntos.RData") #puntos


################### end variables #################



################### variables generales #################
# input images
# img(src = "rstudio.png", height = 70, width = 200),

# read CSV data
# deforesta_1_100m_ANUAL <- read_csv("data/csv/anual/deforesta_1_100m_ANUAL.csv")
# View(deforesta_1_100m_ANUAL)
# min_deforesta_1_100m_ANUAL <- min(deforesta_1_100m_ANUAL$RASTERVALU)
# max_deforesta_1_100m_ANUAL <- max(deforesta_1_100m_ANUAL$RASTERVALU)
# View(deforesta_1_100m_ANUAL)



# fecha <- 1

# filtro <- which(deforesta_1_100m_ANUAL$RASTERVALU == fecha)
# View(filtro)
# se extrae solo el year elegido
# filtrado <- deforesta_1_100m_ANUAL[filtro]#,1:4]
# View(filtrado)


# View(deforesta_1_100m_ANUAL)



#############################################################################
#############################################################################
# Referencias extras
# http://lab.fengxiao.info/2016/11/23/ENM-in-R-workshop.html
#############################################################################
#############################################################################


# maxentResults <- read.csv("data/output/maxentResults.csv")
