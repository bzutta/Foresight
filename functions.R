# https://gist.github.com/stevenworthington/3178163
# ipak function: install and load multiple R packages.
# check to see if packages are installed. 
# Install them if they are not, then load them into the R session.
# ipak
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

subs <- function(x, n=1,m=0){
    substr(x, nchar(x)-n-m+1, nchar(x)-m)
}

fun.crop <- function(dato,vector){
    ext <- extent(vector)
    CROP <- crop(dato,ext)
    data_FIN <- mask(CROP,vector)
}

UTM2GEO <- function(data){
    data_geo <- spTransform(data, CRS("+proj=longlat +datum=WGS84"))
    return(data_geo)
}

GEO2UTM <- function(data){
    data_utm <- spTransform(data, CRS("+proj=utm +datum=WGS84"))
    return(data_utm)
}

fun.plot<-function(data,title){ 
    cuts<-seq(0,1,0.1)
    col.regions<-c("#006837", "#31a354", "#78c679", "#addd8e", "#d9f0a3", 
                   "#ffffcc","#ffffb2", "#fed976", "#feb24c", "#fd8d3c", 
                   "#f03b20")
    spplot(as(data, 'SpatialGridDataFrame'),
           at=cuts,
           col.regions=col.regions,
           colorkey=list(labels=list(at=cuts),at=cuts), 
           pretty=T,
           scales=list(draw=T),
           main=title)
}


pre.table <- function(area,variable_input){
    maxentResults <- read.csv("data/tmp/descargar/csv/maxentResults.csv")
    nombre <- c("Distancia Areas Naturales Protegidas", 
                "Distancia a No Bosques del 2000", 
                "Distancia Comunidades Nativas", 
                "Distancia a Centros Problados", 
                "Distancia Agricultura del 2000", 
                "Distancia a Áreas Mineras", 
                "Distancia a Pastizales", 
                "Mapa de Elevación", 
                "Mapa de Pendiente", 
                "Distancia Red vial", 
                "Distancia a Rios")
    nom <- c("anp", 
             "nb00", 
             "ccnn", 
             "ccpp", 
             "agri00", 
             "min00", 
             "past00", 
             "elev", 
             "slope",
             "vias", 
             "hidro")
    
    vari <- cbind(nombre,nom)
    nombre_contribution <- paste0("dist_",variable_input,
                                  "_",area,".contribution")
    nombre_permutation <- paste0("dist_",variable_input,
                                 "_",area,".permutation.importance")
    contri <- match(nombre_contribution,names(maxentResults))
    permut <- match(nombre_permutation,names(maxentResults))
    tabla_final <- cbind(t(maxentResults[3,na.exclude(contri)]),
                         t(maxentResults[3,na.exclude(permut)])
    )
    nombrefila<-rownames(tabla_final)
    patron<-substr(nombrefila,1,(nchar(nombrefila)-19))
    patron <- substr(patron,6,nchar(patron))
    
    index <- NULL
    for(i in 1:length(patron)){
        tmo <- which(vari[,2]==patron[i])
        index <- c(index,tmo)
    }
    
    rownames(tabla_final) <- vari[index,1]
    colnames(tabla_final) <- c("Percent Contribution",
                               "Permutation Importance")
    
    tabla_final <- tabla_final[order(tabla_final[,1],decreasing = T),]
    return(as.data.frame(tabla_final))
}





# permite descargar la data directa de punto de incendios ...
# se usa solo los datos de 7 dias para contar con mas puntos.
download_fire <- function(){
    root <-  "https://firms.modaps.eosdis.nasa.gov/data/active_fire/c6/csv/"
    #fire24 <- paste0("MODIS_C6_South_America_",24,"h.csv")
    #fire48 <- paste0("MODIS_C6_South_America_",48,"h.csv")
    fire07 <- paste0("MODIS_C6_South_America_",7,"d.csv")
    
    url_fire <- paste0(root,fire07)
    fire <- read.csv(url_fire)
    nombre <- paste0("data/csv/fire/dato_",fire[1,6],".csv")
    if(!file.exists(nombre)){
        write.csv(fire, nombre)
    }
    return(fire)
}

clean_pts <- function(pts, year){
    year <- sort(year)
    occ <- NULL
    for(i in year){ 
        tmp <- pts[which(pts[4] == i), 1:3]
        occ <- rbind(occ,tmp)
    }
    write.csv(occ,"data/tmp/csv/samples.csv", row.names = F)
}

zip2file <- function(dirZip='descargar/'){
    setwd("data/tmp/")
    mover <- c("output/plots/Deforestacion_proyeccion_avg.png",
               "output/plots/Deforestacion_proyeccion_max.png",
               "output/plots/Deforestacion_proyeccion_median.png",
               "output/plots/Deforestacion_proyeccion_min.png",
               "output/plots/Deforestacion_proyeccion_stddev.png",
               "output/plots/Deforestacion_roc.png",
               "output/plots/Deforestacion_stddev.png",
               "output/maxentResults.csv")
    file.move(mover,"descargar")
    
    files2zip <- dir(dirZip, full.names = TRUE)
    if(length(files2zip)!=0){
        zip(zipfile = 'OutPut_MaxEnt', files = files2zip)
    }
    
    setwd(workdir)
}

regresar.datos <- function(){
    regresando_input <- list.files("data/tmp/entrada/",
                                   pattern = "*.asc",
                                   full.names = T)
    file.move(regresando_input,"data/asc/input/")
    
    regresando_project <- list.files("data/tmp/proyeccion/",
                                     pattern = "*.asc",
                                     full.names = T)
    file.move(regresando_project, "data/asc/project/")
}

limpiar <- function(){
    unlink("data/tmp/proyeccion/maxent.cache",recursive = T)
    file.remove("data/tmp/csv/samples.csv")
    
    salida_raster <- list.files("data/tmp/output/", 
                                pattern = "species_proyeccion_avg.asc", 
                                full.names = T)
    file.copy(salida_raster,
              "data/tmp/descargar/asc/",
              overwrite = T)
    
    salida_csv <- "data/tmp/output/maxentResults.csv"
    
    
    file.copy(salida_csv,
              "data/tmp/descargar/csv/",
              overwrite = T)
    # unlink("data/tmp/output", recursive = T,force = T)
    
}

dir.plot <-  function(){
    path_plots <- "data/tmp/output/plots"
    if (!file.exists(path_plots)) {
        path <- trim(path_plots)
        dir.create(path, recursive=TRUE, showWarnings=FALSE)
    }
}

runMaxEnt <- function(interacciones){
    # get predictor variables
    fnames <- list.files("data/tmp/entrada/", pattern = "*.asc",full.names=TRUE )
    predictors <- stack(fnames)
    
    # file with presence points
    occ <- read.table("data/tmp/csv/samples.csv", header=TRUE, sep=',')[,-1]
    
    mxnt.args[length(mxnt.args)+1] <- paste0("replicates=",interacciones)
    
    maxent(predictors, #datos de ingreso
           occ,  #puntos 
           path = "data/tmp/output",
           args = mxnt.args#argumentos input, esta en global.R
    )
}
# 
# resultado <- function(){
#     maxentResults <- read.csv("data/tmp/output/maxentResults.csv")
# }

# ################################################################################
# src/org/bccvl/compute/rscripts/brt.R# set CRAN mirror in case we need to download something
# # TODO: this should be done on demand or on user basis...
# r <- getOption("repos")
# r["CRAN"] <- "http://cran.ms.unimelb.edu.au/"
# options(repos=r)
# # TODO: alse creating and populating add on package location is something that should not be done system wide
# 
# #script to run to develop distribution models
# ###check if libraries are installed, install if necessary and then load them
# necessary=c("dismo","SDMTools", "rgdal", "pROC", "R2HTML", "png") #list the libraries needed
# installed = necessary %in% installed.packages() #check if library is installed
# if (length(necessary[!installed]) >=1) {
#     install.packages(necessary[!installed], dep = T) #if library is not installed, install it
# }
# for (lib in necessary) {
#     library(lib,character.only=T) #load the libraries
# }
# 
# ###read in the necessary observation, background and environmental data
# #setwd(wd) #set the working directory
# populate.data = TRUE #variable to define if there is a need to generate occur & background environmental info
# if (populate.data) {
#     occur = read.csv(occur.data) #read in the observation data lon/lat
#     if (!is.null(bkgd.data)) {
#         bkgd = read.csv(bkgd.data) #read in teh background position data lon.lat
#     }
#     for (ii in 1:length(enviro.data.current)) {
#         cat(ii,'of',length(enviro.data.current),'\n') #cycle through each of the environmental datasets and append the data
#         #tasc = read.asc(enviro.data.current[ii]) #read in the envirodata
#         tasc = readGDAL(enviro.data.current[ii]) #read in the envirodata
#         occur[,enviro.data.names[ii]] = extract.data(cbind(occur$lon,occur$lat),tasc) #extract envirodata for observations
#         if (!is.null(bkgd.data)) bkgd[,enviro.data.names[ii]] = extract.data(cbind(bkgd$lon,bkgd$lat),tasc) #extract envirodata for background data
#     }
# }
# 
# current.climate.scenario = stack(enviro.data.current)
# if (is.null(enviro.data.future)) {
#     project.brt=FALSE
# } else {
#     future.climate.scenario = stack(enviro.data.future)
# }
# 
# ###run the models and store models
# #############################################################################################
# #
# # MACHINE LEARNING METHODS - use both presence and absence or background data: Maxent, BRT
# #
# #############################################################################################
# 
# ###############
# #
# # MAXENT
# #
# ###############
# 
# # maxent is being run as a system call with the same data. Arguments include:
# # table below is "Flag -- Type -- Default -- Description"
# # responsecurves -- boolean -- FALSE -- Create graphs showing how predicted relative probability of occurrence depends on the value of each environmental variable
# # pictures -- boolean -- TRUE -- Create a .png image for each output grid
# # jackknife -- boolean -- FALSE -- Measure importance of each environmental variable by training with each environmental variable first omitted, then used in isolation
# # outputformat -- string -- logistic -- Representation of probabilities used in writing output grids. See Help for details
# # outputfiletype -- string -- asc -- File format used for writing output grids
# # outputdirectory -- directory --  -- Directory where outputs will be written. This should be different from the environmental layers directory.
# # projectionlayers -- file/directory --  -- Location of an alternate set of environmental variables. Maxent models will be projected onto these variables.
# # --  --  -- Can be a .csv file (in SWD format) or a directory containing one file per variable.
# # --  --  -- Multiple projection files/directories can be separated by commas.
# # samplesfile -- file --  -- Please enter the name of a file containing presence locations for one or more species.
# # environmentallayers -- file/directory --  -- Environmental variables can be in a directory containing one file per variable,
# # --  --  -- or all together in a .csv file in SWD format. Please enter a directory name or file name.
# # randomseed -- boolean -- FALSE -- If selected, a different random seed will be used for each run, so a different random test/train partition
# # --  --  -- will be made and a different random subset of the background will be used, if applicable.
# # logscale -- boolean -- TRUE -- If selected, all pictures of models will use a logarithmic scale for color-coding.
# # warnings -- boolean -- TRUE -- Pop up windows to warn about potential problems with input data.
# # --  --  -- Regardless of this setting, warnings are always printed to the log file.
# # tooltips -- boolean -- TRUE -- Show messages that explain various parts of the interface, like this message
# # askoverwrite -- boolean -- TRUE -- If output files already exist for a species being modeled,
# # --  --  -- pop up a window asking whether to overwrite or skip. Default is to overwrite.
# # skipifexists -- boolean -- FALSE -- If output files already exist for a species being modeled,
# # --  --  -- skip the species without remaking the model.
# # removeduplicates -- boolean -- TRUE -- Remove duplicate presence records.
# # --  --  -- If environmental data are in grids, duplicates are records in the same grid cell.
# # --  --  -- Otherwise, duplicates are records with identical coordinates.
# # writeclampgrid -- boolean -- TRUE -- Write a grid that shows the spatial distribution of clamping.
# # --  --  -- At each point, the value is the absolute difference between prediction values with and without clamping.
# # writemess -- boolean -- TRUE -- A multidimensional environmental similarity surface (MESS) shows where novel climate conditions exist in the projection layers.
# # --  --  -- The analysis shows both the degree of novelness and the variable that is most out of range at each point.
# # randomtestpoints -- integer -- 0 -- Percentage of presence localities to be randomly set aside as test points, used to compute AUC, omission etc.
# # betamultiplier -- double -- 1 -- Multiply all automatic regularization parameters by this number. A higher number gives a more spread-out distribution.
# # maximumbackground -- integer -- 10000 -- If the number of background points / grid cells is larger than this number, then this number of cells is chosen randomly for background points
# # biasfile -- file --  -- Sampling is assumed to be biased according to the sampling distribution given in this grid file.
# # --  --  -- Values in this file must not be zero or negative. MaxEnt will factor out the bias.
# # --  --  -- Requires environmental data to be in grids, rather than a SWD format file
# # testsamplesfile -- file --  -- Use the presence localities in this file to compute statistics (AUC, omission etc.)
# # --  --  -- The file can contain different localities for different species.
# # --  --  -- It takes precedence over the random test percentage.
# # replicates -- integer -- 1 -- Number of replicate runs to do when cross-validating, bootstrapping or doing sampling with replacement runs
# # replicatetype -- string -- crossvalidate -- If replicates > 1, do multiple runs of this type:
# # --  --  -- Crossvalidate: samples divided into replicates folds; each fold in turn used for test data.
# # --  --  -- Bootstrap: replicate sample sets chosen by sampling with replacement.
# # --  --  -- Subsample: replicate sample sets chosen by removing random test percentage without replacement to be used for evaluation.
# # perspeciesresults -- boolean -- FALSE -- Write separate maxentResults file for each species
# # writebackgroundpredictions -- boolean -- FALSE -- Write .csv file with predictions at background points
# # responsecurvesexponent -- boolean -- FALSE -- Instead of showing the logistic value for the y axis in response curves, show the exponent (a linear combination of features)
# # linear -- boolean -- TRUE -- Allow linear features to be used
# # quadratic -- boolean -- TRUE -- Allow quadratic features to be used
# # product -- boolean -- TRUE -- Allow product features to be used
# # threshold -- boolean -- TRUE -- Allow threshold features to be used
# # hinge -- boolean -- TRUE -- Allow hinge features to be used
# # addsamplestobackground -- boolean -- TRUE -- Add to the background any sample for which has a combination of environmental values that isn't already present in the background
# # addallsamplestobackground -- boolean -- FALSE -- Add all samples to the background, even if they have combinations of environmental values that are already present in the background
# # autorun -- boolean -- FALSE -- Start running as soon as the the program starts up
# # writeplotdata -- boolean -- FALSE -- Write output files containing the data used to make response curves, for import into external plotting software
# # fadebyclamping -- boolean -- FALSE -- Reduce prediction at each point in projections by the difference between
# # --  --  -- clamped and non-clamped output at that point
# # extrapolate -- boolean -- TRUE -- Predict to regions of environmental space outside the limits encountered during training
# # visible -- boolean -- TRUE -- Make the Maxent user interface visible
# # autofeature -- boolean -- TRUE -- Automatically select which feature classes to use, based on number of training samples
# # doclamp -- boolean -- TRUE -- Apply clamping when projecting
# # outputgrids -- boolean -- TRUE -- Write output grids. Turning this off when doing replicate runs causes only the summary grids (average, std deviation etc.) to be written, not those for the individual runs.
# # plots -- boolean -- TRUE -- Write various plots for inclusion in .html output
# # appendtoresultsfile -- boolean -- FALSE -- If false, maxentResults.csv file is reinitialized before each run
# # maximumiterations -- integer -- 500 -- Stop training after this many iterations of the optimization algorithm
# # convergencethreshold -- double -- 1.00E-05 -- Stop training when the drop in log loss per iteration drops below this number
# # adjustsampleradius -- integer -- 0 -- Add this number of pixels to the radius of white/purple dots for samples on pictures of predictions.
# # --  --  -- Negative values reduce size of dots.
# # threads -- integer -- 1 -- Number of processor threads to use. Matching this number to the number of cores on your computer speeds up some operations, especially variable jackknifing.
# # lq2lqptthreshold -- integer -- 80 -- Number of samples at which product and threshold features start being used
# # l2lqthreshold -- integer -- 10 -- Number of samples at which quadratic features start being used
# # hingethreshold -- integer -- 15 -- Number of samples at which hinge features start being used
# # beta_threshold -- double -- -1 -- Regularization parameter to be applied to all threshold features; negative value enables automatic setting
# # beta_categorical -- double -- -1 -- Regularization parameter to be applied to all categorical features; negative value enables automatic setting
# # beta_lqp -- double -- -1 -- Regularization parameter to be applied to all linear, quadratic and product features; negative value enables automatic setting
# # beta_hinge -- double -- -1 -- Regularization parameter to be applied to all hinge features; negative value enables automatic setting
# # logfile -- string -- maxent.log -- File name to be used for writing debugging information about a run in output directory
# # cache -- boolean -- TRUE -- Make a .mxe cached version of ascii files, for faster access
# # defaultprevalence -- double -- 0.5 -- Default prevalence of the species: probability of presence at ordinary occurrence points.
# # --  --  -- See Elith et al., Diversity and Distributions, 2011 for details.
# # applythresholdrule -- string --  -- Apply a threshold rule, generating a binary output grid in addition to the regular prediction grid. Use the full name of the threshold rule in Maxent's html output as the argument. For example, 'applyThresholdRule=Fixed cumulative value 1'.
# # togglelayertype -- string --  -- Toggle continuous/categorical for environmental layers whose names begin with this prefix (default: all continuous)
# # togglespeciesselected -- string --  -- Toggle selection of species whose names begin with this prefix (default: all selected)
# # togglelayerselected -- string --  -- Toggle selection of environmental layers whose names begin with this prefix (default: all selected)
# # verbose -- boolean -- FALSE -- Gived detailed diagnostics for debugging
# # allowpartialdata -- boolean -- FALSE -- During model training, allow use of samples that have nodata values for one or more environmental variables.
# # prefixes -- boolean -- TRUE -- When toggling samples or layers selected or layer types, allow toggle string to be a prefix rather than an exact match.
# # nodata -- integer -- -9999 -- Value to be interpreted as nodata values in SWD sample data
# 
# if (model.maxent) {
#     outdir = paste(wd,'output_maxent/',sep=''); dir.create(outdir,recursive=TRUE); #create the output directory
#     write.csv(data.frame(species=species,occur),paste(outdir,"occur.csv",sep=''),row.names=FALSE)### create occur.csv for maxent
#     write.csv(data.frame(species="bkgd",bkgd),paste(outdir,"bkgd.csv",sep=''),row.names=FALSE)### create bkgd.csv for maxent
#     ###not user modified section
#     tstr = paste('java -mx2048m -jar ',maxent.jar,' ',sep='') #start the maxent string
#     tstr = paste(tstr,'environmentallayers=',outdir,'bkgd.csv ',sep='')
#     tstr = paste(tstr,'samplesfile=',outdir,'occur.csv ',sep='')
#     tstr = paste(tstr,'outputdirectory=',outdir,' ',sep='')
#     tstr = paste(tstr,'autorun=TRUE visible=FALSE warnings=FALSE tooltips=FALSE ',sep='')
#     tstr = paste(tstr,'askoverwrite=FALSE skipifexists=FALSE prefixes=TRUE verbose=FALSE ',sep='')
#     tstr = paste(tstr,'responsecurves=TRUE pictures=TRUE jackknife=TRUE writeclampgrid=TRUE ',sep='')
#     tstr = paste(tstr,'writemess=TRUE writebackgroundpredictions=TRUE writeplotdata=FALSE outputgrids=TRUE ',sep='')
#     tstr = paste(tstr,'plots=TRUE appendtoresultsfile=FALSE threads=1 adjustsampleradius=0 ',sep='')
#     tstr = paste(tstr,'logfile=maxent.log cache=FALSE allowpartialdata=FALSE outputfiletype="asc" ',sep='')
#     tstr = paste(tstr,'perspeciesresults=FALSE responsecurvesexponent=FALSE	dontcache nocache ',sep='')
#     if (any(enviro.data.type!='continuous')){
#         catvals = which(enviro.data.type!='continuous')
#         for (ii in catvals) {
#             tstr = paste(tstr,'togglelayertype=',enviro.data.names[ii],' ',sep='') #toggle the layer type
#         }
#     }
#     ### based on user modified
#     tstr = paste(tstr,'outputformat=',maxent.outputformat,' ',sep='')
#     tstr = paste(tstr,'randomseed=',maxent.randomseed,' ',sep='')
#     tstr = paste(tstr,'logscale=',maxent.logscale,' ',sep='')
#     tstr = paste(tstr,'removeduplicates=',maxent.removeduplicates,' ',sep='')
#     tstr = paste(tstr,'randomtestpoints=',maxent.randomtestpoints,' ',sep='')
#     tstr = paste(tstr,'betamultiplier=',maxent.betamultiplier,' ',sep='')
#     tstr = paste(tstr,'maximumbackground=',maxent.maximumbackground,' ',sep='')
#     tstr = paste(tstr,'biasfile=',maxent.biasfile,' ',sep='')
#     tstr = paste(tstr,'testsamplesfile=',maxent.testsamplesfile,' ',sep='')
#     tstr = paste(tstr,'replicates=',maxent.replicates,' ',sep='')
#     tstr = paste(tstr,'replicatetype=',maxent.replicatetype,' ',sep='')
#     tstr = paste(tstr,'linear=',maxent.linear,' ',sep='')
#     tstr = paste(tstr,'quadratic=',maxent.quadratic,' ',sep='')
#     tstr = paste(tstr,'product=',maxent.product,' ',sep='')
#     tstr = paste(tstr,'threshold=',maxent.threshold,' ',sep='')
#     tstr = paste(tstr,'hinge=',maxent.hinge,' ',sep='')
#     tstr = paste(tstr,'addsamplestobackground=',maxent.addsamplestobackground,' ',sep='')
#     tstr = paste(tstr,'addallsamplestobackground=',maxent.addallsamplestobackground,' ',sep='')
#     tstr = paste(tstr,'fadebyclamping=',maxent.fadebyclamping,' ',sep='')
#     tstr = paste(tstr,'extrapolate=',maxent.extrapolate,' ',sep='')
#     tstr = paste(tstr,'autofeature=',maxent.autofeature,' ',sep='')
#     tstr = paste(tstr,'doclamp=',maxent.doclamp,' ',sep='')
#     tstr = paste(tstr,'maximumiterations=',maxent.maximumiterations,' ',sep='')
#     tstr = paste(tstr,'convergencethreshold=',maxent.convergencethreshold,' ',sep='')
#     tstr = paste(tstr,'lq2lqptthreshold=',maxent.lq2lqptthreshold,' ',sep='')
#     tstr = paste(tstr,'l2lqthreshold=',maxent.l2lqthreshold,' ',sep='')
#     tstr = paste(tstr,'hingethreshold=',maxent.hingethreshold,' ',sep='')
#     tstr = paste(tstr,'beta_threshold=',maxent.beta_threshold,' ',sep='')
#     tstr = paste(tstr,'beta_categorical=',maxent.beta_categorical,' ',sep='')
#     tstr = paste(tstr,'beta_lqp=',maxent.beta_lqp,' ',sep='')
#     tstr = paste(tstr,'beta_hinge=',maxent.beta_hinge,' ',sep='')
#     tstr = paste(tstr,'defaultprevalence=',maxent.defaultprevalence,' ',sep='')
#     tstr = paste(tstr,'nodata=',maxent.nodata,' ',sep='')
#     system(tstr)	
# }
# 
# ###############
# #
# # predict(object, x, ext=NULL, filename="", progress='text', ...)
# #
# # object A fitted model of class Bioclim, Domain, MaxEnt, ConvexHull, or Mahalanobis (classes that inherit from DistModel)
# # x A Raster* object or a data.frame
# # ext An extent object to limit the prediction to a sub-region of 'x'. Or an object that can be coerced to an Extent object by extent; such as a Raster* or Spatial* object
# # filename Output filename for a new raster; if NA the result is not written to a file but returned with the RasterLayer object, in the data slot
# # progress Character. Valid values are "" (no progress bar), "text" and "windows" (on that platform only)
# # ... Additional model specific arguments. And additional arguments for file writing as for writeRaster
# #
# # For maxent models, there is an additional argument 'args' used to pass arguments (options) to the maxent software.
# # For bioclim models, there is an additional argument 'tails' which you can use to ignore the left or right tail of the percentile distribution for a variable.
# # For geoDist models, there is an additional argument fun that allows you to use your own (inverse) distance function, and argument scale=1 that allows you to scale
# # the values (distances smaller than this value become one, and the others are divided by this value before computing the inverse distance).
# # For spatial predictions with BRT, randomForest, etc., see 'predict' in the Raster package
# #
# ###############
# 
# if (project.maxent) {
#     
#     #*************** UNDER CONSTRUCTION ***************
#     # maxent model creation was run as a system call outside of R, need to do the same for projection
#     # EMG check to see if argument defaults / modifiables are the same as during creation
#     
#     # create output directory
#     model.dir = paste(wd, "output_maxent/", sep="")
#     
#     ### not user modified section
#     tstr = paste("java -cp ", maxent.jar, " density.Project ", model.dir, species, ".lambdas ", sep="")
#     # where to find the climate scenarios
#     tstr = paste(tstr, dirname(enviro.data[1]), " ", sep="")
#     # where to put, what to name the output
#     tstr = paste(tstr, model.dir, es.name, ".asc", sep="")
#     # optional arguments
#     tstr = paste(tstr, " nowriteclampgrid nowritemess fadebyclamping dontcache", sep="")
#     system(tstr)
#     
#     # EMG cache=FALSE nocache and dontcache all manage to be ignored and a maxent.cache is created
#     # EMG 'outputfiletype' = asc, mxe, grd, bil only NOT geotiff; can create *.png ('pictures=TRUE')
# }
# 
# 
# ######################################################################################
# # model accuracy helpers
# ######################################################################################
# 
# # function to save evaluate output
# saveModelEvaluation = function(out.model, out.biomod.model, model.name) {
#     model.dir = paste(wd, "/output_", model.name, "/", sep="")
#     save(out.model, file=paste(model.dir, "dismo.eval.object.RData", sep=''))	# save the 'dismo::ModelEvalution' object
#     
#     # save all the model accuracy statistics provided in both dismo and biomod2
#     rownames(out.biomod.model) <- c("Testing.data","Cutoff","Sensitivity", "Specificity")
#     write.csv(t(round(out.biomod.model, digits=3)), file=paste(model.dir, "combined.modelEvaluation.csv", sep=""))
#     # EMG no guarantee these value are correct
#     
#     # save AUROC curve
#     
#     png(file=paste(model.dir, "AUC.png", sep=''));
#     plot(out.model, 'ROC');
#     dev.off()
# }
# 
# my.Find.Optim.Stat <- function(Stat='TSS',Fit,Obs,Precision = 5, Fixed.thresh = NULL){
#     if(length(unique(Obs)) == 1 | length(unique(Fit)) == 1){
#         # warning("\nObserved or fited data contains only a value.. Evaluation Methods switched off\n",immediate.=T)
#         # best.stat <- cutoff <- true.pos <- sensibility <- true.neg <- specificity <- NA
#         warning("\nObserved or fited data contains a unique value.. Be carefull with this models predictions\n",immediate.=T)
#         #best.stat <- cutoff <- true.pos <- sensibility <- true.neg <- specificity <- NA
#     } #else {
#     if(Stat != 'ROC'){
#         StatOptimum <- my.getStatOptimValue(Stat)
#         if(is.null(Fixed.thresh)){ # test a range of threshold to get the one giving the best score
#             if(length(unique(Fit)) == 1){
#                 valToTest <- unique(Fit)
#                 valToTest <- round(c(mean(c(0,valToTest)), mean(c(1000,valToTest))))
#             } else{
#                 mini <- max(min(quantile(Fit,0.05, na.rm=T), na.rm=T),0)
#                 maxi <- min(max(quantile(Fit,0.95, na.rm=T), na.rm=T),1000)
#                 # valToTest <- unique( round(c(seq(mini,maxi,length.out=100), mini, maxi)) )
#                 # EMG no idea why the round() is here, it makes vals between 0 and 1 (ie bioclim) all 0
#                 valToTest <- unique( c(seq(mini,maxi,length.out=100)))
#                 # deal with unique value to test case
#                 if(length(valToTest)<3){
#                     valToTest <- round(c(mean(0,mini), valToTest, mean(1000,maxi)))
#                 }
#             }
#             # valToTest <- unique( c(seq(mini,maxi,by=Precision), mini, maxi) )
#         } else{
#             valToTest <- Fixed.thresh
#         }
#         
#         calcStat <- sapply(lapply(valToTest, function(x){return(table(Fit>x,Obs))} ), my.calculate.stat, stat=Stat)
#         
#         # scal on 0-1 ladder.. 1 is the best
#         calcStat <- 1 - abs(StatOptimum - calcStat)
#         
#         best.stat <- max(calcStat, na.rm=T)
#         
#         cutoff <- median(valToTest[which(calcStat==best.stat)]) # if several values are selected
#         
#         misc <- table(Fit >= cutoff, Obs)
#         misc <- .contagency.table.check(misc)
#         true.pos <- misc['TRUE','1']
#         true.neg <- misc['FALSE','0']
#         specificity <- (true.neg * 100)/sum(misc[,'0'])
#         sensibility <- (true.pos * 100)/sum(misc[,'1'])
#     } else{
#         roc1 <- roc(Obs, Fit, percent=T)
#         roc1.out <- coords(roc1, "best", ret=c("threshold", "sens", "spec"))
#         best.stat <- as.numeric(auc(roc1))/100
#         cutoff <- as.numeric(roc1.out["threshold"])
#         sensibility <- as.numeric(roc1.out["sensitivity"])
#         specificity <- as.numeric(roc1.out["specificity"])
#     }
#     #}
#     return(cbind(best.stat,cutoff,sensibility,specificity))
# }
# 
# my.getStatOptimValue <- function(stat){
#     if(stat == 'TSS') return(1)
#     if(stat == 'KAPPA') return(1)
#     if(stat == 'ACCURACY') return(1)
#     if(stat == 'BIAS') return(1)
#     if(stat == 'POD') return(1)
#     if(stat == 'FAR') return(0)
#     if(stat == 'POFD') return(0)
#     if(stat == 'SR') return(1)
#     if(stat == 'CSI') return(1)
#     if(stat == 'ETS') return(1)
#     if(stat == 'HK') return(1)
#     if(stat == 'HSS') return(1)
#     if(stat == 'OR') return(1000000)
#     if(stat == 'ORSS') return(1)
#     
#     #dismo
#     if(stat == 'ODP') return(1)
#     # if(stat == 'CCR') return(1) # same as ACCURACY
#     # if(stat == 'TPR') return(1) # same as POD
#     if(stat == 'TNR') return(1)
#     if(stat == 'FPR') return(0)
#     if(stat == 'FNR') return(0)
#     # if(stat == 'PPP') return(1) # same as SR
#     if(stat == 'NPP') return(1)
#     if(stat == 'MCR') return(0)
#     if(stat == 'OR') return(1000000)
#     # if(stat == 'kappa') return(1) # same as KAPPA
# }
# 
# my.calculate.stat <-
#     function(Misc, stat='TSS') {
#         # Contagency table checking
#         Misc <- .contagency.table.check(Misc)
#         
#         # Defining Classification index
#         hits <- Misc['TRUE','1']
#         misses <- Misc['FALSE','1']
#         false_alarms <- Misc['TRUE','0']
#         correct_negatives <- Misc['FALSE','0']
#         
#         total <- sum(Misc)
#         forecast_1 <- sum(Misc['TRUE',])
#         forecast_0 <- sum(Misc['FALSE',])
#         observed_1 <- sum(Misc[,'1'])
#         observed_0 <- sum(Misc[,'0'])
#         
#         # Calculating choosen evaluating metric
#         if(stat=='TSS'){
#             return( (hits/(hits+misses)) + (correct_negatives/(false_alarms+correct_negatives)) -1 )
#         }
#         
#         if(stat=='KAPPA'){
#             Po <- (1/total) * (hits + correct_negatives)
#             Pe <- ((1/total)^2) * ((forecast_1 * observed_1) + (forecast_0 * observed_0))
#             return( (Po - Pe) / (1-Pe) )
#         }
#         
#         if(stat=='ACCURACY'){
#             return( (hits + correct_negatives) / total)
#         }
#         
#         if(stat=='BIAS'){
#             return( (hits + false_alarms) / (hits + misses))
#         }
#         
#         if(stat=='POD'){
#             return( hits / (hits + misses))
#         }
#         
#         if(stat=='FAR'){
#             return(false_alarms/(hits+false_alarms))
#         }
#         
#         if(stat=='POFD'){
#             return(false_alarms / (correct_negatives + false_alarms))
#         }
#         
#         if(stat=='SR'){
#             return(hits / (hits + false_alarms))
#         }
#         
#         if(stat=='CSI'){
#             return(hits/(hits+misses+false_alarms))
#         }
#         
#         if(stat=='ETS'){
#             hits_rand <- ((hits+misses)*(hits+false_alarms)) / total
#             return( (hits-hits_rand) / (hits+misses+false_alarms-hits_rand))
#         }
#         
#         # if(stat=='HK'){
#         # return((hits/(hits+misses)) - (false_alarms/(false_alarms + correct_negatives)))
#         # }
#         
#         # if(stat=='HSS'){
#         # expected_correct_rand <- (1/total) * ( ((hits+misses)*(hits+false_alarms)) +
#         # ((correct_negatives + misses)*(correct_negatives+false_alarms)) )
#         # return((hits+correct_negatives-expected_correct_rand) / (total - expected_correct_rand))
#         # }
#         
#         # if(stat=='OR'){
#         # return((hits*correct_negatives)/(misses*false_alarms))
#         # }
#         
#         # if(stat=='ORSS'){
#         # return((hits*correct_negatives - misses*false_alarms) / (hits*correct_negatives + misses*false_alarms))
#         # }
#         
#         # if(stat=="BOYCE"){
#         #
#         # }
#         
#         #dismo
#         if(stat=='ODP'){
#             return((false_alarms + correct_negatives) / total)
#         }
#         
#         # if(stat=='CCR'){
#         # return((hits + correct_negatives) / total)
#         # }
#         
#         # if(stat=='TPR'){
#         # return(hits / (hits + misses))
#         # }
#         
#         if(stat=='TNR'){
#             return(correct_negatives / (false_alarms + correct_negatives))
#         }
#         
#         if(stat=='FPR'){
#             return(false_alarms / (false_alarms + correct_negatives))
#         }
#         
#         if(stat=='FNR'){
#             return(misses / (hits + misses))
#         }
#         
#         # if(stat=='PPP'){
#         # return(hits / (hits + false_alarms))
#         # }
#         
#         if(stat=='NPP'){
#             return(correct_negatives / (misses + correct_negatives))
#         }
#         
#         if(stat=='MCR'){
#             return((false_alarms + misses) / total)
#         }
#         
#         if(stat=='OR'){
#             return((hits * correct_negatives) / (misses * false_alarms))
#         }
#         
#         # if(stat=='kappa'){
#         # return(((hits + correct_negatives) - (((hits + misses)*(hits + false_alarms) + (false_alarms + correct_negatives)*(misses + correct_negatives)) / total)) /
#         # (total -(((hits + misses)*(hits + false_alarms) + (false_alarms + correct_negatives)*(misses + correct_negatives)) / total)))
#         # }
#     }
# 
# .contagency.table.check <- function(Misc){
#     # Contagency table checking
#     if(dim(Misc)[1]==1){
#         if(row.names(Misc)[1]=="FALSE"){
#             Misc <- rbind(Misc, c(0,0))
#             rownames(Misc) <- c('FALSE','TRUE')
#         } else{
#             a <- Misc
#             Misc <- c(0,0)
#             Misc <- rbind(Misc, a)
#             rownames(Misc) <- c('FALSE','TRUE')
#         }
#     }
#     
#     if(ncol(Misc) != 2 | nrow(Misc) !=2 ){
#         Misc = matrix(0, ncol=2, nrow=2, dimnames=list(c('FALSE','TRUE'), c('0','1')))
#     }
#     
#     if((sum(colnames(Misc) %in% c('FALSE','TRUE','0','1')) < 2) | (sum(rownames(Misc) %in% c('FALSE','TRUE','0','1')) < 2) ){
#         stop("Unavailable contagency table given")
#     }
#     
#     if('0' %in% rownames(Misc)) rownames(Misc)[which(rownames(Misc)=='0')] <- 'FALSE'
#     if('1' %in% rownames(Misc)) rownames(Misc)[which(rownames(Misc)=='1')] <- 'TRUE'
#     
#     return(Misc)
# }
# 
# 
# ###############
# #
# # evaluate(p, a, model, x, tr, ...)
# #
# # p presence points (x and y coordinate or SpatialPoints* object)
# # Or, if x is missing, values at presence points (EMG: values returned by a predict())
# # Or, a matrix with values to compute predictions for
# # a absence points (x and y coordinate or SpatialPoints* object)
# # Or, if x is missing, values at absence points (EMG: values returned by a predict())
# # Or, a matrix with values to compute predictions for
# # model any fitted model, including objects inheriting from 'DistModel'; not used when x is missing
# # x Optional. Predictor values (object of class Raster*). If present, p and a are interpreted
# # as (spatial) points (EMG: lon/lat)
# # tr Optional. a vector of threshold values to use for computing the confusion matrices
# # ... Additional arguments for the predict function (EMG: evaluate() calls predict())
# #
# # 'ModelEvaluation' output based on Fielding and Bell (1997) with attributes:
# # presence - presence data used
# # absence - absence data used
# # np - number of presence points
# # na - number of absence points
# # auc - Area under the receiver operator (ROC) curve
# # pauc - p-value for the AUC (for the Wilcoxon test W statistic
# # cor - Correlation coefficient
# # pcor - p-value for correlation coefficient
# # t - vector of thresholds used to compute confusion matrices
# # confusion - confusion matrices
# # prevalence - Prevalence
# # ODP - Overall diagnostic power
# # CCR - Correct classification rate
# # TPR - True positive rate
# # TNR - True negative rate
# # FPR - False positive rate
# # FNR - False negative rate
# # PPP - Positive predictive power
# # NPP - Negative predictive power
# # MCR - Misclassification rate
# # OR - Odds-ratio
# # kappa - Cohen's kappa
# #
# ###############
# 
# # model accuracy statistics - combine stats from dismo and biomod2 for consistent output
# model.accuracy = c(dismo.eval.method, biomod.models.eval.meth)
# 
# ###evaluate the models and save the outputs
# if (evaluate.maxent) {
#     # read in the Maxent predictions at the presence and background points, and 
#     #	extract the columns we need
#     model.dir <- paste(wd, "output_maxent", sep=""); setwd(model.dir);
#     presence <- read.csv(paste(model.dir, "/", species, "_samplePredictions.csv", sep=""))
#     background <- read.csv(paste(model.dir, "/", species, "_backgroundPredictions.csv", sep=""))
#     log.presence <- presence$Logistic.prediction
#     log.absence <- background$logistic
#     maxent.eval.obj = dismoModelEvaluation(log.presence, log.absence) # use predictions to generate dismo-like model evaluation object
#     
#     # need predictions and observed values to create confusion matrices for accuracy statistics
#     maxent.fit = c(log.presence, log.absence)
#     maxent.obs = c(rep(1, length(log.presence)), rep(0, length(log.absence)))
#     
#     # get the model accuracy statistics using a modified version of biomod2's Evaluate.models.R
#     maxent.combined.eval = sapply(model.accuracy, function(x){
#         return(my.Find.Optim.Stat(Stat = x, Fit = maxent.fit, Obs = maxent.obs))
#     })
#     saveModelEvaluation(maxent.eval.obj, maxent.combined.eval)	# save output
#     rm(list=c("maxent.eval.obj", "maxent.combined.eval")); #clean up the memory
# }
# #########################################################################

# require(rgdal)
# 
# 
# fire <- download_fire()
# 
# # data_fire$nombre
# fire <- fire[,1:2]
# head(fire)
# 
# coordinates(fire) <- ~longitude+latitude
# proj4string(fire) <- CRS("+proj=longlat +datum=WGS84") 
# 
# plot(fire)
# 
# fire_crop <- fun.crop(as.Spatial,shapeFiles$all)
#  
#     plot(fire_crop)
#     plot(shapeFiles$all, add=T)
# proj4string(fire) <-   CRS("+proj=utm +zone=18 ellps=WGS84")
# 
# 
# class(fire)
# class(shapeFiles$all)
# proj4string(shapeFiles$all)
# proj4string(fire)
# head(fire)
# proj4string(fire)
# fire <-  as.data.frame(fire)
# head(fire)
# 
# coordinates(xy) <- c("X", "Y")
# proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
# as.data.frame(xy)
# res <- spTransform(xy, CRS("+proj=utm +zone=18 ellps=WGS84"))
# res
# 
# 
# 
# # base de datos.
# # global gee
# # global forest change
# # 
# # si hay o no perdida de vegetación 
# # capa de confiabilidad






# environmentallayers <- system.file("extdata",
#                                    package = "ntbox")
#maxent_path <- "~/Downloads/maxent"
#outputdirectory <- "~/Downloads/"
#occ_data_path <- system.file("extdata",
#                             "cardon_occs.csv",
#                             package = "ntbox")
#maxent_parm <- maxent_call(maxentjar_path = maxent_path,run_fromR=TRUE,
#                           environmentallayers = environmentallayers,
#                           samplesfile=occ_data_path,
#                           betamultiplier = .1,
#                           features=c("l","q","p"),
#                           outputdirectory = outputdirectory)
#print(maxent_parm)

# }

# maxent_call <- function(maxentjar_path,
#                         run_fromR=TRUE,
#                         features,
#                         memory_assigned = 2000,
#                         environmentallayers,
#                         samplesfile,
#                         testsamplesfile,
#                         outputdirectory=NULL,
#                         projectionlayers = NULL,
#                         responsecurves = FALSE,
#                         pictures =TRUE,
#                         jackknife = FALSE,
#                         outputformat= "cloglog",
#                         randomseed=FALSE,
#                         logscale = TRUE,
#                         warnings = TRUE,
#                         askoverwrite=FALSE,
#                         skipifexists=FALSE,
#                         removeduplicates=TRUE,
#                         writeclampgrid=FALSE,
#                         writemess =FALSE,
#                         randomtestpoints =0,
#                         betamultiplier=1,
#                         maximumbackground=10000,
#                         biasfile = "",
#                         replicates=NULL,
#                         replicatetype="crossvalidate",
#                         perspeciesresults=FALSE,
#                         writebackgroundpredictions=FALSE,
#                         responsecurvesexponent=FALSE,
#                         addsamplestobackground=TRUE,
#                         addallsamplestobackground=FALSE,
#                         autorun=TRUE,
#                         writeplotdata=FALSE,
#                         fadebyclamping=FALSE,
#                         extrapolate=FALSE,
#                         visible= FALSE,
#                         autofeature =FALSE,
#                         doclamp = FALSE,
#                         outputgrids =TRUE,
#                         plots =TRUE,
#                         appendtoresultsfile =FALSE,
#                         maximumiterations = 500,
#                         convergencethreshold = 1.0E-5,
#                         adjustsampleradius =0,
#                         threads =1,
#                         lq2lqptthreshold = 80,
#                         l2lqthreshold = 10,
#                         hingethreshold = 15,
#                         beta_threshold = -1.0,
#                         beta_categorical = -1.0,
#                         beta_lqp = -1.0,
#                         beta_hinge = -1.0,
#                         logfile = "maxent.log",
#                         cache = TRUE,
#                         defaultprevalence = 0.5,
#                         applythresholdrule=NULL ,
#                         togglelayertype =NULL,
#                         togglespeciesselected =NULL,
#                         togglelayerselected = NULL,
#                         verbose = FALSE,
#                         allowpartialdata = FALSE,
#                         prefixes =TRUE,
#                         nodata = -9999){
#     if(!is.list(features)) features <-list(features)
#     selected_features <- paste(unlist(lapply(features, .maxent_features)),collapse = " ")
#     #print(selected_features)
#     maxentpath <- normalizePath(file.path(maxentjar_path,
#                                           "maxent.jar"))
#     
#     if(.Platform$OS.type == "unix") {
#         sl <- "/"
#         dl <- "/"
#     } else {
#         sl <- "\\"
#         dl <- "\\\\"
#     }
#     env_name <- unlist(stringr::str_split(environmentallayers ,dl))
#     ifelse(nchar(env_name[length(env_name)])>0L,
#            env_name <- env_name[length(env_name)],
#            env_name <- env_name[length(env_name)-1])
#     samplesfile <- normalizePath(samplesfile)
#     if(is.null(outputdirectory) || !dir.exists(outputdirectory))
#         outputdirectory <- getwd()
#     f_act <- paste(unlist(features),collapse = "")
#     
#     outputdirectory <- file.path(outputdirectory,
#                                  paste0(env_name,
#                                         "_bmult_",
#                                         betamultiplier,"_",
#                                         f_act))
#     
#     lapply(outputdirectory, function(x){
#         if(!dir.exists(x))
#             dir.create(x)
#     })
#     outputdirectory <- normalizePath(outputdirectory)
#     maxent_call <-   paste0("java ","-mx",
#                             memory_assigned,"m -jar ",
#                             maxentpath," environmentallayers=",
#                             normalizePath(environmentallayers),
#                             " samplesfile=",samplesfile,
#                             " outputdirectory=",outputdirectory,
#                             ifelse(is.null(projectionlayers) || !dir.exists(projectionlayers),"",
#                                    paste0(" projectionlayers=",normalizePath(projectionlayers))),
#                             " ",selected_features,
#                             " maximumbackground=", maximumbackground,
#                             ifelse(responsecurves," responsecurves=true"," responsecurves=false"),
#                             ifelse(pictures," pictures=true"," pictures=false"),
#                             ifelse(jackknife," jackknife=true"," jackknife=false"),
#                             ifelse(!outputformat %in% c("raw","cloglog","logistic","cumulative"),
#                                    " outputformat=cloglog", paste0(" outputformat=",outputformat)),
#                             ifelse(randomseed," randomseed=true"," randomseed=false"),
#                             ifelse(logscale," logscale=true",
#                                    " logscale=false"),
#                             ifelse(warnings," warnings=true",
#                                    " warnings=false"),
#                             ifelse(askoverwrite," askoverwrite=true",
#                                    " askoverwrite=false"),
#                             ifelse(skipifexists," skipifexists=true",
#                                    " skipifexists=false"),
#                             ifelse(removeduplicates," removeduplicates=true",
#                                    " removeduplicates=false"),
#                             ifelse(writeclampgrid," writeclampgrid=true",
#                                    " writeclampgrid=false"),
#                             ifelse(writemess," writemess=true",
#                                    " writemess=false"),
#                             ifelse(is.numeric(randomtestpoints),
#                                    paste0(" randomtestpoints=",
#                                           randomtestpoints),""),
#                             ifelse(is.numeric(betamultiplier),
#                                    paste0(" betamultiplier=",
#                                           betamultiplier),""),
#                             ifelse(is.numeric(maximumbackground),
#                                    paste0(" maximumbackground=",
#                                           maximumbackground),""),
#                             ifelse(!file.exists(biasfile),"",
#                                    paste0(" biasfile=",biasfile)),
#                             ifelse(is.numeric(replicates),
#                                    paste0(" replicates=",
#                                           replicates),""),
#                             ifelse(replicatetype %in% c("crossvalidate",
#                                                         "bootstrap",
#                                                         "subsample"),
#                                    paste0(" replicatetype=",
#                                           replicatetype)," replicatetype=crossvalidate"),
#                             ifelse(is.logical(perspeciesresults) && isTRUE(perspeciesresults),
#                                    paste0(" perspeciesresults=","true")," perspeciesresults=false"),
#                             ifelse(is.logical(writebackgroundpredictions) && isTRUE(writebackgroundpredictions),
#                                    paste0(" writebackgroundpredictions=","true")," writebackgroundpredictions=false"),
#                             ifelse(is.logical(responsecurvesexponent) && isTRUE(responsecurvesexponent),
#                                    paste0(" responsecurvesexponent=","true")," responsecurvesexponent=false"),
#                             ifelse(is.logical(addsamplestobackground) && isFALSE(addsamplestobackground),
#                                    paste0(" addsamplestobackground=","false")," addsamplestobackground=true"),
#                             ifelse(is.logical(addallsamplestobackground) && isTRUE(addsamplestobackground),
#                                    paste0(" addallsamplestobackground=","true")," addallsamplestobackground=false"),
#                             ifelse(is.logical(writeplotdata) && isTRUE(writeplotdata),
#                                    paste0(" writeplotdata=","true")," writeplotdata=false"),
#                             ifelse(is.logical(fadebyclamping) && isTRUE(fadebyclamping),
#                                    paste0(" fadebyclamping=","true")," fadebyclamping=false"),
#                             ifelse(is.logical(extrapolate) && isTRUE(extrapolate),
#                                    paste0(" extrapolate=","true")," extrapolate=false"),
#                             ifelse(is.logical(visible) && isTRUE(visible),
#                                    paste0(" visible=","true")," visible=false"),
#                             ifelse(is.logical(autofeature) && isTRUE(autofeature),
#                                    paste0(" autofeature=","true")," autofeature=false"),
#                             ifelse(is.logical(doclamp) && isTRUE(doclamp),
#                                    paste0(" doclamp=","true")," doclamp=false"),
#                             ifelse(is.logical(outputgrids) && isTRUE(outputgrids),
#                                    paste0(" outputgrids=","true")," outputgrids=false"),
#                             ifelse(is.logical(plots) && isTRUE(plots),
#                                    paste0(" plots=","true")," plots=false"),
#                             ifelse(is.logical(appendtoresultsfile) && isTRUE(appendtoresultsfile),
#                                    paste0(" appendtoresultsfile=","true")," appendtoresultsfile=false"),
#                             ifelse(is.numeric(maximumiterations),
#                                    paste0(" maximumiterations=",
#                                           maximumiterations)," maximumiterations=500"),
#                             ifelse(is.numeric(convergencethreshold),
#                                    paste0(" convergencethreshold=",
#                                           toupper(convergencethreshold))," convergencethreshold=1.0E-5"),
#                             ifelse(is.numeric(adjustsampleradius),
#                                    paste0(" adjustsampleradius=",
#                                           adjustsampleradius)," adjustsampleradius=0"),
#                             ifelse(is.numeric(threads),
#                                    paste0(" threads=",
#                                           threads)," threads=1"),
#                             ifelse(lq2lqptthreshold > 0 && lq2lqptthreshold<= 100,
#                                    paste0(" lq2lqptthreshold=",
#                                           lq2lqptthreshold)," lq2lqptthreshold=80"),
#                             ifelse(lq2lqptthreshold >0 && lq2lqptthreshold<=100,
#                                    paste0(" l2lqthreshold=",
#                                           l2lqthreshold)," l2lqthreshold=10"),
#                             ifelse(hingethreshold>0 && hingethreshold<=100,
#                                    paste0(" hingethreshold=",
#                                           hingethreshold)," hingethreshold=15"),
#                             ifelse(beta_threshold < 0,
#                                    paste0(" beta_threshold=",
#                                           beta_threshold)," beta_threshold=-1.0"),
#                             ifelse(beta_categorical < 0,
#                                    paste0(" beta_categorical=",
#                                           beta_categorical)," beta_categorical=-1.0"),
#                             ifelse(beta_lqp < 0,
#                                    paste0(" beta_lqp=",
#                                           beta_lqp)," beta_lqp=-1.0"),
#                             ifelse(beta_hinge < 0,
#                                    paste0(" beta_hinge=",
#                                           beta_hinge)," beta_hinge=-1.0"),
#                             ifelse(is.character(logfile),
#                                    paste0(" logfile=",
#                                           logfile)," logfile=maxent.log"),
#                             ifelse(is.logical(cache) && isTRUE(cache),
#                                    paste0(" cache=","true")," cache=false"),
#                             ifelse(defaultprevalence > 0 && defaultprevalence<=1,
#                                    paste0(" defaultprevalence=",
#                                           defaultprevalence)," defaultprevalence=0.5"),
#                             ifelse(applythresholdrule %in% c("Fixed cumulative value 1",
#                                                              "Fixed cumulative value 5",
#                                                              "Fixed cumulative value 10",
#                                                              "Minimum training presence",
#                                                              "10 percentile training presence",
#                                                              "Equal training sensitivity and specificity",
#                                                              "Maximum training sensitivity plus specificity",
#                                                              "Equal test sensitivity and specificity",
#                                                              "Maximum test sensitivity plus specificity",
#                                                              "Equate entropy of thresholded and original distributions"),
#                                    paste0(" applythresholdrule=",
#                                           paste0("'",applythresholdrule,"'")),""),
#                             ifelse(is.character(togglelayertype),
#                                    paste0(" togglelayertype=",togglelayertype),""),
#                             ifelse(is.character(togglespeciesselected),
#                                    paste0(" togglespeciesselected=",togglespeciesselected),""),
#                             ifelse(is.character(togglelayerselected),
#                                    paste0(" togglelayerselected=",togglelayerselected),""),
#                             ifelse(is.logical(verbose) && isTRUE(verbose),
#                                    paste0(" verbose=","true")," verbose=false"),
#                             ifelse(is.logical(allowpartialdata) && isTRUE(allowpartialdata),
#                                    paste0(" allowpartialdata=","true")," allowpartialdata=false"),
#                             ifelse(is.logical(prefixes) && isTRUE(prefixes),
#                                    paste0(" prefixes=","true")," prefixes=false"),
#                             ifelse(!is.null(nodata),paste0(" nodata=",nodata)," nodata=-9999"),
#                             ifelse(is.logical(autorun) && isTRUE(autorun),
#                                    " autorun","")
#     )
#     if(run_fromR){
#         if(.Platform$OS.type == "unix")
#             system(command = maxent_call,intern = T,wait = T)
#         else
#             system2(command = maxent_call, wait = T, invisible = FALSE)
#     }
#     #system(command = maxent_call)
#     #cat(paste0("#!/bin/sh\n",maxent_call),file = "~/Dropbox/maxentcommand.sh")
#     
#     return(maxent_call)
#     
#     
# }
# 
# 
# .maxent_features <- function(x){
#     
#     test_0 <- c("l","linear",
#                 "q","quadratic",
#                 "p","product",
#                 "h","hinge",
#                 "t","threshold")
#     #if(length(test_0  %in% x)==0L | "autofeature" %in% x)
#     #  autofeature <- "autofeature=true"
#     #else autofeature <- "autofeature=false"
#     if("l"  %in% x | "linear" %in% x)
#         l <- 'linear=true'
#     else l <- 'linear=false'
#     if("q" %in% x | "quadratic" %in% x)
#         q <- 'quadratic=true'
#     else q <- 'quadratic=false'
#     if("p" %in% x | "product" %in% x)
#         p <- 'product=true'
#     else p <- 'product=false'
#     if("t"  %in% x | "threshold" %in% x)
#         t <- 'threshold=true'
#     else t <- 'threshold=false'
#     if("h"  %in% x | "hing" %in% x)
#         h <- 'hinge=true'
#     else h <- 'hinge=false'
#     return(c(l,q,p,t,h))
# }
##############################################################################
# maxent_call(maxentjar_path, run_fromR = TRUE, features,
#             memory_assigned = 2000, environmentallayers, samplesfile,
#             testsamplesfile, outputdirectory = NULL, projectionlayers = NULL,
#             responsecurves = FALSE, pictures = TRUE, jackknife = FALSE,
#             outputformat = "cloglog", randomseed = FALSE, logscale = TRUE,
#             warnings = TRUE, askoverwrite = FALSE, skipifexists = FALSE,
#             removeduplicates = TRUE, writeclampgrid = FALSE, writemess = FALSE,
#             randomtestpoints = 0, betamultiplier = 1,
#             maximumbackground = 10000, biasfile = "", replicates = NULL,
#             replicatetype = "crossvalidate", perspeciesresults = FALSE,
#             writebackgroundpredictions = FALSE, responsecurvesexponent = FALSE,
#             addsamplestobackground = TRUE, addallsamplestobackground = FALSE,
#             autorun = TRUE, writeplotdata = FALSE, fadebyclamping = FALSE,
#             extrapolate = FALSE, visible = FALSE, autofeature = FALSE,
#             doclamp = FALSE, outputgrids = TRUE, plots = TRUE,
#             appendtoresultsfile = FALSE, maximumiterations = 500,
#             convergencethreshold = 1e-05, adjustsampleradius = 0, threads = 1,
#             lq2lqptthreshold = 80, l2lqthreshold = 10, hingethreshold = 15,
#             beta_threshold = -1, beta_categorical = -1, beta_lqp = -1,
#             beta_hinge = -1, logfile = "maxent.log", cache = TRUE,
#             defaultprevalence = 0.5, applythresholdrule = NULL,
#             togglelayertype = NULL, togglespeciesselected = NULL,
#             togglelayerselected = NULL, verbose = FALSE,
#             allowpartialdata = FALSE, prefixes = TRUE, nodata = -9999)
##############################################################################
##############################################################################




