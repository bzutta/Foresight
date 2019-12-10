shinyServer(function(input, output, session) {


    # output$myMap <- renderLeaflet({
    #     condi <- input$area_estudio
    #     if(condi == "Norte"){
    #         a <- leaflet(shapeFiles$norte)
    #     }
    #     if(condi == "Centro"){
    #         a <- leaflet(shapeFiles$centro)
    #     }
    #     if(condi == "Sur"){
    #         a <- leaflet(shapeFiles$sur)
    #     }
    #     a %>%
    #     addProviderTiles(providers$Esri.WorldImagery, group = "Satelite") %>%
    #     addProviderTiles(providers$CartoDB.Positron, group = "Mapa") %>%
    #     # addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Maptile") %>%
    #     setView(-75, -10, zoom = 5) %>%
    #     addTiles() %>% addPolygons() %>%
    #     addLayersControl(
    #         baseGroups = c("Mapa",
    #                        "Satelite"
    #                        # "Dark Maptile",
    #         ),
    #         options = layersControlOptions(collapsed = FALSE)
    #     )
    #
    # })
    
    output$myMap <- renderLeaflet({
        condi <- input$area_estudio
        if(condi == "Norte"){
            a <- leaflet(shapeFiles$norte)
        }
        if(condi == "Centro"){
            a <- leaflet(shapeFiles$centro)
        }
        if(condi == "Sur"){
            a <- leaflet(shapeFiles$sur)
        }
        a %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "Satelite") %>%
            addProviderTiles(providers$CartoDB.Positron, group = "Mapa") %>% 
            # addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Maptile") %>%
            setView(-75, -10, zoom = 5) %>% 
            addTiles() %>% addPolygons() %>%
            addLayersControl(
                baseGroups = c("Mapa", 
                               "Satelite"
                               # "Dark Maptile", 
                ),
                options = layersControlOptions(collapsed = FALSE)
            )
        
    })
    
    
    ejecutando <- eventReactive(input$runbtn, {
        variable_input <- input$variables
        yearinput <-  as.numeric(input$year_input)
        condi <- input$area_estudio
        
        
        if (length(variable_input)>0 && length(yearinput)>0) {
            # cpfile2tmp()
            
            if(condi == "Norte"){
                clean_pts(puntos$northP,yearinput)
                datos_input <- paste0("data/asc/input/dist_",
                                      variable_input,
                                      "_north.asc")
                datos_project <- paste0("data/asc/project/dist_",
                                        variable_input,
                                        "_north.asc")
            }
            if(condi == "Centro"){
                clean_pts(puntos$centerP,yearinput)
                datos_input <- paste0("data/asc/input/dist_",
                                      variable_input,
                                      "_center.asc")
                datos_project <- paste0("data/asc/project/dist_",
                                        variable_input,
                                        "_center.asc")
            }
            if(condi == "Sur"){
                clean_pts(puntos$southP,yearinput)
                datos_input <- paste0("data/asc/input/dist_",
                                      variable_input,
                                      "_south.asc")
                datos_project <- paste0("data/asc/project/dist_",
                                        variable_input,
                                        "_south.asc")
            }
            file.move(datos_input,
                      "data/tmp/entrada/")
            
            file.move(datos_project,
                      "data/tmp/proyeccion/")
            
            dir.plot()

            runMaxEnt(input$inter_deforesta)
            regresar.datos()
            limpiar()
            dir.plot()
            # print(paste(input$year_input, class(input$year_input)))
            # print(head(puntos$southP,50))
            print("Fin del proceso")
        }else{
            # shinyalert("Oops!", "Something went wrong.", type = "error")
            print("Es necesario seleccionar alguna variable y año")
        }
    })
    
    
    output$ejecutarapp <- renderText(
        ejecutando()
    )
    
    
    # output$verresultado <- renderPlot({
    #     input$verresultadobtn
    #     # Add a little noise to the cars data
    #     fin.plot <- raster("data/tmp/descargar/asc/species_proyeccion_avg.asc")
    #     plot(fin.plot)
    # })
    
    # 
    viendo <- eventReactive(input$verrasterplot, {
        areastu <- input$area_estudio
        fin.plot <- raster("data/tmp/descargar/asc/species_proyeccion_avg.asc")
        # fun.plot(fin.plot, paste("Área de Estudio",areastu))
        plot(fin.plot)
        # image(fin.plot)
    })
    

    output$verresultado <- renderPlot(
        viendo()
    )
    ############################
    tableando <- eventReactive(input$vertabla,{
        areastu <- input$area_estudio
        if(areastu == "Norte"){
                area <- "north"
        }
        if(areastu == "Centro"){
            area <- "center"
        }
        if(areastu == "Sur"){
            area <- "south"
        }
        
        tabla <- pre.table(area,input$variables)
    })
    output$vertabla <-  renderTable(
        tableando()
    )
    
    
    
    ########################################################################33
    ##########################################################################
    ##########################################################################
    output$myMapincen <- renderLeaflet({
        condi <- input$area_estudioincen
        if(condi == "Norte"){
            a <- leaflet(shapeFiles$norte)
        }
        if(condi == "Centro"){
            a <- leaflet(shapeFiles$centro)
        }
        if(condi == "Sur"){
            a <- leaflet(shapeFiles$sur)
        }
        a %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "Satelite") %>%
            addProviderTiles(providers$CartoDB.Positron, group = "Mapa") %>% 
            # addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Maptile") %>%
            setView(-75, -10, zoom = 5) %>% 
            addTiles() %>% addPolygons() %>%
            addLayersControl(
                baseGroups = c("Mapa", 
                               "Satelite"
                               # "Dark Maptile", 
                ),
                options = layersControlOptions(collapsed = FALSE)
            )
        
    })
    
    
    
    ########################################################################33
    ##########################################################################
    ##########################################################################
    
    
    ########################################################################33
    ##########################################################################
    ##########################################################################
    output$myMapalerta <- renderLeaflet({
        condi <- input$area_estudioalerta
        if(condi == "Norte"){
            a <- leaflet(shapeFiles$norte)
        }
        if(condi == "Centro"){
            a <- leaflet(shapeFiles$centro)
        }
        if(condi == "Sur"){
            a <- leaflet(shapeFiles$sur)
        }
        a %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "Satelite") %>%
            addProviderTiles(providers$CartoDB.Positron, group = "Mapa") %>% 
            # addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Maptile") %>%
            setView(-75, -10, zoom = 5) %>% 
            addTiles() %>% addPolygons() %>%
            addLayersControl(
                baseGroups = c("Mapa", 
                               "Satelite"
                               # "Dark Maptile", 
                ),
                options = layersControlOptions(collapsed = FALSE)
            )
        
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
})