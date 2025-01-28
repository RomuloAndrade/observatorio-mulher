library(shiny)
library(leaflet)
library(sf)
library(dplyr)

bairros_CD2022 <- readRDS('Dados/bairros_CD2022.rds') 
bairros_CD2022  <-  mutate(bairros_CD2022,idd=row_number())
shp_Bairros <- readRDS('Dados/shp_Bairros.rds')

shp_Bairros_correc_nome <- shp_Bairros |> 
  mutate(Bairro = case_when(
    Bairro == "Boa Vista/Castelão" ~ "Boa Vista / Castelão",
    Bairro == "Dias Macêdo" ~ "Dias Macedo",
    Bairro == "Sapiranga/Coité" ~ "Sapiranga / Coité",    
    Bairro == "Tauape" ~ "São João do Tauape",
    TRUE ~ Bairro
  ))
  
  
  
bairros_demografia <- readRDS('Dados/bairros_demografia.rds') 

bairros_ <- bairros_demografia |> 
  filter(!is.na(CD_BAIRRO)) |> 
  select( CD_BAIRRO,
          NM_BAIRRO,
          V01006,
          V01007,
          V01008) |> 
  mutate(Razão = V01008/V01007*100,
         Percentual = round(V01008/V01006*100,2)) 

bairros_shp <- left_join(shp_Bairros_correc_nome,bairros_,by=c('Bairro'='NM_BAIRRO')) 
  
saveRDS(bairros_shp,'Dados/shp_bairros_demografia.rds')



leaflet() |>
  setMapWidgetStyle(list(background = "white"))  |> 
  addProviderTiles(providers$CartoDB.PositronNoLabels, group = "CartoDB.PositronNoLabels")  |> #CartoDB.Positron
  addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap")  |> 
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter")  |>
  addProviderTiles(providers$Esri.WorldImagery, group = "WorldImagery")  |>
  addLayersControl(
    baseGroups = c("CartoDB.Positron","OpenStreetMap",'CartoDB.DarkMatter','WorldImagery'),
    overlayGroups = c(
      "Bairros")
  ) |>
  hideGroup("Elementos do mapa")  |>
  hideGroup('Nomes dos bairros') |>
  addScaleBar(position = "bottomright",
              options = scaleBarOptions(imperial = F))  |>
  setView(-38.52782, -3.785804, zoom = 12) |>
  addFullscreenControl(pseudoFullscreen = T)|> 
  addPolygons(data=bairros_CD2022,
              weight=1,
              color='black',
              opacity = 0.5,
              fillColor = "white",
              popup =  ~NM_BAIRRO,
              label =  ~NM_BAIRRO,
              dashArray = "0",
              fillOpacity = 0,
              group = "Bairros"
  )  |>
  addMeasure(
    position = "topright",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    secondaryLengthUnit = "kilometers",
    activeColor = "#3D535D",
    completedColor = "#7D4479",
    localization = "pt_BR",
    captureZIndex = 10000
  ) 
######## shiny

{
  observeEvent(input$current_tab, { 
    
    output$map_bairro <-  renderLeaflet({
      leaflet() |>
        setMapWidgetStyle(list(background = "white"))  |> 
        addProviderTiles(providers$CartoDB.PositronNoLabels, group = "CartoDB.PositronNoLabels")|> #CartoDB.Positron
        # addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap")  |> 
        # addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter")  |>
        # addProviderTiles(providers$Esri.WorldImagery, group = "WorldImagery")  |>
        addLayersControl(
          # baseGroups = c("CartoDB.Positron","OpenStreetMap",'CartoDB.DarkMatter','WorldImagery'),
          overlayGroups = c(
            "Bairros",
            'id_final'
            # "Assentamento",
            # "Setores censitários"#,
            #'Nomes dos bairros')
          )) |>
        # hideGroup("Elementos do mapa")  |>
        # hideGroup('Nomes dos bairros') |>
        addScaleBar(position = "bottomright",
                    options = scaleBarOptions(imperial = F))  |>
        setView(-38.52782, -3.785804, zoom = 12) |>
        addFullscreenControl(pseudoFullscreen = T)#|> 
      # addPolygons(data=shp_Bairros,
      #             weight=1,
      #             color='black',
      #             opacity = 1,
      #             fillColor = "white",
      #             popup =  ~bairro,
      #             dashArray = "0",
      #             fillOpacity = 0,
      #             group = "Bairros"
      # )  |>
      # addLabelOnlyMarkers(data = shp_Bairros,group='Nomes dos bairros',
      #                     lng = ~x, lat = ~y, label = ~shp_Bairros$bairro  ,
      #                     labelOptions = labelOptions(noHide = T,  textOnly = T,
      #                                                 direction = "right",
      #                                                 style = list(
      #                                                   "color" = "black",
      #                                                   'background-color'= 'rgba(255,255,255, 0)',
      #                                                   'padding'= '0px 0px 0px 0px'      ))) |> 
      # addMeasure(
      #   position = "topright",
      #   primaryLengthUnit = "meters",
      #   primaryAreaUnit = "sqmeters",
      #   secondaryLengthUnit = "kilometers",
      #   activeColor = "#3D535D",
      #   completedColor = "#7D4479",
      #   localization = "pt_BR",
      #   captureZIndex = 10000
      # )   
    }) 
    output$table_bairro <- renderReactable({
      
      
      areaB <- round(sum(shp_Bairros$Área_km2),2)      
      pessoB <- sum(shp_Bairros$Pessoas)   
      domiB <- sum(shp_Bairros$Domicílios)   
      densB <-round( sum(shp_Bairros$Pessoas)/sum(shp_Bairros$Área_km2) )
      medB <- round(sum(shp_Bairros$Pessoas)/sum(shp_Bairros$Domicílios),2)
      
      shp_Bairros <-   shp_Bairros |>
        st_drop_geometry() |> 
        select(-x,-y,) |> 
        mutate(Densidade=round(Densidade),
               Área_km2=round(Área_km2,2),
               Média_moradores=round(Média_moradores,2))
      
      
      reactable(shp_Bairros,
                bordered = TRUE,compact = TRUE,defaultPageSize = 10,
                highlight = TRUE, searchable = TRUE,defaultColDef = colDef(#headerClass = "sort-header",
                  footerStyle = list(fontWeight = "bold",align = "right")),
                columns = list(
                  Bairro = colDef(name = "Bairro",footer='Geral', style = list(fontFamily = "monospace"
                                                                               # whiteSpace = "pre",fontSize = "1.5rem"
                  )),
                  Área_km2 = colDef(name = "Área km2", footer=format(areaB,big.mark = ".",decimal.mark=","), 
                                    align = "right", cell = function(value) {
                                      width <- paste0(value / max(shp_Bairros$Área_km2) * 100, "%")
                                      value <- format(value,big.mark = ".",decimal.mark=",")
                                    }, style = list(fontFamily = "monospace", whiteSpace = "pre")),
                  Pessoas = colDef( align = "left", footer=format(pessoB,big.mark = ".",decimal.mark=","), 
                                    cell = function(value) {
                                      width <- paste0(value / max(shp_Bairros$Pessoas) * 100, "%")
                                      value <- format(value,big.mark = ".",decimal.mark=",")
                                      value <- format(value, width = 7, justify = "right")
                                      bar_chart(value, width = width,background = "#e1e1e1")
                                    }, style = list(fontFamily = "monospace", whiteSpace = "pre")),
                  Domicílios = colDef( align = "left",footer=format(domiB,big.mark = ".",decimal.mark=","), 
                                       cell = function(value) {
                                         width <- paste0(value / max(shp_Bairros$Domicílios) * 100, "%")
                                         value <- format(value,big.mark = ".",decimal.mark=",")
                                         value <- format(value, width = 7, justify = "right")
                                         bar_chart(value, width = width, fill = "#fc5185", background = "#e1e1e1")
                                       }, style = list(fontFamily = "monospace", whiteSpace = "pre")),
                  Densidade = colDef( align = "left",footer=format(densB,big.mark = ".",decimal.mark=","), 
                                      cell = function(value) {
                                        width <- paste0(value / max(shp_Bairros$Densidade) * 100, "%")
                                        value <- format(value,big.mark = ".",decimal.mark=",")
                                        value <- format(value, width = 7, justify = "right")
                                        bar_chart(value, width = width, fill = "#245", background = "#e1e1e1")
                                      }, style = list(fontFamily = "monospace", whiteSpace = "pre")),
                  Média_moradores = colDef(name = "Média morad.",footer=format(medB,big.mark = ".",decimal.mark=","), 
                                           
                                           align = "right", 
                                           style = list(fontFamily = "monospace", whiteSpace = "pre"))
                )
      )
      
      
    })
  },once=T)
  
  observeEvent(req(input$current_tab=='Bairros'), {
    
    leafletProxy('map_bairro') |>
      addPolygons(data=shp_Bairros,
                  weight=.6,
                  fillColor=~pal_ba(shp_Bairros$Pessoas),
                  color = '#452',
                  opacity = 1,
                  group = 'id_final',
                  popup =  popupTable(bairro_pop_,zcol = c("Bairro", "Pessoas",
                                                           "Domicílios","Densidade",
                                                           "Área_km2"),
                                      feature.id = FALSE,
                                      row.numbers = FALSE),
                  dashArray = "0",
                  fillOpacity =1 ,
                  highlightOptions=highlightOptions(
                    color = "#CC0000", weight = 3, bringToFront = F)
      ) |>
      addLegend(data = shp_Bairros ,"bottomright", pal = pal_ba, values = ~Pessoas,
                title = "População",
                opacity = 1
      )
    
  },once=T)
  # 
  { # aux
    pal_ba <- colorNumeric(
      palette = "YlGnBu",
      domain = shp_Bairros$Pessoas
    )
    
    pal_dens_ba<- colorNumeric(
      palette = "YlOrBr",
      domain = shp_Bairros$Densidade
    )
    
    bairro_pop_ <- st_drop_geometry(shp_Bairros) |>
      mutate(Pessoas = format(Pessoas, big.mark = ".", decimal.mark = ","),
             Domicílios = format(Domicílios, big.mark = ".", decimal.mark = ","),
             Densidade = format(Densidade, big.mark = ".", decimal.mark = ","),
             Área_km2 = format(Área_km2, big.mark = ".", decimal.mark = ","),
             
      )
    
    # sca_ba <- colourScale(shp_Bairros$Densidade, breaks=6,
    #                       style="quantile",dec=1,revCol=F)
    # 
    # colorpal_dens_ba <- colorBin("YlOrBr", shp_Bairros$Densidade, bins = sca_ba$breaks)
    # 
    # 
    
    
  } # Aux mapa
  
  observeEvent(req(input$tabs_ba),{ 
    
    
    leafletProxy("map_bairro" )  |> 
      clearGroup(c('id_final')) |> 
      clearControls() #|> 
    # removeMeasure() |> 
    # addMeasure(
    #   position = "topright",
    #   primaryLengthUnit = "meters",
    #   primaryAreaUnit = "sqmeters",
    #   secondaryLengthUnit = "kilometers",
    #   activeColor = "#3D535D",
    #   completedColor = "#7D4479",
    #   localization = "pt_BR",
    #   captureZIndex = 10000
    # )
    
    if(input$tabs_ba == 'População' )
    {
      
      
      leafletProxy('map_bairro') |>
        addPolygons(data=shp_Bairros,
                    weight=.6,
                    fillColor=~pal_ba(shp_Bairros$Pessoas),
                    color = '#452',
                    opacity = 1,
                    group = 'id_final',
                    popup =  popupTable(bairro_pop_,zcol = c("Bairro", "Pessoas",
                                                             "Domicílios","Densidade",
                                                             "Área_km2"),
                                        feature.id = FALSE,
                                        row.numbers = FALSE),
                    dashArray = "0",
                    fillOpacity =1 ,
                    highlightOptions=highlightOptions(
                      color = "#CC0000", weight = 3, bringToFront = F)
        ) |>
        addLegend(data = shp_Bairros,"bottomright", pal = pal_ba, values = ~Pessoas,
                  title = "População",
                  opacity = 1
        )
    }
    
    
    if(input$tabs_ba == 'Densidade' )
    {
      
      leafletProxy('map_bairro') |>
        addPolygons(data=shp_Bairros ,
                    weight=.6,
                    # fillColor=~colorpal_dens_ba(shp_Bairros$Densidade),
                    fillColor=~pal_dens_ba(shp_Bairros$Densidade),
                    color = '#452',
                    opacity = 1,
                    group = 'id_final',
                    popup =  popupTable(bairro_pop_,zcol = c("Bairro", "Pessoas",
                                                             "Domicílios","Densidade",
                                                             "Área_km2"),
                                        feature.id = FALSE,
                                        row.numbers = FALSE),
                    dashArray = "0",
                    fillOpacity =1 ,
                    highlightOptions=highlightOptions(
                      color = "#CC0000", weight = 3, bringToFront = F)
        ) |>
        addLegend(data = shp_Bairros,position ="bottomright", pal = pal_dens_ba,
                  values = ~Densidade,
                  labFormat = my_labelFormat(
                    big.mark='.',
                    digits=0),
                  title = "Densidade populacional",
                  opacity = 1
        )
      
      
    }
    
  },ignoreInit = T) # 
  }















########

library(shiny)
library(leaflet)
library(sf)
library(dplyr)

#load shapefile
nc <- st_read(system.file("shape/nc.shp", package="sf")) %>%
  st_transform(4326)

shinyApp(
  ui = fluidPage(
    
    "Update selectize input by clicking on the map",
    
    leafletOutput("map"),
    "I would like the selectize input to update to show all the locations selected,",
    "but also when items are removed here, they are removed on the map too, so linked to the map.",
    selectizeInput(inputId = "selected_locations",
                   label = "Selected:",
                   choices = bairros_CD2022$NM_BAIRRO,
                   selected = NULL,
                   multiple = TRUE)
  ),
  
  server <- function(input, output, session){
    
    #create empty vector to hold all click ids
    selected_ids <- reactiveValues(ids = vector())
    
    #initial map output
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = bairros_CD2022,
                    fillColor = "white",
                    fillOpacity = 0.5,
                    color = "black",
                    stroke = TRUE,
                    weight = 1,
                    layerId = ~NM_BAIRRO,
                    group = "regions",
                    label = ~NM_BAIRRO) %>%
        addPolygons(data = bairros_CD2022,
                    fillColor = "red",
                    fillOpacity = 0.5,
                    weight = 1,
                    color = "black",
                    stroke = TRUE,
                    layerId = ~idd,
                    group = ~NM_BAIRRO) %>%
        hideGroup(group = bairros_CD2022$NM_BAIRRO) # nc$CNTY_ID
    }) #END RENDER LEAFLET
    
    #define leaflet proxy for second regional level map
    proxy <- leafletProxy("map")
    
    #create empty vector to hold all click ids
    selected <- reactiveValues(groups = vector())
    
    observeEvent(input$map_shape_click, {
      if(input$map_shape_click$group == "regions"){
        selected$groups <- c(selected$groups, input$map_shape_click$id)
        proxy %>% showGroup(group = input$map_shape_click$id)
      } else {
        selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
        proxy %>% hideGroup(group = input$map_shape_click$group)
      }
      updateSelectizeInput(session,
                           inputId = "selected_locations",
                           choices = bairros_CD2022$NM_BAIRRO,
                           selected = selected$groups)
    })
    
    observeEvent(input$selected_locations, {
      removed_via_selectInput <- setdiff(selected$groups, input$selected_locations)
      added_via_selectInput <- setdiff(input$selected_locations, selected$groups)
      
      if(length(removed_via_selectInput) > 0){
        selected$groups <- input$selected_locations
        proxy %>% hideGroup(group = removed_via_selectInput)
      }
      
      if(length(added_via_selectInput) > 0){
        selected$groups <- input$selected_locations
        proxy %>% showGroup(group = added_via_selectInput)
      }
    }, ignoreNULL = FALSE)
    
  })











{
  
  library(shiny)
  library(leaflet)
  library(sf)
  library(dplyr)
  
  #load shapefile
  nc <- st_read(system.file("shape/nc.shp", package="sf")) %>%
    st_transform(4326)
  
  shinyApp(
    ui = fluidPage(
      
      "Update selectize input by clicking on the map",
      
      leafletOutput("map"),
      "I would like the selectize input to update to show all the locations selected,",
      "but also when items are removed here, they are removed on the map too, so linked to the map.",
      selectizeInput(inputId = "selected_locations",
                     label = "Selected:",
                     choices = nc$NAME,
                     selected = NULL,
                     multiple = TRUE)
    ),
    
    server <- function(input, output, session){
      
      #create empty vector to hold all click ids
      selected_ids <- reactiveValues(ids = vector())
      
      #initial map output
      output$map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addPolygons(data = nc,
                      fillColor = "white",
                      fillOpacity = 0.5,
                      color = "black",
                      stroke = TRUE,
                      weight = 1,
                      layerId = ~NAME,
                      group = "regions",
                      label = ~NAME) %>%
          addPolygons(data = nc,
                      fillColor = "red",
                      fillOpacity = 0.5,
                      weight = 1,
                      color = "black",
                      stroke = TRUE,
                      layerId = ~CNTY_ID,
                      group = ~NAME) %>%
          hideGroup(group = nc$NAME) # nc$CNTY_ID
      }) #END RENDER LEAFLET
      
      #define leaflet proxy for second regional level map
      proxy <- leafletProxy("map")
      
      #create empty vector to hold all click ids
      selected <- reactiveValues(groups = vector())
      
      observeEvent(input$map_shape_click, {
        if(input$map_shape_click$group == "regions"){
          selected$groups <- c(selected$groups, input$map_shape_click$id)
          proxy %>% showGroup(group = input$map_shape_click$id)
        } else {
          selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
          proxy %>% hideGroup(group = input$map_shape_click$group)
        }
        updateSelectizeInput(session,
                             inputId = "selected_locations",
                             choices = nc$NAME,
                             selected = selected$groups)
      })
      
      observeEvent(input$selected_locations, {
        removed_via_selectInput <- setdiff(selected$groups, input$selected_locations)
        added_via_selectInput <- setdiff(input$selected_locations, selected$groups)
        
        if(length(removed_via_selectInput) > 0){
          selected$groups <- input$selected_locations
          proxy %>% hideGroup(group = removed_via_selectInput)
        }
        
        if(length(added_via_selectInput) > 0){
          selected$groups <- input$selected_locations
          proxy %>% showGroup(group = added_via_selectInput)
        }
      }, ignoreNULL = FALSE)
      
    })
  
  
  
}


