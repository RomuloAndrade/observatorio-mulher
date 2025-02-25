
df_faixa_ <- df_faixa |> 
  mutate(grupo_de_idade =as.factor(grupo_de_idade ),
         grupo_de_idade = forcats::fct_relevel(grupo_de_idade,
                                               '0 a 4 anos', 
                                               '5 a 9 anos',
                                               '10 a 14 anos',
                                               '15 a 19 anos',
                                                '20 a 24 anos',
                                               '25 a 29 anos',
                                              '30 a 39 anos',
                                               '40 a 49 anos',
                                               '50 a 59 anos',
                                              '60 a 69 anos',
                                               '70 anos ou mais' 
                                               )) |> 
  arrange(NM_BAIRRO,grupo_de_idade)






 df_faixa_ |> filter(NM_BAIRRO=='FORTALEZA') |> 
  e_charts(x=grupo_de_idade) |>
  e_bar(perc_masc,stack = "grp2",color='#40711A',#bind=perc_masc,
        name='Homens',barWidth= '90%',
        itemStyle=list( opacity= .9)
       #  label=list(formatter=  htmlwidgets::JS("
       # function (params) {
       #  return Math.abs(params.name)+'%';}"),
       #             show= T,position= "left"#,color='green'#,fontWeight= "bold"
       #  )
  )  |>
  e_bar(perc_fem,stack = "grp",#bind=perc_fem,
        name='Mulheres',color='#A02EA0',
        itemStyle=list( opacity= 0.5),
       #  label=list(formatter=  htmlwidgets::JS("
       # function (params) {
       #  return Math.abs(params.name)+'%';}"),
       #             show= T,position= "right"#,color='blue'#,fontWeight= "bold"
       #  )
  ) |>
  e_flip_coords() |>
  e_y_axis(offset=30) |>
  e_x_axis(#max=117799,
    #min=-127799,
    type= 'value',
    show= F,
    axisLabel=list(
      formatter= htmlwidgets::JS("function(params) {
               return Math.abs(params);
             }")
    )
  ) |>
  e_grid(left='20%',) |>
  #  e_theme('macarons') |>
  e_legend( top= "3%",selected= list()) |>
  e_tooltip( trigger = c("axis"),axisPointer=list(type='shadow')) |>

  e_title(text='População residente em Fortaleza',subtext="Segundo sexo e grupo de idade" )



# library(stringr)
# library(dplyr)
# library(sf)
# library(echarts4r)
# library(leaflet)
# library(leaflet.extras)
# # bairros_demografia<- readxl::read_excel("Dados/Agregados_por_bairros_demografia_BR.xlsx")
# # 
# # bairros_demografia <- bairros_demografia |>
# #   filter(str_detect(CD_BAIRRO, "^230440")) |>
# #   mutate_at(vars(starts_with("V")), as.numeric )
# # 
# # saveRDS(bairros_demografia,'Dados/bairros_demografia.rds')
# # bairros_demografia <- readRDS('Dados/bairros_demografia.rds') 
# # 
# # bairros_demografia_fort <- data.frame(NM_BAIRRO = c('FORTALEZA'),
# #            V01006 = c(2428708),
# #            V01007 = c(1126929),
# #            V01008 = c(1301779))
# # 
# # bairros_demografia <- bind_rows(bairros_demografia,bairros_demografia_fort)
# # 
# # saveRDS(bairros_demografia,'Dados/bairros_demografia.rds')
# 
# 
# ##
# 
# # bairros_CD2022 <- read_sf('Dados/CE_bairros_CD2022.shp')|> 
# #   filter(NM_MUN == "Fortaleza") |> 
# #   select(NM_BAIRRO,
# #          AREA_KM2,
# #          v0001:v0007)
# # 
# # bairros_CD2022 <- bairros_CD2022 |> st_transform(4326)
# # saveRDS(bairros_CD2022,'Dados/bairros_CD2022.rds')
# 
# 
# st_crs(bairros_CD2022)
# 
# # Raca cor Fortaleza
# 
# df_raca_fort <- df_cor_ |> 
#   filter(!is.na(CD_BAIRRO )) |> 
#   group_by(Cor_raça,Sexo) |> 
#   summarise(Qtd = sum(Qtd)) |> 
#   mutate(NM_BAIRRO = 'FORTALEZA',
#          Cor_raça = as.factor(Cor_raça),
#          Cor_raça = forcats::fct_relevel(Cor_raça, "branca",'preta','amarela','parda','indígena')) |> 
#   arrange(Cor_raça)
# 
# 
# df_cor_ <- bind_rows(df_cor_ |> 
#                        filter(!is.na(CD_BAIRRO )) ,df_raca_fort)
# 
# saveRDS(df_cor_,'Dados/df_cor_.rds')
# 
# # Sexo
# df_sexo <- df_cor_ |> 
#   filter(!is.na(CD_BAIRRO )) |> 
#   group_by(Sexo,NM_BAIRRO) |> 
#   summarise(Qtd = sum(Qtd)) 
# 
# 
# df_sexo_fort <- data.frame(NM_BAIRRO = c('FORTALEZA','FORTALEZA'),
#                            Sexo = c('Homens','Mulheres'),
#                            Qtd = c(1126929, 1301779))
#   
# 
# 
# df_sexo <- bind_rows(df_sexo,df_sexo_fort)
# 
# saveRDS(df_sexo,'Dados/df_sexo.rds')
# 
# 
# 
# 
# df_sexo |> 
#   filter(NM_BAIRRO == 'Centro') |> 
#   #summarise(Qtd=sum(Qtd)) |> 
#   e_charts(Sexo,reorder = F) |> 
#   e_pie(Qtd, roseType = "radius", clockwise=F,   radius = c("5%", "65%"),
#         label = list(color='black',show = T,formatter = "{b} \n {d}%"),
#         emphasis = list(
#           label = list(
#             show = T,
#             fontSize = 20,
#             fontWeight = "bold"))) |>
#   e_tooltip() |> 
#   e_legend(show=F) |> 
#   e_color(c("#2ec7c9", "#b6a2de"))|> 
#   e_title(text='input$select_1[2]')    
# ##
# 
# # Mapa
# {
#   leaflet() |>
#     setMapWidgetStyle(list(background = "white"))  |> 
#     addProviderTiles(providers$CartoDB.PositronNoLabels, group = "CartoDB.PositronNoLabels")  |> #CartoDB.Positron
#     addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap")  |> 
#     addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter")  |>
#     addProviderTiles(providers$Esri.WorldImagery, group = "WorldImagery")  |>
#     addLayersControl(
#       baseGroups = c("CartoDB.Positron","OpenStreetMap",'CartoDB.DarkMatter','WorldImagery'),
#       overlayGroups = c(
#         "Bairros")
#     ) |>
#     hideGroup("Elementos do mapa")  |>
#     hideGroup('Nomes dos bairros') |>
#     addScaleBar(position = "bottomright",
#                 options = scaleBarOptions(imperial = F))  |>
#     setView(-38.52782, -3.785804, zoom = 12) |>
#     addFullscreenControl(pseudoFullscreen = T)|> 
#     addPolygons(data=bairros_CD2022,
#                 weight=1,
#                 color='black',
#                 opacity = 0.5,
#                 fillColor = "white",
#                 popup =  ~NM_BAIRRO,
#                 label =  ~NM_BAIRRO,
#                 dashArray = "0",
#                 fillOpacity = 0,
#                 group = "Bairros"
#     )  |>
#     addMeasure(
#       position = "topright",
#       primaryLengthUnit = "meters",
#       primaryAreaUnit = "sqmeters",
#       secondaryLengthUnit = "kilometers",
#       activeColor = "#3D535D",
#       completedColor = "#7D4479",
#       localization = "pt_BR",
#       captureZIndex = 10000
#     ) 
#   
#   
# }
# 
# pop_fort <- readxl::read_excel('Dados/Censo 2022 - Pirâmide etária - Fortaleza (CE).xlsx') |> 
#   janitor::clean_names()
# 
# # Piramide 
# {
#   e_theme(name='macarons') 
#   pop_fort_graf <- pop_fort |>
#     rename(masculina=populacao_masculina_pessoas,
#            feminina=populacao_feminina_pessoas) |> 
#     mutate( masculina_inv= masculina*-1,
#             seq = 1:dim(pop_fort)[1],
#             perc_fem = round(feminina/sum(feminina+masculina)*100,1),
#             perc_masc = round(feminina/sum(feminina+masculina)*100,1)
#     ) |> 
#     arrange(-seq) 
# 
#   
#   
#   b <- pop_fort_graf |> 
#     e_charts(x=grupo_de_idade) |> 
#     e_bar(masculina_inv,stack = "grp2",bind=perc_masc,name='Homens',
#           label=list(fontSize =14,formatter=  htmlwidgets::JS("
#        function (params) {
#         return Math.abs(params.name)+'%';}"),
#                      show= T,position= "left",fontWeight= "bold"#,color='green'#
#           )
#     ) |> 
#     e_bar(feminina,stack = "grp2",bind=perc_fem,name='Mulheres',
#           label=list(fontSize =14,formatter=  htmlwidgets::JS("
#        function (params) {
#         return Math.abs(params.name)+'%';}"),
#                      show= T,position= "right",fontWeight= "bold"#,color='blue'#
#           )
#     ) |> 
#     e_flip_coords() |> 
#     e_y_axis(offset=30) |> 
#     e_x_axis(max=117799,
#              min=-127799,
#              type= 'value',
#              show= F,
#              axisLabel=list(
#                formatter= htmlwidgets::JS("function(params) {
#                return Math.abs(params);
#              }")
#              )
#     ) |>
#     e_grid(left='20%',) |> 
#     e_theme('macarons') |> 
#     e_legend( top= "3%") |> 
#     e_title(text='População residente em Fortaleza - 2022',subtext="Segundo sexo e grupo de idade" )
#   b
#   htmlwidgets::saveWidget(b, "C:/Romulo/Dados/Censo2022/temp.html");
#   webshot2::webshot("C:/Romulo/Dados/Censo2022/temp.html","C:/Romulo/Dados/Censo2022/piramide_22_.png",
#                     vwidth = 900, vheight = 800,delay = 1)
#   
# }
# 
#Tratamento piramide por bairros
{


  faixa_M <- bairros_demografia |>
    select(NM_BAIRRO,
           V01009:V01019) |>
    pivot_longer(cols = V01009:V01019,
                 names_to = 'grupo_de_idade',
                 values_to = 'Masculina'  )
  faixa_F <- bairros_demografia |>
    select(NM_BAIRRO,
           V01020:V01030) |>
    pivot_longer(cols = V01020:V01030,
                 names_to = 'grupo_de_idade',
                 values_to = 'Feminina'  )

  faixa_M <- faixa_M |>
    mutate(grupo_de_idade = case_match(
      grupo_de_idade,
      'V01009'	~ '0 a 4 anos',
      'V01010'	~ '5 a 9 anos',
      'V01011'	~ '10 a 14 anos',
      'V01012'  ~ '15 a 19 anos',
      'V01013'	~ '20 a 24 anos',
      'V01014'	~ '25 a 29 anos',
      'V01015'	~ '30 a 39 anos',
      'V01016'	~ '40 a 49 anos',
      'V01017'	~ '50 a 59 anos',
      'V01018'	~ '60 a 69 anos',
      'V01019'	~ '70 anos ou mais'),
      seq = rep(seq(from=11,to=1),121)
      )

  faixa_F <- faixa_F |>
    mutate(grupo_de_idade = case_match(
      grupo_de_idade,
      'V01020' ~ '0 a 4 anos',
      'V01021' ~ '5 a 9 anos',
      'V01022' ~ '10 a 14 anos',
      'V01023' ~ '15 a 19 anos',
      'V01024' ~ '20 a 24 anos',
      'V01025' ~ '25 a 29 anos',
      'V01026' ~ '30 a 39 anos',
      'V01027' ~ '40 a 49 anos',
      'V01028' ~ '50 a 59 anos',
      'V01029' ~ '60 a 69 anos',
      'V01030' ~ '70 anos ou mais')
    )

df_faixa <- left_join(faixa_F,faixa_M)

#rm(faixa_F,faixa_M)


df_faixa <- df_faixa |>
    group_by(NM_BAIRRO) |>
      mutate(
      perc_fem = round(Feminina/sum(Feminina+Masculina)*100,1),
      perc_masc =round(Masculina/sum(Feminina+Masculina)*100,1),
      Masculina = Masculina*(-1)) |>
    rename(Homens=Masculina,
           Mulheres=Feminina)

#Fortaleza

#df_faixa <- readRDS('Dados/df_faixa.rds')

df_faixa_fort <- df_faixa |>
  select(1:4) |>
  group_by(grupo_de_idade ) |>
  summarise(Mulheres=sum(Mulheres),
            Homens=(-1)*sum(Homens)) |>
  mutate(
    perc_fem = round(Mulheres/sum(Mulheres+Homens)*100,1),
    perc_masc =round(Homens/sum(Mulheres+Homens)*100,1),
    Homens = Homens*(-1),
    seq = seq(from=11,to=1),
    NM_BAIRRO = 'FORTALEZA' )


df_faixa <- bind_rows(df_faixa,df_faixa_fort)


#saveRDS(df_faixa_,"Dados/df_faixa_.rds")
lista_demog <- unique(df_faixa$NM_BAIRRO)

saveRDS(lista_demog,"Dados/lista_demog.rds")

}
# 
# 
#   
# #Grafico
# {
#  
#  #  e_theme(name='macarons') 
# 
#   
#   
#   b <- df_faixa |> 
#     filter(NM_BAIRRO == 'Pedras') |> 
#     e_charts(x=grupo_de_idade) |> 
#     e_bar(Masculina,stack = "grp2",bind=perc_masc,name='Homens',
#           label=list(fontSize =14,formatter=  htmlwidgets::JS("
#        function (params) {
#         return Math.abs(params.name)+'%';}"),
#                      show= T,position= "left",fontWeight= "bold"#,color='green'#
#           )
#     ) |> 
#     e_bar(Feminina,stack = "grp2",bind=perc_fem,name='Mulheres',
#           label=list(fontSize =14,formatter=  htmlwidgets::JS("
#        function (params) {
#         return Math.abs(params.name)+'%';}"),
#                      show= T,position= "right",fontWeight= "bold"#,color='blue'#
#           )
#     ) |> 
#     e_flip_coords() |> 
#     e_y_axis(offset=30) |> 
#     e_x_axis(#max=117799,
#              #min=-127799,
#              type= 'value',
#              show= F,
#              axisLabel=list(
#                formatter= htmlwidgets::JS("function(params) {
#                return Math.abs(params);
#              }")
#              )
#     ) |>
#     e_grid(left='20%',) |> 
#     e_theme('macarons') |> 
#     e_legend( top= "3%") |> 
#     e_title(text='População residente em Fortaleza - 2022',subtext="Segundo sexo e grupo de idade" )
#   b
#   
#   # htmlwidgets::saveWidget(b, "C:/Romulo/Dados/Censo2022/temp.html");
#   # webshot2::webshot("C:/Romulo/Dados/Censo2022/temp.html","C:/Romulo/Dados/Censo2022/piramide_22_.png",
#   #                   vwidth = 900, vheight = 800,delay = 1) 
#   # 
# }
# 
# {
# # pizza sexo
# library(tidyr)
# 
# bairros_demografia |> 
#   select(NM_BAIRRO,V01007,V01008) |>
#   rename(Feminino = V01008,
#          Masculino = V01007
#          ) |> 
#   pivot_longer(cols = 2:3,names_to = 'Sexo',values_to = 'Qtd.') |> 
#   group_by(Sexo) |> 
#   summarise(Qtd.=sum(Qtd.)) |> 
#   e_charts(Sexo,reorder = F) |> 
#   e_pie(Qtd., roseType = "radius", clockwise=F,   radius = c("20%", "70%"),
#         label = list(show = T,formatter = "{b} \n {d}%"),
#         emphasis = list(
#           label = list(
#             show = T,
#             fontSize = 20,
#             fontWeight = "bold"
#           )
#         )
#         ) |>
#   e_tooltip() |> 
#   e_color(c("#2ec7c9", "#b6a2de"))
# 
# 
#      sexo_bairro |> 
#       filter(NM_BAIRRO == 'Aldeota') |> 
#       summarise(Qtd.=sum(Qtd.)) |> 
#       e_charts(Sexo,reorder = F) |> 
#       e_pie(Qtd., roseType = "radius", clockwise=F,   radius = c("5%", "90%"),
#             label = list(show = T,formatter = "{b} \n {d}%"),
#             emphasis = list(
#               label = list(
#                 show = T,
#                 fontSize = 20,
#                 fontWeight = "bold"))) |>
#       e_tooltip() |> 
#       #e_legend(show=F) |> 
#       e_color(c("#2ec7c9", "#b6a2de"))#|> 
#      # e_title(text=input$select_1[2])  
# 
#      
#      
# }
# 
# 
# 
library(dplyr)
 
 bairros_CD2022 <- readRDS('Dados/bairros_CD2022.rds') 
 bairros_CD2022  <-  mutate(bairros_CD2022,idd= dplyr::row_number())
 AIS <-  readxl::read_excel('Dados/AIS.xlsx') 
 bairros_AIS <- left_join(bairros_CD2022,AIS,by=c('NM_BAIRRO'='Bairros')) |> 
   group_by(AIS) |> 
   summarise() |> 
   mutate(idd= dplyr::row_number())
 
 leaflet() |>
   addTiles() |>
   addPolygons(data = bairros_AIS,
               fillColor = "white",
               fillOpacity = 0.5,
               color = "black",
               stroke = TRUE,
               weight = 1,
               layerId = ~AIS ,
               group = "regions",
               label = ~AIS)
 
 
 shinyApp(
   ui = fluidPage(
     
     "Update selectize input by clicking on the map",
     
     leafletOutput("map"),
     "I would like the selectize input to update to show all the locations selected,",
     "but also when items are removed here, they are removed on the map too, so linked to the map.",
     selectizeInput(inputId = "selected_locations",
                    label = "Selected:",
                    choices = bairros_AIS$AIS,
                    selected = NULL,
                    multiple = TRUE,
                    options = list('plugins' = list('remove_button')))
   ),
   
   server = function(input, output, session){
     
     #create empty vector to hold all click ids
     selected_ids <- reactiveValues(ids = vector())
     
     #initial map output
     output$map <- renderLeaflet({
       leaflet() |>
         addTiles() |>
         addPolygons(data = bairros_AIS,
                     fillColor = "white",
                     fillOpacity = 0.5,
                     color = "black",
                     stroke = TRUE,
                     weight = 1,
                     layerId = ~AIS,
                     group = "regions",
                     label = ~AIS) |>
         addPolygons(data = bairros_AIS,
                     fillColor = "red",
                     fillOpacity = 0.5,
                     weight = 1,
                     color = "black",
                     stroke = TRUE,
                     layerId = ~idd,
                     group = ~AIS) |>
         hideGroup(group = bairros_AIS$AIS) # nc$CNTY_ID
     }) #END RENDER LEAFLET
     
     #define leaflet proxy for second regional level map
     proxy <- leafletProxy("map")
     
     #create empty vector to hold all click ids
     selected <- reactiveValues(groups = vector())
     
     observeEvent(input$map_shape_click, {
       if(input$map_shape_click$group == "regions"){
         selected$groups <- c(selected$groups, input$map_shape_click$id)
         proxy |> showGroup(group = input$map_shape_click$id)
       } else {
         selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
         proxy |> hideGroup(group = input$map_shape_click$group)
       }
       updateSelectizeInput(session,
                            inputId = "selected_locations",
                            choices = bairros_AIS$AIS,
                            selected = selected$groups)
     })
     
     observeEvent(input$selected_locations, {
       removed_via_selectInput <- setdiff(selected$groups, input$selected_locations)
       added_via_selectInput <- setdiff(input$selected_locations, selected$groups)
       
       if(length(removed_via_selectInput) > 0){
         selected$groups <- input$selected_locations
         proxy |> hideGroup(group = removed_via_selectInput)
       }
       
       if(length(added_via_selectInput) > 0){
         selected$groups <- input$selected_locations
         proxy |> showGroup(group = added_via_selectInput)
       }
     }, ignoreNULL = FALSE)
     
   })
 
 
 
library(leaflet)

 
 
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
                   multiple = TRUE,
                   options = list('plugins' = list('remove_button'),maxItems = 3))
  ),

  server = function(input, output, session){

    #create empty vector to hold all click ids
    selected_ids <- reactiveValues(ids = vector())

    #initial map output
    output$map <- renderLeaflet({
      leaflet() |>
        addTiles() |>
        addPolygons(data = bairros_CD2022,
                    fillColor = "white",
                    fillOpacity = 0.5,
                    color = "black",
                    stroke = TRUE,
                    weight = 1,
                    layerId = ~NM_BAIRRO,
                    group = "regions",
                    label = ~NM_BAIRRO) |>
        addPolygons(data = bairros_CD2022,
                    fillColor = "red",
                    fillOpacity = 0.5,
                    weight = 1,
                    color = "black",
                    stroke = TRUE,
                    layerId = ~idd,
                    group = ~NM_BAIRRO) |>
        hideGroup(group = bairros_CD2022$NM_BAIRRO) # nc$CNTY_ID
    }) #END RENDER LEAFLET

    #define leaflet proxy for second regional level map
    proxy <- leafletProxy("map")

    #create empty vector to hold all click ids
    selected <- reactiveValues(groups = vector())

    observeEvent(input$map_shape_click, {
      if(input$map_shape_click$group == "regions"){
        selected$groups <- c(selected$groups, input$map_shape_click$id)
        proxy |> showGroup(group = input$map_shape_click$id)
      } else {
        selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
        proxy |> hideGroup(group = input$map_shape_click$group)
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
        proxy |> hideGroup(group = removed_via_selectInput)
      }

      if(length(added_via_selectInput) > 0){
        selected$groups <- input$selected_locations
        proxy |> showGroup(group = added_via_selectInput)
      }
    }, ignoreNULL = FALSE)

  })

 
 
 
composicao <- readxl::read_excel('Dados/Composição de domicílios por sexo do responsável.xlsx') |> 
  dplyr::arrange(`Responsável mulher`)

saveRDS(composicao,'Dados/composicao.rds')

composicao |> 
e_chart(x=Composição) |> 
  e_bar(`Responsável mulher`,barGap=0.02) |> 
  e_bar(`Responsável homens`) |> 
  e_tooltip(trigger = c("axis")) |> 
  e_x_axis(axisLabel = list(color='black',  fontWeight= "bolder") ) |> 
  e_color(c('#b6a2de','#2ec7c9')) |> 
  e_flip_coords() |> 
  e_grid(left='40%')



favela <- readxl::read_excel('Dados/Favela.xlsx')
saveRDS(favela,'Dados/favela.rds')

favela |> 
  group_by(V2007   ) |> 
  e_chart(x=Raca  ) |> 
  e_bar(Perc,barGap=0.02) |> 
  e_tooltip(trigger = c("axis"),
            e_tooltip_pointer_formatter(locale = "PT-BR",style = c("percent"),digits = 1,
            )) |> 
  e_x_axis(name= 'Raça',axisLabel = list(color='black',  fontWeight= "bolder") ) |> 
  e_color(c('#008acd','#b6a2de','#2ec7c9')) |> 
  e_y_axis(min= .10,formatter=e_axis_formatter(style = c("percent"),
                                      locale = 'PT-BR') ) |> 
  e_legend(selected=list('Geral'=F),right= 0,top= "3%") 
