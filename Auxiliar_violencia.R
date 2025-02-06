library(dplyr)
library(lubridate)
library(forcats)
cvli_base <- readxl::read_excel('Dados/clvi_base.xlsx')

cvli_base_ <- cvli_base |>
  filter(Município=='Fortaleza') |> 
  dplyr::rename(Semana = `Dia da Semana`) |> 
  mutate(Semana = forcats::fct_relevel(Semana, 'Domingo','Segunda',
                              'Terça','Quarta','Quinta','Sexta','Sábado'),
         `Meio Empregado` = dplyr::if_else( `Meio Empregado`== 'Meio não informado','Não informado',`Meio Empregado`),
        Ano=lubridate::year(Data) 
         )

saveRDS(cvli_base_,'Dados/cvli_base.rds')
df_cvli <- readRDS('Dados/cvli_base.rds') 


clvi_base_sexo <- clvi_base_ |> 
  group_by(mes = lubridate::floor_date(Data, "month"),Gênero    ) |> 
  summarise(Qtd. = n()) |> 
  mutate(mes = format(mes,'%Y-%b')) |> 
  group_by(Gênero) 



clvi_base_sexo |> 
  e_chart(x = mes) |>
  e_line(Qtd.,symbolSize= 0) |> 
  e_tooltip(trigger = c("axis")) |> 
  e_color(c('#b6a2de','#2ec7c9','#008acd')) |> 
  e_legend(selected=list('Masculino'=F,'Não Informado'=F)) 
  

df_cvli |> 
  group_by(Semana,Gênero) |> 
  summarise(Qtd. = n()) |> 
  ungroup() |> 
  mutate(Perc. = Qtd./sum(Qtd.)) |> 
  group_by(Gênero) |>
  e_chart(x=Semana,stack='1') |> 
  e_bar(Perc.) |> 
  e_y_axis(formatter = e_axis_formatter("percent", digits = 0))  |>   
  e_legend(selected=list('Não Informado'=F)) |> 
  e_tooltip(trigger="item", formatter =
              htmlwidgets::JS(
                "function(p) {
          v = Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(p.value[1]);
          return('<strong>' + p.seriesName + '</strong>' +
          '<br>Percentual: ' + Intl.NumberFormat('pt-BR', { style: 'decimal',minimumFractionDigits: 1, 
  maximumFractionDigits: 1}).format(p.value[1]*100) + '%'
            );
          }"), textStyle=list(fontFamily="arial", fontSize=13)) |> 
  e_color(c('#b6a2de','#2ec7c9','#008acd')) |> 
  e_labels(color='black',position='insideTop',formatter =
             htmlwidgets::JS(
               "function(p) {
          v = Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(p.value[1]);
          return(Intl.NumberFormat('pt-BR', { style: 'decimal',minimumFractionDigits: 1, 
  maximumFractionDigits: 1}).format(p.value[1]*100) + '%'
            );
          }"))





df_cvli |> 
  group_by(Semana,Gênero) |> 
  summarise(Qtd. = n()) |> 
  ungroup() |> 
  mutate(Perc. = Qtd./sum(Qtd.)) |> 
  group_by(Gênero) |>
  e_chart(x=Semana,stack='1') |> 
  e_bar(Perc.) 

  df <- data.frame(x = 1:10, y = seq(1, 20, by = 2))

library(echarts4r)

  df  |>  
  e_charts(x) |>  
  e_polar() |>  
  e_angle_axis()  |>  
  e_radius_axis() |>  
  e_bar(y, coordinateSystem = "polar") 


  df <- data.frame(x = 1:10, y = seq(1, 20, by = 2))
  
  
  df_cvli |> 
    filter(Gênero != 'Não Informado') |> 
    group_by(Semana,Gênero) |>  #
    summarise(Qtd. = n()) |> 
    group_by(Gênero) |> 
    mutate(Perc. = Qtd./sum(Qtd.)) |> 
    e_chart(x=Semana) |> 
    e_polar(radius=c(5, "80%")) |>
    e_angle_axis(Semana) |> # angle = x
    e_radius_axis(show = F) |> 
    e_legend(right=1) |> 
    e_bar(Perc., coord_system = "polar", barGap=0,barWidth= "47%") |> 
    e_tooltip(trigger="item", formatter =
                htmlwidgets::JS(
                  "function(p) {
          v = Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(p.value);
          return('<strong>' + p.seriesName + '</strong>' +
          '<br>Percentual: ' + Intl.NumberFormat('pt-BR', { style: 'decimal',minimumFractionDigits: 1, 
  maximumFractionDigits: 1}).format(p.value*100) + '%'
            );
          }")) |> 
    e_color(c('#b6a2de','#2ec7c9','#008acd'))
  
  

  df_cvli |> 
    filter(Gênero != 'Não Informado') |> 
    group_by(Semana,Gênero) |>  #
    summarise(Qtd. = n()) |> 
    group_by(Gênero) |> 
    mutate(Perc. = Qtd./sum(Qtd.)) |> 
    pivot_wider(
      names_from = Gênero,
      values_from = c(Perc.,Qtd.)) |> 
    e_chart(x=Semana) |> 
    e_radar(Perc._Feminino,max = .19,name='Mulher',emphasis=list(
      lineStyle=list(width= 4 ))) |> 
    e_radar_opts( splitLine=list(show=T),axisLine=list(show=F)) |>
    e_radar(Perc._Masculino,max = .19,name='Homem') |> 
    e_tooltip(trigger="item", formatter =
                          htmlwidgets::JS(
     "function(p) { v = Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(p.value[1]);
          return('<strong>' + p.name + '</strong>' +
          '<br>Percentual: ' + Intl.NumberFormat('pt-BR', { style: 'decimal',minimumFractionDigits: 1, 
  maximumFractionDigits: 1}).format(p.value[1]*100) + '%')}"))  |> 
    e_color(c('#b6a2de','#2ec7c9'))

  
  
  
  
  
  
  
  
  
  
  
  
  


df_cvli |>
  group_by(`Meio Empregado`,Gênero    ) |> 
  summarise(Qtd. = n()) |> 
  ungroup() |> 
  group_by(Gênero) |>
  e_chart(x=`Meio Empregado`) |> 
  e_bar(Qtd.,barGap =0) |> 
  e_x_axis(axisLabel = list(interval= 0)) |>    
  e_y_axis(formatter = e_axis_formatter("decimal", digits = 0,locale = 'pt-BR')) |> 
  e_legend(selected=list('Masculino'=T)) |> 
  e_tooltip(trigger = c("axis")) |> 
  e_color(c('#b6a2de','#2ec7c9','#008acd'))


a1 <- df_cvli |>
  filter(Gênero=='Masculino') |> 
  group_by(`Meio Empregado` ) |> 
  summarise(Qtd. = n()) |> 
  e_chart(x=`Meio Empregado`) |> 
  e_pie(Qtd., roseType = "radius", clockwise=F,   radius = c("25%", "65%"),
        label = list(color='black',show = T,formatter = "{b} \n {d}%"),
        emphasis = list(
          label = list( 
            show = T,
            fontSize = 20,
            fontWeight = "bold"))) |>
  #e_tooltip() |> 
  e_legend(show=F) |> 
  e_group("meio")

a2<- df_cvli |>
  filter(Gênero=='Feminino') |> 
  group_by(`Meio Empregado` ) |> 
  summarise(Qtd. = n()) |> 
  e_chart(x=`Meio Empregado`) |> 
  e_pie(Qtd., roseType = "radius", clockwise=F,   radius = c("25%", "65%"),
        label = list(color='black',show = T,formatter = "{b} \n {d}%"),
        emphasis = list(
          label = list( 
            show = T,
            fontSize = 20,
            fontWeight = "bold"))) |>
  e_legend(show=F) |> 
  e_group("meio") |> 
  e_connect_group("meio")


e_arrange(a1,a2,cols=2)
 # e_color(c('#b6a2de','#2ec7c9','#008acd'))


  library(dplyr)
  
  #mapa
  
  bairros_CD2022 <- readRDS('Dados/bairros_CD2022.rds') 
  bairros_CD2022  <-  mutate(bairros_CD2022,idd= dplyr::row_number())
  AIS <-  readxl::read_excel('Dados/AIS.xlsx') 
  bairros_AIS <- left_join(bairros_CD2022,AIS,by=c('NM_BAIRRO'='Bairros')) |> 
    group_by(AIS) |> 
    summarise() |> 
    mutate(idd= dplyr::row_number())
  
 AIS_ano <-  cvli_base_ |>  mutate(Ano= year(Data)) |> 
    group_by(Ano,AIS) |> 
    summarise(Qtd=n())
  
  
  bairros_AIS_ <-  left_join(bairros_AIS,AIS_ano) 
  
saveRDS(bairros_AIS_,'Dados/bairros_AIS_.rds')


bairros_AIS <- bairros_AIS_[bairros_AIS_$Ano==2017,]

  pal_viol<- colorNumeric(
    palette = 'inferno',
    domain = bairros_AIS$Qtd,
    reverse =T
  )  
  
  leaflet() |>
    addTiles() |>
    addPolygons(data = bairros_AIS,
                fillOpacity = 0.5,
                color = "black",
                stroke = TRUE,
                weight = 1,
                fillColor=~pal_viol(bairros_AIS$Qtd),
                layerId = ~AIS ,
                group = "regions",
                label = ~AIS) |> 
    addLegend(data = bairros_AIS ,"bottomright", pal = pal_viol, values = ~Qtd,
              title = "CVLI",
              opacity = 1 )
    
  
 { 
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
          addProviderTiles(providers$CartoDB.PositronNoLabels, group = "CartoDB.PositronNoLabels") |> 
          addPolygons(data = bairros_AIS,
                      fillOpacity = 0.5,
                      color = "black",
                      stroke = TRUE,
                      weight = 1,
                      fillColor=~pal_viol(bairros_AIS$Qtd),
                      layerId = ~AIS ,
                      group = "regions",
                      label = ~AIS) |> 
          addLegend(data = bairros_AIS ,"bottomright", pal = pal_viol, values = ~Qtd,
                    title = "CVLI",
                    opacity = 1 ) |>
          addPolygons(data = bairros_AIS,
                      #fillColor = "green",
                      fillOpacity = 0.1,
                      weight = 4,
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
      
    })}
  
## Piramide
  
 #grupo_de_idade Mulheres Homens   seq perc_fem perc_masc
 
  library(dplyr) 
df_cvli_idade <- df_cvli |> 
  rename(idade=`Idade da Vítima`) |> 
  mutate( idade =as.numeric(idade),  
    grupo_de_idade = case_when(
     idade <10 ~ '0 a 10 anos',
     idade <=17 ~ '11 a 17 anos',
     idade <=23 ~ '18 a 23 anos',
     idade <=29 ~ '24 a 29 anos',
     idade <=39 ~ '30 a 39 anos',
     idade <=49 ~ '40 a 49 anos',
     idade <=59 ~ '50 a 59 anos',
     idade >=60 ~ 'Mais de 60'
    ),
    Ano=lubridate::year(Data)
    ) |> 
  group_by(grupo_de_idade,Gênero,Ano   ) |># ,Gênero  
  summarise(Qtd = n())  |> 
  filter(!is.na(grupo_de_idade) ) |> 
  ungroup() |> 
  group_by(Gênero,Ano) |> 
  filter(Gênero!='Não Informado') |> 
  mutate(Perc= Qtd/sum(Qtd)) 
saveRDS(df_cvli_idade,'Dados/df_cvli_idade.rds')

# 
# 
# GM <- df_cvli_idade |> 
#   filter(Gênero=='Masculino' ) |> 
#   mutate(Perc= Qtd/sum(Qtd)) |> 
#   e_charts(x=grupo_de_idade) |> 
#   e_bar(Perc,stack = "grp2",name='Homem', #bind='perc_masc',
#         label=list(fontSize =14,
#                    formatter =
#                      htmlwidgets::JS(
#                        "function(p) {
#           v = Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(p.value[0]);
#           return(Intl.NumberFormat('pt-BR', { style: 'decimal',minimumFractionDigits: 2, 
#   maximumFractionDigits: 2}).format(p.value[0]*100) + '%' );
#           }"),
#                   
#                    show= T,position= "outside"#,fontWeight= "bold"#,color='green'#
#         )) |> 
#   e_flip_coords() |> 
#   #e_y_axis(offset=30) |> 
#   e_x_axis(
#     show= T ,
#     max=0.35) |>
#   e_grid(left='25%') |> 
#   e_theme('macarons') |> 
#   e_legend( top= "3%") |> 
#   e_group("grp")
# 
# GF <- df_cvli_idade |> 
#   filter(Gênero=='Feminino' ) |> 
#   mutate(Perc= Qtd/sum(Qtd)) |> 
#   e_charts(x=grupo_de_idade) |> 
#   e_bar(Perc,stack = "grp2",name='Mulher', #bind='perc_masc',
#         label=list(fontSize =14,
#                    formatter =
#                      htmlwidgets::JS(
#                        "function(p) {
#           v = Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(p.value[0]);
#           return(Intl.NumberFormat('pt-BR', { style: 'decimal',minimumFractionDigits: 2, 
#   maximumFractionDigits: 2}).format(p.value[0]*100) + '%' );
#           }"),
#                    
#                    show= T,position= "outside"#,fontWeight= "bold"#,color='green'#
#         )) |> 
#   e_flip_coords() |> 
#   #e_y_axis(offset=30) |> 
#   e_x_axis(
#     show= T ,
#     max=0.35) |>
#   e_grid(left='25%') |> 
#   e_theme('macarons') |> 
#   e_legend( top= "3%") |> 
#   e_group("grp") |> # assign group
#   e_connect_group("grp")
# 
# 
# e_arrange(GM,GF,cols=2)
# 

df_cvli_idade |>
  e_charts(x=grupo_de_idade) |> 
  e_bar(Perc,barGap=0,#name='Mulher', #bind='perc_masc',
        label=list(fontSize =14,  rotate = 90, color='black',
                   formatter =
                     htmlwidgets::JS(
                       "function(p) {
          v = Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(p.value[1]);
          return(Intl.NumberFormat('pt-BR', { style: 'decimal',minimumFractionDigits: 2, 
  maximumFractionDigits: 2}).format(p.value[1]*100) + '%' );
          }"),
                   
                   show= T,position= c('27%','-10%')#,fontWeight= "bold"#,color='green'#
        )) |> 
  
  e_y_axis(show= F) |> 
  e_x_axis(
    axisLabel = list(interval= 0,rotate = 45) ) |>
  #e_grid(left='25%') |> 
  e_color(c('#b6a2de','#2ec7c9')) |> 
  e_legend( top= "3%", bottom= 1,right= 0) 
  


    summary(df_cvli_$idade)
  e_theme(name='macarons')
    pop_fort_graf <- pop_fort |>
      rename(masculina=populacao_masculina_pessoas,
             feminina=populacao_feminina_pessoas) |>
      mutate( masculina_inv= masculina*-1,
              seq = 1:dim(pop_fort)[1],
              perc_fem = round(feminina/sum(feminina+masculina)*100,1),
              perc_masc = round(feminina/sum(feminina+masculina)*100,1)
      ) |>
      arrange(-seq)



    b <- pop_fort_graf |>
      e_charts(x=grupo_de_idade) |>
      e_bar(masculina_inv,stack = "grp2",bind=perc_masc,name='Homens',
            label=list(fontSize =14,formatter=  htmlwidgets::JS("
         function (params) {
          return Math.abs(params.name)+'%';}"),
                       show= T,position= "left",fontWeight= "bold"#,color='green'#
            )
      ) |>
      e_bar(feminina,stack = "grp2",bind=perc_fem,name='Mulheres',
            label=list(fontSize =14,formatter=  htmlwidgets::JS("
         function (params) {
          return Math.abs(params.name)+'%';}"),
                       show= T,position= "right",fontWeight= "bold"#,color='blue'#
            )
      ) |>
      e_flip_coords() |>
      e_y_axis(offset=30) |>
      e_x_axis(max=117799,
               min=-127799,
               type= 'value',
               show= F,
               axisLabel=list(
                 formatter= htmlwidgets::JS("function(params) {
                 return Math.abs(params);
               }")
               )
      ) |>
      e_grid(left='20%',) |>
      e_theme('macarons') |>
      e_legend( top= "3%") |>
      e_title(text='População residente em Fortaleza - 2022',subtext="Segundo sexo e grupo de idade" )
    b
    htmlwidgets::saveWidget(b, "C:/Romulo/Dados/Censo2022/temp.html");
    webshot2::webshot("C:/Romulo/Dados/Censo2022/temp.html","C:/Romulo/Dados/Censo2022/piramide_22_.png",
                      vwidth = 900, vheight = 800,delay = 1)
