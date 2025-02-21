# Load library --------------------------------------------------------------------------------


box::use(
  shiny[...],
  bs4Dash[...],
  shinyjs[disable,
          enable,
          delay],
  forcats[fct_reorder],
  echarts4r[e_theme_register,
            renderEcharts4r,
            e_axis_formatter,
            e_tooltip_pointer_formatter,
            e_tooltip_choro_formatter,
            e_title,
            e_radar,
            e_radar_opts,
            e_data,
            e_tooltip_item_formatter,
            e_bar,
            e_line,
            e_effect_scatter,
            e_toolbox_feature,
            e_legend,
            e_theme,
            e_grid,
            e_x_axis,
            e_y_axis,
            e_flip_coords,
            e_charts,
            e_color,
            e_tooltip,
            e_pie,
            e_group,
            e_connect_group,
            e_toolbox,
            e_labels,
            e_chart],
  waiter[...],
  sf[st_drop_geometry,
    #sf_use_s2,
    #st_read,
    ],
  tidyr[pivot_wider,pivot_longer],
  dplyr[#left_join,
        #right_join,
        #bind_rows,
          if_else, 
          n,
          ungroup,
          mutate,
          #rename,
          select,
          filter,
          summarise,
          group_by],
    # readr[read_csv2,read_csv],
    leaflet[leaflet,
            leafletOptions,
            renderLeaflet,
            addProviderTiles,
            providers,
            addPolygons,
            addLegend,
            hideGroup,
            labelOptions,
            addScaleBar,
            setView,
            clearControls,
            clearGroup,
            addLabelOnlyMarkers,
            addMeasure,
            removeMeasure,
            scaleBarOptions,
            highlightOptions,
            addLayersControl,
            colorNumeric,
            colorBin,
            labelFormat,
            leafletProxy,
            colorFactor],
    leaflet.extras[addFullscreenControl,
                   setMapWidgetStyle],
    leafpop[popupTable],
    reactable[reactable,
              colDef,
              renderReactable],
    htmltools[div],
    mapmisc[colourScale],
    writexl[write_xlsx],
    D3plusR[d3plus,
            d3plusSize,
            d3plusUi,
            d3plusColor,
            d3plusDepth,
            d3plusLabels,
            d3plusTitle,
            renderD3plus],
  datamods[select_group_server]
    
)



# Load data -----------------------------------------------------------------------------------


## dados demografia --------------------------------------------------------

bairros_demografia <- readRDS('Dados/bairros_demografia.rds') 
df_faixa <- readRDS('Dados/df_faixa_.rds')
df_cor_ <- readRDS('Dados/df_cor_.rds')
df_sexo <- readRDS('Dados/df_sexo.rds')
shp_Bairros <- readRDS('Dados/shp_bairros_demografia.rds')
Atividade_CNAE <- readRDS("Dados/Atividade_CNAE2.rds")


## dados mercado -----------------------------------------------------------

# Atividade_CNAE <- readRDS("C:/Romulo/Comandos R/Testes/Testes_ar/Atividade_CNAE2.rds")


Tipos_trab2 <- readRDS('Dados/Tipos_trab_2023.rds')


df_desemp <- readRDS('Dados/df_desemp.rds')
df_inf <- readRDS('Dados/df_inf.rds')
df_ocup <- readRDS('Dados/df_ocup.rds') 
df_renda <- readRDS('Dados/df_renda_.rds') # todas as fontes habitual real
df_pnadc <- readRDS('Dados/pnadc_12_23_df_cnae.rds')

## dados violencia ---------------------------------------------------------

df_cvli <- readRDS('Dados/cvli_base.rds') 
bairros_AIS_ <- readRDS('Dados/bairros_AIS_.rds')
df_cvli_idade <- readRDS('Dados/df_cvli_idade.rds')


# dados educação ----------------------------------------------------------

shp_Bairros_alfab <- readRDS('Dados/shp_Bairros_alfab.rds')
bairros_alfab <- readRDS('Dados/tx_bairros_alfab.rds')
Qtd_alfab <- readRDS('Dados/Qtd_alfab.rds')
Tx_faixa_alfab <- readRDS('Dados/Tx_faixa_alfab.rds')
# Load functions ------------------------------------------------------------------------------
bar_chart <- function(label, width = "100%", height = "1rem", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "0.375rem", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}


my_labelFormat <- function(...) {
  fun <- labelFormat(...)
  evalq(
    formatNum <-   function(x) { # era  <-
    format(round(transform(x), digits), trim = TRUE, scientific = FALSE, 
           big.mark = big.mark, decimal.mark = ',')
  }, environment(fun))
  return(fun)
}
# Server --------------------------------------------------------------------------------------

server <- function(input, output, session) {
  
# observe( #input[["Filtro_merc-Ano"]]
#   print(input[["Filtro_merc-V2010"]])
# )
# Controle de carregamento ------------------------------------------------

  
  # # Lista com os IDs das abas
  tab_ids <- c(
    "tab-Abertura", "tab-Demografia","tab-Educação", "tab-Saúde",
    "tab-Mercado", "tab-Violência", "tab-Glossário", "tab-Referências", "tab-Sobre"
  )
  
  observeEvent(req(input$current_tab=="Demografia"), {
    lapply(tab_ids, disable)# Desabilitar os botões do menu
    delay(2000, lapply(tab_ids, enable)) # Espera 2 segundos antes de reativar os botões
  },once = T)
  
  observeEvent(req(input$current_tab=="Educação"), {
    lapply(tab_ids, disable)# Desabilitar os botões do menu
    delay(2000, lapply(tab_ids, enable)) # Espera 2 segundos antes de reativar os botões
  },once = T)
  
  observeEvent(req(input$current_tab=="Mercado"), {
    lapply(tab_ids, disable)# Desabilitar os botões do menu
    delay(2000, lapply(tab_ids, enable)) # Espera 2 segundos antes de reativar os botões
  },once = T)
  
  
  observeEvent(req(input$current_tab=="Violência"), {
    lapply(tab_ids, disable)# Desabilitar os botões do menu
    delay(2000, lapply(tab_ids, enable)) # Espera 2 segundos antes de reativar os botões
  },once = T)
  
  
  
# server demografia -------------------------------------------------------

  observeEvent(input$select_1 ,{
    if(length(input$select_1)==3)  { 
      
      updateSelectizeInput(session = getDefaultReactiveDomain(),
                           inputId = "select_1",
                           selected = input$select_1[2:3])
    } 
    
  },ignoreInit =T)
  
 
  
## Moradores ---------------------------------------------------------------

  
  output$vbox2.1 <- renderValueBox({

    
    valueBox(
      value = format(sum(bairros_demografia[bairros_demografia$NM_BAIRRO==input$select_1[1],'V01006']),  big.mark   = '.',decimal.mark = ',') ,
      subtitle = "Moradores",
      color = "info",
      icon = icon("people-roof"),
      footer = input$select_1[1],
      gradient = T
    )
  })
  
  output$vbox2.2 <- renderValueBox({
    valueBox(
      value = format(sum(bairros_demografia[bairros_demografia$NM_BAIRRO==input$select_1[2],'V01006']),  big.mark   = '.',decimal.mark = ',') ,
      subtitle = "Moradores",
      color = "info",
      icon = icon("people-roof"),
      footer = input$select_1[2],
      gradient = T
    )
  })


## Razao sexo --------------------------------------------------------------


razao_sexo1  <- reactive({ sum(bairros_demografia[bairros_demografia$NM_BAIRRO==input$select_1[1],'V01008'])/
               sum(bairros_demografia[bairros_demografia$NM_BAIRRO==input$select_1[1],'V01007'])*100 })
razao_sexo2  <- reactive({ sum(bairros_demografia[bairros_demografia$NM_BAIRRO==input$select_1[2],'V01008'])/
    sum(bairros_demografia[bairros_demografia$NM_BAIRRO==input$select_1[2],'V01007'])*100 })

output$vbox_mulher.1 <- renderValueBox({
  valueBox(
    value = format(sum(bairros_demografia[bairros_demografia$NM_BAIRRO==input$select_1[1],'V01008']) ,  big.mark   = '.',decimal.mark = ',') ,
    subtitle = "Mulheres",
    color = "fuchsia",
    icon = icon("person-dress"),
    footer = input$select_1[1],
    gradient = T
  )
})
output$vbox_mulher.2 <- renderValueBox({
  valueBox(
    value = format(sum(bairros_demografia[bairros_demografia$NM_BAIRRO==input$select_1[2],'V01008']) ,  big.mark   = '.',decimal.mark = ',') ,
    subtitle = "Mulheres",
    color = "fuchsia",
    icon = icon("person-dress"),
    footer = input$select_1[2],
    gradient = T
  )
})
output$vbox1.1 <- renderValueBox({
    valueBox(
      value = format(round(razao_sexo1(),2) ,  big.mark   = '.',decimal.mark = ',') ,
      subtitle = "Mulheres p/ cada 100 Homem",
      color = "maroon",
      icon = icon("venus-mars"),
      footer = input$select_1[1],
      gradient = T
    )
  })
output$vbox1.2 <- renderValueBox({
    valueBox(
      value = format(round(razao_sexo2(),2) ,  big.mark   = '.',decimal.mark = ',') ,
      subtitle = "Mulheres p/ cada 100 Homem

",
      color = "maroon",
      icon = icon("venus-mars"),
      footer = input$select_1[2],
      gradient = T
    )
  })

## Pizza sexo --------------------------------------------------------------

output$pizza_1.1 <- renderEcharts4r({
  df_sexo |> 
    filter(NM_BAIRRO == input$select_1[1]) |> 
    summarise(Qtd=sum(Qtd)) |> 
    e_charts(Sexo,reorder = F) |> 
    e_pie(Qtd, roseType = "radius", clockwise=F,   radius = c("5%", "65%"),
          label = list(color='black',show = T,formatter = "{b} \n {d}%"),
          emphasis = list(
            label = list( 
              show = T,
              fontSize = 20,
              fontWeight = "bold"))) |>
    e_tooltip() |> 
    e_legend(show=F) |> 
    e_color(c("#2ec7c9", "#b6a2de"))|> 
    e_title(text=input$select_1[1])      }) 
  
output$pizza_1.2 <- renderEcharts4r({
    
    if( !is.na(input$select_1[2])  ){
      
      df_sexo |> 
        filter(NM_BAIRRO == input$select_1[2]) |> 
        summarise(Qtd=sum(Qtd)) |> 
        e_charts(Sexo,reorder = F) |> 
        e_pie(Qtd, roseType = "radius", clockwise=F,   radius = c("5%", "65%"),
              label = list(color='black',show = T,formatter = "{b} \n {d}%"),
              emphasis = list(
                label = list(
                  show = T,
                  fontSize = 20,
                  fontWeight = "bold"))) |>
        e_tooltip() |> 
        e_legend(show=F) |> 
        e_color(c("#2ec7c9", "#b6a2de"))|> 
        e_title(text=input$select_1[2])    
    }
  })
 
## Piramide --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




output$piramide_1.1 <- renderEcharts4r({
  
    df_faixa |> 
     filter(NM_BAIRRO == input$select_1[1]) |> 
      e_charts(x=grupo_de_idade) |> 
      e_bar(Homens,stack = "grp2",bind=perc_masc,name='Homem',
            label=list(fontSize =14,formatter=  htmlwidgets::JS("
       function (params) {
        return Math.abs(params.name)+'%';}"),
                       show= T,position= "left",fontWeight= "bold"#,color='green'#
            )
      ) |> 
      e_bar(Mulheres,stack = "grp2",bind=perc_fem,name='Mulher',
            label=list(fontSize =14,formatter=  htmlwidgets::JS("
       function (params) {
        return Math.abs(params.name)+'%';}"),
                       show= T,position= "right",fontWeight= "bold"#,color='blue'#
            )
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
      e_grid(left='20%') |> 
      e_theme('macarons') |> 
      e_legend( top= "3%")# |> 
    #e_title(text=input$select_1[1])#,subtext="Segundo sexo e grupo de idade" )  
  })  
output$piramide_1.2 <- renderEcharts4r({
  req(length(input$select_1)>=1)
  
    df_faixa |> 
    filter(NM_BAIRRO == input$select_1[2]) |> 
    e_charts(x=grupo_de_idade) |> 
    e_bar(Homens,stack = "grp2",bind=perc_masc,name='Homem',
          label=list(fontSize =14,formatter=  htmlwidgets::JS("
       function (params) {
        return Math.abs(params.name)+'%';}"),
                     show= T,position= "left",fontWeight= "bold"#,color='green'#
          )
    ) |> 
    e_bar(Mulheres,stack = "grp2",bind=perc_fem,name='Mulher',
          label=list(fontSize =14,formatter=  htmlwidgets::JS("
       function (params) {
        return Math.abs(params.name)+'%';}"),
                     show= T,position= "right",fontWeight= "bold"#,color='blue'#
          )
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
    e_grid(left='28%') |> 
    e_theme('macarons') |> 
    e_legend( top= "3%")|> 
    e_title(text=input$select_1[2])#,subtext="Segundo sexo e grupo de idade" )  
})  
output$piramide_2 <- renderEcharts4r({
  req(length(input$select_1)>=1)
  
  df_faixa |> 
    filter(NM_BAIRRO == input$select_1[1]) |> 
    e_charts(x=grupo_de_idade) |> 
    e_bar(Homens,stack = "grp2",bind=perc_masc,name='Homem',
          label=list(fontSize =14,formatter=  htmlwidgets::JS("
       function (params) {
        return Math.abs(params.name)+'%';}"),
                     show= T,position= "left",fontWeight= "bold"#,color='green'#
          )
    ) |> 
    e_bar(Mulheres,stack = "grp2",bind=perc_fem,name='Mulher',
          label=list(fontSize =14,formatter=  htmlwidgets::JS("
       function (params) {
        return Math.abs(params.name)+'%';}"),
                     show= T,position= "right",fontWeight= "bold"#,color='blue'#
          )
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
    e_grid(left='28%') |> 
    e_theme('macarons') |> 
    e_legend( top= "3%") |>
    e_title(text=input$select_1[1])
})  

# Barras cor

output$raca1 <- renderEcharts4r({
df_cor_ |> 
  filter(NM_BAIRRO== input$select_1[1]) |>  #
  group_by(Sexo) |> 
  mutate(Perc = Qtd/sum(Qtd)) |>   
  e_chart(x=Cor_raça,name='Moradores') |> 
  e_bar(Perc) |> 
  e_color(c("#2ec7c9","#b6a2de" )) |> 
  e_toolbox(iconStyle= list(
    color= "rgba(35, 210, 32, 1)"
  )) |>
    e_tooltip(
      trigger="axis",formatter = e_tooltip_pointer_formatter(
        style = c( "percent"),digits = 1) )|>  
    e_legend( orient= "vertical",
                   right= 0) |> 
    e_y_axis(show=F) |> 
    e_grid(top = "-5%") |> 
    e_group("decomp") 
})

output$raca2 <- renderEcharts4r({
  df_cor_ |> 
    filter(NM_BAIRRO==input$select_1[2]) |> 
    group_by(Sexo) |> 
    mutate(Perc = Qtd/sum(Qtd)) |>   
    e_chart(x=Cor_raça,name='Moradores') |> 
    e_bar(Perc) |> 
    e_color(c("#2ec7c9","#b6a2de" )) |> 
    e_toolbox(iconStyle= list(
      color= "rgba(35, 210, 32, 1)"
    )) |>
    e_tooltip(
      trigger="axis",formatter = e_tooltip_pointer_formatter(
        style = c( "percent"),digits = 1) )|>  
    e_legend( orient= "vertical",
              right= 0) |> 
    e_y_axis(show=F) |> 
    e_grid(top = "-5%")|>
    e_group("decomp") |> 
    e_connect_group("decomp")
})

## Mapa demografia ---------------------------------------------------------

output$map_bairro <-  renderLeaflet({
  leaflet( ) |>
    setMapWidgetStyle(list(background = "white"))  |> 
    addProviderTiles(providers$CartoDB.PositronNoLabels, group = "CartoDB.PositronNoLabels")|> 
    # addLayersControl(
    #   overlayGroups = c(
    #     "Bairros",
    #     'id_final')) |>
    addScaleBar(position = "bottomright",
                options = scaleBarOptions(imperial = F))  |>
     setView(-38.51782, -3.795804, zoom = 11) |> #-38.52782, -3.785804,
    addFullscreenControl(pseudoFullscreen = T)
}) 

output$table_bairro <- renderReactable({
  
  
  areaB <- round(sum(shp_Bairros$Área_km2),2)      
  pessoB <- sum(shp_Bairros$Pessoas)   
  domiB <- sum(shp_Bairros$Domicílios)   
  densB <-round( sum(shp_Bairros$Pessoas)/sum(shp_Bairros$Área_km2) )
  medB <- round(sum(shp_Bairros$Pessoas)/sum(shp_Bairros$Domicílios),2)
  razB <- round(sum(shp_Bairros$V01008)/sum(shp_Bairros$V01007)*100,2)
  percB <- round(sum(shp_Bairros$V01008)/sum(shp_Bairros$V01006)*100,2)
  mulB <- round(sum(bairros_demografia[bairros_demografia$NM_BAIRRO=="FORTALEZA",'V01008']),2)
  
  shp_Bairros <-   shp_Bairros |>
    st_drop_geometry() |> 
    select(-x,-y,-V01006:-V01007,-CD_BAIRRO,-Média_moradores) |> 
    mutate(Densidade=round(Densidade),
           Área_km2=round(Área_km2,2),
          # Média_moradores=round(Média_moradores,2),
           Razão=round(Razão,2))
  
  
  reactable(shp_Bairros[,c(1,6,3,7,8,4,5,2)],
            bordered = TRUE,compact = TRUE,defaultPageSize = 15,
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
              Razão = colDef(name = "Razão de sexo", align = "left", footer=format(razB,big.mark = ".",decimal.mark=","), 
                                cell = function(value) {
                                  width <- paste0(value / max(shp_Bairros$Razão) * 100,"%")
                                  value <- format(value,big.mark = ".",decimal.mark=",")
                                  value <- format(value, width = 7, justify = "right")
                                  bar_chart(value, width = width,fill = "#720072",background = "#e1e1e1")
                                }, style = list(fontFamily = "monospace", whiteSpace = "pre")),
              Percentual = colDef(name='Percentual',  align = "left", footer=format(percB,big.mark = ".",decimal.mark=","), 
                              cell = function(value) {
                                width <- paste0(value / max(shp_Bairros$Percentual) * 100,"%")
                                value <- format(value,big.mark = ".",decimal.mark=",")
                                value <- format(value, width = 7, justify = "right")
                                bar_chart(value, width = width,fill = "pink",background = "#e1e1e1")
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
              V01008 = colDef(name = "Mulheres",  align = "left", footer=format(mulB,big.mark = ".",decimal.mark=","), 
                              cell = function(value) {
                                width <- paste0(value / max(shp_Bairros$V01008) * 100,"%")
                                value <- format(value,big.mark = ".",decimal.mark=",")
                                value <- format(value, width = 7, justify = "right")
                                bar_chart(value, width = width,fill = "#E016AB",background = "#e1e1e1")
                              }, style = list(fontFamily = "monospace", whiteSpace = "pre"))
            )
  )
  
  
})

observeEvent(req(input$current_tab=='Demografia'), {
  
  leafletProxy('map_bairro') |>
    addPolygons(data=shp_Bairros,
                weight=.6,
                fillColor=~pal_perc_ba(shp_Bairros$Percentual),
                color = 'purple',
                opacity = 1,
                group = 'id_final',
                popup =  popupTable(bairro_pop_,zcol = c("Bairro", "Pessoas",
                                                         "Percentual"),
                                    feature.id = FALSE,
                                    row.numbers = FALSE),
                dashArray = "0",
                fillOpacity =1 ,
                label = ~Bairro,
                highlightOptions=highlightOptions(
                  color = "#500050", weight = 3, bringToFront = F)
    ) |>
    addLegend(data = shp_Bairros ,"bottomright", pal = pal_perc_ba, values = ~Percentual,
              title = "% mulheres",
              opacity = 1 )
  
},once=T)
# 
{ # aux
  pal_ba <- colorNumeric(
    palette ='YlGnBu',# "YlGnBu",
    domain = shp_Bairros$Pessoas
  )
  
  pal_dens_ba<- colorNumeric(
    palette ='YlOrBr', #"",
    domain = shp_Bairros$Densidade
  )
  
  pal_perc_ba<- colorNumeric(
    palette = 'BuPu',
    domain = shp_Bairros$Percentual
  )

  pal_razao_ba<- colorNumeric(
    palette = "RdPu",
    domain = shp_Bairros$Razão
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
    clearControls() 
  
  if(input$tabs_ba == '% de mulheres' )
  {
    
    
    leafletProxy('map_bairro') |>
      addPolygons(data=shp_Bairros,
                  weight=.6,
                  fillColor=~pal_perc_ba(shp_Bairros$Percentual),
                  color = 'purple',
                  opacity = 1,
                  group = 'id_final',
                  popup =  popupTable(bairro_pop_,zcol = c("Bairro", "Pessoas",
                                                           "Percentual"),
                                      feature.id = FALSE,
                                      row.numbers = FALSE),
                  dashArray = "0",
                  fillOpacity =1 ,
                  label = ~Bairro,
                  highlightOptions=highlightOptions(
                    color = "#500050", weight = 3, bringToFront = F)
      ) |>
      addLegend(data = shp_Bairros,"bottomright", pal = pal_perc_ba, values = ~Percentual,
                title = "% Mulheres",
                opacity = 1
      )
  }
  
  if(input$tabs_ba == 'Razão sexo' )
  {
    
    
    leafletProxy('map_bairro') |>
      addPolygons(data=shp_Bairros,
                  weight=.6,
                  fillColor=~pal_razao_ba(shp_Bairros$Razão),
                  color = 'purple',
                  opacity = 1,
                  group = 'id_final',
                  popup =  popupTable(bairro_pop_,zcol = c("Bairro", "Pessoas",
                                                           "Razão"),
                                      feature.id = FALSE,
                                      row.numbers = FALSE),
                  dashArray = "0",
                  fillOpacity =1 ,
                  label = ~Bairro,
                  highlightOptions=highlightOptions(
                    color = "#500050", weight = 3, bringToFront = F)
      ) |>
      addLegend(data = shp_Bairros,"bottomright", pal = pal_razao_ba, values = ~Razão,
                title = "Razão sexo",
                opacity = 1
      )
  }
  
  if(input$tabs_ba == 'População' )
  {
    
    
    leafletProxy('map_bairro') |>
      addPolygons(data=shp_Bairros,
                  weight=.6,
                  fillColor=~pal_ba(shp_Bairros$Pessoas),
                  color = 'purple',
                  opacity = 1,
                  group = 'id_final',
                  popup =  popupTable(bairro_pop_,zcol = c("Bairro", "Pessoas",
                                                           "Domicílios"
                                                           ),
                                      feature.id = FALSE,
                                      row.numbers = FALSE),
                  dashArray = "0",
                  fillOpacity =1 ,
                  label = ~Bairro,
                  highlightOptions=highlightOptions(
                    color = "#500050", weight = 3, bringToFront = F)
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
                  color = 'purple',
                  opacity = 1,
                  group = 'id_final',
                  popup =  popupTable(bairro_pop_,zcol = c("Bairro", "Pessoas",
                                                           "Densidade",
                                                           "Área_km2"),
                                      feature.id = FALSE,
                                      row.numbers = FALSE),
                  dashArray = "0",
                  fillOpacity =1 ,
                  label = ~Bairro,
                  highlightOptions=highlightOptions(
                    color = "#500050", weight = 3, bringToFront = F)
      ) |>
      addLegend(data = shp_Bairros,position ="bottomright", pal = pal_dens_ba,
                values = ~Densidade,
                labFormat = my_labelFormat(
                  big.mark='.',
                  digits=0),
                title = "Densidade",
                opacity = 1
      )
    
    
  }
  
},ignoreInit = T) 








# Server mercado ----------------------------------------------------------

#output$pedro <- renderText({'<div class="flourish-embed flourish-hierarchy" data-src="visualisation/21354009"><script src="https://public.flourish.studio/resources/embed.js"></script><noscript><img src="https://public.flourish.studio/visualisation/21354009/thumbnail" width="100%" alt="hierarchy visualization" /></noscript></div>' })


# Tabela renda ------------------------------------------------------------

output$tab_ocup <- renderReactable({
  
   rend <-  df_pnadc |>  
      filter(Ano == 2023) |> 
      group_by(`Ocupação e categoria do emprego do trabalho`,V2007) |> #V2007 "Denominação"   "Denom_Seção"   "Denom_Divisão" "Composição"   
      summarise(Rendimento = weighted.mean(VD4019*CO2,w = V1032,na.rm =TRUE)) |>
      mutate(Rendimento = round(Rendimento,2)) |> 
      pivot_wider(names_from = V2007, values_from =Rendimento ) |> 
      select(1,3,2) |> 
      drop_na()

  
   rend |> 
      reactable(bordered = TRUE,compact = T,defaultPageSize = 15,
                highlight = TRUE, 
                defaultColDef = colDef(
                  style = list(fontSize = 14,headerClass = "sort-header"
                               )#headerClass = "sort-header",
                ),
                columns = list(
                  
                  Homem = colDef(name = "Homens (R$)", align = "left",  width = 200,
                                  cell = function(value) {
                                    width <- paste0(value / max(rend$Homem) * 100,'%')
                                    value <- format(value,big.mark = ".",decimal.mark=",")
                                    value <- format(value, width = 9, justify = "right")
                                    bar_chart(value, width = width,background = "#e1e1e1",fill = '#2ec7c9')
                                  }, style = list(fontFamily = "monospace", whiteSpace = "pre")),
                  Mulher = colDef(name = "Mulheres (R$)", align = "left", width = 200,
                                   cell = function(value) {
                                     width <- paste0(value / max(rend$Homem) * 100,'%')
                                     value <- format(value,big.mark = ".",decimal.mark=",")
                                     value <- format(value, width = 9, justify = "right")
                                     bar_chart(value, width = width,background = "#e1e1e1",fill ='#b6a2de')
                                   }, style = list(fontFamily = "monospace", whiteSpace = "pre")) 
                )
      )

})

## reativos mercados -------------------------------------------------------



Tipos_trab2_ano <- reactive({
  Tipos_trab2[Tipos_trab2$Ano %in% input$Ano_filter,] 
})

Filtro_Tipos_trab <- { select_group_server(
  id = "Filtro_merc",
  data_r = Tipos_trab2_ano,
  vars_r = reactive(c('V2010','VD3004','idadeEco2','Composição'))
)}



Tipos_trab_Cnae <- reactive( aggregate(Qtd.~ Ano+V2007+Denominação+Denom_Seção+Denom_Divisão,
                                       Filtro_Tipos_trab(),sum))



## treemap -----------------------------------------------------------------
output$F_cnaes <- renderD3plus({
    
 

  Tipos_trab_Cnae_G <-aggregate(Qtd.~ Ano+Denominação+Denom_Seção+Denom_Divisão,
                                Tipos_trab_Cnae(),sum)


  d3plus(
    data = if(nrow(Tipos_trab_Cnae_G)==0) {
      data.frame(Qtd.= NaN)
    } else {
      Tipos_trab_Cnae_G
    },
    type = "tree_map",
    id = c("Denom_Seção", "Denominação"), #,"Denominação" Denom_Divisão
    locale = "pt_BR",
    width = "100%",
    currency = "",
    number_text = c("Mil", "Milhão", "Milhões", "Bilhão", "Bilhões"),
    height = 500)  |>
    d3plusSize("Qtd.") |>
    # d3plusUi(list( list(method = "depth", type = "drop",
    #                     value = list(list('1ª Seção' = 0), list('2ª Divisão' = 1)))))  |>  #,list('3ª Denominação' = 2)
    #
    d3plusColor("Denom_Seção")  |>
    d3plusDepth(0) |>
    d3plusLabels(value = TRUE, valign = "top")  |>
    d3plusTitle(value = paste0('Geral ', input$Ano_filter), font = list(size = 22, weight = 900),
                total = list(value = list(prefix = "Total: "),
                             font = list(size = 16, weight = 900)))


})
output$F_cnaes_form <- renderD3plus({


  d3plus(
    data = if(nrow(Tipos_trab_Cnae()[Tipos_trab_Cnae()$V2007=="Mulher",])==0) {
      data.frame(Qtd.= NaN)
    } else {
      Tipos_trab_Cnae()[Tipos_trab_Cnae()$V2007=="Mulher",]
    },
    type = "tree_map",
    id = c("Denom_Seção"), #, "Denom_Divisão"),
    locale = "pt_BR",
    width = "100%",
    currency = "",
    number_text = c("Mil", "Milhão", "Milhões", "Bilhão", "Bilhões"),
    height = 500)  |> 
    d3plusSize("Qtd.")  |> 
    # d3plusUi(list( list(method = "depth", type = "drop",
    #                     value = list(list('1ª Seção' = 0), list('2ª Divisão' = 1)))))  |> 

    d3plusColor("Denom_Seção")  |> 
    d3plusDepth(0) |> 
    d3plusLabels(value = TRUE, valign = "top")  |> 
    d3plusTitle(value = paste0('Mulher ', input$Ano_filter), font = list(size = 22, weight = 900),
                total = list(value = list(prefix = "Total: "),
                             font = list(size = 16, weight = 900)))


})
output$F_cnaes_inform <- renderD3plus({
  req(Filtro_Tipos_trab())


  d3plus(
    data = if(nrow(Tipos_trab_Cnae()[Tipos_trab_Cnae()$V2007=="Homem",])==0) {
      data.frame(Qtd.= NaN)
    } else {
      Tipos_trab_Cnae()[Tipos_trab_Cnae()$V2007=="Homem",]
    },
    type = "tree_map",
    id = c("Denom_Seção"),#, "Denom_Divisão"),
    locale = "pt_BR",
    width = "100%",
    currency = "",
    number_text = c("Mil", "Milhão", "Milhões", "Bilhão", "Bilhões"),
    height = 500) |> 
    d3plusSize("Qtd.") |> 
    # d3plusUi(list( list(method = "depth", type = "drop",
    #                     value = list(list('1ª Seção' = 0), list('2ª Divisão' = 1)))))  |> 

    d3plusColor("Denom_Seção") |> 
    d3plusDepth(0) |> 
    d3plusLabels(value = TRUE, valign = "top")  |> 
    d3plusTitle(value = paste0('Homem ', input$Ano_filter), font = list(size = 22, weight = 900),
                total = list(value = list(prefix = "Total: "),
                             font = list(size = 16, weight = 900)))

})


## informalidade -----------------------------------------------------------



output$ocupação_sexo <-   renderEcharts4r({
 
  e_charts(data = df_ocup |> group_by(V2007), x=Ano) |> 
    e_legend(selected=list('Geral'=F),right= 0,top= "3%") |> 
    e_line(Ocupação,symbolSize=0) |> 
    e_tooltip(trigger = c("axis"),
              e_tooltip_pointer_formatter(locale = "PT-BR")
    ) |> 
    e_color(c('#008acd','#2ec7c9','#b6a2de')) |> 
    e_y_axis(name="Qtd.",min=500,formatter=e_axis_formatter(style = c("decimal"),
                                                  locale = 'PT-BR') )
  
# 
# 
#     e_charts(data = df_ocup[df_ocup$V2007=='Homem',], x=Ano) |>
#     e_legend(selected=list('Geral'=F), bottom= 0,right= 0) |> 
#     e_line(Qtd.,smooth=T, color='#2ec7c9',legend =F,symbolSize= 10,
#            emphasis=list( focus= 'series' ),
#            endLabel=list( show=T, color='#2ec7c9',fontWeight= "bold" ,fontSize =10,
#                           formatter= htmlwidgets::JS(" function (params) { return 'Homem' }")))  |>
#     e_y_axis(show=F)  |>
#     e_data(data=df_ocup[df_ocup$V2007=='Mulher',] , x= Ano    ) |>
#     e_line(Qtd.,smooth=T,color='#b6a2de',legend =F,symbolSize= 10,
#            emphasis=list( focus= 'series' ),
#            endLabel=list( show=T, color='#b6a2de',fontWeight= "bold" ,fontSize =10,
#                           formatter= htmlwidgets::JS(" function (params) { return 'Mulher' }"))) |> 
#     e_data(data=df_ocup[df_ocup$V2007=='Geral',] , x= Ano    ) |>
#     e_line(Qtd.,smooth=T,color='#008acd', legend = T,symbolSize= 10,
#            emphasis=list( focus= 'series' ),
#            endLabel=list( show=T, color='#008acd',fontWeight= "bold" ,fontSize =10,
#                           formatter= htmlwidgets::JS(" function (params) { return 'Geral' }"))) |> 
#     e_tooltip(trigger="item", formatter =
#                    htmlwidgets::JS("function(p) {
#           v = Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(p.value[1]);
#           return('<strong>' + p.seriesName + '</strong>' +
#           '<br>Pessoas ocupadas: ' + Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(p.value[1]) +
#           '<br>Ano: ' +p.value[0]  );}"),textStyle=list(fontFamily="arial", fontSize=13)) |> 
#     e_grid(top='2%') 
})

output$perc_informal_sexo <-   renderEcharts4r({
  
  e_charts(data = df_inf |> group_by(V2007), x=Ano) |> 
    e_legend(selected=list('Geral'=F),right= 0,top= "3%") |> 
    e_line(`Taxa de Informalidade`,symbolSize=0) |> 
    e_tooltip(trigger = c("axis"),
              e_tooltip_pointer_formatter(locale = "PT-BR",style = c("percent"),digits = 1,
              )) |> 
    e_color(c('#008acd','#2ec7c9','#b6a2de')) |> 
    e_y_axis(formatter=e_axis_formatter(style = c("percent"),
                                        locale = 'PT-BR') )
  # 
  #   e_charts(data = df_inf[df_inf$V2007=='Homem',], x=Ano) |>
  #   e_legend(selected=list('Geral'=F), bottom= 0,right= 0) |> 
  #   e_line(Informalidade,smooth=T, color='#2ec7c9',legend =F,symbolSize= 10,
  #          emphasis=list( focus= 'series' ),
  #          endLabel=list( show=T, color='#2ec7c9',fontWeight= "bold" ,fontSize =10,
  #                         formatter= htmlwidgets::JS(" function (params) { return 'Homem' }")))  |>
  #   e_y_axis(show=F,formatter = e_axis_formatter("percent", digits = 0),min=0.3)  |>
  #   e_data(data=df_inf[df_inf$V2007=='Mulher',] , x= Ano    ) |>
  #   e_line(Informalidade,smooth=T,color='#b6a2de',legend =F,symbolSize= 10,
  #            emphasis=list( focus= 'series' ),
  #            endLabel=list( show=T, color='#b6a2de',fontWeight= "bold" ,fontSize =10,
  #                           formatter= htmlwidgets::JS(" function (params) { return 'Mulher' }"))) |> 
  #   e_data(data=df_inf[df_inf$V2007=='Geral',] , x= Ano    ) |>
  #   e_line(Informalidade,smooth=T,color='#008acd', legend = T,symbolSize= 10,
  #            emphasis=list( focus= 'series' ),
  #            endLabel=list( show=T, color='#008acd',fontWeight= "bold" ,fontSize =10,
  #                           formatter= htmlwidgets::JS(" function (params) { return 'Geral' }"))) |> 
  #   e_tooltip(trigger="item", formatter =
  #                 htmlwidgets::JS(
  #                   "function(p) {
  #         v = Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(p.value[1]);
  #         return('<strong>' + p.seriesName + '</strong>' +
  #         '<br>Taxa de informalidade: ' + Intl.NumberFormat('pt-BR', { style: 'decimal',minimumFractionDigits: 2, 
  # maximumFractionDigits: 2}).format(p.value[1]*100) + '%'+
  #         '<br>Ano: ' +p.value[0]  );
  #         }"), textStyle=list(fontFamily="arial", fontSize=13)) |> 
  #   e_grid(top='2%')
  #   
})

output$taxa_desemp_fort <-   renderEcharts4r({   

  e_charts(data = df_desemp |> group_by(V2007), x=Ano) |> 
    e_legend(selected=list('Geral'=F),right= 0,top= "3%") |> 
    e_line(`Taxa de Desocupação`,symbolSize=0) |> 
    e_tooltip(trigger = c("axis"),
              e_tooltip_pointer_formatter(locale = "PT-BR",style = c("percent"),digits = 1,
    )) |> 
    e_color(c('#008acd','#2ec7c9','#b6a2de')) |> 
    e_y_axis(formatter=e_axis_formatter(style = c("percent"),
                                                  locale = 'PT-BR') )
  
#   
#   
# e_charts(data = df_desemp[df_desemp$V2007=='Homem',], x=Ano) |> # HOMEM
#   e_legend(selected=list('Geral'=F), bottom= 0,right= 0) |> 
#   e_line(Tx_desocupacao ,smooth=T, color='#2ec7c9',legend =F,symbolSize= 10,
#          emphasis=list( focus= 'series' ),
#          endLabel=list( show=T, color='#2ec7c9',fontWeight= "bold" ,fontSize =10,
#                         formatter= htmlwidgets::JS(" function (params) { return 'Homem' }")))  |>
#   e_y_axis(show=F,formatter = e_axis_formatter("percent", digits = 0))  |>
#   e_data(data=df_desemp[df_desemp$V2007=='Mulher',] , x= Ano    ) |>  #MULHER
#   e_line(Tx_desocupacao ,smooth=T,color='#b6a2de',legend =F,symbolSize= 10,
#          emphasis=list( focus= 'series' ),
#          endLabel=list( show=T, color='#b6a2de',fontWeight= "bold" ,fontSize =10,
#                         formatter= htmlwidgets::JS(" function (params) { return 'Mulher' }"))) |> 
#   e_data(data=df_desemp[df_desemp$V2007=='Geral',] , x= Ano    ) |>  #GERAL
#   e_line(Tx_desocupacao ,smooth=T,color='#008acd', legend = T,symbolSize= 10,
#          emphasis=list( focus= 'series' ),
#          endLabel=list( show=T, color='#008acd',fontWeight= "bold" ,fontSize =10,
#                         formatter= htmlwidgets::JS(" function (params) { return 'Geral' }"))) |> 
#   e_tooltip(trigger="item", formatter =
#               htmlwidgets::JS(
#                 "function(p) {
#           v = Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(p.value[1]);
#           return('<strong>' + p.seriesName + '</strong>' +
#           '<br>Taxa de desemprego: ' + Intl.NumberFormat('pt-BR', { style: 'decimal',minimumFractionDigits: 2, 
#   maximumFractionDigits: 2}).format(p.value[1]*100) + '%'+
#           '<br>Ano: ' +p.value[0]  );
#           }"), textStyle=list(fontFamily="arial", fontSize=13)) |> 
#     e_grid(top='2%')
#   
  
}) 

output$renda_fort <-   renderEcharts4r({   
  
  e_charts(data = df_renda |> group_by(V2007), x=Ano) |> 
    e_legend(selected=list('Geral'=F),right= 0,top= "3%") |> 
    e_line(Renda,symbolSize=0) |> 
    e_tooltip(trigger = c("axis"),
              e_tooltip_pointer_formatter(locale = "PT-BR")
              ) |> 
    e_color(c('#008acd','#2ec7c9','#b6a2de')) |> 
    e_y_axis(name="R$",formatter=e_axis_formatter(style = c("decimal"),
      locale = 'PT-BR') )
  # 
  # 
  # e_charts(data = df_renda[df_renda$V2007=='Homem',], x=Ano) |> # HOMEM
  #  # e_legend(selected=list('Geral'=F), bottom= 0,right= 0) |> 
  #   e_line(renda ,smooth=T, color='#2ec7c9',legend =T,symbolSize= 0,
  #          #emphasis=list( focus= 'series' ),
  #          # endLabel=list( show=T, color='#2ec7c9',fontWeight= "bold" ,fontSize =10,
  #          #                formatter= htmlwidgets::JS(" function (params) { return 'Homem' }"))
  #          )  |>
  #   e_tooltip(trigger="axis") |> 
  #   e_y_axis(show=F,formatter = e_axis_formatter("percent", digits = 0))  |>
  #   e_data(data=df_renda[df_renda$V2007=='Mulher',] , x= Ano    ) |>  #MULHER
  #   e_line(renda ,smooth=T,color='#b6a2de',legend =F,symbolSize= 0,
  #          emphasis=list( focus= 'series' ),
  #          endLabel=list( show=T, color='#b6a2de',fontWeight= "bold" ,fontSize =10,
  #                         formatter= htmlwidgets::JS(" function (params) { return 'Mulher' }"))) |> 
  #   e_data(data=df_renda[df_renda$V2007=='Total',] , x= Ano    ) |>  #GERAL
  #   e_line(renda ,smooth=T,color='#008acd', legend = T,symbolSize= 0,
  #          emphasis=list( focus= 'series' ),
  #          endLabel=list( show=T, color='#008acd',fontWeight= "bold" ,fontSize =10,
  #                         formatter= htmlwidgets::JS(" function (params) { return 'Geral' }"))) |> 
  #   e_tooltip(
  #     # trigger="item", formatter =
  #     #           htmlwidgets::JS(
  #     #             "function(p) {
  #     #     v = Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(p.value[1]);
  #     #     return('<strong>' + p.seriesName + '</strong>' +
  #     #     '<br>Rendimento médio: ' + Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(p.value[1]) + 'R$' +
  #     #     '<br>Ano: ' +p.value[0]  );
  #     #     }"), textStyle=list(fontFamily="arial", fontSize=13)
  #     )
  
  
}) 





# Server Violencia ---------------------------------------------------------------
output$text_ano_mapa <- renderText({paste0('Caso de CVLI por AIS ', input$select_ano_viol)}) 
output$text_ano_meios <- renderText({paste0('CVLI por meios empregados ', input$select_ano_viol)}) 
output$text_ano_etária <- renderText({paste0('CVLI por faixa etária ', input$select_ano_viol)}) 
output$text_ano_semana <- renderText({paste0('CVLI por dia da semana ', input$select_ano_viol)}) 
  
  
output$viol_femin<-   renderEcharts4r({
  
  df_cvli |> 
    filter(Natureza == 'FEMINICÍDIO') |> 
    group_by(mes = lubridate::floor_date(Data, "year"),Gênero    ) |> 
    summarise(Qtd. = n()) |> 
    mutate(mes = format(mes,'%Y')) |> 
    group_by(Gênero) |>  
    e_chart(x = mes) |>
    e_line(Qtd.,name ="Feminicídios" , symbolSize= 10) |> 
    e_tooltip(trigger = c("axis")) |> 
    e_color(c('#b6a2de','#2ec7c9','#008acd')) |> 
    e_legend(selected=list('Masculino'=T,'Não Informado'=F))  |> 
    e_labels()
  
})

output$viol_sexo_ano <-   renderEcharts4r({
  
  df_cvli |> 
    group_by(mes = lubridate::floor_date(Data, "year"),Gênero    ) |> 
    summarise(Qtd. = n()) |> 
    mutate(mes = format(mes,'%Y')) |> 
    group_by(Gênero) |>  
    e_chart(x = mes) |>
    e_line(Qtd.,symbolSize= 0) |> 
    e_tooltip(trigger = c("axis"),
              e_tooltip_pointer_formatter(locale = "PT-BR")) |> 
    e_color(c('#b6a2de','#2ec7c9','#008acd')) |> 
    e_legend(selected=list('Masculino'=T,'Não Informado'=F)) |> 
    e_labels()  |> 
    e_y_axis(formatter=e_axis_formatter(
    locale = 'PT-BR') ) 
})

output$viol_sexo <-   renderEcharts4r({
  
  df_cvli |> 
  group_by(mes = lubridate::floor_date(Data, "month"),Gênero    ) |> 
    summarise(Qtd. = n()) |> 
    mutate(mes = format(mes,'%Y-%b')) |> 
    group_by(Gênero) |>  
  e_chart(x = mes) |>
  e_line(Qtd.,symbolSize= 0) |> 
  e_tooltip(trigger = c("axis")) |> 
  e_color(c('#b6a2de','#2ec7c9','#008acd')) |> 
  e_legend(selected=list('Masculino'=T,'Não Informado'=F)) 

  })

output$viol_semana <-   renderEcharts4r({

  
temp <-   df_cvli |> 
    filter(Gênero != 'Não Informado', Ano ==input$select_ano_viol) |> # input$select_ano_viol) |> 
    group_by(Semana,Gênero) |>  #
    summarise(Qtd. = n()) |> 
    group_by(Gênero) |> 
    mutate(Perc. = round(Qtd./sum(Qtd.),2)) |> 
    pivot_wider(
      names_from = Gênero,
      values_from = c(Perc.,Qtd.))
maximo <- max( temp$Perc._Feminino, temp$Perc._Masculino)

temp|> 
    e_chart(x=Semana) |> 
    e_radar(Perc._Feminino,max = maximo,name='Mulher',emphasis=list(
      lineStyle=list(width= 4 ))) |> 
    e_radar_opts( splitLine=list(show=T),axisLine=list(show=T),axisName=list(color='black')) |>
    e_radar(Perc._Masculino,max = maximo,name='Homem') |> 
    e_tooltip(#formatter = e_tooltip_pie_formatter("percent")
    #   trigger = c("axis"),
    # formatter = e_tooltip_choro_formatter("percent")
  ) |> 
  #   e_tooltip(trigger="item", formatter =
  #               htmlwidgets::JS(
  #                 "function(p) { v = Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(p.value[1]);
  #         return('<strong>' + p.name + '</strong>' +
  #         '<br>Percentual: ' + Intl.NumberFormat('pt-BR', { style: 'decimal',minimumFractionDigits: 1, 
  # maximumFractionDigits: 1}).format(p.value[1]*100) + '%')}"))  |> 
    e_color(c('#b6a2de','#2ec7c9')) |> 
    e_legend(right=1) 
  

})

output$viol_meio_H <-   renderEcharts4r({
 df_cvli |>
  filter(Gênero=='Masculino' , Ano == input$select_ano_viol) |> 
  group_by(`Meio Empregado` ) |> 
  summarise(Qtd. = n()) |> 
  e_chart(x=`Meio Empregado`) |> 
  e_pie(Qtd., roseType = "radius", clockwise=F,   radius = c("20%", "50%"),
        label = list(color='black',show = T,formatter = "{b} \n {d}%"),
        emphasis = list(
          label = list( 
            show = T,
            fontSize = 18,
            fontWeight = "bold"))) |>
  e_legend(show=F) |> 
  e_title(text='Homem') 
})

output$viol_meio_M <- renderEcharts4r({
  
  df_cvli |>
  filter(Gênero=='Feminino', Ano == input$select_ano_viol) |> 
  group_by(`Meio Empregado` ) |> 
  summarise(Qtd. = n()) |> 
  e_chart(x=`Meio Empregado`) |> 
  e_pie(Qtd., roseType = "radius", clockwise=F,   radius = c("20%", "50%"),
        label = list(color='black',show = T,formatter = "{b} \n {d}%"),
        emphasis = list(
          label = list( 
            show = T,
            fontSize = 18,
            fontWeight = "bold"))) |>
  e_legend(show=F) |> 
  e_title(text='Mulher')
})

output$faixa_etaria <-   renderEcharts4r({
df_cvli_idade |> 
  filter(Gênero!='Não Informado', Ano == input$select_ano_viol) |> 
  group_by(Gênero) |> 
  mutate(Perc= Qtd/sum(Qtd)) |> 
  e_charts(x=grupo_de_idade) |> 
  e_bar(Perc,barGap=0.1,#name='Mulher', #bind='perc_masc',
        label=list(fontSize =14,  rotate = 90, color='black',
                   formatter =
                     htmlwidgets::JS(
                       "function(p) {
          v = Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(p.value[1]);
          return(Intl.NumberFormat('pt-BR', { style: 'decimal',minimumFractionDigits: 1, 
  maximumFractionDigits: 1}).format(p.value[1]*100) + '%' );
          }"),
                   
                   show= T,position= c('27%','-10%')#,fontWeight= "bold"#,color='green'#
        )) |> 
  
  e_y_axis(show= F) |> 
  e_x_axis(
    axisLabel = list(interval= 0,rotate = 45,color='black') ) |>

  e_color(c('#b6a2de','#2ec7c9')) |> 
  e_legend( top= "3%", bottom= 1,right= 0) 
})


## Mapa Violencia ----------------------------------------------------------


df_bairros_AIS<-reactive({
  bairros_AIS_ |> 
    filter(Ano == input$select_ano_viol)
})

output$map <- renderLeaflet({
  
  pal_viol<- colorNumeric(
    palette = 'inferno',
    domain = df_bairros_AIS()$Qtd,
    reverse =T
  ) 
  
  leaflet(options = leafletOptions(zoomControl = FALSE)) |>
    addProviderTiles(providers$CartoDB.PositronNoLabels, group = "CartoDB.PositronNoLabels") |> 
    addPolygons(data = df_bairros_AIS(),
                fillOpacity = 0.5,
                color = "black",
                stroke = TRUE,
                weight = 1,
                popup = popupTable(df_bairros_AIS() ,
                                   zcol = c("AIS","Qtd",'Ano'),
                                   feature.id = FALSE,
                                   row.numbers = FALSE) ,
                fillColor=~pal_viol(df_bairros_AIS()$Qtd),
                layerId = ~AIS ,
                group = "regions",
                label = ~AIS) |> 
    addLegend(data = df_bairros_AIS() ,"bottomright", pal = pal_viol, values = ~Qtd,
              title = "CVLI",
              opacity = 1 )
}) 



#  Serve Educação ----------------------------------------------------------------


output$alf_M <- renderValueBox({
valueBox(
  value = paste0(format( bairros_alfab[bairros_alfab$NM_BAIRRO==selected() ,"Tx_alfab_mais_de_15_M"]*100  ,digits= 2  , nsmall = 2, big.mark   = '.',decimal.mark = ','),"%") ,
  subtitle = "Taxa de Alfabetização" ,
  color = "purple",
  icon = icon("book-open-reader"),
  footer = 'Mulheres' ,
  gradient = T
)
})
output$alf_posicao <- renderValueBox({
  valueBox(
    value = paste0(bairros_alfab[bairros_alfab$NM_BAIRRO==selected() , "posição"],'º') ,
    subtitle = "Entre as melhores taxas" ,
    color = "teal",
    icon = icon("book-open-reader"),
    footer = paste0('Posição entre ', if_else(selected() == 'Fortaleza', 'as capitais', 'os bairros')    ) , 
    gradient = T
  )
})
output$alf <- renderValueBox({
  
  
  valueBox(
    value = paste0(format( bairros_alfab[bairros_alfab$NM_BAIRRO=='Fortaleza' ,'Tx_alfab_mais_de_15']*100 ,digits= 2,nsmall = 2,  big.mark   = '.',decimal.mark = ','),'%') ,
    subtitle = "Taxa de alfabetização",
    color = "warning",
    icon = icon("book-open-reader"),
    footer = 'Geral em Fortaleza',
    gradient = T
  )
})
output$pizza_alfab_H <- renderEcharts4r({
Qtd_alfab |> 
  filter(NM_BAIRRO == selected()) |> 
  filter(V2007== 'Homem' ) |> 
  e_charts(Tipo,reorder = F) |> 
  e_pie(Qtd,radius = c("30%", "60%"),startAngle = 150,
        label = list(color='black',show = T,formatter = "{b} \n {d}%"),
        emphasis = list(
          label = list( 
            show = T,
            fontSize = 15,
            fontWeight = "bold"))) |>
  e_tooltip(formatter=e_tooltip_choro_formatter(
    locale = 'PT-BR')) |> 
  e_legend(show=F) |> 
  e_color(c( "#4B9695","#D1EB28")) |> 
  e_title(text='Homem',left= "center") 
})
output$pizza_alfab_M <- renderEcharts4r({
  Qtd_alfab |> 
    filter(NM_BAIRRO == selected()) |> 
    filter(V2007== 'Mulher' ) |> 
    e_charts(Tipo,reorder = F) |> 
    e_pie(Qtd,radius = c("30%", "60%"),startAngle = 150,
          label = list(color='black',show = T,formatter = "{b} \n {d}%"),
          emphasis = list(
            label = list( 
              show = T,
              fontSize = 15,
              fontWeight = "bold"))) |>
    e_tooltip(formatter=e_tooltip_choro_formatter(
      locale = 'PT-BR')) |> 
    e_legend(show=F) |> 
    e_color(c( "#4B9695","#D1EB28"))|> 
    e_title(text='Mulher',left= "center") 
})

output$text_selected <- renderText({ selected()})


selected <- reactiveVal('Fortaleza')



observeEvent(input$map_educ_shape_click, {
 
  #capture the info of the clicked polygon
  select <- input$map_educ_shape_click$id


  #if click id isn't null render the table
  if(selected() == select){
    select <- 'Fortaleza'
  }
  
  selected(select)


})

#input$input$map_educ_shape_click$id

output$idade_alfab <- renderEcharts4r({

  
  Tx_faixa_alfab |> 
  filter(NM_BAIRRO == selected(), V2007!='Geral') |>
  group_by(V2007) |>   
  e_chart(x=Tipo,) |> 
  e_bar(Alfabetização) |> 
  e_y_axis(min=.19) |> 
  e_tooltip(
    trigger="axis",formatter = e_tooltip_pointer_formatter(
      style = c( "percent"),digits = 1) )|>  
      # e_legend( orient= "vertical",
      #           ) |> 
      e_y_axis(show=T,name="Taxa de alfabetização",
               formatter=e_axis_formatter(style = c("percent"),
                                          locale = 'PT-BR')) |> 
      #e_grid(top = "-5%") |> 
  e_color(c('#2ec7c9','#b6a2de'))
  
})
## Mapa Educação ----------------------------------------------------------

# Aux map Educação 
{
pal_tx_alfab<- colorNumeric(
  palette = 'OrRd',
  domain = shp_Bairros_alfab$`Tx. alfabetização`
)
# pal_tx_alfab_h<- colorNumeric(
#   palette = 'PuBu',
#   domain = shp_Bairros_alfab$`Tx. alfab. homem`
# )
# pal_tx_alfab_m<- colorNumeric(
#   palette = 'PuRd',
#   domain = shp_Bairros_alfab$`Tx. alfab. mulher`
# )

}

output$map_educ <-  renderLeaflet({
  
  leaflet(options = leafletOptions(zoomControl = FALSE,
                                   dragging = F,
                                   doubleClickZoom = FALSE,
                                   scrollWheelZoom = F) ) |>
    setMapWidgetStyle(list(background = "white"))  |> 
    addScaleBar(position = "bottomright",
                options = scaleBarOptions(imperial = F))  |>
    setView(-38.51782, -3.795804, zoom = 12) |> 
    addPolygons(data=shp_Bairros_alfab,
                weight=.6,
                fillColor=~pal_tx_alfab(shp_Bairros_alfab$`Tx. alfabetização`),
                color = 'purple',
                opacity = 1,
                group = 'id_final_',
                popup =  popupTable(shp_Bairros_alfab,zcol = c("Bairro", 
                                                                 "Tx. alfab. mulher",
                                                                 "Tx. alfab. homem",
                                                                 "Tx. alfabetização"
                                                                 ),
                                    feature.id = FALSE,
                                    row.numbers = FALSE),
                dashArray = "0",
                fillOpacity =1 ,
                layerId = ~Bairro,
                label = ~Bairro,
                highlightOptions=highlightOptions(
                  color = "#500050", weight = 3, bringToFront = F)
    ) |>
    addLegend(data = shp_Bairros_alfab ,"bottomright", pal = pal_tx_alfab, values = ~`Tx. alfabetização`,
              title = "Taxa de alfabetização",
              opacity = 1 )#-38.52782, -3.785804
    

  
}) 

# observeEvent(req(input$current_tab=='Educação'), {
#   
#   leafletProxy('map_educ') |>
#     addPolygons(data=shp_Bairros_alfab,
#                 weight=.6,
#                 fillColor=~pal_tx_alfab(shp_Bairros_alfab$`Tx. alfabetização`),
#                 color = 'purple',
#                 opacity = 1,
#                 group = 'id_final_',
#                 popup =  popupTable(shp_Bairros_alfab,zcol = c("Bairro", 
#                                                                "Tx. alfabetização"),
#                                     feature.id = FALSE,
#                                     row.numbers = FALSE),
#                 dashArray = "0",
#                 fillOpacity =1 ,
#                 layerId = ~Bairro,
#                 label = ~Bairro,
#                 highlightOptions=highlightOptions(
#                   color = "#500050", weight = 3, bringToFront = F)
#     ) |>
#     addLegend(data = shp_Bairros_alfab ,"bottomright", pal = pal_tx_alfab, values = ~`Tx. alfabetização`,
#               title = "Taxa de alfabetização",
#               opacity = 1 )
#   
# },once=T)

# observeEvent(req(input$tabs_alf),{ 
#   
#   
#   leafletProxy("map_educ" )  |> 
#     clearGroup(c('id_final_')) |> 
#    clearControls() 
#   
#   if(input$tabs_alf =='Em mulheres' )
#   {
#     leafletProxy('map_educ') |>
#       addPolygons(data=shp_Bairros_alfab,
#                   weight=.6,
#                   fillColor=~pal_tx_alfab_m(shp_Bairros_alfab$`Tx. alfab. mulher`),
#                   color = 'purple',
#                   opacity = 1,
#                   group = 'id_final_',
#                   popup =  popupTable(shp_Bairros_alfab,zcol = c("Bairro", 
#                                                                  "Tx. alfab. mulher",
#                                                                  "Tx. alfab. homem",
#                                                                  "Tx. alfabetização"
#                                                                  ),
#                                       feature.id = FALSE,
#                                       row.numbers = FALSE),
#                   dashArray = "0",
#                   fillOpacity =1 ,
#                   label = ~Bairro,
#                   highlightOptions=highlightOptions(
#                     color = "#500050", weight = 3, bringToFront = F)
#       ) |>
#       addLegend(data = shp_Bairros_alfab ,"bottomright", pal = pal_tx_alfab_m, values = ~`Tx. alfab. mulher`,
#                 title = "Taxa de alfabetização",
#                 opacity = 1 )  
#     
#   
#   }
#   
#   if(input$tabs_alf == 'Em homens' )
#   {
#     
#     leafletProxy('map_educ') |>
#       addPolygons(data=shp_Bairros_alfab,
#                   weight=.6,
#                   fillColor=~pal_tx_alfab_h(shp_Bairros_alfab$`Tx. alfab. homem`),
#                   color = 'purple',
#                   opacity = 1,
#                   group = 'id_final_',
#                   popup =  popupTable(shp_Bairros_alfab,zcol = c("Bairro", 
#                                                                  "Tx. alfab. homem",
#                                                                  "Tx. alfab. mulher",
#                                                                  "Tx. alfabetização"),
#                                       feature.id = FALSE,
#                                       row.numbers = FALSE),
#                   dashArray = "0",
#                   fillOpacity =1 ,
#                   label = ~Bairro,
#                   highlightOptions=highlightOptions(
#                     color = "#500050", weight = 3, bringToFront = F)
#       ) |>
#       addLegend(data = shp_Bairros_alfab ,"bottomright", pal = pal_tx_alfab_h, values = ~`Tx. alfab. homem`,
#                 title = "Taxa de alfabetização",
#                 opacity = 1 )
# 
#   }
#   
#   if(input$tabs_alf == 'Geral' )
#   {
#   leafletProxy('map_educ') |>
#     addPolygons(data=shp_Bairros_alfab,
#                 weight=.6,
#                 fillColor=~pal_tx_alfab(shp_Bairros_alfab$`Tx. alfabetização`),
#                 color = 'purple',
#                 opacity = 1,
#                 group = 'id_final_',
#                 popup =  popupTable(shp_Bairros_alfab,zcol = c("Bairro", 
#                                                                "Tx. alfabetização",
#                                                                "Tx. alfab. mulher",
#                                                                "Tx. alfab. homem"),
#                                     feature.id = FALSE,
#                                     row.numbers = FALSE),
#                 dashArray = "0",
#                 fillOpacity =1 ,
#                 label = ~Bairro,
#                 highlightOptions=highlightOptions(
#                   color = "#500050", weight = 3, bringToFront = F)
#     ) |>
#     addLegend(data = shp_Bairros_alfab ,"bottomright", pal = pal_tx_alfab, values = ~`Tx. alfabetização`,
#               title = "Taxa de alfabetização",
#               opacity = 1 )
#   }
#   
# 
# },ignoreInit = T) 
#   
  


}
