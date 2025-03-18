library(tidyr)
library(dplyr)
library(stringr)
bairros_alfab<- read.csv2("Dados/Agregados_por_bairros_alfabetizacao_BR.csv",encoding ='latin1')

bairros_alfab <- bairros_alfab |>
  filter(str_detect(CD_BAIRRO, "^230440")) |>
  mutate_at(vars(starts_with("V")), as.numeric )

posicao <- bairros_alfab |> 
  mutate(posição = rank(-Tx_alfab_mais_de_15_M)) |> 
  select(2,posição,Tx_alfab_mais_de_15_M,
         Pessoas_mais_de_15_H,
         Pessoas_mais_de_15_M,
         Alfab_mais_de_15_H,
         Alfab_mais_de_15_M,
         Tx_alfab_mais_de_15,
         Tx_alfab_mais_de_15_M,
         Tx_alfab_mais_de_15_H)

#Fortaleza
posicao_for <- bairros_alfab_ |> 
  mutate(posição = 22) |> 
  select(2,posição,Tx_alfab_mais_de_15_M,
         Pessoas_mais_de_15_H,
         Pessoas_mais_de_15_M,
         Alfab_mais_de_15_H,
         Alfab_mais_de_15_M,
         Tx_alfab_mais_de_15,
         Tx_alfab_mais_de_15_M,
         Tx_alfab_mais_de_15_H) |> 
  rename(NM_BAIRRO=NM_MUN)

posic<- bind_rows(posicao,posicao_for)

posic_ <- posic |>
  mutate(
    Analfab_mais_de_15_H = Pessoas_mais_de_15_H - Alfab_mais_de_15_H,
    Analfab_mais_de_15_M = Pessoas_mais_de_15_M - Alfab_mais_de_15_M)

names(posic_)

Qtd_alfab <- posic_ |>
  select(NM_BAIRRO,6,7,10,11) |>
  pivot_longer(cols=2:5,names_to='Tipo',values_to = 'Qtd') |>
  mutate(V2007 = if_else(stringr::str_detect(Tipo,'M'), 'Mulher','Homem'),
         Tipo = if_else(stringr::str_detect(Tipo,'Alfab'), 'Alfabetizado','Não\n alfabetizado'))




saveRDS(posic,'Dados/tx_bairros_alfab.rds')

saveRDS(Qtd_alfab,'Dados/Qtd_alfab.rds')

save

{
# 
# bairros_alfab_ <- mutate(bairros_alfab,
#   Pessoas_mais_de_15 = rowSums(across(V00644:V00656)),
#   Alfab_mais_de_15 = rowSums(across(V00748:V00760)),
#   Pessoas_mais_de_15_H = rowSums(across(V00722:V00734)),
#   Pessoas_mais_de_15_M = rowSums(across(V00735:V00747)),
#   Alfab_mais_de_15_H = rowSums(across(V00826:V00838)),
#   Alfab_mais_de_15_M = rowSums(across(V00839:V00851)),
#   Tx_alfab_mais_de_15 = Alfab_mais_de_15/Pessoas_mais_de_15,
#   Tx_alfab_mais_de_15_M = Alfab_mais_de_15_M/Pessoas_mais_de_15_M,
#   Tx_alfab_mais_de_15_H = Alfab_mais_de_15_H/Pessoas_mais_de_15_H ) |> 
#   select(!starts_with("V"))
# 
# shp_Bairros <- readRDS('Dados/shp_bairros_demografia.rds') |> 
#   select(Bairro )
# 
# shp_Bairros_alfab <- left_join(shp_Bairros,bairros_alfab_,by=c('Bairro'='NM_BAIRRO'))
# 
# shp_Bairros_alfab_ <-shp_Bairros_alfab |> 
#                         mutate(
#                             Tx_alfab_mais_de_15 = round(Tx_alfab_mais_de_15*100,2),
#                             Tx_alfab_mais_de_15_M = round(Tx_alfab_mais_de_15_M*100,2),
#                             Tx_alfab_mais_de_15_H= round(Tx_alfab_mais_de_15_H*100,2)
#                             ) |> 
#   dplyr::rename(`Tx. alfabetização` = Tx_alfab_mais_de_15,
#          `Tx. alfab. mulher` = Tx_alfab_mais_de_15_M,
#          `Tx. alfab. homem` = Tx_alfab_mais_de_15_H) |> 
#   mutate()
# 
# saveRDS(shp_Bairros_alfab_,'Dados/shp_Bairros_alfab.rds')


#Mapa
# {pal_tx_alfab<- colorNumeric(
#   palette = 'BuPu',
#   domain = shp_Bairros_alfab$Tx_alfab_mais_de_15
# )
# 
# pal_tx_alfab_H<- colorNumeric(
#   palette = 'OrRd',
#   domain = shp_Bairros_alfab$Tx_alfab_mais_de_15_H
# )
# 
# 
#  b <-  leaflet( ) |>
#     setMapWidgetStyle(list(background = "white"))  |> 
#     addProviderTiles(providers$CartoDB.PositronNoLabels, group = "CartoDB.PositronNoLabels")|> 
#     addLayersControl(
#       overlayGroups = c(
#         "Bairros",
#         'id_final')) |>
#     addScaleBar(position = "bottomright",
#                 options = scaleBarOptions(imperial = F))  |>
#     setView(-38.52782, -3.785804, zoom = 11) |>
#     addFullscreenControl(pseudoFullscreen = T)
#  
#  b |>  
#   #leafletProxy('map_bairro') |>
#     addPolygons(data=shp_Bairros_alfab,
#                 weight=.6,
#                 fillColor=~pal_tx_alfab_H(shp_Bairros_alfab$Tx_alfab_mais_de_15_H),
#                 color = 'purple',
#                 opacity = 1,
#                 group = 'id_final',
#                 popup =  popupTable(shp_Bairros_alfab,zcol = c("Bairro", 
#                                                          "Tx_alfab_mais_de_15_H"),
#                                     feature.id = FALSE,
#                                     row.numbers = FALSE),
#                 dashArray = "0",
#                 fillOpacity =1 ,
#                 label = ~Bairro,
#                 highlightOptions=highlightOptions(
#                   color = "#500050", weight = 3, bringToFront = F)
#     ) |>
#     addLegend(data = shp_Bairros_alfab ,"bottomright", pal = pal_tx_alfab_H, values = ~Tx_alfab_mais_de_15_H,
#               title = "Taxa de alfabetismo",
#               opacity = 1 )
#   
#  b|>  
#    #leafletProxy('map_bairro') |>
#    addPolygons(data=shp_Bairros_alfab,
#                weight=.6,
#                fillColor=~pal_tx_alfab_H(shp_Bairros_alfab$Tx_alfab_mais_de_15),
#                color = 'purple',
#                opacity = 1,
#                group = 'id_final',
#                popup =  popupTable(shp_Bairros_alfab,zcol = c("Bairro", 
#                                                               "Tx_alfab_mais_de_15"),
#                                    feature.id = FALSE,
#                                    row.numbers = FALSE),
#                dashArray = "0",
#                fillOpacity =1 ,
#                label = ~Bairro,
#                highlightOptions=highlightOptions(
#                  color = "#500050", weight = 3, bringToFront = F)
#    ) |>
#    addLegend(data = shp_Bairros_alfab ,"bottomright", pal = pal_tx_alfab_H, values = ~Tx_alfab_mais_de_15,
#              title = "Taxa de alfabetismo",
#              opacity = 1 )
# }
# bairros_alfab_|> 
#   summarise(Alfab = sum(Alfab_mais_de_15),
#             Pessoas = sum(Pessoas_mais_de_15 ),
#             Tx_alfab = Alfab/ Pessoas,
#             Alfab_H = sum(Alfab_mais_de_15_H ),
#             Alfab_M = sum(Alfab_mais_de_15_M ),
#             Pessoas_H = sum(Pessoas_mais_de_15_H),
#             Pessoas_M = sum(Pessoas_mais_de_15_M),
#             H_M = Pessoas_H+Pessoas_M ,
#             Alfab_H_M = Alfab_H + Alfab_M
#             );
}
bairros_alfab_fort <- readxl::read_excel("Dados/Agregados_fortaleza_alfabetizacao.xlsx")


bairros_alfab_ <- mutate(bairros_alfab_fort,  #bairros_alfab_fort  bairros_alfab
                         Pessoas_mais_de_15 = rowSums(across(V00644:V00656)),
                         Alfab_mais_de_15 = rowSums(across(V00748:V00760)),
                         Pessoas_mais_de_15_H = rowSums(across(V00722:V00734)),
                         Pessoas_mais_de_15_M = rowSums(across(V00735:V00747)),
                         Alfab_mais_de_15_H = rowSums(across(V00826:V00838)),
                         Alfab_mais_de_15_M = rowSums(across(V00839:V00851)),
                         
                         Analfab_mais_de_15_H = Pessoas_mais_de_15_H - Alfab_mais_de_15_H,
                         Analfab_mais_de_15_M = Pessoas_mais_de_15_M - Alfab_mais_de_15_M,
                         
                         Tx_alfab_mais_de_15 = Alfab_mais_de_15/Pessoas_mais_de_15,
                         Tx_alfab_mais_de_15_M = Alfab_mais_de_15_M/Pessoas_mais_de_15_M,
                         Tx_alfab_mais_de_15_H = Alfab_mais_de_15_H/Pessoas_mais_de_15_H ,
                         `15 a 19 anos` = V00748/V00644,
                         `20 a 24 anos` = V00749/V00645,
                         `25 a 29 anos` = V00750/V00646,
                         `30 a 34 anos` = V00751/V00647,
                         `35 a 39 anos` = V00752/V00648,
                         `40 a 44 anos` = V00753/V00649,
                         `45 a 49 anos` = V00754/V00650,
                         `50 a 54 anos` = V00755/V00651,
                         `55 a 59 anos` = V00756/V00652,
                         `60 a 64 anos` = V00757/V00653,
                         `65 a 69 anos` = V00758/V00654,
                         `70 a 79 anos` = V00759/V00655,
                   
                         
                         `25 a 34 anos` = rowSums(across(V00750:V00751))/ rowSums(across(V00646:V00647)),
                         `35 a 44 anos` = rowSums(across(V00752:V00753))/ rowSums(across(V00648:V00649)),
                         `45 a 54 anos` = rowSums(across(V00754:V00755))/ rowSums(across(V00650:V00651)),
                         `55 a 64 anos` = rowSums(across(V00756:V00757))/ rowSums(across(V00652:V00653)),
                         `Mais de 65` = rowSums(across(V00758:V00760))/ rowSums(across(V00654:V00656)),
                         `Mais de 80`   = V00760/V00656,
                         
                         `Homem 15 a 19 anos` = V00826/V00722,
                         `Homem 20 a 24 anos` = V00827/V00723,
                         
                         `Homem 25 a 34 anos` = rowSums(across(V00828:V00829))/ rowSums(across(V00724:V00725)),
                         `Homem 35 a 44 anos` = rowSums(across(V00830:V00831))/ rowSums(across(V00726:V00727)),
                         `Homem 45 a 54 anos` = rowSums(across(V00832:V00833))/ rowSums(across(V00728:V00729)),
                         `Homem 55 a 64 anos` = rowSums(across(V00834:V00835))/ rowSums(across(V00730:V00731)),
                         `Homem Mais de 65` = rowSums(across(V00836:V00838))/ rowSums(across(V00732:V00734)),
                         `Homem Mais de 80`   = V00838/V00734,
                         
                         `Homem 25 a 29 anos` = V00828/V00724,
                         `Homem 30 a 34 anos` = V00829/V00725,
                         `Homem 35 a 39 anos` = V00830/V00726,
                         `Homem 40 a 44 anos` = V00831/V00727,
                         `Homem 45 a 49 anos` = V00832/V00728,
                         `Homem 50 a 54 anos` = V00833/V00729,
                         `Homem 55 a 59 anos` = V00834/V00730,
                         `Homem 60 a 64 anos` = V00835/V00731,
                         `Homem 65 a 69 anos` = V00836/V00732,
                         `Homem 70 a 79 anos` = V00837/V00733,
                        
                         
                         `Mulher 15 a 19 anos` = V00839/V00735,
                         `Mulher 20 a 24 anos` = V00840/V00736,
                         
                         `Mulher 25 a 34 anos` = rowSums(across(V00841:V00842))/ rowSums(across(V00737:V00738)),
                         `Mulher 35 a 44 anos` = rowSums(across(V00843:V00844))/ rowSums(across(V00739:V00740)),
                         `Mulher 45 a 54 anos` = rowSums(across(V00845:V00846))/ rowSums(across(V00741:V00742)),
                         `Mulher 55 a 64 anos` = rowSums(across(V00847:V00848))/ rowSums(across(V00743:V00744)),
                         `Mulher Mais de 65` = rowSums(across(V00849:V00851))/ rowSums(across(V00745:V00747)),
                         `Mulher Mais de 80`   = V00851/V00747,
                         
                         
                         `Mulher 25 a 29 anos` = V00841/V00737,
                         `Mulher 30 a 34 anos` = V00842/V00738,
                         `Mulher 35 a 39 anos` = V00843/V00739,
                         `Mulher 40 a 44 anos` = V00844/V00740,
                         `Mulher 45 a 49 anos` = V00845/V00741,
                         `Mulher 50 a 54 anos` = V00846/V00742,
                         `Mulher 55 a 59 anos` = V00847/V00743,
                         `Mulher 60 a 64 anos` = V00848/V00744,
                         `Mulher 65 a 69 anos` = V00849/V00745,
                         `Mulher 70 a 79 anos` = V00850/V00746,
                        
                         
                         `Tx_analfab_mais_de_15_M` = 1 - `Tx_alfab_mais_de_15_M`,
                         `Tx_analfab_mais_de_15_H` = 1 - `Tx_alfab_mais_de_15_H`
                         
                         ) |> 
  select(!starts_with("V"))


# saveRDS(bairros_alfab_,'Dados/bairros_alfab.rds')
# 
# bairros_alfab<- readRDS('Dados/bairros_alfab.rds')

names(bairros_alfab)
# 
# Qtd_alfab <- bairros_alfab |> 
#   select(NM_BAIRRO,7:10) |>
#   pivot_longer(cols=2:5,names_to='Tipo',values_to = 'Qtd') |> 
#   mutate(V2007 = if_else(stringr::str_detect(Tipo,'M'), 'Mulher','Homem'),
#          Tipo = if_else(stringr::str_detect(Tipo,'Alfab'), 'Alfabetizado','Não\n alfabetizado'))
# 
# saveRDS(Qtd_alfab,'Dados/Qtd_alfab.rds')
# 
# bairros_alfab |> 
#   select(NM_BAIRRO,7:10) |>
#   pivot_longer(cols=2:5,names_to='Tipo',values_to = 'Qtd') |> 
#   mutate(V2007 = if_else(stringr::str_detect(Tipo,'M'), 'Mulher','Homem'),
#          Tipo = if_else(stringr::str_detect(Tipo,'Alfab'), 'Alfabetizado','Não\n alfabetizado')) |> 
#   filter(NM_BAIRRO == 'Centro') |> 
#   filter(V2007== 'Homem' ) |> 
#   e_charts(Tipo,reorder = F) |> 
#   e_pie(Qtd,radius = c("40%", "70%"),# roseType = "area", clockwise=F,   radius = c("5%", "65%"),
#         label = list(color='black',show = T,formatter = "{b} \n {d}%"),
#         emphasis = list(
#           label = list( 
#             show = T,
#             fontSize = 20,
#             fontWeight = "bold"))) |>
#   e_tooltip(formatter=e_tooltip_choro_formatter(
#                                        locale = 'PT-BR')) |> 
#   e_legend(show=F) |> 
#   e_color(c( "#4B9695","#D1EB28"))
# #|> 
#   #e_title(text=input$select_1[1])     
# 
#   
#   
# 
# 
# 


Qtd_alfab <- bairros_alfab |>
  select(NM_BAIRRO,7:10) |>
  pivot_longer(cols=2:5,names_to='Tipo',values_to = 'Qtd') |>
  mutate(V2007 = if_else(stringr::str_detect(Tipo,'M'), 'Mulher','Homem'),
         Tipo = if_else(stringr::str_detect(Tipo,'Alfab'), 'Alfabetizado','Não alfabetizado'))



Tx_faixa <- bairros_alfab_ |> 
  select(NM_BAIRRO,14,15,26:31) |> 
  pivot_longer(cols=2:9,names_to='Tipo',values_to = 'Alfabetização') |> 
  mutate(V2007 = "Geral",
         Tipo = gsub('Homem ','',Tipo),
         Tipo = gsub('Mulher ','',Tipo))

Tx_faixa_Sexo <- bairros_alfab_ |> 
  select(NM_BAIRRO,32:39,50:57) |> 
  pivot_longer(cols=2:17,names_to='Tipo',values_to = 'Alfabetização') |> 
  mutate(V2007 = if_else(stringr::str_detect(Tipo,'Mulher'), 'Mulher','Homem'),
    Tipo = gsub('Homem ','',Tipo),
    Tipo = gsub('Mulher ','',Tipo))

Tx_faixa_ <- bind_rows(Tx_faixa,Tx_faixa_Sexo)

Tx_faixa |> 
  group_by(NM_BAIRRO) |> 
  filter(NM_BAIRRO=='Pedras') |> 
  e_chart(x=Tipo) |> 
  e_bar(Alfabetização) |> 
  e_y_axis(min=.7) |> 
  e_tooltip()

#### FORTALEZA
Tx_faixa_fort<- bairros_alfab_ |> 
  select(NM_MUN,14,15,26:31) |> 
  pivot_longer(cols=2:9,names_to='Tipo',values_to = 'Alfabetização') |> 
  mutate(V2007 = "Geral",
         Tipo = gsub('Homem ','',Tipo),
         Tipo = gsub('Mulher ','',Tipo))



Tx_faixa_Sexo_fort <- bairros_alfab_ |> 
  select(NM_MUN,32:39,50:57) |> 
  pivot_longer(cols=2:17,names_to='Tipo',values_to = 'Alfabetização') |> 
  mutate(V2007 = if_else(stringr::str_detect(Tipo,'Mulher'), 'Mulher','Homem'),
         Tipo = gsub('Homem ','',Tipo),
         Tipo = gsub('Mulher ','',Tipo))



Tx_faixa_fort_ <- bind_rows(Tx_faixa_fort,Tx_faixa_Sexo_fort)
Tx_faixa_fort_ <- Tx_faixa_fort_ |> rename(NM_BAIRRO=NM_MUN)

## Juntar fort e bairro
Tx_faixa_alfab <- bind_rows(Tx_faixa_fort_,Tx_faixa_)


saveRDS(Tx_faixa_alfab,'Dados/Tx_faixa_alfab.rds')
Tx_faixa_alfab <- readRDS('Dados/Tx_faixa_alfab.rds')



Tx_faixa_Sexo_fort |> 

    group_by(V2007) |> 
  
  e_chart(x=Tipo) |> 
  e_bar(Alfabetização) |> 
  e_y_axis(min=.7) |> 
  e_tooltip()







names(bairros_alfab_)

names(bairros_alfab)


listaCapitais <- rvest::read_html('https://pt.wikipedia.org/wiki/Lista_de_capitais_do_Brasil_por_%C3%A1rea')  |> 
  rvest::html_node("table")  |> 
  rvest::html_table() |> 
  dplyr::rename(Cod=`Código do IBGE`,
                Local=`Sede de governo` ) |> 
  select(Cod,Local) |> 
  mutate(Local=gsub('\\[nota 1]',"",Local),
         Cod=stringr::str_sub(Cod,1,6))

posicao_forta <- readxl::read_excel("Dados/Censo 2022 - Taxa de alfabetização - Ranking por Município.xlsx")

 posição_ <- dplyr::inner_join(posicao_forta,listaCapitais[,1],by=c("Código"="Cod"))













