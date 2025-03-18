
rend <-  df_pnadc |>
  filter(Ano == 2023) |>
  group_by(`Grupos ocupacionais`,V2007) |>
  summarise(Rendimento = weighted.mean(VD4019*CO2,w = V1032,na.rm =TRUE),
            Qtd = round(sum(V1032))) |>
  mutate(Rendimento = round(Rendimento,2)) |>
  pivot_wider(names_from = V2007, values_from =c(Rendimento,Qtd )) |>
  #select(1,3,2) |>
  drop_na()






rend <-  df_pnadc |>  
  filter(Ano == input$Ano_filter) |> 
  group_by(`Categoria do emprego`,V2007) |>
  summarise(Rendimento = weighted.mean(VD4019*CO2,w = V1032,na.rm =TRUE)) |>
  mutate(Rendimento = round(Rendimento,2)) |> 
  pivot_wider(names_from = V2007, values_from =Rendimento ) |> 
  #select(1,3,2) |> 
  drop_na()  

rend |> 
  reactable(bordered = TRUE,compact = T,defaultPageSize = 15,
            highlight = TRUE, 
            defaultColDef = colDef(
              style = list(fontSize = 14,headerClass = "sort-header"
              )#headerClass = "sort-header",
            ),
           
            columns = list(

              Rendimento_Homem = colDef(name = "Homens", align = "left", # width = 200,
                             cell = function(value) {
                               width <- paste0(value / max(rend$Rendimento_Homem) * 100,'%')
                               value <- format(value,big.mark = ".",decimal.mark=",")
                               value <- format(value, width = 9, justify = "right")
                               bar_chart(value, width = width,background = "#e1e1e1",fill = cor_h)
                             }, style = list(fontFamily = "monospace", whiteSpace = "pre")),
              Rendimento_Mulher = colDef(name = "Mulheres", align = "left", # width = 200,
                              cell = function(value) {
                                width <- paste0(value / max(rend$Rendimento_Homem) * 100,'%')
                                value <- format(value,big.mark = ".",decimal.mark=",")
                                value <- format(value, width = 9, justify = "right")
                                bar_chart(value, width = width,background = "#e1e1e1",fill =cor_m)
                              }, style = list(fontFamily = "monospace", whiteSpace = "pre")),
              Qtd_Homem = colDef(name = "Homens", align = "left", # width = 200,
                                        cell = function(value) {
                                          width <- paste0(value / max(rend$Qtd_Mulher) * 100,'%')
                                          value <- format(value,big.mark = ".",decimal.mark=",")
                                          value <- format(value, width = 9, justify = "right")
                                          bar_chart(value, width = width,background = "#e1e1e1",fill = cor_h)
                                        }, style = list(fontFamily = "monospace", whiteSpace = "pre")),
              Qtd_Mulher = colDef(name = "Mulheres", align = "left", # width = 200,
                                         cell = function(value) {
                                           width <- paste0(value / max(rend$Qtd_Mulher) * 100,'%')
                                           value <- format(value,big.mark = ".",decimal.mark=",")
                                           value <- format(value, width = 9, justify = "right")
                                           bar_chart(value, width = width,background = "#e1e1e1",fill =cor_m)
                                         }, style = list(fontFamily = "monospace", whiteSpace = "pre"))
            ),
            columnGroups = list(
              colGroup(name = "Rendimento (R$)", columns = c("Rendimento_Homem", "Rendimento_Mulher")),
              colGroup(name = "Quantidade", columns = c("Qtd_Homem", "Qtd_Mulher"))
            ),
  )



w






































# df_renda <- readxl::read_xlsx('Dados/renda.xlsx') 
# saveRDS(df_renda,'Dados/df_renda_.rds')
#   
# df_desemp <- readxl::read_xlsx('Dados/Desocupação.xlsx') |> 
#   mutate(`Taxa de Desocupação`=`Taxa de Desocupação`/100)
# saveRDS(df_desemp,'Dados/df_desemp.rds')
# 
# 
# df_inf <- readxl::read_xlsx('Dados/Informalidade.xlsx') |> 
#   mutate(`Taxa de Informalidade`=`Taxa de Informalidade`/100)
# saveRDS(df_inf,'Dados/df_inf.rds')
# 
# df_ocup <- readxl::read_xlsx('Dados/ocupação.xlsx')
# saveRDS(df_ocup,'Dados/df_ocup.rds')
# 
# 
# 
# Tipos_trab23 <- readRDS('Dados/Tipos_trab_2023.rds')
# 
# pnadc_12_23 <- readRDS('Dados/pnadc12_23_fort.rds')
# 
# 
# #Dados grafico linha
# {
# df_ocup_geral  <- aggregate(Qtd.~ Ano,Tipos_trab23,sum)  |>
#   mutate(Qtd.=round(Qtd.),
#          V2007 = 'Geral')
# 
# df_inf_geral <-   pivot_wider(
#   aggregate(Qtd.~ Ano+Tipo,Tipos_trab23,sum) |>
#     mutate(Qtd.=ifelse(is.na(Qtd.), 0, Qtd.)),names_from=Tipo,values_from = Qtd.,values_fill = 0) |>
#   group_by(Ano) |>
#   summarise(Informalidade=`Trabalho informal`/(`Trabalho formal`+`Trabalho informal`)) |>
#   mutate(V2007 = 'Geral')
# 
# 
# df_desemp_geral <- group_by(pnadc_12_23,
#             Ano) |>
#   summarise( Tx_desocupacao  = sum(V1032[VD4002=="Pessoas desocupadas"],na.rm = TRUE)/
#                sum(V1032[VD4001=="Pessoas na força de trabalho"],na.rm = TRUE),.groups ='keep' ) |>
#   mutate(V2007 = 'Geral') |>
#   ungroup()
# 
# 
# 
# # desemprego
# df_desemp_temp <- group_by(pnadc_12_23,
#                            Ano,V2007)  |>
#   summarise( Tx_desocupacao = sum(V1032[VD4002=="Pessoas desocupadas"],na.rm = TRUE)/
#                sum(V1032[VD4001=="Pessoas na força de trabalho"],na.rm = TRUE),.groups ='keep' )
# 
# df_desemp <- bind_rows(df_desemp_temp,df_desemp_geral)|>
#   group_by(V2007)
# 
# saveRDS(df_desemp,'Dados/df_desemp.rds')  # save para estatico
# 
# 
# # ocupação
# 
# df_ocup_temp  <- aggregate(Qtd.~ Ano+V2007,Tipos_trab23,sum)  |> # Alternativa para reativo dinamico
#   group_by(V2007) |>
#   mutate(Qtd.=round(Qtd.))
# 
# df_ocup <- bind_rows(df_ocup_temp,df_ocup_geral)
# 
# saveRDS(df_ocup,'Dados/df_ocup.rds')  # save para estatico
# 
# # informal
# df_inf_temp <-   pivot_wider( aggregate(Qtd.~ Ano+Tipo+V2007,Tipos_trab23,sum)  |>  # Aternativa para dinamico
#                                 mutate(Qtd.=ifelse(is.na(Qtd.), 0, Qtd.)),names_from=Tipo,values_from = Qtd.,values_fill = 0) |>
#   group_by(Ano,V2007) |>
#   summarise(Informalidade=`Trabalho informal`/(`Trabalho formal`+`Trabalho informal`)) |>
#   ungroup() |>
#   group_by(V2007)
# 
# 
# df_inf <- bind_rows(df_inf_temp,df_inf_geral)
# 
# saveRDS(df_inf,'Dados/df_inf.rds')  # save para estatico
# 
# }
# 
# 
# 
# # Rendimento mensal habitual de todos os trabalhos para pessoas de 14 anos ou mais de idade (apenas para pessoas que receberam em dinheiro, produtos ou mercadorias em qualquer trabalho)
# # Rendimento médio real habitual de todos os trabalhos
# #(R$/mês)
# library(forcats)
# library(PNADcIBGE)
# library(tidyr)
# library(dplyr)
# 
# pnadc_12_23 <- readRDS('Dados/pnadc12_23_fort.rds')
# 
# pnadc_12_23_df <-  pnadc_deflator(pnadc_12_23,
#                                   "Dados/deflator_PNADC_2023.xls")
# 
# 
# pnadc_12_23_df_ <- pnadc_12_23_df |> 
#   filter(Capital=="Município de Fortaleza (CE)",VD4002 == 'Pessoas ocupadas' ) |> 
#   select(Capital,Ano,V1032,VD4019,VD4009,VD4010,VD4011,V4013,V2007,V2010,VD3004,idadeEco2,CO2)
# 
# 
# pnadc_12_23_df_cnae<- dplyr::left_join(pnadc_12_23_df_,Atividade_CNAE,by=c("V4013"="Classe")) |> 
#   dplyr::rename(`Categoria do emprego` = VD4009 ,
#          `Atividade do empreendimento`= VD4010,
#          `Grupos ocupacionais` = VD4011)
# 
# saveRDS(pnadc_12_23_df_cnae,'Dados/pnadc_12_23_df_cnae.rds')
# 
# rend <- pnadc_12_23_df_cnae |> 
#   filter(Ano %in% 2012:2012) |> 
# group_by(`Ocupação e categoria do emprego`,V2007) |> #V2007 "Denominação"   "Denom_Seção"   "Denom_Divisão" "Composição"   
#   summarise(Rendimento = weighted.mean(VD4019*CO2,w = V1032,na.rm =TRUE)) |>
#   mutate(Rendimento = round(Rendimento,2)) |> 
#   pivot_wider(names_from = V2007, values_from =Rendimento ) |> 
#   drop_na()  
#   
# 
# rend |> 
#   reactable(bordered = TRUE,compact = TRUE,defaultPageSize = 15,
#             highlight = TRUE, searchable = TRUE,defaultColDef = colDef(
#               style = list(fontSize = 15,fontFamily = "monospace"),#headerClass = "sort-header",
#               footerStyle = list(fontWeight = "bold",align = "right")),
#             columns = list(
#         
#              Homem = colDef( align = "left", 
#                                 cell = function(value) {
#                                   width <- paste0(value / max(rend$Homem) * 100,'%')
#                                   value <- format(value,big.mark = ".",decimal.mark=",")
#                                   value <- format(value, width = 9, justify = "right")
#                                   bar_chart(value, width = width,background = "#e1e1e1")
#                                 }, style = list(fontSize = 14,fontFamily = "monospace", whiteSpace = "pre")),
#              Mulher = colDef( align = "left", 
#                              cell = function(value) {
#                                width <- paste0(value / max(rend$Homem) * 100,'%')
#                                value <- format(value,big.mark = ".",decimal.mark=",")
#                                value <- format(value, width = 9, justify = "right")
#                                bar_chart(value, width = width,background = "#e1e1e1")
#                              }, style = list(fontFamily = "monospace", whiteSpace = "pre"))
#               # Razão = colDef(name = "Razão de sexo", align = "left", footer=format(razB,big.mark = ".",decimal.mark=","), 
#               #                cell = function(value) {
#               #                  width <- paste0(value / max(shp_Bairros$Razão) * 100,"%")
#               #                  value <- format(value,big.mark = ".",decimal.mark=",")
#               #                  value <- format(value, width = 7, justify = "right")
#               #                  bar_chart(value, width = width,fill = "#720072",background = "#e1e1e1")
#               #                }, style = list(fontFamily = "monospace", whiteSpace = "pre")),
#               # Percentual = colDef(name='Percentual',  align = "left", footer=format(percB,big.mark = ".",decimal.mark=","), 
#               #                     cell = function(value) {
#               #                       width <- paste0(value / max(shp_Bairros$Percentual) * 100,"%")
#               #                       value <- format(value,big.mark = ".",decimal.mark=",")
#               #                       value <- format(value, width = 7, justify = "right")
#               #                       bar_chart(value, width = width,fill = "pink",background = "#e1e1e1")
#               #                     }, style = list(fontFamily = "monospace", whiteSpace = "pre")),
#               # Domicílios = colDef( align = "left",footer=format(domiB,big.mark = ".",decimal.mark=","), 
#               #                      cell = function(value) {
#               #                        width <- paste0(value / max(shp_Bairros$Domicílios) * 100, "%")
#               #                        value <- format(value,big.mark = ".",decimal.mark=",")
#               #                        value <- format(value, width = 7, justify = "right")
#               #                        bar_chart(value, width = width, fill = "#fc5185", background = "#e1e1e1")
#               #                      }, style = list(fontFamily = "monospace", whiteSpace = "pre")),
#               # Densidade = colDef( align = "left",footer=format(densB,big.mark = ".",decimal.mark=","), 
#               #                     cell = function(value) {
#               #                       width <- paste0(value / max(shp_Bairros$Densidade) * 100, "%")
#               #                       value <- format(value,big.mark = ".",decimal.mark=",")
#               #                       value <- format(value, width = 7, justify = "right")
#               #                       bar_chart(value, width = width, fill = "#245", background = "#e1e1e1")
#               #                     }, style = list(fontFamily = "monospace", whiteSpace = "pre")),
#               # V01008 = colDef(name = "Mulheres",  align = "left", footer=format(mulB,big.mark = ".",decimal.mark=","), 
#               #                 cell = function(value) {
#               #                   width <- paste0(value / max(shp_Bairros$V01008) * 100,"%")
#               #                   value <- format(value,big.mark = ".",decimal.mark=",")
#               #                   value <- format(value, width = 7, justify = "right")
#               #                   bar_chart(value, width = width,fill = "#E016AB",background = "#e1e1e1")
#               #                 }, style = list(fontFamily = "monospace", whiteSpace = "pre"))
#             )
#   )
# 
# rend
# 
# na<- pnadc_12_23_df_cnae%>% summarise_all(list(name = ~sum(!is.na(.))))
# 
# 
# names(Atividade_CNAE)
# 
# df <- dplyr::full_join(w,pivot_wider(ws,values_from = Rendimento,names_from = V2007))
# 
# df_renda <- df |>
#   rename(Geral='Rendimento') |> 
#   pivot_longer(cols = 2:4,
#                    names_to = 'V2007',
#                    values_to = 'Renda' ) |> 
#   group_by(V2007)
# 
# #saveRDS(df_renda,'Dados/df_renda.rds')
# 
# #Atividade_CNAE <- readRDS("C:/Romulo/Iplanfor/Pnadc/Pnadc_Visita_5/Atividade_CNAE2.rds")
# 
# Tipos_trab <-  rbind(   ### Organizando dados
#   group_by(pnadc12_23_idade[pnadc12_23_idade$Capital %in% "Município de Fortaleza (CE)" , ],
#            Ano,V4013,V2007,V2010,VD3004,idadeEco2)  %>%
#     summarise(Qtd. = sum(V1032[  (VD4012 == "Contribuinte" & VD4009 == "Empregador")    |
#                                    (VD4012 == "Contribuinte" & VD4009 == "Conta-própria") |
#                                    VD4009 %in% c("Empregado no setor privado com carteira de trabalho assinada",
#                                                  "Empregado no setor público com carteira de trabalho assinada",
#                                                  "Trabalhador doméstico com carteira de trabalho assinada",
#                                                  "Militar e servidor estatutário") ],
#                          na.rm = TRUE),.groups="keep")  %>%
#     filter(Qtd.!=0) %>%
#     mutate(Tipo="Trabalho formal")
#   ,
#   group_by(pnadc12_23_idade[pnadc12_23_idade$Capital %in% "Município de Fortaleza (CE)", ],
#            Ano,V4013,V2007,V2010,VD3004,idadeEco2)  %>%
#     summarise(Qtd. = sum(V1032[VD4009 %in% c("Empregado no setor privado sem carteira de trabalho assinada",
#                                              "Trabalhador doméstico sem carteira de trabalho assinada",
#                                              "Trabalhador familiar auxiliar",
#                                              "Empregado no setor público sem carteira de trabalho assinada") |
#                                  (VD4012 == "Não contribuinte" & VD4009 == "Conta-própria")|
#                                  (VD4012 == "Não contribuinte" & VD4009 == "Empregador") ],
#                          na.rm = TRUE),.groups="keep") %>%
#     filter(Qtd.!=0) %>%
#     mutate(Tipo="Trabalho informal")) %>%
#   left_join(Atividade_CNAE,by=c("V4013"="Classe"))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Subdivisão Mercado
# 
# Mercado_trabalho <-   group_by(pnadcV1_23,Ano)  |>
#   summarise(
#     Pop_idade_trab = sum(V1032[V2009>=14],na.rm = TRUE),
#     Pop_abaixo_idade_trab = sum(V1032[V2009>=14],na.rm = TRUE),
#     Ocupacao = sum(V1032[VD4002=="Pessoas ocupadas"],na.rm = TRUE),
#     Desocupacao = sum(V1032[VD4002=="Pessoas desocupadas"],na.rm = TRUE),
#     Forc_trab = sum(V1032[VD4001=="Pessoas na força de trabalho"],na.rm = TRUE),
#     Fora_força_trab = Pop_idade_trab - Forc_trab,
#     Forca_trab_potenc = sum(V1032[VD4003=="Pessoas fora da força de trabalho e na força de trabalho potencial"],na.rm = TRUE),
#     Fora_forca_trab_potenc = Fora_força_trab - Forca_trab_potenc,
#     Busca_trab_mas_nao_disp =  sum(V1032[V4077=="Sim"],na.rm = TRUE),
#     Não_busca_mas_disp = Forca_trab_potenc - Busca_trab_mas_nao_disp,
#     Subocupacao =sum(V1032[VD4004A=='Pessoas subocupadas'],na.rm = TRUE),
#     Ocupado_horas_sufic = Ocupacao - Subocupacao,  
#     Desalentos = sum(V1032[VD4005=='Pessoas desalentadas'],na.rm = TRUE),
#     Não_desalento = Não_busca_mas_disp - Desalentos,
#     Pop_subutilizada=sum(V1032[VD4004A=='Pessoas subocupadas'|
#                                  VD4002=="Pessoas desocupadas" |
#                                  VD4003=="Pessoas fora da força de trabalho e na força de trabalho potencial"],
#                          na.rm = TRUE),
#     Trab_formal = sum(V1032[VD4012 == "Contribuinte" & 
#                               VD4009 %in% c("Conta-própria",
#                                             "Empregado no setor privado com carteira de trabalho assinada",
#                                             "Empregado no setor público com carteira de trabalho assinada",
#                                             "Empregador",
#                                             "Militar e servidor estatutário",
#                                             "Trabalhador doméstico com carteira de trabalho assinada")],na.rm = TRUE),
#     Trab_informal = sum(V1032[VD4009 %in% c("Empregado no setor privado sem carteira de trabalho assinada",
#                                             "Trabalhador doméstico sem carteira de trabalho assinada",
#                                             "Trabalhador familiar auxiliar",
#                                             "Empregado no setor público sem carteira de trabalho assinada") |
#                                 (VD4012 == "Não contribuinte" & VD4009 == "Conta-própria")|
#                                 (VD4012 == "Não contribuinte" & VD4009 == "Empregador") ],na.rm = TRUE)
#   ) |> 
#   mutate(Taxa_composta_subutilizacao=(Desocupacao+Forca_trab_potenc+Subocupacao)/(Forc_trab+Forca_trab_potenc),
#          Taxa_desocupação=Desocupacao/Forc_trab, 
#          Nivel_ocupacao=Ocupacao/(Forc_trab+Forca_trab_potenc),
#          Perc_ocup=Ocupacao/sum(Ocupacao))
# 
# 
# 
# sankey<- data.frame(
# source=c(
#   'Pop_idade_trab',
#   'Pop_idade_trab',
#   'Forc_trab',
#   'Forc_trab',
#   'Fora_força_trab',
#   'Fora_força_trab',
#   'Fora_forca_trab_potenc',
#   'Fora_forca_trab_potenc',
#   'Ocupacao',
#   'Ocupacao',
#   'Não_busca_mas_disp',
#   'Não_busca_mas_disp'
# ),
# target=c(
#   'Forc_trab',
#   'Fora_força_trab',
#   'Ocupacao',
#   'Desocupacao',
#   'Forca_trab_potenc',
#   'Fora_forca_trab_potenc',
#   'Busca_trab_mas_nao_disp',
#   'Não_busca_mas_disp',
#   'Ocupado_horas_sufic',
#   'Subocupacao',
#   'Desalentos',
#   'Não_desalento'
# ),
# value =c(10,
#          6,
#          9,
#          1,
#          8,
#          5,
#          1,
#          7,
#          8,
#          1,
#          5,
#          2
#          ),
# 
# stringsAsFactors = T
# )
# 
# sankey |>
#   arrange(target) |> 
#   e_charts() |>
#   e_sankey(source, target, value,nodeAlign = 'left',#  justify  leftlayout = "vertical",
#            nodeGap = 18, nodeWidth = 30,
#            label= list(
#              show= T
#            ),
#            layoutIterations= 0) |> 
#   e_tooltip()
# 
#   
# 
# source=c(
#   'Pop_idade_trab',
#   'Pop_idade_trab',
#   'Forc_trab',
#   'Forc_trab',
#   'Fora_força_trab',
#   'Fora_força_trab',
#   'Ocupacao',
#   'Ocupacao',
#   'Fora_forca_trab_potenc',
#   'Fora_forca_trab_potenc',
#   'Não_busca_mas_disp',
#   'Não_busca_mas_disp'
# ),
# target=c(
#   'Forc_trab',
#   'Fora_força_trab',
#   'Ocupacao',
#   'Desocupacao',
#   'Forca_trab_potenc',
#   'Fora_forca_trab_potenc',
#   'Ocupado_horas_sufic',
#   'Subocupacao',
#   'Busca_trab_mas_nao_disp',
#   'Não_busca_mas_disp',
#   'Desalentos',
#   'Não_desalento'
# ),
# value =c(10,6,9,1,8,5,8,1,1,7,5,2),
# 
# 
# Pop_idade_trab 
# Pop_abaixo_idade_trab 
# Ocupacao 
# Desocupacao 
# Forc_trab 
# Fora_força_trab 
# Forca_trab_potenc 
# Fora_forca_trab_potenc 
# Busca_trab_mas_nao_disp
# 
# Subocupacao 
# Desalentos 
# Pop_subutilizada
# 
# library(echarts4r)
# 
# sankey <- data.frame(
#   source = c("a", "b", "c", "d", "c"),
#   target = c("b", "c", "d", "e", "e"),
#   value = ceiling(rnorm(5, 10, 1)),
#   stringsAsFactors = FALSE
# )
# 
# sankey |>
#   e_charts() |>
#   e_sankey(source, target, value) |> 
#   e_flip_coords()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
