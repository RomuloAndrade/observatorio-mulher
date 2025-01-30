# library(tidyverse)
# library(echarts4r)
# 
# #Ler dados
# pop_fort <- readxl::read_excel('C:/Romulo/Dados/Censo2022/Censo 2022 - Pirâmide etária - Fortaleza (CE).xlsx') |> 
#   janitor::clean_names()
# 
# pop_fort10_22 <- readxl::read_excel('Dados/Censo 2022 e 2010 - Pirâmide etária _ Comparação - Fortaleza (CE).xlsx',
#                                     skip = 1) |> 
#   janitor::clean_names() |> 
#   filter(!is.na(codigo_do_municipio)) |> 
#   rename(pf10=populacao_feminina_pessoas_censo_2010,
#          pf22=populacao_feminina_pessoas_censo_2022,
#          pm10=populacao_masculina_pessoas_censo_de_2010,
#          pm22=populacao_masculina_pessoas_censo_de_2022)
# 
# 
# #Grafico piramide 2022
# {
#   e_theme(name='macarons') 
#   pop_fort_graf <- pop_fort |>
#     rename(masculina=populacao_masculina_pessoas,
#            feminina=populacao_feminina_pessoas) |> 
#     mutate( masculina_inv= masculina*-1,
#             seq = 1:dim(pop_fort)[1],
#             perc_fem = round(feminina/sum(feminina)*100,1),
#             perc_masc = round(feminina/sum(masculina)*100,1)
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
#   }
# 
# 
#   
# #Grafico piramide 2010
# {
# 
#   e_theme(names='macarons') 
#   pop_fort_graf10 <- pop_fort10_22 |>
#     rename(masculina=pm10,
#            feminina=pf10) |> 
#     mutate( masculina=as.numeric(masculina),
#             feminina=as.numeric(feminina),
#             masculina_inv= masculina*-1,
#             seq = 1:dim(pop_fort10_22)[1],
#             perc_fem = round(feminina/sum(feminina)*100,2),
#             perc_masc = round(masculina/sum(masculina)*100,2)
#     ) |> 
#     arrange(-seq) 
#   
#   
#   
#   a <- pop_fort_graf10 |> 
#     e_charts(x=grupo_de_idade) |> 
#     e_bar(masculina_inv,stack = "grp2",bind=perc_masc,name='Homens',
#           label=list(formatter=  htmlwidgets::JS("
#        function (params) {
#         return Math.abs(params.name)+'%';}"),
#                      show= T,position= "left"#,color='green'#,fontWeight= "bold"
#           )
#     ) |> 
#     e_bar(feminina,stack = "grp2",bind=perc_fem,name='Mulheres',
#           label=list(formatter=  htmlwidgets::JS("
#        function (params) {
#         return Math.abs(params.name)+'%';}"),
#                      show= T,position= "right"#,color='blue'#,fontWeight= "bold"
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
#     e_title(text='População residente em Fortaleza - 2010',subtext="Segundo sexo e grupo de idade" )
#   a
#   htmlwidgets::saveWidget(a, "C:/Dados/Censo2022/temp.html");
#   webshot2::webshot("C:/Dados/Censo2022/temp.html","C:/Dados/Censo2022/piramide_10.png",
#                     vwidth = 900, vheight = 800,delay = 1)
#   
#   
# }
# 
# ##
# 
# #Grafico distribuição por grupo de idade criancas e idosos
# pop_00_22 <- readxl::read_excel('C:/Romulo/Dados/Censo2022/censo_2000_2022.xlsx') 
# 
# 
# pop_t <- pop_00_22 |>
#   group_by(Ano,Faixa) |> 
#   summarise(pop=sum(pop)) |> 
#   group_by(Ano) |> 
#   mutate(Perc=round(pop/sum(pop)*100,2)) |> 
#   arrange(-Ano) |> 
#   mutate(Ano=as.character(Ano)) 
# 
# pop_s <- pop_00_22 |>  
#   group_by(Ano,Sexo) |> 
#   mutate(Perc=round(pop/sum(pop)*100,2)) |> 
#   arrange(-Ano) |> 
#   mutate(Ano=as.character(Ano)) 
# 
# pop_t |>
#   group_by(Ano) |> 
#   summarise(sum(pop))
# 
# M <- pop_s |>
#   group_by(Faixa) |> 
#   filter(Sexo=='Mulheres') |> 
#   e_chart(x=Ano) |> 
#   e_bar(Perc,stack='r',bind=Perc,
#         label=list(formatter=  htmlwidgets::JS("
#        function (params) {
#         return Math.abs(params.name)+'%';}"),
#                    show= T,position= "inside",fontWeight= "bold"
#         )) |> 
#   e_tooltip() |> 
#   e_flip_coords() |> 
#   e_x_axis(max=100,show=F) |> 
#   e_theme('macarons') |> 
#   e_legend(show=F) |> 
#   e_title(subtext='Mulheres')
# M
# 
# H <- pop_s |>
#   group_by(Faixa) |> 
#   filter(Sexo=='Homens') |> 
#   e_chart(x=Ano) |> 
#   e_bar(Perc,stack='r',bind=Perc,
#         label=list(formatter=  htmlwidgets::JS("
#        function (params) {
#         return Math.abs(params.name)+'%';}"),
#                    show= T,position= "inside",fontWeight= "bold"
#         )) |> 
#   e_tooltip() |> 
#   e_flip_coords() |> 
#   e_x_axis(max=100,show=T) |> 
#   e_theme('macarons') |> 
#   e_legend(show=F)|> 
#   e_title(subtext='Homens')
# H
# 
# #writexl::write_xlsx(pop_t,"C:/Romulo/Dados/Censo2022/pop_t.xlsx")
# pop_t <- readxl::read_excel("C:/Romulo/Dados/Censo2022/pop_t.xlsx") |> 
#   mutate(Perc=round(Perc,1))
# 
# 
# Tot <- pop_t |>
#   group_by(Faixa) |> 
#   e_chart(x=Ano) |> 
#   e_bar(Perc,stack='r',bind=Perc,
#         label=list(formatter=  htmlwidgets::JS("
#        function (params) {
#         return Math.abs(params.name)+'%';}"),
#                    show= T,position= "inside",fontWeight= "bold"
#         )) |> 
#   e_tooltip() |> 
#   e_flip_coords() |> 
#   #e_x_axis(max=100,show=F) |> 
#   e_theme('macarons') |> 
#   e_legend(show=T,top= "6%")|> 
#   e_title(text='Distribuição populacional de Fortaleza por grupo etário')#,subtext = 'Geral') #|> 
#   # e_data(pop_s |> group_by(Faixa) |> 
#   #          filter(Sexo=='Homens') ,x=Ano) |> 
#   # e_bar(Perc,stack='rs',bind=Perc,
#   #       label=list(formatter=  htmlwidgets::JS("
#   #      function (params) {
#   #       return Math.abs(params.name)+'%';}"),
#   #                  show= T,position= "inside",fontWeight= "bold"
#   #       ))|> 
#   # e_flip_coords() |> 
#   # e_x_axis(max=100,show=F)
# 
# Tot
# htmlwidgets::saveWidget(Tot, "C:/Romulo/Dados/Censo2022/temp.html");
# webshot2::webshot("C:/Romulo/Dados/Censo2022/temp.html","C:/Romulo/Dados/Censo2022/grupoetario_.png",
#                   vwidth = 700, vheight = 500,delay = 1)
# 
# a <- e_arrange(H,M,rows = 2)
# 
# htmlwidgets::saveWidget(a, "C:/Dados/Censo2022/temp.html");
# webshot2::webshot("C:/Dados/Censo2022/temp.html","C:/Dados/Censo2022/dist00_22.png",
#                   vwidth = 700, vheight = 600,delay = 1)
# ####
# 
# 
# # #Grafico serie historica fortaleza
# 
# pop_serie <- readxl::read_excel('Dados/censo_2000_2022.xlsx',2) |> 
#   mutate(Ano=as.character(Ano))
# formatar_numero_br <- function(serie) {
#   htmlwidgets::JS(
#     glue::glue(
#       "function(params) {return Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(params.value[{{serie}}]);}",
#       .open = "{{",
#       .close = "}}"
#     )
#   )
# }
# pop_serie2 <- pop_serie |> 
#   mutate(desc='População',
#          Ano=as.numeric(Ano))
# 
# 
# 
# 
#  p <-  e_chart(pop_serie2   ,x=Ano) |> 
#   e_theme('macarons') |> 
#   e_line(População,smooth = T,bind=desc,
#          endLabel=list( show=T, color='green',fontWeight= "bold" ,fontSize =12,
#                         formatter= htmlwidgets::JS("
#          function (params) {
#          return params.name }"))
#          ) |> 
#   e_labels(formatter = formatar_numero_br(1),fontSize = 13,color='black')   |> 
#   e_format_y_axis(suffix = "",prefix = "",formatter = e_axis_formatter(locale = "PT",digits = 0)) |> 
#   #e_format_x_axis(suffix = "",prefix = "") |> #formatter = e_axis_formatter(locale = "PT",digits = 0)) |> 
#   e_y_axis(show=T) |> 
#   e_x_axis(min=1872,max=2022, 
#            axisLabel= list(
#            customValues= list(c('1870', '1890'))
#            ),
#            
#            
#     formatter= htmlwidgets::JS("function(params) {
#                return Math.abs(params);
#              }")
#   )  |> 
#   e_legend(show=F) 
# p
# 
# htmlwidgets::saveWidget(a, "C:/Dados/Censo2022/temp.html");
# webshot2::webshot("C:/Dados/Censo2022/temp.html","C:/Dados/Censo2022/dist00_22.png",
#                   vwidth = 1224, vheight = 540,delay = 1)
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
# 
# 
# 
# 
# 
# 
# 
# pop_fort_graf10 <- pop_fort10_22 |>
#     mutate( pf22=as.numeric(pf22),
#             pm22=as.numeric(pm22),
#             pf10=as.numeric(pf10),
#             pm10=as.numeric(pm10),
#             pm22_inv= pm22*-1,
#             pm10_inv= pm10*-1,
#           seq = 1:dim(pop_fort10_22)[1],
#           # perc_fem = round(feminina/sum(feminina)*100,2),
#           # perc_masc = round(feminina/sum(masculina)*100,2)
#   ) |> 
#   arrange(-seq) 
# 
# 
# 
# a <- pop_fort_graf10 |> 
#   e_charts(x=grupo_de_idade) |> 
#   e_bar(pm22_inv,stack = "grp2",color='#40711A',#bind=perc_masc,
#         name='Homens',barWidth= '90%',
#         itemStyle=list( opacity= .9)
#        #  label=list(formatter=  htmlwidgets::JS("
#        # function (params) {
#        #  return Math.abs(params.name)+'%';}"),
#        #             show= T,position= "left"#,color='green'#,fontWeight= "bold"
#        #  )
#   )  |>
#   e_bar(pf22,stack = "grp",#bind=perc_fem,
#         name='Mulheres',color='#A02EA0',
#         itemStyle=list( opacity= 0.5),
#        #  label=list(formatter=  htmlwidgets::JS("
#        # function (params) {
#        #  return Math.abs(params.name)+'%';}"),
#        #             show= T,position= "right"#,color='blue'#,fontWeight= "bold"
#        #  )
#   ) |> 
#  e_data(data=pop_fort_graf10,x=grupo_de_idade) |> 
#  e_bar(pm10_inv,stack = "grp",color='#103A13',#bind=perc_masc,
#         name='2010',#barWidth= '70%',
#         itemStyle=list( opacity= 0.5)
#         #  label=list(formatter=  htmlwidgets::JS("
#         # function (params) {
#         #  return Math.abs(params.name)+'%';}"),
#         #             show= T,position= "left"#,color='green'#,fontWeight= "bold"
#         #  )
#   ) |>
#  e_bar(pf22,stack = "grp2",#bind=perc_fem,
#       name='2022', color='#4C1D64',
#       barGap='-90%',barWidth= '80%',
#       itemStyle=list( opacity= 0.5)
#              #  label=list(formatter=  htmlwidgets::JS("
#      # function (params) {
#      #  return Math.abs(params.name)+'%';}"),
#      #             show= T,position= "right"#,color='blue'#,fontWeight= "bold"
#      #  )
# ) |>
#   e_flip_coords() |> 
#   e_y_axis(offset=30) |> 
#   e_x_axis(#max=117799,
#     #min=-127799,
#     type= 'value',
#     show= F,
#     axisLabel=list(
#       formatter= htmlwidgets::JS("function(params) {
#                return Math.abs(params);
#              }")
#     )
#   ) |>
#   e_grid(left='20%',) |> 
#   #  e_theme('macarons') |> 
#   e_legend( top= "3%",selected= list()) |> 
#   e_tooltip( trigger = c("axis"),axisPointer=list(type='shadow')) |> 
#   
#   e_title(text='População residente em Fortaleza',subtext="Segundo sexo e grupo de idade" )
# a
# htmlwidgets::saveWidget(a, "C:/Dados/Censo2022/temp.html");
# webshot2::webshot("C:/Dados/Censo2022/temp.html","C:/Dados/Censo2022/piramide_10.png",
#                   vwidth = 900, vheight = 800,delay = 1)
# 
# 
# 
