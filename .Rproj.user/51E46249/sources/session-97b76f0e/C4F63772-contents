#setwd('C:/Romulo/Dados/Censo Bairros')

library(stringr)
library(dplyr)
library(tidyr)
library(sf)
library(echarts4r)
library(leaflet)
library(leaflet.extras)
bairros_cor<- readxl::read_excel("Dados/Agregados_por_bairros_cor_ou_raca_BR.xlsx")

bairros_cor <- bairros_cor |>
  filter(str_detect(CD_BAIRRO, "^230440")) |>
  mutate_at(vars(starts_with("V")), as.numeric )

saveRDS(bairros_demografia,'Dados/bairros_cor.rds')
bairros_cor <- readRDS('Dados/bairros_cor.rds') 

bairros_cor_M <- bairros_cor |> 
  select(1:2,V01322:V01326) |> 
  pivot_longer(cols = V01322:V01326,
               names_to = 'Cor_raça',
               values_to = 'Homens') |> 
  mutate(Cor_raça = case_match(
    Cor_raça,
    'V01322'	~ 'branca',
    'V01323'	~ 'preta',
    'V01324'	~ 'amarela',
    'V01325'  ~ 'parda',
    'V01326'  ~ 'indígena'
    )
  )


bairros_cor_F <- bairros_cor |> 
  select(1:2,V01327:V01331) |> 
  pivot_longer(cols = V01327:V01331,
               names_to = 'Cor_raça',
               values_to = 'Mulheres') |> 
  mutate(Cor_raça = case_match(
    Cor_raça,
    'V01327'	~ 'branca',
    'V01328'	~ 'preta',
    'V01329'	~ 'amarela',
    'V01330'  ~ 'parda',
    'V01331'  ~ 'indígena')
  )


df_cor <- left_join(bairros_cor_M,bairros_cor_F)
df_cor_ <- df_cor |> pivot_longer(cols=  Homens:Mulheres,  
                        names_to = 'Sexo' , 
                        values_to = 'Qtd'  )

saveRDS(df_cor_,'Dados/df_cor_.rds')


df_cor_ |> 
  filter(NM_BAIRRO=='Centro') |> 
  group_by(Sexo) |> 
  e_chart(x=Cor_raça,name='Moradores') |> 
  e_bar(Qtd) |> 
  e_color(c("#b6a2de","#2ec7c9" )) |> 
  e_toolbox(iconStyle= list(
    color= "rgba(35, 210, 32, 1)"
  )) |>
  e_toolbox_feature(feature = "dataView") |>  
  e_tooltip(trigger="item", formatter =
              htmlwidgets::JS(
                "function(p) {
          v = Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(p.value[1]);
          return('<strong>' + p.seriesName + '</strong>' +
          '<br>Moradores: ' + Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(p.value[1]) +
          '<br>Raça_cor: ' +p.value[0]  );
          }"),
            textStyle=list(fontFamily="arial", fontSize=12)
            
  )|>
  e_format_y_axis(
    suffix = "",
    prefix = "",
    formatter =  e_axis_formatter(locale = "PT", digits = 0)
  ) |>
  e_legend( orient= "vertical",
              right= 0) |> 
  e_y_axis(show=F) |> 
  e_grid(top = "-5%")











e1 <- cars |>
  e_charts(
    speed,
    height = 200
  ) |>
  e_scatter(dist) |>
  e_datazoom(show = FALSE) |>
  e_group("grp") # assign group

e2 <- cars |>
  e_charts(
    dist,
    height = 200
  ) |>
  e_scatter(speed) |>
  e_datazoom() |>
  e_group("grp") |> # assign group
  e_connect_group("grp") # connect

if (interactive()) {
  e_arrange(e1, e2, title = "Linked datazoom")
}
