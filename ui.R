
# Load library --------------------------------------------------------------------------------
# Oi Romulo
box::use(
  shiny[...],
  bs4Dash[...],
  bslib[...],
  reactable[reactableOutput],
  echarts4r[echarts4rOutput,
            e_theme_register],
  datamods[select_group_ui],
  waiter[...],
  leaflet[leafletOutput],
  #reactable[reactableOutput],
  shinycssloaders[withSpinner],
  fresh[use_theme,
        use_googlefont,
        create_theme,
        bs4dash_font,bs4dash_color,bs4dash_status,bs4dash_layout],
  D3plusR[d3plusOutput],
  #shinyWidgets[pickerInput]
)





# Load data -----------------------------------------------------------------------------------
df_faixa <- readRDS('Dados/df_faixa.rds')
glossario <-readRDS('Dados/glossario.rds')

# Funcao --------------------------------------------------------------------------------------




constroi_glossario <- function(nchunks = 3) {
  df <- split(
    glossario,
    rep(1:nchunks,
        length.out = nrow(glossario),
        each = ceiling(nrow(glossario)/nchunks)))
  
  width <- 12%/%nchunks
  
  lapply(1:nchunks, function(i){
    div(
      class = sprintf("col-xl-%s", width),
      bs4Dash::box(
        width = 12,
        closable = FALSE,
        maximizable = TRUE,
        collapsible = FALSE,
        do.call("accordion", c(
          list(id = sprintf('accordion_glossario_%s', i)),
          apply(df[[i]], 1, function(r) bs4Dash::accordionItem(title = r[1], r[2]))))))})
}




# Tema ----------------------------------------------------------------------------------------


light_blue <- "#014ffd"
red <- "#ff585d"

yellow <- "#fbb81c"

cyan <- "#1eb1e7"
blue2 <- "#1a649d"
orange1 <- "#ff9900"
violet2 <- "#5b5b9b"

orange2 <- "#925106"



## Status colors ##


primary <- yellow
secondary <- violet2
success <- NULL
info <- blue2
warning <- orange1
danger <- red
light <- NULL
dark <- NULL


## Main colors ##

blue <- light_blue
lightblue <- cyan
navy <- NULL
cyan <- NULL
teal <- NULL
olive <- NULL
green <- NULL
lime <- NULL
orange <- NULL
yellow <- NULL
fuchsia <- NULL
purple <- NULL
maroon <- NULL
red <- red
black <- NULL
gray_x_light <- NULL
gray_600 <- NULL
gray_800 <- NULL
gray_900 <- NULL
white <- NULL

main_bg <- NULL
text_dark <- NULL
text_light <- NULL
sidebar_light_bg <- main_bg
sidebar_light_color <- text_light
sidebar_light_hover_color <- NULL
sidebar_light_submenu_bg <- main_bg
sidebar_light_submenu_color <- NULL
sidebar_light_submenu_hover_color <- NULL


font1 <- "Exo 2"

font2 <- "Roboto"

font3 <- "Roboto Mono"

main_font <- "'Exo 2', sans-serif"

secondary_font <- "'Roboto', sans-serif"

monospace_font <- "'Roboto Mono', monospace"

base_font <- "Exo 2"

diobs_theme <- create_theme(
  bs4dash_font(
    size_base = "1rem",
    size_lg = NULL,
    size_sm = NULL,
    size_xs = NULL,
    size_xl = NULL,
    weight_light = NULL,
    weight_normal = NULL,
    weight_bold = NULL,
    family_sans_serif = main_font,
    family_monospace = monospace_font,
    family_base = main_font
  ),
  bs4dash_color(
    blue = blue,
    lightblue = lightblue,
    navy = navy,
    cyan = cyan,
    teal = teal,
    olive = olive,
    green = green,
    lime = lime,
    orange = orange,
    yellow = yellow,
    fuchsia = fuchsia,
    purple = purple,
    maroon = maroon,
    red = red,
    black = black,
    gray_x_light = gray_x_light,
    gray_600 = gray_600,
    gray_800 = gray_800,
    gray_900 = gray_900,
    white = white
  ),
  bs4dash_status(
    primary = primary,
    secondary = secondary,
    success = success,
    info = info,
    warning = warning,
    danger = danger,
    light = light,
    dark = dark
  ),
  bs4dash_layout(
    font_size_root = NULL,
    sidebar_width = NULL,
    sidebar_padding_x = NULL,
    sidebar_padding_y = NULL,
    sidebar_mini_width = NULL,
    control_sidebar_width = NULL,
    boxed_layout_max_width = NULL,
    screen_header_collapse = NULL,
    main_bg = main_bg,
    content_padding_x = NULL,
    content_padding_y = NULL
  )
)




# Load UI -------------------------------------------------------------------------------------




ui <- dashboardPage(
 
  # preloader = list(
  #   html = tagList(
  #     spin_cube_grid(),
  #     br(),
  #     "Carregando ..."
  #   ),
  #   color = primary
  # ),

  dark = F,
  help = NULL,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  header = dashboardHeader(
    
    title = dashboardBrand(
     
      title = "Mulher",
      color ='secondary',# "primary",
      # href = "https://observatoriodefortaleza.fortaleza.ce.gov.br/",
      image = 'Brasão_de_Fortaleza.svg',
      opacity = 1
    ),
    fixed = FALSE
  ),

  
  
## Sibebar -----------------------------------------------------------------

  
  sidebar = dashboardSidebar(
    fixed = TRUE,
    skin = "light",
    status = "info",
    id = "sidebar",
    collapsed = FALSE,
    sidebarMenu(
      id = "current_tab",
      flat = FALSE,
      compact = FALSE,
      childIndent = TRUE,
      sidebarHeader("Introdução"),
      menuItem(
        "Abertura",
        tabName = "Abertura",
        icon = icon("venus")
      ),
      sidebarHeader("Visualizações"),
      menuItem(
        "Demografia",
        tabName = "Demografia",
        icon = icon("users")
      ),
      menuItem(
        "Educação",
        tabName = "Educação",
        icon = icon("book-open")
      ),
      menuItem(
        "Saúde",
        tabName = "Saúde",
        icon = icon("heart-pulse")
      ),
      
      menuItem(
        "Mercado de trabalho",
        tabName = "Mercado",
        icon = icon("money-bill-trend-up")
      ),
      
      menuItem(
        "Violência",
        tabName = "Violência",
        icon = icon("gun")
      ),
      # menuItem(
      #   "Bairros",
      #   #tabName = "metodologia",
      #   icon = icon("map"),
      #   
      #   menuSubItem("Raça",
      #               tabName = "Raça",
      #               icon = icon("circle")
      #   ),
      #   menuSubItem("Idade",
      #               tabName = "Idade",
      #               icon = icon("circle")
      #   ),
      #   menuSubItem("Infraestrutura",
      #               tabName = "infraestrutura",
      #               icon = icon("circle")
      #   )
      #   
      # ),
      sidebarHeader("Metadados"),
      menuItem(
        "Glossário",
        tabName = "glossario",
        icon = icon("magnifying-glass-chart")
      ),
      menuItem(
        "Referências",
        tabName = "fonte",
        icon = icon("server")
      ),
      menuItem(
        "Sobre",
        tabName = "sobre",
        icon = icon("info")
      )
    )
  ),
  



## Corpo -------------------------------------------------------------------

  
  body = dashboardBody(
    use_googlefont(font1),
    use_googlefont(font2),
    use_googlefont(font3),
    use_theme(diobs_theme),
    e_theme_register(
      paste(readLines("www/atlas_capital_humano.json"), collapse = ""),
      "diobs"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      # Listen for dark-mode messages
      tags$script("
      $(document).ready(function(){
        $('.brand-image').removeClass('elevation-3 img-circle');
      })

      Shiny.addCustomMessageHandler('dark-mode', function(dark_mode) {
        if (dark_mode) {
          $('#footer-logo').find('img').map(function() { $(this).attr('src', $(this).attr('src').replace('dark', 'light')) });
        } else {
          $('#footer-logo').find('img').map(function() { $(this).attr('src', $(this).attr('src').replace('light', 'dark')) });
        }
      });
    "),
      tags$script(src = "https://polyfill.io/v3/polyfill.min.js?features=es6"),
      tags$script(id="MathJax-script", src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
    ),
  tabItems(


    
### Vizualizações --------------------------------------------------------------

#### Demografia --------------------------------------------------------------
tabItem(
  tabName = "Demografia",
  #h1("Demografia dos bairros de Fortaleza - Censo 2022"),
  hr(), 
  selectizeInput(inputId='select_1',label = 'Bairro:', choices = sort(unique(df_faixa$NM_BAIRRO)),multiple = TRUE,
                 selected= 'FORTALEZA',width = '40%',
                 options = list('plugins' = list('remove_button'),maxItems = 3)),
   
   fluidRow(
    
    ### Pizza sexo 1
    column(width = 3,
           echarts4rOutput('pizza_1.1',height = '80%')),
    ### Qtd Moradores 1
    valueBoxOutput("vbox_mulher.1", width = 2),
    valueBoxOutput("vbox2.1", width = 2),
    valueBoxOutput("vbox1.1", width = 2),
    echarts4rOutput('raca1', width = '25%',height=200)
    
    
  ),
  conditionalPanel(condition  = "input.select_1.length >1",
                   fluidRow(
                     ### Pizza sexo 2
                     column(width = 3,
                            echarts4rOutput('pizza_1.2',height = '80%')),
                     ### Qtd Moradores 2
                     valueBoxOutput("vbox_mulher.2", width = 2),  
                     valueBoxOutput("vbox2.2", width = 2), 
                     valueBoxOutput("vbox1.2", width = 2),
                     echarts4rOutput('raca2', width = '25%',height=200)
                     ) )
  

  ,
  fluidRow(
  box(
    title = 'Pirâmide etária',
    id = "mybox",
    status = "danger",
    # background = 'white',
    solidHeader = F,
    width = 8,
    # gradient = TRUE,
    # collapsible = TRUE,
    # closable = TRUE,
    
    
    conditionalPanel(condition  = "input.select_1.length ==1 ",
                     layout_columns( 
                       withSpinner( echarts4rOutput("piramide_1.1")))        
    ),
    conditionalPanel(condition  = "input.select_1.length == 2 ",
                     layout_columns( 
                       echarts4rOutput("piramide_2"),
                       echarts4rOutput("piramide_1.2"))
    )),
  box(
    title = 'Mapa',
    id = "mybox",
    status = "danger",
    # background = 'white',
    solidHeader = F,
    width = 4,
    tabsetPanel(
      id = "tabs_ba",
      tabPanel(
        title = '% de mulheres' ,
       
      ),
      tabPanel(
        title = 'Razão sexo',
      
      ),
      tabPanel(
        title = 'População',
        
      ),
      tabPanel(
        title = 'Densidade' ,
       

      )
      ),
    
     withSpinner(leafletOutput('map_bairro',width = '100%', height = 355))
  
    # br(),
    # fluidRow( downloadButton("down_bairro", ".GEOJSON"), downloadButton("down_bairro_xlsx", ".XLSX"))
  )
  ),
  
  
  
   reactableOutput("table_bairro")
  
  ),
  
 
  
#### Violencia --------------------------------------------------------------  
tabItem(
  tabName = "Violência",
  h1("sdg"),
  hr() ),
     
#### Mercado de trabalho  --------------------------------------------------------  
tabItem(
  tabName = "Mercado",
  hr(),
  fluidRow(
  column(width = 2,
        
  shinyWidgets::pickerInput( inputId = "Ano_filter",
                             label = 'Ano',
                             choices = 2012:2022,
                             multiple = FALSE,
                             selected = 2022,
                             choicesOpt = list(content = 2012:2022)
                          )
  ),
  column(width = 6,
  select_group_ui(
    id = "Filtro_merc",
    inline = TRUE ,
    btn_reset_label = "Resetar filtros",
    params = list(
      list(inputId = "V2007", label = "Sexo", placeholder = 'Todos' ,  updateOn = "close",selected = 'Mulher'),
      list(inputId = "V2010", label = "Raça/cor", placeholder = 'Todos',  updateOn = "close"),
      list(inputId = "VD3004", label = "Escolaridade", placeholder = 'Todos',  updateOn = "close"),
      list(inputId = "Composição", label = "Composição", placeholder = 'Todos',  updateOn = "close")
                  ) 
                ),
  ),
  
  br(),
  
  box(
    title = 'Tipo de ocupação',
    id = "mybox",
    status = "danger",
    # background = 'white',
    solidHeader = F,
    width = 8,
    # gradient = TRUE,
    # collapsible = TRUE,
    # closable = TRUE,
    radioButtons(
      "select_tipo_merc", NULL ,
      c( "Formal e informal", "Geral"),
      selected = "Formal e informal",
      inline = F
    ),
    
    
    conditionalPanel(condition  = "input.select_tipo_merc == 'Geral' ",
                     layout_columns( 
                       #withSpinner(d3plusOutput('F_cnaes'))
                       htmlOutput("pedro", container = tags$li, class = "custom-li-output")
                       
                       )        
    ),
    conditionalPanel(condition  = "input.select_tipo_merc =='Formal e informal' ",
                     layout_columns( 
                       # withSpinner(d3plusOutput('F_cnaes_form')),
                       # withSpinner(d3plusOutput('F_cnaes_inform'))
                       )
    )),
  echarts4rOutput('perc_informal',width = "32%"),
  echarts4rOutput('perc_informal_sexo',width = "32%"),
  
  )
  
  ),     

#### Educação  --------------------------------------------------------  
tabItem(
  tabName = "Educação",
  h1("sdg"),
  hr() ),  

#### Saúde  --------------------------------------------------------  
tabItem(
  tabName = "Saúde",
  h1("sdg"),
  hr() ),  

### Glossário ---------------------------------------------------------------

     
    tabItem(
      tabName = "glossario",
      tagList(
        fluidRow(
          class = "align-center justify-content-center text-center mt-2",
          column(
            width = 8,
            h1("Glossário"),
            hr(class = "divider")
          )
        ),
        fluidRow(
          constroi_glossario(nchunks = 3)
        )
        
      )
    ),


### Referências -------------------------------------------------------------


    tabItem(
      tabName = "fonte",
      
      h2('Referências bibliográficas'),
      br(),
      column(8,
      p("BARREIRA, Irlys. GONÇALVES, Danyelle. DANTAS, Eustógio (org).",
      strong('Aprendizados e desafios da participação: a experiência do Plano
      Integrado de Regularização Fundiária (PIRF).'), "Fortaleza: Expressão
      Gráfica e Editora, 2021."),
      
      p("BRASIL.",
        strong('Estatuto da Cidade Lei no 10.257/2001.'),"Brasília, DF: Senado Federal, Coordenação de Edições Técnicas, 2024. "),
      
      
      p("BRASIL. ",
        strong('Guia para o mapeamento e caracterização de assentamentos precários.'),"Brasília: Ministério das Cidades, 2010."),
      
     
      
      p("BRASIL. ",
        strong('O que é regularização fundiária de assentamentos urbanos?'), "Brasília: Ministério das cidades, 2023. 
        Disponível em: https://www.gov.br/cidades/pt-br/acesso-a-informacao/perguntas-frequentes/desenvolvimento-regional/regularizacao-fundiaria/1-o-que-e-regularizacao"),
      
      
      p("BRASIL. ",
        strong('Sistema Nacional de Habitação de Interesse Social.'), "Brasília: Ministério das cidades, 2024. 
        Disponível em: https://www.gov.br/cidades/pt-br/assuntos/habitacao/sistema-nacional-de-habitacao-de-interesse-social"),
      
      p("FORTALEZA. ",
        strong('Conheça as etapas.'), "Fortaleza: plataforma do Plano Diretor de Fortaleza, 2024. Disponível em: https://planodiretor.fortaleza.ce.gov.br"),
      
      
      p("IBGE – INSTITUTO BRASILEIRO DE GEOGRAFIA E ESTATÍSTICA.",
        strong(' Estrutura
      territorial: malha dos setores censitários.'), "Rio de Janeiro, RJ, 2021b.
      Disponível em:
        <https://www.ibge.gov.br/geociencias/organizacao-do-territorio/estrutura-territorial/26565->
        malhas-de-setores-censitarios-divisoes-intramunicipais.html?=&t=saiba-mais-edicao.
      Acesso: 08 mar. 2021."),
      
      p("IBGE.",
        strong(' Malha de Setores Censitários.')," Brasília: IBGE 2022. Disponível em:  https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/26565-malhas-de-setores-censitarios-divisoes-intramunicipais.html?=&t=o-que-e"),
      
      p("OBSERVATÓRIO DAS METRÓPOLES.",
        strong(' Zonas Especiais de Interesse Social (ZEIS) de Fortaleza.'), "Fortaleza: Observatório das metrópoles, 2021. Disponível em: https://www.observatoriodasmetropoles.net.br/zonas-especiais-de-interesse-social-zeis-de-fortaleza/"),
      
      p("R Core Team (2024). ",
        strong('R: A Language and Environment for Statistical
      Computing.'), "R Foundation for Statistical Computing, Vienna, Austria.
      <https://www.R-project.org/>."),
      
      )
    

      
### Sobre -------------------------------------------------------------------

      
    ),
    tabItem(
      tabName = "sobre",
      fluidPage(
        h1("Sobre o painel"),
        p("Atualização de dados demográficos das Zonas Especiais de Interesse Social, 
        Assentamentos Precários e bairros de Fortaleza com base nas informações dos setores censitários
          do Ibge de 2022."),
  
        h2("Autor"),
        p(
          "Rômulo Andrade",
          a(
            href = "mailto:romulo.andrade@iplanfor.fortaleza.ce.gov.br",
            "<romulo.andrade@iplanfor.fortaleza.ce.gov.br>"
          ),
          br(),
          tags$i("Analista de Planejamento e Gestão - IPPLAN")
        ),
        h2("Contribuições"),
 
        p(
          "Felipe Neto",
          a(
            href = "mailto:felipefranklinneto@gmail.com",
            "felipefranklinneto@gmail.com"
          ),
          br(),
          tags$i("Núcleo de Difusão de Conhecimento - IPPLAN")
        ),
        p(
          "Larissa Tabita",
          a(
            href = "mailto:larissatabita@gmail.com ",
            "larissatabita@gmail.com "
          ),
          br(),
          tags$i("Estagiária de Estatística - IPPLAN")
        ),
        
        
        # h2("Código-Fonte"),
        # p(
        #   a(
        #     href="https://github.com/RomuloAndrade/Previo",
        #     "https://github.com/RomuloAndrade/Previo"
        #   )
        # )
      )
    )
  )
  ),


### Footer ------------------------------------------------------------------


footer = dashboardFooter(
  fixed = FALSE,
  left = tagList(
    # Versão mobile
    div(
      span(
        format(Sys.Date(), "%Y, "),
        a(
          href = "https://observatoriodefortaleza.fortaleza.ce.gov.br/",
          target = "_blank", "Observatório de Fortaleza"
        )
      )
    )
  ),
  right = tagList(
    tags$ul(
      id = "footer-logo",
      tags$li(
        a(
          href = "https://observatoriodefortaleza.fortaleza.ce.gov.br/",
          target = "_blank",
          alt = "",
          img(
            src = "img/logo-diobs-dark.svg",
            height = "30"
          )
        )
      ),
      tags$li(
        img(
          src = "img/logo-iplanfor-dark.svg",
          height = "25"
        )
      ),
      tags$li(
        a(
          href = "https://www.fortaleza.ce.gov.br/",
          target = "_blank",
          alt = "",
          img(
            src = "img/logo-pmf-dark.svg",
            height = "35"
          )
        )
      )
    )
  )
),
 
  title = "Censo nos Bairros"
)






#: Este projeto propõe a criação de um painel de indicadores para monitorar a
# proximidade da população a equipamentos públicos, como postos de saúde e 
# escolas, com o objetivo de auxiliar gestores públicos e urbanistas na tomada 
# de decisões. O painel fornecerá uma ferramenta visual e interativa para medir 
# a acessibilidade a serviços essenciais, identificar áreas desassistidas e 
# promover políticas públicas mais justas. Através de dados georreferenciados, 
# será possível monitorar a distribuição dos serviços na cidade e avaliar a 
# equidade no acesso, contribuindo para uma gestão pública mais eficiente e inclusiva.