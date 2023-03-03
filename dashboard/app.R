# 0.1 Load libraries ----
library(shiny)
library(shinyWidgets)
library(shinydashboard)
#library(shinycssloaders)
library(waiter)
library(tidyverse)
library(sf)
library(geojsonsf)
library(leaflet)
library(plotly)
library(geojsonio)
library(reactable)
library(kableExtra)
library(htmltools)
library(paletteer)
library(comprehenr)
library(tippy)
library(profvis)

# 0.2 Load data -------
load("data/datos.RData") # cargar datos
load("data/geo_datos.RData")
load("data/diccionario.RData") # cargar diccionario
#load("data/intro_table.RData") # cargar intro_table
load("data/data_sun.RData") # cargar data para sunburst
admin0 <- geojson_sf('data/admin_0.json')

# 0.3 Source utils----
source("utils.R")

# temporal for manual checking only
#diccionario['unidad'][diccionario['codigo']=='sb2'] <- 'porcentaje2'
# datos['sb2'] <- datos['sb2']*100 # Esto sirve para escalar cancer y diabetes y que se puedan mostrar las barras

# 0.4 Sunburst of variables ----
sun_vars <- plot_ly(
  data_sun,
  labels = ~Name, 
  parents = ~parents, 
  values= ~values, 
  type = 'sunburst',
  rotation = 155,
  width = 450,
  height = 350,
  branchvalues = "total",
  hoverinfo = "text",
  hovertext = ~paste0("<b>", ids, ':</b><br>', nombre, '<br>',
                      "<b>Indicador:</b><br>",
                      str_wrap(indicadores, width = 35)
                      )
  ) %>% 
  layout(
    font = pl_sunburst,
    margin = list(l = 1, r = 1, b = 25, t = 0),
    sunburstcolorway = c(col_d1, col_d2, col_d3, col_d4)) %>%
  config(displaylogo = FALSE)

# 0.5 Sunburst mini ----
sun_mini <- plot_ly(
  filter(
    data_sun, ids != "Variable"),
  labels = ~Name, 
  parents = ~parents, 
  values = ~values, 
  type = 'sunburst', 
  rotation = 155,
  branchvalues ="total",
  width = 150,
  height = 150) %>% 
  layout(
    font = pl_sunburst,
    autosize = FALSE, 
    hovermode = FALSE,
    margin = list(l = 1, r = 1, b = 1, t = 1),
    sunburstcolorway = c(col_d1, col_d2, col_d3, col_d4)) %>%
  config(displayModeBar = F)

# 0.6 Items input indicadores----
items_ind <- setNames(diccionario$nombre_input, diccionario$ind_html)

# 0.7 Items input paises----
items_reg <- setNames(unique(datos$region_input), unique(datos$reg_html))

# 0.8 PickerInput styling:----
## Color list
col.list <- c("Black",
              rep_len(col_d3, 10), 
              rep_len(col_d4, 9), 
              rep_len(col_d1, 13),
              rep_len(col_d2, 12))
## Change the color
picker_style <- paste0("color:",col.list,";")
## Change the font
picker_style <- paste0(picker_style,"font-family: Arial;")
## Change to bold
## picker_style <- paste0(picker_style,"font-weight: bold;")

# 0.9 Crear Leaflet----
map <- leaflet(
    width = "100%", height = "600px",
    options = leafletOptions(
      minZoom = 3, maxZoom = 8, 
      zoomControl = FALSE)) %>%
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
        }") %>% 
  setView(lng = -74.99, lat = -9.19, zoom = 3) %>% 
  setMaxBounds(lng1 = -120, lng2 = -34,
               lat1 = 34 , lat2 = -58) %>%
  addMapPane("hombres", zIndex = 410) %>% # Level 1: bottom
  addMapPane("adm0", zIndex = 420) %>%  # Level 2: middle
  addMapPane("labels", zIndex = 430) %>% # Level 3: top
  addMapPane("mujeres", zIndex = 415) %>% # Level 3: top
  addMapPane("brecha", zIndex = 417) %>% # Level 3: top
  addProviderTiles(
    providers$CartoDB.PositronOnlyLabels,
    providerTileOptions(opacity = 0.8),
    options = pathOptions(pane = "labels")) %>% 
  addPolygons(
    data = admin0,
    weight = 0.5,
    color = "black",
    fill = FALSE,
    fillColor = "white",
    fillOpacity = 1,
    smoothFactor = 0,
    options = pathOptions(pane = "adm0")) 

# 0.10 Loaders options----
options(spinner.color = central_blue)

# 1 Shiny UI #########################################
ui <- fluidPage(
  tags$head( 
    tags$head(includeCSS("www/styles.css"))),

  useShinydashboard(),
  
  autoWaiter(
    html = spin_throbber(),
    color = transparent(.2)
    ),
  
  fluidRow( ## Encabezado----
    column(2, ### Mini sunburst----
      sun_mini,
      align = "center",
      class = "mini-sunburst"
      ),
    column(10, ### Título----
      fluidRow(
        column(12,
          h1("Índice del Desarrollo Social de la Mujer y el Hombre en Países
             de América Latina 2022 - IDSMH")
           )
        ),
      fluidRow( ### Texto----
        column(12, p(txt_intro, class = "subtitulo"))
        ),
      fluidRow( ### Botón descarga----
        column(12, downloadButton("downloadpublication",
                                  "DESCARGA LA PUBLICACIÓN",
                                  class = "dwnld-button",
                                  style = button_style))
        )
      )
    ),
  tabsetPanel( ## T0 Tabset----
    tabPanel( ## T1 Índice y resultados generales----
      "EL IDSMH", icon = icon("life-ring"),
      fluidRow(
        style = "padding-bottom:0px; border-bottom:2px dotted #798BFE;",
        column(6, ### El IDSMH----
          style = "background-color:white; padding: 20px 30px 10px 15px;", # ; height:100vh
          fluidRow(
            column(12, h3("DIMENSIONES, FACTORES Y VARIABLES"))),
          fluidRow(
            column(12, p(txt_indice_a, style = "padding-bottom:0px"))
            ),
          fluidRow(
            column(12, style = "padding-top:0px;",
                   sun_vars, align = "center")
            ),
          fluidRow(
            # style = "margin-top: 20px;",
            column(12, p(txt_indice_b))
            ),
          fluidRow(
            column(12, p(txt_indice_c))
            )
          ),
        column(6, ### Resumen ejecutivo----
          style = "background-color:#E8EBF0; border-left:2px dotted #798BFE; padding: 20px 15px 40px 30px", #height:100vh
          fluidRow(
            column(12,
              h3("RESUMEN EJECUTIVO"))
            ),
          fluidRow(
            column(12, p(txt_articulo))
            )
          )
        ),
      fluidRow( ### Créditos----
        column(12, style = "padding-top:40px;",
          h3("CRÉDITOS"))
        ),
      fluidRow(
        column(12, style = "padding-bottom:40px;",
          class = "frow-bottom",
          p(class = "articulo", txt_creditos)
          )
        )
      ),
    tabPanel( ## T2 Territorios e indicadores----
      "EXPLORA INDICADORES Y TERRITORIOS", icon = icon("chart-bar"),
      fluidRow(
        class = "frow-bottom",
        style = "padding-bottom:0px; background-color: #E8EBF0",
        column(6, ### Mapa----
          height = 800, 
          style = "border-right:2px dotted #798BFE; padding: 20px 30px 40px 15px; background-color:white",
          fluidRow(
            column(12, h3("MAPA REGIONAL"))
          ),
          fluidRow(
            column(12, p(txt_map))
          ),
          fluidRow(
            column(12,
              selectizeInput("indicador_mapa", 
                label = "INDICADOR:",
                choices = items_ind,
                # selected = 0,
                width = 500,
                options = list(
                  render = I("
                    {
                    item: function(item, escape) { return '<div>' + item.label + '</div>'; },
                    option: function(item, escape) { return '<div>' + item.label + '</div>'; }
                  }"))
                ))
            ),
          fluidRow(
            column(12,
                  # withSpinner(
                  leafletOutput("mapa", height = 600)#, 
                   #  hide.ui = FALSE)
                   )
            )
          ),
        column(6, ### Dumbell----
          style = "padding: 20px 15px 20px 30px",
          fluidRow(
            column(12, h3("PERFIL TERRITORIAL POR FACTORES"))
            ),
          fluidRow(
            column(12,
                   p(txt_dumbell))
            ),
          fluidRow(
            column(12,
              selectizeInput("region_dumbell",
                label = "TERRITORIO:",
                choices = items_reg,
                selected = "PERÚ",
                options = list(
                  render = I("
                    {
                    item: function(item, escape) { return '<div>' + item.label + '</div>'; },
                    option: function(item, escape) { return '<div>' + item.label + '</div>'; }
                    }")
                  )
                )
              )
            ),
          fluidRow(
            column(12,
                   #withSpinner(
                   plotlyOutput("dumbell", height = 600)#, hide.ui = FALSE)
                   )
                   )
          )
        )
      )
    )
  )


# 2 Shiny Server ###################################

server <- function(input, output, session) {

  ## O Download publication----
  output$downloadpublication <- downloadHandler(
    filename = "prueba.pdf",
    content = function(file) {
      file.copy("www/prueba.pdf", file)
    }
  )
  
  
  ## O Mapa----
  output$mapa <- renderLeaflet({ 
    
    ### Inputs mapa----
    nombre <- str_trim(input$indicador_mapa)
    index <- diccionario[diccionario$nombre==nombre,]$codigo
    unid <- diccionario[diccionario$nombre==nombre,]$unidad
    unid_short <- diccionario[diccionario$nombre==nombre,]$unidad_short
    ord_inv <- diccionario[diccionario$nombre==nombre,]$orden_inverso
    
    df_indicador <- geo_datos %>% filter(nivel == "R") %>% 
      select(pais:nivel, index)
    df_indicador_h <- df_indicador %>% filter(sexo == "h") 
    df_indicador_m <- df_indicador %>% filter(sexo == "m") 
    df_indicador_b <- df_indicador %>% filter(sexo == "brecha") 
    
    
    ### Labels tooltip hombres----
    tooltip_h <- sprintf( 
      '<b>%s</b> - %s<br/>%s<br/>%s: %g %s',
      df_indicador_h$region, df_indicador_h$pais,
      nombre,
      "Hombres", round(pull(df_indicador_h,index), 1),
      unid_short)%>%
      lapply(htmltools::HTML)
    
    ### Labels tooltip mujeres----
    tooltip_m <- sprintf(
      '<b>%s</b> - %s<br/>%s<br/>%s: %g %s',
      df_indicador_m$region, df_indicador_m$pais,
      nombre,
      "Mujeres", round(pull(df_indicador_m,index), 1),
      unid_short)%>%
      lapply(htmltools::HTML)
    
    ### Labels para tooltip brecha----
    tooltip_b <- sprintf(
      '<b>%s</b> - %s<br/>%s<br/>%s:<br/>%g %s',
      df_indicador_b$region, df_indicador_b$pais,
      nombre,
      "Brecha (hombres menos mujeres)",
      round(pull(df_indicador_b,index), 1),
      ifelse(unid_short == "%", "pp", unid_short))%>%
      lapply(htmltools::HTML)
    
    ### Paletas----
    
    if (unid == "Puntaje") {
      palette_seq <- colorBin(
        palette_puntaje_import,
        bins = c(0,35,45,55,65,75,85,100),
        domain = c(0, 100))
    } else {
      palette_seq <- colorNumeric(
        palette_seq_import,
        reverse = ifelse(ord_inv == TRUE, TRUE, FALSE),
        domain = c(
          if (unid == "Variación porcentual") {
            min(
              min(pull(df_indicador_h, index), na.rm = TRUE),
              min(pull(df_indicador_m, index), na.rm = TRUE))
            } else {
              0
              },
          if (index == "sb2" | index == "oc2") {
            max(
              max(pull(df_indicador_h, index), na.rm = TRUE),
              max(pull(df_indicador_m, index), na.rm = TRUE))
            } else {
              if (unid == "Porcentaje") {
                100
                } else {
                  max(
                    max(pull(df_indicador_h, index), na.rm = TRUE),
                    max(pull(df_indicador_m, index), na.rm = TRUE))
                }
              }
          )
        )
      }

    palette_div <- colorNumeric( # paleta para brecha -sirve para poner el blanco en cero-
      # palette_div_import,
      c("#9e68ef", "#ffffff", "#1dc37c"), 
      reverse = FALSE,
      domain = c(-max(c(abs(min(pull(df_indicador_b,index), na.rm = TRUE)), 
                        abs(max(pull(df_indicador_b,index), na.rm = TRUE)))),
                 max(c(abs(min(pull(df_indicador_b,index), na.rm = TRUE)), 
                       abs(max(pull(df_indicador_b,index), na.rm = TRUE))))))
    
    ### Option labels----
    opciones_label <- labelOptions(
      className = "tooltip-mapa")
    ### Mapa----
    map <- map %>% 
      addPolygons(data = df_indicador_h,
                  color = 'gray', 
                  weight = 0.1, 
                  fillOpacity = 1, 
                  fillColor = ~palette_seq(pull(df_indicador_h, index)), 
                  smoothFactor = 0,
                  options = pathOptions(pane = "hombres"),
                  label = tooltip_h,
                  labelOptions = opciones_label,
                  highlightOptions = highlightOptions(
                    weight = 4,
                    color = 'white'),
                  group = "Hombres") %>% 
      addPolygons(data = df_indicador_m,
                  color = 'gray', 
                  weight = 0.1, 
                  fillOpacity = 1, 
                  fillColor = ~palette_seq(pull(df_indicador_m, index)), 
                  smoothFactor = 0,
                  options = pathOptions(pane = "mujeres"),
                  label = tooltip_m,
                  labelOptions = opciones_label,
                  highlightOptions = highlightOptions(
                    weight = 4,
                    color = 'white'),
                  group = "Mujeres") %>% 
      addPolygons(data = df_indicador_b,
                  color = 'gray', 
                  weight = 0.1, 
                  fillOpacity = 1, 
                  fillColor = ~palette_div(pull(df_indicador_b, index)), 
                  smoothFactor = 0,
                  options = pathOptions(pane = "brecha"),
                  label = tooltip_b,
                  labelOptions = opciones_label,
                  highlightOptions = highlightOptions(
                    weight = 4,
                    color = 'white'),
                  group = "Brecha") %>% 
      addLegend(
        "bottomleft", 
        pal = palette_seq, 
        values = 
          c(pull(df_indicador_h, index), pull(df_indicador_m, index)),
        title = nombre,
        # labFormat = labelFormat(prefix = "$"),
        opacity = 1,
        group = "Mujeres",
        className = "info legend Mujeres"
      ) %>% 
      addLegend(
         "bottomleft",
         pal = palette_seq,
         values =
           c(pull(df_indicador_h, index),pull(df_indicador_m, index)),
         title = nombre,
         # labFormat = labelFormat(prefix = "$"),
         opacity = 1,
         group = "Hombres",
         className = "info legend Hombres"
         ) %>%
      addLegend(
        "bottomleft", 
        pal = palette_div, 
        values = c(pull(df_indicador_b, index), -pull(df_indicador_b, index)),
        title = paste(nombre, "<br>(diferencia a favor de<br><span style='color: #9e68ef'>mujeres</span> o de <span style='color: #1dc37c'>hombres</span>)"),
        # labFormat = labelFormat(prefix = "$"),
        opacity = 1,
        group = "Brecha",
        className = "info legend Brecha"
        ) %>% 
      addLayersControl(
        baseGroups = c("Mujeres", "Hombres", "Brecha"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      htmlwidgets::onRender("
        function(el, x) {
           var updateLegend = function () {
              var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);
  
              document.querySelectorAll('.legend').forEach(a => a.hidden=true);
              document.querySelectorAll('.legend').forEach(l => {
                 if (l.classList.contains(selectedGroup)) l.hidden=false;
              });
           };
           updateLegend();
           this.on('baselayerchange', el => updateLegend());
        }"
      )
    
    map
    
  })
  

  ## O Dumbell----
  output$dumbell <- renderPlotly({ 
    # reg <- input$region_dumbell
    #reg <- input$subregion_dumbell
    #print(region)
    nomb_reg <- input$region_dumbell
    reg <- datos[datos$region_input==nomb_reg,]$iso_3166_2

    recode_var <- setNames(diccionario$nombre, diccionario$codigo)
    
    datos_barras <- datos %>% 
      ungroup() %>% 
      select(pais:nivel) %>% 
      bind_cols(ungroup(datos)[,nchar(colnames(datos)) == 2]) %>% 
      filter(
        sexo %in% c("h", "m"),
        iso_3166_2 == reg) %>% 
      pivot_longer(ea:oc, names_to = "var", values_to = "valor") %>% 
      pivot_wider(
        names_from = sexo,
        values_from = valor) %>% 
      mutate(
        nombre_var = recode(var, !!!recode_var)#,
        # nombre_dim = case_when( # Esto funciona localmente pero no en deployment
        #   substr(datos_barras$var, 1, 1) == "e" ~ "EDUCACIÓN",
        #   substr(datos_barras$var, 1, 1) == "s" ~ "SALUD",
        #   substr(datos_barras$var, 1, 1) == "a" ~ "AUTONOMÍA",
        #   TRUE ~ "OPORTUNIDAD"
          # )
    )
    

    fig <- plot_ly(datos_barras, color = I("gray80"))
    fig <- fig %>% 
      add_segments(
        x = ~m, xend = ~h, y = ~nombre_var, yend = ~nombre_var,
        showlegend = FALSE, color = I("gray80")
        )
    fig <- fig %>% 
      add_markers(
        x = ~m, y = ~nombre_var,
        showlegend = TRUE,
        name = "Mujeres", 
        color = I(col_mujeres), 
        marker = list(
          symbol = 'diamond',
          size = 10),
        hovertemplate = paste0(
          "<span style='color:white'>Dimensión: ", datos_barras$nombre_dim, "<br>",
          "Factor: ", datos_barras$nombre_var, "<br>",
          "Puntaje: <b>%{x:.1f}</b><br></span>",
          "<extra><b>MUJERES</b></extra>"),
        hoverlabel = list(
          bordercolor = "white")
        )
    fig <- fig %>% 
      add_markers(
        x = ~h, y = ~nombre_var, 
        showlegend = TRUE,
        name = "Hombres", 
        color = I(col_hombres), 
        marker = list(symbol = 'diamond',
        size = 10),
        hovertemplate = paste0(
          "<span style='color:white'>Dimensión: ", datos_barras$nombre_dim, "<br>",
          "Factor: ", datos_barras$nombre_var, "<br>",
          "Puntaje: <b>%{x:.1f}</b><br></span>",
          "<extra><b>HOMBRES</b></extra>"),
        hoverlabel = list(
          bordercolor = "white")
        )
    fig <- fig %>% layout(
      title = list( 
        text = ifelse(datos_barras[datos_barras$iso_3166_2==reg,]$nivel == "R",
                      paste0(datos_barras[datos_barras$iso_3166_2==reg,]$region, ", ", 
                             datos_barras[datos_barras$iso_3166_2==reg,]$pais),
                      datos_barras[datos_barras$iso_3166_2==reg,]$region),
        font = pl_title,
        x = 0.5, y = 0.99, yanchor = "top", 
        xref = "paper", yref = "container"), # paste(region, pais, sep = ", ")
      legend = list(font = pl_legend,
                    orientation = "h",
                    x = 0.5, xanchor = "center",
                    y = 1.1, yanchor = "top"),
      paper_bgcolor = cool_neutral,
      plot_bgcolor = cool_neutral,
      font = pl_font,
      xaxis = list(title = list(text = "Puntaje", # "Puntaje", 
                                font = pl_axis_title),
                   range = list(-5, 110)),
      yaxis = list(title = list(text = "", # "Factor", 
                                standoff = 5,
                                font = pl_axis_title),
                   categoryorder = "array",
                   categoryarray = rev(dput(datos_barras$nombre_var)),
                   tickmode = "array",
                   tickvals = dput(datos_barras$nombre_var),
                   ticktext = c(
                     "<span style='color:#524aa6'>PRIMARIA</span>",
                     "<span style='color:#524aa6'>SECUNDARIA</span>",
                     "<span style='color:#524aa6'>LOGRO EDUCATIVO</span>",
                     "<span style='color:#524aa6'>ACCESO</span>",
                     "<span style='color:#008df2'>MORBILIDAD</span>",
                     "<span style='color:#008df2'>CUIDADOS BÁSICOS</span>",
                     "<span style='color:#ef3d01'>ECONÓMICA</span>",
                     "<span style='color:#ef3d01'>FÍSICA</span>",
                     "<span style='color:#ef3d01'>TOMA DE DECISIONES</span>",
                     "<span style='color:#00bcb0'>ACCESO A EDUC. SUPERIOR</span>",
                     "<span style='color:#00bcb0'>EMPLEO</span>",
                     "<span style='color:#00bcb0'>GESTIÓN Y TIEMPO</span>"
                   )
                   ),
      margin = list( l = 40, r = 40, b = 0, t = 80, pad = 4
      )
      ) %>% 
      config(modeBarButtonsToRemove = c('hoverClosestGl2d','select', 'autoScale', 
                                        'hoverCompareCartesian', 'pan', 'lasso2d',
                                        'hoverClosestCartesian', 'zoom', 'zoomIn', 
                                        'zoomOut'),
             displaylogo = FALSE)
    
    fig
    
  })

  

  
  ## List of dynamic regions----
  region_choices <- reactive({
    
    tmp <- datos[(datos$pais==input$region_dumbell),]
    tmp <- na.omit(tmp)
      
    choice <- unique(tmp$region)
    
    choice
  })
  
  ## Dynamic selector
  #observe({
  #  updateSelectInput(session = session, inputId = "subregion_dumbell", choices = region_choices())
  #  
  #})
  
  #outputOptions(output, {"mapa"}, suspendWhenHidden = FALSE)
  #outputOptions(output, "dumbell", suspendWhenHidden = FALSE)
  #outputOptions(output, "tabla_pais", suspendWhenHidden = FALSE)
  
}

# Run the application #########

shinyApp(ui = ui, server = server)

