# COLORES----
## Colores----
central_bold <- "#363661"
central_blue <- "#3B69FA"
central_blue_70 <- "#798BFE"
cool_neutral <- "#E8EBF0"
cool_neutral_40 <- "#F6F7F9"
col_hombres <- "#1dc37c"
col_mujeres <- "#9e68ef"
col_pos <- "#4599c0"
col_neg <- "#e93587"
colores <- c("#04cffb", "#27a7a0", "#fabe00", "#971510",
             "#008df2", "#ff791a", "#56cd47", "#e40269")
col_d3 <- "#524aa6" #educ
col_d4 <- "#008DF2"
col_d1 <- "#ef3d01"
col_d2 <- "#00bcb0"

col_texto_kable <- "Black"

# tmp
load("data/datos.RData") # cargar datos
load("data/geo_datos.RData")
load("data/diccionario.RData") # cargar diccionario
# load("data/intro_table.RData") # cargar intro_table
load("data/data_sun.RData") # cargar data para sunburst
load("data/libro.RData") # cargar intro_table


#Styling de botones
button_style <- 
  "color:#3B69FA;border-color:#3B69FA;border-width:2px;border-radius:10px;"

# FONTS de plots----
pl_font <- list(
  family = "'iAWriter', 'IBM Plex Mono', 'PT Mono', 'Andale Mono', Courier",
  color = central_bold)
pl_title <- list(
  # family = "Calibre, Helvetica, Arial, Sans-Serif",
  color = central_blue
  # size = 22
  )
# pl_axis_label <- list(
#   #family = "iA Writer Duospace, Courier",
#   color = central_bold)
pl_axis_title <- list(
  #family = "iA Writer Duospace, Courier",
  color = central_bold)
pl_legend <- list(
  family = "Calibre, Helvetica, Arial, Sans-Serif",
  color = central_bold)
pl_yaxis_scatter_ord <- list(
  family = "'iAWriter', 'IBM Plex Mono', 'PT Mono', 'Andale Mono', Courier",
  color = central_bold,
  size = 10)
pl_sunburst <- list(
  family = "Calibre, Arial, Sans-Serif",
  size = 20)

# FUNCIONES:----

## Download table button---
csvDownloadButton <- function(id, filename = "data.csv", label = "DESCARGA LA TABLA") {
  tags$button(
    tagList(icon("download"),
            label),
    style = button_style,
    class = "btn btn-default shiny-download-link dwnld-button shiny-bound-output",
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", 
                      id, 
                      filename)
    )
  }

## Tooltips en header de reactables----
with_tooltip <- function(value, tooltip) {
  tags$abbr(style = "text-decoration: underline; text-decoration-style: solid; cursor: help",
            title = tooltip, value)
}

## Render a bar chart in the background of the cell----
bar_style <- function(width = 1, fill = col_hombres, height = "75%", 
                      color = "black") {
  position <- paste0(width * 100, "%")
  image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, 
                   position)
  list(
    backgroundImage = image,
    backgroundSize = paste("100%", height),
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center",
    marginLeft = "0.75rem",
    marginRight = "0.25rem",
    textAlign = "Left",
    fontSize = "1.3rem",  # added for checking values of small bar in cells
    # backgroundColor = "#00bfc4", # toda la celda
    color = color
  )
}

bar_style3 <- function(width = 1, fill = col_hombres, height = "75%", 
                      color = "white", dftmp) {
  
  val <- width/max(max(pull(data_pais[,c(paste0(indexes[1],"_h"))])),max(pull(data_pais[,c(paste0(indexes[1],"_m"))])))
  position <- paste0(width * 100, "%")
  image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, 
                   position)
  list(
    backgroundImage = image,
    backgroundSize = paste("100%", height),
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center",
    marginLeft = "0.75rem",
    marginRight = "0.25rem",
    textAlign = "Left",
    # backgroundColor = "#00bfc4", # toda la celda
    color = color
  )
}


## Render a bar chart with a label on the left----
bar_chart <- function(label, width = "100%", height = "2rem", fill = "#82758F", 
                      background = "#e1e1e1") {
  bar <- div(
    style = list(
      background = fill, 
      width = width, 
      height = height))
  chart <- div(
    style = list(
      flexGrow = 1, 
      marginLeft = "0rem", 
      background = background), 
    bar)
  div(style = list(display = "flex", 
                   alignItems = "center"), 
      format(round(label, 1), nsmall = 1), 
      chart)
}

## Render a bar chart with positive and negative values----
bar_chart_pos_neg <- function(label, value, max_value = 1, height = "1rem",
                              pos_fill = col_pos, neg_fill = col_neg) {
  neg_chart <- div(style = list(flex = "1 1 0"))
  pos_chart <- div(style = list(flex = "1 1 0"))
  width <- paste0(abs(value) * 100, "%") # definir
  
  if (value < 0) {
    bar <- div(
      style = list(marginLeft = "0.5rem", background = neg_fill,
                   width = width, height = height))
    chart <- div(
      style = list(display = "flex", alignItems = "center", 
                   justifyContent = "flex-end"),
      label,
      bar
    )
    neg_chart <- tagAppendChild(neg_chart, chart)
  } else {
    bar <- div(
      style = list(marginRight = "0.5rem", background = pos_fill,
                   width = width, height = height))
    chart <- div(
      style = list(display = "flex", alignItems = "center"),
      bar,
      label)
    pos_chart <- tagAppendChild(pos_chart, chart)
  }
  
  div(style = list(display = "flex"), neg_chart, pos_chart)
}

## Palette for intro_kable y mapa----
pal_kable <- # Paleta intro kable secuencial
  paletteer_c("ggthemes::Blue-Green Sequential", 100)

palette_seq_import <- 
  ggthemes::tableau_div_gradient_pal(palette = "Red-Green-Gold Diverging")

palette_puntaje_import <- 
  ggthemes::tableau_div_gradient_pal(palette = "Red-Green-Gold Diverging")

palette_div_import <- "PRGn"
#  ggthemes::tableau_div_gradient_pal(palette = "Classic Orange-White-Blue")

spec_color2 <- function(x, alpha = 1, begin = 0, end = 1,
                        direction = 1, option = "D",
                        na_color = "#BBBBBB", scale_from = NULL,
                        palette = viridisLite::viridis(256, alpha, begin, end, 
                                                       direction, option)) {
  n <- length(palette)
  if (is.null(scale_from)) {
    x <- round(scales::rescale(x, c(1, n)))
  } else {
    x <- round(scales::rescale(x, to = c(1, n),
                               from = scale_from))
  }
  
  color_code <- palette[x]
  color_code[is.na(color_code)] <- na_color
  return(color_code)
}

# OTROS:----
## Custom table container for DT intro_table ----
# sketch <- htmltools::withTags(table(
#   class = 'display',
#   thead(
#     tr(
#       th(rowspan = 2, 'IDSR'),
#       th(rowspan = 2, 'Dimensi??n'),
#       th(rowspan = 2, 'Factores'),
#       th(colspan = 2, 'PER??'),
#       th(colspan = 2, 'ARGENTINA'),
#       th(colspan = 2, 'BOLIVIA'),
#       th(colspan = 2, 'BRASIL'),
#       th(colspan = 2, 'CHILE'),
#       th(colspan = 2, 'COLOMBIA'),
#       th(colspan = 2, 'ECUADOR'), 
#       th(colspan = 2, 'M??XICO')),
#     tr(
#       lapply(rep(c('M', 'H'), 8), th)
#     )
#   )
# ))

## N de territorios subnacionales----
n_subnacionales <- unique(datos$iso_3166_2[datos$nivel== "R"])%>% length() 


# TEXTOS----
## Intro----
txt_intro <- HTML("El ??ndice del Desarrollo Social de la Mujer y el Hombre en Pa??ses de Am??rica Latina 2022 (IDSMH) mide la situaci??n actual del acceso a recursos por parte de mujeres y hombres. El ??ndice abarca 191 territorios subnacionales de 8 pa??ses de Am??rica Latina. Esta presentaci??n permite comparar los indicadores en unidades geogr??ficas subnacionales, y permite tambi??n identificar las brechas en el desarrollo entre hombres y mujeres.")

## El ??ndice a----
txt_indice_a <- HTML("<p class = 'spaced'>EL IDSMH recopila indicadores de desarrollo social desagregados por sexo y por unidades territoriales subnacionales en ocho pa??ses de Latinoam??rica. Los indicadores permiten ver el nivel de desarrollo y las brechas entre hombres y mujeres para cuatro dimensiones del desarrollo social, compuestas por tres factores en cada una de ellas. Haz click en las dimensiones para explorar los factores y variables que las componen.</p>")

## El ??ndice b----
txt_indice_b <- HTML("El </b>IDSMH</b> resume la informaci??n del conjunto de indicadores. Como tal, es una suma ponderada de sus cuatro dimensiones. Cada dimensi??n incluye los siguientes factores:  
  <ul>
  <li><span style='color: #524aa6;'>Educaci??n</span>: Primaria, Secundaria, Logro educativo</li>
  <li><span style='color: #008DF2;'>Salud</span>: Acceso, Morbilidad, Cuidados b??sicos</li>
  <li><span style='color: #ef3d01;'>Autonom??a</span>: Econ??mica, F??sica, Toma de decisiones</li>
  <li><span style='color: #00bcb0;'>Oportunidades</span>: Acceso a educaci??n superior y t??cnica, Empleo, Gesti??n y tiempo</li>
</ul>")
txt_indice_c <- HTML("El ??ndice, las dimensiones y los factores son medidos a partir de un puntaje que va de 0 a 100, donde 100 representa el mayor nivel de desarrollo. Por su parte, cada variable tiene su propio indicador o unidad de medida, visible al posicionar el cursor sobre el nombre de cada variable del gr??fico. No obstante, cada pa??s realiza la medici??n seg??n su propio criterio. Estos criterios, as?? como el a??o correspondiente al levantamiento de la informaci??n en cada pa??s se puede encontrar en la secci??n de metodolog??a.")

## Resumen ejecutivo----
txt_articulo <- HTML("<p class = 'spaced'>El estudio revela que la b??squeda de la igualdad entre mujeres y hombres, a??n debe sostenerse con firmeza en Am??rica Latina ya que existe una brecha favorable a los hombres sobre las mujeres en todos los pa??ses. S??lo una regi??n de un total de 191 regiones o estados evaluados (Michoacan de Ocampo en M??xico) logr?? una brecha favorable a las mujeres (0.8%). La brecha promedio en Am??rica Latina es de 7% en favor de los hombres y llega a ampliarse por encima del 10% en Per??.</p>
  
<p class = 'spaced'>Por otro lado, la ventaja promedio en el ??ndice de desarrollo social entre hombres y mujeres se incrementa si consideramos las diferencias al interior de las regiones o estados que componen estos pa??ses, registr??ndose una ventaja promedio en Am??rica Latina de 14,3% de hombres sobre mujeres, llegando a superar el 20% en pa??ses como Ecuador y Per??.</p>

<p class = 'spaced'>Los resultados evidencian que en salud y educaci??n existen menores brechas y ventajas de hombres sobre mujeres; incluso existe un alto porcentaje de regiones donde la brecha es favorable a las mujeres. En Salud, la brecha es favorable a las mujeres en 148 regiones de un total de 191; mientras que en Educaci??n la brecha es favorable a las mujeres en 107 regiones. Los resultados son totalmente contrapuestos en las dimensiones autonom??a y oportunidades. A nivel de autonom??a la brecha es absolutamente favorable a los hombres en todas las regiones; mientras que en la dimensi??n oportunidades la brecha es favorable a los hombres en 161 regiones.</p>  

<p class = 'spaced'>Los IDS m??s altos en mujeres los registran Argentina con 59,8 puntos, seguido de Brasil con 58,8 puntos y Colombia con 55,7 puntos; mientras que los resultados m??s bajos los registran Bolivia y Per??, con 42,3 y 40,2 puntos, respectivamente.</p>

<p class = 'spaced'>Los IDS m??s altos en hombres los registran tambi??n Argentina con 64,7 puntos, seguido de Brasil con 64,5 puntos y Colombia con 61,7 puntos; mientras que los resultados m??s bajos lo registran Bolivia y M??xico, con 51 y 50,9 puntos, respectivamente.</p>

<p class = 'spaced'>A modo de conclusi??n, se puede inferir que se requieren equilibrar las pol??ticas para reducir las diferencias de g??nero. En salud y educaci??n, las pol??ticas han permitido incluso una ventaja de las mujeres sobre los hombres. Sin embargo, se debe mantener un equilibrio, de modo tal que no se generen mayores desigualdades o inclusive un deterioro de los IDS de los hombres. Por otro lado, las pol??ticas deben ser m??s agresivas para reducir las elevadas brechas favorables a los hombres en autonom??a y oportunidades. Sin lugar a dudas, las diferencias son preocupantes sobre todo en autonom??a econ??mica (por las diferencias en el ingreso, trabajo no remunerado y la dependencia econ??mica), y la toma de decisiones; as?? como tambi??n, en cuanto a variables como el empleo (empleo adecuado, y subempleo) y la gesti??n del tiempo, (tiempo de trabajo remunerado y el g??nero del empleador).")

txt_kable <- "El IDSMH, sus dimensiones y sus factores son indicadores cuyos puntajes representan niveles de desarrollo en un rango que va desde 0 hasta 100."

txt_kable2 <- "La tabla siguiente presenta los resultados generales, agregados para cada uno de los ocho pa??ses incluidos en el estudio y divididos entre hombres y mujeres. Las variables que componen cada uno de los factores aparecen al pasar el cursor sobre cada uno de ellos."

## Mapa----
txt_map <- "El IDSMH y todos los indicadores se han calculado para los niveles administrativos subnacionales (departamentos, estados, provincias o regiones) de cada pa??s, seg??n su organizaci??n territorial. Elige el indicador que quieres mostrar y explora su distribuci??n en los distintos territorios."

## Scatter ordenado----
txt_scatter_ord <- "Este gr??fico muestra 191 territorios subnacionales incluidos en el estudio ordenados de mayor a menor valor del ??ndice para el sexo seleccionado. El punto se??ala el valor para el sexo elegido, y la longitud de la l??nea indica la magnitud de la brecha respecto al sexo opuesto. El color de los puntos representa el pa??s al que pertenece cada territorio."

## Dumbell----
txt_dumbell <- HTML("Cada territorio tiene distintos niveles de desarrollo social dependiendo de los factores analizados. <br>
                    Selecciona un territorio para ver el puntaje de hombres y de mujeres en cada factor. La distancia horizonal entre ellos muestra el tama??o de las brechas de g??nero.")

## Scatter h, m y brecha----
txt_scatters <- "Algunos indicadores guardan cierta asociaci??n con otros indicadores. A continuaci??n, el gr??fico muestra la naturaleza de esta asociaci??n para los dos indicadores seleccionados. El panel de la izquierda muestra la asociaci??n entre indicadores para el caso de los hombres, el panel central para las mujeres, y el de la derecha para la brecha. El color de cada punto representa el pa??s al que pertenece cada territorio."

txt_tabla_pais <- "Selecciona hasta cinco indicadores para producir una tabla que muestre los valores de hombres y mujeres para cada uno de los pa??ses del estudio."

txt_tabla_region <- "Selecciona hasta cinco indicadores para producir una tabla que muestre los valores de hombres y mujeres para cada uno de los territorios incluidos del estudio."

txt_creditos <- HTML("<span style='font-weight:bold;color:#3B69FA'>EL ??NDICE DEL DESARROLLO SOCIAL DE LA MUJER Y EL HOMBRE EN LOS PA??SES DE AM??RICA LATINA 2022</span><br>
Una publicaci??n de CENTRUM PUCP - Escuela de Negocios de la Pontificia Universidad Cat??lica del Per??")

## Metodolog??a----


m_q_es <- HTML("El ??ndice del Desarrollo Social de la Mujer y el Hombre (IDSMH) es una herramienta que permite identificar, desde una visi??n amplia, la realidad de la brecha de g??nero de las regiones (estados, departamentos y provincias) en los principales pa??ses de Am??rica Latina.
<br>  
Es decir, presentar un an??lisis de la situaci??n actual del acceso a los recursos por parte de mujeres y hombres en las dimensiones de Educaci??n, Salud, Autonom??a y Oportunidades.") 

m_q_mide <- HTML("El IDSMH mide la situaci??n de mujeres y hombres con respecto a su desarrollo en las cuatro dimensiones evaluadas por medio de un grupo de indicadores. Adem??s, este ??ndice ofrece un mapa con todos los resultados indicando las diferencias entre mujeres y hombres a partir del real acceso a los recursos disponibles en cada pa??s y regi??n de Am??rica Latina. 
<br>  
El IDSMH toma en cuenta las particularidades dentro de cada pa??s a partir de la evaluaci??n de cuatro dimensiones: Educaci??n, Salud, Autonom??a y Oportunidades. El ??ndice representa una herramienta para medir la situaci??n de mujeres y hombres a trav??s de un enfoque hol??stico e integrador.")

m_caracteristicas <- HTML("
  <ul>
    <li>El ??ndice solo considera indicadores de resultados, no de procesos o gesti??n.</li>
    <li>Incluye una visi??n para entender las diferencias entre mujeres y hombres en 191 regiones.</li>
    <li>Se utilizan bases de datos de fuentes p??blicas y oficiales, que incluyen encuestas nacionales de hogares, estad??sticas de bancos centrales, institutos de estad??sticas, informes, investigaciones, entre otras fuentes.</li>
    </ul>")

m_modelo <- HTML("<ul>
  <li>El IDSMH eval??a cuatro dimensiones: Educaci??n, Salud, Autonom??a y Oportunidades.</li>
  <li>Cada dimensi??n comprende tres componentes. Es decir, en total existen 12 componentes.</li>
  <li>Cada componente se determina a partir de un n??mero determinado de variables. En total se consideran 28 variables para las mujeres y 28 variables para los hombres.</li>
</ul>")

m_educacion <-HTML("La Dimensi??n Educaci??n cubre los diferentes aspectos del alcance del nivel educativo. Se mide el acceso a la educaci??n que tienen las mujeres y los hombres en los niveles de primaria y secundaria; adem??s, se considera los logros educativos alcanzados.")

m_salud <-HTML("La Dimensi??n Salud mide la capacidad que tienen las mujeres y los hombres de acceder a un servicio de salud y qu?? tan favorables son las condiciones para facilitar tal acceso. Asimismo, calcula el nivel de morbilidad y el acceso a los servicios de cuidados b??sicos diferenciado entre ambos sexos.")

m_autonomia <-HTML("La Dimensi??n Autonom??a mide qu?? tan aut??nomos son las mujeres y los hombres econ??micamente, f??sicamente y al momento de tomar decisiones. Estos tres factores son importantes para poder medir la acci??n de la persona como individuo y como parte de la sociedad.")

m_oportunidades <-HTML("La Dimensi??n Oportunidades mide componentes relacionados con el acceso a la educaci??n superior, el nivel de empleo adecuado y no adecuado, y la gesti??n y el tiempo destinado al trabajo.")

m_consideraciones <-HTML("Los indicadores de desarrollo por g??nero se obtuvieron en los 26 departamentos de Per?? (incluyendo Callao, Lima Metropolitana y Lima provincia), 16 regiones de Chile, 33 departamentos de Colombia, 24 provincias de Ecuador, 9 departamentos de Bolivia, 27 estados de Brasil, 24 provincias de Argentina y 32 estados de M??xico.
<br>  
Si bien los 28 indicadores son los mismos para los 8 pa??ses, su c??lculo requiri?? de una adaptaci??n a cada realidad, con el objetivo de hacerlos comparables entre pa??ses. La denominaci??n de los indicadores corresponde al caso espec??fico peruano, pero la precisi??n t??cnica en su construcci??n depende de la forma de c??lculo de cada pa??s.
<br>  
Para poner un ejemplo, en el Per?? los indicadores del componente Educaci??n en los que se mide la tasa neta de matr??cula y la asistencia a la educaci??n primaria de ni??os, tiene como par??metro rango las edades que van entre los 6 y los 11 a??os. Mientras que en Chile el rango de edad de los ni??os que asisten a educaci??n primaria se enmarca entre los 6 y los 13 a??os, y en Colombia entre los 6 y los 10 a??os.
<br>  
Para estos casos, se adaptaron los indicadores a la realidad de cada pa??s, en el ejemplo para Chile y Colombia se aplicaron las tasas de matr??cula y asistencia de primaria a los rangos de edad que corresponden a dichos pa??ses.   
<br>  
Dada la heterogeneidad de fuentes de informaci??n entre pa??ses, adem??s de las diferentes formas de c??lculo de los indicadores, para algunos indicadores fue imposible conseguir informaci??n y se tuvo que estimar los datos de la informaci??n incompleta.
<br>  
La forma en que se estim?? el indicador incompleto del pa??s (denominados ???indicador del pa??s objetivo???) fue tomando como referencia a los pa??ses que si contaban con informaci??n completa (denominados ???indicadores de pa??ses de referencia???), requiriendo para ello un factor de ponderaci??n que incluyera el concepto de brechas de g??nero.
<br>  
A continuaci??n, se detallan los pasos realizados para la estimaci??n del indicador del pa??s objetivo:")

m_calculo1 <- HTML("Los insumos utilizados para la construcci??n del factor de ponderaci??n de los pa??ses de referencia fueron los ??ndices Globales de Brecha de G??nero (IGBG) publicados por el Foro Econ??mico Mundial, los cuales miden la diferencia de g??nero en las dimensiones salud, econom??a, educaci??n y pol??tica para 78 pa??ses del mundo.
<br>  
Los ??ndices oscilan entre el 0 y 1, representando el ???0??? la m??xima desigualdad de g??nero y ???1??? perfecta igualdad entre hombres y mujeres. En el cuadro siguiente se muestran los indicadores por dimensi??n, seg??n pa??s:")

m_calculo2 <- HTML("Los IGBG utilizados para la ponderaci??n de los pa??ses de referencia dependieron del indicador incompleto estimado. Por ejemplo, si el indicador a estimar ten??a que ver con Educaci??n, el factor utilizado fue el IGBG de la dimensi??n educaci??n, mientras que, si el indicador a estimar era el de mujeres y hombres no remunerados, el factor utilizado fue el IGBG de la dimensi??n econom??a. A continuaci??n, se presenta la tabla de conversiones utilizadas entre los indicadores del IDSMHRP y el IGBG.")

m_calculo3 <- HTML("Para obtener las ponderaciones de los pa??ses de referencia fueron calculadas las desviaciones relativas del Indicador Global de Brecha de G??nero (IGBG) de los pa??ses de referencia respecto al indicador del pa??s objetivo.
<br>  
La desviaci??n del IGBG del pa??s de referencia resulta del cociente entre la resta del IGBG del pa??s objetivo (IPa??so) y el pa??s de referencia (IPa??sRi), y el IGBG del pa??s de referencia (IPa??sRi). Como se puede apreciar en la siguiente f??rmula:")

m_calculo4 <- HTML("A los pa??ses de referencia que registraron una menor desviaci??n se les asign?? una mayor ponderaci??n debido a su semejanza en cuanto a brechas de g??nero; mientras que aquellos pa??ses que registraron una mayor desviaci??n se les asign?? una menor ponderaci??n.
<br>  
A partir de los resultados anteriores se aplicaron las siguientes reglas de decisi??n:
<br>  
Reglas de decisi??n:
<ul>
  <li>	Si Desviaci??n Pa??s Ri = [0 ??? 5%]      ->  60% de peso</li>
  <li>	Si Desviaci??n Pa??s Ri = [6% ??? 15%] ->  30% de peso</li>
  <li>	Si Desviaci??n Pa??s Ri = [16% a m??s -> 10% de peso</li>
</ul>
Si la desviaci??n del pa??s de referencia se ubicaba entre 0 y 5% (en t??rminos absolutos) se les asign?? una ponderaci??n de 60%, mientras si la desviaci??n oscilaba entre 6 y 15% se aplic?? un peso de 30%. Finalmente, si la desviaci??n superaba el 15% se le imput?? una ponderaci??n de 10%.
<br>  
Finalmente, se calibro la suma de las ponderaciones de los pa??ses de referencia para que lleguen a 100%.")

m_aplicacion1 <- HTML("Habiendo obtenido las ponderaciones de los pa??ses de referencia, se pas?? a multiplicarlas por los Indicadores Regionales de Desarrollo Social de La Mujer y Hombre de dichos pa??ses para la consecuci??n del indicador del pa??s objetivo. Sin embargo, previo a ello se realiz?? una estandarizaci??n o ajuste de los datos de los pa??ses de referencia para obtener un indicador objetivo m??s consistente. 
<br>  
Dado que se contaba con indicadores a nivel nacional tanto de los pa??ses objetivo como de los pa??ses de referencia, se usaron ratios hombre - mujer para ajustar las cifras de los pa??ses de referencia a la realidad de cada pa??s objetivo. A continuaci??n, se detallan los pasos:
  <ul>
    <li>Paso 1: Se obtuvo un ratio hombre ??? mujer a nivel nacional de los pa??ses de referencia y objetivo.</li>
    <li>Paso 2: Se multiplic?? el ratio hombre ??? mujer a nivel nacional de cada pa??s de referencia por un factor diferenciado de tal forma que el nuevo ratio sea similar al del pa??s objetivo.</li>
    <li>Paso 3: Se aplic?? dicho ratio a los indicadores de hombres a nivel regional de cada pa??s de referencia, generando as?? indicadores por g??nero que se ajustaron a la realidad del pa??s objetivo.</li>
</ul>
")

m_aplicacion2 <- HTML("As??, se obtuvieron indicadores por g??nero en cada una de las regiones del pa??s objetivo, manteniendo sus indicadores a nivel nacional originales.")

met_tabla1 <- tribble(
  ~Dimensi??n,  ~Chile,  ~Colombia, ~Brasil,  ~Argentina, ~M??xico, ~Ecuador, ~Bolivia,
  "Econom??a",  "0,610", "0,702",   "0,665",  "0,639",    "0,590", "0,675",  "0,595", 
  "Salud",     "0,970", "0,975",    "0,980", "0,977",    "0,975", "0,968",  "0,962", 
  "Educaci??n", "1,000", "1,000",    "1,000", "1,000",    "0,997", "0,997",  "0,981", 
  "Pol??tica",  "0,283", "0,216",    "0,138", "0,390",    "0,468", "0,318",  "0,352" )

met_tabla2 <- tribble(
  ~"DIMENSI??N DEL INDICADOR",	~"C??DIGO DEL INDICADOR", ~"INDICADORES REGIONALES DE DESARROLLO SOCIAL DE LA MUJER Y HOMBRE", 	~"INDICADOR IGBG",
  "EDUCACI??N", "EA1", "Tasa neta de matr??cula escolar de ni??as y ni??os de 6 a 11 a??os de edad a educaci??n primaria", "EDUCACI??N",
  "EDUCACI??N", "EA2", "Tasa neta de asistencia a educaci??n primaria de ni??as y ni??os de 6 a 11 a??os de edad", "EDUCACI??N",
  "EDUCACI??N", "EB1", "Tasa neta de matr??cula a educaci??n secundaria de las y los adolescentes de 12 a 16 a??os de edad", "EDUCACI??N",
  "EDUCACI??N", "EB2", "Tasa neta de asistencia a educaci??n secundaria de las y los adolescentes de 12 a 16 a??os de edad", "EDUCACI??N",
  "EDUCACI??N", "EC1", "Promedio de a??os de estudio alcanzado por mujeres y hombres de 15 y m??s a??os de edad, (A??os: Inicial,  primaria y secundaria)", "EDUCACI??N",
  "EDUCACI??N", "EC2", "Tasa de alfabetizaci??n de mujeres y hombres de 15 y m??s a??os de edad, seg??n sexo", "EDUCACI??N",
  "SALUD", "SA1", "Porcentaje de la poblaci??n de Mujeres y hombres que cuentan con alg??n seguro de salud (No SIS)", "SALUD",
  "SALUD", "SA2", "Variaci??n anual de registros de Mujeres y hombres que tienen DNI ", "SALUD",
  "SALUD", "SB1.1", "Disponibilidad de camas para hombres y mujeres ", "SALUD",
  "SALUD", "SB2.1", "Porcentaje de mujeres y hombres que padecen de c??ncer o diabetes ", "SALUD",
  "SALUD", "SC1", "Casos de mujeres y hombres que reciben consulta m??dica por cada 10 mil habitantes", "SALUD",
  "AUTONOM??A", "AA1", "Ingreso promedio mensual del trabajo de hombres y mujeres", "ECONOMIA",
  "AUTONOM??A", "AA2", "Mujeres y hombres ocupados no remunerados (Trabajador independiente o trabajador familiar)", "ECONOMIA",
  "AUTONOM??A", "AA3", "Mujeres y hombres sin ingresos propios", "ECONOMIA",
  "AUTONOM??A", "AB1", "Delito de trata de Hombres y Mujeres", "POL??TICA",
  "AUTONOM??A", "AB2", "Poblaci??n de Mujeres y Hombres menores de 18 a??os de edad que sufri?? violaci??n sexual", "POL??TICA",
  "AUTONOM??A", "AB3", "Poblaci??n de Mujeres y Hombres de 18 y m??s a??os de edad que sufri?? violaci??n sexual", "POL??TICA",
  "AUTONOM??A", "AB4", "Violencia familiar y/o sexual de Mujeres y Hombres ", "POL??TICA",
  "AUTONOM??A", "AC1", "Alcaldes y Alcaldesas", "POL??TICA",
  "AUTONOM??A", "AC2", "Mujeres y hombres parlamentarios", "POL??TICA",
  "OPORTUNIDAD", "OA1", "Matriculados en Centros de Formaci??n T??cnico Productivo ", "EDUCACI??N",
  "OPORTUNIDAD", "OA2", "Matriculados en Centros de Formaci??n Tecnol??gica ", "EDUCACI??N",
  "OPORTUNIDAD", "OA3", "Tasa de asistencia de educaci??n superior universitaria", "EDUCACI??N",
  "OPORTUNIDAD", "OB1", "Empleo Adecuado", "ECONOM??A",
  "OPORTUNIDAD", "OB2", "Tasa de informalidad de mujeres y hombres, ", "ECONOM??A",
  "OPORTUNIDAD", "OB3", "Mujeres y hombres subempleados", "ECONOM??A",
  "OPORTUNIDAD", "OC1.3", "Horas trabajadas de lunes a domingo en mujeres y hombres de 15 a??os a m??s", "ECONOM??A",
  "OPORTUNIDAD", "OC2", "Proporci??n de empleadores de un negocio o empresa registrado en SUNAT", "ECONOM??A"
 )

