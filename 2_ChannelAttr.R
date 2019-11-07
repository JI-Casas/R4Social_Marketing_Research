##############################################################################
#####     XI JORNADAS DE USUARIOS DE R. MADRID, 14-16 NOVIEMBRE 2019     #####
#####     TALLER HERRAMIENTAS DE R PARA LA INVESTIGACIÓN DE MERCADOS     #####
#####            2. CHANNEL ATTRIBUTION (CANAL DE ATRIBUCIÓN)            #####
#####                      POR José Ignacio Casas                        #####
##############################################################################
# 0. INSTALACIÒN DE PAQUETES
# 1. EJEMPLO PRIMERO
# 1.1. CREAMOS EL EJEMPLO
# 1.2. CREACIÓN DEL MODELO DE MARKOV
# 1.3. RESULTADOS DE LA ATRIBUCIÓN
# 1.4. TRAZADO DEL DIAGRAMA DE MARKOV
# 1.4.1. AÑADIMOS CASOS PARA TRAZAR EL DIAGRAMA
# 1.4.2. ORDENAMOS LOS CANALES
# 1.4.3. CREAMOS EL OBJETO markovchain
# 1.4.4. TRAZAMOS EL DIAGRAMA
# 2. EJEMPLO SEGUNDO
# 2.1. CARGAMOS EL dataset "Data" INCLUIDO EN EL PAQUETE {ChannelAttribution}
# 2.2. CALCULAMOS LOS MODELOS HEURÍSTICOS Y DE MARKOV
# 2.2.1. MODELOS HEURÍSTICOS
# 2.2.2. MODELO DE MARKOV
# 2.2.3. FUSIONAMOS TODOS LOS MODELOS
# 3. VISUALIZACIÓN DEL EJEMPLO SEGUNDO
# 3.1. VISUALIZACIÓN DEL TOTAL DE CONVERSIONES
# 3.1.1. PREPARAMOS LOS DATOS
# 3.1.2. TRAZAMOS EL GRÁFICO
# 3.2. VISUALIZACIÓN DEL VALOR TOTAL DE LAS CONVERSIONES
# 3.2.1. PREPARAMOS LOS DATOS
# 3.2.2. TRAZAMOS EL GRÁFICO
# 3.3. HEATMAP DE LA MATRIZ DE TRANSICIONES DEL MODELO DE MARKOV
# 3.3.1. PREPARAMOS LOS DATOS
# 3.3.2. CREAMOS EL GRÁFICO HEATMAP


#============================#
# 0. INSTALACIÒN DE PAQUETES #
#============================#

library (dplyr)
library (reshape2)
library (ggplot2)
library (ChannelAttribution)
library (markovchain)
library (ggthemes)


#====================#
# 1. EJEMPLO PRIMERO #
#====================#

# 1.1. CREAMOS EL EJEMPLO
#========================

miChAttr1 <- data.frame (
  ruta = c (
    'c1 > c2 > c3',
	'c1 > c2 > c3',
	'c1 > c2 > c3',
	'c1',
	'c1 > c2 > c1',
	'c2 > c3'),
  conversion = c (1, 0, 1, 0, 1, 0),						# 1: Sí; 0: No
  no_convers = c (0, 1, 0, 1, 0, 1))						# lo contrario

# 1.2. CREACIÓN DEL MODELO DE MARKOV
#===================================

mi_Mark1 <- markov_model (miChAttr1,						# data.frame conteniendo las rutas y conversiones
  var_path = 'ruta',										# variable conteniendo las rutas
  var_conv = 'conversion',									# variable que contiene las conversiones (= 1)
  var_value = NULL,											# valor de las conversiones: por defecto NULL
  var_null = 'no_convers',									# variable que contiene las NO conversiones (= 1)
  order = 1,												# cadena de Markov de primer orden (por defecto)
  sep = ">",												# separador: por defecto ">"
  out_more = TRUE)											# por defecto: FALSE; si TRUE: devuelve "transition probabilities" entre canales y "removal effects"
# Vemos lo que tenemos:
mi_Mark1

# 1.3. RESULTADOS DE LA ATRIBUCIÓN
#=================================
mi_Mark1$result												# total de conversiones por canal
trans_matrix <- mi_Mark1$transition_matrix					# matriz de transiciones
trans_sint <- dcast (trans_matrix,							# matriz sintética
  channel_from ~ channel_to,
  value.var = 'transition_probability')

# 1.4. TRAZADO DEL DIAGRAMA DE MARKOV
#====================================
#
# 1.4.1. AÑADIMOS CASOS PARA TRAZAR EL DIAGRAMA
df_dummy <- data.frame (
  channel_from = c ('(start)', '(conversion)', '(null)'),
  channel_to = c ('(start)', '(conversion)', '(null)'),
  transition_probability = c (0, 1, 1))
para_graph <- rbind (trans_matrix, df_dummy)				# unimos amobos data.frame
#
# 1.4.2. ORDENAMOS LOS CANALES
para_graph$channel_from <- factor (para_graph$channel_from, levels = c ('(start)', '(conversion)', '(null)', 'c1', 'c2', 'c3'))
para_graph$channel_to <- factor (para_graph$channel_to, levels = c ('(start)', '(conversion)', '(null)', 'c1', 'c2', 'c3'))
para_graph <- dcast (para_graph, channel_from ~ channel_to, value.var = 'transition_probability')
#
# 1.4.3. CREAMOS EL OBJETO markovchain
trans_matrix <- matrix (data = as.matrix (para_graph [ , -1]),
  nrow = nrow (para_graph [ , -1]), ncol = ncol (para_graph [ , -1]),
  dimnames = list (c (as.character (para_graph [ , 1])),
  c (colnames (para_graph [ , -1]))))
trans_matrix [is.na (trans_matrix)] <- 0
trans_matrix1 <- new ("markovchain", transitionMatrix = trans_matrix)
#
# 1.4.4. TRAZAMOS EL DIAGRAMA
plot (trans_matrix1, edge.arrow.size = 0.35)


#====================#
# 2. EJEMPLO SEGUNDO #
#====================#

# 2.1. CARGAMOS EL dataset "Data" INCLUIDO EN EL PAQUETE {ChannelAttribution}
#============================================================================
data (PathData)
# Las variables son:
# Path Variable: rutas seguidas por clientes
# Conversion Variable: cuántas veces esa ruta acaba en conversión
# Value Variable: valor monetario de la conversión
# Null Variable: cuántas veces esa ruta acaba en NO conversión
str (Data)													# vemos lo que tenemos
head (Data)													# vemos lo que tenemos

# 2.2. CALCULAMOS LOS MODELOS HEURÍSTICOS Y DE MARKOV
#====================================================
#
# 2.2.1. MODELOS HEURÍSTICOS
# Estima tres modelos heurísticos: Primera interacción (first_touch), Última interacción (last_touch) y Modelo de Atribución Lineal (linear_touch)
miHeuris2 <- heuristic_models (
  Data,														# data.frame con las rutas (paths) y las conversiones
  var_path = 'path',										# variable con los "paths"
  var_conv = 'total_conversions',							# variable con las conversiones
  var_value = 'total_conversion_value',						# variable con el valor total de la conversión: por defecto NULL
  sep = ">")												# separador entre los canales: por defecto ">"
#
# 2.2.2. MODELO DE MARKOV
mi_Mark2 <- markov_model (
  Data,
  var_path = 'path',
  var_conv = 'total_conversions',
  var_value = 'total_conversion_value',
  order = 1,												# modelo de Markov de orden 1 (por defecto)
  seed = 1,													# para garantizar idénticos resultados: por defecto NULL
  out_more = TRUE)
#
# 2.2.3. FUSIONAMOS TODOS LOS MODELOS
all_models <- merge (miHeuris2, mi_Mark2$result, by = 'channel_name')


#======================================#
# 3. VISUALIZACIÓN DEL EJEMPLO SEGUNDO #
#======================================#

# 3.1. VISUALIZACIÓN DEL TOTAL DE CONVERSIONES
#=============================================

# 3.1.1. PREPARAMOS LOS DATOS
# Seleccionamos las columnas que nos interesan:
datosConversiones <- all_models [ , c (1,2,4,6,8)]
# Renombramos las columnas:
colnames (datosConversiones) <- c ('channel_name', 'first_touch', 'last_touch', 'linear_touch', 'markov_model')
# Transformamos el dataset en data.frame para ser usado con ggplot2:
datosConversiones <- melt (datosConversiones, id = 'channel_name')
# Renombramos las columnas:
colnames (datosConversiones) <- c ('canal', 'modelo', 'valor')
#
# 3.1.2. TRAZAMOS EL GRÁFICO
ggplot (datosConversiones, aes (canal, valor, fill = modelo)) +
geom_bar (stat = 'identity', position = 'dodge') +
ggtitle ('Conversiones totales atribuidas a cada canal') + 
theme (axis.title.x = element_text (vjust = -1)) +
theme (axis.title.y = element_text (vjust = +2)) +
theme (title = element_text (size = 16)) +
theme (plot.title = element_text (size = 20)) +
xlab ("Canales") +
ylab ("Número de Conversiones")

# 3.2. VISUALIZACIÓN DEL VALOR TOTAL DE LAS CONVERSIONES
#=======================================================

# 3.2.1. PREPARAMOS LOS DATOS
# Seleccionamos las columnas que nos interesan:
datosValor <- all_models [ , c (1,3,5,7,9)]
colnames (datosValor) <- c ('channel_name', 'first_touch', 'last_touch', 'linear_touch', 'markov_model')
# Transformamos el dataset en data.frame para ser usado con ggplot2:
datosValor <- melt (datosValor, id = 'channel_name')
# Renombramos las columnas:
colnames (datosValor) <- c ('canal', 'modelo', 'valor')
#
# 3.2.2. TRAZAMOS EL GRÁFICO
ggplot (datosValor, aes (canal, valor, fill = modelo)) +
geom_bar (stat = 'identity', position='dodge') +
ggtitle ('Valor Total atribuido a cada canal') + 
theme (axis.title.x = element_text (vjust = -1)) +
theme (axis.title.y = element_text (vjust = +1)) +
theme (title = element_text (size = 16)) +
theme (plot.title = element_text (size = 20)) +
xlab ("Canales") +
ylab ("Valor Total")

# 3.3. HEATMAP DE LA MATRIZ DE TRANSICIONES DEL MODELO DE MARKOV
#===============================================================

# 3.3.1. PREPARAMOS LOS DATOS
trans_matrix2 <- mi_Mark2$transition_matrix
cols <- c ("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a", "#e29421", "#e29421", "#f05336", "#ce472e")
t <- max (trans_matrix2$transition_probability)
#
# 3.3.2. CREAMOS EL GRÁFICO HEATMAP
ggplot (trans_matrix2, aes (y = channel_from, x = channel_to, fill = transition_probability)) +
theme_minimal () +
geom_tile (colour = "white", width = .9, height = .9) +
scale_fill_gradientn (colours = cols, limits = c (0, t),
  breaks = seq (0, t, by = t/4),
  labels = c ("0", round (t/4*1, 2), round (t/4*2, 2), round (t/4*3, 2), round (t/4*4, 2)),
  guide = guide_colourbar (ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
geom_text (aes (label = round (transition_probability, 2)), fontface = "bold", size = 4) +
theme (legend.position = 'bottom',
  legend.direction = "horizontal",
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.title = element_text (size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
  axis.title.x = element_text (size = 20, face = "bold"),
  axis.title.y = element_text (size = 20, face = "bold"),
  axis.text.y = element_text (size = 8, face = "bold", color = 'black'),
  axis.text.x = element_text (size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain")) +
labs (x = "Canal HASTA", y = "Canal DESDE") +
ggtitle ("Heatmap de la Matriz de Transiciones del Modelo de Markov")