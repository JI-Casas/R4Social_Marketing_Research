##############################################################################
#####     XI JORNADAS DE USUARIOS DE R. MADRID, 14-16 NOVIEMBRE 2019     #####
#####     TALLER HERRAMIENTAS DE R PARA LA INVESTIGACIÓN DE MERCADOS     #####
#####                      7. MAPAS PERCEPTUALES                         #####
#####                      POR José Ignacio Casas                        #####
##############################################################################
# 0. CARGA DE PAQUETES Y DATASETS
# 1. MAPAS PERCEPTUALES
# 1.1. MAPA PERCEPTUAL
# 1.2. ANÁLISIS DE CORRESPONDENCIAS (CA)
# 1.3. MAPA PERCEPTUAL CON caPlot()


#=================================#
# 0. CARGA DE PAQUETES Y DATASETS #
#=================================#
library (CAinterprTools)									# Cargamos paquete necesario
data (breakfast)											# Cargamos fichero de prueba, incluido en el paquete anterior
str (breakfast)												# Vemos lo que tenemos


#=======================#
# 1. MAPAS PERCEPTUALES #
#=======================#

# 1.1. MAPA PERCEPTUAL
#=====================

caPercept (breakfast,										# Tabla de contingencia en formato data.frame
  x = 1,													# Primera de las dos dimensiones, a mostrar en el eje de las x (1 por defecto)
  y = 2,													# Segunda de las dos dimensiones, a mostrar en el eje de las y (2 por defecto)
  focus = "row",											# Por defecto "row", evalúa la contribución de las filas en la definición de las dimensiones. "col" para columnas
  guide = FALSE,											# TRUE o FALSE (por defecto) si se quiere que los puntos estén coloreados según la dimensión con la que tienen una
															# correlación relativamente más alta
  size.labls = 3)											# Tamaño de las etiquetas de los ejes que muestran el mapa perceptual, 3 por defecto

# 1.2. ANÁLISIS DE CORRESPONDENCIAS (CA)
#=======================================

caPlot (breakfast,											# Tabla de contingencia en formato data.frame
  x = 1,													# Primera de las dos dimensiones, a mostrar en el eje de las x (1 por defecto)
  y = 2,													# Segunda de las dos dimensiones, a mostrar en el eje de las y (2 por defecto)
  adv.labls = TRUE,											# Muestra las etiquetas (TRUE, por defecto)
  cntr = "columns",											# Cuando adv.labls = TRUE, cntr = "rows" o "columns": muestra su contribución a las dimensiones seleccionadas
  percept = FALSE,											# Si percept = TRUE, se muestra el gráfico como mapa perceptual (por defecto, FALSE)
  qlt.thres = NULL,											# quality of the display’s threshold under which points will not be given labels. NULL is the default
  dot.size = 2.5,											# Tamaño de los puntos, 2.5 por defecto
  cex.labls = 3,											# Tamaño de los rótulos, 3 por defecto
  cex.percept = 3)											# Tamaño de las etiquetas de los ejes que muestran el mapa perceptual, 3 por defecto
#
# La función devuelve un data.frame con el siguiente contenido para los puntos de las filas y columnas:
## coordenadas de la primera dimenión seleccionada
## coordenadas de la segunda dimenión seleccionada
## contribución a la primera dimensión seleccionada
## contribución a la segunda dimensión seleccionada
## calidad en la primera dimensión seleccionada (% de inercia)
## calidad en la segunda dimensión seleccionada (% de inercia)
## correlación con la primera dimensión seleccionada
## correlación con la segunda dimensión seleccionada
## asteriscos indicando si la categoría correspodiente es una contribuidora importante para la primera y/o segunda dimensión seleccionada
#
# Inercia es la medida de la distribución de los puntos de los datos, semejante a la varianza. Las dimensiones se ordenan empezando por la que es más responsable
# por la distribución de los puntos de los datos, la primera dimensión siempre la de mayor inercia. La inercia de una sola dimensión no aporta mucha información.
# Conparando la inercia de una dimensión con la del total obtenemos la proporción de inercia de esa dimensión en el total de la tabla

# 1.3. MAPA PERCEPTUAL CON caPlot()
#==================================

# Devuelve el data.frame descrito más arriba
caPlot (breakfast,
  x = 1,
  y = 2,
  adv.labls = TRUE,
  cntr = "columns",
  percept = TRUE,											# percept = TRUE: muestra el gráfico como mapa perceptual (por defecto, FALSE)
  qlt.thres = NULL,
  dot.size = 2.5,
  cex.labls = 3,
  cex.percept = 3)
#
caPlot (breakfast,
  x = 1,
  y = 2,
  adv.labls = TRUE,
  cntr = "rows",											# "rows": similar a caPercept()
  percept = TRUE,											# percept = TRUE: muestra el gráfico como mapa perceptual (por defecto, FALSE)
  qlt.thres = NULL,
  dot.size = 2.5,
  cex.labls = 3,
  cex.percept = 3)