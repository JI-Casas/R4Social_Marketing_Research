##############################################################################
#####     XI JORNADAS DE USUARIOS DE R. MADRID, 14-16 NOVIEMBRE 2019     #####
#####     TALLER HERRAMIENTAS DE R PARA LA INVESTIGACI�N DE MERCADOS     #####
#####                      7. MAPAS PERCEPTUALES                         #####
#####                      POR Jos� Ignacio Casas                        #####
##############################################################################
# 0. CARGA DE PAQUETES Y DATASETS
# 1. MAPAS PERCEPTUALES
# 1.1. MAPA PERCEPTUAL
# 1.2. AN�LISIS DE CORRESPONDENCIAS (CA)
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
  focus = "row",											# Por defecto "row", eval�a la contribuci�n de las filas en la definici�n de las dimensiones. "col" para columnas
  guide = FALSE,											# TRUE o FALSE (por defecto) si se quiere que los puntos est�n coloreados seg�n la dimensi�n con la que tienen una
															# correlaci�n relativamente m�s alta
  size.labls = 3)											# Tama�o de las etiquetas de los ejes que muestran el mapa perceptual, 3 por defecto

# 1.2. AN�LISIS DE CORRESPONDENCIAS (CA)
#=======================================

caPlot (breakfast,											# Tabla de contingencia en formato data.frame
  x = 1,													# Primera de las dos dimensiones, a mostrar en el eje de las x (1 por defecto)
  y = 2,													# Segunda de las dos dimensiones, a mostrar en el eje de las y (2 por defecto)
  adv.labls = TRUE,											# Muestra las etiquetas (TRUE, por defecto)
  cntr = "columns",											# Cuando adv.labls = TRUE, cntr = "rows" o "columns": muestra su contribuci�n a las dimensiones seleccionadas
  percept = FALSE,											# Si percept = TRUE, se muestra el gr�fico como mapa perceptual (por defecto, FALSE)
  qlt.thres = NULL,											# quality of the display�s threshold under which points will not be given labels. NULL is the default
  dot.size = 2.5,											# Tama�o de los puntos, 2.5 por defecto
  cex.labls = 3,											# Tama�o de los r�tulos, 3 por defecto
  cex.percept = 3)											# Tama�o de las etiquetas de los ejes que muestran el mapa perceptual, 3 por defecto
#
# La funci�n devuelve un data.frame con el siguiente contenido para los puntos de las filas y columnas:
## coordenadas de la primera dimeni�n seleccionada
## coordenadas de la segunda dimeni�n seleccionada
## contribuci�n a la primera dimensi�n seleccionada
## contribuci�n a la segunda dimensi�n seleccionada
## calidad en la primera dimensi�n seleccionada (% de inercia)
## calidad en la segunda dimensi�n seleccionada (% de inercia)
## correlaci�n con la primera dimensi�n seleccionada
## correlaci�n con la segunda dimensi�n seleccionada
## asteriscos indicando si la categor�a correspodiente es una contribuidora importante para la primera y/o segunda dimensi�n seleccionada
#
# Inercia es la medida de la distribuci�n de los puntos de los datos, semejante a la varianza. Las dimensiones se ordenan empezando por la que es m�s responsable
# por la distribuci�n de los puntos de los datos, la primera dimensi�n siempre la de mayor inercia. La inercia de una sola dimensi�n no aporta mucha informaci�n.
# Conparando la inercia de una dimensi�n con la del total obtenemos la proporci�n de inercia de esa dimensi�n en el total de la tabla

# 1.3. MAPA PERCEPTUAL CON caPlot()
#==================================

# Devuelve el data.frame descrito m�s arriba
caPlot (breakfast,
  x = 1,
  y = 2,
  adv.labls = TRUE,
  cntr = "columns",
  percept = TRUE,											# percept = TRUE: muestra el gr�fico como mapa perceptual (por defecto, FALSE)
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
  percept = TRUE,											# percept = TRUE: muestra el gr�fico como mapa perceptual (por defecto, FALSE)
  qlt.thres = NULL,
  dot.size = 2.5,
  cex.labls = 3,
  cex.percept = 3)