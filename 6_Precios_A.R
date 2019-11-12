##############################################################################
#####     XI JORNADAS DE USUARIOS DE R. MADRID, 14-16 NOVIEMBRE 2019     #####
#####     TALLER HERRAMIENTAS DE R PARA LA INVESTIGACIÓN DE MERCADOS     #####
#####               6. Técnicas de exploración de precios                #####
#####                    A. PRICE SENSITIVITY METER                      #####
#####                      POR José Ignacio Casas                        #####
##############################################################################
# 0. CARGA DE PAQUETES Y DATOS
# 0.1. PAQUETES
# 0.2. DATASET DE EJEMPLO
# 1. ANÁLISIS
# 1.1. CREAMOS UN OBJETO DE CLASE psm
# 1.2. DISTRIBUCIÓN DE RESPONDIENTES SEGÚN PRECIO
# 2. VISUALIZACIÓN DE LOS RESULTADOS
# 2.1. CREAMOS EL GRÁFICO CON TODOS LOS ELEMENTOS, SIN ETIQUETAS
# 2.2. ETIQUETAS Y COLORES

#==============================#
# 0. CARGA DE PAQUETES Y DATOS #
#==============================#

# 0.1. PAQUETES
#==============

library (pricesensitivitymeter)								# Paquete necesario
library	(ggplot2)											# Paquete necesario para los gráficos

# 0.2. DATASET DE EJEMPLO
#========================

set.seed (123)
datos_psm <- data.frame ("demasiadoBarato" = 50 * (rpois (400, 4) + runif (50, 0, 1)))
datos_psm$barato <- datos_psm$demasiadoBarato + 50 * (runif(50, 1, 2))
datos_psm$caro <- datos_psm$barato + 50 * (runif(50, 2, 4))
datos_psm$demasiadoCaro <- datos_psm$caro + 50 * (runif(50, 3, 4))

head (datos_psm)											# Vemos lo que tenemos
str (datos_psm)												# Vemos lo que tenemos


#=============#
# 1. ANÁLISIS #
#=============#

# 1.1. CREAMOS UN OBJETO DE CLASE psm
#====================================

output_psm <- psm_analysis (								# Crea un objeto de clase "psm"
  toocheap = "demasiadoBarato",
  cheap = "barato",
  expensive = "caro",
  tooexpensive = "demasiadoCaro",
  data = datos_psm)
#
summary (output_psm)										# Vemos el resultado
# -- Accepted Price Range									# Intervalo de precios aceptables
# -- Indifference Price Point								# Precio de Indiferencia
# -- Optimal Price Point									# Precio Óptimo

# 1.2. DISTRIBUCIÓN DE RESPONDIENTES SEGÚN PRECIO
#================================================
# Por ejemplo:
output_psm$data_vanwestendorp [100, ]						# Datos correspondientes a la línea 100 del dataset
## linea													# 100
## price													# Precio: 165.1452
## ecdf_toocheap											# % que lo consideran demasiado barato: 0.705
## ecdf_cheap												# % que lo consideran barato: 0.9325
## ecdf_expensive											# % que lo consideran caro: 0
## ecdf_tooexpensive										# % que lo consideran demasiado caro: 0
## ecdf_not_cheap	
## ecdf_not_expensive										# % que NO lo consideran caro: 1


#====================================#
# 2. VISUALIZACIÓN DE LOS RESULTADOS #
#====================================#

# 2.1. CREAMOS EL GRÁFICO CON TODOS LOS ELEMENTOS, SIN ETIQUETAS
#===============================================================

psm_plot <- ggplot (
  data = output_psm$data_vanwestendorp,
  aes (x = price)) +
annotate (
  geom = "rect",											# área sombreada del rango de precios aceptables
  xmin = output_psm$pricerange_lower,
  xmax = output_psm$pricerange_upper,
  ymin = 0,
  ymax = Inf,
  fill = "grey50",
  alpha = 0.3) +
geom_line(													# línea demasiado barato
  aes (y = ecdf_toocheap,
    colour = "demasiado barato",
    linetype = "demasiado barato"),
  size = 1) +
geom_line (													# línea demasiado caro
  aes (y = ecdf_tooexpensive,
    colour = "demasiado caro",
    linetype = "demasiado caro"),
  size = 1) + 
geom_line (													# línea NO barato
  aes (y = ecdf_not_cheap,
    colour = "no barato",
    linetype = "no barato"),
  size = 1) +
geom_line (													# línea NO caro
  aes (y = ecdf_not_expensive,
    colour = "no caro",
    linetype = "no caro"),
  size = 1) + 
annotate (													# IDP (Indifference Price Point o Precio de Indiferencia): intersección de "barato" y "caro"
  geom = "point",
  x = output_psm$idp, 
  y = output_psm$data_vanwestendorp$ecdf_not_cheap [output_psm$data_vanwestendorp$price == output_psm$idp],
  size = 5,
  shape = 18,
  colour = "#009E73") + 
annotate(													# OPP (Optimal Price Point o Precio Óptimo): intersección de "demasiado brato" y "demasiado caro"
  geom = "point",
  x = output_psm$opp, 
  y = output_psm$data_vanwestendorp$ecdf_toocheap [output_psm$data_vanwestendorp$price == output_psm$opp],
  size = 3,
  shape = 17,
  colour = "#009E73")

# 2.2. ETIQUETAS Y COLORES
#=========================

psm_plot +
labs (
  x = "Precio",
  y = "Share de respondientes (0-1)",
  title = "Gráfico de Price Sensitivity Meter",
  caption = "Área sombreada: rango de precios aceptables\nDatos generados aleatoriamente")  + 
scale_colour_manual (
  name = "Leyenda",
  values = c (
    "demasiado barato" = "#009E73",
    "no barato" = "#009E73",
    "no caro" = "#D55E00",
    "demasiado caro" = "#D55E00")
  ) + 
scale_linetype_manual (
  name = "Leyenda",
  values = c(
    "demasiado barato" = "dotted",
    "no barato" = "solid",
    "no caro" = "solid",
    "demasiado caro" = "dotted")
  ) + 
annotate (
  geom = "text",											# Etiqueta del IDP
  x = output_psm$idp + 100, 
  y = output_psm$data_vanwestendorp$ecdf_not_cheap [output_psm$data_vanwestendorp$price == output_psm$idp],
  label = paste ("IDP: ", round (output_psm$idp, 2))) + 
annotate (													# Etiqueta del OPP
  geom = "text",
  x = output_psm$opp + 100,
  y = output_psm$data_vanwestendorp$ecdf_toocheap [output_psm$data_vanwestendorp$price == output_psm$opp],
  label = paste ("OPP: ", round (output_psm$opp, 2))) +
theme_minimal()