##############################################################################
#####     XI JORNADAS DE USUARIOS DE R. MADRID, 14-16 NOVIEMBRE 2019     #####
#####     TALLER HERRAMIENTAS DE R PARA LA INVESTIGACIÓN DE MERCADOS     #####
#####                        REQUISITOS TALLER 2                         #####
#####                      POR José Ignacio Casas                        #####
##############################################################################

#=========================#
# 1. CONFIGURACIÓN DEL PC #
#=========================#
# R version 3.6.1 (2019-07-05)
# RStudio Version 1.1.463


#==============================================#
# 2. PAQUETES A INSTALAR PREVIAMENTE AL TALLER #
#==============================================#

# 2.1. VECTOR DE PAQUETES
#========================
paquetes_taller2 <- c (
  "AlgDesign",
  "arules",
  "CAinterprTools",
  "car",
  "cluster",
  "conjoint",
  "ChannelAttribution",
  "eRm",
  "factoextra",
  "fpc",
  "ggthemes",
  "ltm",
  "markovchain",
  "mirt",
  "mlogit",
  "NbClust",
  "pheatmap",
  "pricesensitivitymeter",
  "reshape2",
  "rfm",
  "survival",
  "tidyverse"
  )

# 2.2. INSTALACIÓN
#=================
install.packages (paquetes_taller2)


#====================================================================#
# 3. DATASETS A INCORPORAR (NO INCLUIDOS EN LOS PAQUETES ANTERIORES) #
#====================================================================#

# 3.1. dataset_clus. DATASET PREPARADO EX-PROFESO, DESCARGABLE DESDE GitHub
#==========================================================================
ruta_dataset1 <- "https://github.com/JI-Casas/R4Social_Marketing_Research/raw/master/dataset_clus.RData"		# ruta en internet
load (url (ruta_dataset1))									# Incorporamos del data.frame "dataset_clus"
save (dataset_clus, file = "dataset_clus.RData")			# Lo guardamos en nuestro directorio de trabajo
load (file = "dataset_clus.RData")							# Para incorporar el dataset desde nuestro directorio de trabajo
# Las variables son:
# id:				chr - identificador de cada caso/objeto
# habitat:			Ord.factor w/ 7 levels - tamaño del hábitat
# item1:			logi - Ha adquirido este item
# item2:			logi - Ha adquirido este item
# item3:			logi - Ha adquirido este item
# item4:			logi - Ha adquirido este item
# item5:			logi - Ha adquirido este item
# online_shopping:	num - % de compras online
# sexo:				Factor w/ 2 levels "Hombre","Mujer"
# edad:				num
# ingresos:			Ord.factor w/ 11 levels - Nivel de ingresos
# estudios:			Ord.factor w/ 6 levels - Nivel de estudios
# estatus:			Ord.factor w/ 5 levels - Estatus social

# 3.2. dataset para el Conjoint Analysis
#=======================================
cbc_df <- read.csv (										# convertimos el ".csv" en data.frame
  "http://goo.gl/5xQObB",									# ruta del fichero en formato ".csv"
  colClasses = c (											# cualificamos las variables
    seat = "factor",
    price = "factor",
    choice = "integer")
  )
save (cbc_df, file = "cbc_df.RData")						# Lo guardamos en nuestro directorio de trabajo
load (file = "cbc_df.RData")								# Para incorporar el dataset desde nuestro directorio de trabajo