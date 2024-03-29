##############################################################################
#####     XI JORNADAS DE USUARIOS DE R. MADRID, 14-16 NOVIEMBRE 2019     #####
#####     TALLER HERRAMIENTAS DE R PARA LA INVESTIGACI�N DE MERCADOS     #####
#####              4.COSTUMER VALUE (VALOR DEL CLIENTE) Y                #####
#####           RFM (RECENCY, FREQUENCY, MONETARY VALUE) CON R           #####
#####                      POR Jos� Ignacio Casas                        #####
##############################################################################
# 0. INSTALACI�N DE PAQUETES Y DATASETS
# 0.1. PAQUETES
# 0.2 DATASET DE EJEMPLO
# 1. AN�LISIS DESCRIPTIVO
# 1.1. CREACI�N DE LA TABLA RFM
# 1.2. GR�FICOS DESCRIPTIVOS
# 2. SEGMENTOS
# 2.1. CREACI�N DE LOS SEGMENTOS
# 2.2. EXPLORACI�N DE LOS SEGMENTOS
# 2.2.1. TAMA�O DE CADA SEGMENTO
# 2.2.2. GRAFICOS DE LAS MEDIANAS DE Recency, Frequency Y Monetary POR SEGMENTO


#=======================================#
# 0. INSTALACI�N DE PAQUETES Y DATASETS #
#=======================================#

# 0.1. PAQUETES
#==============

library (rfm)
library (tidyverse)

# 0.2 DATASET DE EJEMPLO
#=======================

# Incorporamos el dataset de ejemplo incluido en el paquete {rfm}
data (rfm_data_orders)
str (rfm_data_orders)										# Vemos lo que tenemos
head (rfm_data_orders)										# Vemos lo que tenemos


#=========================#
# 1. AN�LISIS DESCRIPTIVO #
#=========================#

# 1.1. CREACI�N DE LA TABLA RFM
#==============================

# Fijamos la fecha de referencia:
analysis_date <- lubridate::as_date ("2006-12-31", tz = "UTC")
# Creamos la Tabla RFM
result <- rfm_table_order (rfm_data_orders,					# dataset inicial: data.frame o tibble
  customer_id,												# identificador del cliente
  order_date,												# fecha de la compra
  revenue,													# cantidad gastada por el cliente
  analysis_date,											# nuestra fecha de referencia
  recency_bins = 5,											# n�mero de segmentos de clientes seg�n el valor de recency: por defecto 5
  frequency_bins = 5,										# n�mero de segmentos de clientes seg�n el valor de frequency: por defecto 5
  monetary_bins = 5)										# n�mero de segmentos de clientes seg�n el valor de monetary: por defecto 5
result$rfm													# Vemos el contenido de la Tabla RFM
View (result$rfm)											# Lo vemos en otro formato
# El contenido de la Tabla RFM es:
# -- customer_id:		identificador del cliente 
# -- date_most_recent:	fecha de la transacci�n m�s reciente
# -- recency_days:		d�as desde la transacci�n m�s reciente
# -- transaction_count:	n�mero de transacciones por cliente
# -- amount:			ingresos totales generados por el cliente
# -- recency_score:		recency del cliente
# -- frequency_score:	frequency del cliente
# -- monetary_score:	monetary del cliente
# -- rfm_score:			RFM del cliente: concatenaci�n de los tres "scores" anteriores

# 1.2. GR�FICOS DESCRIPTIVOS
#===========================

rfm_order_dist (result)										# Distribuci�n de los clientes por n�mero de transacciones
rfm_histograms (result)										# Histogramas de Monetary / Recency / Frequency
rfm_heatmap (result)										# Heatmap de la media del valor monetario (Monetary) seg�n Frequency & Recency
rfm_bar_chart (result)										# Gr�fico del valor monetario (Monetary) seg�n Frequency & Recency
rfm_rm_plot (result)										# Gr�fico Recency vs. Monetary
rfm_fm_plot (result)										# Gr�fico Frequency vs. Monetary
rfm_rf_plot (result)										# Gr�fico Recency vs. Frequency


#==============#
# 2. SEGMENTOS #
#==============#

# 2.1. CREACI�N DE LOS SEGMENTOS
#===============================

# Creamos el vector de nombres de segmentos:
segment_names <- c("Campeones", "Clientes leales", "Potencialmente leales",
  "Nuevos clientes", "Prometedores", "Atenci�n necesaria", "A punto de dormir",
  "Con riesgo", "No podemos perderlos", "Ya perdidos")
# Dise�amos los perfiles de cada segmento seg�n su puntaci�n en Recency, 					# MEDIANA DE Recency POR SEGMENTO y Monetary:
recency_lower <- c (4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper <- c (5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c (4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c (5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower <- c (4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper <- c (5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
#
segmentos <- rfm_segment (									# Crea un data.frame de los segmentos
  result,													# Tabla RFM
  segment_names,											# Vector de nombres de segmentos
  recency_lower,
  recency_upper,
  frequency_lower,
  frequency_upper,
  monetary_lower,
  monetary_upper)

# 2.2. EXPLORACI�N DE LOS SEGMENTOS
#==================================

# 2.2.1. TAMA�O DE CADA SEGMENTO
segmentos %>%
  count (segment) %>%
  arrange (desc(n)) %>%
  rename (Segmento = segment, Tama�o = n)

# 2.2.2. GRAFICOS DE LAS MEDIANAS DE Recency, Frequency Y Monetary POR SEGMENTO
rfm_plot_median_recency (segmentos)							# MEDIANA DE Recency POR SEGMENTO
rfm_plot_median_frequency (segmentos)						# MEDIANA DE Frequency POR SEGMENTO
rfm_plot_median_monetary (segmentos)						# MEDIANA DE Monetary POR SEGMENTO