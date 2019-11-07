##############################################################################
#####     XI JORNADAS DE USUARIOS DE R. MADRID, 14-16 NOVIEMBRE 2019     #####
#####     TALLER HERRAMIENTAS DE R PARA LA INVESTIGACIÓN DE MERCADOS     #####
#####                 2. SALES FUNNEL (EMBUDO DE VENTAS)                 #####
#####                      POR José Ignacio Casas                        #####
##############################################################################
# 0. INSTALACIÒN DE PAQUETES
# 1. CREACIÓN DEL EJEMPLO
# 1.1. CONTENIDO
# 1.2. CLIENTES
# 1.3. COMBINAMOS AMBOS DATASETS
# 2. CREACIÓN DE DATOS COMPLEMENTARIOS PARA EL GRÁFICO DE EMBUDO DE VENTAS
# 2.1. VALORES AUXILIARES "DUMMIES", MÁXIMOS Y MÍNIMOS DE CADA VARIABLE
# 2.2. data.frame PARA DIBUJAR LAS LÍNEAS DEL EMBUDO
# 2.3. data.frame CON DUMMIES
# 2.4. data.frame CON RATIOS
# 3. CREACIÓN DEL data.frame FINAL
# 4. FIJAMOS EL ORDEN DE LAS FASES
# 5. CALCULAMOS LA POSICIÓN DE LOS RÓTULOS
# 6. CREACIÓN DE LA PALETA DE COLORES, CON 'WHITE' PARA DUMMIES
# 7. TRAZADO DEL GRÁFICO


#============================#
# 0. INSTALACIÒN DE PAQUETES #
#============================#

library (dplyr)
library (ggplot2)
library (reshape2)


#=========================#
# 1. CREACIÓN DEL EJEMPLO #
#=========================#

# 1.1. CONTENIDO
#===============

contenido_df <- data.frame (
  contenido = c ('Principal', 'Ad Landing', 'Producto1', 'Producto2', 'Producto3', 'Producto4', 'Carro compra', 'Página gracias'),
  fase = c ('Atención', 'Atención', 'Interés', 'Interés', 'Interés', 'Interés', 'Deseo', 'Acción'),
  numero = c (150, 80, 80, 40, 35, 25, 90, 80))

# 1.2. CLIENTES
#==============

clientes_df <- data.frame (
  contenido = c ('Nuevo', 'Comprometido', 'Leal'),
  fase = c ('Nuevo', 'Comprometido', 'Leal'),
  numero = c (20, 25, 35))

# 1.3. COMBINAMOS AMBOS DATASETS
#===============================

salesFun_df <- rbind (contenido_df, clientes_df)


#==========================================================================#
# 2. CREACIÓN DE DATOS COMPLEMENTARIOS PARA EL GRÁFICO DE EMBUDO DE VENTAS #
#==========================================================================#

# 2.1. VALORES AUXILIARES "DUMMIES", MÁXIMOS Y MÍNIMOS DE CADA VARIABLE
#======================================================================

salesFun_df <- salesFun_df %>%
  group_by (fase) %>%
  mutate (totnum = sum(numero)) %>%
  ungroup () %>%
  mutate (dum = (max (totnum) - totnum)/2, maxx = totnum + dum, minx = dum)

# 2.2. data.frame PARA DIBUJAR LAS LÍNEAS DEL EMBUDO
#===================================================

lineas_df <- salesFun_df %>%
  distinct (fase, maxx, minx)

# 2.3. data.frame CON DUMMIES
#============================

dummies_df <- salesFun_df %>%
  distinct (fase, dum) %>%
  mutate (contenido = 'dummy', numero = dum) %>%
  select (contenido, fase, numero)

# 2.4. data.frame CON RATIOS
#===========================

conv <- salesFun_df$totnum [salesFun_df$fase == 'Acción']
#
ratios_df <- salesFun_df %>%
  distinct (fase, totnum) %>%
  mutate (prevnum = lag (totnum),
  rate = ifelse (fase == 'Nuevo' | fase == 'Comprometido' | fase == 'Leal',
    round (totnum / conv, 3),
    round (totnum / prevnum, 3))) %>%
  select (fase, rate)
ratios_df <- na.omit (ratios_df)


#==================================#
# 3. CREACIÓN DEL data.frame FINAL #
#==================================#

salesFun_df <- salesFun_df %>%
  select (contenido, fase, numero)
salesFun_df <- rbind (salesFun_df, dummies_df)


#==================================#
# 4. FIJAMOS EL ORDEN DE LAS FASES #
#==================================#

salesFun_df$fase <- factor (salesFun_df$fase, levels = c ('Leal', 'Comprometido', 'Nuevo', 'Acción', 'Deseo', 'Interés', 'Atención'))
salesFun_df <- salesFun_df %>%
  arrange (desc (fase))
list1 <- salesFun_df %>%
  distinct (contenido) %>%
  filter (contenido != 'dummy')
salesFun_df$contenido <- factor (salesFun_df$contenido, levels = c (as.character (list1$contenido), 'dummy'))


#==========================================#
# 5. CALCULAMOS LA POSICIÓN DE LOS RÓTULOS #
#==========================================#

salesFun_df <- salesFun_df %>%
  arrange (fase, desc (contenido)) %>%
  group_by (fase) %>%
  mutate (pos = cumsum (numero) - 0.5 * numero) %>%
  ungroup()


#===============================================================#
# 6. CREACIÓN DE LA PALETA DE COLORES, CON 'WHITE' PARA DUMMIES #
#===============================================================#

cols <- c ("#fec44f", "#fc9272", "#a1d99b", "#fee0d2", "#2ca25f", "#8856a7", "#43a2ca", "#fdbb84", "#e34a33", "#a6bddb", "#dd1c77", "#ffffff")


#========================#
# 7. TRAZADO DEL GRÁFICO #
#========================#
ggplot() +
theme_minimal() +
coord_flip() +
scale_fill_manual (values = cols) +
geom_bar (
  data = salesFun_df,
  aes (x = fase, y = numero, fill = contenido),
  stat = "identity",
  width = 1) +
geom_text (data = salesFun_df [salesFun_df$contenido != 'dummy', ],
  aes (x = fase, y = pos, label = paste0 (contenido, '\n', numero)),
  size = 4, color = 'black', fontface = "bold") +
geom_ribbon (data = lineas_df, aes (x = fase, ymax = max (maxx), ymin = maxx, group = 1), fill = 'white') +
geom_line (data = lineas_df, aes (x = fase, y = maxx, group = 1),
  color = 'darkred', size = 4) +
geom_ribbon (data = lineas_df, aes (x = fase, ymax = minx, ymin = min (minx), group = 1), fill = 'white') +
geom_line (data = lineas_df, aes (x = fase, y = minx, group = 1), color = 'darkred', size = 4) +
geom_text (data = ratios_df, aes (x = fase, y = (lineas_df$minx [-1]),
  label = paste0 (rate * 100, '%')), hjust = 1.2, color = 'darkblue', fontface = "bold") +
theme (legend.position = 'none', axis.ticks = element_blank (),
  axis.text.x = element_blank (), axis.title.x = element_blank ())