##############################################################################
#####     XI JORNADAS DE USUARIOS DE R. MADRID, 14-16 NOVIEMBRE 2019     #####
#####     TALLER HERRAMIENTAS DE R PARA LA INVESTIGACIÓN DE MERCADOS     #####
#####        3. MARKET BASKET ANALYSIS & ASSOCIATION RULES CON R         #####
#####      ANÁLISIS DE LA CESTA DE LA COMPRA & REGLAS DE ASOCIACIÓN      #####
#####                      POR José Ignacio Casas                        #####
##############################################################################
# 0. INSTALACIÓN DE PAQUETES Y DATASETS
# 0.1. PAQUETES
# 0.2. FORMATOS DE DATOS DE PARTIDA
# 0.2.1. FORMATO DE TABLA LARGA O SINGLE. EJEMPLO 1
# 0.2.2. FORMATO DE TABLA LARGA O SINGLE. EJEMPLO 2
# 0.2.3. FORMATO DE CESTA O BASKET
# 0.2.4. HACEMOS LIMPIEZA
# 1. ANÁLISIS DESCRIPTIVO DEL EJEMPLO 1
# 1.1. FRECUENCIAS DE LOS ITEMS
# 1.2. ITEMS & ITEMSETS FRECUENTES CON EL ALGORITMO Apriori
# 1.3. ITEMS & ITEMSETS FRECUENTES CON EL ALGORITMO Eclat
# 2. ANÁLISIS DESCRIPTIVO DEL DATASET "Groceries" INCLUIDO EN EL PAQUETE {arules}
# 2.1. FRECUENCIAS DE LOS ITEMS
# 2.2. ITEMS & ITEMSETS FRECUENTES CON EL ALGORITMO Apriori
# 2.3. ITEMS & ITEMSETS FRECUENTES CON EL ALGORITMO Eclat
# 3. SELECCIÓN DE ITEMSETS
# 3.1. ITEMSETS QUE CONTIENEN UN ITEM DETERMINADO
# 3.2. ITEMSETS QUE CONTIENEN DOS ITEMS DETERMINADOS
# 4. REGLAS DE ASOCIACIÓN CON EL EJEMPLO 1
# 4.1. CREACIÓN DE LAS REGLAS DE ASOCIACIÓN
# 4.2. IDENTIFICACIÓN DE LAS REGLAS PRINCIPALES
# 5. REGLAS DE ASOCIACIÓN CON EL DATASET "Groceries" INCLUIDO EN EL PAQUETE {arules}
# 5.1. CREACIÓN DE LAS REGLAS DE ASOCIACIÓN
# 5.2. IDENTIFICACIÓN DE LAS REGLAS PRINCIPALES
# 5.3. ELIMINACIÓN DE REGLAS REDUNDANTES
# 5.4. REGLAS RELACIONADAS CON ITEMS DETERMINADOS
# 5.4.1. PARA IDENTIFICAR LOS ITEMS QUE INFLUYEN EN LA COMPRA DEL ITEM X
# 5.4.2. PARA IDENTIFICAR QUÉ ITEMS ESTÁN ASOCIADOS A LA COMPRA DEL ITEM X
# 5.5. TRANSACCIONES QUE CUMPLEN UNA DETERMINADA REGLA


#=======================================#
# 0. INSTALACIÓN DE PAQUETES Y DATASETS #
#=======================================#

# 0.1. PAQUETES
#==============

library (arules)

# 0.2. FORMATOS DE DATOS DE PARTIDA
#==================================

# 0.2.1. FORMATO DE TABLA LARGA O SINGLE. EJEMPLO 1
# Cada línea contiene un item y el identificador de la transacción (compra) a la que pertenece
# Creamos un dataset de ejemplo en formato single
# La columna 1 contiene la id de la transacción y la olumna 2 el item
datos_single1 <- paste (
  "id1 Pan",
  "id1 Leche",
  "id1 Carne",
  "id1 Pescado",
  "id2 Pan",
  "id2 Leche",
  "id2 Pescado",
  "id3 Pan",
  "id3 Leche",
  "id4 Leche",
  "id4 Carne",
  "id4 Pescado",
  "id5 Leche",
  "id5 Carne",
  "id6 Carne",
  "id6 Pescado",
  "id7 Leche",
  "id7 Pescado",
  sep ="\n")
cat (datos_single1)
write (datos_single1, file = "demo_single1")
# Volcamos los datos a un contenedor apropiado:
trans_demo_single1 <- read.transactions (
  "demo_single1",
  format = "single",
  cols = c (1,2))
inspect (trans_demo_single1)
colnames (trans_demo_single1)								# los items han sido ordenados alfabéticamente
#
# 0.2.2. FORMATO DE TABLA LARGA O SINGLE. EJEMPLO 2
# Creamos un dataset de ejemplo en formato single con cabeceras de columnas
datos_single2 <- paste (
  "item_id;trans_id",
  "Pan;id1",
  "Leche;id1",
  "Carne;id1",
  "Pescado;id1",
  "Pan;id2",
  "Leche;id2",
  "Pescado;id2",
  "Pan;id3",
  "Leche;id3",
  "Leche;id4",
  "Carne;id4",
  "Pescado;id4",
  "Leche;id5",
  "Carne;id5",
  "Carne;id6",
  "Pescado;id6",
  "Leche;id7",
  "Pescado;id7",
  sep ="\n")
cat (datos_single2)
write (datos_single2, file = "demo_single2")
# Volcamos los datos a un contenedor apropiado:
trans_demo_single2 <- read.transactions(
  "demo_single2",
  format = "single",
  header = TRUE,
  sep = ";",
  cols = c("trans_id", "item_id"))
inspect (trans_demo_single2)
#
# 0.2.3. FORMATO DE CESTA O BASKET
# Cada fila contiene todos los items que forman parte de una misma transacción
# Creamos un dataset de ejemplo en formato basket
datos_basket <- paste (										
  "# Estos son datos de ejemplo en formato basket",
  "Pan, Leche, Carne, Pescado",
  "Pan, Leche, Pescado",
  "Pan, Leche",
  "Leche, Carne, Pescado",
  "Leche, Carne",
  "Carne, Pescado",
  "Leche, Pescado",
  sep="\n")
cat (datos_basket)
write (datos_basket, file = "demo_basket")
# Volcamos los datos a un contenedor apropiado:
trans_demo_basket <- read.transactions (
  "demo_basket",											# fichero de origen
  format = "basket",										# en formato "basket": cada línea una cesta
  sep=",",													# separador de items
  skip = 1)													# saltamos la primera línea
#
inspect (trans_demo_basket)									# examinamos el contenido

# 0.2.4. HACEMOS LIMPIEZA
unlink ("demo_single1")
unlink ("demo_single2")
unlink ("demo_basket")


#=======================================#
# 1. ANÁLISIS DESCRIPTIVO DEL EJEMPLO 1 #
#=======================================#

size (trans_demo_single1)									# vector conteniendo el tamaño de cada trasnsacción
inspect (trans_demo_single1)								# Echamos una ojeada a las transacciones

# 1.1. FRECUENCIAS DE LOS ITEMS
#==============================

frec_items_single1 <- itemFrequency (						# obtenemos la frecuencia de cada item
  x = trans_demo_single1,
  type = "relative")										# por defecto. También "absolute"
head (sort (frec_items_single1, decreasing = TRUE))			# de forma ordenada
# Gráfico con los items más frecuentes:
itemFrequencyPlot (trans_demo_single1, topN = 10, type = "absolute", main = "Items más frecuentes")

# 1.2. ITEMS & ITEMSETS FRECUENTES CON EL ALGORITMO Apriori
#==========================================================

frequentItems_single1_apri <- apriori (						# Aplicamos el algoritmo Apriori
  data = trans_demo_single1,								# Tomamos el dataset de transacciones
  parameter = list (										# Definimos parámetros del algoritmo: existe una amplia variedad de otros parámetros
    support = 3 / dim(trans_demo_single1)[1],				# Definimos como soporte mínimo el que el item haya sido adquirido al menos 3 veces
	minlen = 1,												# Número mínimo de items que debe tener un itemset para ser incluido en los resultados. Por defecto 1
	maxlen = 20,											# Número máximo de items que puede tener un itemset para ser incluido en los resultados. Por defecto 10
	target = "frequent itemset"))							# Resultados. Otros: "rules" (por defecto), "maximally frequent itemsets", "closed frequent itemsets" o “hyperedgesets”
inspect (frequentItems_single1_apri)						# Inspeccionamos el resultado
summary (frequentItems_single1_apri)						# Inspeccionamos el resultado

# 1.3. ITEMS & ITEMSETS FRECUENTES CON EL ALGORITMO Eclat
#========================================================

frequentItems_single1_eclat <- eclat (trans_demo_single1,	# items & itemsets con un soporte igual o mayor al indicado
  parameter = list (										# Definimos parámetros del algoritmo
    supp = 3 / 7,											# Soporte mínimo
	maxlen = 15))											# Número máximo de items que puede tener un itemset para ser incluido en los resultados. Por defecto 10
inspect (frequentItems_single1_eclat)						# Inspeccionamos el resultado
summary (frequentItems_single1_eclat)						# Inspeccionamos el resultado


#=================================================================================#
# 2. ANÁLISIS DESCRIPTIVO DEL DATASET "Groceries" INCLUIDO EN EL PAQUETE {arules} #
#=================================================================================#

data ("Groceries")											# incorporamos los datos
class (Groceries)											# comprobamos que es un dataset de transacciones
#
size (Groceries)											# vector conteniendo el tamaño de cada trasnsacción
inspect (Groceries [1:5])									# Echamos una ojeada a las primeras transacciones

# 2.1. FRECUENCIAS DE LOS ITEMS
#==============================

frec_items_groce <- itemFrequency (							# obtenemos la frecuencia de cada item
  x = Groceries,
  type = "relative")										# por defecto. También "absolute"
head (sort (frec_items_groce, decreasing = TRUE))			# de forma ordenada
# Gráfico con los items más frecuentes:
itemFrequencyPlot (Groceries, topN = 10, type = "absolute", main = "Items más frecuentes")

# 2.2. ITEMS & ITEMSETS FRECUENTES CON EL ALGORITMO Apriori
#==========================================================

frequentItems_groce_apri <- apriori (data = Groceries,		# Tomamos el dataset de transacciones
  parameter = list (										# Definimos parámetros del algoritmo: existe una amplia variedad de otros parámetros
    support = 30 / dim (Groceries)[1],						# Definimos como soporte mínimo el que el item haya sido adquirido al menos 30 veces
	minlen = 1,												# Número mínimo de items que debe tener un itemset para ser incluido en los resultados. Por defecto 1
	maxlen = 20,											# Número máximo de items que puede tener un itemset para ser incluido en los resultados. Por defecto 10
	target = "frequent itemset"))							# Resultados. Otros: "rules" (por defecto), "maximally frequent itemsets", "closed frequent itemsets" o “hyperedgesets”
inspect (frequentItems_groce_apri [1:5])					# Inspeccionamos el resultado
summary (frequentItems_groce_apri)							# Inspeccionamos el resultado

# 2.3. ITEMS & ITEMSETS FRECUENTES CON EL ALGORITMO Eclat
#========================================================

frequentItems_groce_eclat <- eclat (Groceries,				# items & itemsets con un soporte igual o mayor al indicado
  parameter = list (										# Definimos parámetros del algoritmo: existe una amplia variedad de otros parámetros
    supp = 0.1,												# Soporte mínimo
	maxlen = 15))											# Número máximo de items que puede tener un itemset para ser incluido en los resultados. Por defecto 10
inspect (frequentItems_groce_eclat)							# Inspeccionamos el resultado
summary (frequentItems_groce_eclat)							# Inspeccionamos el resultado


#==========================#
# 3. SELECCIÓN DE ITEMSETS #
#==========================#

# Utilizamos el dataset de los itemsets más frecuentes de Groceries, habiendo usado el algoritmo Apriori, aunque podemos utilizar directamente el dataset Groceries

# 3.1. ITEMSETS QUE CONTIENEN UN ITEM DETERMINADO
#================================================

sub1_groce_apri <- arules::subset (
  frequentItems_groce_apri,
  subset = items %in% "tropical fruit")						# Todos los itemsets que incluyan "tropical fruit"
sub1_groce_apri												# Muestra el número de itemsets seleccionados
inspect (sub1_groce_apri[1:10])								# Muestra los 10 primeros itemsets seleccionados

# 3.2. ITEMSETS QUE CONTIENEN DOS ITEMS DETERMINADOS
#===================================================

sub2_groce_apri <- arules::subset (
  frequentItems_groce_apri,
  subset = items %ain% c("tropical fruit", "whole milk"))	# Todos los itemsets que incluyan "tropical fruit" y "whole milk"
sub2_groce_apri												# Muestra el número de itemsets seleccionados
inspect (sub2_groce_apri[1:10])								# Muestra los 10 primeros itemsets seleccionados

# Selectores posibles:
# %in%:		itemsets que incluyen cualquiera de los items determinados
# %pin%:	itemsets que incluye el item determinado
# %ain%:	itemsets que incluyen todos los items determinados
# %oin%:	itemsets que incluyen sólo los items determinados


#==========================================#
# 4. REGLAS DE ASOCIACIÓN CON EL EJEMPLO 1 #
#==========================================#

# 4.1. CREACIÓN DE LAS REGLAS DE ASOCIACIÓN
#==========================================

reglas_asoc_single1 <- apriori (
  data = trans_demo_single1,								# Tommos el dataset de transacciones
  parameter = list (										# Definimos parámetros del algoritmo
    support = 3 / dim (trans_demo_single1) [1],				# Definimos como soporte mínimo el que el item haya sido adquirido al menos 3 veces
	minlen = 2,												# Número mínimo de items que debe tener un itemset para ser incluido en los resultados. Por defecto 1
	maxlen = 20,											# Número máximo de items que puede tener un itemset para ser incluido en los resultados. Por defecto 10
	target = "rules",										# por defecto
    confidence = 0.7))										# Confianza mínima que debe de tener una regla de asociación para ser incluida en los resultados. Por defecto 0.8
#
summary (reglas_asoc_single1)								# Vemos lo que tenemos
inspect (reglas_asoc_single1)								# Vemos lo que tenemos

# 4.2. IDENTIFICACIÓN DE LAS REGLAS PRINCIPALES
#==============================================

inspect (sort (												# Ordenamos..
  x = reglas_asoc_single1,									# ..las reglas de asociación..
  decreasing = TRUE,										# ..en orden decreciente..
  by = "confidence"))										# ..de confianza
#
inspect (sort (												# Ordenamos..
  x = reglas_asoc_single1,									# ..las reglas de asociación..
  decreasing = TRUE,										# ..en orden decreciente..
  by = "lift"))												# ..de "lift"


#====================================================================================#
# 5. REGLAS DE ASOCIACIÓN CON EL DATASET "Groceries" INCLUIDO EN EL PAQUETE {arules} #
#====================================================================================#

# 5.1. CREACIÓN DE LAS REGLAS DE ASOCIACIÓN
#==========================================

reglas_groceries <- apriori (
  data = Groceries,											# Tommos el dataset de transacciones
  parameter = list (										# Definimos parámetros del algoritmo
    support = soporte <- 30 / dim (Groceries) [1],			# Soporte mínimo
	minlen = 1,												# Número mínimo de items que debe tener un itemset para ser incluido en los resultados. Por defecto 1
	maxlen = 20,											# Número máximo de items que puede tener un itemset para ser incluido en los resultados. Por defecto 10
	target = "rules",										# por defecto
    confidence = 0.7))										# Confianza mínima que debe de tener una regla de asociación para ser incluida en los resultados. Por defecto 0.8
#
summary (reglas_groceries)									# Vemos lo que tenemos
inspect (reglas_groceries)									# Vemos lo que tenemos

# 5.2. IDENTIFICACIÓN DE LAS REGLAS PRINCIPALES
#==============================================

inspect (sort (												# Ordenamos..
  x = reglas_groceries,										# ..las reglas de asociación..
  decreasing = TRUE,										# ..en orden decreciente..
  by = "confidence"))										# ..de confianza
#
inspect (sort (												# Ordenamos..
  x = reglas_groceries,										# ..las reglas de asociación..
  decreasing = TRUE,										# ..en orden decreciente..
  by = "lift"))												# ..de "lift"

# 5.3. ELIMINACIÓN DE REGLAS REDUNDANTES
#=======================================

# A veces es conveniente excluir las reglas que son un "subset" de reglas más grandes, con el siguiente código:
subset_reglas_groceries <- which (colSums (is.subset (reglas_groceries, reglas_groceries)) > 1) # obtenemos las reglas redundantes
length (subset_reglas_groceries)																# Se identifican en este caso 3
reglas_groceries_depuradas <- reglas_groceries [-subset_reglas_groceries]						# Excluimos las reglas redundantes

# 5.4. REGLAS RELACIONADAS CON ITEMS DETERMINADOS
#================================================

# Modificamos los contenidos del atributo "appearance" en la función apriori()
#
# 5.4.1. PARA IDENTIFICAR LOS ITEMS QUE INFLUYEN EN LA COMPRA DEL ITEM X
# Qué han comprado los clientes que les ha hecho comprar también, por ejemplo leche entera ("whole milk")
reglas_para_leche_entera <- subset (
  reglas_groceries,
  subset = rhs %pin% "whole milk")
#
inspect (sort (												# Ordenamos..
  x = reglas_para_leche_entera,								# ..las reglas de asociación..
  decreasing = TRUE,										# ..en orden decreciente..
  by = "confidence"))										# ..de confianza
#
# 5.4.2. PARA IDENTIFICAR QUÉ ITEMS ESTÁN ASOCIADOS A LA COMPRA DEL ITEM X
# Los clientes que compraron leche entera ("whole milk") también compraron...
reglas_con_leche_entera <- subset (
  reglas_groceries,
  subset = lhs %pin% "whole milk")
#
inspect (sort (												# Ordenamos..
  x = reglas_con_leche_entera,								# ..las reglas de asociación..
  decreasing = TRUE,										# ..en orden decreciente..
  by = "confidence"))										# ..de confianza

# 5.5. TRANSACCIONES QUE CUMPLEN UNA DETERMINADA REGLA
#=====================================================

# Identificamos, por ejemplo, la regla con un valor de confianza más alto:
head (arrange (as (reglas_groceries, "data.frame"), desc (confidence)), 1)$rules
# Puede verse que contiene los items: "citrus fruit", "root vegetables", "tropical fruit", "whole milk" y "other vegetables"
# Identificamos las transacciones que contienen estos items:
mis_transacciones <- subset (
  x = Groceries,
  subset = items %ain% c ("citrus fruit", "root vegetables", "tropical fruit", "whole milk", "other vegetables"))
mis_transacciones											# Vemos lo que tenemos
View (mis_transacciones)									# Vemos lo que tenemos