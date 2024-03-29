##############################################################################
#####     XI JORNADAS DE USUARIOS DE R. MADRID, 14-16 NOVIEMBRE 2019     #####
#####     TALLER HERRAMIENTAS DE R PARA LA INVESTIGACI�N DE MERCADOS     #####
#####        3. MARKET BASKET ANALYSIS & ASSOCIATION RULES CON R         #####
#####      AN�LISIS DE LA CESTA DE LA COMPRA & REGLAS DE ASOCIACI�N      #####
#####                      POR Jos� Ignacio Casas                        #####
##############################################################################
# 0. INSTALACI�N DE PAQUETES Y DATASETS
# 0.1. PAQUETES
# 0.2. FORMATOS DE DATOS DE PARTIDA
# 0.2.1. FORMATO DE TABLA LARGA O SINGLE. EJEMPLO 1
# 0.2.2. FORMATO DE TABLA LARGA O SINGLE. EJEMPLO 2
# 0.2.3. FORMATO DE CESTA O BASKET
# 0.2.4. HACEMOS LIMPIEZA
# 1. AN�LISIS DESCRIPTIVO DEL EJEMPLO 1
# 1.1. FRECUENCIAS DE LOS ITEMS
# 1.2. ITEMS & ITEMSETS FRECUENTES CON EL ALGORITMO Apriori
# 1.3. ITEMS & ITEMSETS FRECUENTES CON EL ALGORITMO Eclat
# 2. AN�LISIS DESCRIPTIVO DEL DATASET "Groceries" INCLUIDO EN EL PAQUETE {arules}
# 2.1. FRECUENCIAS DE LOS ITEMS
# 2.2. ITEMS & ITEMSETS FRECUENTES CON EL ALGORITMO Apriori
# 2.3. ITEMS & ITEMSETS FRECUENTES CON EL ALGORITMO Eclat
# 3. SELECCI�N DE ITEMSETS
# 3.1. ITEMSETS QUE CONTIENEN UN ITEM DETERMINADO
# 3.2. ITEMSETS QUE CONTIENEN DOS ITEMS DETERMINADOS
# 4. REGLAS DE ASOCIACI�N CON EL EJEMPLO 1
# 4.1. CREACI�N DE LAS REGLAS DE ASOCIACI�N
# 4.2. IDENTIFICACI�N DE LAS REGLAS PRINCIPALES
# 5. REGLAS DE ASOCIACI�N CON EL DATASET "Groceries" INCLUIDO EN EL PAQUETE {arules}
# 5.1. CREACI�N DE LAS REGLAS DE ASOCIACI�N
# 5.2. IDENTIFICACI�N DE LAS REGLAS PRINCIPALES
# 5.3. ELIMINACI�N DE REGLAS REDUNDANTES
# 5.4. REGLAS RELACIONADAS CON ITEMS DETERMINADOS
# 5.4.1. PARA IDENTIFICAR LOS ITEMS QUE INFLUYEN EN LA COMPRA DEL ITEM X
# 5.4.2. PARA IDENTIFICAR QU� ITEMS EST�N ASOCIADOS A LA COMPRA DEL ITEM X
# 5.5. TRANSACCIONES QUE CUMPLEN UNA DETERMINADA REGLA


#=======================================#
# 0. INSTALACI�N DE PAQUETES Y DATASETS #
#=======================================#

# 0.1. PAQUETES
#==============

library (arules)

# 0.2. FORMATOS DE DATOS DE PARTIDA
#==================================

# 0.2.1. FORMATO DE TABLA LARGA O SINGLE. EJEMPLO 1
# Cada l�nea contiene un item y el identificador de la transacci�n (compra) a la que pertenece
# Creamos un dataset de ejemplo en formato single
# La columna 1 contiene la id de la transacci�n y la olumna 2 el item
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
colnames (trans_demo_single1)								# los items han sido ordenados alfab�ticamente
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
# Cada fila contiene todos los items que forman parte de una misma transacci�n
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
  format = "basket",										# en formato "basket": cada l�nea una cesta
  sep=",",													# separador de items
  skip = 1)													# saltamos la primera l�nea
#
inspect (trans_demo_basket)									# examinamos el contenido

# 0.2.4. HACEMOS LIMPIEZA
unlink ("demo_single1")
unlink ("demo_single2")
unlink ("demo_basket")


#=======================================#
# 1. AN�LISIS DESCRIPTIVO DEL EJEMPLO 1 #
#=======================================#

size (trans_demo_single1)									# vector conteniendo el tama�o de cada trasnsacci�n
inspect (trans_demo_single1)								# Echamos una ojeada a las transacciones

# 1.1. FRECUENCIAS DE LOS ITEMS
#==============================

frec_items_single1 <- itemFrequency (						# obtenemos la frecuencia de cada item
  x = trans_demo_single1,
  type = "relative")										# por defecto. Tambi�n "absolute"
head (sort (frec_items_single1, decreasing = TRUE))			# de forma ordenada
# Gr�fico con los items m�s frecuentes:
itemFrequencyPlot (trans_demo_single1, topN = 10, type = "absolute", main = "Items m�s frecuentes")

# 1.2. ITEMS & ITEMSETS FRECUENTES CON EL ALGORITMO Apriori
#==========================================================

frequentItems_single1_apri <- apriori (						# Aplicamos el algoritmo Apriori
  data = trans_demo_single1,								# Tomamos el dataset de transacciones
  parameter = list (										# Definimos par�metros del algoritmo: existe una amplia variedad de otros par�metros
    support = 3 / dim(trans_demo_single1)[1],				# Definimos como soporte m�nimo el que el item haya sido adquirido al menos 3 veces
	minlen = 1,												# N�mero m�nimo de items que debe tener un itemset para ser incluido en los resultados. Por defecto 1
	maxlen = 20,											# N�mero m�ximo de items que puede tener un itemset para ser incluido en los resultados. Por defecto 10
	target = "frequent itemset"))							# Resultados. Otros: "rules" (por defecto), "maximally frequent itemsets", "closed frequent itemsets" o �hyperedgesets�
inspect (frequentItems_single1_apri)						# Inspeccionamos el resultado
summary (frequentItems_single1_apri)						# Inspeccionamos el resultado

# 1.3. ITEMS & ITEMSETS FRECUENTES CON EL ALGORITMO Eclat
#========================================================

frequentItems_single1_eclat <- eclat (trans_demo_single1,	# items & itemsets con un soporte igual o mayor al indicado
  parameter = list (										# Definimos par�metros del algoritmo
    supp = 3 / 7,											# Soporte m�nimo
	maxlen = 15))											# N�mero m�ximo de items que puede tener un itemset para ser incluido en los resultados. Por defecto 10
inspect (frequentItems_single1_eclat)						# Inspeccionamos el resultado
summary (frequentItems_single1_eclat)						# Inspeccionamos el resultado


#=================================================================================#
# 2. AN�LISIS DESCRIPTIVO DEL DATASET "Groceries" INCLUIDO EN EL PAQUETE {arules} #
#=================================================================================#

data ("Groceries")											# incorporamos los datos
class (Groceries)											# comprobamos que es un dataset de transacciones
#
size (Groceries)											# vector conteniendo el tama�o de cada trasnsacci�n
inspect (Groceries [1:5])									# Echamos una ojeada a las primeras transacciones

# 2.1. FRECUENCIAS DE LOS ITEMS
#==============================

frec_items_groce <- itemFrequency (							# obtenemos la frecuencia de cada item
  x = Groceries,
  type = "relative")										# por defecto. Tambi�n "absolute"
head (sort (frec_items_groce, decreasing = TRUE))			# de forma ordenada
# Gr�fico con los items m�s frecuentes:
itemFrequencyPlot (Groceries, topN = 10, type = "absolute", main = "Items m�s frecuentes")

# 2.2. ITEMS & ITEMSETS FRECUENTES CON EL ALGORITMO Apriori
#==========================================================

frequentItems_groce_apri <- apriori (data = Groceries,		# Tomamos el dataset de transacciones
  parameter = list (										# Definimos par�metros del algoritmo: existe una amplia variedad de otros par�metros
    support = 30 / dim (Groceries)[1],						# Definimos como soporte m�nimo el que el item haya sido adquirido al menos 30 veces
	minlen = 1,												# N�mero m�nimo de items que debe tener un itemset para ser incluido en los resultados. Por defecto 1
	maxlen = 20,											# N�mero m�ximo de items que puede tener un itemset para ser incluido en los resultados. Por defecto 10
	target = "frequent itemset"))							# Resultados. Otros: "rules" (por defecto), "maximally frequent itemsets", "closed frequent itemsets" o �hyperedgesets�
inspect (frequentItems_groce_apri [1:5])					# Inspeccionamos el resultado
summary (frequentItems_groce_apri)							# Inspeccionamos el resultado

# 2.3. ITEMS & ITEMSETS FRECUENTES CON EL ALGORITMO Eclat
#========================================================

frequentItems_groce_eclat <- eclat (Groceries,				# items & itemsets con un soporte igual o mayor al indicado
  parameter = list (										# Definimos par�metros del algoritmo: existe una amplia variedad de otros par�metros
    supp = 0.1,												# Soporte m�nimo
	maxlen = 15))											# N�mero m�ximo de items que puede tener un itemset para ser incluido en los resultados. Por defecto 10
inspect (frequentItems_groce_eclat)							# Inspeccionamos el resultado
summary (frequentItems_groce_eclat)							# Inspeccionamos el resultado


#==========================#
# 3. SELECCI�N DE ITEMSETS #
#==========================#

# Utilizamos el dataset de los itemsets m�s frecuentes de Groceries, habiendo usado el algoritmo Apriori, aunque podemos utilizar directamente el dataset Groceries

# 3.1. ITEMSETS QUE CONTIENEN UN ITEM DETERMINADO
#================================================

sub1_groce_apri <- arules::subset (
  frequentItems_groce_apri,
  subset = items %in% "tropical fruit")						# Todos los itemsets que incluyan "tropical fruit"
sub1_groce_apri												# Muestra el n�mero de itemsets seleccionados
inspect (sub1_groce_apri[1:10])								# Muestra los 10 primeros itemsets seleccionados

# 3.2. ITEMSETS QUE CONTIENEN DOS ITEMS DETERMINADOS
#===================================================

sub2_groce_apri <- arules::subset (
  frequentItems_groce_apri,
  subset = items %ain% c("tropical fruit", "whole milk"))	# Todos los itemsets que incluyan "tropical fruit" y "whole milk"
sub2_groce_apri												# Muestra el n�mero de itemsets seleccionados
inspect (sub2_groce_apri[1:10])								# Muestra los 10 primeros itemsets seleccionados

# Selectores posibles:
# %in%:		itemsets que incluyen cualquiera de los items determinados
# %pin%:	itemsets que incluye el item determinado
# %ain%:	itemsets que incluyen todos los items determinados
# %oin%:	itemsets que incluyen s�lo los items determinados


#==========================================#
# 4. REGLAS DE ASOCIACI�N CON EL EJEMPLO 1 #
#==========================================#

# 4.1. CREACI�N DE LAS REGLAS DE ASOCIACI�N
#==========================================

reglas_asoc_single1 <- apriori (
  data = trans_demo_single1,								# Tommos el dataset de transacciones
  parameter = list (										# Definimos par�metros del algoritmo
    support = 3 / dim (trans_demo_single1) [1],				# Definimos como soporte m�nimo el que el item haya sido adquirido al menos 3 veces
	minlen = 2,												# N�mero m�nimo de items que debe tener un itemset para ser incluido en los resultados. Por defecto 1
	maxlen = 20,											# N�mero m�ximo de items que puede tener un itemset para ser incluido en los resultados. Por defecto 10
	target = "rules",										# por defecto
    confidence = 0.7))										# Confianza m�nima que debe de tener una regla de asociaci�n para ser incluida en los resultados. Por defecto 0.8
#
summary (reglas_asoc_single1)								# Vemos lo que tenemos
inspect (reglas_asoc_single1)								# Vemos lo que tenemos

# 4.2. IDENTIFICACI�N DE LAS REGLAS PRINCIPALES
#==============================================

inspect (sort (												# Ordenamos..
  x = reglas_asoc_single1,									# ..las reglas de asociaci�n..
  decreasing = TRUE,										# ..en orden decreciente..
  by = "confidence"))										# ..de confianza
#
inspect (sort (												# Ordenamos..
  x = reglas_asoc_single1,									# ..las reglas de asociaci�n..
  decreasing = TRUE,										# ..en orden decreciente..
  by = "lift"))												# ..de "lift"


#====================================================================================#
# 5. REGLAS DE ASOCIACI�N CON EL DATASET "Groceries" INCLUIDO EN EL PAQUETE {arules} #
#====================================================================================#

# 5.1. CREACI�N DE LAS REGLAS DE ASOCIACI�N
#==========================================

reglas_groceries <- apriori (
  data = Groceries,											# Tommos el dataset de transacciones
  parameter = list (										# Definimos par�metros del algoritmo
    support = soporte <- 30 / dim (Groceries) [1],			# Soporte m�nimo
	minlen = 1,												# N�mero m�nimo de items que debe tener un itemset para ser incluido en los resultados. Por defecto 1
	maxlen = 20,											# N�mero m�ximo de items que puede tener un itemset para ser incluido en los resultados. Por defecto 10
	target = "rules",										# por defecto
    confidence = 0.7))										# Confianza m�nima que debe de tener una regla de asociaci�n para ser incluida en los resultados. Por defecto 0.8
#
summary (reglas_groceries)									# Vemos lo que tenemos
inspect (reglas_groceries)									# Vemos lo que tenemos

# 5.2. IDENTIFICACI�N DE LAS REGLAS PRINCIPALES
#==============================================

inspect (sort (												# Ordenamos..
  x = reglas_groceries,										# ..las reglas de asociaci�n..
  decreasing = TRUE,										# ..en orden decreciente..
  by = "confidence"))										# ..de confianza
#
inspect (sort (												# Ordenamos..
  x = reglas_groceries,										# ..las reglas de asociaci�n..
  decreasing = TRUE,										# ..en orden decreciente..
  by = "lift"))												# ..de "lift"

# 5.3. ELIMINACI�N DE REGLAS REDUNDANTES
#=======================================

# A veces es conveniente excluir las reglas que son un "subset" de reglas m�s grandes, con el siguiente c�digo:
subset_reglas_groceries <- which (colSums (is.subset (reglas_groceries, reglas_groceries)) > 1) # obtenemos las reglas redundantes
length (subset_reglas_groceries)																# Se identifican en este caso 3
reglas_groceries_depuradas <- reglas_groceries [-subset_reglas_groceries]						# Excluimos las reglas redundantes

# 5.4. REGLAS RELACIONADAS CON ITEMS DETERMINADOS
#================================================

# Modificamos los contenidos del atributo "appearance" en la funci�n apriori()
#
# 5.4.1. PARA IDENTIFICAR LOS ITEMS QUE INFLUYEN EN LA COMPRA DEL ITEM X
# Qu� han comprado los clientes que les ha hecho comprar tambi�n, por ejemplo leche entera ("whole milk")
reglas_para_leche_entera <- subset (
  reglas_groceries,
  subset = rhs %pin% "whole milk")
#
inspect (sort (												# Ordenamos..
  x = reglas_para_leche_entera,								# ..las reglas de asociaci�n..
  decreasing = TRUE,										# ..en orden decreciente..
  by = "confidence"))										# ..de confianza
#
# 5.4.2. PARA IDENTIFICAR QU� ITEMS EST�N ASOCIADOS A LA COMPRA DEL ITEM X
# Los clientes que compraron leche entera ("whole milk") tambi�n compraron...
reglas_con_leche_entera <- subset (
  reglas_groceries,
  subset = lhs %pin% "whole milk")
#
inspect (sort (												# Ordenamos..
  x = reglas_con_leche_entera,								# ..las reglas de asociaci�n..
  decreasing = TRUE,										# ..en orden decreciente..
  by = "confidence"))										# ..de confianza

# 5.5. TRANSACCIONES QUE CUMPLEN UNA DETERMINADA REGLA
#=====================================================

# Identificamos, por ejemplo, la regla con un valor de confianza m�s alto:
head (arrange (as (reglas_groceries, "data.frame"), desc (confidence)), 1)$rules
# Puede verse que contiene los items: "citrus fruit", "root vegetables", "tropical fruit", "whole milk" y "other vegetables"
# Identificamos las transacciones que contienen estos items:
mis_transacciones <- subset (
  x = Groceries,
  subset = items %ain% c ("citrus fruit", "root vegetables", "tropical fruit", "whole milk", "other vegetables"))
mis_transacciones											# Vemos lo que tenemos
View (mis_transacciones)									# Vemos lo que tenemos