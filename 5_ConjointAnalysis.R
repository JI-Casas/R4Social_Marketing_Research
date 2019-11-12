##############################################################################
#####     XI JORNADAS DE USUARIOS DE R. MADRID, 14-16 NOVIEMBRE 2019     #####
#####     TALLER HERRAMIENTAS DE R PARA LA INVESTIGACIÓN DE MERCADOS     #####
#####                       5. CONJOINT ANALYSIS                         #####
#####                      POR José Ignacio Casas                        #####
##############################################################################
# 0. CARGA DE PAQUETES
# 1. CREACIÓN DE "TARJETAS" (SETS
# 1.1. EJEMPLO FICTICIO PRIMERO SOBRE AUTOMÓVILES. DESCRIPCIÓN
# 1.2. CREACIÓN DE UN DISEÑO FACTORIAL COMPLETO
# 1.3. CREACIÓN DE UN DISEÑO FACTORIAL FRACCIONAL
# 1.3.1. CREACIÓN DE UN PRIMER DISEÑO FACTORIAL FRACCIONAL
# 1.3.2. CREACIÓN DE UN SEGUNDO DISEÑO FACTORIAL FRACCIONAL
# 1.4. CREACIÓN DE LOS SETS
# 2. CHOICE-BASED CONJOINT (CBC) ANALYSIS
# 2.1. EJEMPLO FICTICIO SEGUNDO SOBRE MINIVANS. DESCRIPCIÓN
# 2.2. TRANSFORMACIÓN DE LOS DATOS A UN FORMATO ESPECIAL PARA LA CREACIÓN DEL MODELO
# 2.3. ESTIMACIÓN DEL MODELO 1 CON mlogit()
# 2.4. ANÁLISIS DE RESULTADOS
# 2.5. GENERACIÓN DEL MODELO 2 Y COMPARACIÓN
# 2.6. GENERACIÓN DEL MODELO 3: EL PRECIO COMO NUMÉRICO
# 3. WILLINGNESS-TO-PAY
# 4. SIMULACIÓN DE CUOTAS DE MERCADO
# 5. GRÁFICOS DE SENSIBILIDAD


#======================#
# 0. CARGA DE PAQUETES #
#======================#

library (AlgDesign)											# Paquete necesario para la creación de tarjetas (SETS)
library (mlogit)											# Paquete necesario para el CBC Analysis


#==================================#
# 1. CREACIÓN DE "TARJETAS" (SETS) #
#==================================#

# 1.1. EJEMPLO FICTICIO PRIMERO SOBRE AUTOMÓVILES. DESCRIPCIÓN
#=============================================================

# Se analizan las preferencias de los consumidores respecto a algunos atributos de tipos de automóvil
# Cada tipo de automóvil tiene tres atributos:
# --AUT: el automóvil tiene cambio automático. Niveles: 1 = NO; 2 = SI
# --ELE: el automóvil cuenta con motor eléctrico. Niveles: 1 = NO; 2 = SI
# --PRE: precio del automóvil en miles de euros. Niveles: 1 = 20; 2 = 30; 3 = 40; 4 = 50
# El tipo de automóvil se supone igual para los demás atributos no reseñados

# 1.2. CREACIÓN DE UN DISEÑO FACTORIAL COMPLETO
#==============================================

ffd <- gen.factorial (										# Generación del diseño factorial completo: 2 x 2 x 4 = 16 perfiles
  c (2, 2, 4),												# Número de niveles de cada atributo
  varNames = c ("AUT", "ELE", "PRE"),						# Nombres de los atributos
  factors = "all"											# Indica que todos los atributos son "factor"
  )

# 1.3. CREACIÓN DE UN DISEÑO FACTORIAL FRACCIONAL
#================================================

# 1.3.1. CREACIÓN DE UN PRIMER DISEÑO FACTORIAL FRACCIONAL
set.seed (54321)											# Partimos de una generación aleatoria
des <- optFederov (											# Genera un diseño factorial fraccional a partir de uno completo
  ~.,														# Todas las variables de datos se usan linearmente y se usan sus nombres
  ffd,														# El diseño factorial completo de partida
  8															# Número de alternativas a generar
  )
alt1 <- des$design											# Asignamos el diseño factorial fraccional a un primer objeto "alt1"
#
# 1.3.2. CREACIÓN DE UN SEGUNDO DISEÑO FACTORIAL FRACCIONAL
alt2 <- alt1												# Creamos una nueva copia igual

# 1.4. CREACIÓN DE LOS SETS
#==========================

# Seleccionamos aleatoriamente filas de los diseños factoriales fraccionales sin reemplazamiento añadiendo a cada diseño una nueva variable aleatoria:
alt1 <- transform (alt1, r1 = runif (8))					# Incorporamos a "alt1" una nueva variable aleatoria con ocho valores (tantas como filas tiene "alt1")
alt2 <- transform (alt2, r2 = runif (8))					# Hacemos lo mismo con "alt2"

alt1_sort <- alt1 [order (alt1$r1), ]						# Ordenamos ambos diseños según la nueva variable aleatoria
alt2_sort <- alt2 [order (alt2$r2), ]						# Idem
# Creamos las parejas de alternativas que preseantaremos a los encuestados, ligando por su orden "alt1_sort" y "alt2_sort":
cbind (alt1_sort, alt2_sort)								# Vemos cómo se emparejan
# SI EN ALGÚN CASO APARECE LA MISMA ALTERNATIVA DOS VECES EN UN MISMO SET, SE REPITE TODO ESTE PROCESO 1.4.


#=========================================#
# 2. CHOICE-BASED CONJOINT (CBC) ANALYSIS #
#=========================================#

# 2.1. EJEMPLO FICTICIO SEGUNDO SOBRE MINIVANS. DESCRIPCIÓN
#==========================================================
# Basado en el cap 13 de C. Chapman y E.M. Feit, R For Marketing Research and Analytics (Second Edition) Springer 2019

cbc.df <- read.csv ("http://goo.gl/5xQObB",					# Volcamos el fichero utilizado en la publicación reseñada
  colClasses = c (seat = "factor", price = "factor"))		# Convertimos esas variables en factor
summary (cbc.df)											# Idem
str (cbc.df)												# Idem
head (cbc.df)												# Idem
View (cbc.df)												# Idem
#
# CONTENIDO DEL DATASET (formato "long"):
# --resp.id:	identificador correlativo del respondients (int)
# --ques:		identificador de la pregunta (SET de PERFILES) sobre el que se pronuncian los respondientes (int)
# --alt:		identificador de la alternativa (PERFIL) dentro de la pregunta (SET) (int)
# --carpool:	¿Piensa utilizar el minivan de forma compartida? Factor con 2 levels: no (1), yes (2)
# --seat:		Número de asientos del minivan. Factor con 3 levels: "6" (1),"7" (2), "8"(3)
# --cargo:		Capacidad de carga, en pies de profundidad del maletero. Factor con 2 levels: "2ft" (1), "3ft" (2)
# --eng:		Motor. Factor con 3 levels: "elec" (1),"gas" (2),"hyb" (3)
# --price:		Precio, en miles. Factor con 3 levels: "30" (1),"35" (2),"40" (3)
# --choice:		Alternativa (PERFIL) escogida (1) o no escogida (0)
#
# 200 respondientes han elegido una de las tres alternativas (PERFILES) posibles en cada una de las 15 preguntas (SETS)

# 2.2. TRANSFORMACIÓN DE LOS DATOS A UN FORMATO ESPECIAL PARA LA CREACIÓN DEL MODELO
#===================================================================================

cbc.mlogit <- mlogit.data (									# Función utilizada
data = cbc.df,												# Datos de partida
choice = "choice",											# Columna con los datos de respuesta
shape = "long",												# Datos presentados en formato "long" (por defecto) en vez de "wide"
varying = 5:8,												# Columnas con los atributos
alt.levels = paste ("pos", 1:3),							# Nombramos las alternativas
id.var = "resp.id")											# Columna de los IDs de los entrevistados
# Obtenemos un dataset class "mlogit.data" y "data.frame"

# 2.3. ESTIMACIÓN DEL MODELO 1 CON mlogit()
#==========================================

m1 <- mlogit (choice ~ 0 +									# Con "0 +" no queremos que aparezca los (dos) "intercept", que expresan la posición que ocupan en la presentación del "choice"
  seat + cargo + eng + price,								# Atributos incluidos en el modelo
  data = cbc.mlogit)										# dataset de origen

# 2.4. ANÁLISIS DE RESULTADOS
#============================

summary (m1)												# Inspeccionamos los resultados
# --"Estimate":		recoge los valores medios para cada NIVEL, siempre en relación con el NIVEL "base" de cada ATRIBUTO. Están en escala logística entre -2 y +2
# --"Std. Error":	alude a la precisión del "Estimate", para los datos analizados, junto al test que indica si el coeficiente es significativamente diferente a cero

# 2.5. GENERACIÓN DEL MODELO 2 Y COMPARACIÓN
#===========================================
m2 <- mlogit (choice ~										# Sin excluir los intercept 
  seat + cargo + eng + price,
  data = cbc.mlogit)
#
summary (m2)												# Inspeccionamos los resultados
# Dos nuevos parámetros que indican la preferencia por diferentes posiciones (izquierda,centro, derecha) dentro del SET presentado a los entrevistados
# --pos2:(intercept) indica la preferencia relativa por la segunda posición en comparación con la primera
# --pos3:(intercept) indica la preferencia relativa por la tercera posición en comparación con la primera
# A veces se les llama ALTERNATIVE SPECIFIC CONSTANTS o ASC’s para diferenciarlos del valor en el origen de los modelos lineales
# Normalmente no cabe esperar que sus valores sean significativamente diferentes de cero
#
lrtest (m1, m2)												# Comparamos modelos, siendo uno subgrupo del otro. La diferencia no es relevante: Pr(>Chisq) = 0.7122

# 2.6. GENERACIÓN DEL MODELO 3: EL PRECIO COMO NUMÉRICO
#======================================================

# Consideramos la variable "price" como variable numérica dentro de un nuevo modelo:
m3 <- mlogit (choice ~ 0 +
  seat + cargo + eng +										# Atributos incluidos en el modelo
  as.numeric (as.character (price)),						# "price" convertida en variable numérica
  data = cbc.mlogit)										# dataset de origen
summary (m3)												# Inspeccionamos los resultados
#
lrtest (m1, m3)												# Comparamos modelos, comprobando que son similares


#=======================#
# 3. WILLINGNESS-TO-PAY #
#=======================#

# Con el modelo m3 podemos calcular el willingness-to-pay promedio para un nivel concreto de un atributo
# Dividimos el coeficiente de ese nivel por el coeficiente del precio:
# Usando la función coef() del paquete {stats}:
coef (m3) ["cargo3ft"] / (-coef (m3) ["as.numeric(as.character(price))"] / 1000)
# Usando los propios datos del modelo:
m3$coefficients ["cargo3ft"] / (-m3$coefficients ["as.numeric(as.character(price))"] / 1000)
# En promedio, los clientes estarían equilibradamente repartidos entre un minivan con capacidad de carga de 2 ft, y uno de 3 ft que cueste $2.750,60 más.
# Otra forma es que 2.750,60 dólares es el precio al que los clientes son indiferentes entre las dos opciones de capacidad de carga


#====================================#
# 4. SIMULACIÓN DE CUOTAS DE MERCADO #
#====================================#

# No existe una función para simulaciones utilizando los modelos MNL de mlogit, por lo que usamos una función ad-hoc:
# (tomado de Chapman-Feit, R For Marketing Research and Analytics)
predict.mnl <- function (									# Función para predecir las cuotas de mercado en un modelo MN
  model,													# El modelo estimado con mlogit()
  data)														# data.frame con los diseños a predecir su cuota de mercado; mismo formato que los datos para estimar el modelo
  {
    data.model <- model.matrix (update (model$formula , 0 ~ .), data = data) [ , -1]
    utility <- data.model%*%model$coef
    share <- exp (utility) / sum (exp (utility))
    cbind (share, data)
  }
# Si queremos saber las cuotas de mercado de todos los perfiles uncluidos en la investigación:
perfiles <- unique (cbc.df [ , c(5:8)])						# Primero creamos un data.frame con esos perfiles
perfiles$price <- as.numeric (as.character (perfiles$price))	# Convertimos la variable price a numérica
# Aplicamos la función para predecir las cuotas de cada perfil:
predict.mnl (m3, perfiles)									# crea un data.frame, donde la variable "share" muesta la cuota de cada perfil
# Se trata siempre de cuotas en relación con los otros perfiles incluidos

 
#=============================#
# 5. GRÁFICOS DE SENSIBILIDAD #
#=============================#

# Objetivo: estimar las variaciones de cuota de mercado si cambian los niveles de los atributos, a partir de un perfil determinado
# Creamos una lista en R llamada "attrib" para almacenar los Atributos y sus Niveles:
attrib <- list (seat = c ("6", "7", "8"),
  cargo = c ("2ft", "3ft"),
  eng = c ("gas", "hyb", "elec"),
  price = c("30", "35", "40"))
#
# Usamos una función ad-hoc (tomado de Chapman-Feit, R For Marketing Research and Analytics):
sensitivity.mnl <- function (								# Función para crear los datos para un gráfico de sensibilidad
  model,													# El modelo estimado con mlogit()
  attrib,													# list de vectores con los niveles de los atributos
  base.data,												# data.frame conteniendo el diseño (perfil) base del producto target
  competitor.data)											# data.frame conteniendo el diseño del competitive set
  {
    data <- rbind(base.data , competitor.data)
    base.share <- predict.mnl(model , data)[1,1]
    share <- NULL
    for (a in seq_along(attrib)) {
      for (i in attrib[[a]]) {
        data [1, ] <- base.data
        data [1, a] <- i
        share <- c(share , predict.mnl (model , data) [1, 1])
      }
    }
  data.frame (
    level = unlist (attrib),
	share = share,
	increase = share - base.share)
  }
base.data <- expand.grid(attrib) [c(8), ]					# datos del diseño (perfil) base del producto target
competitor.data <- expand.grid (attrib) [c (1, 3, 41, 49, 26), ]	# datos del diseño del competitive set
(tradeoff <- sensitivity.mnl (m1, attrib, base.data, competitor.data))
#
# Trazamos el gráfico:
barplot (
  tradeoff$increase,
  horiz = FALSE,
  names.arg = tradeoff$level,
  main = "Variaciones de la cuota de mercado para el producto target")