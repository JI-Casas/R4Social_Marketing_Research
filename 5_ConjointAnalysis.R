##############################################################################
#####     XI JORNADAS DE USUARIOS DE R. MADRID, 14-16 NOVIEMBRE 2019     #####
#####     TALLER HERRAMIENTAS DE R PARA LA INVESTIGACI�N DE MERCADOS     #####
#####                       5. CONJOINT ANALYSIS                         #####
#####                      POR Jos� Ignacio Casas                        #####
##############################################################################
# 0. CARGA DE PAQUETES
# 1. CREACI�N DE "TARJETAS" (SETS
# 1.1. EJEMPLO FICTICIO PRIMERO SOBRE AUTOM�VILES. DESCRIPCI�N
# 1.2. CREACI�N DE UN DISE�O FACTORIAL COMPLETO
# 1.3. CREACI�N DE UN DISE�O FACTORIAL FRACCIONAL
# 1.3.1. CREACI�N DE UN PRIMER DISE�O FACTORIAL FRACCIONAL
# 1.3.2. CREACI�N DE UN SEGUNDO DISE�O FACTORIAL FRACCIONAL
# 1.4. CREACI�N DE LOS SETS
# 2. CHOICE-BASED CONJOINT (CBC) ANALYSIS
# 2.1. EJEMPLO FICTICIO SEGUNDO SOBRE MINIVANS. DESCRIPCI�N
# 2.2. TRANSFORMACI�N DE LOS DATOS A UN FORMATO ESPECIAL PARA LA CREACI�N DEL MODELO
# 2.3. ESTIMACI�N DEL MODELO 1 CON mlogit()
# 2.4. AN�LISIS DE RESULTADOS
# 2.5. GENERACI�N DEL MODELO 2 Y COMPARACI�N
# 2.6. GENERACI�N DEL MODELO 3: EL PRECIO COMO NUM�RICO
# 3. WILLINGNESS-TO-PAY
# 4. SIMULACI�N DE CUOTAS DE MERCADO
# 5. GR�FICOS DE SENSIBILIDAD


#======================#
# 0. CARGA DE PAQUETES #
#======================#

library (AlgDesign)											# Paquete necesario para la creaci�n de tarjetas (SETS)
library (mlogit)											# Paquete necesario para el CBC Analysis


#==================================#
# 1. CREACI�N DE "TARJETAS" (SETS) #
#==================================#

# 1.1. EJEMPLO FICTICIO PRIMERO SOBRE AUTOM�VILES. DESCRIPCI�N
#=============================================================

# Se analizan las preferencias de los consumidores respecto a algunos atributos de tipos de autom�vil
# Cada tipo de autom�vil tiene tres atributos:
# --AUT: el autom�vil tiene cambio autom�tico. Niveles: 1 = NO; 2 = SI
# --ELE: el autom�vil cuenta con motor el�ctrico. Niveles: 1 = NO; 2 = SI
# --PRE: precio del autom�vil en miles de euros. Niveles: 1 = 20; 2 = 30; 3 = 40; 4 = 50
# El tipo de autom�vil se supone igual para los dem�s atributos no rese�ados

# 1.2. CREACI�N DE UN DISE�O FACTORIAL COMPLETO
#==============================================

ffd <- gen.factorial (										# Generaci�n del dise�o factorial completo: 2 x 2 x 4 = 16 perfiles
  c (2, 2, 4),												# N�mero de niveles de cada atributo
  varNames = c ("AUT", "ELE", "PRE"),						# Nombres de los atributos
  factors = "all"											# Indica que todos los atributos son "factor"
  )

# 1.3. CREACI�N DE UN DISE�O FACTORIAL FRACCIONAL
#================================================

# 1.3.1. CREACI�N DE UN PRIMER DISE�O FACTORIAL FRACCIONAL
set.seed (54321)											# Partimos de una generaci�n aleatoria
des <- optFederov (											# Genera un dise�o factorial fraccional a partir de uno completo
  ~.,														# Todas las variables de datos se usan linearmente y se usan sus nombres
  ffd,														# El dise�o factorial completo de partida
  8															# N�mero de alternativas a generar
  )
alt1 <- des$design											# Asignamos el dise�o factorial fraccional a un primer objeto "alt1"
#
# 1.3.2. CREACI�N DE UN SEGUNDO DISE�O FACTORIAL FRACCIONAL
alt2 <- alt1												# Creamos una nueva copia igual

# 1.4. CREACI�N DE LOS SETS
#==========================

# Seleccionamos aleatoriamente filas de los dise�os factoriales fraccionales sin reemplazamiento a�adiendo a cada dise�o una nueva variable aleatoria:
alt1 <- transform (alt1, r1 = runif (8))					# Incorporamos a "alt1" una nueva variable aleatoria con ocho valores (tantas como filas tiene "alt1")
alt2 <- transform (alt2, r2 = runif (8))					# Hacemos lo mismo con "alt2"

alt1_sort <- alt1 [order (alt1$r1), ]						# Ordenamos ambos dise�os seg�n la nueva variable aleatoria
alt2_sort <- alt2 [order (alt2$r2), ]						# Idem
# Creamos las parejas de alternativas que preseantaremos a los encuestados, ligando por su orden "alt1_sort" y "alt2_sort":
cbind (alt1_sort, alt2_sort)								# Vemos c�mo se emparejan
# SI EN ALG�N CASO APARECE LA MISMA ALTERNATIVA DOS VECES EN UN MISMO SET, SE REPITE TODO ESTE PROCESO 1.4.


#=========================================#
# 2. CHOICE-BASED CONJOINT (CBC) ANALYSIS #
#=========================================#

# 2.1. EJEMPLO FICTICIO SEGUNDO SOBRE MINIVANS. DESCRIPCI�N
#==========================================================
# Basado en el cap 13 de C. Chapman y E.M. Feit, R For Marketing Research and Analytics (Second Edition) Springer 2019

cbc.df <- read.csv ("http://goo.gl/5xQObB",					# Volcamos el fichero utilizado en la publicaci�n rese�ada
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
# --carpool:	�Piensa utilizar el minivan de forma compartida? Factor con 2 levels: no (1), yes (2)
# --seat:		N�mero de asientos del minivan. Factor con 3 levels: "6" (1),"7" (2), "8"(3)
# --cargo:		Capacidad de carga, en pies de profundidad del maletero. Factor con 2 levels: "2ft" (1), "3ft" (2)
# --eng:		Motor. Factor con 3 levels: "elec" (1),"gas" (2),"hyb" (3)
# --price:		Precio, en miles. Factor con 3 levels: "30" (1),"35" (2),"40" (3)
# --choice:		Alternativa (PERFIL) escogida (1) o no escogida (0)
#
# 200 respondientes han elegido una de las tres alternativas (PERFILES) posibles en cada una de las 15 preguntas (SETS)

# 2.2. TRANSFORMACI�N DE LOS DATOS A UN FORMATO ESPECIAL PARA LA CREACI�N DEL MODELO
#===================================================================================

cbc.mlogit <- mlogit.data (									# Funci�n utilizada
data = cbc.df,												# Datos de partida
choice = "choice",											# Columna con los datos de respuesta
shape = "long",												# Datos presentados en formato "long" (por defecto) en vez de "wide"
varying = 5:8,												# Columnas con los atributos
alt.levels = paste ("pos", 1:3),							# Nombramos las alternativas
id.var = "resp.id")											# Columna de los IDs de los entrevistados
# Obtenemos un dataset class "mlogit.data" y "data.frame"

# 2.3. ESTIMACI�N DEL MODELO 1 CON mlogit()
#==========================================

m1 <- mlogit (choice ~ 0 +									# Con "0 +" no queremos que aparezca los (dos) "intercept", que expresan la posici�n que ocupan en la presentaci�n del "choice"
  seat + cargo + eng + price,								# Atributos incluidos en el modelo
  data = cbc.mlogit)										# dataset de origen

# 2.4. AN�LISIS DE RESULTADOS
#============================

summary (m1)												# Inspeccionamos los resultados
# --"Estimate":		recoge los valores medios para cada NIVEL, siempre en relaci�n con el NIVEL "base" de cada ATRIBUTO. Est�n en escala log�stica entre -2 y +2
# --"Std. Error":	alude a la precisi�n del "Estimate", para los datos analizados, junto al test que indica si el coeficiente es significativamente diferente a cero

# 2.5. GENERACI�N DEL MODELO 2 Y COMPARACI�N
#===========================================
m2 <- mlogit (choice ~										# Sin excluir los intercept 
  seat + cargo + eng + price,
  data = cbc.mlogit)
#
summary (m2)												# Inspeccionamos los resultados
# Dos nuevos par�metros que indican la preferencia por diferentes posiciones (izquierda,centro, derecha) dentro del SET presentado a los entrevistados
# --pos2:(intercept) indica la preferencia relativa por la segunda posici�n en comparaci�n con la primera
# --pos3:(intercept) indica la preferencia relativa por la tercera posici�n en comparaci�n con la primera
# A veces se les llama ALTERNATIVE SPECIFIC CONSTANTS o ASC�s para diferenciarlos del valor en el origen de los modelos lineales
# Normalmente no cabe esperar que sus valores sean significativamente diferentes de cero
#
lrtest (m1, m2)												# Comparamos modelos, siendo uno subgrupo del otro. La diferencia no es relevante: Pr(>Chisq) = 0.7122

# 2.6. GENERACI�N DEL MODELO 3: EL PRECIO COMO NUM�RICO
#======================================================

# Consideramos la variable "price" como variable num�rica dentro de un nuevo modelo:
m3 <- mlogit (choice ~ 0 +
  seat + cargo + eng +										# Atributos incluidos en el modelo
  as.numeric (as.character (price)),						# "price" convertida en variable num�rica
  data = cbc.mlogit)										# dataset de origen
summary (m3)												# Inspeccionamos los resultados
#
lrtest (m1, m3)												# Comparamos modelos, comprobando que son similares


#=======================#
# 3. WILLINGNESS-TO-PAY #
#=======================#

# Con el modelo m3 podemos calcular el willingness-to-pay promedio para un nivel concreto de un atributo
# Dividimos el coeficiente de ese nivel por el coeficiente del precio:
# Usando la funci�n coef() del paquete {stats}:
coef (m3) ["cargo3ft"] / (-coef (m3) ["as.numeric(as.character(price))"] / 1000)
# Usando los propios datos del modelo:
m3$coefficients ["cargo3ft"] / (-m3$coefficients ["as.numeric(as.character(price))"] / 1000)
# En promedio, los clientes estar�an equilibradamente repartidos entre un minivan con capacidad de carga de 2 ft, y uno de 3 ft que cueste $2.750,60 m�s.
# Otra forma es que 2.750,60 d�lares es el precio al que los clientes son indiferentes entre las dos opciones de capacidad de carga


#====================================#
# 4. SIMULACI�N DE CUOTAS DE MERCADO #
#====================================#

# No existe una funci�n para simulaciones utilizando los modelos MNL de mlogit, por lo que usamos una funci�n ad-hoc:
# (tomado de Chapman-Feit, R For Marketing Research and Analytics)
predict.mnl <- function (									# Funci�n para predecir las cuotas de mercado en un modelo MN
  model,													# El modelo estimado con mlogit()
  data)														# data.frame con los dise�os a predecir su cuota de mercado; mismo formato que los datos para estimar el modelo
  {
    data.model <- model.matrix (update (model$formula , 0 ~ .), data = data) [ , -1]
    utility <- data.model%*%model$coef
    share <- exp (utility) / sum (exp (utility))
    cbind (share, data)
  }
# Si queremos saber las cuotas de mercado de todos los perfiles uncluidos en la investigaci�n:
perfiles <- unique (cbc.df [ , c(5:8)])						# Primero creamos un data.frame con esos perfiles
perfiles$price <- as.numeric (as.character (perfiles$price))	# Convertimos la variable price a num�rica
# Aplicamos la funci�n para predecir las cuotas de cada perfil:
predict.mnl (m3, perfiles)									# crea un data.frame, donde la variable "share" muesta la cuota de cada perfil
# Se trata siempre de cuotas en relaci�n con los otros perfiles incluidos

 
#=============================#
# 5. GR�FICOS DE SENSIBILIDAD #
#=============================#

# Objetivo: estimar las variaciones de cuota de mercado si cambian los niveles de los atributos, a partir de un perfil determinado
# Creamos una lista en R llamada "attrib" para almacenar los Atributos y sus Niveles:
attrib <- list (seat = c ("6", "7", "8"),
  cargo = c ("2ft", "3ft"),
  eng = c ("gas", "hyb", "elec"),
  price = c("30", "35", "40"))
#
# Usamos una funci�n ad-hoc (tomado de Chapman-Feit, R For Marketing Research and Analytics):
sensitivity.mnl <- function (								# Funci�n para crear los datos para un gr�fico de sensibilidad
  model,													# El modelo estimado con mlogit()
  attrib,													# list de vectores con los niveles de los atributos
  base.data,												# data.frame conteniendo el dise�o (perfil) base del producto target
  competitor.data)											# data.frame conteniendo el dise�o del competitive set
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
base.data <- expand.grid(attrib) [c(8), ]					# datos del dise�o (perfil) base del producto target
competitor.data <- expand.grid (attrib) [c (1, 3, 41, 49, 26), ]	# datos del dise�o del competitive set
(tradeoff <- sensitivity.mnl (m1, attrib, base.data, competitor.data))
#
# Trazamos el gr�fico:
barplot (
  tradeoff$increase,
  horiz = FALSE,
  names.arg = tradeoff$level,
  main = "Variaciones de la cuota de mercado para el producto target")