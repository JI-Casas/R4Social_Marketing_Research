##############################################################################
#####     XI JORNADAS DE USUARIOS DE R. MADRID, 14-16 NOVIEMBRE 2019     #####
#####     TALLER HERRAMIENTAS DE R PARA LA INVESTIGACIÓN DE MERCADOS     #####
#####      1. SEGMENTACIÓN DE MERCADOS: USO DE HERRAMIENTAS CLUSTER      #####
#####                      POR José Ignacio Casas                        #####
##############################################################################
# 0. CARGA DE PAQUETES Y DATOS
# 0.1. PAQUETES
# 0.2. CARGA DE DATASETS Y PREPARACIÓN DE LOS MISMOS
# 0.2.1. USArrests
# 0.2.2. dataset_clus
# 0.2.3. flower, incluido en {cluster}
# 1. DISTANCIAS ENTRE OBJETOS
# 1.1. DISTANCIAS EUCLIDEANAS
# 1.1.1. CON LA FUNCIÓN dist() DEL PAQUETE BASE {stats}
# 1.1.2. CON LA FUNCIÓN get_dist() DEL PAQUETE {factoextra}
# 1.2. DISTANCIAS BASADAS EN CORRELACIONES (CORRELATION-BASED DISTANCES)
# 1.2.1. EJEMPLO CON USArrests
# 1.2.2. EJEMPLO CON DATASET SIMULADO
# 1.3. DISTANCIAS PARA DATOS BINARIOS
# 1.4. DISTANCIAS PARA DATOS MIXTOS
# 1.4.1. CON EL DATASET flower
# 1.4.2. CON DATASET dataset_clus
# 1.5. VISUALIZACIÓN DE LA MATRIZ DE DISTANCIAS
# 2. EXISTENCIA DE CLUSTERS: ASSESSING CLUSTERING TENDENCY
# 3. DETERMINACIÓN DEL NÚMERO ÓPTIMO DE CLUSTERS
# 3.1. CON LA FUNCIÓN fviz_nbclust() DEL PAQUETE {factoextra}
# 3.2. CON EL PAQUETE {NbClust}
# 3.3. USANDO NbClust::NbClust MÄS factoextra::fviz_nbclust
# 4. CLUSTER JERÁRQUICO
# 4.1. CLUSTER AGLOMERATIVO
# 4.2. RESULTADOS
# 4.3. VISUALIZACIÓN DEL CLUSTER JERÁRQUICO
# 4.4. NÚMERO ÓPTIMO DE CLUSTERS
# 5. K-means
# 5.1. CÁLCULO PARA datUSA2
# 5.2. ANÁLISIS DEL RESULTADO
# 5.3. VISUALIZACIÓN DE LOS CLUSTERS SEGÚN k-means CON LA FUNCIÓN “fviz_cluster” DEL PAQUETE {factoextra}
# 6. PAM (PARTITIONING AROUND MEDOIDS)
# 6.1. CÁLCULO PARA datUSA2
# 6.2. ANÁLISIS DEL RESULTADO
# 6.3. VISUALIZACIÓN DE LOS CLUSTERS SEGÚN PAM CON LA FUNCIÓN “fviz_cluster” DEL PAQUETE {factoextra}
# 7. CLUSTERS CON CLARA (Clustering LARge Applications)
# 7.1. CÁLCULO PARA datUSA2
# 7.2. ANÁLISIS DEL RESULTADO
# 7.3. VISUALIZACIÓN DE LOS CLUSTERS SEGÚN CLARA CON LA FUNCIÓN “fviz_cluster” DEL PAQUETE {factoextra}
# 8. VALIDACIÓN
# 8.1. LA FUNCIÓN eclust(), DEL PAQUETE {factoextra}
# 8.2. CÁLCULO PARA datUSA2
# 8.3. ANÁLISIS DEL RESULTADO
# 8.4. VALIDACIÓN DEL CLUSTER CON SILHOUETTE Y DUNN
# 8.4.1 PLOT SILHOUETTE
# 8.4.2. COEFICIENTE SILHOUETTE
# 8.4.3. ELEMENTOS CON COEFICIENTE SILHOUETTE NEGATIVO
# 8.4.4 CÁLCULO DE ÍNDICE DE DUNN Y OTRAS ESTADÍSTICAS DE VALIDACIÓN CON LA FUNCIÓN cluster.stats() DEL PAQUETE {fpc}


#==============================#
# 0. CARGA DE PAQUETES Y DATOS #
#==============================#

# 0.1. PAQUETES
#==============
library (cluster)											# para el cálculo de los clusters
library (factoextra)										# para visualizar los datos, apoyado en el paquete ggplot2
library (tidyverse)											# carga una variedad de paquetes
library (NbClust)											# para estimar el número de clusters con NbClust()
library (pheatmap)											# Paquete necesario para "mapa de calor"
library (fpc)												# para la función pamk() y cluster.stats()

# 0.2. CARGA DE DATASETS Y PREPARACIÓN DE LOS MISMOS
#===================================================
#
# 0.2.1. USArrests
data (USArrests)											# Utilizamos este dataset incluido ya en R
datUSA1 <- na.omit (USArrests)								# Eliminamos los NAs
datUSA2 <- scale (datUSA1)									# Estandarizamos para evitar disparidades de magnitud
datUSA2 <- USArrests %>% na.omit () %>% scale ()			# Lo mismo con una sola línea de código, utilizando el "piping"
head (datUSA2, n = 3)										# Vemos lo que tenemos
#
# 0.2.2. dataset_clus
# Dataset preparado ex-profeso, descargable desde GitHub:
githubURL <- "https://github.com/JI-Casas/R4Social_Marketing_Research/raw/master/dataset_clus.RData"
load (url (githubURL))
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
#
# Seleccionamos una muestra aleatoria de 50 elementos, reteniendo sólo las variables lógicas: item1 a item5:
dataBinary <- dataset_clus [sample (nrow (dataset_clus), 50), ] [ , c(3:7)]
#
# 0.2.3. flower, incluido en {cluster}
data (flower)												# Incorporamos un nuevo dataset
head (flower, 3)											# Vemos su contenido
str (flower)												# Vemos su estructura


#=============================#
# 1. DISTANCIAS ENTRE OBJETOS #
#=============================#

# 1.1. DISTANCIAS EUCLIDEANAS
#============================

# 1.1.1. CON LA FUNCIÓN dist() DEL PAQUETE BASE {stats}:
dd_USA_eucl <- dist (datUSA2, method = "euclidean")			# Sólo datos numéricos. Métodos: "euclidean" (por defecto),"maximum", "manhattan", "canberra", "binary", "minkowski"

# 1.1.2. CON LA FUNCIÓN get_dist() DEL PAQUETE {factoextra}:
# get_dist() del paquete {factoextra}: sólo acepta como input datos numéricos, pero soporta correlation-based distance
dd_USA_mink <- get_dist (USArrests,							# Sólo datos numéricos
  stand = TRUE,												# Estandarizamos las variables
  method = "minkowski",										# Métodos: "euclidean" (por defecto),"maximum", "manhattan", "canberra", "binary", "minkowski"
  p = 2)													# En el caso del método minkowski, p es el exponente, por defecto 2
# Los valores representan las distancias entre objetos (filas).
# Para calcular la distancia entre variables se transponen previamente los datos con la función "t()

# 1.2. DISTANCIAS BASADAS EN CORRELACIONES (CORRELATION-BASED DISTANCES)
#=======================================================================

# 1.2.1. EJEMPLO CON USArrests
dd_USA_pear <- get_dist (datUSA2, method = "pearson")		# Sólo datos numéricos. Métodos: "pearson", "kendall" y "spearman"

# 1.2.2. EJEMPLO CON DATASET SIMULADO
set.seed (1410)
y <- matrix (
  rnorm (50), 10, 5,
  dimnames = list (
    paste ("r", 1:10, sep = ""), 
    paste("c", 1:5, sep = "")
  ))
dim (y)
#
yscaled <- t (scale (t (y)))								# Estandarizamos los datos por filas
apply (yscaled, 1, sd)
cor_y <- cor (t (y), method = "pearson")					# Creamos la matriz de correlaciones 
as.matrix (cor_y) [1:4, 1:4]								# Vemos lo que tenemos
dd_y <- as.dist (1 - cor_y)									# Creamos la matriz de distancias basadas en correlaciones
as.matrix (dd_y) [1:4, 1:4]									# Vemos lo que tenemos
dd_y_pear <- get_dist (y, method = "pearson")				# Con la función get_dist() del paquete {factoextra}
dd_y_eucl <- get_dist (y, method = "euclidean")				# Comparado con distancia euclideana
#
# COMPARACIÓN GRÁFICA ENTRE AMBOS:
fviz_dist (dd_y_pear, gradient = list (low = "#00AFBB", mid = "white", high = "#FC4E07"))
fviz_dist (dd_y_eucl, gradient = list (low = "#00AFBB", mid = "white", high = "#FC4E07"))

# 1.3. DISTANCIAS PARA DATOS BINARIOS
#====================================
jacc <- dist (dataBinary, method = "binary")
jacc <- as.data.frame (as.matrix (jacc))					# ggplot necesita un data.frame
jaccsim <- 1 - jacc											# Queremos la similaridad de Jaccard, no la distancia
View (jaccsim)												# Vemos lo que tenemos
jaccsim$names <- rownames (jaccsim)							# añadimos una variable con los "rownames" para la función melt()
jacc.m <- reshape2::melt (jaccsim, id.vars = "names")
jacc.m <- plyr::arrange (jacc.m, variable, plyr::desc (names))	# ordena el data.frame
jacc.m$names <- factor (jacc.m$names, rownames (jaccsim))	# convierte en factor
jacc.m$variable <- factor (jacc.m$variable, rev (rownames (jaccsim)))
#
# GRÁFICO CON ggplot
library (ggplot2)
pt <- ggplot (jacc.m, aes (names, variable)) +
  geom_tile (aes (fill = value), colour = "white") +
  scale_fill_gradient (low = "#b7f7ff", high = "#0092a3")
base_size <- 20
pt +
theme_light(base_size = base_size) +
labs (x = "", y = "") +
scale_x_discrete (expand = c (0, 0)) +
scale_y_discrete (expand = c (0, 0)) +
guides (fill = guide_legend (title = NULL)) +
theme (
  axis.ticks = element_blank(),
  axis.text.x = element_text (size = base_size * 0.8, angle = 330, hjust = 0),
  axis.text.y = element_text (size = base_size * 0.8)
  )

# 1.4. DISTANCIAS PARA DATOS MIXTOS
#==================================

# 1.4.1. CON EL DATASET flower
# daisy() del paquete {cluster}: permite otro tipo de variables (e.g. nominal, ordinal, (a)symmetric binary)
# En ese caso usa automáticamente como métrica el coeficiente de Gower. Es una de las medidas de proximidad más populares para tipos de datos mixtos.
dd_flo <- daisy (flower)									# del paquete {cluster}
round (as.matrix (dd_flo) [1:3, 1:3], 2)					# 3 primeras filas y columnas, como matriz y un decimal
# daisy() permite otro tipo de variables (e.g. nominal, ordinal, (a)symmetric binary). En ese caso usa automáticamente como métrica el coeficiente de

# 1.4.2. CON DATASET dataset_clus
# Como hay variables de tipo mixto, se calcula la matriz de disimilaridades usando la métrica de Gower, por defecto cuando se detectan variables mixtas:
dd_clus <- daisy (											# Utilizamos la función "daisy" del paquete {cluster}. OJO: requiere "data.frame"
  dataset_clus [ , c(2:13)],								# Tomamos todas las variables menos la primera ("id")
  type = list (asymm = c ("item1", "item2", "item3", "item4", "item5"))	# Advertimos que esas variables (logic) las tome como binarias asimétricas
  )
summary (dd_clus)	

# 1.5. VISUALIZACIÓN DE LA MATRIZ DE DISTANCIAS
#==============================================

fviz_dist (dd_flo)											# del paquete {factoextra}. Objectos del mismo cluster aparecen en orden consecutivo
# El nivel de color es proporcional a la distancia entre observaciones: rojo puro = iguales / azul puro = totalmente diferentes
# Podemos cambiar los colores:
fviz_dist (dd_flo, gradient = list (low = "yellow", mid = "green", high = "blue"))


#==========================================================#
# 2. EXISTENCIA DE CLUSTERS: ASSESSING CLUSTERING TENDENCY #
#==========================================================#

# 2.1. EJEMPLO 1: dd_flo
#=======================
get_clust_tendency (										# Del paquete {factoextra}. Por defecto toma "seed = 123"
  as.matrix (dd_flo),										# data: un dataframe o matrix
  n = nrow (as.matrix (dd_flo)) - 1)						# n: número de puntos a seleccionar de los datos, siempre menor que el número de observaciones
# RESULTADO:
## $hopkins_stat
## [1] 0.3568634											# CLUSTERABLE: ~ 0; RANDOM: ~ 0.5; UNIFORME: >> 0.5
## $plot													# por defecto crea un gráfico significativo. Para evitarlo: "graph = FALSE"

# 2.2. EJEMPLO 2: dd_clus (Se toma su tiempo)
#============================================
get_clust_tendency (										# Del paquete {factoextra}. Por defecto toma "seed = 123"
  as.matrix (dd_clus),										# data: un dataframe o matrix
  n = nrow (as.matrix (dd_clus)) - 1,						# n: número de puntos a seleccionar de los datos, siempre menor que el número de observaciones
  graph = FALSE)											# por defecto crea un gráfico significativo. Para evitarlo: "graph = FALSE"
# RESULTADO:
## $hopkins_stat
## [1] 0.1160362											# CLUSTERABLE: ~ 0; RANDOM: ~ 0.5; UNIFORME: >> 0.5
## $plot													# por defecto crea un gráfico significativo. Para evitarlo: "graph = FALSE"
#
# 2.3. 	EJEMPLO 3: "iris"
#========================
df_iris <- iris [ , -5]										# tomamos el dataset "iris" quitando la variable "Species", que no es numérica
get_clust_tendency (df_iris, n = 150 - 1)					# n no puede ser más grande que el número de objetos
# RESULTADO:
## $hopkins_stat
## [1] 0.1627845											# CLUSTERABLE: ~ 0; RANDOM: ~ 0.5; UNIFORME: >> 0.5
## $plot


#================================================#
# 3. DETERMINACIÓN DEL NÚMERO ÓPTIMO DE CLUSTERS #
#================================================#

# 3.1. CON LA FUNCIÓN fviz_nbclust() DEL PAQUETE {factoextra} 
#============================================================

muestra <- sample (as.data.frame (as.matrix (dd_clus)), 50)	# Tomamos una muestra
fviz_nbclust (
  muestra,													# data.frame o matrix numérica
  kmeans,													# También pam, clara y hcut (para cluster jerárquico)
  method = "wss")											# Método "elbow" de cálculo del número de cluster. También: "silhouette", "gap_stat" (muy lento)

# 3.2. CON EL PAQUETE {NbClust}
#==============================

NbClust (													# Aplica un conjunto de 30 procedimientos, presentando el número de clusters más probable
  data = muestra,											# dataframe, matrix u objeto creado con NbClust() del paquete {NbClust}
  diss = NULL,												# Matriz de disimilaridad: por defecto NULL
  distance = "euclidean",									# Método de cálculo de distancias
  min.nc = 2,												# Número mínimo de clusters
  max.nc = 15,												# Número máximo de clusters
  method = "complete",										# Método de creación de los clusters
  index = "all")											# Índices usados para calcular el número de clusters

# 3.3. USANDO NbClust::NbClust MÁS factoextra::fviz_nbclust
#==========================================================

num_clus <- NbClust (
  muestra,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 10,
  method = "kmeans")
fviz_nbclust (num_clus)										# Visualizamos el número de clusters


#=======================#
# 4. CLUSTER JERÁRQUICO #
#=======================#

# 4.1. CLUSTER AGLOMERATIVO
#==========================

res_hc <- hclust (											# Del paquete {stats}
  dist (datUSA2),											# Toma como input la matriz de disimilaridad, calculada con la función dist()
  method = "ward.D2")
# Métodos: "ward.D", "ward.D2", "single", "complete" (por defecto), "average" (=UPGMA), "mcquitty" (=WPGMA), "median" (=WPGMC) o "centroid" (=UPGMC)

# 4.2. RESULTADOS
#================
# Devuelve un objeto "hclust" con estructura de "list". En ésta algunos elementos son:
res_hc$merge	# Orden en que se han realizado las agrupaciones. Cada elemento: con signo negativo. Sin signo negativo: número de paso anterior
res_hc$height	# "Altura" a la que se ha realizado cada una de las agrupaciones, en el orden efectuado
USArrests$numerocluster <- stats::cutree (res_hc, 2)		# Añadimos a cada elemento el cluster al que pertenece

# 4.3. VISUALIZACIÓN DEL CLUSTER JERÁRQUICO
#==========================================
plot (res_hc)
#
# Con la función fviz_dend() del paquete {factoextra}:
fviz_dend (res_hc, cex = 0.5, k = 4, palette = "jco")		# k = 4 distribuye los casos en cuatro posibles clusters
#
# VISUALIZACIÓN CON HEATMAP ("MAPA DE CALOR")
# Los valores de los datos se convierten a una escala de colores. Visualiza simultáneamente grupos de casos y rasgos (variables).
# En heatmap, generalmente, las columnas son los casos y las filas las variables, por lo que hay que transponer los datos:
pheatmap (t (datUSA2), cutree_cols = 4)

# 4.4. NÚMERO ÓPTIMO DE CLUSTERS
#===============================
# Un procedimiento habitual es cortar el árbol a la altura de la mayor diferencia de "heights" entre dos nodos
alturas <- res_hc$height									# Los valores "height" forman parte del output de la función "hclust"
alturas														# Vemos lo que tenemos
alturas2 <- c (0, alturas [-length(alturas)])				# Vector para las substracciones de "heights"
alturas - alturas2											# Vector de diferencias entre "heights" sucesivos
max (alturas - alturas2)									# Identificación de la mayor diferencia
which.max (alturas - alturas2)								# Identificación del paso con la mayor diferencia
# De acuerdo con ello el número apropiado de clusters es 2, ya que la mayor diferencia se localiza en el último paso del proceso de agrupación de casos


#============#
# 5. K-means #
#============#

# 5.1. CÁLCULO PARA datUSA2
#==========================

set.seed (123)												# aseguramos resultados iguales
km_USA <- kmeans (											# función del paquete {stats}
  x = datUSA2,												# matrix numerica u objeto "cohercible"  a matrix numérica
  iter.max = 10,											# Número máximo de iteraciones permitidas, por defecto 10.
  centers = 3,												# número k de clusters o grupo de centroides predeterminados
  nstart = 25)												# Número de particiones iniciales aleatorias cuando "centers" es un número, por defecto 10:
															# se recomienda comenzar con nstart > 1.

# 5.2. ANÁLISIS DEL RESULTADO
#============================
View (km_USA)	# List con los siguientes elementos:
# --cluster:		Vector de número enteros (desde 1 a k) indicando el cluster al que cada objeto es asignado
# --centers:		Matriz con los valores centrales de cada cluster (cluster means)
# --totss:			Suma total de cuadrados (TSS: Total Sum of Squares). Mide la varianza total en los datos
# --withinss:		Vector de la suma de cuadrados intra-cluster: un componente por cluster
# --tot.withinss:	Total de sumas de cuadrados intra-clusters: suma de withinss
# --betweenss:		Suma de cuadrados entre clusters = totss - tot.withinss
# --size:			Número de casos en cada cluster
#
print (km_USA)												# Vemos parte de lo que tenemos
# --Número y tamaño de cada cluster
# --Cluster means: matrix de los valores de cada centroide en cada variable
# --Clustering vector: vector de números enteros (de 1 a k) del cluster al que se ha asignado cada objeto
#
# Para calcular la media de cada variable en cada cluster, según los datos originales:
aggregate (USArrests, by = list (cluster = km_USA$cluster), mean)
# Podemos añadir al dataframe original el cluster al que pertenece cada caso:
USArrestsConKmeans <- cbind (USArrests, cluster = km_USA$cluster)
head (USArrestsConKmeans)

# 5.3. VISUALIZACIÓN DE LOS CLUSTERS SEGÚN k-means CON LA FUNCIÓN “fviz_cluster” DEL PAQUETE {factoextra}
#========================================================================================================

# Como los datos contienen más de dos variables, se aplica un algoritmo de reducción de la dimensionalidad, como PCA
fviz_cluster (km_USA, data = datUSA2,						# Toma como argumentos los resultados de k-means y los datos originales
  ellipse.type = "convex",									# Otras formas: "confidence", "t", "norm", "euclid"
  palette = c("#EE0000", "#32CD32", "#000080", "#CD6600"),	# Colores: red2, limegreen, navyblue, darkorange3
  repel = T,												# Para que los rótulos no se superpongan: proceso algo más lento
  ggtheme = theme_minimal ())
  
# Otra versión:
fviz_cluster (km_USA, data = datUSA2,
  palette = c("#EE0000", "#32CD32", "#000080", "#CD6600"),
  ellipse.type = "euclid",									# Otra forma de representación gráfica de los clusters
  star.plot = TRUE,											# Añade líneas de conexión de cada objeto con su centroide
  repel = TRUE,
  ggtheme = theme_minimal()
)


#======================================#
# 6. PAM (PARTITIONING AROUND MEDOIDS) #
#======================================#

# 6.1. CÁLCULO PARA datUSA2
#==========================

# Obtenemos los clusters, fijando de antemano un número de tres:
pam_USA <- pam (
  x = datUSA2,				# matrix o data frame numéricos, admitiendo NAs siempre que cada par de observaciones tenga al menos un caso no missing,...
  k = 3)					# Número de medoides previamente definido,...

# 6.2. ANÁLISIS DEL RESULTADO
#============================

View (pam_USA)	# List con los siguientes elementos:
# --medoids:		los medoides y valores estandarizados de las variables
# --id.med:			vector de enteros de las posiciones de medoides en el dataset
# --clustering:		Vector de números enteros (de 1 a k) del cluster asignado a cada objeto
# --objective:		valor de la función objetivo tras las fases BUILD y SWAP del algoritmo
# --isolation:		clusters que están aislados (L- o L*-clusters) y que no lo están:
# ---- L*-cluster:	su diámetro es menor que su separación
# ---- L-cluster:	para cada objeto i la máxima disimilaridad entre i y cualquier otro objeto del cluster es menor que la mínima disimilaridad entre i cualquier objeto de otro cluster
# ---- Cada L*-cluster es también un L-cluster
# --clusinfo:		matrix de datos para cada cluster:
# ---- número de objetos,
# ---- disimilaridad máxima y promedio de los objetos del cluster y su medoide,
# ---- diámetro (máxima disimilaridad entre dos objetos del cluster),
# ---- separación (mínima disimilaridad entre un objeto del cluster y un objeto de otro cluster)
# --silinfo:		lista con el ancho de silueta (silhouette width) de cada objeto, el promedio en cada cluster y el promedio global.
#					Valores: de -1 (peor integración) a 1 (mayor)
# --diss:			matriz de disimilariadades entre los objetos
# --call:			atributos de partida
# --data:			datos etandarizados
#
print (pam_USA)												# Vemos parte de lo que tenemos
# --Medoids, objeto, índice y valores estandarizados de las variables
# --Clustering vector: vector de números enteros (de 1 a k) del cluster asignado a cada objeto
# --Objective function: valor alcanzado por la función objetivo
# --Available components: componentes de la list
#
# Podemos añadir al dataframe original el cluster al que pertenece cada caso:
USArrestsConPAM <- cbind (USArrests, cluster = pam_USA$clustering)
head (USArrestsConPAM)

# 6.3. VISUALIZACIÓN DE LOS CLUSTERS SEGÚN PAM CON LA FUNCIÓN “fviz_cluster” DEL PAQUETE {factoextra}
#====================================================================================================

# Como los datos contienen más de dos variables, se aplica un algoritmo de reducción de la dimensionalidad, como PCA:
fviz_cluster (pam_USA,
  palette = c("#EE0000", "#32CD32", "#000080", "#CD6600"),	# Colores: red2, limegreen, navyblue, darkorange3
  ellipse.type = "t",										# dibuja elipses
  repel = TRUE,												# Para que los rótulos no se superpongan
  ggtheme = theme_classic()
)

# 6.4. CLUSTERS CON PAM SIN FIJAR DE ANTEMANO K: FUNCIÓN pamk() DEL PAQUETE {fpc}
#================================================================================

pamk_USA <-pamk (datUSA2)	# "wrap" de cluster::pam, incorporando el método del promedio del ancho de silueta para determinar el numero de clusters


#=======================================================#
# 7. CLUSTERS CON CLARA (Clustering LARge Applications) #
#=======================================================#

# 7.1. CÁLCULO PARA datUSA2
#==========================
clara_USA <- clara (
  x = datUSA2,			# matrix o data frame numéricos, admitiendo NAs siempre que cada par de observaciones tenga al menos un caso no missing,...
  k = 3,				# Número de medoides previamente definido
  samples = 5,			# número de muestras a tomar, por defecto 5, aunque es recomendable tomar un número mayor 
  sampsize = 40+2*3,	# tamaños de las muestras, por defecto 40+2k
  rngR = FALSE)			# si se usa un generador aleatorio de números. Por defecto FALSE, para usar el interno incluido en clara()

# 7.2. ANÁLISIS DEL RESULTADO
#============================
View (clara_USA)	# List con los siguientes elementos:
# --sample:			composición de la muestra que devuelve el mejor resultado
# --medoids:		los medoides y valores estandarizados de las variables
# --i.med:			vector de enteros de las posiciones de medoides en el dataset
# --clustering:		Vector de números enteros (de 1 a k) del cluster asignado a cada objeto
# --objective:		valor de la función objetivo
# --isolation:		clusters que están aislados (L- o L*-clusters) y que no lo están:
# ----L*-cluster: su diámetro es menor que su separación
# ----L-cluster: para cada objeto i la máxima disimilaridad entre i y cualquier otro objeto del cluster es menor que la mínima disimilaridad entre i cualquier objeto de otro cluster
# ----Cada L*-cluster es también un L-cluster
# --clusinfo:		matrix de datos para cada cluster:
# ----número de objetos,
# ----disimilaridad máxima y promedio de los objetos del cluster y su medoide,
# ----separación (mínima disimilaridad entre un objeto del cluster y un objeto de otro cluster)
# --diss:			matriz de disimilariadades entre los objetos
# --silinfo:		lista con el ancho de silueta (silhouette width) de la mejor muestra, el promedio en cada cluster y el promedio global. Valor de -1 (peor integración) a 1 (mayor)
# --diss:			matriz de disimilariadades entre los objetos
# --call:			atributos de partida
# --data:			datos originales
#
print (clara_USA)											# Vemos parte de lo que tenemos
# --Medoids, objeto, índice y valores estandarizados de las variables
# --Objective function:	valor alcanzado por la función objetivo
# --Clustering vector:	vector de números enteros (de 1 a k) del cluster asignado a cada objeto
# --Cluster sizes:		tamaño de los clusters
# --Best sample:	composición de la muestra que devuelve el mejor resultado
# --Available components: componentes de la list
#
# Podemos añadir al dataframe original el cluster al que pertenece cada caso:
USArrestsConClara <- cbind (USArrests, cluster = clara_USA$clustering)
head (USArrestsConClara)
#
# 7.3. VISUALIZACIÓN DE LOS CLUSTERS SEGÚN CLARA CON LA FUNCIÓN “fviz_cluster” DEL PAQUETE {factoextra}
#======================================================================================================
# Como los datos contienen más de dos variables, se aplica un algoritmo de reducción de la dimensionalidad, como PCA
fviz_cluster (clara_USA,
  palette = c("#EE0000", "#32CD32", "#000080", "#CD6600"),	# Colores: red2, limegreen, navyblue, darkorange3
  ellipse.type = "t",										# dibuja elipses
  repel = TRUE,												# Para que los rótulos no se superpongan
  ggtheme = theme_classic()
)


#===============#
# 8. VALIDACIÓN #
#===============#

# 8.1. LA FUNCIÓN eclust(), DEL PAQUETE {factoextra}
#===================================================

# Simplifica el flujo de trabajo del análisis cluster
# Puede usarse para calcular tanto el cluster jerárquico como el de partición con una única función en una misma línea
# eclust() calcula automáticamente el estadístico gap para estimar el número adecuado de clusters
# Suministra la información "silhouette" para todos los métodos de particiòn y jerárquicos
# Dibuja gráficos utilizando ggplot2

# 8.2. CÁLCULO PARA datUSA2
#==========================

# 8.2.1. CÁLCULO DE UN CLUSTER DE PARTICIÓN COMO k-means CON k = 3:
km_USA_eclust <- eclust (datUSA2, "kmeans", k = 3, nstart = 25, graph = FALSE)		# cluster K-means
fviz_cluster (km_USA_eclust, geom = "point", ellipse.type = "norm",			# Visualización
  palette = "jco", ggtheme = theme_minimal())
#
# 8.2.2. CÁLCULO DE UN CLUSTER JERÁRQUICO:
hc_USA_eclust <- eclust (datUSA2, "hclust", k = 3, hc_metric = "euclidean",			# cluster jerárquico
  hc_method = "ward.D2", graph = FALSE)
fviz_dend (hc_USA_eclust, show_labels = FALSE,									# Visualización
  palette = "jco", as.ggplot = TRUE)

# 8.3. ANÁLISIS DEL RESULTADO
#============================
# La función eclust() devuelve un objeto de clase eclust con el resultado de la función estándar usada (kmeans, pam, hclust, agnes, diana, etc.)
# También incluye:
# --cluster:	cluster asignado a cada objeto
# --nbclust:	número de clusters
# --silinfo:	coeficiente silhouette de cada objeto
# --size:		tamaño de los clusters
# --data:		matriz de los datos originales o estandarizados, si stand = TRUE
# --gap_stat:	estadístico gap

# 8.4. VALIDACIÓN DEL CLUSTER CON SILHOUETTE Y DUNN
#==================================================

# 8.4.1 PLOT SILHOUETTE
fviz_silhouette (
  km_USA_eclust,
  palette = "jco",
  ggtheme = theme_classic(),
  print.summary = TRUE)										# Por defecto. Si FALSE no imprime un summary del resultado
#
# 8.4.2. COEFICIENTE SILHOUETTE
silinfo <- km_USA_eclust$silinfo							# list con tres elementos:
# --widths:				de cada elemento: cluster al que pertenece; cluster más cercano; coeficiente silhouette
# --clus.avg.widths:	coeficiente silhouette de cada cluster
# --avg.width:			coeficiente silhouette global
#
# 8.4.3. ELEMENTOS CON COEFICIENTE SILHOUETTE NEGATIVO
sil_indiv <- km_USA_eclust$silinfo$widths[, 1:3]			# de cada elemento: cluster al que pertenece; cluster más cercano; coeficiente silhouette
neg_sil_index <- which (sil_indiv [, 'sil_width'] < 0)		# Objectos concoeficiente silhouette negativo
sil_indiv [neg_sil_index, , drop = FALSE]					# Vemos la lista de objetos "mal agrupados"
#
# 8.4.4 CÁLCULO DE ÍNDICE DE DUNN Y OTRAS ESTADÍSTICAS DE VALIDACIÓN CON LA FUNCIÓN cluster.stats() DEL PAQUETE {fpc}
# Previamente calculamos los clusters en iris con K-means:
set.seed (123)												# Aseguramos datos iguales
km_iris <- kmeans (											# función del paquete {stats}
x = df_iris,												# matrix numerica u objeto "cohercible"  a matrix numérica
iter.max = 10,												# Número máximo de iteraciones permitidas, por defecto 10.
centers = 3,												# número k de clusters o grupo de centroides predeterminados
nstart = 25)
# CALCULAMOS UN CONJUNTO AMPLIO DE ÍNDICES DE VALIDACIÓN
km_iris_stats <- cluster.stats (							# Estadísticas para el clustering k-means
  dist (df_iris),											# objeto "distancias"
  km_iris$cluster)											# vector con el nñumero de clusters
km_iris_stats$dunn											# Índice Dunn. Valores de 0 a infinito
km_iris_stats												# Para ver todas las estadísticas
# ANÁLISIS DEL RESULTADO
View (km_iris_stats)										# Algunos resultados son los siguientes:
# --cluster.number:		número de clusters
# --cluster.size:		vector de tamaños de cada cluster
# --average.distance:	vector de distancias promedio dentro de cada cluster
# --median.distance:	vector de distancias medianas dentro de cada cluster
# --average.between:	distancia promedio entre clusters: cuanto mayor mejor
# --average.within:		distancia promedio dentro de los clusters: cuanto menor mejor
# --clus.avg.silwidths: vector de los coeficientes silhouette de cada cluster. Valores entre -1 (mal cluster) y 1 (buen cluster)
# --dunn, dunn2:		Índices Dunn y similares
# --corrected.rand, vi: dos índices para evalur la similitud entre dos análisis clusters: el índice corregido de Rand y el VI de Meila