##############################################################################
#####     XI JORNADAS DE USUARIOS DE R. MADRID, 14-16 NOVIEMBRE 2019     #####
#####     TALLER HERRAMIENTAS DE R PARA LA INVESTIGACIÓN DE MERCADOS     #####
#####       8. ITEM RESPONSE THEORY (TEORÍA DE LA REPUESTA AL ITEM)      #####
#####                      POR José Ignacio Casas                        #####
##############################################################################
# 0. CARGA DE PAQUETES Y DATASETS
# 0.1. PAQUETES
# 0.2. DATASET DE EJEMPLO
# 1. CREAMOS EL MODELO IRT
# 2. ANÁLISIS
# 2.1. PUNTUACIONES (SCORES)
# 2.2. GRÁFICOS
# 2.3. ITEM FIT


#=================================#
# 0. CARGA DE PAQUETES Y DATASETS #
#=================================#

# 0.1. PAQUETES
#==============

library (ltm)												# Paquete necesario

# 0.2. DATASET DE EJEMPLO
#========================

# Dataset preparado ex-profeso, descargable desde GitHub:
githubURL <- "https://github.com/JI-Casas/R4Social_Marketing_Research/raw/master/lealtad.RData"
load (url (githubURL))
# El dataset es un data.frame que contiene respuestas dicotómicas 0 = No; 1 = Si a las siguientes variables:
# habito:	1. "Habitualmente compro la marca X"
# cara:		2. "Compro la marca X aunque sea más cara que otras"
# tienda:	3. "Si la tienda en la que habitualmente compro no tuviera la marca X, iría a otra tienda tienda antes de comprar otra marca"
# presta:	4. "Pediría dinero prestado para poder comprar X"


#==========================#
# 1. CREAMOS EL MODELO IRT #
#==========================#
lealtad_irt <- ltm (lealtad ~ z1)							# Modelo de 2 parámetros


#=============#
# 2. ANÁLISIS #
#=============#

# 2.1. PUNTUACIONES (SCORES)
#===========================

factor.scores.ltm (lealtad_irt, method = "EAP")
# EAP = Expected a posteriori scores
# Combinaciones existentes de las cuatro preguntas, sus frecuencias observadas (Obs.) y estimadas por el modelo (Exp.)
# z1: "Ability" o grado de lealtad a la marca y su error standard (se.z1)
#
factor.scores.ltm (lealtad_irt, method = "EAP", resp.patterns = lealtad)
# Lo mismo pero detallando los resultados para cada una de las observaciones del data.frame original

# 2.2. GRÁFICOS
#==============
# Item Charasteristic Curves (ICC):
plot (lealtad_irt, lwd = 3, legend = F)
plot (lealtad_irt, lwd = 3, item = 4, legend = F)
plot (lealtad_irt, lwd = 3, item = c (1:3), legend = F)
# Item Information Curves (IIC):
plot (lealtad_irt, type = "IIC", lwd = 3, legend = F)
plot (lealtad_irt, type = "IIC", lwd = 3, item = 4, legend = F)
plot (lealtad_irt, type = "IIC", lwd = 3, item = c (1:3), legend = F)

# 2.3. ITEM FIT
#==============
# Hasta qué punto todos los items encajan en el modelo logit estimado: encajan para un Pr(>X^2) inferior a 0.05
item.fit (lealtad_irt)
# Hasta qué punto las combinaciones de las respuestas encajan con el modelo: para un Pr(<Lz) superior a 0.05
person.fit (lealtad_irt)
# Hasta qué punto los respondientes encajan con el modelo: para un Pr(<Lz) superior a 0.05
person.fit (lealtad_irt, resp.patterns = lealtad)