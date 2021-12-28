##############################################
##### DMC : Metodos supervisados H2o    #####
##### Profesor: Daniel Chavez Gallo      #####
##### Fecha: 04092020                    #####
##############################################

#-------------------------------------------------------------------------------------------------------------------#

########################------------------- CASO ----------------------###############################
######################################################################################################
#####################  Prediccion de fuga de clientes de una entidad financiera  #####################
######################################################################################################
# Variables predictoras:
# Tasa_Interes      	      Tasa de interes de la cuenta CTS.
# Saldo_Soles	              Monto de Saldo de la cuenta CTS, en Soles.
# Edad	                    Edad del cliente.
# Estado_Civil	            Estado Civil: Div_Sol_Viu = Divorciado, Soltero y Viudo.
#                                         Cas_Conv = Casado, Conviviente.
# Region	                  Zona a la que pertenece el cliente: 
#                                         NORTE_SUR, ORIENTE, CENTRO, LIMA_CALLAO.
# CrossSelling     	        Numero de productos vigentes con el banco, tanto pasivos o activos.
# Ratio_Ant                 Ant_Cts / Ant_Banco  
#                                         Ant_Banco : Tiempo de antiguedad del cliente (en meses)
#                                         Ant_Cts	  : Tiempo de antiguedad de la cuenta CTS (en meses)
#
# Variable dependiente:
#                           Fuga
#                                         0 = cliente no fugado, 1 = cliente fugado
######################################################################################################

# Limpiar memoria, eliminar objetos
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Cargar librerias
library(h2o)
library(tidyverse)
library(tictoc)
library(lubridate)

# Programar cluster para uso de h2o
h2o.init(nthreads = -1, max_mem_size = "4G")

# Ingreso de dataset y splitting
d_h2o = h2o.importFile("Datos/Fuga_CTS.csv", destination_frame = "d_csv")
h2o.str(d_h2o) # estructura de la tabla
h2o.dim(d_h2o) # dimension de la tabla
h2o.colnames(d_h2o) # encabezados de la tabla
class(d_h2o) # clase de la tabla importada
summary(d_h2o) # estadisticos de un data.frame
h2o.describe(d_h2o) # estadisticos de un h2o tabla. En h2o todas las variables deben ser cuantitativas, 
                    # por lo cual h2o contabiliza los nominales como numericos, asignando un valor de 0, 1, 2..
                    # dependiendo el numerio de categorias en la variable cualitativa.

# Trasformando a dummy las variables nominales
length(which(sapply(as.data.frame(d_h2o), class) == "integer"))
length(which(sapply(as.data.frame(d_h2o), class) == "numeric"))
length(which(sapply(as.data.frame(d_h2o), class) == "factor"))
which(sapply(as.data.frame(d_h2o), class) == "factor")

# creamos un tibble, ya que muchas funciones para tratar una tabla no es reconocido por las funciones propias del h2o.
d_h2o_df <- as.tibble(d_h2o)
str(d_h2o_df)
head(d_h2o_df)

# A dummies
library(dummies)
datos_dummy <- d_h2o_df %>% select(which(sapply(as.data.frame(d_h2o), class) == "factor"))
d_h2o_df2 <- data.frame(d_h2o_df,
                        dummy(datos_dummy$Estado_Civil),
                        dummy(datos_dummy$Region)) 
head(d_h2o_df2)

d_h2o_df2 = d_h2o_df2 %>% 
              select(-c(Estado_Civil, Region)) # no tomamos en cuenta las variables cualitativas ya trasformadas a dummies
head(d_h2o_df2)
colnames(d_h2o_df2)

# Particion aleatoria de la tabla
D_h2o = as.h2o(d_h2o_df2)
D_h2o$Fuga = h2o.asfactor(D_h2o$Fuga) # se transforma a factor
h2o.str(D_h2o)

split_frac_3 = h2o.splitFrame(D_h2o, seed = 123,
                              ratios = c(0.7, 0.15), # el complemento le pertenece al test
                                destination_frames = c("train", "control", "test")
                              ) # 3 datas

D_h2o_train   = split_frac_3[[1]]
D_h2o_control = split_frac_3[[2]]
D_h2o_test    = split_frac_3[[3]]
h2o.dim(D_h2o_train)
h2o.dim(D_h2o_control)
h2o.dim(D_h2o_test)
h2o.table(D_h2o$Fuga)
h2o.mean(D_h2o$Fuga) # proporcion estimada de Fugados

# Construyendo el primer algoritmo 
  # definición de etiquetas, target y demas.
y = "Fuga"
x = setdiff(h2o.colnames(D_h2o), y) # las etiquetas complemento
x

# Ayuda
?h2o.glm

Mod_h2o_1 = h2o.glm(y = y, # Target
                    x = x, # variables intervinientes
                    family = "binomial", # familia a ejecutarse, debido al target. Si no especifica por defecto es Regresion.
                    training_frame =  D_h2o_train, # data de entrenamiento
                    model_id = "primer_modelo" # nombre del modelo
                    )
Mod_h2o_1

# que tan bueno es mi primer modelo?
h2o.performance(Mod_h2o_1)
h2o.performance(Mod_h2o_1, newdata = D_h2o_test)

# Evaluacion del modelo
library(caret)
# train
pred_train <- h2o.predict(Mod_h2o_1, newdata = D_h2o_train)
pred_train
clase_Train <- as.data.frame(as.numeric(as.character(pred_train)))[,1] 

conf_train_glm <- confusionMatrix(as.factor(clase_Train), 
                                  as.factor(as.vector(as.matrix(D_h2o_train$Fuga))), 
                                  positive = "1")
conf_train_glm

#test
pred_test <- h2o.predict(Mod_h2o_1, newdata = D_h2o_test)
pred_test
clase_test <- as.data.frame(as.numeric(as.character(pred_test)))[,1] 

conf_test_glm <- confusionMatrix(as.factor(clase_test), 
                                  as.factor(as.vector(as.matrix(D_h2o_test$Fuga))), 
                                 positive = "1")
conf_test_glm

# validation 
algo_h2o_valid = h2o.glm(y = y,
                        x = x,
                        training_frame =  D_h2o_train,
                        validation_frame = D_h2o_control, # Data de validacion
                        model_id = "validacion",
                        family = "binomial"
                 )

h2o.performance(algo_h2o_valid, D_h2o_test)

# validacion cruzada -------------------------------------------
 
algo_h2o_cv = h2o.glm(y = y,
                       x = x,
                       training_frame =  D_h2o_train,
                       model_id = "validacion_cruzada",
                       #k-folds
                       nfolds = 5,
                       keep_cross_validation_models = T,
                       keep_cross_validation_predictions = T,
                       keep_cross_validation_fold_assignment = T,
                       fold_assignment = "Modulo",
                       seed = 123,
                       family = "binomial"
                )

h2o.performance(algo_h2o_cv, D_h2o_test)

# Balanceo de clases

h2o.mean(D_h2o$Fuga)  #5%

library(ROSE)
datos_train_80 = ovun.sample(Fuga ~ ., data = as.data.frame(D_h2o_train), method = "both", p = 0.2, seed = 13)$data
table(datos_train_80$Fuga)
prop.table(table(datos_train_80$Fuga))

algo_h2o_balance = h2o.glm(y = y,
                           x = x,
                           training_frame = as.h2o(datos_train_80),
                           model_id ="balanceo_clases",
                           family = "binomial"
                           )

h2o.performance(algo_h2o_balance)
h2o.performance(Mod_h2o_1)


# Otros algoritmos de machine learning

algo_h2o_RF = h2o.randomForest(y = y,
                               x = x,
                               training_frame = D_h2o_train,
                               validation_frame = D_h2o_control,
                               model_id ="sin_parada",
                               #algoritmo
                               ntrees = 500
              )

algo_h2o_RF@model$scoring_history %>%
  select(number_of_trees, training_logloss, validation_logloss) %>%
  gather(frame, value, -number_of_trees) %>%
  ggplot(aes(x = number_of_trees, y = value, col = frame)) +
    geom_line()


algo_h2o_gbm = h2o.gbm(y = y,
                       x = x,
                       training_frame = D_h2o_train,
                       validation_frame = D_h2o_control,
                       model_id ="con_parada",
                       #parada temprana
                       stopping_rounds = 5,
                       stopping_metric = "logloss",
                       stopping_tolerance = 0.0001,
                       #algoritmo
                       ntrees = 500
                )

algo_h2o_gbm@model$scoring_history %>%
  select(number_of_trees, training_logloss, validation_logloss) %>%
  gather(frame, value, -number_of_trees) %>%
  ggplot(aes(x = number_of_trees, y = value, col = frame)) +
  geom_line()

algo_h2o_RF %>% h2o.performance(D_h2o_test) %>% h2o.auc
algo_h2o_gbm %>% h2o.performance(D_h2o_test) %>% h2o.auc


# Importancia de las variables

algo_h2o_cv %>% h2o.varimp_plot(num_of_features = 10) #importancia variables GLM
algo_h2o_RF %>% h2o.varimp_plot(num_of_features = 10) #importancia variables randomforest
algo_h2o_gbm %>% h2o.varimp_plot(num_of_features = 10) #importancia variables gbm

# Guardando y cargando algoritmos

h2o.saveModel(algo_h2o_cv, path = "Datos/")
algo_cv = h2o.loadModel("Datos/validacion_cruzada")

# Salir por completo
h2o.removeAll()
h2o.shutdown()
Y
