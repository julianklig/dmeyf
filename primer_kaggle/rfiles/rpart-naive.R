#Arbol elemental con libreria  rpart
require("data.table")
require("rpart")
require("splitstackshape")
require("foreach")
require("doSNOW")

# Calculo la utilidad en función de las condiciones de la competencia
# 
#   Ganancia = 50000 * aciertos - 1250 * envios
# 
# Los envios son calculados como la cantidad predicha, los aciertos son los
# verdaderos positivos de la predicción.
# Acepta valores numéricos 0 y 1 para las clases.
tp1_utilidad <- function(predicted_values, true_values) {
    conf.matrix <- table(factor(predicted_values,
                                levels=c(0, 1),
                                labels("FALSE", "TRUE"),
                                ordered=TRUE),
                         factor(true_values,
                                levels=c(0, 1),
                                labels("FALSE", "TRUE"),
                                ordered=TRUE),
                         dnn = c("Predicted", "True"))
    return(conf.matrix[2,2] * 50000 - 1250 * sum(conf.matrix[2,]))
}

# Predice y da puntaje con el modelo dado según las reglas de la competencia
tp1_predict_and_score <- function(model, data, prob) {
    train_true <-  as.numeric(data[, clase_ternaria == "BAJA+2"])
    prediciones <- predict(model, data, type = "prob")
    train_predicted <- as.numeric(
        prediciones[, "BAJA+2"] >= prob)
    
    return(tp1_utilidad(train_predicted, train_true))
}

# Grid search simple para el algoritmo rpart
tp1_grid_search <- function(grid, data, cv = 4, njobs = 5) {
    results <- grid
    cl <- makeCluster(njobs)
    registerDoSNOW(cl)
    scores <- foreach(i = 1:nrow(results),
                      .combine = 'c',
                      .export = c("tp1_predict_and_score", "tp1_utilidad"),
                      .packages = c("rpart", "data.table", "splitstackshape")) %dopar% {
            # Muestreo in test/train
            subsets <- stratified(data, 0.2,
                                  group = "clase_ternaria",
                                  bothSets = TRUE)
            data_test <- subsets[[1]] 
            data_train <- subsets[[2]]
            
            # genero el modelo
            modelo_candidato  <- rpart(clase_ternaria ~ .,
                                       data = data_train,
                                       xval=      cv,
                                       cp=        results$cp[i], 
                                       minsplit=  results$minsplit[i],
                                       minbucket= results$minbucket[i],
                                       maxdepth=  results$maxdepth[i]
                                       )
            # Evaluación sobre el subset de test
            return(tp1_predict_and_score(modelo_candidato,
                                         data_test,
                                         results$prob[i]))
        }
    results <- cbind(results, scores)
    stopCluster(cl)
    return(results)
}

#Aqui se debe poner la carpeta de la computadora local
setwd(".")  #Establezco el Working Directory

# cargo los datos de 202009 que es donde voy a ENTRENAR el modelo
dtrain  <- fread("./datasetsOri/paquete_premium_202009.csv")
# cargo los datos de 202011, que es donde voy a APLICAR el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")


# Búsqueda de hyperparámetros en rpart con una grilla

# Genero una grilla para testear
parameter_grid <- expand.grid(
    cp = seq(-1e-4, 1e-4, 1e-4),
    minsplit = seq(90, 120, 10),
    minbucket = 1,
    maxdepth = seq(12, 14, 1),
    prob = 0.025
)

nrow(parameter_grid)

results <- tp1_grid_search(parameter_grid, dtrain)

# Con el mejor set de hiperparámetros encontrado entreno el modelo con 
# validación cruzada en todos los datos
modelo <- rpart(clase_ternaria ~ .,
                data = dtrain,
                xval=      4,
                cp=        1e-4, 
                minsplit=  100,
                minbucket= 1,
                maxdepth=  13
                )


# aplico al modelo  a los datos de 202011
prediccion  <- predict( modelo, dapply , type = "prob") #aplico el modelo

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]
dapply[ , Predicted  := as.numeric(prob_baja2 > 0.025) ]


entrega  <- dapply[   , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, file="./kaggle/K101_001.csv", sep="," )
