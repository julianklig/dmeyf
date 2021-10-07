#Este LightGBM fue construido  para destronar a quienes desde el inicio utilizaron XGBoost y  LightGBM
#mientras sus compañeros luchaban por correr un rpart

#Con los pibes NO

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
require("lightgbm")

setwd("~" )  #establezco la carpeta donde voy a trabajar


kscript <- "lightgbm_drift_check"
kexperimento <- "DRIFTED"
kimp        <- paste0("./work/E",  kexperimento, "_", kscript, "_" )

#cargo el dataset
dataset_ori  <- fread("./datasetsOri/paquete_premium_202009.csv")
dataset_drifted  <- fread("./datasetsOri/paquete_premium_202011.csv")

dataset_ori[, data_drifted := 0]
dataset_drifted[, data_drifted := 1]

dataset <- rbind(dataset_ori, dataset_drifted)

# Cargo las variables que ya identifiqué con data drift.
identified_drifted <- c("internet",
                        "Visa_fultimo_cierre",
                        "Master_fultimo_cierre",
                        "cmobile_app_trx",
                        "tmobile_app"
                        )

#Quito las variables que determinan dataset
campos_buenos  <- setdiff( colnames(dataset),
                           c("data_drifted",
                             "clase_ternaria",
                             "numero_de_cliente",
                             "foto_mes",
                             identified_drifted
                             )
                           )

#genero el formato requerido por LightGBM
dtrain  <- lgb.Dataset( data=  data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset[ , data_drifted]
                      )

modelo  <- lightgbm( data= dtrain,
                     params= list( objective= "binary",
                                   max_bin= 15,
                                   min_data_in_leaf= 500,
                                   learning_rate= 0.05 )  )

#calculo la importancia de variables
tb_importancia  <- lgb.importance( model= modelo )
fwrite( tb_importancia,
        file= paste0(kimp, "imp.txt"),
        sep="\t" )



