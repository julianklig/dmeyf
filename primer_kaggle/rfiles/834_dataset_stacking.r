#Necesita para correr en Google Cloud
#64 GB de memoria RAM
#256 GB de espacio en el disco local
#4 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

setwd("~/buckets/b1/" )

version  <- "v002"  #cambiar cada vez, asi se tiene versionado del dataset

dataset  <- fread( "./datasets/dataset_epic_simple_v003.csv.gz" )
columnas_lindas  <- c("ctrx_quarter", "cpayroll_trx", "mtarjeta_visa_consumo", "mcaja_ahorro", # 1060_34
                      "ctrx_full", "mingresos_total", "mdeudas", "cr_trx_payroll", # 1053_29
                      "mpayroll", "mcuentas_saldo" # 1053_10
                      )
dataset  <- copy(  dataset[  , c("numero_de_cliente","foto_mes","clase_ternaria", columnas_lindas),  with=FALSE] )
gc()


#leo TODOS los archivos que estan en la carpeta  modelitos
#y hago el join con  dataset  <numero_de_cliente, foto_mes, clase_ternaria>

archivos  <- list.files( pattern="modelitos.csv.gz", path="./modelitos/" )
for( archivo  in archivos )
{
  darchivo  <- fread( paste0("./modelitos/", archivo ) )
  dataset  <- merge( dataset, darchivo, by=c("numero_de_cliente","foto_mes") )
}

gc()

fwrite( dataset,
        file=paste0( "./datasets/dataset_stacking_", version, ".csv.gz"),
        sep="," )

