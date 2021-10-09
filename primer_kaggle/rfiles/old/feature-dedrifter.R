#Feature Engineering
#creo nuevas variables dentro del mismo mes
#Condimentar a gusto con nuevas variables

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")



#Establezco el Working Directory
setwd( "~" )

# Cargo las variables que ya identifiqué con data drift.
identified_drifted <- c("internet",
                        "Visa_fultimo_cierre",
                        "Master_fultimo_cierre",
                        "cmobile_app_trx",
                        "tmobile_app"
                        )

EnriquecerDataset <- function( dataset , arch_destino )
{
  columnas_originales <-  copy(colnames( dataset ))

  campos_malos <- identified_drifted  # Elimino todas las variables con data drift
  dataset[, (campos_malos):=NULL ]

  #grabo con nombre extendido
  fwrite( dataset,
          file=arch_destino,
          sep= "," )
}
#------------------------------------------------------------------------------

dir.create( "./datasets/" )


#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset1  <- fread("./datasetsOri/paquete_premium_202009.csv")
dataset2  <- fread("./datasetsOri/paquete_premium_202011.csv")

EnriquecerDataset( dataset1, "./datasets/paquete_premium_202009_dedrifted.csv" )
EnriquecerDataset( dataset2, "./datasets/paquete_premium_202011_dedrifted.csv" )

#Don't quit unless we are not part of the pipeline
#quit( save="no")
