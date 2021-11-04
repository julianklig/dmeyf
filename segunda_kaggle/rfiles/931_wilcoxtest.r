#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
setwd("~/buckets/b1/")


corrida <- list()

corrida$arch_testing1  <- "./work/E4006/E4006_981_covid_epic.txt"
corrida$arch_testing2  <- "./work/E4008/E4008_982_pre_covid_epic.txt"

#leo los datasets
resultados_testing1  <- fread( corrida$arch_testing1 )
resultados_testing2  <- fread( corrida$arch_testing2 )


#divido por un millon para visualizar mas facil
resultados_testing1[   , ganancia  := ganancia/1e6 ]
resultados_testing2[   , ganancia  := ganancia/1e6 ]


#Sobre el mismo experimento
#Deberia dar que es lo mismo !
wilcox.test(  resultados_testing1[ oficial==1, ganancia ][ 1:25],
              resultados_testing1[ oficial==1, ganancia ][26:50] )


#Sobre el experimento 1 y el experimento 2
#Deberia dar que son distintos
wilcox.test(  resultados_testing1[ oficial==1, ganancia ],
              resultados_testing2[ oficial==1, ganancia ] )

mean( resultados_testing1[oficial==1, ganancia] )
mean( resultados_testing2[oficial==1, ganancia] )
