require("data.table")
require("ggplot2")


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

setwd( "~/buckets/b1/" )

#leo el dataset , aqui se puede usar algun super dataset con Feature Engineering
datasetOri  <- fread( "datasetsOri/paquete_premium.csv.gz")
datasetOri  <-  datasetOri[ foto_mes>=202001  & foto_mes<=202011, ]
gc()

dataset  <- fread( "datasets/dataset_clusterizado_v100.csv.gz")
datasetOri <- merge(datasetOri, dataset[, .SD, .SDcols= c("numero_de_cliente", "cluster2")], by = "numero_de_cliente")
seguimiento <- datasetOri[numero_de_cliente %in% datasetOri[foto_mes == 202011]$numero_de_cliente & foto_mes <= 202011, .SD, .SDcols = -c("clase_ternaria")]

rm(datasetOri, dataset)
gc()


means <- seguimiento[, lapply(.SD, mean, na.rm=TRUE), by=list(foto_mes, cluster2) , .SDcols=-c("numero_de_cliente")]
setorder(means, cols = foto_mes)

campos_buenos <- setdiff(colnames(means), c("foto_mes", "cluster2"))
cluster_sizes <- seguimiento[ foto_mes==202011 , .N,  cluster2 ]  #tamaño de los clusters

pdf( paste0( paste0("./work/clusters_variables.pdf" ) ), 12, 8)
#campo = "ctrx_quarter"
for( campo in  campos_buenos ) {
  tbl <- means[,.SD, .SDcols = c("foto_mes", "cluster2", campo)]
  print(
    ggplot(tbl,
      aes(x=as.Date(paste0(as.character(foto_mes), '01'), format='%Y%m%d'),
        y=get(campo),
        color=as.character(cluster2)
      )
    ) +
    geom_line() +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%Y%m") +
    ggtitle(paste0("Promedio de clusters - ", campo, " - ", paste(cluster_sizes$N, collapse = ", "))) +
        xlab("Período") +
        ylab(campo)
  )
}

dev.off()

