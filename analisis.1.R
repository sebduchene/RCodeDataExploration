#Analisis desplazamientos
setwd("~/Desktop/blog/")
desp <- read.table("desplazamientoExpulsionAS.csv", sep=",", head=T)
deptos <- names(table(desp$Departamento))


datos_deptos <- matrix(0, nrow=length(deptos), ncol=1)
datos_deptos <- data.frame(datos_deptos, stringsAsFactors=F)
datos_deptos[,1] <- deptos

for(j in 3:15){
    datos_temp <- vector()
    for(i in 1:length(deptos)){
        datos_temp[i] <-  sum(desp[,j][desp$Departamento==deptos[i] & is.na(desp[,j])==F  ] )
    }
    datos_deptos <- cbind(datos_deptos, datos_temp)
}

colnames(datos_deptos) <- c("departamento", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011.1")

datos_deptos$departamento <- toupper(datos_deptos$departamento)

library(maptools)

mapa <- readShapePoly("~/Desktop/blog/MapaDepartamental/departamental.shp")



datos_deptos$departamento[datos_deptos$depar=="#N/A"] <- "NN"

deptos_no_match <-  datos_deptos$departamento[!((datos_deptos$departamento %in% levels(attr(mapa, "data")$NOM)[as.numeric(attr(mapa, "data")$NOM)]))]
deptos_mapa <- levels(attr(mapa, "data")$NOM)[as.numeric(attr(mapa, "data")$NOM)]

# [1] "ARCHIPI\303\251LAGO DE SAN ANDR\303\251S"
# [2] "ATL\303\241NTICO"
# [3] "BOGOT\303\241 D.C."
# [4] "BOL\303\255VAR"
# [5] "BOYAC\303\241"
# [6] "CAQUET\303\241"
# [7] "CHOC\303\263"
# [8] "C\303\263RDOBA"
# [9] "GUAIN\303\255A"
#[10] "NARI\303\261O"
#[11] "VAUP\303\251S"

datos_deptos[datos_deptos==grep(substr(deptos_mapa[1], 1,3),deptos_mapa,value=T )]

for( i in 1:7){
    to_replace <- which(datos_deptos$departamento==deptos_no_match[i])
    replacement_str <-  grep(paste("^",substr(deptos_no_match[i], 1,3)[1], sep=""), deptos_mapa, value=T)
    datos_deptos$departamento[to_replace] <- replacement_str
}

for( i in 9:11){
    to_replace <- which(datos_deptos$departamento==deptos_no_match[i])
    replacement_str <-  grep(paste("^",substr(deptos_no_match[i], 1,3)[1], sep=""), deptos_mapa, value=T)
    datos_deptos$departamento[to_replace] <- replacement_str
}

datos_deptos$departamento[datos_deptos$departamento=="C\303\263RDOBA"] <- deptos_mapa[8]

datos_deptos_original <- datos_deptos

datos_deptos <- matrix(NA, nrow(datos_deptos_original), ncol(datos_deptos_original))

datos_deptos <- as.data.frame(datos_deptos)
colnames(datos_deptos) <- colnames(datos_deptos_original)

for(i in 1:length(deptos_mapa)){
    datos_deptos[i, ] <- datos_deptos_original[datos_deptos_original$departamento==deptos_mapa[i],]
}

#########
#Editar esta parte, categorizar estadisticas en grupos discretos para asignar colores

datos_deptos_mat <- as.matrix(datos_deptos[,-1])

datos_cut <-cut(datos_deptos_mat, c(-Inf, 100, 1000, 10000, 50000 ,Inf))
datos_cut_mat <- matrix(as.numeric(datos_cut), nrow=34)
datos_cut_mat <- cbind(datos_deptos[,1], datos_cut_mat)




#cols <- heat.colors(nrow(datos_deptos))

#cols <- heat.colors(34)
library(RColorBrewer)
colors <- brewer.pal(5, "Reds")
pal <- colorRampPalette(colors)
cols <- pal(5)
#cols <- cols[seq(from=1, to=30, by=3)]

tabla_colores <- matrix(NA, 34, 14)
tabla_colores[,1] <- datos_deptos$departamento

#tabla_colores[-1,] <- cols[order(datos_deptos[-1,], decreasing=T)]
#tabla_colores[,-1] <- cols[-rank(datos_deptos[,-1])*10]

for(i in 2:ncol(tabla_colores)){
    tabla_colores[,i] <- cols[as.numeric(datos_cut_mat[,i])]
}
#for(i in 2:ncol(tabla_colores)){
#    tabla_colores[,i] <- cols[datos_deptos[,i]]
#}



write.table(datos_deptos, file="desp_depto.csv", sep=",")
write.table(datos_cut_mat, file="datos_cortados.csv", sep=",")


par(mar=c(0.5,0.5,0.5,0.5))
for(i in 2:13){
    jpeg(paste(colnames(datos_deptos)[i], ".jpg", sep=""))
    plot(mapa, col=tabla_colores[,i], xlim=c(20000, 3000000), bg="black", axes=T)
    title(paste("Total desplazados en", colnames(datos_deptos)[i]))
    legend(x=1500000, y=2200000, legend=c("<100", "100-1,000", "1,000-10,000", "10,000-50,000", ">50,000"), fill=cols, text.col="white", text.width=4, bty="n", text.font=2)
    dev.off()
}

######
datos_cut_ind <- matrix(NA, nrow=nrow(datos_deptos), ncol=ncol(datos_deptos)-1)

for(i in 1:(ncol(datos_deptos)-1)){
    datos_cut_ind[,i] <- as.numeric(cut(datos_deptos[,i+1], quantile(datos_deptos[,i+1])))
}
datos_cut_ind <- cbind(datos_deptos[,1] , datos_cut_ind)
datos_cut_ind[is.na(datos_cut_ind)] <- "1"

colors <- brewer.pal(5, "Blues")
pal <- colorRampPalette(colors)
cols <- pal(5)

tabla_colores <- matrix(NA, 34, 14)
tabla_colores[,1] <- datos_deptos$departamento

for(i in 2:ncol(tabla_colores)){
    tabla_colores[,i] <- cols[as.numeric(datos_cut_ind[,i])]
}

par(mar=c(0.5,0.5,0.5,0.5))
for(i in 2:13){
    jpeg(paste(colnames(datos_deptos)[i],".proporcion.", ".jpg", sep=""))
    plot(mapa, col=tabla_colores[,i], xlim=c(20000, 3000000), bg="black")
    title(paste("Desplazados anuales por percentiiles en", colnames(datos_deptos)[i]))
    legend(x=1500000, y=2200000, legend=c("0","25", "50", "75", "100"), fill=cols, text.col="white", text.width=4, bty="n", text.font=2, title="Percentiles")
    dev.off()
}

