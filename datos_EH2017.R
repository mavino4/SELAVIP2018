library(readr)
require(data.table)
setwd("/media/marco/165d5eb4-ca0f-4d82-a5d6-3052d1663348/TECHO/SELAVIP")
personas <- read_delim("CSV/EH2017_Persona.csv",
                  ";", escape_double = FALSE, trim_ws = TRUE)

personas <- personas[,c("folio","nro","area","yhog" , "yhogpc" , "factor", "niv_ed" , "condact" , "s02a_02", "s06b_16","s06b_17","s06b_18","s06b_19","s06b_19a")]
personas <- as.data.table(personas)
personas <- personas[personas[, .I[nro == max(nro)], by=folio]$V1]
personas$sqrtNro <- sqrt(personas$nro)
head(personas)

#Uniendo las bases de datos
general <- personas
general_factor <- general[rep(seq_len(nrow(general)), general$factor),]
general_Urbano <- general_factor[which( general_factor$area == 1),]
general_Urbano$yhog[is.na(general_Urbano$yhog)] <- 0
head(general_Urbano)


quantile(general_Urbano$yhog, prob = seq(0, 1, length = 11), type = 5)

general_Urbano$yhogpc[is.na(general_Urbano$yhogpc)] <- 0

general_Urbano <- within(general_Urbano, div <- as.integer(cut(yhogpc, quantile(yhogpc, probs=0:10/10), include.lowest=TRUE)))
general_Urbano <- within(general_Urbano, quartile <- as.integer(cut(yhogpc, quantile(yhogpc, probs=0:4/4), include.lowest=TRUE)))
general_Urbano <- within(general_Urbano, quintile <- as.integer(cut(yhogpc, quantile(yhogpc, probs=0:5/5), include.lowest=TRUE)))


# indicadores ingreso 
mean(general_Urbano[which(general_Urbano$div ==1),]$yhog)
hist(general_Urbano[which(general_Urbano$quartile ==1),]$yhog)
hist(general_Urbano[which(general_Urbano$quintile ==1),]$yhog)

# indicadores porcentaje de jefes de hogar mujeres 
table(general_Urbano[which(general_Urbano$quintile ==1),]$s02a_02)
table(general_Urbano[which(general_Urbano$quartile ==1),]$s02a_02)
table(general_Urbano[which(general_Urbano$div ==1),]$s02a_02)

#Número de integrantes promedio 
mean(general_Urbano[which(general_Urbano$div ==1),]$nro)
mean(general_Urbano[which(general_Urbano$quartile ==1),]$nro)
mean(general_Urbano[which(general_Urbano$quintile ==1),]$nro)

table(general_Urbano$nro)

# Empleo informal
table(general_Urbano[which(general_Urbano$div ==1),]$s06b_17)
table(general_Urbano[which(general_Urbano$quartile ==1),]$s06b_17)
table(general_Urbano[which(general_Urbano$quintile ==1),]$s06b_17)

table(general_Urbano[which(general_Urbano$div ==1),]$s06b_19)
table(general_Urbano[which(general_Urbano$quartile ==1),]$s06b_19)
table(general_Urbano[which(general_Urbano$quintile ==1),]$s06b_19)


vivienda <- read_delim("CSV/EH2017_Vivienda.csv",
                       ";", escape_double = FALSE, trim_ws = TRUE)

total <- merge(vivienda, personas, by = "folio")
total <- total[which(total$area.x == 1),]

total$yhogpc[is.na(total$yhogpc)] <- 0
total <- within(total, div <- as.integer(cut(yhogpc, quantile(yhogpc, probs=0:10/10), include.lowest=TRUE)))
total <- total[rep(seq_len(nrow(total)), total$factor.x),]

table(total[which(total$div== 1),]$s01a_16)
names <- c("Banio o pozo con descarga", "Letrina de pozo ciego", "POzo abierto, sin piso" ,
           "Banio ecologico", "ninguno")
table(total[which(total$div== 1),]$s01a_16)
table(total[which(total$div== 1),]$s01a_25)
prop.table(table(total[which(total$div== 3),]$s01a_25))


table(total[which(total$div== 1),]$s01a_15)
prop.table(table(total[which(total$div== 3),]$s01a_15))

names = c("Propia y totalmente pagada", "Propia y la estan pagando", "Alquilada", "Mixto", "Anticrético", "Cedida por servicios", "Prestada (parientes o amigos)", "Otra")
total$s01a_02 = factor(total$s01a_02)
levels(total$s01a_02) <- names
table(total[which(total$div== 1),]$s01a_02)
prop.table(table(total[which(total$div== 3),]$s01a_02))
