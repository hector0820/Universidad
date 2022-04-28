# Dirección del archivo

## Características Personales
cp <- "/home/hector/Universidad/Econometria/conjunto_de_datos_enut_2019/conjunto_de_datos_tmodulo_enut_2019/conjunto_de_datos/conjunto_de_datos_tmodulo_enut_2019.csv"

## Cararcterísticas para todos
ct <- "/home/hector/Universidad/Econometria/conjunto_de_datos_enut_2019/conjunto_de_datos_tsdem_enut_2019/conjunto_de_datos/conjunto_de_datos_tsdem_enut_2019.csv"

# Cargar el data frame
cape <- read.csv(file = cp, fileEncoding = "utf-8")
cato <- read.csv(file = ct, fileEncoding = "utf-8")

# Eliminar variables
remove(cp)
remove(ct)

# Convertir en caracteres
for (i in names(cape)[1:4]){
  cape[,i] <- as.character(cape[,i])
}

for (i in names(cato)[1:4]){
  cato[,i] <- as.character(cato[,i])
}

for (i in names(cape)[c(2,4)]){
  cape[,i][nchar(cape[,i]) < 2 ] <-
    paste0("0", cape[,i][nchar(cape[,i]) < 2 ])
}

for (i in names(cato)[c(2,4)]){
  cato[,i][nchar(cato[,i]) < 2 ] <-
    paste0("0", cato[,i][nchar(cato[,i]) < 2 ])
}

n <- 2

while(range(nchar(cape[,1]))[1] != range(nchar(cape[,1]))[2]){
  cape[,1][nchar(cape[,1]) < n ] <-
    paste0("0", cape[,1][nchar(cape[,1]) < n ])
  n <- n + 1
}

n <- 2

while(range(nchar(cato[,1]))[1] != range(nchar(cato[,1]))[2]){
  cato[,1][nchar(cato[,1]) < n ] <-
    paste0("0", cato[,1][nchar(cato[,1]) < n ])
  n <- n + 1
}

# Concatenar
cape$id <- NULL
cato$id <- NULL

for (i in names(cape)[1:4]){
  cape$id <- paste0(cape$id, cape[,i])
}

for (i in names(cato)[1:4]){
  cato$id <- paste0(cato$id, cato[,i])
}

uno <- cato[c(5:13,19)]
dos <- cape[c(8:548,553)]
final <- merge(x=uno, y=dos, by.x="id", by.y="id")

library(dplyr)
mj <- filter(final, SEXO == "2", PAREN < 3, P5_1 == "1")
hm <- filter(final, SEXO == "1", PAREN < 3, P5_1 == "1")

mj$id <- paste0("m", mj$id)
hm$id <- paste0("h", hm$id)

excluidos <- NULL
trabajo_no_remunerado <- NULL
si_o_no <- NULL
preguntas <- names(mj[71:275])

for(i in preguntas){
  if(nchar(i) > 8){
    trabajo_no_remunerado <- c(trabajo_no_remunerado, i)
  } else {
    si_o_no <- c(si_o_no, i)
  }
}

ht_m <- mj[,trabajo_no_remunerado][,c(TRUE,FALSE)] * 60
m_t <- rowSums(ht_m, na.rm = TRUE)

ht_m <- mj[,trabajo_no_remunerado][,!c(TRUE,FALSE)] 
m_t <- (m_t + rowSums(ht_m, na.rm = TRUE) )/60
mj$tnp <- m_t #Trabajo no pagado en casa (tnp)

# Hombres

excluidos <- NULL
trabajo_no_remunerado <- NULL
si_o_no <- NULL
preguntas <- names(hm[71:275])

for(i in preguntas){
  if(nchar(i) > 8){
    trabajo_no_remunerado <- c(trabajo_no_remunerado, i)
  } else {
    si_o_no <- c(si_o_no, i)
  }
}

ht_h<- hm[,trabajo_no_remunerado][,c(TRUE,FALSE)] * 60
m_t <- rowSums(ht_h, na.rm = TRUE)

ht_h <- hm[,trabajo_no_remunerado][,!c(TRUE,FALSE)] 
m_t <- (m_t + rowSums(ht_h, na.rm = TRUE) )/60
hm$tnp <- m_t #Trabajo no pagado en casa (tnp)

write.csv(mj, file ="mj.csv")
write.csv(hm, file ="mj.csv")