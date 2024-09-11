# Data Prep
data = read.csv("/Users/samuelmarzano/Documents/proy/full_1_202324.csv", 
                  as.is = TRUE)


library(dplyr)

# Assuming your data frame is named df
data <- data %>% 
  filter(Resultado %in% c("W", "L"))
day_mapping <- c("V", "S", "D", "L", "M", "X", "J")
data$Dia <- match(data$Dia, day_mapping)

unique(data$Equipo)
cosa <- c("Almería" =1,"Rayo"=2,"Sevilla"=3,"Valencia"=4,"Real Sociedad"=5,"Girona"=6,"Las Palmas"=7,"Mallorca"=8,"Athletic"=9,"Real Madrid"=10,"Celta"=11,"Osasuna"=18 ,"Villarreal"=19,"Real Betis"=20,   
 "Getafe"=12,"FC Barcelona"=13,"Cádiz"=14,"Alavés"=15,"Atlético"=16,"Granada"=17, "Espanyol"=21, "Valladolid"=22,"Elche"=23,"Levante"=24,"Eibar"=25,"Huesca"=26,"Leganés"=27)
data$Equipo <- cosa[data$Equipo]

data$Oponente <- cosa[data$Oponente]

casa <- c('Si'=1, 'No'=0)
data$Casa <- casa[data$Casa]

resul <- c('W'=2, 'D'=1, 'L'=0)
data$Resultado <- resul[data$Resultado]


convert_time_to_minutes <- function(time_string){
time_string <- strsplit(time_string,':')
time_df <- do.call(rbind, time_string)
time = as.numeric(time_df[1])*60 + as.numeric(time_df[2])
return(time)
}

data$Hora <- sapply(data$Hora, convert_time_to_minutes)



#PLS

# Assuming your dataframe is named df
data <- subset(data, Jornada > 10)

factor(data$Resultado)



keep_y <- ('Resultado')
Y<- data[,keep_y]

keep_x<- c("Puntos","GD","GF.game","GA.game","Posesion.game","Remates.a.Puerta.game",        
            "Remates.Fuera.game","Remates.de.Palo.game","Asistencias.game",         
            "Asistencias.de.gol.game" ,      "Faltas.Cometidas.game"    ,     "Faltas.Recibidas.game"     ,   
            "Tarjetas.Amarillas.game",       "Tarjetas.Rojas.game"   ,        "Pases.Correctos.game"   ,      
            "Pases.Fallados.game"      ,     "Fueras.de.Juego.game"  ,        "Paradas.game"              ,   
            "Corners.game"          ,        "Penaltis.a.Favor.game"       ,  "Penaltis.en.Contra.game"     , 
            "Presición.de.Pases.game" ,      "Remates.Totales.game" ,         "Conversión.Tiros.Totales.game",
            "Conversión.Tiros.Puerta.game", "Jornada", "Dia", "Hora", "Equipo", "Oponente", "Casa", "pos")
X <- data[,keep_x]


data2 <- X
data2$Resultado <- Y
library(caret)
set.seed(100)
trainFilas = createDataPartition(data2$Resultado, p=0.8, list=FALSE)
Xtrain = subset(data2[trainFilas,], select = -Resultado)
ytrain = data2$Resultado[trainFilas]
Xtest = subset(data2[-trainFilas,], select = -Resultado)
ytest = data2$Resultado[-trainFilas]

library(ropls)
myplsda = opls(x = Xtrain, y = ytrain, predI = NA, crossvalI = 10, 
               scaleC = "standard", fig.pdfC = "none")

plot(1:maxNC, myplsda@modelDF$`R2Y(cum)`, type = "o", pch = 16, col = "blue3",
     lwd = 2, xlab = "Components", ylab = "", ylim = c(0,0.8),
     main = "PLS-DA model: Breast cancer")
lines(1:maxNC, myplsda@modelDF$`Q2(cum)`, type = "o", pch = 16, col = "red3",
      lwd = 2)
abline(h = 0.5, col = "red3", lty = 2)
legend("bottomleft", c("R2Y", "Q2"), lwd = 2, 
       col = c("blue3", "red3"), bty = "n")
myplsda = opls(x = Xtrain, y = ytrain, predI = 4, crossvalI = 10, 
               scaleC = "standard", fig.pdfC = "none")
mypred = predict(myplsda)
library(caret)

caret::confusionMatrix(factor(ytrain),factor(mypred))



#otra idea


data = read.csv("/Users/samuelmarzano/Documents/proy/full_1_202324.csv",,
                as.is = TRUE)


keep_x<- c("Puntos","GD","GF.game","GA.game","Posesion.game","Remates.a.Puerta.game",        
           "Remates.Fuera.game","Remates.de.Palo.game","Asistencias.game",         
           "Asistencias.de.gol.game" ,      "Faltas.Cometidas.game"    ,     "Faltas.Recibidas.game"     ,   
           "Tarjetas.Amarillas.game",       "Tarjetas.Rojas.game"   ,        "Pases.Correctos.game"   ,      
           "Pases.Fallados.game"      ,     "Fueras.de.Juego.game"  ,        "Paradas.game"              ,   
           "Corners.game"          ,        "Penaltis.a.Favor.game"       ,  "Penaltis.en.Contra.game"     , 
           "Presición.de.Pases.game" ,      "Remates.Totales.game" ,         "Conversión.Tiros.Totales.game",
           "Conversión.Tiros.Puerta.game", "Jornada", "Dia", "Hora", "Equipo", "Oponente", "Casa", "pos")
keep_y <- c('GF','GA')

Y<- data[,keep_y]
X <- data[,keep_x]

day_mapping <- c("V", "S", "D", "L", "M", "X", "J")
data$Dia <- match(X$Dia, day_mapping)

cosa <- c("Almería" =1,"Rayo"=2,"Sevilla"=3,"Valencia"=4,"Real Sociedad"=5,"Girona"=6,"Las Palmas"=7,"Mallorca"=8,"Athletic"=9,"Real Madrid"=10,"Celta"=11,"Osasuna"=18 ,"Villarreal"=19,"Real Betis"=20,   
          "Getafe"=12,"FC Barcelona"=13,"Cádiz"=14,"Alavés"=15,"Atlético"=16,"Granada"=17, "Espanyol"=21, "Valladolid"=22,"Elche"=23,"Levante"=24,"Eibar"=25,"Huesca"=26,"Leganés"=27)
X$Equipo <- cosa[data$Equipo]

X$Oponente <- cosa[data$Oponente]

casa <- c('Si'=1, 'No'=0)
X$Casa <- casa[data$Casa]

convert_time_to_minutes <- function(time_string){
  time_string <- strsplit(time_string,':')
  time_df <- do.call(rbind, time_string)
  time = as.numeric(time_df[1])*60 + as.numeric(time_df[2])
  return(time)
}

X$Hora <- sapply(X$Hora, convert_time_to_minutes)

data2 <- X
data2$GF <- Y$GF
data2$GA <- Y$GA
library(caret)
set.seed(100)
trainFilas = createDataPartition(data$Temporada, p=0.8, list=FALSE)
Xtrain = subset(data2[trainFilas,])
Xtrain = Xtrain[,keep_x]
ytrain = Y[trainFilas,]
Xtest = subset(X[-trainFilas,])
ytest = Y[-trainFilas,]

Xtrain$Dia <- match(Xtrain$Dia, day_mapping)
library(ropls)
myplsda = opls(x = Xtrain, y = as.matrix(ytrain), predI = NA, crossvalI = 10, 
               scaleC = "standard", fig.pdfC = "none")

plot(1:16, myplsda@modelDF$`R2Y(cum)`, type = "o", pch = 16, col = "blue3",
     lwd = 2, xlab = "Components", ylab = "", ylim = c(0,0.8),
     main = "PLS-DA model: Breast cancer")
lines(1:maxNC, myplsda@modelDF$`Q2(cum)`, type = "o", pch = 16, col = "red3",
      lwd = 2)
abline(h = 0.5, col = "red3", lty = 2)
legend("bottomleft", c("R2Y", "Q2"), lwd = 2, 
       col = c("blue3", "red3"), bty = "n")
myplsda = opls(x = Xtrain, y = as.matrix(ytrain), predI = 6, crossvalI = 10, 
               scaleC = "standard", fig.pdfC = "none")
mypred = predict(myplsda)

# Using Metrics package
# install.packages("Metrics")
library(Metrics)

ytrain$GA
# MAE, MSE, RMSE, MAPE
mae(ytrain$GA, mypred$GA)
mse(ytrain$GA, mypred$GA)
rmse(ytrain$GA, mypred$GA)
mape(ytrain$GA, mypred$GA)


mypred$GF<-round(mypred$GF)
mypred$GA<-round(mypred$GA)


# Assuming you have a data frame df with 'goals_scored' and 'goals_against'
library(dplyr)

mypred <- mypred %>%
  mutate(Resultado = case_when(
    GF > GA ~ "W",   # Win
    GF < GA ~ "L",   # Loss
    TRUE ~ "D"                            # Draw
  ))

prueba <- data[trainFilas,]

caret::confusionMatrix(factor(prueba$Resultado),factor(mypred$Resultado))


#PCA and relations

data = read.csv("/Users/samuelmarzano/Documents/proy/full_1_202324.csv", 
                as.is = TRUE)

keep<- c("Puntos","GD","GF.game","GA.game","Posesion.game","Remates.a.Puerta.game",        
           "Remates.Fuera.game","Remates.de.Palo.game","Asistencias.game",         
           "Asistencias.de.gol.game" ,      "Faltas.Cometidas.game"    ,     "Faltas.Recibidas.game"     ,   
           "Tarjetas.Amarillas.game",       "Tarjetas.Rojas.game"   ,        "Pases.Correctos.game"   ,      
           "Pases.Fallados.game"      ,     "Fueras.de.Juego.game"  ,        "Paradas.game"              ,   
           "Corners.game"          ,        "Penaltis.a.Favor.game"       ,  "Penaltis.en.Contra.game"     , 
           "Presición.de.Pases.game" ,      "Remates.Totales.game" ,         "Conversión.Tiros.Totales.game",
           "Conversión.Tiros.Puerta.game", "Jornada", "Dia", "Hora", "Equipo", "Oponente", "Casa", "pos", "Resultado")



rownames(descCere) = descCere$variable
all <- data[,keep]

descCere = data.frame("variable" = colnames(all),
                      "tipo" = c(rep("numerical", 26), "categorical", 
                                 rep("numerical", 1), 'categorical','categorical','categorical','numerical','categorical'), stringsAsFactors = FALSE)


categ = descCere$variable[descCere$tipo == "categorical"]
for (cc in categ) {
  all[,cc] = factor(all[,cc])
}
data <- data %>% 
  filter(Resultado %in% c("W", "L"))
day_mapping <- c("V", "S", "D", "L", "M", "X", "J")
all$Dia <- match(all$Dia, day_mapping)


cosa <- c("Almería" =1,"Rayo"=2,"Sevilla"=3,"Valencia"=4,"Real Sociedad"=5,"Girona"=6,"Las Palmas"=7,"Mallorca"=8,"Athletic"=9,"Real Madrid"=10,"Celta"=11,"Osasuna"=18 ,"Villarreal"=19,"Real Betis"=20,   
          "Getafe"=12,"FC Barcelona"=13,"Cádiz"=14,"Alavés"=15,"Atlético"=16,"Granada"=17, "Espanyol"=21, "Valladolid"=22,"Elche"=23,"Levante"=24,"Eibar"=25,"Huesca"=26,"Leganés"=27)
all$Equipo <- cosa[all$Equipo]

all$Oponente <- cosa[all$Oponente]

casa <- c('Si'=1, 'No'=0)
all$Casa <- casa[all$Casa]

resul <- c('W'=2, 'D'=1, 'L'=0)
all$Resultado <- resul[all$Resultado]

convert_time_to_minutes <- function(time_string){
  time_string <- strsplit(time_string,':')
  time_df <- do.call(rbind, time_string)
  time = as.numeric(time_df[1])*60 + as.numeric(time_df[2])
  return(time)
}

all$Hora <- sapply(all$Hora, convert_time_to_minutes)
library(FactoMineR)
library(factoextra)
res.pca = PCA(all, scale.unit = TRUE, graph = FALSE, ncp = 10)
eig.val <- get_eigenvalue(res.pca)
VPmedio = 100 * (1/nrow(eig.val))
fviz_eig(res.pca, addlabels = TRUE) +
  geom_hline(yintercept=VPmedio, linetype=2, color="red")

K = 3
res.pca = PCA(all, scale.unit = TRUE, graph = FALSE, ncp = K)

fviz_pca_var(res.pca, axes = c(1,2), repel = TRUE, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


#otro
data = read.csv("/Users/samuelmarzano/Documents/proy/full_1_202324.csv", 
                as.is = TRUE)

keep<- c("Puntos","GD","GF.game","GA.game","Posesion.game","Remates.a.Puerta.game",        
         "Remates.Fuera.game","Remates.de.Palo.game","Asistencias.game",         
         "Asistencias.de.gol.game" ,      "Faltas.Cometidas.game"    ,     "Faltas.Recibidas.game"     ,   
         "Tarjetas.Amarillas.game",       "Tarjetas.Rojas.game"   ,        "Pases.Correctos.game"   ,      
         "Pases.Fallados.game"      ,     "Fueras.de.Juego.game"  ,        "Paradas.game"              ,   
         "Corners.game"          ,        "Penaltis.a.Favor.game"       ,  "Penaltis.en.Contra.game"     , 
         "Presición.de.Pases.game" ,      "Remates.Totales.game" ,         "Conversión.Tiros.Totales.game",
         "Conversión.Tiros.Puerta.game", "Jornada", "Dia", "Hora", "Equipo", "Oponente", "Casa", "pos", "Resultado")



rownames(descCere) = descCere$variable
all <- data[,keep]

descCere = data.frame("variable" = colnames(all),
                      "tipo" = c(rep("numerical", 26), "categorical", 
                                 rep("numerical", 1), 'categorical','categorical','categorical','numerical','categorical'), stringsAsFactors = FALSE)


categ = descCere$variable[descCere$tipo == "categorical"]
for (cc in categ) {
  all[,cc] = factor(all[,cc])
}
all <- all %>% 
  filter(Resultado %in% c("W", "L"))
day_mapping <- c("V", "S", "D", "L", "M", "X", "J")
all$Dia <- match(all$Dia, day_mapping)


cosa <- c("Almería" =1,"Rayo"=2,"Sevilla"=3,"Valencia"=4,"Real Sociedad"=5,"Girona"=6,"Las Palmas"=7,"Mallorca"=8,"Athletic"=9,"Real Madrid"=10,"Celta"=11,"Osasuna"=18 ,"Villarreal"=19,"Real Betis"=20,   
          "Getafe"=12,"FC Barcelona"=13,"Cádiz"=14,"Alavés"=15,"Atlético"=16,"Granada"=17, "Espanyol"=21, "Valladolid"=22,"Elche"=23,"Levante"=24,"Eibar"=25,"Huesca"=26,"Leganés"=27)
all$Equipo <- cosa[all$Equipo]

all$Oponente <- cosa[all$Oponente]

casa <- c('Si'=1, 'No'=0)
all$Casa <- casa[all$Casa]

resul <- c('W'=2, 'D'=1, 'L'=0)
all$Resultado <- resul[all$Resultado]

convert_time_to_minutes <- function(time_string){
  time_string <- strsplit(time_string,':')
  time_df <- do.call(rbind, time_string)
  time = as.numeric(time_df[1])*60 + as.numeric(time_df[2])
  return(time)
}

all$Hora <- sapply(all$Hora, convert_time_to_minutes)
library(FactoMineR)
library(factoextra)
res.pca = PCA(all, scale.unit = TRUE, graph = FALSE, ncp = 10)
eig.val <- get_eigenvalue(res.pca)
VPmedio = 100 * (1/nrow(eig.val))
fviz_eig(res.pca, addlabels = TRUE) +
  geom_hline(yintercept=VPmedio, linetype=2, color="red")

K = 3
res.pca = PCA(all, scale.unit = TRUE, graph = FALSE, ncp = K)

fviz_pca_var(res.pca, axes = c(1,2), repel = TRUE, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
#otro
data = read.csv("/Users/samuelmarzano/Documents/proy/full_1_202324.csv", 
                as.is = TRUE)

keep<- c("Puntos","GD","GF.game","GA.game","Posesion.game","Remates.a.Puerta.game",        
         "Remates.Fuera.game","Remates.de.Palo.game","Asistencias.game",         
         "Asistencias.de.gol.game" ,      "Faltas.Cometidas.game"    ,     "Faltas.Recibidas.game"     ,   
         "Tarjetas.Amarillas.game",       "Tarjetas.Rojas.game"   ,        "Pases.Correctos.game"   ,      
         "Pases.Fallados.game"      ,     "Fueras.de.Juego.game"  ,        "Paradas.game"              ,   
         "Corners.game"          ,        "Penaltis.a.Favor.game"       ,  "Penaltis.en.Contra.game"     , 
         "Presición.de.Pases.game" ,      "Remates.Totales.game" ,         "Conversión.Tiros.Totales.game",
         "Conversión.Tiros.Puerta.game", "Jornada", "Dia", "Hora", "Equipo", "Oponente", "Casa", "pos", "Resultado")



rownames(descCere) = descCere$variable
all <- data[,keep]

descCere = data.frame("variable" = colnames(all),
                      "tipo" = c(rep("numerical", 26), "categorical", 
                                 rep("numerical", 1), 'categorical','categorical','categorical','numerical','categorical'), stringsAsFactors = FALSE)


categ = descCere$variable[descCere$tipo == "categorical"]
for (cc in categ) {
  all[,cc] = factor(all[,cc])
}
data <- data %>% 
  filter(Resultado %in% c("D"))
day_mapping <- c("V", "S", "D", "L", "M", "X", "J")
all$Dia <- match(all$Dia, day_mapping)


cosa <- c("Almería" =1,"Rayo"=2,"Sevilla"=3,"Valencia"=4,"Real Sociedad"=5,"Girona"=6,"Las Palmas"=7,"Mallorca"=8,"Athletic"=9,"Real Madrid"=10,"Celta"=11,"Osasuna"=18 ,"Villarreal"=19,"Real Betis"=20,   
          "Getafe"=12,"FC Barcelona"=13,"Cádiz"=14,"Alavés"=15,"Atlético"=16,"Granada"=17, "Espanyol"=21, "Valladolid"=22,"Elche"=23,"Levante"=24,"Eibar"=25,"Huesca"=26,"Leganés"=27)
all$Equipo <- cosa[all$Equipo]

all$Oponente <- cosa[all$Oponente]

casa <- c('Si'=1, 'No'=0)
all$Casa <- casa[all$Casa]

resul <- c('W'=2, 'D'=1, 'L'=0)
all$Resultado <- resul[all$Resultado]

convert_time_to_minutes <- function(time_string){
  time_string <- strsplit(time_string,':')
  time_df <- do.call(rbind, time_string)
  time = as.numeric(time_df[1])*60 + as.numeric(time_df[2])
  return(time)
}

all$Hora <- sapply(all$Hora, convert_time_to_minutes)
library(FactoMineR)
library(factoextra)
res.pca = PCA(all, scale.unit = TRUE, graph = FALSE, ncp = 10)
eig.val <- get_eigenvalue(res.pca)
VPmedio = 100 * (1/nrow(eig.val))
fviz_eig(res.pca, addlabels = TRUE) +
  geom_hline(yintercept=VPmedio, linetype=2, color="red")

K = 3
res.pca = PCA(all, scale.unit = TRUE, graph = FALSE, ncp = K)

fviz_pca_var(res.pca, axes = c(1,2), repel = TRUE, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


library(corrplot)
corrplot(cor(all), method="number", number.cex = 0.5, tl.cex = 0.4)
chisq.test(table(factor(data$Resultado), factor(data$Dia)))

           