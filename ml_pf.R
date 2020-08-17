### Bibliotecas (Libraries)
library(caret)
library(dplyr)
library(knitr)

### Leitura dos dados (Reading data)
setwd("/home/quarteto/Downloads")
treino <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", header = T, stringsAsFactors = F)
teste  <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", header = T, stringsAsFactors = F)
dim(treino)
dim(teste)
head(treino)
str(treino)

### Limpeza dos dados (Cleaning data)
treino <- treino[,nearZeroVar(treino, saveMetrics=TRUE)$nzv == FALSE]
teste  <- teste[,nearZeroVar(teste, saveMetrics=TRUE)$nzv == FALSE]
treino <- treino[, (colSums(is.na(treino)) == 0)]
teste  <- teste[, (colSums(is.na(teste)) == 0)]
treino <- select(treino, -c(X, user_name, cvtd_timestamp))
teste  <- select(teste, -c(X, user_name, cvtd_timestamp))

### Divisao (partioning)
set.seed(1111) 
emTreino   <- createDataPartition(treino$classe, p = 0.70, list = FALSE)
validacao <- treino[-emTreino, ]
treino    <- treino[emTreino, ]

### Predicao com metodo Random Forest (Prediction with Ramdom Forest method)
mod_rf  <- train(classe ~ ., data = treino, method = "rf")
pred_rf <- predict(mod_rf, validacao)
confusionMatrix(validacao$classe, pred_rf)
