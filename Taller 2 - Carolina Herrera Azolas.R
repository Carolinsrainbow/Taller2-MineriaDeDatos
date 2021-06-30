## Incorporación de datos
datos <- read.csv("/Users/user/Desktop/titanic.csv", header=TRUE)

# Instalamos los paquetes necesarios, en caso que no los tengamos instaladas
install.packages("rpart")
install.packages("rpart.plot")

# Cargamos las librerias que utilizaremos
library(rpart)
library(rpart.plot)

## Resumen estadístico
summary(datos)

## Cantidad de datos únicos por tipo de clase
unique(datos$Pclass)

## Pregunta 1 
## Específicamos la variable endógena (sobrevivir) como función de las
## las variables exogenas (Sexo, edad y tipo de cabina Pclass)

## Aplicación de Métodos:

## Índice de entropía
## S = -P0*Log2(P0)-P1*Log2(P1)

## Índice de Gini
## G0 = P0*(1-P0)+ P1*(1-P1)

## Árbol de clasificación según Entropía

modelo1 <- rpart(Survived ~ Pclass + Age + Sex,
                 data = datos,
                 method = "class",
                 parms = list(split = "information"),
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 5,
                                         cp = 0))

## Creación del gráfico Modelo 1 - Entropía
rpart.plot(modelo1)

## Resultado del primer análisis 
printcp(modelo1)

## Método de Aplicación Gini

modelo1Gini <- rpart(Survived ~ Pclass + Age + Sex,
                 data = datos,
                 method = "class",
                 parms = list(split = "gini"),
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 5,
                                         cp = 0))

## Creación del gráfico Modelo 1 - Gini
rpart.plot(modelo1Gini)

## Resultado del segundo análisis - Gini
printcp(modelo1Gini)

## Pregunta 2 
## 2.	Impacto del parámetro CP asociado al costo de complejidad

## Modelo Entropía
modelo1CP <- rpart(Survived ~ Pclass + Age + Sex,
                 data = datos,
                 method = "class",
                 parms = list(split = "information"),
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 5,
                                         cp = 0.005))
## Modelo Gini 
modelo1CPGini <- rpart(Survived ~ Pclass + Age + Sex,
                     data = datos,
                     method = "class",
                     parms = list(split = "gini"),
                     control = rpart.control(minsplit = 1,
                                             minbucket = 1,
                                             maxdepth = 5,
                                             cp = 0.005))

## Impresión de sugerencias de R para el CP
rpart.plot(modelo1CP)
printcp(modelo1CP)
plotcp(modelo1CP)

rpart.plot(modelo1CPGini)
printcp(modelo1CPGini)
plotcp(modelo1CPGini)


##Pregunta 3

## Otras combinaciones de parametros de control 

##MINSPLIT
## Minsplit determina la cantidad mínima de observaciones 
## para intentar dividir un nodo

## Modelo Entropía
modeloMinsplitEntropia <- rpart(Survived ~ Pclass + Age + Sex,
                   data = datos,
                   method = "class",
                   parms = list(split = "information"),
                   control = rpart.control(minsplit = 1,
                                           minbucket = 1,
                                           maxdepth = 5,
                                           cp = 0))

rpart.plot(modeloMinsplitEntropia)
printcp(modeloMinsplitEntropia)
plotcp(modeloMinsplitEntropia)

modeloMinsplitEntropia2 <- rpart(Survived ~ Pclass + Age + Sex,
                                data = datos,
                                method = "class",
                                parms = list(split = "information"),
                                control = rpart.control(minsplit = 3,
                                                        minbucket = 1,
                                                        maxdepth = 5,
                                                        cp = 0))

rpart.plot(modeloMinsplitEntropia2)
printcp(modeloMinsplitEntropia2)
plotcp(modeloMinsplitEntropia2)

## MINBUCKET 
## minbucket determina la cantidad mínima 
## de observaciones a tener en un nodo terminal

## Modelo Entropía

modeloMinbucketEntropia <- rpart(Survived ~ Pclass + Age + Sex,
                                data = datos,
                                method = "class",
                                parms = list(split = "information"),
                                control = rpart.control(minsplit = 1,
                                                        minbucket = 1,
                                                        maxdepth = 5,
                                                        cp = 0))

rpart.plot(modeloMinbucketEntropia)
printcp(modeloMinbucketEntropia)
plotcp(modeloMinbucketEntropia)

modeloMinbucketEntropia2 <- rpart(Survived ~ Pclass + Age + Sex,
                                 data = datos,
                                 method = "class",
                                 parms = list(split = "information"),
                                 control = rpart.control(minsplit = 1,
                                                         minbucket = 2,
                                                         maxdepth = 5,
                                                         cp = 0))

rpart.plot(modeloMinbucketEntropia2)
printcp(modeloMinbucketEntropia2)
plotcp(modeloMinbucketEntropia2)


## MAXDEPTH 
## maxdepth determina la profundidad máxima 
## del árbol (el nodo raíz cuenta como 0)

## Modelo Entropía
modeloMaxdepthEntropia <- rpart(Survived ~ Pclass + Age + Sex,
                                 data = datos,
                                 method = "class",
                                 parms = list(split = "information"),
                                 control = rpart.control(minsplit = 1,
                                                         minbucket = 1,
                                                         maxdepth = 3,
                                                         cp = 0))

rpart.plot(modeloMaxdepthEntropia)
printcp(modeloMaxdepthEntropia)
plotcp(modeloMaxdepthEntropia)

modeloMaxdepthEntropia2 <- rpart(Survived ~ Pclass + Age + Sex,
                                data = datos,
                                method = "class",
                                parms = list(split = "information"),
                                control = rpart.control(minsplit = 1,
                                                        minbucket = 1,
                                                        maxdepth = 2,
                                                        cp = 0))

rpart.plot(modeloMaxdepthEntropia2)
printcp(modeloMaxdepthEntropia2)
plotcp(modeloMaxdepthEntropia2)