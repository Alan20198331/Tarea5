#Hecho por Alan E. Ramirez, 2019-8331

#Aqui vamos a instalar y leer los paquetes requeridos para que el programa funcione.
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("boot")
install.packages("car")
install.packages("QuantPsyc")
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(boot)
library(car)
library(QuantPsyc)

#Luego, vamos a pasar los datos de nuestra tabla a nuestro programa convirtiendolo en variables.

Datos <- read_csv2("https://docs.google.com/uc?export=download&id=1I400U6fRUDOyC2Z78B-5NkJjbQeZClLT")

#Vamos ahora a confirmar si la variable ha recibido los datos exitosamente.

View(Datos)

#Voy a renombrar las columnas para que sea mas facil hacer las graficas.

Datos = Datos[complete.cases(Datos),]
names(Datos)=c("PUBLICIDAD","VENTAS")

View(Datos)

#Ahora que tenemos nuestra tabla, podemos comenzar nuestro ejercicio.

attach(Datos)

#Buscamos la Media, Mediana, y Moda del presupuesto inicial de las ventas.

mean(VENTAS) #La media es 193.2
median(VENTAS) #La mediana es 200
table(VENTAS)
which.max(table(VENTAS)) #La moda es 230, se repite 22 veces

#Utilizaremos la funcion LM para crear un modelo el cual utilizaremos para nuestra prediccion.

model1= lm(VENTAS ~ PUBLICIDAD, data = Datos, no.action= no.exclude)

summary(model1)

#Ahora que sabemos los datos de nuestro modelo, vamos a hacer nuestra grafica incluyendo la recta de regresion lineal

Grafica1= ggplot(Datos, aes(PUBLICIDAD,VENTAS))
Grafica1 + geom_point()
Grafica1 + geom_point() + geom_smooth(method = "lm", colour = "Red ")

#7.742e-05 es la pendiente de la linea de regresion lineal.
#El intercepto es 1.519

cor.test(PUBLICIDAD, VENTAS)

#Nuestros elementos poseen una correlacion media.

#Ahora, haremos prediciones a base del modelo que tenemos.

predicciones <- predict (model1)

view(predicciones)

#Estas son las predicciones generadas del pronostico de las ventas a partir de la publicidad de nuestra tabla.
#Podemos representar estas predicciones en la siguiente grafica:

ggplot(Datos, aes(x=PUBLICIDAD, y=VENTAS)) +
  geom_smooth(method="lm", se=FALSE, color="lightgrey") +
  geom_segment(aes(xend=PUBLICIDAD, yend=predicciones), col='red', lty='dashed') +
  geom_point() +
  geom_point(aes(y=predicciones), col='red') +
  theme_light()

#Probamos predecir algunos componentes individuales.

predict(model1, data.frame(PUBLICIDAD = 985685, VENTAS = 120)) #Nuestro resultado es 228.1923 (El segundo componente)
predict(model1, data.frame(PUBLICIDAD = 471814, VENTAS = 70)) #Nuestro resultado es 188.4073 (El septimo componente)
predict(model1, data.frame(PUBLICIDAD = 251192, VENTAS = 150))#Nuestro resultado es 171.3262  (El 13vo componente)

#Esto nos deja comprobar que nuestras predicciones son las mismas que nuestra tabla, nuestras predicciones estan completas.


