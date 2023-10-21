# -----------------------------------------------------------------------------
# Galilea Orellana
# 21 de octubre 2023

# Abrir el script .txt
ins_ta <- read.table("/Users/orchidaceae/Desktop/Tagua.Research/insectos_tagua.txt",header=T,sep="\t",dec=",")

# Descargar y cargar paquetes importantes
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(lubridate)

# Crear una nueva columna que contenga el total de conteos de insectos por cada fila
ins_ta$total <- rowSums(ins_ta[, 12:88], na.rm = TRUE)

# Transformar formato de character a formato de hora en hora_retiro 
ins_ta$hora_retiro <- as.POSIXct(ins_ta$hora_retiro,format="%H:%M")
ins_ta$hora_retiro <- hms::as_hms(ins_ta$hora_retiro)

# Transformar de horas a minutos para poder graficar la regresión en ambos sexos
ins_ta$minuto = as.numeric(difftime(ins_ta$hora_retiro, hms::as_hms(00:00:00), units = "mins")) 
class(ins_ta$minuto)

# Crear un subset que solo tenga los conteos de flores femeninas y otro solo de masculinas
Femenino <- subset(ins_ta, sexo == "Femenino")
Masculino <- subset(ins_ta, sexo == "Masculino")

# Probar normalidad para ambos dataframes
# No son normales pero lo vamos a ignorar momentáneamente
# Para seguir con el ejercicio
hist(Femenino$total)
hist(Masculino$total)

# Probar linearidad para ambos dataframes
# Tampoco son lineares, pero seguiremos con el ejercicio
plot(total ~ minuto, data = Femenino, type = "p")
plot(total ~ minuto, data = Masculino, type = "p")

# Modelo lineal aplicado a los subset de ambos sexos
lm_F <- lm(total ~ minuto, data = Femenino)
lm_M <- lm(total ~ minuto, data = Masculino)

# Resumen de los modelos lineales de ambos sexos
summary(lm_F)
summary(lm_M)

# Histograma para verificar normalidad en los datos crudos de ambos sexos
# y en los residuos del modelo lineal
hist(lm_F$residuals)
hist(lm_M$residuals)

# Probar homocedasticidad para los modelos lineares de ambos sexos
par(mar = c(4, 4, 2, 2))  
plot(lm_F)

par(mar = c(4, 4, 2, 2))  
plot(lm_M)

# Gráfico del modelo lineal para flores femeninas y masculinas
ggplot(Femenino, aes(minuto, total)) + geom_point() + 
  geom_smooth( method ="lm", col="blue") +  # Añadir la línea de regresión al gráfico
  stat_regline_equation(label.x = 800, label.y = 1500) +  # Añadir la ecuación
  labs(x = "Hora de Retiro", y ="Total de Insectos") +  
  ggtitle("Total de Insectos en inflorescencias femeninas") + 
  theme_classic()

ggplot(Masculino, aes(minuto, total)) + geom_point() + 
  geom_smooth( method ="lm", col="blue") +  # Añadir la línea de regresión al gráfico
  stat_regline_equation(label.x = 1000, label.y = 1300) +  # Añadir la ecuación
  labs(x = "Hora de Retiro", y ="Total de Insectos") +  
  ggtitle("Total de Insectos en inflorescencias femeninas") + 
  theme_classic()















