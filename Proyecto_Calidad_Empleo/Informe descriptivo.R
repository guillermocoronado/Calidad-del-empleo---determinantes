####Entrega 2 Estadística####

rm(list = ls())
###Acondicionamiento y limpieza de datos

#Carga de paquetes y bases de datos
library(tidyverse)
library(foreign)
library(dplyr)
caracteristicas <- read.spss("Enaho01-2020-200.sav", to.data.frame = T)
educa <- read.spss("Enaho01A-2020-300.sav", to.data.frame = T)
salud <- read.spss("Enaho01A-2020-400.sav", to.data.frame = T)
empleo <- read.spss("Enaho01A-2020-500.sav", to.data.frame = T)

#Merge
names(caracteristicas)
datosA <- caracteristicas[, c(1, 3:5, 11, 17, 18, 20)]
names(educa)
datosB <- educa[, c(1, 3:5, 13, 14, 23, 564)]
names(salud)
datosC <- salud[, c(1, 3:5, 102:117)]
names(empleo)
datosD <- empleo[, c(1, 3:5, 19:21, 41, 45, 59:61, 95:98, 107, 108, 115, 116)]

#Filtrar por trabajadores ocupados
library(car)
datosD$P501n <- as.numeric(datosD$P501)
datosD$P502n <- as.numeric(datosD$P502)
datosD$P503n <- as.numeric(datosD$P503)
datosD$P501.1 <- car::recode(datosD$P501n, '1 = 2; 2 = 0; NA = 0')
datosD$P502.1 <- car::recode(datosD$P502n, '1 = 2; 2 = 0; NA = 0')
datosD$P503.1 <- car::recode(datosD$P503n, '1 = 2; 2 = 0; NA = 0')
datosD$situacion <- (datosD$P501.1+datosD$P502.1+datosD$P503.1)
datosD$situacion <- cut(datosD$situacion, breaks = c(0,1,Inf), 
                        labels = c("Desocupado", "Ocupado"))
datosD1 <- datosD %>% 
  filter(situacion == "Ocupado")

#Merge final
base1 <- merge(datosA, datosB, by = c("AÑO", "CONGLOME", "VIVIENDA", "HOGAR"))
base2 <- merge(datosC, datosD1, by = c("AÑO", "CONGLOME", "VIVIENDA", "HOGAR"))
empleo_calid <- merge(base1, datosD1, by = c("AÑO", "CONGLOME", "VIVIENDA", "HOGAR"))
empleo_calid1 <- empleo_calid %>% 
  filter(Edad > 13)


##Recodificación

#Renombrar variables
empleo_calid <- empleo_calid %>% 
  rename("Rol_hogar"="P203", "Sexo" = "P207", "Edad" = "P208A", "Estatus_marital" = "P209",
         "Lengua_materna" = "P300A", "Nivel_educa" = "P301A", "Contrato" = "P511A", 
         "Tamaño_empresa" = "P512A", "Horas" = "P520", "Deseo_horas" = "P521", 
         "Disponibilidad_horas" = "P521A", "Ingreso_liquido" = "P524E1", "Registro"="P510A1")

#Recodificación grupo de edad
empleo_calid$grupo_etario <- cut(empleo_calid$Edad, breaks = c(14,24,44,65,Inf), 
                                 labels = c("14-24 años", "25-44 años", "45-65 años",
                                            "Mayor de 65 años"))

#Recodificación presencia de contrato
empleo_calid$Contrato1 <- as.numeric(empleo_calid$Contrato)
empleo_calid$Contrato1 <- cut(empleo_calid$Contrato1, breaks = c(0, 6, 7, Inf),
                               labels = c("Con contrato", "Sin contrato", "Otro"))
prop.table(table(empleo_calid$Contrato1))*100

#Exceso de horas
empleo_calid$exceso_horas <- cut(empleo_calid$Horas, breaks = c(0, 48, Inf),
                                  labels = c("Hasta 48 horas", "Mas de 48 horas"))
prop.table(table(empleo_calid$exceso_horas))*100

#Subempleo por horas
empleo_calid$Disponibilidad_horas1 <- as.numeric(empleo_calid$Disponibilidad_horas)
table(empleo_calid$Disponibilidad_horas1)
empleo_calid$Deseo_horas1 <- as.numeric(empleo_calid$Deseo_horas)
table(empleo_calid1$Deseo_horas1)
empleo_calid$Horas1 <- cut(empleo_calid$Horas, breaks = c(0, 34, Inf),
                            labels = c("Menos de 35 horas", "De 35 horas a más"))
empleo_calid$Horas2 <- as.numeric(empleo_calid$Horas1)
table(empleo_calid1$Horas2)
empleo_calid$Horas2<- car::recode(empleo_calid$Horas2, '1 = 2; 2 = 0; NA = 0')
empleo_calid$Deseo_horas1<- car::recode(empleo_calid$Deseo_horas1, '1 = 2; 2 = 0; NA = 0')
empleo_calid$Disponibilidad_horas1<- car::recode(empleo_calid$Disponibilidad_horas1, '1 = 2; 2 = 0; NA = 0')

empleo_calid$subempleo_horas <- (empleo_calid$Horas2 + empleo_calid$Deseo_horas1 + empleo_calid$Disponibilidad_horas1)
empleo_calid$subempleo_horas <- cut(empleo_calid$subempleo_horas, breaks = c(0, 5, Inf),
                                     labels = c("No subempleado por horas", "Subempleado por horas"))
prop.table(table(empleo_calid$subempleo_horas))*100


### Construcción de indicador de calidad del empleo
1. Remuneración (mayor o menor a RMV)
empleo_calid$subempleo_ingresos <- cut(empleo_calid$Ingreso_liquido, breaks = c(0,929, Inf), 
                                       labels = c("0", "1"))
prop.table(table(empleo_calid1$subempleo_ingresos))
empleo_calid1$subempleo_ingresos <- as.numeric(empleo_calid1$subempleo_ingresos)
empleo_calid1$subempleo_ingresos <- car::recode(empleo_calid1$subempleo_ingresos, '1 = 0; 2 = 1; NA = NA')

2. Indicador de horas
prop.table(table(empleo_calid1$exceso_horasn))
prop.table(table(empleo_calid1$subempleo_horasn))

empleo_calid1$exceso_horasn <- as.numeric(empleo_calid1$exceso_horas)
empleo_calid1$subempleo_horasn <- as.numeric(empleo_calid1$subempleo_horas)
empleo_calid1$exceso_horas1 <- car::recode(empleo_calid1$exceso_horasn, '1 = 1; 2 = 0; NA = NA')
empleo_calid1$subempleo_horas1 <- car::recode(empleo_calid1$subempleo_horasn, '1 = 1; 2 = 0; NA = NA')

empleo_calid1$indhoras <- (empleo_calid1$exceso_horas1 + empleo_calid1$subempleo_horas1)
empleo_calid1$indhoras <- car::recode(empleo_calid1$indhoras, '1 = 0; 2 = 1; NA = NA')

3. Formalidad
prop.table(table(empleo_calid1$Contrato2))*100
prop.table(table(empleo_calid1$Registro1))*100
empleo_calid1$Contrato1n <- as.numeric(empleo_calid1$Contrato1)
empleo_calid1$Contrato2 <- car::recode(empleo_calid1$Contrato1n, '1 = 1; 2 = 0; 3 = NA; NA = NA')
empleo_calid1$Registron <- as.numeric(empleo_calid1$Registro)
empleo_calid1$Registro1 <- car::recode(empleo_calid1$Registron, '1 = 1; 2 = 1; 3 = 0; NA = NA')

empleo_calid1$form <- (empleo_calid1$Contrato2 + empleo_calid1$Registro1)
prop.table(table(empleo_calid1$form))*100
empleo_calid1$form <- car::recode(empleo_calid1$form, '0 = 0; 1 = 1; 2 = 1; NA = NA')

#Adición 
empleo_calid1$calidad <- (empleo_calid1$form + empleo_calid1$indhoras + empleo_calid1$subempleo_ingresos)
prop.table(table(empleo_calid1$calidad))*100
hist(empleo_calid1$calidad)

####Cargamos base ya trabajada y empezamos el análisis

#BASE FINAL PARA TRABAJO
save(empleo_calid1, file = "Base_final.Rda")

save(empleo_calid1, file = "base_trabajada.Rda")
table(empleo_calid1$Edad)

##Análisis descriptivo
#Análisis univariado
summary(empleo_calid1$Edad)
descr(empleo_calid1$Edad)
hist(empleo_calid1$Edad)
tabla1 <- table(empleo_calid1$grupo_etario)
prop.table(tabla1)*100
library(summarytools)
resumen <- dfSummary(empleo_calid1)
stview(resumen)
summary(empleo_calid1$Edad)

p <- empleo_calid1 %>%
  ggplot( aes(x=Edad)) +
  geom_histogram( binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Gráfico 1: Edades de la PEA ocupada dependiente en el Perú - 2020") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

p

p1 <- empleo_calid1 %>%
  ggplot( aes(x=Ingreso_liquido)) +
  geom_histogram( binwidth=700, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Gráfico 4: Ingresos laborales líquidos PEA ocupada dependiente en el Perú - 2020") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

p1

hist(empleo_calid1$Edad)

install.packages("hrbrthemes")
library(hrbrthemes)

hist_ingreso <- ggplot(empleo_calid1, aes(x=Ingreso_liquido)) + 
  geom_histogram()
hist_ingreso
hist(empleo_calid1$Ingreso_liquido)
tablaedad <- prop.table(table(empleo_calid1$grupo_etario))*100
tablaedad
barplot(tablaedad, main = "Gráfico 2: PEA ocupada dependiente por grupos de edad - 2020 (porcentaje)", col = "aquamarine")

table(empleo_calid1$Nivel_educa)
class(empleo_calid1$Nivel_educa)
head(empleo_calid1$Nivel_educa)

tablaeduca <- prop.table(table(empleo_calid1$Nivel_educa))*100

tabla1educa <- as.data.frame(tablaeduca)
view(tabla1educa)
library(writexl)
write_xlsx(tabla1educa, "tabla1.xlsx")

barplot(tablaeduca, main = "Gráfico 3: PEA ocupada por nivel educativo - 2020 (porcentaje)", col = "aquamarine")

6.39745092+5.85876481 

tablatamano <- prop.table(table(empleo_calid1$Tamaño_empresa))*100                        
tablatamano
barplot(tablatamano, main = "Gráfico 3: PEA ocupada dependiente por tamaño de empresa - 2020 (porcentaje)",
        col = "aquamarine")

tablasubingreso <- prop.table(table(empleo_calid1$subempleo_ingresos))*100
tablasubingreso
barplot(tablasubingreso)
tablasubhoras <- prop.table(table(empleo_calid1$subempleo_horas))*100
tablasubhoras
barplot(tablasubhoras)
tablaexhoras <- prop.table(table(empleo_calid1$exceso_horas))*100
tablaexhoras
barplot(tablaexhoras)
hist_horas <- ggplot(empleo_calid1, aes(x=Horas)) + 
  geom_histogram()
hist_horas
tablacontrato <- prop.table(table(empleo_calid1$Contrato1))*100
tablacontrato
barplot(tablacontrato)


##Análisis bivariado descriptivo
#Tamaño de empresa en la que se trabaja según edad y nivel educativo
tablacruzada1 <- prop.table(table(empleo_calid1$Tamaño_empresa, empleo_calid1$grupo_etario),2)*100
tablacruzada1

write.csv(tablacruzada1, "tabla2.csv")

tabla2tamano <- read.csv2("tabla2.csv", header = TRUE, sep = ",")

library(writexl)

write_xlsx(tabla2tamano, "tabla2.xlsx")

tablacruzada2 <- prop.table(table(empleo_calid1$Nivel_educa, empleo_calid1$Tamaño_empresa),1)*100
tablacruzada2

write.csv(tablacruzada2, "tabla3.csv")

tabla3tamano <- read.csv2("tabla3.csv", header = TRUE, sep = ",")
write_xlsx(tabla3tamano, "tabla3.xlsx")

#Ingreso y horas según edad
tablacruzada3 <- tapply(empleo_calid1$Ingreso_liquido, empleo_calid1$grupo_etario, summary)
tablacruzada3
tablacruzada4 <- tapply(empleo_calid1$Horas, empleo_calid1$grupo_etario, summary)
tablacruzada4

#Subempleo por ingresos, por horas, exceso de horas y presencia de contrato según edad
tablacruzada5 <- prop.table(table(empleo_calid1$subempleo_ingresos, empleo_calid1$grupo_etario),2)*100
tablacruzada5
tablacruzada6 <- prop.table(table(empleo_calid1$subempleo_horas, empleo_calid1$grupo_etario),2)*100
tablacruzada6
tablacruzada7 <- prop.table(table(empleo_calid1$exceso_horas, empleo_calid1$grupo_etario),2)*100
tablacruzada7
tablacruzada8 <- prop.table(table(empleo_calid1$Contrato1, empleo_calid1$grupo_etario),2)*100
tablacruzada8

write.csv(tablacruzada5, "tabla4.1.csv")
tabla4ingreso <- read.csv2("tabla4.1.csv", header = TRUE, sep = ",")
write_xlsx(tabla4ingreso, "tabla4.1.xlsx")

write.csv(tablacruzada8, "tabla4.2.csv")
tabla4contrato <- read.csv2("tabla4.2.csv", header = TRUE, sep = ",")
write_xlsx(tabla4contrato, "tabla4.2.xlsx")

write.csv(tablacruzada6, "tabla4.3.csv")
tabla4subhoras <- read.csv2("tabla4.3.csv", header = TRUE, sep = ",")
write_xlsx(tabla4subhoras, "tabla4.3.xlsx")

write.csv(tablacruzada7, "tabla4.4.csv")
tabla4exhoras <- read.csv2("tabla4.4.csv", header = TRUE, sep = ",")
write_xlsx(tabla4exhoras, "tabla4.4.xlsx")

#Ingreso y horas según nivel educativo
tablacruzada9 <- tapply(empleo_calid1$Ingreso_liquido, empleo_calid1$Nivel_educa, summary)
tablacruzada9
tablacruzada10 <- tapply(empleo_calid1$Horas, empleo_calid1$Nivel_educa, summary)
tablacruzada10

#Subempleo por ingresos, por horas, exceso de horas y presencia de contraro según nivel educativo
tablacruzada11 <- prop.table(table(empleo_calid1$Nivel_educa, empleo_calid1$subempleo_ingresos),1)*100
tablacruzada11
tablacruzada12 <- prop.table(table(empleo_calid1$Nivel_educa, empleo_calid1$subempleo_horas),1)*100
tablacruzada12
tablacruzada13 <- prop.table(table(empleo_calid1$Nivel_educa, empleo_calid1$exceso_horas),1)*100
tablacruzada13
tablacruzada14 <- prop.table(table(empleo_calid1$Nivel_educa, empleo_calid1$Contrato1),1)*100
tablacruzada14

write.csv(tablacruzada11, "tabla5.1.csv")
tabla5ingresos <- read.csv2("tabla5.1.csv", header = TRUE, sep = ",")
write_xlsx(tabla5ingresos, "tabla5.1.xlsx")

write.csv(tablacruzada14, "tabla5.2.csv")
tabla5contrato <- read.csv2("tabla5.2.csv", header = TRUE, sep = ",")
write_xlsx(tabla5contrato, "tabla5.2.xlsx")

write.csv(tablacruzada12, "tabla5.3.csv")
tabla5subhoras <- read.csv2("tabla5.3.csv", header = TRUE, sep = ",")
write_xlsx(tabla5subhoras, "tabla5.3.xlsx")

write.csv(tablacruzada13, "tabla5.4.csv")
tabla5exhoras <- read.csv2("tabla5.4.csv", header = TRUE, sep = ",")
write_xlsx(tabla5exhoras, "tabla5.4.xlsx")

#Ingreso y horas según tamaño de empresa
tablacruzada15 <- tapply(empleo_calid1$Ingreso_liquido, empleo_calid1$Tamaño_empresa, summary)
tablacruzada15
tablacruzada16 <- tapply(empleo_calid1$Horas, empleo_calid1$Tamaño_empresa, summary)
tablacruzada16

#Subempleo por ingresos, por horas, exceso de horas y presencia de contraro según tamaño de empresa
tablacruzada17 <- prop.table(table(empleo_calid1$subempleo_ingresos, empleo_calid1$Tamaño_empresa),2)*100
tablacruzada17
tablacruzada18 <- prop.table(table(empleo_calid1$subempleo_horas, empleo_calid1$Tamaño_empresa),2)*100
tablacruzada18
tablacruzada19 <- prop.table(table(empleo_calid1$exceso_horas, empleo_calid1$Tamaño_empresa),2)*100
tablacruzada19
tablacruzada20 <- prop.table(table(empleo_calid1$Contrato1, empleo_calid1$Tamaño_empresa),2)*100
tablacruzada20

write.csv(tablacruzada17, "tabla6.1.csv")
tabla6ingresos <- read.csv2("tabla6.1.csv", header = TRUE, sep = ",")
write_xlsx(tabla6ingresos, "tabla6.1.xlsx")

write.csv(tablacruzada20, "tabla6.2.csv")
tabla6contrato <- read.csv2("tabla6.2.csv", header = TRUE, sep = ",")
write_xlsx(tabla6contrato, "tabla6.2.xlsx")

write.csv(tablacruzada18, "tabla6.3.csv")
tabla6subhoras <- read.csv2("tabla6.3.csv", header = TRUE, sep = ",")
write_xlsx(tabla6subhoras, "tabla6.3.xlsx")

write.csv(tablacruzada19, "tabla6.4.csv")
tabla6exhoras <- read.csv2("tabla6.4.csv", header = TRUE, sep = ",")
write_xlsx(tabla6exhoras, "tabla6.4.xlsx")

##Pruebas estadísticas bivariadas
#Tamaño de empresa en la que se trabaja según grupo de edad
tabla1 <- table(empleo_calid1$Tamaño_empresa, empleo_calid1$grupo_etario)
tabla1
chisq.test(tabla1)
library(vcdExtra)
GKgamma(tabla1)

#Tamaño de empresa en la que se trabaja según nivel educativo
tabla2 <- table(empleo_calid1$Tamaño_empresa, empleo_calid1$Nivel_educa)
tabla2
chisq.test(tabla2)
GKgamma(tabla2)

#subempleo por ingresos según grupo de edad
tabla3 <- table(empleo_calid1$subempleo_ingresos, empleo_calid1$grupo_etario)
tabla3
prop.table((tabla3),2)*100
chisq.test(tabla3)
library(vcd)
assocstats(tabla3)

#Ingresos líquidos según edad
cor.test(empleo_calid1$Ingreso_liquido, empleo_calid1$Edad)

#Ingresos líquidos según grupo de edad
boxplot(empleo_calid1$Ingreso_liquido ~ empleo_calid1$grupo_etario)
Anova1 <- aov(empleo_calid1$Ingreso_liquido ~ empleo_calid1$grupo_etario)
summary(Anova1)
TukeyHSD(Anova1)

#Subempleo por horas según grupo de edad
tabla4 <- table(empleo_calid1$subempleo_horas, empleo_calid1$grupo_etario)
tabla4
prop.table((tabla4),2)*100
chisq.test(tabla4)
assocstats(tabla4)

#Exceso de horas según grupo de edad
tabla5 <- table(empleo_calid1$exceso_horas, empleo_calid1$grupo_etario)
tabla5
prop.table((tabla5),2)*100
chisq.test(tabla5)
assocstats(tabla5)

#Existencia de un contrato según grupo de edad
tabla6 <- table(empleo_calid1$Contrato1, empleo_calid1$grupo_etario)
prop.table((tabla6),2)*100
chisq.test(tabla6)
assocstats(tabla6)

#Subempleo por ingresos según nivel educativo
tabla7 <- table(empleo_calid1$subempleo_ingresos, empleo_calid1$Nivel_educa)
prop.table((tabla7), 2)
chisq.test(tabla7)
assocstats(tabla7)

#Ingresos líquidos según nivel educativo
boxplot(empleo_calid1$Ingreso_liquido ~ empleo_calid1$Nivel_educa)
Anova2 <- aov(empleo_calid1$Ingreso_liquido ~ empleo_calid1$Nivel_educa)
summary(Anova2)
TukeyHSD(Anova2)

#Subempleo por horas según nivel educativo
tabla8 <- table(empleo_calid1$subempleo_horas, empleo_calid1$Nivel_educa)
prop.table((tabla8),2)*100
chisq.test(tabla8)
assocstats(tabla8)

#Exceso de horas según nivel educativo
tabla9 <- table(empleo_calid1$exceso_horas, empleo_calid1$Nivel_educa )
prop.table((tabla9),2)*100
chisq.test(tabla9)
assocstats(tabla9)

#Existencia de contrato según nivel educativo
tabla10 <- table(empleo_calid1$Contrato1, empleo_calid1$Nivel_educa)
prop.table((tabla10),2)
chisq.test(tabla10)
assocstats(tabla10)

#Ingreso líquido según tamaño de empresa
boxplot(empleo_calid1$Ingreso_liquido ~ empleo_calid1$Tamaño_empresa)
Anova3 <- aov(empleo_calid1$Ingreso_liquido ~ empleo_calid1$Tamaño_empresa)
summary(Anova3)
TukeyHSD(Anova3)

#Subempleo por ingreso según tamaño de empresa
tabla11 <- table(empleo_calid1$subempleo_ingresos, empleo_calid1$Tamaño_empresa)
prop.table((tabla11),2)*100
chisq.test(tabla11)
assocstats(tabla11)

#Subempleo por horas según tamaño de empresa
tabla12 <- table(empleo_calid1$subempleo_horas, empleo_calid1$Tamaño_empresa)
prop.table((tabla12),2)*100
chisq.test(tabla12)
assocstats(tabla12)

#Exceso de horas según tamaño de empresa
tabla13 <- table(empleo_calid1$exceso_horas, empleo_calid1$Tamaño_empresa)
prop.table((tabla13),2)*100
chisq.test(tabla13)
assocstats(tabla13)

#Presencia de contrato según tamaño de empresa
tabla14 <- table(empleo_calid1$Contrato1, empleo_calid1$Tamaño_empresa)
prop.table((tabla14),2)*100
chisq.test(tabla14)
assocstats(tabla14)
