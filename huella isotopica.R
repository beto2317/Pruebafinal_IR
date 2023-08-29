install.packages("gridExtra")
install.packages("effectsize")
install.packages("nicheROVER")
install.packages("vegan")
install.packages("maptools")
library(nicheROVER)
library(vegan)
library(ggplot2)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(effectsize)
library(maptools)

#  alt+shif+k      Muestra kista de atajos
#  alt+shif+j      ir lista de seciones
#  ctrl+shif+R     Se inserta una label de sección

###########################  HUELLA ISOTOPICA   ##############################

# LEEMOS LA BASE DE DATOS
BD_iso<-read.csv("BD_Isotopos_huella.csv",header = TRUE, dec=",",sep = ";")
BD_iso_BASE<-read.csv("BD_Isotopos_huella.csv",header = TRUE, dec=",",sep = ";")
BD_iso_BASE<-BD_iso_BASE[-c(52),]
BD_iso<-BD_iso[-c(74:82),]  # Eliminamos las filas por muestreos en fecha diferente 03-11-2022
summary(BD_iso)  # Vemos un resumen de los datos
str(BD_iso)  # Revisamos su estructura 
names(BD_iso) # Mostramos los nombres de las variables
class(BD_iso$fecha)
BD_iso$fecha <- as.Date(BD_iso$fecha, format = "%d-%m-%y")   # Cambiamo formato date 
skip(datos, n = 300)

# Dividimos la variable Long._Cefal_mm en una nueva variable grupo  con juvenil y adulto, usando dplyr
BD_iso <- BD_iso %>%
  mutate(grupo = ifelse(Long._Cefal_mm <= 30.00, "Juvenil", "Adulto"))

# Dividimos la variable Long._Cefal_mm en una nueva variable grupo  con juvenil y adulto, usando dplyr
 ## BD_iso_BASE <- BD_iso_BASE %>%
  mutate(grupo = ifelse(Long._Cefal_mm <= 30.00, "Juvenil", "Adulto"))

# Dividimos la variable fecha en 2 periodos

BD_iso<- BD_iso %>%
  mutate(periodo = case_when(
    Fecha == "2022-11-03" ~ "Periodo 1",
    Fecha == "2023-01-07" ~ "Periodo 2",
    TRUE ~ "" ))

cantidad_fechas_distintas <- BD_iso %>%
  summarize(n_distinct(BD_iso$Fecha))

fechas_distintas <- BD_iso %>% distinct(Fecha) # Cuantas fechas distintas hay


#Aqui le damos la clase correspondiente a las variables
# que no correspondia su clase
BD_iso$ID_Sample<-as.factor(BD_iso$ID_Sample)
BD_iso$Muestra<-as.factor(BD_iso$Muestra)
BD_iso$Estacion<-as.factor(BD_iso$Estacion)
BD_iso$E_Reprod.<-as.factor(BD_iso$REP_Reprod..)
BD_iso$SEXO<-as.factor(BD_iso$SEXO)
BD_iso$X.<-as.numeric(BD_iso$X.)
BD_iso$Peso_grs<-as.numeric(BD_iso$Peso_grs)
BD_iso$Mass_mg<-as.numeric(BD_iso$Mass_mg)
BD_iso$periodo<-as.factor(BD_iso$periodo) # tiene que ver con las fechas
BD_iso$grupo<-as.factor(BD_iso$grupo)
names(BD_iso)

 
longitud <- na.omit(BD_iso$Long_Cefal_mm)  # datos de longitud corporal
masa <- BD_iso$Peso_grs            # y masa corporal

SMA <- (longitud - mean(longitud)) / sd(longitud)  # Calcular el SMA
bSMA <- as.numeric(exp(SMA)) #Calcular bSMA
BD_iso$SMII <- masa/bSMA # Agregar la columna SMII a BD_iso

names(BD_iso)


## data <- mutate(data, suma = x + y)

# Definir el orden deseado de las localidades
orden_localidades <- c("Cuya", "Conanoxa", "Huancarane")

# Convertir la variable localidad a factor con el orden deseado
BD_iso$Estacion <- factor(BD_iso$Estacion, levels = orden_localidades)

# Cambiamos el SEXO de numero a palabras
BD_iso$SEXO <- factor(BD_iso$SEXO, levels = c(1, 2,3), labels = c("Macho", "Hembra", "Indeter"))
## BD_iso_BASE$SEXO <- factor(BD_iso_BASE$SEXO, levels = c(1, 2,3), labels = c("Macho", "Hembra", "Indeter"))
levels(BD_iso$SEXO)
class(BD_iso$Estacion) # Vemos la clase de Estación
levels(BD_iso$Estacion) # Vemos sus niveles

class(BD_iso$SEXO) # Vemos la clase de Estación
levels(BD_iso$SEXO)

names(BD_iso)

str(BD_iso) # Volvemos a revisar su estructura

summary(BD_iso) # Realizamos un resumen de totas las variables

var_num<-BD_iso[c(8:19)] # Filtramos solo las variables numericas
str(var_num)
 # par(mar = c(5, 4, 4, 2) + 0.1)    Ajusta los margenes del grafico

dev.off()  ### Limpia la ventana de los graficos

plot(BD_iso$Long_Total,BD_iso$Peso_grs)  ### REVISAR GRAFICOS  #########
plot(BD_iso$X15N,BD_iso$Peso_grs)
plot(BD_iso$X13C,BD_iso$Peso_grs)
plot(BD_iso$X34S,BD_iso$Peso_grs)
plot(BD_iso$X.N,BD_iso$Peso_grs)
plot(BD_iso$X.C,BD_iso$Peso_grs)
plot(BD_iso$C.N,BD_iso$Peso_grs)
plot(BD_iso$X15N,BD_iso$SMII)
plot(BD_iso$SMII,BD_iso$BD_iso$X13C)
plot(BD_iso$SMII,BD_iso$BD_iso$X34S)
plot(BD_iso$X34S,BD_iso$SMII)

plot(datos.Cuya$X15N,datos.Cuya$Peso_grs)
plot(datos.Huancarane$X15N,datos.Huancarane$Peso_grs)
plot(datos.Conanoxa$X15N,datos.Conanoxa$Peso_grs)
names(datos.Cuya$X15N)

dev.off()  ### Limpia la ventana de los graficos

## Aqui filtramos por Estacion
datos.cuya=BD_iso[BD_iso$Estacion=="Cuya",]
datos.Huancarane=BD_iso[BD_iso$Estacion=="Huancarane",]
datos.Conanoxa=BD_iso[BD_iso$Estacion=="Conanoxa",]
datos.Conanoxa=BD_iso[BD_iso$Estacion=="Conanoxa",(8:18)]

plot(datos.cuya$Peso_grs,datos.cuya$Long_Total)
plot(datos.Huancarane$Peso_grs,datos.Huancarane$Long_Total)
plot(datos.Conanoxa$Peso_grs,datos.Conanoxa$Long_Total)
names(datos.cuya)
mean(datos.cuya$Peso_grs)
mean(datos.Huancarane$Peso_gr)
mean(datos.Conanoxa$Peso_gr)



datos.juvenil=BD_iso[BD_iso$grupo=="Juvenil",]  ## Filtramos por grupo
datos.adulto=BD_iso[BD_iso$grupo=="Adulto",]

###################   Boxplot y anovas   _________________________________

boxplot(BD_iso$SMII ~ BD_iso$Estacion, data = BD_iso) # SMII por estacion
boxplot(BD_iso$Peso_grs ~ BD_iso$SEXO, data = BD_iso) # Hacemos un boxplot por sexo
anova_sexo<-aov(BD_iso$Peso_grs~BD_iso$SEXO,data=BD_iso) # Realizamos un ANOVA
summary(anova_sexo)  #  Resumen del ANOVA
# Como el p-valor es menos a 0.05 se recha hipotesis nula H0 y se acepta la alternativa H1
# Lo que implica que hay diferencias significativas en la medias.


boxplot(BD_iso$Peso_grs ~ BD_iso$grupo, data = BD_iso) # Hacemos un boxplot
# "Peso_grs" numerica con los niveles de la variable categorica "grupo"
anova_grupo<-aov(BD_iso$Peso_grs~BD_iso$grupo,data=BD_iso) # Comparamos sus medias

# H0: Las medias son iguales entre juveniles y adultos 
# H1: Las media son distintas distinta

summary(anova_grupo)   # Como el p-valor es <  0.05 rechazamos la 
# hipotesis nula Ho y aceptamos la alternativa H1, o sea, las medias de juveniles
# y adultos son diferentes.

##### Test de tukey
resultado_tukey <- TukeyHSD(anova_grupo)
summary(resultado_tukey)

##### Test de Bonferroni
resultado_bonferroni <- pairwise.t.test(BD_iso$Peso_grs, BD_iso$grupo, p.adjust.method = "bonferroni")
print(resultado_bonferroni)

##### Test de comparaciones múltiples de Tukey-Kramer
resultado_tukey_kramer <- TukeyHSD(anova_grupo, alpha = 0.05)
print(resultado_tukey_kramer)

#______________________________________________________________________________________
# Comparacion general Estaciones vs pesos

boxplot(BD_iso$Peso_grs ~ BD_iso$Estacion, data = BD_iso)

stripchart(BD_iso$Peso_grs ~ BD_iso$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(BD_iso$Estacion)))

anova_estación<-aov(BD_iso$Peso_grs~BD_iso$Estacion,data=BD_iso) # Comparamos sus medias

# H0: Las medias son iguales entre las estaciones 
# H1: Las media son al menos una distinta.

summary(anova_estación)   # Como el p-valor es <  0.05 rechazamos la 
# hipotesis nula Ho y aceptamos la alternativa H1, o sea, las medias al menos una es diferente.


##### Test de tukey
resultado_tukey <- TukeyHSD(anova_estación)
print(resultado_tukey)

##### Test de Bonferroni
resultado_bonferroni <- pairwise.t.test(BD_iso$Peso_grs, BD_iso$Estacion, p.adjust.method = "bonferroni")
print(resultado_bonferroni)

##### Test de comparaciones múltiples de Tukey-Kramer
resultado_tukey_kramer <- TukeyHSD(anova_estación, alpha = 0.05)
print(resultado_tukey_kramer)


#___________________________________________________________________________________________________
# pesos de adultos y juveniles de Cuya

boxplot(datos.cuya$Peso_grs ~ datos.cuya$grupo, data = datos.cuya)


boxplot(datos.Conanoxa$Peso_grs ~ datos.Conanoxa$grupo, data = datos.Conanoxa)
boxplot(datos.Huancarane$Peso_grs ~ datos.Huancarane$grupo, data = datos.Huancarane)



##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
## X15N
boxplot(BD_iso$X15N ~ BD_iso$Estacion, data = BD_iso)

stripchart(BD_iso$X15N ~ BD_iso$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(BD_iso$Estacion)))

anova_X15N_estación<-aov(BD_iso$X15N~BD_iso$Estacion,data=BD_iso)
summary(anova_X15N_estación)
##### Test de comparaciones múltiples de Tukey-Kramer
resultado_tukey_kramer <- TukeyHSD(anova_X15N_estación, alpha = 0.05)
print(resultado_tukey_kramer)
summary(BD_iso$X15N)
length(BD_iso$X15N)

# X13C
boxplot(BD_iso$X13C ~ BD_iso$Estacion, data = BD_iso)

stripchart(BD_iso$X13C ~ BD_iso$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(BD_iso$Estacion)))

anova_X13C_estación<-aov(BD_iso$X13C~BD_iso$Estacion,data=BD_iso)
summary(anova_X13C_estación)
resultado_tukey_kramer <- TukeyHSD(anova_X13C_estación, alpha = 0.05)
print(resultado_tukey_kramer)


# X34S
boxplot(BD_iso$X34S ~ BD_iso$Estacion, data = BD_iso)

stripchart(BD_iso$X34S ~ BD_iso$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(BD_iso$Estacion)))

anova_X34S_estación<-aov(BD_iso$X34S~BD_iso$Estacion,data=BD_iso)
summary(anova_X34S_estación)
resultado_tukey_kramer <- TukeyHSD(anova_X34S_estación, alpha = 0.05)
print(resultado_tukey_kramer)

summary(BD_iso$X34S)
length(BD_iso$X34S)

#  C.N
boxplot(BD_iso$C.N ~ BD_iso$Estacion, data = BD_iso)

stripchart(BD_iso$C.N ~ BD_iso$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(BD_iso$Estacion)))

anova_C.N_estación<-aov(BD_iso$C.N~BD_iso$Estacion,data=BD_iso)
summary(anova_C.N_estación)
resultado_tukey_kramer <- TukeyHSD(anova_C.N_estación, alpha = 0.05)
print(resultado_tukey_kramer)




# ## dev.off() ------------------------------------------------------------


##ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
dev.off()
datos_filtrados <- subset(BD_iso, SEXO == "Hembra")

# Crear el gráfico de caja para los datos filtrados
boxplot(Peso_grs ~ SEXO, data = datos_filtrados)


SEXO_hembra<-BD_iso[BD_iso$SEXO=="Hembra",]                             
summary(por_SEXO_Hembra)
SEXO_macho<-BD_iso[BD_iso$SEXO=="Macho",]
SEXO_indeter<-BD_iso[BD_iso$SEXO=="Indeter",]

##########  macho por estacion y X15











.








# macho-X15N
a<-boxplot(SEXO_macho$X15N ~ SEXO_macho$Estacion,data=SEXO_macho)
stripchart(SEXO_macho$X15N ~ SEXO_macho$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(SEXO_macho$Estacion)))

anova_X15N_estación_machos<-aov(SEXO_macho$X15N~SEXO_macho$Estacion,data=SEXO_macho)
summary(anova_X15N_estación_machos)
resultado_tukey_kramer <- TukeyHSD(anova_X15N_estación_machos, alpha = 0.05)
print(resultado_tukey_kramer)

# hembra-X15N
boxplot(SEXO_hembra$X15N ~ SEXO_hembra$Estacion,data=SEXO_hembra)
stripchart(SEXO_hembra$X15N ~ SEXO_hembra$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(SEXO_hembra$Estacion)))

anova_X15N_estación_hembras<-aov(SEXO_hembra$X15N~SEXO_hembra$Estacion,data=SEXO_hembra)
summary(anova_X15N_estación_hembras)
resultado_tukey_kramer <- TukeyHSD(anova_X15N_estación_hembras, alpha = 0.05)
print(resultado_tukey_kramer)

# indeterminado-X15N
c<-boxplot(SEXO_indeter$X15N ~ SEXO_indeter$Estacion,data=SEXO_indeter)
stripchart(SEXO_indeter$X15N ~ SEXO_indeter$Estacion, vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, col = 1:length(levels(SEXO_indeter$Estacion)))

anova_X15N_estación_indeter<-aov(SEXO_indeter$X15N~SEXO_indeter$Estacion,data=SEXO_indeter)
summary(anova_X15N_estación_indeter)

##########  macho por estacion y X13C
# macho-X13C

boxplot(SEXO_macho$X13C ~ SEXO_macho$Estacion,data=SEXO_macho)
stripchart(SEXO_macho$X13C ~ SEXO_macho$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(SEXO_macho$Estacion)))

anova_X13C_estación_machos<-aov(SEXO_macho$X13C~SEXO_macho$Estacion,data=SEXO_macho)
summary(anova_X13C_estación_machos)
resultado_tukey_kramer <- TukeyHSD(anova_X13C_estación_machos, alpha = 0.05)
print(resultado_tukey_kramer)

# hembra-X13C
boxplot(SEXO_hembra$X13C ~ SEXO_hembra$Estacion,data=SEXO_hembra)
stripchart(SEXO_hembra$X13C ~ SEXO_hembra$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(SEXO_hembra$Estacion)))

anova_X13C_estación_hembras<-aov(SEXO_hembra$X13C~SEXO_hembra$Estacion,data=SEXO_hembra)
summary(anova_X13C_estación_hembras)
resultado_tukey_kramer <- TukeyHSD(anova_X13C_estación_hembras, alpha = 0.05)
print(resultado_tukey_kramer)

# indeterminado-X13C
boxplot(SEXO_indeter$X13C ~ SEXO_indeter$Estacion,data=SEXO_indeter)
stripchart(SEXO_indeter$X13C ~ SEXO_indeter$Estacion, vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, col = 1:length(levels(SEXO_indeter$Estacion)))

anova_X13C_estación_indeter<-aov(SEXO_indeter$X13C~SEXO_indeter$Estacion,data=SEXO_indeter)
summary(anova_X13C_estación_indeter)

##########  macho por estacion y X34S
# macho-X34S

boxplot(SEXO_macho$X34S ~ SEXO_macho$Estacion,data=SEXO_macho)
stripchart(SEXO_macho$X34S ~ SEXO_macho$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(SEXO_macho$Estacion)))

anova_X34S_estación_machos<-aov(SEXO_macho$X34S~SEXO_macho$Estacion,data=SEXO_macho)
summary(anova_X34S_estación_machos)
resultado_tukey_kramer <- TukeyHSD(anova_X34S_estación_machos, alpha = 0.05)
print(resultado_tukey_kramer)

# hembra-X34S
boxplot(SEXO_hembra$X34S ~ SEXO_hembra$Estacion,data=SEXO_hembra)
stripchart(SEXO_hembra$X34S ~ SEXO_hembra$Estacion, vertical = TRUE, method = "jitter",
           pch = 20, add = TRUE, col = 1:length(levels(SEXO_hembra$Estacion)))

anova_X34S_estación_hembras<-aov(SEXO_hembra$X34S~SEXO_hembra$Estacion,data=SEXO_hembra)
summary(anova_X34S_estación_hembras)
resultado_tukey_kramer <- TukeyHSD(anova_X34S_estación_hembras, alpha = 0.05)
print(resultado_tukey_kramer)

# indeterminado-X34S
boxplot(SEXO_indeter$X34S ~ SEXO_indeter$Estacion,data=SEXO_indeter)
stripchart(SEXO_indeter$X34S ~ SEXO_indeter$Estacion, vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, col = 1:length(levels(SEXO_indeter$Estacion)))

anova_X34S_estación_indeter<-aov(SEXO_indeter$X34S~SEXO_indeter$Estacion,data=SEXO_indeter)
summary(anova_X34S_estación_indeter)

###############
###############
###############
###############






b<-boxplot(SEXO_hembra$X15N ~ SEXO_hembra$Estacion,data=SEXO_hembra)
c<-boxplot(SEXO_no-idnentf$X15N ~ SEXO_no-idnentf$Estacion,data=SEXO_no-idnentf)

d<- boxplot(BD_iso$X15N ~ BD_iso$Estacion, BD_iso)
e<- boxplot(BD_iso$X15N ~ BD_iso$Estacion, datos.cuya)

f <- boxplot(BD_iso$X13C ~ BD_iso$Estacion, BD_iso)   
gg <- boxplot(BD_iso$SMII ~ BD_iso$Estacion, BD_iso)
estadisticos_g<-boxplot.stats(gg)
###############################################################################
############### Crear el gráfico de barras horizontales #####################║
###############################################################################

###  https://rbiologos.com/blog/a011/   LINK GRAFICOS GGPLOT2
###  https://rbiologos.com/blog/a011/   ###  https://rbiologos.com/blog/a011/ 

##### isótopo X.S  por estación
graf1<-ggplot()+
  geom_bar(data=BD_iso,
           aes(x=BD_iso$Estacion,y=BD_iso$X.S,fill = BD_iso$Estacion),
           stat = "identity",
           position = "dodge")+
  labs(x="zona", y="Isótopos de XS",fill="Colores de zonas")+
  coord_flip()+   # pone las barras horizontales
  theme_minimal()
print(graf1)

##### isótopo X.C  por estación
graf2<-ggplot()+
  geom_bar(data=BD_iso,
           aes(x=BD_iso$Estacion,y=BD_iso$X.C,fill = BD_iso$Estacion),
           stat = "identity",
           position = "dodge")+
  labs(x="zona", y="Isótopos de X.C",fill="Colores de zonas")+
  coord_flip()+   # pone las barras horizontales
  theme_minimal()
View(graf2)

##### isótopo C.N  por estación
graf3<-ggplot()+
  geom_bar(data=BD_iso,
           aes(x=BD_iso$Estacion,y=BD_iso$C.N,fill = BD_iso$Estacion),
           stat = "identity",
           position = "dodge")+
  labs(x="zona", y="Isótopos de C.N",fill="Colores de zonas")+
  coord_flip()+   # pone las barras horizontales
  theme_minimal()

##### isótopo X15N  por estación
ggplot()+
  geom_bar(data=BD_iso,
           aes(x=BD_iso$Estacion,y=BD_iso$X15N,fill = BD_iso$Estacion),
           stat = "identity",
           position = "dodge")+
  labs(x="zona", y="Isótopos de X15N",fill="Colores de zonas")+
  coord_flip()+   # pone las barras horizontales
  theme_minimal()

##### isótopo X13C  por estación

ggplot()+
  geom_bar(data=BD_iso,
           aes(x=BD_iso$Estacion,y=BD_iso$X13C,fill = BD_iso$Estacion),
           stat = "identity",
           position = "dodge")+
  labs(x="zona", y="Isótopos de X13C",fill="Colores de zonas")+
  coord_flip()+   # pone las barras horizontales
  theme_minimal()

##### isótopo X34S  por estación

ggplot()+
  geom_bar(data=BD_iso,
           aes(x=BD_iso$Estacion,y=BD_iso$X34S,fill = BD_iso$Estacion),
           stat = "identity",
           position = "dodge")+
  labs(x="zona", y="Isótopos de X34S",fill="Colores de zonas")+
  coord_flip()+   # pone las barras horizontales
  theme_minimal()

ggplot() +
  geom_bar(data = BD_iso,
           aes(x = Estacion, y = X.S, fill = combined),
           stat = "identity",
           position = position_dodge(width = 0.9)) +  # Ajusta el ancho de la agrupación
  coord_flip() +
  theme_minimal() +
  labs(fill = "Nombres de colores")

# Crear el primer gráfico
plot1 <- barplot(height = datos1$variable2, names.arg = datos1$variable1, main = "Gráfico 1")

# Crear el segundo gráfico
plot2 <- plot(datos2$variable3, datos2$variable4, type = "l", main = "Gráfico 2")

# Definir la estructura de la combinación de gráficos
      par(mfrow = c(2, 2))
      #layout(matrix(c(1, 2), nrow = 1))
grid.arrange(plot1, plot2, nrow = 2)
graf1
graf2

# Mostrar los gráficos en una única ventana gráfica
plot1
plot2


# ANOVAS ------------------------------------------------------------------


###############################################################################
############### ANOVAS #####################║
###############################################################################

#  ANOVA por Estación
attach(BD_iso)
datos.fil.peso<-na.omit(BD_iso[c("Peso_grs","Estacion","SEXO","grupo")])

# Estadisticos descriptivos
round(tapply(Peso_grs,Estacion,mean),1)
round(tapply(Peso_grs,SEXO,mean),1)
round(tapply(Peso_grs,list(Estacion,SEXO),mean),1)
round(tapply(Peso_grs,list(Estacion,SEXO),sd),1)
round(tapply(Peso_grs,list(Estacion,SEXO),length),1)

# Analisis de graficos
interaction.plot(Estacion,SEXO,Peso_grs, col = 1:12, lwd=2)
interaction.plot(fecha,SEXO,Peso_grs, col = 1:12, lwd=2)
interaction.plot(grupo,SEXO,Peso_grs, col = 1:12, lwd=2)


# Ajuste de Anova
mod1<-aov(Peso_grs~Estacion*SEXO,datos.fil.peso[SEXO=="Macho",])
mod2<-aov(Peso_grs~Estacion*SEXO,datos.fil.peso[SEXO=="Hembra",])
mod3<-aov(Peso_grs~Estacion*grupo,datos.fil.peso[grupo=="Juvenil",])
mod4<-aov(Peso_grs~Estacion*grupo,datos.fil.peso[grupo=="Adulto",])
mod5<-aov(Peso_grs~Estacion*grupo,data=datos.fil.peso)
anova(mod1)
anova(mod2)
anova(mod3)
anova(mod4)
anova(mod5)







boxplot(BD_iso$Peso_grs ~ BD_iso$Estacion, data = BD_iso)

stripchart(BD_iso$Peso_grs ~ BD_iso$Estacion, vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, col = 1:length(levels(BD_iso$Estacion)))

anova1<-aov(BD_iso$Peso_grs~BD_iso$Estacion,data=BD_iso)

# H0: Las medias son iguales en las 3 estaciones
# H1: Al menos una estación tiene una media distinta

summary(anova1)

#  ANOVA por SEXO

boxplot(BD_iso$Peso_grs ~ BD_iso$SEXO, data = BD_iso)

stripchart(BD_iso$Peso_grs ~ BD_iso$SEXO, vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, col = 1:length(levels(BD_iso$SEXO)))


anova2<-aov(BD_iso$Peso_grs~BD_iso$SEXO,data=BD_iso)

# H0: Las medias son iguales en las 3 estaciones
# H1: Al menos una estación tiene una media distinta

summary(anova2)



#  ANOVA por SEXO=1

boxplot(BD_iso$Peso_grs ~ BD_iso$Estacion, data = BD_iso)

stripchart(BD_iso$Peso_grs ~ BD_iso$Estacion, vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, col = 1:length(levels(BD_iso$Estacion)))


anova3<-aov(BD_iso$Peso_grs~BD_iso$Estacion,data=BD_iso)


# H0: Las medias son iguales en las 3 estaciones
# H1: Al menos una estación tiene una media distinta

summary(anova3)

#  ANOVA por SEXO=2

boxplot(por_SEXO_2$Peso_grs ~ por_SEXO_2$Estacion, data = por_SEXO_2)

stripchart(por_SEXO_2$Peso_grs ~ por_SEXO_2$Estacion, vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, col = 1:length(levels(por_SEXO_2$Estacion)))


anova4<-aov(por_SEXO_2$Peso_grs~por_SEXO_2$Estacion,data=por_SEXO_2)


# H0: Las medias son iguales en las 3 estaciones
# H1: Al menos una estación tiene una media distinta

summary(anova4)


#  ANOVA sólo Estación  Conanoxa y Huancarane

boxplot(BD_iso$Peso_grs ~ BD_iso$Estacion, data = BD_iso, 
        subset = BD_iso$Estacion %in% c("Conanoxa", "Huancarane"))

stripchart(BD_iso$Peso_grs ~ BD_iso$Estacion, vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, col = 1:length(levels(BD_iso$Estacion)))

anova5<-aov(BD_iso$Peso_grs~BD_iso$Estacion,data=BD_iso)

# H0: Las medias son iguales en las 3 estaciones
# H1: Al menos una estación tiene una media distinta

summary(anova5)

mean(datos.cuya$Peso_grs,na.rm=TRUE)
mean(datos.Conanoxa$Peso_grs,na.rm=TRUE)
mean(datos.Huancarane$Peso_grs,na.rm=TRUE)
?mean
class(datos.cuya$Peso_grs)




#_------------------------------------------------------------------------------------

BD_iso$combined <- interaction(BD_iso$X15N, BD_iso$X13C, BD_iso$X34S)

ggplot() +
  geom_bar(data = BD_iso,
           aes(x = Estacion, y = X.S, fill = combined),
           stat = "identity",
           position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(fill = "Nombres de colores")

##========================================================================
ggplot(BD_iso, aes(x = Estacion, y = X13C)) +
  geom_boxplot(fill = "#0099f8") +
  labs(
    title = "Concentraci?n isotopos X13C por Estaci?n",
    subtitle = "Is?topos musculos camar?n",
    caption = "IFOP: Hern?n Padilla",
    x = "Estaci?n de muestreo",
    y = "Is?topos X13C"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "#0099f8", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  )
##====================================================================================
ggplot(BD_iso, aes(x = BD_iso$Estacion, y = BD_iso$X.C)) +
  geom_boxplot(fill = "#0099f8") +
  labs(
    title = "Concentraci?n isotopos X.C por Estaci?n",
    subtitle = "Is?topos musculos camar?n",
    caption = "IFOP: Hern?n Padilla",
    x = "Estaci?n de muestreo",
    y = "Is?topos X.C"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "#0099f8", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  )

##====================================================================================
ggplot(BD_iso, aes(x = BD_iso$Estacion, y = BD_iso$C.N)) +
  geom_boxplot(fill = "#0099f8") +
  labs(
    title = "Concentraci?n isotopos C.N por Estaci?n",
    subtitle = "Is?topos musculos camar?n",
    caption = "IFOP: Hern?n Padilla",
    x = "Estaci?n de muestreo",
    y = "Is?topos X.C"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "#0099f8", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  )

##====================================================================================
ggplot(BD_iso, aes(x = Estacion, y = Long._Cefal_mm)) +
  geom_boxplot(fill = Estacion) +
  labs(
    title = "Concentraci?n isotopos Long._Cefal_mm por Estaci?n",
    subtitle = "Is?topos musculos camar?n",
    caption = "IFOP: Hern?n Padilla",
    #col=(c("red", "blue","green")),
    x = "Estaci?n de muestreo",
    y = "Is?topos X.C"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "#0099f8", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  )

##====================================================================================

ggplot(BD_iso, aes(x = BD_iso$Estacion, y = BD_iso$X.S)) +
  geom_boxplot(fill = "#0099f8") +
  labs(
    title = "Concentraci?n isotopos X.S por Estaci?n",
    subtitle = "Is?topos musculos camar?n",
    caption = "IFOP: Hern?n Padilla",
    x = "Estaci?n de muestreo",
    y = "Is?topos X.C"
  ) +
  theme_classic(
    
  )  +
  theme(
    plot.title = element_text(color = "#0099f8", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  )

+
  axis(1, at = c(0,1,2,3,4,5,6), labels = c("Cuya - Machos", "Cuya - Hembras", "Conanoxa - Machos",
                                            "Conanoxa - Hembras", "Huancarane - Machos", "Huancarane - Hembras")
  )




##====================================================================================
summary(m2)
m3 <- boxplot(BD_iso$X34S ~ BD_iso$Estacion, BD_iso)
m4 <- boxplot(BD_iso$X.N ~ BD_iso$Estacion, BD_iso)
m5 <- boxplot(BD_iso$X.C ~ BD_iso$Estacion, BD_iso)
m6 <- boxplot(BD_iso$X.S ~ BD_iso$Estacion, BD_iso)
m7 <- boxplot(BD_iso$C.N ~ BD_iso$Estacion, BD_iso)



# Crear grupos combinando las variables categ?ricas
grupos <- interaction(BD_iso$localidad, BD_iso$SEXO)
interact_var <- interaction(BD_iso$Estacion, BD_iso$SEXO)
interact_var <- interaction(BD_iso$SEXO,BD_iso$Estacion)

# Crear el diagrama de caja con la variable combinada
boxplot(BD_iso$X15N ~ interact_var, data = BD_iso)

axis(1, at = c(1,2,3 ), labels = c("Cuya - Machos", "Cuya - Hembras", "Conanoxa - Machos",
                                   "Conanoxa - Hembras", "Huancarane - Machos", "Huancarane - Hembras"))


##################################################
datos.especie48=datos.ejemplo[datos.ejemplo$COD_ESPECIE=="48",]
names(datos.especie48)
#################################################################################
##################################################################################
ggplot(df, aes(x = cyl, y = mpg)) +
  geom_boxplot(fill = "#0099f8") +
  labs(
    title = "Miles per gallon among different cylinder options",
    subtitle = "Made by Appsilon",
    caption = "Source: MTCars dataset",
    x = "Number of cylinders",
    y = "Miles per gallon"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "#0099f8", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  )
#################################################################################
#################################################################################
datos_tallas_cuya<-BD_iso[BD_iso$Estacion=="Cuya",9]
datos_tallas_conanoxa<-BD_iso[BD_iso$Estacion=="Conanoxa",9]
datos_tallas_Huancarane<-BD_iso[BD_iso$Estacion=="Huancarane",9]
hist(datos_tallas_cuyas)
hist(datos_tallas_conanoxa)
hist(datos_tallas_Huancarane)
class(BD_iso$Estacion)
levels(BD_iso$Estacion)
####################################################################################

attach(datos.especie48[datos.especie48$PESO>0,])
##################################################


x<-plot(var_num$Long_Total,var_num$Long._Cefal_mm)
?abline
abliine(x)
boxplot(Estacion)
cor(var_num)[1,]
?par
par(var_num)
class(BD_iso$Peso_grs)
class(Mass_mg)
cor(var_num)[8,]



cor(data.trabajo)[1,]
cor(BD_iso)[10,11]

####################################################################
################ chi cuadrado  #####################################

# Crear una tabla de contingencia con los datos de las dos variables
tabla_contingencia1<- table(BD_iso$Estacion, BD_iso$SEXO)  # Estacion y SEXO
tabla_contingencia2<- table(BD_iso$Estacion, BD_iso$grupo) # Estación y grupo




tabla_contingencia3<- table(BD_iso$Estacion, BD_iso$grupo) # Estación (Cuya y Conanoxa)
tabla_filtrada1 <- tabla_contingencia3[c("Cuya", "Conanoxa"), ]
tabla_filtrada2 <- tabla_contingencia3[c("Cuya", "Huancarane"), ]
tabla_contingencia4<- table(BD_iso$Estacion, BD_iso$grupo)
tabla_contingencia5<- table(BD_iso$Estacion, BD_iso$grupo)

prop_3<-prop.table(tabla_filtrada2) # da la proporcion de c/u en la tabla 
barplot(prop_3,main = "Comparación",legend=c("Cuya","Conanoxa"),xlab = "Grupo", beside = TRUE)

prop_1<-prop.table(tabla_contingencia1) # da la proporcion de c/u en la tabla 
barplot(prop_1,main = "Comparación",legend=c("Cuya","Conanoxa","Huancarane"),xlab = "SEXO", beside = TRUE)

# grados de libertad es df=(nf-1)*(nc-1)    tabla contingencia1
#                       df=(3-1)*(3-1)
#                       dF=4

# Realizar la prueba de chi-cuadrado de independencia
resultado3 <- chisq.test(tabla_filtrada)
resultado4 <- chisq.test(tabla_filtrada2)






resultado2 <- chisq.test(tabla_contingencia2)


### NicheRover
# analysis for fish data

# NicheRover --------------------------------------------------------------

names(BD_iso)
BD_iso_nicho<-na.omit(BD_iso[,c(4,(12:14))])
names(BD_iso_nicho)
aggregate(BD_iso_nicho[2:4], BD_iso_nicho[1], mean) # isotope means per BD_iso
result <- aggregate(BD_iso_nicho[2:4], BD_iso_nicho[1], mean)   ##########################?
length(BD_iso_nicho[2:4])
length(BD_iso_nicho[4])
str(BD_iso_nicho)

# random draws from posterior distribution with default prior
nsamples <- 500
BD_iso_nicho.par <- tapply(1:nrow(BD_iso_nicho), BD_iso_nicho$Estacion,
                   function(ii) niw.post(nsamples = nsamples, X = BD_iso_nicho[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(4.4, 4.4, 3, 3)+.1)
niche.par.plot(BD_iso_nicho.par, col = clrs)
legend(x = "topright", legend = names(BD_iso_nicho.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 100
BD_iso_nicho.par <- tapply(1:nrow(BD_iso_nicho), BD_iso_nicho$Estacion,
                   function(ii) niw.post(nsamples = nsamples, X = BD_iso_nicho[ii,2:4]))

# format data for plotting function
BD_iso_nicho.data <- tapply(1:nrow(BD_iso_nicho), BD_iso_nicho$Estacion, function(ii) X = BD_iso_nicho[ii,2:4])

niche.plot(niche.par = BD_iso_nicho.par, niche.data = BD_iso_nicho.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 500
over.stat <- overlap(BD_iso_nicho.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)

## analisis utilizando por estacion y prporcion isitopica

BD_iso_nicho<-na.omit(BD_iso[,c(4,(15:17))])

names(BD_iso_nicho)
aggregate(BD_iso_nicho[2:4], BD_iso_nicho[1], mean) # isotope means per BD_iso
result <- aggregate(BD_iso_nicho[2:4], BD_iso_nicho[1], mean)   ##########################?
length(BD_iso_nicho[2:4])
length(BD_iso_nicho[4])
str(BD_iso_nicho)

# random draws from posterior distribution with default prior
nsamples <- 500
BD_iso_nicho.par <- tapply(1:nrow(BD_iso_nicho), BD_iso_nicho$Estacion,
                           function(ii) niw.post(nsamples = nsamples, X = BD_iso_nicho[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(4.4, 4.4, 3, 3)+.1)
niche.par.plot(BD_iso_nicho.par, col = clrs)
legend(x = "topright", legend = names(BD_iso_nicho.par), fill = clrs)

# 2-d projections of 10 niche regions
dev.off()
nsamples <- 100
BD_iso_nicho.par <- tapply(1:nrow(BD_iso_nicho), BD_iso_nicho$Estacion,
                           function(ii) niw.post(nsamples = nsamples, X = BD_iso_nicho[ii,2:4]))

# format data for plotting function
BD_iso_nicho.data <- tapply(1:nrow(BD_iso_nicho), BD_iso_nicho$Estacion, function(ii) X = BD_iso_nicho[ii,2:4])

niche.plot(niche.par = BD_iso_nicho.par, niche.data = BD_iso_nicho.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 500
over.stat <- overlap(BD_iso_nicho.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)



##### Proporcio isitopica por Sexo

names(BD_iso)
levels(BD_iso$SEXO)
nicho_juvenil_estacion<-na.omit(BD_iso[(BD_iso$SEXO=="Macho"),c(4,(15:17))])
BD_iso_nicho_hembra<-na.omit(BD_iso[(BD_iso$SEXO=="Hembra"),c(4,(15:17))])

BD_iso_nicho<-na.omit(BD_iso[,c(4,(15:17))])

names(BD_iso_nicho)
aggregate(nicho_juvenil_estacion[2:4], nicho_juvenil_estacion[1], mean) # isotope means per BD_iso
result <- aggregate(nicho_juvenil_estacion[2:4], nicho_juvenil_estacion[1], mean)   ##########################?
length(nicho_juvenil_estacion[2:4])
length(nicho_juvenil_estacion[4])
str(nicho_juvenil_estacion)

# random draws from posterior distribution with default prior
nsamples <- 500
nicho_juvenil_estacion.par <- tapply(1:nrow(nicho_juvenil_estacion), nicho_juvenil_estacion$Estacion,
                           function(ii) niw.post(nsamples = nsamples, X = nicho_juvenil_estacion[ii,2:4]))
names(nicho_juvenil_estacion)

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(4.4, 4.4, 3, 3)+.1)
niche.par.plot(nicho_juvenil_estacion.par, col = clrs)
legend(x = "topright", legend = names(nicho_juvenil_estacion.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 100
nicho_juvenil_estacion.par <- tapply(1:nrow(nicho_juvenil_estacion), nicho_juvenil_estacion$Estacion,
                           function(ii) niw.post(nsamples = nsamples, X = nicho_juvenil_estacion[ii,2:4]))

# format data for plotting function
nicho_juvenil_estacion.data <- tapply(1:nrow(nicho_juvenil_estacion), nicho_juvenil_estacion$Estacion, function(ii) X = BD_iso_nicho[ii,2:4])

niche.plot(niche.par = nicho_juvenil_estacion.par, niche.data = nicho_juvenil_estacion.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 500
over.stat <- overlap(nicho_juvenil_estacion.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)



# SEGUNDA ETAPA -----------------------------------------------------------

nicho_juvenil_estacion<-na.omit(BD_iso[(BD_iso$SEXO=="Macho"),c(4,(15:17))])
## analisis utilizando por grupo= adulto, estacion y prporcion isitopica

BD_iso_nicho2<-na.omit(BD_iso[BD_iso$grupo == "Adulto" & BD_iso$Estacion %in% c("Cuya","Conanoxa"), c(4,(15:17))])

names(BD_iso_nicho)
aggregate(BD_iso_nicho[2:4], BD_iso_nicho[1], mean) # isotope means per BD_iso
result <- aggregate(BD_iso_nicho[2:4], BD_iso_nicho[1], mean)   ##########################?
length(BD_iso_nicho[2:4])
length(BD_iso_nicho[4])
str(BD_iso_nicho)

# random draws from posterior distribution with default prior
nsamples <- 500
BD_iso_nicho.par <- tapply(1:nrow(BD_iso_nicho), BD_iso_nicho$Estacion,
                           function(ii) niw.post(nsamples = nsamples, X = BD_iso_nicho[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(1.2, 1.2, 1, 1)+.1)
niche.par.plot(BD_iso_nicho.par, col = clrs)
legend(x = "topright", legend = names(BD_iso_nicho.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 100
BD_iso_nicho.par <- tapply(1:nrow(BD_iso_nicho), BD_iso_nicho$Estacion,
                           function(ii) niw.post(nsamples = nsamples, X = BD_iso_nicho[ii,2:4]))

# format data for plotting function
BD_iso_nicho.data <- tapply(1:nrow(BD_iso_nicho), BD_iso_nicho$Estacion, function(ii) X = BD_iso_nicho[ii,2:4])

niche.plot(niche.par = BD_iso_nicho.par, niche.data = BD_iso_nicho.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 500
over.stat <- overlap(BD_iso_nicho.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")

###    nicho ADULTO POR ESTACION  _____________________________________________________________________________________

#####  nicho_juvenil_estacion<-na.omit(BD_iso[BD_iso$grupo == "Adulto" & BD_iso$Estacion %in% c("Cuya"), c(4,(15:17))])
nicho_adulto_estacion<-na.omit(BD_iso[BD_iso$grupo == "Adulto" , c(4,(15:17))])

nsamples <- 500
nicho_adulto_estacion.par <- tapply(1:nrow(nicho_adulto_estacion), nicho_adulto_estacion$Estacion,
                                    function(ii) niw.post(nsamples = nsamples, X = nicho_adulto_estacion[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(1.5, 4,1, 1)+.3)
niche.par.plot(nicho_adulto_estacion.par, col = clrs)
legend(x = "topright", legend = names(nicho_adulto_estacion.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 100
nicho_adulto_estacion.par <- tapply(1:nrow(nicho_adulto_estacion), nicho_adulto_estacion$Estacion,
                                    function(ii) niw.post(nsamples = nsamples, X = nicho_adulto_estacion[ii,2:4]))

# format data for plotting function
nicho_adulto_estacion.data <- tapply(1:nrow(nicho_adulto_estacion), nicho_adulto_estacion$Estacion, function(ii) X = nicho_adulto_estacion[ii,2:4])

niche.plot(niche.par = nicho_adulto_estacion.par, niche.data = nicho_adulto_estacion.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 500
over.stat <- overlap(nicho_adulto_estacion.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)

##  nicho JUVENIL POR ESTACION  __________________________________________


nicho_juvenil_estacion<-na.omit(BD_iso[BD_iso$grupo == "Adulto" , c(4,(15:17))])

nsamples <- 500
nicho_juvenil_estacion.par <- tapply(1:nrow(nicho_juvenil_estacion), nicho_juvenil_estacion$Estacion,
                                    function(ii) niw.post(nsamples = nsamples, X = nicho_juvenil_estacion[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(1.5, 4,1, 1)+.3)
niche.par.plot(nicho_juvenil_estacion.par, col = clrs)
legend(x = "topright", legend = names(nicho_juvenil_estacion.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 100
nicho_juvenil_estacion.par <- tapply(1:nrow(nicho_juvenil_estacion), nicho_juvenil_estacion$Estacion,
                                    function(ii) niw.post(nsamples = nsamples, X = nicho_juvenil_estacion[ii,2:4]))

# format data for plotting function
nicho_juvenil_estacion.data <- tapply(1:nrow(nicho_juvenil_estacion), nicho_juvenil_estacion$Estacion, function(ii) X = nicho_juvenil_estacion[ii,2:4])

niche.plot(niche.par = nicho_juvenil_estacion.par, niche.data = nicho_juvenil_estacion.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 500
over.stat <- overlap(nicho_juvenil_estacion.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)

# filtrado de base de datos solo estacion y deltas isotopos  -----------------------------------------------------
  
nicho_estacion<-na.omit(BD_iso[ ,c(4,(12:14))])

nicho_adulto_macho<-na.omit(BD_iso[BD_iso$grupo == "Adulto" & BD_iso$SEXO %in% c("Macho"), c(4,(12:14))])  #  delta isotopos

names(BD_iso_BASE)
nicho_adulto_hembra<-na.omit(BD_iso_BASE[BD_iso_BASE$grupo == "Adulto" & BD_iso_BASE$SEXO %in% c("Hembra"), c(4,(12:14))])   #  delta isotopos
nicho_adulto_hembra[nicho_adulto_hembra$Estacion=="Conanoxa" & nicho_adulto_hembra$X15N=="4.147461" & nicho_adulto_hembra$X13C=="-27.06187" & nicho_adulto_hembra$X34S=="5.309763",]
nicho_adulto_hembra[nicho_adulto_hembra$Estacion=="Conanoxa" ,]

nicho_adulto_hembra<-na.omit(BD_iso[BD_iso$grupo == "Adulto" & BD_iso$SEXO %in% c("Hembra"), c(4,(12:14))])
nicho_hembra<-na.omit(BD_iso[BD_iso$SEXO=="Hembra", c(4,(12:14))])   #  delta isotopos

##  Aquí con 3 condiciones
nicho_adulto_hembra2<-na.omit(BD_iso[BD_iso$grupo == "Adulto" & BD_iso$SEXO %in% c("Hembra") & BD_iso$Estacion %in% c("Huancarane"), c(4,(12:14))])

names(BD_iso)

######################################################################################################################
######################################################################################################################
# TRATAMIENTOS DE NICHOS --------------------------------------------------

###    nicho de todos por estación 

nsamples <- 500
nicho_estacion.par <- tapply(1:nrow(nicho_estacion), nicho_estacion$Estacion,
                                 function(ii) niw.post(nsamples = nsamples, X = nicho_estacion[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(4.4, 4.4, 3, 3)+.1)
niche.par.plot(nicho_estacion.par, col = clrs)
legend(x = "topright", legend = names(nicho_estacion.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 100
nicho_estacion.par <- tapply(1:nrow(nicho_estacion), nicho_estacion$Estacion,
                                 function(ii) niw.post(nsamples = nsamples, X = nicho_estacion[ii,2:4]))

# format data for plotting function
nicho_estacion.data <- tapply(1:nrow(nicho_estacion), nicho_estacion$Estacion, function(ii) X = nicho_estacion[ii,2:4])

niche.plot(niche.par = nicho_estacion.par, niche.data = nicho_estacion.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 500
over.stat <- overlap(nicho_estacion.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)

##-------------------------------------------------------------------------------------------------------------


###    nicho ADULTO MACHO POR ESTACION

nsamples <- 500
nicho_adulto_macho.par <- tapply(1:nrow(nicho_adulto_macho), nicho_adulto_macho$Estacion,
                                     function(ii) niw.post(nsamples = nsamples, X = nicho_adulto_macho[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(4.4, 4.4, 3, 3)+.1)
niche.par.plot(nicho_adulto_macho.par, col = clrs)
legend(x = "topright", legend = names(nicho_adulto_macho.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 100
nicho_adulto_macho.par <- tapply(1:nrow(nicho_adulto_macho), nicho_adulto_macho$Estacion,
                                     function(ii) niw.post(nsamples = nsamples, X = nicho_adulto_macho[ii,2:4]))

# format data for plotting function
nicho_adulto_macho.data <- tapply(1:nrow(nicho_adulto_macho), nicho_adulto_macho$Estacion, function(ii) X = nicho_adulto_macho[ii,2:4])

niche.plot(niche.par = nicho_adulto_macho.par, niche.data = nicho_adulto_macho.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 500
over.stat <- overlap(nicho_adulto_macho.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)
#---------------------------------------------------------------------------------------
###    nicho ADULTO HEMBRA POR ESTACION
ncol(nicho_adulto_hembra)
nrow(nicho_adulto_hembra)
?niw.post

nsamples <- 2
nicho_adulto_hembra.par <- tapply(1:nrow(nicho_adulto_hembra), nicho_adulto_hembra$Estacion,
                                 function(ii) niw.post(nsamples = nsamples, X = nicho_adulto_hembra[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(4.4, 4.4, 3, 3)+.1)
niche.par.plot(nicho_adulto_hembra.par, col = clrs)
legend(x = "topright", legend = names(nicho_adulto_hembra.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 2
nicho_adulto_hembra.par <- tapply(1:nrow(nicho_adulto_hembra), nicho_adulto_hembra$Estacion,
                                 function(ii) niw.post(nsamples = nsamples, X = nicho_adulto_hembra[ii,2:4]))

# format data for plotting function
nicho_adulto_hembra.data <- tapply(1:nrow(nicho_adulto_hembra), nicho_adulto_hembra$Estacion, function(ii) X = nicho_adulto_hembra[ii,2:4])




niche.plot(niche.par = nicho_adulto_hembra.par, niche.data = nicho_adulto_hembra.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 2
over.stat <- overlap(nicho_adulto_hembra.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)

#_-----------------------------------------------------------------------------------------
###    nicho HEMBRA POR ESTACION
dev.off()

nsamples <- 1000
nicho_hembra.par <- tapply(1:nrow(nicho_hembra), nicho_hembra$Estacion,
                                 function(ii) niw.post(nsamples = nsamples, X = nicho_hembra[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(4.4, 4.4, 3, 3)+.1)
niche.par.plot(nicho_hembra.par, col = clrs)
legend(x = "topright", legend = names(nicho_hembra.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 100
nicho_hembra.par <- tapply(1:nrow(nicho_hembra), nicho_hembra$Estacion,
                                 function(ii) niw.post(nsamples = nsamples, X = nicho_hembra[ii,2:4]))

# format data for plotting function
nicho_hembra.data <- tapply(1:nrow(nicho_hembra), nicho_hembra$Estacion, function(ii) X = nicho_hembra[ii,2:4])

niche.plot(niche.par = nicho_hembra.par, niche.data = nicho_hembra.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 1000
over.stat <- overlap(nicho_hembra.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)

#---------------## -------------------------------------------------------------------------------------------
###      nicho JUVENILES POR ESTACION
nicho_juveniles2<-na.omit(BD_iso[BD_iso$grupo == "Juvenil", c(4,(12:14))])
nicho_juveniles<-na.omit(BD_iso[BD_iso$grupo == "Juvenil" & !BD_iso$Estacion %in% c("Huancarane"), c(4,(12:14))])#  delta isotopos
nicho_juveniles$Estacion <- factor(nicho_juveniles$Estacion, levels = c("Cuya", "Conanoxa"))

str(nicho_juveniles)
levels(nicho_juveniles$Estacion)
summary(nicho_juveniles$Estacion)
dev.off()

nsamples <- 2
nicho_juveniles.par <- tapply(1:nrow(nicho_juveniles), nicho_juveniles$Estacion,
                           function(ii) niw.post(nsamples = nsamples, X = nicho_juveniles[ii,2:4]))

# display p(mu | X) and p(Sigma | X) for each fish
clrs <- c("red", "blue", "orange") # colors for each species
par(mar = c(4.4, 4.4, 3, 3)+.1)
niche.par.plot(nicho_juveniles.par, col = clrs)
legend(x = "topright", legend = names(nicho_juveniles.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 2
nicho_juveniles.par <- tapply(1:nrow(nicho_juveniles), nicho_juveniles$Estacion,
                           function(ii) niw.post(nsamples = nsamples, X = nicho_juveniles[ii,2:4]))

# format data for plotting function
nicho_juveniles.data <- tapply(1:nrow(nicho_juveniles), nicho_juveniles$Estacion, function(ii) X = nicho_juveniles[ii,2:4])

niche.plot(niche.par = nicho_juveniles.par, niche.data = nicho_juveniles.data, pfrac = .05,
           iso.names = expression(delta^{15}*N, delta^{13}*C, delta^{34}*S),
           col = clrs, xlab = expression("Isotope Ratio (\u2030)"))

# niche overlap plots for 95% niche region sizes

# overlap calculation.  use nsamples = nprob = 1e4 for higher accuracy.
nsamples <- 2
over.stat <- overlap(nicho_juveniles.par, nreps = nsamples, nprob = nsamples, alpha = .95)

# overlap plot
overlap.plot(over.stat, col = clrs, mean.cred.col = "turquoise",
             equal.axis = TRUE,
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")


data(wrld_simpl)


############# ESCALAMOS LAS VARIABLES DE ISOTOPOS   ##############################
#================================================================================"

BD_iso_escala<-BD_iso
names(BD_iso_escala)
levels(BD_iso_escala$Estacion)
levels(BD_iso_escala$SEXO)
BD_iso_escala$grupo<-as.factor(BD_iso_escala$grupo)
levels(BD_iso_escala$grupo)


BD_iso_escala$X15N <- scale(BD_iso_escala$X15N, center = TRUE, scale = TRUE)
BD_iso_escala$X13C <- scale(BD_iso_escala$X13C, center = TRUE, scale = TRUE)
BD_iso_escala$X34S <- scale(BD_iso_escala$X34S, center = TRUE, scale = TRUE)

iso.group<-subset(BD_iso_escala, Estacion == "Cuya" | Estacion =="Huancarane")[ ,c("X15N", "X13C", "Estacion","SEXO","grupo")]
##siber.group<-subset(siber.data, group == "lsr" | group ==
"perch")[,c("iso1", "iso2", "group", "community")]





#  X15N vs X13C estacion y sexo
isotopos.NC<-ggplot(iso.group, aes(x = X13C, y = X15N, colour = Estacion)) +
  geom_point(alpha = 0.7, size=2) +
  facet_grid(. ~ SEXO) +
  theme_bw() +
  ylab("isotopo X13C") +
  xlab("isotopo X15N")
isotopos.NC
isotopos.NC+
  #stat_ellipse(position="identity", level=0.4, size=2)+
  stat_ellipse(position="identity", level=0.95, size=1)

#  X15N vs X13C estacion y grupo
isotopos.NC<-ggplot(iso.group, aes(x = X13C, y = X15N, colour = Estacion)) +
  geom_point(alpha = 0.7, size=2) +
  facet_grid(. ~ grupo) +
  theme_bw() +
  ylab("isotopo X13C") +
  xlab("isotopo X15N")
isotopos.NC
isotopos.NC+
  #stat_ellipse(position="identity", level=0.4, size=2)+
  stat_ellipse(position="identity", level=0.95, size=1)







#  X15N vs X13C
isotopos.NC<-ggplot(iso.group, aes(x = X13C, y = X15N, colour = SEXO)) +
  geom_point(alpha = 0.7, size=2) +
  facet_grid(. ~ Estacion) +
  theme_bw() +
  ylab("isotopo X13C") +
  xlab("isotopo X15N")
isotopos.NC
isotopos.NC+
  #stat_ellipse(position="identity", level=0.4, size=2)+
  stat_ellipse(position="identity", level=0.95, size=1)










#  X15N vs X13C
isotopos.NC<-ggplot(BD_iso_escala, aes(x = X15N, y = X13C, colour = SEXO)) +
  geom_point(alpha = 0.7, size=2) +
  facet_grid(. ~ Estacion=="Cuya" | Estacion=="Huancarane") +
  theme_bw() +
  ylab("isotopo X13C") +
  xlab("isotopo X15N")
isotopos.NC
isotopos.NC+
  #stat_ellipse(position="identity", level=0.4, size=2)+
  stat_ellipse(position="identity", level=0.95, size=1)

#  X15N vs X34S
isotopos.NS<-ggplot(BD_iso_escala, aes(x = X15N, y = X34S, colour = Estacion)) +
  geom_point(alpha = 0.7, size=2) +
  facet_grid(. ~ Estacion) +
  theme_bw() +
  ylab("isotopo X34S") +
  xlab("isotopo X15N")
isotopos.NS
isotopos.NS+
  stat_ellipse(position="identity", level=0.4, size=2)+
  stat_ellipse(position="identity", level=0.95, size=1)
ggsave(isotopos.NS)

#  X13C vs X34S
isotopos_CS<-ggplot(BD_iso_escala, aes(x = X13C, y = X34S, colour = Estacion)) +
  geom_point(alpha = 0.7, size=2) +
  facet_grid(. ~ Estacion) +
  theme_bw() +
  ylab("isotopo X34S") +
  xlab("isotopo X13C")
isotopos_CS
isotopos_CS+
  stat_ellipse(position="identity", level=0.4, size=2)+
  stat_ellipse(position="identity", level=0.95, size=1)



# Guardar el gráfico como un archivo PNG en el directorio de trabajo actual
ggsave(filename = "mi_grafico.png", plot = isotopos_CS, width = 6, height = 4, dpi = 300)

ggsave(filename="isotopos_CS", plot = last_plot(isotopos_CS.png), device = NULL, path = NULL, scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300)

#_________________________________________________________________

siber.group<-subset(siber.data, group == "Cuya" | group == "Huancarane",
                     

siber.group.biplots<-ggplot(siber.group, aes(x = iso1, y = iso2,
                                             colour = group)) +
  geom_point(alpha = 0.7, size=2) +
  facet_grid(. ~ community) +
  theme_bw() +
  ylab("iso2") +
  xlab("iso1")






















