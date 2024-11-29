# Adjuntar los datos
attach(cajas)

# Convertir las columnas a factores
cajas$`Diseño de la Caja` <- as.factor(cajas$`Diseño de la Caja`)
cajas$`Peso del Contenido (kg)` <- as.factor(cajas$`Peso del Contenido (kg)`)
cajas$Superficie <- as.factor(cajas$Superficie)

# Resumen de los datos 
tapply(cajas$`Distancia de Deslizamiento (cm)`, 
       list(cajas$`Diseño de la Caja`, cajas$Superficie, cajas$`Peso del Contenido (kg)`), 
       summary)

# Gráficos de cajas por cada factor
par(mfrow=c(1,3)) 
boxplot(cajas$`Distancia de Deslizamiento (cm)` ~ cajas$`Diseño de la Caja`, 
        main="Diseño de la Caja", col="lightblue")
boxplot(cajas$`Distancia de Deslizamiento (cm)` ~ cajas$Superficie, 
        main="Superficie", col="lightgreen")
boxplot(cajas$`Distancia de Deslizamiento (cm)` ~ cajas$`Peso del Contenido (kg)`, 
        main="Peso del Contenido", col="lightpink")

# Gráficos de medias para cada factor
require(gplots)
par(mfrow=c(1,3))
plotmeans(cajas$`Distancia de Deslizamiento (cm)` ~ cajas$`Diseño de la Caja`, 
          main="Medias por Diseño de la Caja")
plotmeans(cajas$`Distancia de Deslizamiento (cm)` ~ cajas$Superficie, 
          main="Medias por Superficie")
plotmeans(cajas$`Distancia de Deslizamiento (cm)` ~ cajas$`Peso del Contenido (kg)`, 
          main="Medias por Peso del Contenido")

# Modelo ANOVA con interacción entre los tres factores
modelo <- aov(`Distancia de Deslizamiento (cm)` ~ `Diseño de la Caja` * Superficie * `Peso del Contenido (kg)`, 
              data = cajas)
summary(modelo)

# Media general de la variable de respuesta
mean(cajas$`Distancia de Deslizamiento (cm)`)

# Pruebas de supuestos
# 1. Normalidad de los residuos
residuals <- rstandard(modelo)
shapiro.test(residuals)

# 2. Homocedasticidad
library(lmtest)
bptest(modelo)

# 3. Independencia
bgtest(modelo)


# Pruebas para los factores principales
require(agricolae)
LSD.test(modelo, "Diseño de la Caja", console = TRUE, group = FALSE)
HSD.test(modelo, "Diseño de la Caja", group = FALSE, console = TRUE)

LSD.test(modelo, "Superficie", console = TRUE, group = FALSE)
HSD.test(modelo, "Superficie", group = FALSE, console = TRUE)

LSD.test(modelo, "Peso del Contenido (kg)", console = TRUE, group = FALSE)
HSD.test(modelo, "Peso del Contenido (kg)", group = FALSE, console = TRUE)

interaction.plot(
  cajas$Superficie,
  cajas$`Diseño de la Caja`,
  cajas$`Distancia de Deslizamiento (cm)`,
  col = c("red", "blue", "green"),
  main = "Interacción Diseño y Superficie por Peso",
  legend = TRUE
)

interaction.plot(
  cajas$`Peso del Contenido (kg)`,
  cajas$`Diseño de la Caja`,
  cajas$`Distancia de Deslizamiento (cm)`,
  col = c("orange", "purple", "cyan"),
  main = "Interacción Diseño y Peso por Superficie",
  legend = TRUE
)

