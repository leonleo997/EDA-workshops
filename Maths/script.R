library(ggplot2)
library(DataExplorer)
library(dplyr)
library(ggthemes)
library(psych)
library(funModeling)
library(nortest)
library(knitr)

library(car)
library(nortest)
library(corrplot)
library(fitur)
library(ggplot2)
library(directlabels)
library(ggthemes)
library(scales)
library(gridExtra)
library(stats)
library(readxl)
library(reshape2)
library(dplyr)
library(BSDA)
library(e1071)

# Funciones para graficar

plotBar <- function(data){
  plt <- funModeling::freq(data, plot = FALSE) %>%
    ggplot(aes(x = var, y = frequency, fill = var)) + 
    geom_bar(stat='identity', color = 'black') +
    labs(fill = 'Tipos', y = 'Porcentaje', title = 'Frecuencia') +
    geom_text(aes(label = paste(percentage,'%')), nudge_y = 10, color = 'black', fontface = "bold") +
    scale_fill_manual(values = c("#0D1F2D", "#546A7B", "#9EA3B0", "#91999F", "#33658A", "#2F4858", "#4F5D75", "#BFC0C0", "#212D40", "#364156")) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  plt
}

plotDoughnut <- function(data){
  funModeling::freq(data, plot = FALSE) %>% ggplot(aes(x=2, y=percentage, fill=var)) +
    geom_bar(stat="identity", width=1, color="black") +
    scale_fill_manual(values = c("#0D1F2D", "#546A7B", "#9EA3B0", "#91999F", "#33658A", "#2F4858", "#4F5D75", "#BFC0C0", "#212D40", "#364156")) +
    coord_polar("y", start=0) +
    labs(fill="Categorias") + 
    theme_void() +
    geom_text(aes(label = percent(percentage/100)),position = position_stack(vjust = 0.5),color = "white", size=5) +
    xlim(0.5, 2.5) 
}

plotBoxplot <- function(data, x, y, title = '', subtitle = '', ylabel = '', caption = ''){
  plt <- ggplot(data, aes(x = x, y = y, fill = x)) +
    geom_boxplot(color = 'gray') +
    scale_fill_manual(values = c("#0D1F2D", "#546A7B", "#9EA3B0", "#91999F", "#33658A", "#2F4858", "#4F5D75", "#BFC0C0", "#212D40", "#364156")) +
    labs(fill = 'Tipos', y = ylabel, title = title, subtitle = subtitle, caption = caption) +
    theme_minimal() +
    theme(
      plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(hjust = 0.5)
      
    ) +
    xlab("")
  plt
}

plotUnivariateBoxplot <- function(data, x, xName, title) {
  ggplot(data, aes(x = x)) +
    geom_boxplot(color = 'grey30', fill = '#4F5D75') +
    labs(x = xName, title = title, caption = '') + 
    scale_x_continuous(breaks = seq(0, 50, 2)) + 
    scale_y_continuous(breaks = seq(0, 120, 20)) +
    coord_flip() +
    theme_minimal() +
    theme(
      plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(hjust = 0.5)
    )
}

plotUnivariateHistogram <- function(data, x, xName, title) {
  ggplot(data, aes(x = x)) +
    geom_histogram(bins = 1+3.322*log(nrow(data)), aes(y=..density..), color = 'black', fill = '#546A7B') +
    geom_vline(aes(xintercept=mean(x)), color="blue", linetype="dashed", size=1) + 
    geom_density(alpha=.1, fill="#FF6666") +
    labs(x = xName, y = 'Frecuencia Relativa', title = title) + 
    theme_minimal() +
    theme(
      plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(hjust = 0.5)
    ) 
}


#Loading data

load("performance.RData")

data <- base_f
head(data)

str(data)
summary(data)

# Casting to factor

educationLevelsLabels = c("none or primary education (4th grade)", "5th to 9th grade", "secondary education", "higher education")

data$Medu <- factor( data$Medu,levels=c(1:4),labels=educationLevelsLabels)
data$Fedu <- factor( data$Fedu,levels=c(1:4),labels=educationLevelsLabels)
data$sex <- factor(data$sex, levels = c('F', 'M'), labels = c('Femenino', 'Masculino'))
data$address <- factor(data$address, levels = c('R', 'U'), labels = c('Rural', 'Urbano'))
data$Pstatus <- factor(data$Pstatus, levels = c('A', 'T'), labels = c('Separados', 'Viviendo juntos'))
data$traveltime <- factor(data$traveltime,levels=c(1:3),labels=c("<15 min.","15 to 30 min.", ">30 min."))
data$absences <- factor( data$absences,levels=c(1:4),labels=c("0 to 5 absences","6 to 10 absences","11 to 20 absences" , ">20 absences"))


str(data)
summary(data)

# ANÁLISIS UNIVARIADO

# G3

## Tabla de información
data %>% select(G3) %>% psych::describe(IQR = TRUE, quant = c(.25,.75))

## Diagrama de caja
plotUnivariateBoxplot(data, data$G3, "Notas de Matematicas", "Boxplot Notas")

## Histograma
plotUnivariateHistogram(data, data$G3, "Notas Matemáticas", "Distribucion de las notas de matemáticas")

## Prueba de normalidad
qqPlot(data$G3)
shapiro.test(data$G3)

#AGE

## Tabla de información
data %>% select(age) %>% psych::describe(IQR = TRUE, quant = c(.25,.75))

# Diagrama de caja
plotUnivariateBoxplot(data, data$age, "Edad", "Boxplot edad")

# Histograma
data %>% select(age) %>% arrange(age) %>%
  ggplot(aes(x = age)) +
  geom_histogram(aes(y=..density..), breaks = seq(14, 25, 1), bins = 1+3.322*log(nrow(data)), color = 'black', fill = '#546A7B') +
  geom_vline(aes(xintercept=mean(age)), color="blue", linetype="dashed", size=1) + 
  geom_density(alpha=.1, fill="#FF6666") +
  labs(x = 'Edades', y = 'Frecuencia Absoluta', title = 'Distribucion de edades') + 
  scale_x_continuous(breaks = seq(10, 25, 1)) + 
  scale_y_continuous(limits=c(0,0.4), breaks = seq(0,0.4,0.1), labels = percent) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0.5)
  ) 

# Prueba normalidad
qqPlot(data$age)
shapiro.test(data$age)
ad.test(data$age)
lillie.test(data$age)

# Internet

## Tabla de frecuencia
freq(data$internet, plot = FALSE)

## Diagrama de barras de frecuencia
plotBar(data$internet)

## Diagrama de torta
plotDoughnut(data$internet)

# Reason

## Tabla de frecuencia
freq(data$reason, plot = FALSE)

## Diagrama de barras de frecuencia
plotBar(data$reason)

## Diagrama de torta
plotDoughnut(data$reason)

# Inasistencias

## Tabla de frecuencia
freq(data$absences, plot = FALSE)

## Diagrama de barras de frecuencia
plotBar(data$absences)


## Diagrama de torta
plotDoughnut(data$absences)

# Sexo

## Tabla de frecuencia
freq(data$sex, plot = FALSE)

## Diagrama de barras de frecuencia
plotBar(data$sex)

## Diagrama de torta
plotDoughnut(data$sex)

# Direccion

## Tabla de frecuencia
freq(data$address, plot = FALSE)

## Diagrama de barras de frecuencia
plotBar(data$address)

## Diagrama de torta
plotDoughnut(data$address)


# Estado de cohabitación de los padres

## Tabla de frecuencia
freq(data$Pstatus, plot = FALSE)

## Diagrama de barras de frecuencia
plotBar(data$Pstatus)

## Diagrama de torta
plotDoughnut(data$Pstatus)


# ANÁLISIS BIVARIADO

# Internet vs G3
internet_G3 <- data %>% select(G3, internet)

## Tabla descriptiva
psych::describeBy(internet_G3, internet_G3$internet, mat=TRUE, IQR = TRUE, quant = c(.25,.75), digits=4 ) %>% 
  na.omit() %>%
  select(-item, -vars) %>%
  rename(category = group1)

## Diagrama de caja
plotBoxplot(data, data$internet, data$G3, title = 'Calificacion final segun si el estudiante tiene acceso a internet', subtitle = 'Distribucion de las calificaciones teniendo si el estudiante tiene acceso a internet', ylabel = 'Calificacion final')

## Prueba Kruskal-Wallis
kruskal.test(G3~internet,data=internet_G3)  # Distribuciones diferentes

# Reason vs G3
reason_G3 <- data %>% select(G3, reason)

## Tabla descriptiva
psych::describeBy(reason_G3, reason_G3$reason, mat=TRUE, IQR = TRUE, quant = c(.25,.75), digits=4 ) %>% 
  na.omit() %>%
  select(-item, -vars) %>%
  rename(category = group1)

## Diagrama de caja
plotBoxplot(data, data$reason, data$G3, title = 'Calificacion final segun la razon de ingreso del estuadiante', subtitle = 'Distribucion de las calificaciones teniendo la razon de ingreso del estudiante', ylabel = 'Calificacion final')

## Prueba Kruskal-Wallis
kruskal.test(G3~reason, data=reason_G3)  # Medias iguales

# Abscences vs G3
absences_G3 <- data %>% select(G3, absences)

## Tabla descriptiva
psych::describeBy(absences_G3, absences_G3$absences, mat=TRUE, IQR = TRUE, quant = c(.25,.75), digits=4 ) %>% 
  na.omit() %>%
  select(-item, -vars) %>%
  rename(category = group1)

## Diagrama de caja
plotBoxplot(data, data$absences, data$G3, title = 'Calificacion final segun la cantidad de inasistencias del estuadiante', subtitle = 'Distribucion de las calificaciones teniendo las inasistencias del estudiante', ylabel = 'Calificacion final')

## Prueba Kruskal-Wallis
kruskal.test(G3~absences, data=absences_G3)  # Distris diferentes

## Prueba Wilcoxon
pairwise.wilcox.test(x=absences_G3$G3,g=absences_G3$absences) 

# Sexo vs G3
sex_G3 <- data %>% select(G3, sex)

## Tabla descriptiva
psych::describeBy(sex_G3, sex_G3$sex, mat=TRUE, IQR = TRUE, quant = c(.25,.75), digits=4 ) %>% 
  na.omit() %>%
  select(-item, -vars) %>%
  rename(category = group1)

## Diagrama de caja
plotBoxplot(data, data$sex, data$G3, title = 'Calificacion final segun el sexo del estudiante', subtitle = 'Distribucion de las calificaciones teniendo en cuenta el sexo del estudiante', ylabel = 'Calificacion final')

## Prueba Kruskal-Wallis
kruskal.test(G3~sex, data=sex_G3)  # Distris iguales

# Edad vs G3

## Grafico dispersion
data %>%
  ggplot(aes(x = age, y = G3)) +
  geom_point() +
  labs(x = 'Edades expresadas en anios', y = 'Calificacion final', title = 'Calificacion final segun la edad del estudiante') + 
  scale_x_continuous(breaks = seq(10, 25, 1)) + 
  scale_y_continuous(limits=c(0,25), breaks = seq(0,24,2)) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0.5)
  ) +
  geom_jitter()


## Correlacion
cor(data$age, data$G3)
cor.test(data$age, data$G3)


# Direccion vs G3
address_G3 <- data %>% select(G3, address)

## Tabla descriptiva
psych::describeBy(address_G3, address_G3$address, mat=TRUE, IQR = TRUE, quant = c(.25,.75), digits=4 ) %>% 
  na.omit() %>%
  select(-item, -vars) %>%
  rename(category = group1)

## Diagrama de caja
plotBoxplot(data, data$address, data$G3, title = 'Calificacion final segun el tipo de domicilio del estuadiante', subtitle = 'Distribucion de las calificaciones teniendo en cuenta el tipo de domicilio del estudiante', ylabel = 'Calificacion final')

## Prueba Kruskal-Wallis 
kruskal.test(G3~address, data=address_G3)  # Distris diferentes

# Pstatus vs G3
pstatus_G3 <- data %>% select(G3, Pstatus)

## Tabla descriptiva
psych::describeBy(pstatus_G3, pstatus_G3$Pstatus, mat=TRUE, IQR = TRUE, quant = c(.25,.75), digits=4 ) %>% 
  na.omit() %>%
  select(-item, -vars) %>%
  rename(category = group1)

## Diagrama de caja
plotBoxplot(data, data$Pstatus, data$G3, title = 'Calificacion final segun el estado de convivencia de los padres de los estudiantes', subtitle = 'Distribucion de las calificaciones teniendo en cuenta estado de convivencia de los padres de los estudiantes', ylabel = 'Calificacion final')

## Prueba Kruskal-Wallis 
kruskal.test(G3~Pstatus, data=pstatus_G3)  # Medias iguales

