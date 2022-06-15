# Paula Ramos, Karen Uribe y Juan D. Urquijo
# update: 15-06-2022

###----------------- Project Set 1----------###

##### ---Limpiar Ambiente --- ###### 
rm(list = ls())

##### ---Cargar Librer?as --- ###### 

library(rvest)
library(stringr)
library(dplyr)
library(RSelenium)
library(Rcpp)

require(pacman)

# usar la funci?n p_load de pacman para instalar/llamar las librer?as de la clase

p_load(rio) # Librer?a para importar datos 
p_load(tidyverse) # Librer?a para limpiar datos
p_load(e1071) # Tiene la funci?n para calcular skewness
p_load(EnvStats) # Transformaci?n Box-Cox
p_load(tidymodels) # Modelos ML
p_load(ggplot2) # Librer?a para visualizar datos
p_load(scales) # Formato de los ejes en las gr?ficas
p_load(ggpubr) # Combinar gr?ficas
p_load(knitr) # Tablas dentro de Rmarkdown
p_load(kableExtra) # Tablas dentro de Rmarkdown
p_load(skimr, # summary data
       caret)  # Classification And REgression Training

library(tidyverse)

### ---- 1.1 General Instructions --- ###

## 1. a) Data acquisition

db= "https://ignaciomsarmiento.github.io/GEIH2018_sample/"

links <- read_html(db) %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  grep("page",.,value=TRUE)

links <- links[-c(1)] 

urls <- links
base_url <- db
final_urls <- paste0(base_url, urls)
# inspect
final_urls

### --- 2. a) Data Cleaning --- ###

# Importamos los datos y los asignamos a un objeto llamado db

db <- import("https://gitlab.com/Lectures-R/bd-meca-2022-summer/lecture-01/-/raw/main/data/GEIH_sample1.Rds")

# Inspeccionamos base de datos

skim(db) %>% head()

db$depto

# Utilizando el diccionario, identificamos variables categ?ricas para volverlas a tipo factor

db <- db %>%
  mutate_at(.vars = c(
    "cclasnr11", "cclasnr2", "cclasnr3", "cclasnr4", "cclasnr5",
    "cclasnr6", "cclasnr7", "cclasnr8", "clase", "college",
    "cotPension", "cuentaPropia", "depto", "directorio", "dominio",
    "dsi", "estrato1", "formal", "ina", "inac", "informal",
    "maxEducLevel", "p6050", "microEmpresa", "ocu", "oficio", 
    "orden", "p6090", "p6100", "p6210", "p6210s1", "p6240", "p6510",
    "p6510s2", "p6545", "p6545s2", "p6580", "p6580s2", "p6585s1",
    "p6585s1a2", "p6585s2", "p6585s2a2", "p6585s4", "p6585s4a2",
    "p6590", "p6610", "p6620", "p6630s1", "p6630s2", "p6630s3",
    "p6630s4", "p6630s6", "p6920", "p7040", "p7050", "p7090",
    "p7110", "p7120", "p7140s1", "p7140s2", "p7150", "p7160",
    "p7310", "p7350", "p7422", "p7472", "p7495", "p7500s1",
    "p7500s2", "p7500s3", "p7505", "p7510s1", "p7510s2",
    "p7510s3", "p7510s5", "p7510s6", "p7510s7", "pea", "pet", 
    "regSalud", "relab", "secuencia_p", "sex", "sizeFirm", "wap"),
    .funs = factor)

# Eliminamos la variable Var.1
db <- db[, 2:ncol(db)]

### --- Filtrado base

db_filtro <- subset(db, age >= 18 & ocu == 1)

### Variables escogidas: Basados en ecuaci?n de Mincer
# Hogar: directorio
# Relaci?n jefe del hogar: p6050
# Factores de expansi?n: fex_c
# y ingreso: ingtotes (ingreso total imputado), ingtotob (ingreso total observado), ingtot (ingreso total)
# 1. Escolaridad: p6210s1, p6210, maxEducLevel
# 2. Experiencia: p6426
# 3. Edad: age
# 4. Raza: No tenemos
# 5. Sexo: sex
# 6. Estrato: estrato1
# 7. Formal o Informal: formal, informal
# 8. Area rural/urbano: clase
# 9. Ocupaci?n: oficio
# 10.Tama?o de la empresa: sizeFirm
# 11.Segundo trabajo: p7040

Base_var <- db_filtro %>% select(directorio, p6050, fex_c, ingtot, ocu, p6210s1, p6210, maxEducLevel, p6426, age, sex,  estrato1, formal, 
                                 informal, clase, oficio, sizeFirm, p7040)
skim(Base_var)

### --- Missing Values

# Sacar cantidad de NAs por variable

cantidad_na <- sapply(Base_var, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(Base_var)

# Porcentaje de observaciones faltantes. 
p <- mean(porcentaje_na[,1])
print(paste0("En promedio el ", round(p*100, 2), "% de las entradas est?n vac?as"))

# Ordenamos de mayor a menor
porcentaje_na <- arrange(porcentaje_na, desc(cantidad_na))

# Convertimos el nombre de la fila en columna
porcentaje_na <- rownames_to_column(porcentaje_na, "variable")

### --- Estadisticas Descriptivas --- ###

library(stargazer)

stargazer(Base_var)

skim(Base_var)  %>% 
  yank("factor")

# Graficas

# Ingresos vs. estrato y sexo

box_plot <- ggplot(data=Base_var , mapping = aes(as.factor(estrato1) , ingtot)) + 
  geom_boxplot()

box_plot <- box_plot +
  geom_point(aes(colour=as.factor(sex))) +
  scale_color_manual(values = c("0"="red" , "1"="blue") , label = c("0"="Hombre" , "1"="Mujer") , 
                     name = "Sexo")
box_plot

# Densidad Ingresos por formal, informal 


graph2 <- ggplot(data = Base_var , 
                 mapping = aes(x = age , y = ingtot , group=as.factor(formal) , color=as.factor(formal))) +
  geom_point()

graph2


###--- 3. Age-earnings profile


