# Paula Ramos, Karen Uribe y Juan D. Urquijo
# update: 18-06-2022
###----------------- Project Set 1----------###

##### ---Limpiar Ambiente --- ###### gncbnnb

rm(list = ls())

##### ---Cargar Librer?as --- ###### 

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
       caret, # Classification And REgression Training
       rvest,
       stringr,
       dplyr,
       RSelenium,
       Rcpp,
       robotstxt,
       sjPlot)

library(tidyverse)
library(robotstxt)

### ---- 1.1 General Instructions --- ###

## 1. a) Data acquisition

geih <- data.frame()
for (i in 1:10){
  url <-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i, ".html")
  temp <- read_html(url) %>%
    html_table()
  geih <- rbind(geih, temp)
}


###validar restricciones en los datos con robots.txt

browseURL("https://ignaciomsarmiento.github.io/GEIH2018_sample/robots.txt")

#No existe robots.txt por tanto no hay restricciones para scrappear

### --- 2. a) Data Cleaning --- ###

# Importamos los datos de otra fuente para validar que sean los mismos y los asignamos a un objeto llamado db

x <- import("https://gitlab.com/Lectures-R/bd-meca-2022-summer/lecture-01/-/raw/main/data/GEIH_sample1.Rds")

# RTA: Se encuentra que son iguales

# Se toma la base que se hizo web scrapping

db <- geih

# Inspeccionamos base de datos

skim(db) %>% head()

db$depto

# Utilizando el diccionario, identificamos variables categoricas para volverlas a tipo factor

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

# Eliminamos la variable Var.1 que solo corresponde al numero de filas
db <- db[, 2:ncol(db)]

### --- Filtrado base para mayores de 18 y ocupados

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

Base_var <- db_filtro %>% select(directorio, p6050, fex_c, ingtot, ocu, p6210s1,
                                 p6210, maxEducLevel, p6426, age, sex, estrato1,
                                 formal, informal, clase, oficio, sizeFirm, p7040,
                                 hoursWorkUsual, relab, cuentaPropia,
                                 microEmpresa, college)
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

### --- Imputar variables---###

#Identificar cuantos ingresos son <=0
ing_0 <- Base_var$ingtot==0
sum(ing_0)

##Crear promedio del ingreso
Base_var = Base_var %>% 
  group_by(directorio) %>% 
  mutate(mean_ingtot = mean(ingtot))

#Impute a ingresos totales iguales a 0 el promedio del ingreso del hogar
Base_var = Base_var %>%
  mutate(ingtot = ifelse(ingtot==0,
                         yes = mean_ingtot,
                         no = ingtot))

#Validar que no queden 0 en ingreso despu?s de imputar

ing_0 <- Base_var$ingtot==0
sum(ing_0)

#Imputar restantes con la media total de la muestra

#Crear media total
mu<- mean(Base_var$ingtot)

#Imputar informaci?n para observaci?n sin miembros de hogar
Base_var = Base_var %>%
  mutate(ingtot = ifelse(ingtot==0,
                         yes = mu,
                         no = ingtot))

#Nueva validacion

ing_0 <- Base_var$ingtot==0
sum(ing_0)

### --- Limpieza outliers --- ###

x<-Base_var$ingtot
lambda<-boxcox(x, objective.name = "Log-Likelihood", optimize = T)$lambda
#Transformamos la variable
Base_var$ingtot_boxcox<-boxcoxTransform(x, lambda)

hist_ingtot<-ggplot()+
  geom_histogram(aes(x = Base_var$ingtot), fill = "darkgreen", alpha = 0.3)

hist_ingtot_boxcox<-ggplot()+
  geom_histogram(aes(x = Base_var$ingtot_boxcox), fill = "darkblue", alpha = 0.3)

ggarrange(hist_ingtot, hist_ingtot_boxcox, nrow = 1, ncol = 2)

### --- Estadisticas Descriptivas --- ###

library(sjPlot)

library(stargazer)

graph_base <- as.data.frame(Base_var)

stargazer(graph_base[c("ingtot", "age", "p6426" )], type="text", flip = TRUE, digits = 0)
stargazer(graph_base[c("ingtot", "age", "p6426" )], type="latex", flip = TRUE, digits = 0)

stargazer(graph_base[c("ingtot", "age", "p6426" )], type='text', flip = TRUE,
          digits=0, header=FALSE, 
          summary.stat=c('N', 'mean', 'sd','min' ,'median', 'max'))

stargazer(graph_base[c("ingtot", "age", "p6426")], type='latex', flip = TRUE,
          digits=0, header=FALSE, 
          summary.stat=c('N', 'mean', 'sd','min' ,'median', 'max'))



# Graficas

# Ingresos vs. estrato y sexo

box_plot <- ggplot(data=Base_var , mapping = aes(as.factor(estrato1) , ingtot)) + 
  geom_boxplot()

box_plot <- box_plot +
  geom_point(aes(colour=as.factor(sex))) +
  scale_color_manual(values = c("0"="red" , "1"="blue") ,
                     label = c("0"="Hombre" , "1"="Mujer") , 
                     name = "Sexo") +
  labs(x= "Estrato Socioecon?mico", y ="Ingresos Totales") 


box_plot_boxcox<- ggplot(data=Base_var , mapping = aes(as.factor(estrato1) , ingtot_boxcox)) + 
  geom_boxplot()

box_plot_boxcox <- box_plot_boxcox +
  geom_point(aes(colour=as.factor(sex))) +
  scale_color_manual(values = c("0"="red" , "1"="blue") ,
                     label = c("0"="Hombre" , "1"="Mujer") , 
                     name = "Sexo") +
  labs(x= "Estrato Socioecon?mico", y ="Ingresos Totales (boxcox)") 

box_plot_boxcox

# Densidad Ingresos por formal, informal 

graph2 <- ggplot(data = Base_var , 
                 mapping = aes(x = age , y = ingtot , group=as.factor(formal))) +
  geom_point(aes(colour=as.factor(formal))) +
  scale_color_manual(values = c("0"="cadetblue3" , "1"="coral") ,
                     label = c("0"="Informal" , "1"="Formal") , 
                     name = "Formal") +
  labs(x = "Edad", y = "Ingresos Totales")

graph2

graph2_boxcox <- ggplot(data = Base_var , 
                        mapping = aes(x = age , y = ingtot_boxcox , group=as.factor(formal))) +
  geom_point(aes(colour=as.factor(formal))) +
  scale_color_manual(values = c("0"="cadetblue3" , "1"="coral") ,
                     label = c("0"="Informal" , "1"="Formal") , 
                     name = "Formal") +
  labs(x = "Edad", y = "Ingresos Totales")

graph2_boxcox


###--- 3. Age-earnings profile
## Escoger variable para salario
# Se escoge la variable ingresos totales después de revisar la base, dado que 
#es la suma del ingreso total + el ingreso imputado (ingresos adicionales)

library(jtools)
##  OLS Age-earnings model - sin transformación
Base_var$age_2 <- (Base_var$age)^2
model_income<-lm(ingtot~age + age_2, 
                 data= Base_var)
summ(model_income)

##  OLS Age-earnings model - Con trasformación boxcox
model_income_boxcox<-lm(ingtot_boxcox~age + age_2, 
                        data= Base_var)
summ(model_income_boxcox)

#Comparar modelos
stargazer(model_income, model_income_boxcox, type = "text")

### Qué tan bueno es el fit del modelo?
#El fit es malo según Rcuadrado - Revisar argumento


# Guardar los predichos del modelo en datos
Base_var$nuevo_pred <- model_income$fitted.values
# Guardar los residuos
Base_var$residuos <- model_income$residuals 

plot1 <- ggplot(Base_var,                                     # Draw plot using ggplot2 package
       aes(x = Base_var$nuevo_pred,
           y = Base_var$ingtot)) +
  geom_point(colour= "gray") +
  geom_abline(intercept = 0,
              slope = 1,
              color = "green",
              size = 2) +
  labs(x= "Ingreso Total Observado", y= "Ingreso Total Predicho")


plot2 <- ggplot(Base_var,                                     # Draw plot using ggplot2 package
       aes(x = Base_var$ingtot_boxcox,
           y = predict(model_income_boxcox)) +
  geom_point(colour= "gray") +
  geom_abline(intercept = 0,
              slope = 1,
              color = "green",
              size = 2) +
  labs(x= "Ingreso Total Observado - Transofrmaci?n Box-cox", y= "Ingreso Total Predicho - Transformaci?n Box-cox"))

ggarrange(plot1, plot2)


#Plot the predicted age-earnings profile implied by the above equation.
Base_var$prediccion1<-predict(model_income)
Base_var$prediccion1_boxcox<-predict(model_income_boxcox)

########
#Graficas predicci?n y valor observado

cor(Base_var$ingtot,Base_var$prediccion1)
ggplot(Base_var, aes(x = prediccion1, y = ingtot)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "green")

g1<-ggplot(Base_var, aes(x = age, y = ingtot)) + geom_point(colour = "darkcyan") + labs(x = "Edad", y = "Ingresos Totales")

g2<-ggplot(Base_var, aes(x = age, y = prediccion1)) + geom_point(colour = "violetred2") + labs(x = "Edad", y = "Ingresos Totales Predicha")

cor(Base_var$ingtot_boxcox,Base_var$prediccion1_boxcox)

ggplot(Base_var, aes(x = prediccion1, y = ingtot_boxcox)) +
  geom_point() + geom_abline(intercept = 0, slope = 1, color = "blue") 
  

g2_boxcox<-ggplot(Base_var, aes(x = age, y = prediccion1_boxcox)) + geom_point(colour = "violetred2") + labs(x = "Edad", y = "Ingresos Totales Predicha - Boxcox")

ggarrange(g1, g2, g2_boxcox, nrow = 1, ncol = 3)

#### Encontrar medida de incertidumbre para los coeficientes del modelo
p_load(boot)
R<-1000
fun<-function(Base_var,index){
  coef(lm(ingtot~age + age_2, data= Base_var, subset = index))
}
boot(Base_var, fun, R)
## Edad optima
b1<-model_income$coefficients[2]
b2<-model_income$coefficients[3]
edad_optima<--(b1/(2*b2))
edad_optima ##56

fun2<-function(Base_var,index){
  coef(lm(ingtot_boxcox~age + age_2, data= Base_var, subset = index))
}
boot(Base_var, fun, R)
## Edad optima
b1_boxcox<-model_income_boxcox$coefficients[2]
b2_boxcox<-model_income_boxcox$coefficients[3]
edad_optima_boxcox<--(b1_boxcox/(2*b2_boxcox))

edad_optima
edad_optima_boxcox#44

#Intervalos de confianza
CI_age<-confint(model_income, level=0.95)
CI_age_boxcox<-confint(model_income_boxcox, level=0.95)

CI_age
CI_age_boxcox

###--- 4. The earnings GAP ---###########
#### Crear variable female y log income
Base_var = Base_var %>% 
  mutate(female = ifelse(sex == 0,1,0))

Base_var$log_income<-log(Base_var$ingtot)
Base_var$female<-as.factor(Base_var$female)
skim(Base_var$log_income)

#Validar es si hay NAS
sum(is.na(Base_var$female))
sum(is.na(Base_var$log_income))

#Modelo unconditional earnings gap
model_income_female<-lm(log_income~female, data= Base_var)
summ(model_income_female)
stargazer(model_income_female, type = "text")
stargazer(model_income_female, type = "latex")

##Qué tan bueno es el modelo? Interpretación del R2


##Estimar y graficar the predicted age-earnings profile by gender
model_income_age_female <-lm(ingtot~age + age_2,data=subset(Base_var,female==1))
summary(model_income_age_female)

model_income_age_male <-lm(ingtot~age + age_2,data=subset(Base_var,female==0))
summary(model_income_age_male)

stargazer(model_income_age_female, model_income_age_male, type = "text") 
##los coeficientes son diferentes para hombres y mujeres; pero el intercepto es muy parecido

#### Encontrar medida de incertidumbre para los coeficientes del modelo
R<-1000
fun_female<-function(Base_var,index){
  coef(lm(ingtot~age + age_2,data=subset(Base_var,female==1), subset = index))
}
boot(Base_var, fun_female, R)
## Edad optima
b1_female<-model_income_age_female$coefficients[2]
b2_female<-model_income_age_female$coefficients[3]
edad_optima_female<--(b1_female/(2*b2_female))
edad_optima_female ##55

fun_male<-function(Base_var,index){
  coef(lm(ingtot~age + age_2,data=subset(Base_var,female==0), subset = index))
}
boot(Base_var, fun_male, R)
## Edad optima
b1_male<-model_income_age_male$coefficients[2]
b2_male<-model_income_age_male$coefficients[3]
edad_optima_male<--(b1_male/(2*b2_male))
edad_optima_male ##54

#Intervalos de confianza
CI_age_female<-confint(model_income_age_female, level=0.95)
CI_age_male<-confint(model_income_age_male, level=0.95)

CI_age_female
CI_age_male

##Calculo de predicciones
Base_var$predict_gender<-ifelse(Base_var$female==1, predict(model_income_age_female), predict(model_income_age_male))

#Graficar predicciones para hombre y para mujer
Base_gender_female<-subset(Base_var, female==1)
Base_gender_female$female_pred<-predict(model_income_age_female)
g_female_p<-ggplot(Base_gender_female, aes(x = age, y = female_pred)) + geom_point(colour = "orange1") + labs(x = "Edad", y = "Ingresos Totales Predicha Mujer")


Base_gender_male<-subset(Base_var, female==0)
Base_gender_male$male_pred<-predict(model_income_age_male)
g_male_p<-ggplot(Base_gender_male, aes(x = age, y = male_pred)) + geom_point(colour = "steelblue1") + labs(x = "Edad", y = "Ingresos Totales Predicha Hombre")


ggarrange(g1, g_female_p, g_male_p, nrow = 1, ncol = 3)

####----Equal Pay for Equal Work?-----###

#Modelo unconditional earnings gap con controles
#p6426 = ¿cuanto tiempo lleva ... Trabajando en esta empresa, negocio, industria, oficina
################################################################
#Modelo unconditional earnings gap con controles

model_controls<-lm(log_income~female + 
                     formal + age + age_2 + estrato1 
                   + maxEducLevel + p6426, data= Base_var)
summ(model_controls)
stargazer(model_controls, type = "text")

##Teorema FWL

##Estimación del modelo con los residuales
#Modelo sin incluir variable de interes
y_controles<-lm(log_income~ formal + age + age_2 + estrato1 
                + maxEducLevel + p6426, data= Base_var)
summ(y_controles)

Base_var = Base_var %>% 
  mutate(femalenum = ifelse(sex == 0,1,0))

class(Base_var$femalenum)

female_controles<-lm(femalenum~ formal + age + age_2 + estrato1 
                     + maxEducLevel + p6426, data= Base_var)
summ(female_controles)

Base_var$res_y= y_controles$residuals
Base_var$res_f= female_controles$residuals

reg_final<-lm(res_y~ res_f, data=Base_var)
summ(reg_final)

stargazer(model_controls, reg_final, type = "text")
stargazer(model_controls, reg_final, type = "latex")

###Falta la interpretación!!!!!
###############
##### Transformación para encontrar experiencia potencial
# Es complicado encontrar una variable que muestre cuántos años ha trabajado una persona en realidad.
#Por ello, en la literatura se ha utilizado como proxy de la experiencia la experiencia potencial.
#Esta nace de restarle a la edad de la persona los años que ha estudiado y, 
#además, cinco (5) años -pues en sus años de primera infancia ni estudió ni trabajó.
#p6210 = ¿Cuál es el nivel educativo más alto alcanzado por .... y el último año o grado
#maxEducLevel = 	max. education level attained

Base_var<- Base_var %>% mutate(maxEducLevel_years = ifelse(maxEducLevel==1, 0,
                                                           ifelse(maxEducLevel==2, 1,
                                                                  ifelse(maxEducLevel==3, 4, 
                                                                         ifelse(maxEducLevel==4, 5,
                                                                                ifelse(maxEducLevel==5, 10,
                                                                                       ifelse(maxEducLevel==6, 11,
                                                                                              ifelse(maxEducLevel==7, 15,
                                                                                                     ifelse(maxEducLevel==9, NA, NA)))))))))  

Base_var<- Base_var %>% mutate(p6210_years = ifelse(p6210==1, 0,
                                                    ifelse(p6210==2, 1,
                                                           ifelse(p6210==3, 5, 
                                                                  ifelse(p6210==4, 9,
                                                                         ifelse(p6210==5, 13,
                                                                                ifelse(p6210==6, 19,
                                                                                       ifelse(p6210==9, 0,0))))))))                                                                                         

Base_var<- Base_var %>% mutate(exp_pot_maxedu = age - maxEducLevel_years - 5)
Base_var<- Base_var %>% mutate(exp_pot_p6210 = age - p6210_years - 5)
Base_var<- Base_var %>% mutate(exp_pot_p6210 = ifelse(exp_pot_p6210>=0,exp_pot_p6210, 0))

a1<-ggplot(Base_var, aes(x = p6210, y = ingtot)) + geom_point()
a2<-ggplot(Base_var, aes(x = maxEducLevel, y = ingtot)) + geom_point()
a3<-ggplot(Base_var, aes(x = exp_pot_p6210, y = ingtot)) + geom_point()
a4<-ggplot(Base_var, aes(x = exp_pot_maxedu, y = ingtot)) + geom_point()
ggarrange(a1, a2, a3, a4, nrow = 2, ncol = 2)

skim(Base_var$p6210)
skim(Base_var$maxEducLevel)

Base_var$exp_pot_p6210_2 <- Base_var$exp_pot_p6210^2

#### ----- 5.Predicting Earnigns 
set.seed(10101)
##Dividir muestra
sample <- sample.int(n = nrow(Base_var), size = floor(.70*nrow(Base_var)), replace = F)
train <- Base_var[sample, ]
test  <- Base_var[-sample, ]

##Estimar modelo solo incluyendo la constante - Benchmark
model_cte<-lm(ingtot~1,data=train)
summary(model_cte)
stargazer(model_cte, type = "text")
coef(model_cte)
mean(train$ingtot)

##Estimar los modelos anteriores
##1. Age
model_ing_train<-lm(ingtot~age + age_2, 
                    data= train)
summ(model_ing_train)

##2. Female
model_ing_fem_train<-lm(log_income~female, data= train)
summ(model_ing_fem_train)

##3. Female and controls
model_cont_train<-lm(log_income~female + 
                       formal + age + age_2 + estrato1 
                     + maxEducLevel + p6426, data= train)
summ(model_cont_train)

stargazer(model_ing_train, model_ing_fem_train, model_cont_train , type = "text")

#################### Discutir modelos
test$model_cte_p<-predict(model_cte,newdata = test)
MSE_ingtot_cte<-with(test,mean((ingtot-model_cte_p)^2))

test$model_ing_p<-predict(model_ing_train,newdata = test)
MSE_ingtot_age<-with(test,mean((ingtot-model_ing_p)^2))

test$model_ing_fem_p<-predict(model_ing_fem_train,newdata = test)
MSE_logincome_fem<-with(test,mean((ingtot-model_ing_fem_p)^2))

test$model_cont_p<-predict(model_cont_train,newdata = test)
MSE_logincome_fem_controls<-with(test,mean((ingtot-model_cont_p)^2))

MSE<-rbind(MSE_ingtot_cte, MSE_ingtot_age, MSE_logincome_fem, MSE_logincome_fem_controls)
min(MSE)
#####################
### ----- 5 models ----#######

##1_Educación y experiencia
train$experiencia_2<-train$p6426^2
model_1_train<-lm(ingtot_boxcox~ maxEducLevel + exp_pot_p6210 + exp_pot_p6210_2, 
                     data= train)
summ(model_1_train)
stargazer(model_1_train , type = "text")

##2_Horas de trabajo y segundo trabajo (p7040)
train<-train %>% mutate(dos_trabajo = ifelse(p7040==2, 0,1))

model_2_train<-lm(ingtot_boxcox ~ maxEducLevel + exp_pot_p6210 + exp_pot_p6210_2
                  +hoursWorkUsual + dos_trabajo, 
data= train)
summ(model_2_train)
stargazer(model_2_train , type = "text")

##3_Sexo,Cuenta propia e informal
model_3_train<-lm(ingtot_boxcox ~ maxEducLevel + exp_pot_p6210 + exp_pot_p6210_2
                  +hoursWorkUsual + dos_trabajo + female + cuentaPropia + informal, 
                  data= train)
summ(model_3_train)
stargazer(model_3_train , type = "text")

##4 Microempresa, tamaño de la firma, relación laboral
model_4_train<-lm(ingtot_boxcox~ maxEducLevel + exp_pot_p6210 + exp_pot_p6210_2
                  +hoursWorkUsual + dos_trabajo + female + cuentaPropia + informal
                  +microEmpresa + sizeFirm + relab, 
data= train)
summ(model_4_train)
stargazer(model_4_train , type = "text")

##5 Age, college*sex y relab, edad
model_5_train<-lm(ingtot_boxcox~ maxEducLevel + exp_pot_p6210 + exp_pot_p6210_2
                  +hoursWorkUsual + dos_trabajo + female + cuentaPropia + informal
                  +microEmpresa + sizeFirm + relab+ age + female + college:female , 
                  data= train)

summ(model_5_train)
stargazer(model_5_train , type = "text")

stargazer(model_1_train,model_2_train,model_3_train,model_4_train,model_5_train, type = "text")
#table(Base_var$sizeFirm)
#levels(Base_var$oficio)
#lm(ingtot_boxcox~relab, train)
#lm(ingtot_boxcox~sizeFirm, train)
#levels(train$relab)
#levels(train$sizeFirm)

##Reportar y comparar el error medio de predicción de los 8 modelos

residuals1<-mean(model_ing_train$residuals)
residuals2<-mean(model_ing_fem_train$residuals)
residuals3<-mean(model_cont_train$residuals)
residuals4<-mean(model_1_train$residuals)
residuals5<-mean(model_2_train$residuals)
residuals6<-mean(model_3_train$residuals)
residuals7<-mean(model_4_train$residuals)
residuals8<-mean(model_5_train$residuals)

table1<-rbind(residuals1,residuals2, residuals3, residuals4, residuals5, residuals6, residuals7, residuals8)
colnames(table1) <- c("value")
min(table1) ##Lowest average prediction error_ modelo_1_train (Modelo de mincer educación y experiencia)

###### Discutir modelos en TEST SAMPLE
test<-test %>% mutate(dos_trabajo = ifelse(p7040==2, 0,1))
test$model_1_p<-predict(model_1_train,newdata = test)
MSE_model_1<-with(test,mean((ingtot_boxcox-model_1_p)^2))

test$model_2_p<-predict(model_2_train,newdata = test)
MSE_model_2<-with(test,mean((ingtot_boxcox-model_2_p)^2))

test$model_3_p<-predict(model_3_train,newdata = test)
MSE_model_3<-with(test,mean((ingtot_boxcox-model_3_p)^2))

test$model_4_p<-predict(model_4_train,newdata = test)
MSE_model_4<-with(test,mean((ingtot_boxcox-model_4_p)^2))

test$model_5_p<-predict(model_5_train,newdata = test)
MSE_model_5<-with(test,mean((ingtot_boxcox-model_5_p)^2))

MSE<-rbind(MSE_ingtot_cte, MSE_ingtot_age, MSE_logincome_fem, MSE_logincome_fem_controls,
           MSE_model_1, MSE_model_2, MSE_model_3, MSE_model_4, MSE_model_5)
min(MSE)

MSE_2<-rbind(MSE_model_1, MSE_model_2, MSE_model_3, MSE_model_4, MSE_model_5)
min(MSE_2)

####

########################################################################################################################################
## Mejor modelo (lowest average prediction error)
##Compute the leverage statistic for each observation in the test sample
library(dplyr)
test$experiencia_2<-test$p6426^2

Coefs_leverage<-c(rep(0, 466))
Coefs_leverage

matriz<-diag(466)

test_mat<-cbind(test,matriz)
colnames(test_mat)

for (i in 37:503){
    i<-37
    #a<-paste0("...",i)
    a<-as.factor(test_mat[ ,i:i])
reg_test_1<-lm(log_income~ maxEducLevel + p6426 + experiencia_2 + a , 
                 data= test_mat)
reg_test_2<-lm(log_income~ maxEducLevel + p6426 + experiencia_2,test_mat)$residuals
reg_test_3 <-lm(a~ maxEducLevel + p6426 + experiencia_2,test_mat)$residuals
reg_test_leverage<-lm(reg_test_2~reg_test_3 )

b2<-reg_test_leverage$coefficients[2]
Coefs_leverage<-rbind(Coefs_leverage,b2)
}

reg_test_1<-lm(log_income~ maxEducLevel + p6426 + experiencia_2 + ...37, 
               data= test_mat)

#Sugerencia de Ignacio#############
GIH<-data.frame(age=runif(30,18,80))
GIH<- GIH %>% mutate(age2=age^2,
                     income=rnorm(30,mean=12+0.06*age-0.001*age2))                

for(i in 1:dim(GIH)[1]){
  #Estimate the regression model using all but the i − th observation
  reg_1<-lm(income~age+age2,GIH[-i,])
  #Calculate the prediction error for the i − th observation, i.e. (yi − yˆi)
  y_hat<-predict(reg_1,newdata=GIH[i,])
  u<-(GIH[i,]$income-y_hat)^2
}


########################################################################################################################################

### ----- K-fold Validación cruzada ------#####
p_load(caret)

modelcte1<-lm(ingtot_boxcox~1,data=train)
summary(modelcte1)
stargazer(modelcte1, type = "text")
coef(modelcte1)
mean(train$ingtot_boxcox)
RMSE_modelcte1<-with(test,mean((ingtot_boxcox-coef(modelcte1))^2))


Base_var<-Base_var %>% mutate(dos_trabajo = ifelse(p7040==2, 0,1))

model1 <- train(ingtot_boxcox~ maxEducLevel + exp_pot_p6210 + exp_pot_p6210_2, 
                data = Base_var, trControl = trainControl(method = "cv", number = 5), 
                method = "lm")


model2 <- train(ingtot_boxcox ~ maxEducLevel + exp_pot_p6210 + exp_pot_p6210_2
                +hoursWorkUsual + dos_trabajo,
                data = Base_var, trControl = trainControl(method = "cv", number = 5), 
                method = "lm")

model3 <- train(ingtot_boxcox ~ maxEducLevel + exp_pot_p6210 + exp_pot_p6210_2
                +hoursWorkUsual + dos_trabajo + female + cuentaPropia + informal, 
                data = Base_var, trControl = trainControl(method = "cv", number = 5), 
                method = "lm")

model4 <- train(ingtot_boxcox~ maxEducLevel + exp_pot_p6210 + exp_pot_p6210_2
                +hoursWorkUsual + dos_trabajo + female + cuentaPropia + informal
                +microEmpresa + sizeFirm + relab,
                data = Base_var, trControl = trainControl(method = "cv", number = 5), 
                method = "lm")


model5 <- train(ingtot_boxcox~ maxEducLevel + exp_pot_p6210 + exp_pot_p6210_2
                +hoursWorkUsual + dos_trabajo + female + cuentaPropia + informal
                +microEmpresa + sizeFirm + relab + age + female + college:female ,
                data = Base_var, trControl = trainControl(method = "cv", number = 5), 
                method = "lm")
###El modelo 6 se estima para validar las anteriores especificaciones
model6 <- train(ingtot_boxcox~ maxEducLevel + exp_pot_p6210 + exp_pot_p6210_2
                +poly(hoursWorkUsual,9) + dos_trabajo + female + cuentaPropia + informal:age
                +microEmpresa + sizeFirm + relab+ poly(age,8) + female:cuentaPropia + college:female ,
                data = Base_var, trControl = trainControl(method = "cv", number = 5), 
                method = "lm")

kfold_RMSE<-rbind(model1$results,model2$results,model3$results,model4$results,model5$results, model6$results)
kfold_RMSE$name<-rbind("model1","model2","model3", "model4", "model5","model6")


ggplot(kfold_RMSE, aes(x = name, y = RMSE)) + geom_line()+ geom_point()
View(kfold_RMSE)

ggplot(kfold_RMSE, aes(x = name, y = RMSE)) + geom_line()+ geom_point()

##########################################################################################################################################################################

### ----- LOOCV ------#####

#LOOCV -sin relab
error_LOOCV <- c()
for(i in 1:dim(train)[1]){
  modelo  <- lm(ingtot_boxcox~ maxEducLevel + exp_pot_p6210 + exp_pot_p6210_2
                +hoursWorkUsual + dos_trabajo + female + cuentaPropia + informal
                +microEmpresa + sizeFirm ,data=train[-i,])
  error_LOOCV[i] <- train$ingtot_boxcox[i]-predict(modelo,train[i,])
  print(i)
}
mean(error_LOOCV*error_LOOCV)

##Compare the results to those obtained in the computation of the leverage statistic


