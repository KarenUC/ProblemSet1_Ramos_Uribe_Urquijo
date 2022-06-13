# Paula Ramos, Karen Uribe y Juan D. Urquijo
# update: 12-06-2022
## install pacman
##install.packages("pacman")
rm(list=ls())
## llamar librerias de la sesion
require(pacman)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       caret)  # Classification And REgression Training


## data adquisition
library(tidyverse)
library(rvest)
Data1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/page1.html")


###----------------- Project Set 1----------###

##### ---Limpiar Ambiente --- ###### 
rm(list = ls())

##### ---Cargar Librerías --- ###### 


library(rvest)
library(stringr)
library(dplyr)
library(RSelenium)
library(Rcpp)


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


