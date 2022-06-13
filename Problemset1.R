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

###hacer el proceso de webscraping para cargar la tabla ###

db <-read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/")
db
allTables <- html_nodes(db, css = "li") ##Li es elemento lista en Html

html_elements(db, "li") 



