# Installations des packages nécessaires
install.packages("tidyverse")
install.packages("janitor")
install.packages("skimr")
install.packages("readxl")
install.packages("shiny")
install.packages("lubridate")

# Charger les packages 
library(tidyverse)
library(janitor)
library(skimr)
library(readxl)
library(shiny)
library(lubridate)

# Importation de la base de donnée
health <- read.csv("data/dirty_health_data_subsaharan.csv")

# Premières vérifications
View(health)

dim(health)

names(health)

glimpse(health)

summary(health)

skim(health)
