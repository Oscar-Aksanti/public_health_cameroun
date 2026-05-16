# Installation packages nécessaires 
install.packages("tidyverse")
install.packages("janitor")
install.packages("skimr")
install.packages("readxl")
install.packages("shiny")
install.packages("readr")


# Chargement depackage.skeleton()
library(tidyverse)
library(janitor)
library(skimr)
library(shiny)
library(readxl)
library(readr)


# Importer la base de donnée 
health <- read.csv("data/dirty_health_data_subsaharan.csv")

View(health)
