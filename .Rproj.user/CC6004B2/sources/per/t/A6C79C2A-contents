# Vérifier dimensions dataset 
dim(health)


# Vérifier nombre de doublons
sum(duplicated(health))

# Vérifier valeurs manquantes globales
sum(is.na(health))

# Vérifier valeurs manquantes par colonne
colSums(is.na(health))


# Vérifier types colonnes
glimpse(health)


# Vérifier catégories uniques 
unique(health$region)

unique(health$gender)

# Vérifier valeurs numériques aberrantes
summary(health$patient_age)

summary(health$treatment_cost)

# Vérifier formats dates
head(health$consultation_date, 100)

# Vérifier distributions simples 
table(health$diagnosis)

table(health$region)



  