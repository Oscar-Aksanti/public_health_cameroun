<<<<<<< HEAD
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



  
=======
# Vérifier le chargement

head(health)

# Voir dimensions
dim(health)


# Voir noms colonnes
names(health)


# Voir structure datasets:
glimpse(health)


# Résumé statistique rapide
summary(health)


# Scanner dataset 
skim(health)




>>>>>>> 128ef0f113241960c8b27e7290aaac52362f81b8
