# Supprimer doubles , traiter valeurs manquantes, ...

# Partie 1 : Supprimer les doublons

sum(duplicated(health))

health <- distinct(health)

dim(health)

# Partie 2 : Traiter valeurs manquantes

colSums(is.na(health) | health == "") 

# Supprimer les lignes critiques
health <- health %>% 
  filter(
    diagnosis !="",
    region !=""
  ) %>% 
  drop_na(diagnosis, region)

colSums(is.na(health) | health == "") 

dim(health)


# Remplacer coût manqquant 
median(health$treatment_cost, na.rm=TRUE)

health <- health %>% 
  mutate(
    treatment_cost = ifelse(
      is.na(treatment_cost),
      median(treatment_cost, na.rm = TRUE),
      treatment_cost
    )
  )

colSums(is.na(health) | health == "") 


# Remplacer gender manquant 

health <- health %>% 
  mutate (
    gender = ifelse(
      is.na(gender) | gender=="",
      "Unknown",
      gender
    )
  )

colSums(is.na(health) | health == "") 


# REmplacer insurance_status manquant 
health <- health %>% 
  mutate (
    insurance_status = ifelse(
      is.na(insurance_status) | insurance_status=="",
      "Unknown",
      insurance_status
    )
  )

colSums(is.na(health)) 

glimpse(health)

# Sauvegarder une version intermédiaire
write_csv(
  health,
  "outputs/health_step1_clean.csv"
)


unique(health$region)

# Corriger catégiries avec mutate + case_when
health <- health %>% 
  mutate(
    region = case_when(
      region %in% c("centre", "CENTER", "Ctr") ~ "Centre",
      region %in% c("litoral", "LITTORAL" ) ~ "Littoral",
      region %in% c("sud ouest", "SW") ~ "Sud-Ouest",
      TRUE ~ region
      
    )
  )

unique(health$region)

unique(health$gender)

# Harmoniser gender
health <- health %>% 
  mutate(
    gender = case_when(
      gender %in% c("male", "Male", "M") ~ "Masculin",
      gender %in% c("Female", "female", "F" ) ~ "Feminin",
      TRUE ~ gender
      
    )
  )
unique(health$gender)

unique(health$consultation_type)
unique(health$diagnosis)

# Vérifier insurance_status
unique(health$insurance_status)

# Sauvegarder une version intermédiaire2
write_csv(
  health,
  "outputs/health_step2_clean.csv"
)

# Corriger lesdates et traiter les valeurs aberrantes (outliers)
class(health$consultation_date)


# Convertir formats mixtes 
health <- health %>% 
  mutate(
    consultation_date = parse_date_time(
      consultation_date,
      orders = c("ymd", "dmy")
    )
  )

health$consultation_date <- as.Date(
  health$consultation_date
)

class(health$consultation_date)
# Traiter valeurs aberrantes 

summary(health$patient_age)


# Filter âges valides
health <- health %>% 
  filter(
    patient_age >=0,
    patient_age <=120
  )

summary(health$treatment_cost)

# Filter coût de traitement valides
health <- health %>% 
  filter(
    treatment_cost >=0,
    treatment_cost <=500
  )

summary(health$treatment_cost)


dim(health)


# Visualiser outiliers avec boxplot 

boxplot(health$treatment_cost)


# Créer variables temporelles utile
health <- health %>% 
  mutate( 
    month = month(consultation_date),
    year = year(consultation_date)
    )
dim(health)

View(health)


# Sauvegarder dataset propre 
write_csv(
  health,
  "outputs/health_clean_final.csv"
)

