# Notre mission business 
# Le ministère de la santé veut comprendre : 
#   - quelles régions ont le plus de consultations?
#   - quelles maladies dominent?
#   - quels coût sont les plus élevés?
#   - qelle couverture assurance?
#   - quels centres sont les plus actifs?
#   - quelle évolution mensuelle?
#   
# Nous allons répondre 

# 1. Groupe d'âge

health <- health %>% 
  mutate(
    age_group = case_when(
      patient_age < 18 ~ "Enfant",
      patient_age >= 18 & patient_age <  60 ~ "Adulte",
      patient_age >=60 ~ "Senior"
    )
  )


# Vérifier varible créée
table(health$age_group)


# Partie 2 - KPI 1 : nombre total consultations
nrow(health)


View(health)

# Partie 3 - KPI 2 : Consultations par région
consultations_region <- health %>% 
  group_by(region) %>% 
  summarise(
    total_consultations = n()
  ) %>% 
  arrange(desc(total_consultations))

# Partie 4 - KPI 3 : Top Diagnostics 

top_diagnosis <- health %>% 
  group_by(diagnosis) %>% 
  summarise(
    total_cases = n()
  ) %>% 
  arrange(desc(total_cases))


# Partie 5 - KPI 4 : Coût moyen traitement 
health %>% 
  summarise(
    avg_cost = mean(treatment_cost)
  )


health %>% 
  summarise(
    median_cost = median(treatment_cost)
  )


# Partie 6 - KPI 5 : Coût moyen par maladie
cost_by_diagnosis <- health %>% 
  group_by(diagnosis) %>% 
  summarise(
    avg_cost = mean(treatment_cost)
  ) %>% 
  arrange(desc(avg_cost))


# Partie 7 - KPI 6 : assurance
health %>% 
  group_by(insurance_status) %>% 
  summarise(
    total = n()
  )

# Partie 8 - KPI : centres les plus actifs
health %>% 
  group_by(facility_name) %>% 
  summarise(
    total_consultations = n()
  ) %>% 
  arrange (desc(total_consultations))


# Partie 9 - KPI 8: évolution mensuelle
monthly_trend <- health %>% 
  group_by(month) %>% 
  summarise(
    consultations =n()
  )


# Partie 10 : Export KPI tables

write_csv(
  consultations_region,
  "outputs/kpi_regions.csv"
)
