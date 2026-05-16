# Les statistiques 

# 1. La moyenne 

mean(health$patient_age)

# 2. Médiane 
median(health$patient_age)

# 3. Maximum/Minimum
max(health$patient_age)
min(health$patient_age)

# 4. Résumé 
summary(health$patient_age)

# 5. Quantiles : comprendre la distribution
quantile(health$patient_age)


# 6. Variance : mesure dispersion
var(health$patient_age)

# 7. Ecart-type (standard deviation - sd)
sd(health$patient_age)


# 8. Comptages catégories
table(health$gender)

# 9. Proportions 
prop.table(table(health$gender))

# 10. Statistiques groupées 

health %>% 
  group_by(region) %>% 
  summarise(
    avg_cost = mean(treatment_cost),
    median_cost = median(treatment_cost), 
    max_cost = max(treatment_cost),
    min_cost = min(treatment_cost)
  )

# Correlation
cor(
  health$patient_age,
  health$treatment_cost
)

skimr::skim(health)


stats_summary <- health %>% 
  summarise(
    avg_age = mean(patient_age),
    median_age = median(patient_age),
    avg_cost = mean(treatment_cost)
  )

write_csv(stats_summary,
          "outputs/stats_summary.csv")
