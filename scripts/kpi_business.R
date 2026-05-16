# Notre mission business
# 
# Le ministère de la santé veut comprendre :
#   
#   quelles régions ont le plus de consultations ?
#   quelles maladies dominent ?
#   quels coûts sont les plus élevés ?
#   quelle couverture assurance ?
#   quels centres sont les plus actifs ?
#   quelle évolution mensuelle ?
#   
# Nous allons répondre.

health <- health %>%
  mutate(
    age_group = case_when(
      patient_age < 18 ~ "Child",
      patient_age >= 18 & patient_age < 60 ~ "Adult",
      patient_age >= 60 ~ "Senior"
    )
  )

table(health$age_group)

health <- health %>%
  mutate(
    treatment_cost_usd = treatment_cost / 600
  )


nrow(health)


# Consultation par région 

consultations_region <- health %>%
  group_by(region) %>%
  summarise(
    total_consultations = n()
  ) %>%
  arrange(desc(total_consultations))

View(consultations_region)


# KPI 3 : top diagnostics
top_diagnosis <- health %>%
  group_by(diagnosis) %>%
  summarise(
    total_cases = n()
  ) %>%
  arrange(desc(total_cases))

top_diagnosis


health %>%
  summarise(
    avg_cost = mean(treatment_cost)
  )

health %>%
  summarise(
    median_cost = median(treatment_cost)
  )

cost_by_diagnosis <- health %>%
  group_by(diagnosis) %>%
  summarise(
    avg_cost = mean(treatment_cost)
  ) %>%
  arrange(desc(avg_cost))

health %>%
  group_by(insurance_status) %>%
  summarise(
    total = n()
  )

health %>%
  group_by(facility_name) %>%
  summarise(
    total_consultations = n()
  ) %>%
  arrange(desc(total_consultations))


monthly_trend <- health %>%
  group_by(month) %>%
  summarise(
    consultations = n()
  )

write_csv(
  consultations_region,
  "outputs/kpi_region.csv"
)
