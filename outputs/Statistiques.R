mean(health$patient_age)

median(health$patient_age)

min(health$treatment_cost)
max(health$treatment_cost)

summary(health$treatment_cost)

quantile(health$treatment_cost)

var(health$treatment_cost)

sd(health$treatment_cost)

table(health$diagnosis)

prop.table(table(health$gender))

health %>%
  group_by(region) %>%
  summarise(
    avg_cost = mean(treatment_cost),
    median_cost = median(treatment_cost)
  )

health %>%
  group_by(diagnosis) %>%
  summarise(
    avg_age = mean(patient_age),
    avg_cost = mean(treatment_cost)
  )

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


write_csv(
  stats_summary,
  "outputs/stats_summary.csv"
)
