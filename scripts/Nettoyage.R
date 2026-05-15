sum(duplicated(health))


health <- distinct(health)


dim(health)


mean(health$treatment_cost)


colSums(is.na(health))


health <- health %>%
  drop_na(diagnosis, region)


View(health)


colSums(is.na(health))

colSums(is.na(health) | health == "")

health <- health %>%
  filter(
    diagnosis != "", 
    region != ""
  ) %>%
  drop_na(diagnosis, region)


median(health$treatment_cost, na.rm = TRUE)


health <- health %>%
  mutate(
    treatment_cost = ifelse(
      is.na(treatment_cost),
      median(treatment_cost, na.rm = TRUE),
      treatment_cost
    )
  )


health <- health %>%
  mutate(
    gender = replace_na(gender, "Unknown")
  )


health <- health %>%
  mutate(
    insurance_status = replace_na(
      insurance_status,
      "Unknown"
    )
  )

colSums(is.na(health))

glimpse(health)


write_csv(
  health,
  "outputs/health_step1_clean.csv"
)


table(health$region)

unique(health$region)


health <- health %>%
  mutate(
    region = case_when(
      region %in% c("centre", "CENTER", "Ctr") ~ "Centre",
      region %in% c("litoral", "LITTORAL") ~ "Littoral",
      region %in% c("sud ouest", "SW") ~ "Sud-Ouest",
      TRUE ~ region
    )
  )


table(health$region)


unique(health$gender)


health <- health %>%
  mutate(
    gender = case_when(
      gender %in% c("male", "M", "Masculin") ~ "Male",
      gender %in% c("female", "F", "Feminin") ~ "Female",
      gender %in% c("", " ") ~ "Unknown",
      TRUE ~ gender
    )
  )

unique(health$insurance_status)

health <- health %>%
  mutate(
    insurance_status = case_when(
      insurance_status %in% c("insured", "INSURED") ~ "Insured",
      insurance_status %in% c("uninsured", "UNINSURED", "") ~ "Uninsured",
      TRUE ~ insurance_status
    )
  )

unique(health$consultation_type)
unique(health$diagnosis)

table(health$gender)
table(health$region)

write_csv(
  health,
  "outputs/health_step2_categories_clean.csv"
)


install.packages("lubridate")
library(lubridate)

health <- health %>%
  mutate(
    consultation_date = parse_date_time(
      consultation_date,
      orders = c("ymd", "dmy")
    )
  )

library(dplyr)
library(lubridate)

health <- health %>%
  mutate(
    consultation_date = parse_date_time(
      consultation_date,
      orders = c("ymd", "dmy")
    )
  )

class(health$consultation_date)


health$consultation_date <- as.Date(
  health$consultation_date
)

class(health$consultation_date)


head(health$consultation_date)

head(health$consultation_date, 100)

health <- health %>%
  filter(
    patient_age >= 0,
    patient_age <= 120
  )


summary(health$patient_age)


summary(health$treatment_cost)

health <- health %>%
  filter(
    treatment_cost >= 0,
    treatment_cost <= 500
  )

boxplot(health$treatment_cost)


health <- health %>%
  mutate(
    month = month(consultation_date),
    year = year(consultation_date)
  )


write_csv(
  health,
  "outputs/health_clean_final.csv"
)


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
