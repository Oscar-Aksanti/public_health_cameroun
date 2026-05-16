# Visualisations 
# 1. Bar chart 

# Consultation par région 

consultation_par_region <- ggplot(consultations_region,
       aes(x = reorder(region, total_consultations),
           y = total_consultations)
       )+
  geom_col( fill="steelblue")+
  coord_flip()

ggsave("outputs/consultation_par_region.pdf")

# 2. Histogramme
# Répartition âge patients

ggplot(health,
       aes (x = patient_age)
       )+
  geom_histogram(bins = 15)


# 3. Boxplot

ggplot(health,
       aes(
         x = diagnosis,
         y = treatment_cost
       ))+
  geom_boxplot()

ggsave("outputs/coup_par_maladie.png")

# 4. Line chart
ggplot(
  monthly_trend,
  aes(x = month, 
      y = consultations)
) + 
  geom_line()+
  geom_point()

# 5. Scatterplot 
ggplot(health,
       aes(x = patient_age,
           y = treatment_cost))+
  geom_point()

# 6. Bar chart groupé
ggplot(health,
       aes(x = region,
           fill = gender))+
  geom_bar(position = "dodge")


# 7. Bar chart empilé 
ggplot(health,
       aes(x = region,
           fill = gender))+
  geom_bar()


# 8. Pie chart

health %>% 
  count(insurance_status) %>% 
  ggplot(
    aes(x = "",
        y = n,
        fill = insurance_status)
  ) +
  geom_col()+
  coord_polar("y")+
  labs(
    title = "Répartition assurance"
  )+ 
  theme_dark()






