ggplot(
  consultations_region,
  aes(
    x = region,
    y = total_consultations
  )
) +
  geom_col()



ggplot(
  consultations_region,
  aes(
    x = reorder(region, total_consultations),
    y = total_consultations
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip()



ggplot(
  health,
  aes(x = patient_age)
) +
  geom_histogram()



ggplot(
  health,
  aes(x = patient_age)
) +
  geom_histogram(bins = 20)


ggplot(
  health,
  aes(
    x = diagnosis,
    y = treatment_cost
  )
) +
  geom_boxplot()


ggplot(
  monthly_trend,
  aes(
    x = month,
    y = consultations
  )
) +
  geom_line()

ggplot(
  monthly_trend,
  aes(
    x = month,
    y = consultations
  )
) +
  geom_line() +
  geom_point()

ggplot(
  health,
  aes(
    x = patient_age,
    y = treatment_cost
  )
) +
  geom_point()


ggplot(
  health,
  aes(
    x = region,
    fill = gender
  )
) +
  geom_bar(position = "dodge")


ggplot(
  health,
  aes(
    x = region,
    fill = gender
  )
) +
  geom_bar()

ggplot(
  health,
  aes(
    x = diagnosis,
    y = treatment_cost
  )
) +
  geom_boxplot() +
  facet_wrap(~ region)


health %>%
  count(insurance_status) %>%
  ggplot(
    aes(
      x = "",
      y = n,
      fill = insurance_status
    )
  ) +
  geom_col() +
  coord_polar("y")

ggplot(
  consultations_region,
  aes(
    x = region,
    y = total_consultations
  )
) +
  geom_col() +
  labs(
    title = "Consultations par région",
    x = "Région",
    y = "Nombre"
  )


ggsave(
  "outputs/consultations_region.png"
)


