


# Count landslides per year
yearly_counts <- slido_time %>%
  st_drop_geometry() %>%
  filter(YEAR != 1996) %>%
  group_by(YEAR) %>%
  summarise(landslides = n())

# Join with precipitation
cor_data <- yearly_counts %>%
  inner_join(precip_clean, by = "YEAR") %>%
  rename(precipitation = PRCP, temperature = TAVG)

# Build correlation matrix
cor_matrix <- cor_data %>%
  select(landslides, precipitation, temperature) %>%
  cor(use = "complete.obs")

# Plot
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 1,
         title = "Correlation Between Landslides, Precipitation, and Temperature",
         mar = c(0,0,2,0))

