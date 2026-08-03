yearly_counts <- slido_time %>%
  st_drop_geometry() %>%
  group_by(YEAR) %>%
  summarise(events = n())

yearly_no1996 <- yearly_counts %>%
  filter(YEAR != 1996)


ggplot() +
  geom_line(data = yearly_counts %>% filter(YEAR != 1996),
            aes(x = YEAR, y = events, color = "Landslide Events"), 
            linewidth = 1) +
  geom_line(data = precip_clean,
            aes(x = YEAR, y = PRCP, color = "Precipitation"),
            linewidth = 1, alpha = 0.7) +
  scale_color_manual(
    name = "Legend",
    values = c("Landslide Events" = "darkgreen",
               "Precipitation" = "steelblue")
  ) +
  scale_x_continuous(breaks = seq(1950, 2023, by = 10)) +
  labs(
    title = "Oregon Landslide Events and Precipitation Over Time (1950-2023)",
    subtitle = "1996 excluded",
    x = "Year",
    y = "Landslide Events / Precipitation (inches)"
  ) +
  theme_minimal() +
  theme(legend.position = c(0.2, 0.8))


ggplot() +
  geom_line(data = yearly_counts %>% filter(),
            aes(x = YEAR, y = events, color = "Landslide Events"), 
            linewidth = 1) +
  geom_line(data = precip_clean,
            aes(x = YEAR, y = PRCP, color = "Precipitation"),
            linewidth = 1, alpha = 0.7) +
  scale_color_manual(
    name = "Legend",
    values = c("Landslide Events" = "darkgreen",
               "Precipitation" = "steelblue")
  ) +
  scale_x_continuous(breaks = seq(1950, 2023, by = 10)) +
  labs(
    title = "Oregon Landslide Events and Precipitation Over Time (1950-2023)",
    subtitle = "1996 included",
    x = "Year",
    y = "Landslide Events / Precipitation (inches)"
  ) +
  theme_minimal() +
  theme(legend.position = c(0.2, 0.8))


