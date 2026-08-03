
# Create cause_clean object
cause_clean <- slido %>%
  st_drop_geometry() %>%
  mutate(CAUSE_CLEAN = case_when(
    str_detect(tolower(CONTR_FACT), "road|cut slope|fill slope|road fill|steep road") ~ "Road Related",
    str_detect(tolower(CONTR_FACT), "clear cut|clearcut|reforested clearcut") ~ "Clear Cut",
    str_detect(tolower(CONTR_FACT), "natural") ~ "Natural",
    str_detect(tolower(CONTR_FACT), "human") ~ "Human",
    str_detect(tolower(CONTR_FACT), "pre-existing|existing|exisitng") ~ "Pre-existing Slide",
    TRUE ~ "Other/Unknown"
  )) %>%
  filter(CAUSE_CLEAN != "Other/Unknown")

write.csv(cause_clean, "C:/OSU/CS_458/Data/landslides_for_gephi.csv", row.names = FALSE)


# Then plot from it
cause_clean %>%
  group_by(CAUSE_CLEAN) %>%
  summarise(events = n()) %>%
  arrange(desc(events)) %>%
  ggplot(aes(x = reorder(CAUSE_CLEAN, events), y = events)) +
  geom_col(fill = "darkorange4") +
  scale_y_continuous(breaks = seq(0, 1800, by = 100)) +
  coord_flip() +
  labs(
    title = "Top 5 Contributing Factors to Oregon Landslides",
    x = "Contributing Factor",
    y = "Number of Events"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
