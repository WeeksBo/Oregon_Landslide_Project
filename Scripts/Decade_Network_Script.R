temporal_data <- slido %>%
  st_drop_geometry() %>%
  filter(!is.na(YEAR), YEAR > 1940, YEAR != 1996) %>%
  mutate(Decade = floor(YEAR / 10) * 10) %>%
  group_by(Decade) %>%
  summarise(
    Id = as.character(first(Decade)),
    Label = paste0(first(Decade), "s"),
    Count = n()
  ) %>%
  inner_join(
    precip_clean %>%
      mutate(Decade = floor(YEAR / 10) * 10) %>%
      group_by(Decade) %>%
      summarise(Avg_Precip = mean(PRCP, na.rm = TRUE)),
    by = "Decade"
  )

temporal_data


# Nodes
write.csv(temporal_data, "C:/OSU/CS_458/Data/temporal_nodes.csv", row.names = FALSE)

# Edges - connect decades with similar precipitation (within 5 inches)
temporal_edges <- temporal_data %>%
  select(Id, Avg_Precip) %>%
  cross_join(temporal_data %>% select(Id, Avg_Precip)) %>%
  filter(Id.x < Id.y,
         abs(Avg_Precip.x - Avg_Precip.y) <= 5) %>%
  mutate(Weight = 1 / (abs(Avg_Precip.x - Avg_Precip.y) + 1)) %>%
  select(Source = Id.x, Target = Id.y, Weight)

write.csv(temporal_edges, "C:/OSU/CS_458/Data/temporal_edges.csv", row.names = FALSE)

temporal_edges
