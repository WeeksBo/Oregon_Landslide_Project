graph1_data <- slido %>%
  st_drop_geometry() %>%
  filter(!is.na(SLOPE), SLOPE > 0) %>%
  mutate(
    SLOPE_CAT = case_when(
      SLOPE <= 10 ~ "Very Gentle (0-10)",
      SLOPE <= 20 ~ "Gentle (11-20)",
      SLOPE <= 30 ~ "Moderate (21-30)",
      SLOPE <= 40 ~ "Steep (31-40)",
      SLOPE <= 50 ~ "Very Steep (41-50)",
      SLOPE <= 60 ~ "Severe (51-60)",
      SLOPE <= 75 ~ "Extreme (61-75)",
      TRUE ~ "Near Vertical (76-90)"
    ),
    MATERIAL_CLEAN = case_when(
      str_detect(tolower(TYPE_MTRL), "rock") ~ "Rock",
      str_detect(tolower(TYPE_MTRL), "debris") ~ "Debris",
      str_detect(tolower(TYPE_MTRL), "earth|soil|loess") ~ "Earth/Soil",
      str_detect(tolower(TYPE_MTRL), "fill") ~ "Fill",
      str_detect(tolower(TYPE_MTRL), "basalt|volcanic|sediment|siletzia|crab|crb|ancient|columbia|coast|early") ~ "Geological",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(MATERIAL_CLEAN))

# Nodes - one per slope category
nodes_g1 <- graph1_data %>%
  group_by(SLOPE_CAT) %>%
  summarise(
    Id = first(SLOPE_CAT),
    Label = first(SLOPE_CAT),
    Count = n()
  )

write.csv(nodes_g1, "C:/OSU/CS_458/Data/graph1_nodes.csv", row.names = FALSE)

edges_g1 <- graph1_data %>%
  select(SLOPE_CAT, MATERIAL_CLEAN) %>%
  inner_join(
    graph1_data %>% select(SLOPE_CAT, MATERIAL_CLEAN),
    by = "MATERIAL_CLEAN",
    relationship = "many-to-many"
  ) %>%
  filter(SLOPE_CAT.x < SLOPE_CAT.y) %>%
  group_by(Source = SLOPE_CAT.x, Target = SLOPE_CAT.y, Material = MATERIAL_CLEAN) %>%
  summarise(Weight = n(), .groups = "drop")

write.csv(edges_g1, "C:/OSU/CS_458/Data/graph1_edges.csv", row.names = FALSE)

nrow(edges_g1)
nrow(nodes_g1)
nrow(edges_g1)


