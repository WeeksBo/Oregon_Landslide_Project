# ---- GRAPH 1: Slope categories as nodes (more categories) ----

slope_categorized <- slope_data %>%
  mutate(SLOPE_CAT = case_when(
    SLOPE <= 10 ~ "Very Gentle (0-10)",
    SLOPE <= 20 ~ "Gentle (11-20)",
    SLOPE <= 30 ~ "Moderate (21-30)",
    SLOPE <= 40 ~ "Steep (31-40)",
    SLOPE <= 50 ~ "Very Steep (41-50)",
    SLOPE <= 60 ~ "Severe (51-60)",
    SLOPE <= 75 ~ "Extreme (61-75)",
    TRUE ~ "Near Vertical (76-90)"
  ))

nodes_slope <- slope_categorized %>%
  group_by(SLOPE_CAT) %>%
  summarise(
    Id = first(SLOPE_CAT),
    Label = first(SLOPE_CAT),
    Count = n(),
    Avg_Slope = mean(SLOPE)
  )

write.csv(nodes_slope, "C:/OSU/CS_458/Data/slope_nodes.csv", row.names = FALSE)

edges_slope <- slope_categorized %>%
  filter(!is.na(TYPE_MOVE), TYPE_MOVE != "") %>%
  select(SLOPE_CAT, TYPE_MOVE) %>%
  inner_join(
    slope_categorized %>%
      filter(!is.na(TYPE_MOVE), TYPE_MOVE != "") %>%
      select(SLOPE_CAT, TYPE_MOVE),
    by = "TYPE_MOVE"
  ) %>%
  filter(SLOPE_CAT.x < SLOPE_CAT.y) %>%
  group_by(Source = SLOPE_CAT.x, Target = SLOPE_CAT.y) %>%
  summarise(Weight = n(), .groups = "drop")

write.csv(edges_slope, "C:/OSU/CS_458/Data/slope_edges.csv", row.names = FALSE)

nrow(nodes_slope)
nrow(edges_slope)


# ---- GRAPH 2: Landslides as nodes, connect if same slope category AND same cause ----

slope_cause <- slope_data %>%
  filter(!is.na(SLOPE), SLOPE > 0) %>%
  mutate(SLOPE_CAT = case_when(
    SLOPE <= 10 ~ "Very Gentle (0-10)",
    SLOPE <= 20 ~ "Gentle (11-20)",
    SLOPE <= 30 ~ "Moderate (21-30)",
    SLOPE <= 40 ~ "Steep (31-40)",
    SLOPE <= 50 ~ "Very Steep (41-50)",
    SLOPE <= 60 ~ "Severe (51-60)",
    SLOPE <= 75 ~ "Extreme (61-75)",
    TRUE ~ "Near Vertical (76-90)"
  )) %>%
  mutate(CAUSE_CLEAN = case_when(
    str_detect(tolower(CONTR_FACT), "road|cut slope|fill slope|road fill|steep road") ~ "Road Related",
    str_detect(tolower(CONTR_FACT), "clear cut|clearcut|reforested clearcut") ~ "Clear Cut",
    str_detect(tolower(CONTR_FACT), "natural") ~ "Natural",
    str_detect(tolower(CONTR_FACT), "human") ~ "Human",
    str_detect(tolower(CONTR_FACT), "pre-existing|existing|exisitng") ~ "Pre-existing Slide",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(CAUSE_CLEAN))

# Sample to keep edges manageable
set.seed(42)
slope_cause_sample <- slope_cause %>%
  slice_sample(n = 500)

nodes_g2 <- slope_cause_sample %>%
  select(Id = UNIQUE_ID, Label = UNIQUE_ID, SLOPE, SLOPE_CAT, CAUSE_CLEAN, YEAR)

write.csv(nodes_g2, "C:/OSU/CS_458/Data/slope_cause_nodes.csv", row.names = FALSE)

edges_g2 <- slope_cause_sample %>%
  select(UNIQUE_ID, SLOPE_CAT, CAUSE_CLEAN) %>%
  cross_join(slope_cause_sample %>% select(UNIQUE_ID, SLOPE_CAT, CAUSE_CLEAN)) %>%
  filter(UNIQUE_ID.x < UNIQUE_ID.y,
         SLOPE_CAT.x == SLOPE_CAT.y,
         CAUSE_CLEAN.x == CAUSE_CLEAN.y) %>%
  mutate(Weight = 1) %>%
  select(Source = UNIQUE_ID.x, Target = UNIQUE_ID.y, Weight)

write.csv(edges_g2, "C:/OSU/CS_458/Data/slope_cause_edges.csv", row.names = FALSE)

nrow(nodes_g2)
nrow(edges_g2)

slido %>%
  st_drop_geometry() %>%
  mutate(MATERIAL_CLEAN = case_when(
    str_detect(tolower(TYPE_MTRL), "rock") ~ "Rock",
    str_detect(tolower(TYPE_MTRL), "debris") ~ "Debris",
    str_detect(tolower(TYPE_MTRL), "earth|soil|loess") ~ "Earth/Soil",
    str_detect(tolower(TYPE_MTRL), "fill") ~ "Fill",
    str_detect(tolower(TYPE_MTRL), "basalt|volcanic|sediment|siletzia|crab|crb|ancient|columbia|coast|early") ~ "Geological",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(MATERIAL_CLEAN)) %>%
  count(MATERIAL_CLEAN)


