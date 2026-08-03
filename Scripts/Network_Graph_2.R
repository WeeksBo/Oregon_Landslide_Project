slido %>%
  st_drop_geometry() %>%
  mutate(CAUSE_CLEAN = case_when(
    str_detect(tolower(CONTR_FACT), "road|cut slope|fill slope|road fill|steep road") ~ "Road Related",
    str_detect(tolower(CONTR_FACT), "clear cut|clearcut|reforested clearcut") ~ "Clear Cut",
    str_detect(tolower(CONTR_FACT), "natural") ~ "Natural",
    str_detect(tolower(CONTR_FACT), "human") ~ "Human",
    str_detect(tolower(CONTR_FACT), "pre-existing|existing|exisitng") ~ "Pre-existing Slide",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(CAUSE_CLEAN), !is.na(SLOPE), SLOPE > 0) %>%
  group_by(CAUSE_CLEAN) %>%
  summarise(
    Count = n(),
    Avg_Slope = mean(SLOPE),
    SD_Slope = sd(SLOPE)
  )

cause_slope <- data.frame(
  Id = c("Clear Cut", "Human", "Natural", "Pre-existing Slide", "Road Related"),
  Label = c("Clear Cut", "Human", "Natural", "Pre-existing Slide", "Road Related"),
  Count = c(648, 65, 523, 23, 696),
  Avg_Slope = c(26.5, 37.1, 26.0, 47.7, 37.1)
)

write.csv(cause_slope, "C:/OSU/CS_458/Data/network1_nodes.csv", row.names = FALSE)

# Connect causes within 5 degrees of each other
edges_n1 <- cause_slope %>%
  cross_join(cause_slope) %>%
  filter(Id.x < Id.y,
         abs(Avg_Slope.x - Avg_Slope.y) <= 15) %>%
  mutate(Weight = 1 / (abs(Avg_Slope.x - Avg_Slope.y) + 1)) %>%
  select(Source = Id.x, Target = Id.y, Weight)

write.csv(edges_n1, "C:/OSU/CS_458/Data/network1_edges.csv", row.names = FALSE)

edges_n1
