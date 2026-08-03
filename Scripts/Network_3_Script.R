cause_counts <- slido %>%
  st_drop_geometry() %>%
  mutate(CAUSE_CLEAN = case_when(
    str_detect(tolower(CONTR_FACT), "road|cut slope|fill slope|road fill|steep road") ~ "Road Construction",
    str_detect(tolower(CONTR_FACT), "clear cut|clearcut|reforested clearcut") ~ "Clear Cut",
    str_detect(tolower(CONTR_FACT), "natural") ~ "Natural Causes",
    str_detect(tolower(CONTR_FACT), "human") ~ "Human",
    str_detect(tolower(CONTR_FACT), "pre-existing|existing|exisitng") ~ "Pre-existing Slide",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(CAUSE_CLEAN)) %>%
  count(CAUSE_CLEAN) %>%
  mutate(Weight = n / sum(n))  # normalize to 0-1

cause_counts

nodes_n2 <- data.frame(
  Id = c("Slope", "Ground Material", "Road Construction", 
         "Clear Cut", "Natural Causes", "Pre-existing Slide",
         "Precipitation", "Landslide Risk"),
  Label = c("Slope", "Ground Material", "Road Construction",
            "Clear Cut", "Natural Causes", "Pre-existing Slide",
            "Precipitation", "Landslide Risk"),
  Type = c("Physical", "Physical", "Human", 
           "Human", "Natural", "Natural",
           "Environmental", "Risk")
)

edges_n2 <- data.frame(
  Source = c("Road Construction", "Clear Cut", "Natural Causes", 
             "Pre-existing Slide", "Human", "Slope", 
             "Ground Material", "Precipitation",
             "Road Construction", "Clear Cut", "Precipitation"),
  Target = c("Landslide Risk", "Landslide Risk", "Landslide Risk",
             "Landslide Risk", "Landslide Risk", "Landslide Risk",
             "Landslide Risk", "Landslide Risk",
             "Slope", "Ground Material", "Ground Material"),
  Weight = c(0.566, 0.220, 0.178, 
             0.014, 0.023, 0.8,
             0.7, 0.75,
             0.6, 0.65, 0.7)
)

write.csv(nodes_n2, "C:/OSU/CS_458/Data/network2_nodes.csv", row.names = FALSE)
write.csv(edges_n2, "C:/OSU/CS_458/Data/network2_edges.csv", row.names = FALSE)