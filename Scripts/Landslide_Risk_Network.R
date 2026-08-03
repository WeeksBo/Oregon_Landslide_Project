fires <- read.csv("C:/OSU/CS_458/Data/Raw_Data/ODF_Fire.csv")

# Count fires per year
fire_counts <- fires %>%
  group_by(FireYear) %>%
  summarise(
    Fire_Count = n(),
    Avg_Acres = mean(FinalFireSizeAcres, na.rm = TRUE)
  ) %>%
  rename(YEAR = FireYear)

fire_counts

# Join fire counts with landslide counts by year
landslide_counts <- slido %>%
  st_drop_geometry() %>%
  filter(!is.na(YEAR), YEAR > 1959, YEAR != 1996) %>%
  count(YEAR)

fire_landslide <- fire_counts %>%
  inner_join(landslide_counts, by = "YEAR") %>%
  rename(Landslide_Count = n)

# Check correlation
cor(fire_landslide$Fire_Count, fire_landslide$Landslide_Count, use = "complete.obs")

fire_landslide


# Lag fire counts by 1 year
fire_lagged <- fire_landslide %>%
  mutate(YEAR = YEAR + 1) %>%
  select(YEAR, Fire_Count_Lagged = Fire_Count)

fire_landslide_lagged <- landslide_counts %>%
  inner_join(fire_lagged, by = "YEAR") %>%
  rename(Landslide_Count = n)

cor(fire_landslide_lagged$Fire_Count_Lagged, 
    fire_landslide_lagged$Landslide_Count, 
    use = "complete.obs")


# 1. Cause weights - already have these (real data)
cause_weights <- cause_clean %>%
  count(CAUSE_CLEAN) %>%
  mutate(Weight = n / sum(n)) %>%
  select(CAUSE_CLEAN, Weight)

# 2. Slope certainty - what % of landslides have slope data
slope_certainty <- sum(!is.na(slido$SLOPE) & slido$SLOPE > 0) / nrow(slido)

# 3. Precipitation correlation - already calculated
precip_cor <- cor(fire_landslide$Fire_Count, 
                  fire_landslide$Landslide_Count, 
                  use = "complete.obs")

# 4. Wildfire correlation - already calculated (-0.22, use absolute value)
fire_cor <- abs(-0.2225936)

# 5. Ground material certainty - what % of landslides have material data
material_certainty <- sum(!is.na(slido$TYPE_MTRL) & slido$TYPE_MTRL != "") / nrow(slido)

# Check all values
cat("Cause weights:\n"); print(cause_weights)
cat("\nSlope certainty:", slope_certainty)
cat("\nPrecip correlation:", precip_cor)
cat("\nWildfire correlation:", fire_cor)
cat("\nMaterial certainty:", material_certainty)


# Road construction effect on slope
slido %>%
  st_drop_geometry() %>%
  filter(str_detect(tolower(CONTR_FACT), "road"), !is.na(SLOPE), SLOPE > 0) %>%
  summarise(avg_slope = mean(SLOPE), n = n())

# Clear cut effect on material
slido %>%
  st_drop_geometry() %>%
  filter(str_detect(tolower(CONTR_FACT), "clear cut|clearcut"), !is.na(TYPE_MTRL)) %>%
  count(TYPE_MTRL)



nodes_causal <- data.frame(
  Id = c("Slope", "Ground Material", "Road Related", 
         "Clear Cut", "Natural Causes", "Pre-existing Slide",
         "Precipitation", "Wildfire", "Landslide Risk"),
  Label = c("Slope", "Ground Material", "Road Related",
            "Clear Cut", "Natural Causes", "Pre-existing Slide",
            "Precipitation", "Wildfire", "Landslide Risk"),
  Type = c("Physical", "Physical", "Human", 
           "Human", "Natural", "Natural",
           "Environmental", "Environmental", "Risk"),
  Certainty = c(0.19, 0.25, 1.0, 1.0, 1.0, 1.0, 0.36, 0.22, 1.0)
)

edges_causal <- data.frame(
  Source = c("Road Related", "Clear Cut", "Natural Causes", 
             "Pre-existing Slide", "Slope", "Ground Material", 
             "Precipitation", "Wildfire"),
  Target = rep("Landslide Risk", 8),
  Weight = c(1.0, 1.0, 1.0, 1.0, 0.19, 0.25, 0.36, 0.22)
)

write.csv(edges_causal, "C:/OSU/CS_458/Data/causal_edges.csv", row.names = FALSE)

write.csv(nodes_causal, "C:/OSU/CS_458/Data/causal_nodes.csv", row.names = FALSE)
write.csv(edges_causal, "C:/OSU/CS_458/Data/causal_edges.csv", row.names = FALSE)
