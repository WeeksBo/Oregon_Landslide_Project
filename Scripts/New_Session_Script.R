# Run when opening session
library(sf)
library(dplyr)
library(ggplot2)
library(stringr)
# Load cleaned data
slido_time <- readRDS("C:/OSU/CS_458/Data/Clean_Data/slido_time.rds")
slido_map <- readRDS("C:/OSU/CS_458/Data/Clean_Data/slido_map.rds")
slido <- st_read(
  "C:/OSU/CS_458/Data/Raw_Data/SLIDO_Release_4p5_wMetadata.gdb",
  layer = "Historic_Landslide_Points"
)

yearly_counts <- slido_time %>%
  st_drop_geometry() %>%
  group_by(YEAR) %>%
  summarise(events = n())

yearly_no1996 <- yearly_counts %>%
  filter(YEAR != 1996)

precip <- read.csv("C:/OSU/CS_458/Data/Raw_Data/percipitation.csv")

precip_clean <- precip %>%
  group_by(DATE) %>%
  summarise(
    PRCP = mean(PRCP, na.rm = TRUE),
    TAVG = mean(TAVG, na.rm = TRUE)
  ) %>%
  rename(YEAR = DATE)

#roads <- st_read(
#  "Data/Raw_Data/Transportation_Road.gdb",
#  layer = "Transportation_Statewide_Road"
#)

precip_2025 <- read.csv("C:/OSU/CS_458/Data/Raw_Data/Oregon_percipitation_data.csv")
precip_2025_clean <- precip_2025 %>%
  filter(!is.na(PRCP))

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

slope_data <- slido %>%
  st_drop_geometry() %>%
  filter(!is.na(SLOPE))


fires <- read.csv("C:/OSU/CS_458/Data/Raw_Data/ODF_Fire.csv")

# Clean and filter to Oregon fires with coordinates
fires_clean <- fires %>%
  filter(!is.na(Latitude), !is.na(Longitude), 
         !is.na(FinalFireSizeAcres), FinalFireSizeAcres > 0) %>%
  select(FireYear, Latitude, Longitude, FinalFireSizeAcres, County)

# Filter to larger fires only (over 100 acres)
fires_large <- fires_clean %>%
  filter(FinalFireSizeAcres >= 100)

# Remove outlier fires and bad coordinates
fires_large <- fires_clean %>%
  filter(FinalFireSizeAcres >= 100,
         FinalFireSizeAcres < 100000,
         Latitude > 41, Latitude < 47,
         Longitude > -125, Longitude < -116)


library(sf)
library(dplyr)


slido_export <- slido_map %>%
  st_transform(crs = 4326)

coords <- st_coordinates(slido_export) %>% as.data.frame()
attrs <- st_drop_geometry(slido_export)

slido_final <- cbind(coords, attrs) %>%
  select(X, Y, YEAR, CONTR_FACT, DATA_SOURC, LOC_METHOD, SLOPE)

slido_final$DataQuality <- ifelse(
  !is.na(slido_final$CONTR_FACT) & slido_final$CONTR_FACT != "" &
    !is.na(slido_final$YEAR) &
    (!is.na(slido_final$DATA_SOURC) & slido_final$DATA_SOURC != "" |
       !is.na(slido_final$LOC_METHOD) & slido_final$LOC_METHOD != ""),
  "Complete", "Incomplete"
)

write.csv(slido_final, "slido_complete.csv", row.names = FALSE)







library(sf)
library(dplyr)
library(ggplot2)
library(tigris)
library(tidyr)
library(viridis)

# Get Oregon county boundaries
oregon_counties <- counties(state = "OR", cb = TRUE, year = 2020) %>%
  st_transform(crs = 4326)

# Load landslide data
slido_map <- readRDS("C:/OSU/CS_458/Data/Clean_Data/slido_map.rds") %>%
  st_transform(crs = 4326)

# Count landslides per county using spatial join
landslides_per_county <- st_join(slido_map, oregon_counties, join = st_within) %>%
  st_drop_geometry() %>%
  group_by(NAME) %>%
  summarise(count = n()) %>%
  filter(!is.na(NAME))

# Join counts back to county boundaries
county_map <- oregon_counties %>%
  left_join(landslides_per_county, by = "NAME") %>%
  mutate(count = replace_na(count, 0))

# Plot choropleth
ggplot(county_map) +
  geom_sf(aes(fill = count), color = "black", linewidth = 0.3) +
  scale_fill_gradient(
    low = "antiquewhite",
    high = "red4",
    name = "Landslide\nCount",
    trans = "sqrt",
    breaks = c(100, 500, 1000, 2000, 3000),
    labels = scales::comma
  ) +
  labs(
    title = "Oregon Landslide Events by County (1928–2023)",
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"),
    plot.caption  = element_text(hjust = 0.5, size = 8,  color = "gray60"),
    legend.position = "right"
  )
