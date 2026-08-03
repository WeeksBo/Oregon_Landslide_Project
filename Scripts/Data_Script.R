# Run when opening session
library(sf)
library(dplyr)
library(ggplot2)
# Load cleaned data
slido_time <- readRDS("C:/OSU/CS_458/Data/Clean_Data/slido_time.rds")
slido_map <- readRDS("C:/OSU/CS_458/Data/Clean_Data/slido_map.rds")

yearly_counts <- slido_time %>%
  st_drop_geometry() %>%
  group_by(YEAR) %>%
  summarise(events = n())

yearly_no1996 <- yearly_counts %>%
  filter(YEAR != 1996)

# Plots line graph with 1996
ggplot(yearly_counts , aes(x = YEAR, y = events)) +
  geom_line(aes(color = "Actual Events"), linewidth = 1) +
  geom_smooth(aes(color = "Overall Trend"), method = "loess", se = FALSE, 
              linetype = "dashed") +
  scale_color_manual(
    name = "Legend",
    values = c("Actual Events" = "steelblue", 
               "Overall Trend" = "darkred")
  ) +
  labs(
    title = "Oregon Landslide Events Over Time (1950-2023)",
    x = "Year",
    y = "Number of Landslide Events"
  ) +
  theme_minimal() + 
  theme(
    legend.position = c(.2,.8))

 # Plots line graph with no 1996
ggplot(yearly_no1996, aes(x = YEAR, y = events)) +
  geom_line(aes(color = "Actual Events"), linewidth = 1) +
  geom_smooth(aes(color = "Overall Trend"), method = "loess", se = FALSE, 
              linetype = "dashed") +
  scale_color_manual(
    name = "Legend",
    values = c("Actual Events" = "steelblue", 
               "Overall Trend" = "darkred")
  ) +
  labs(
    title = "Oregon Landslide Events Over Time (1950-2023)",
    subtitle = "1996 excluded",
    x = "Year",
    y = "Number of Landslide Events"
  ) +
  theme_minimal() + 
  theme(
    legend.position = c(.2,.8))

# Get coordinates from the spatial data
slido_coords <- slido_map %>%
  st_transform(crs = 4326) %>%
  st_coordinates() %>%
  as.data.frame()

# Plotting Map of Oregon
oregon <- map_data("state", region="oregon")
or_base <- ggplot(data=oregon,
                  mapping = aes(x=long, y=lat, group=group)) +
  coord_fixed(1.3) +
  geom_polygon(color="black", fill="gray")
or_base
or_county <- map_data("county", region="oregon")
or_base_c <- or_base +
  geom_polygon(data=or_county, fill=NA, color="white") +
  geom_polygon(color="black", fill=NA)
or_base_c
# Add landslide points on top of Oregon Map
or_base_c +
  geom_point(data = slido_coords, 
             aes(x = X, y = Y, group = NULL),
             color = "red", size = 0.5, alpha = 0.3) +
  labs(
    title = "Oregon Landslide Locations (1928-2023)",
    subtitle = "Red Dots Represent Recorded Landslides",
    x="Longitude",
    y="Latitude"
  )

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

# Then plot from it
cause_clean %>%
  group_by(CAUSE_CLEAN) %>%
  summarise(events = n()) %>%
  arrange(desc(events)) %>%
  ggplot(aes(x = reorder(CAUSE_CLEAN, events), y = events, fill = CAUSE_CLEAN)) +
  geom_col(fill = "darkorange4") +
  coord_flip() +
  labs(
    title = "Top 5 Contributing Factors to Oregon Landslides",
    x = "Contributing Factor",
    y = "Number of Events"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
 


library(viridis)

slido_time %>%
  st_drop_geometry() %>%
#  filter(YEAR != 1996) %>%
  filter(!is.na(TYPE_MOVE)) %>%
  filter(TYPE_MOVE != "") %>%
  mutate(decade = as.factor(floor(YEAR / 10) * 10)) %>%
  mutate(TYPE_CLEAN = case_when(
    str_detect(tolower(TYPE_MOVE), "debris|flow") ~ "Debris Flow",
    str_detect(tolower(TYPE_MOVE), "slide|earthslide") ~ "Slide",
    str_detect(tolower(TYPE_MOVE), "fall|rock") ~ "Rock Fall",
    str_detect(tolower(TYPE_MOVE), "earthflow|efl") ~ "Earthflow",
    TRUE ~ "Other"
  )) %>%
  group_by(TYPE_CLEAN, decade) %>%
  summarise(events = n(), .groups = "drop") %>%
  ggplot(aes(x = decade, y = TYPE_CLEAN, fill = events)) +
  geom_tile(color = "white") +
  scale_fill_viridis(name = "Events") +
  labs(
    title = "Oregon Landslide Types by Decade",
    x = "Decade",
    y = "Landslide Type",
    caption = "Source: DOGAMI SLIDO Database"
  ) +
  theme_minimal()



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

# Check it
head(cor_data)
nrow(cor_data)

library(corrplot)

# Build correlation matrix
cor_matrix <- cor_data %>%
  select(landslides, precipitation, temperature) %>%
  cor(use = "complete.obs")

# Plot correlogram
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 1,
         col = COL2("RdBu"),
         title = "Correlation Between Landslides, Precipitation and Temperature",
         mar = c(0,0,2,0))





library(tigris)

# Get primary and secondary roads for Oregon
oregon_roads <- primary_secondary_roads(state = "OR") %>%
  st_transform(crs = 4326)


ggplot() +
  geom_sf(data = oregon_counties, fill = "grey90", color = "white") +
  geom_point(data = slido_coords,
             aes(x = X, y = Y),
             color = "red", size = 0.8, alpha = 0.3) +
  geom_sf(data = oregon_roads, color = "blue", 
          linewidth = 0.3, alpha = 0.5) +
  labs(
    title = "Oregon Landslide Locations and Roads",
    subtitle = "Red dots = Landslides, Blue Lines = Major Roads"
  ) +
  theme_void()







# Remove rows with no precipitation data
precip_2025_clean <- precip_2025 %>%
  filter(!is.na(PRCP))

ggplot() +
  geom_sf(data = oregon_counties, fill = "grey90", color = "white") +
  geom_point(data = slido_coords,
             aes(x = X, y = Y),
             color = "red", size = 0.7, alpha = 0.3) +
  geom_point(data = precip_2025_clean,
             aes(x = LONGITUDE, y = LATITUDE, color = PRCP),
             size = 6, alpha = 0.3) +
  scale_color_gradient(low = "lightblue", high = "darkblue",
                       name = "2025 Precipitation\n(inches)") +
  geom_sf(data = oregon_roads, color = "chocolate4",
          linewidth = 0.2, alpha = 0.5) +
  labs(
    title = "Predicted Landslide Risk: 2025 Precipitation,\nRoads and Historical Landslides",
    subtitle = "Dark Blue = High Rainfall | Brown = Major Roads | Red = Historical Landslides",
  ) +
  theme_void()







ggplot() +
  geom_line(data = yearly_counts %>% filter(YEAR != 1996),
            aes(x = YEAR, y = events, color = "Landslide Events"), 
            linewidth = 1) +
  geom_smooth(data = yearly_counts %>% filter(YEAR != 1996),
              aes(x = YEAR, y = events, color = "Overall Trend"),
              method = "loess", se = FALSE, linetype = "dashed") +
  geom_line(data = precip_clean,
            aes(x = YEAR, y = PRCP / 3, color = "Precipitation"),
            linewidth = 1, alpha = 0.7) +
  scale_y_continuous(
    name = "Landslide Events",
    sec.axis = sec_axis(~. * 3, name = "Precipitation (inches)")
  ) +
  scale_color_manual(
    name = "Legend",
    values = c("Landslide Events" = "steelblue",
               "Overall Trend" = "darkred",
               "Precipitation" = "darkgreen")
  ) +
  labs(
    title = "Oregon Landslide Events and Precipitation Over Time (1950-2023)",
    subtitle = "1996 excluded",
    x = "Year",
    caption = "Source: DOGAMI SLIDO & NOAA"
  ) +
  theme_minimal() +
  theme(legend.position = c(0.2, 0.8))


sum(slido$YEAR == 1996, na.rm = TRUE)
table(slido_map$YEAR)

# Total records in SLIDO
nrow(slido)

# Missing year
sum(is.na(slido$YEAR))



# Missing cause
sum(is.na(slido$CONTR_FACT) | slido$CONTR_FACT == "")

# Have both year and cause
sum(!is.na(slido$YEAR) & !is.na(slido$CONTR_FACT) & slido$CONTR_FACT != "")

# Percentage missing year
round(sum(is.na(slido$YEAR)) / nrow(slido) * 100, 1)

# Percentage missing cause
round(sum(is.na(slido$CONTR_FACT) | slido$CONTR_FACT == "") / nrow(slido) * 100, 1)

