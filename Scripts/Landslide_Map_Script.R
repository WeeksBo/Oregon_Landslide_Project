# Get coordinates from the spatial data
slido_coords <- slido_map %>%
  st_transform(crs = 4326) %>%
  st_coordinates() %>%
  as.data.frame()

library(tigris)
library(sf)

oregon_counties <- counties(state = "OR", cb = TRUE) %>%
  st_transform(crs = 4326)

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
    caption = "Source: DOGAMI SLIDO Database",
    x="Longitude",
    y="Latitude"
  )


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
  annotate("point", x = -123.97, y = 44.567,
           color = "yellow", size = 5, shape = 16) +
  annotate("text", x = -124.55, y = 44.5,
           label = "Predicted\nNext\nLandslide\nZone",
           color = "black", size = 3, fontface = "bold") +
  labs(
    title = "Predicted Landslide Risk: 2025 Precipitation,\nRoads and Historical Landslides",
    subtitle = "Dark Blue = High Rainfall | Brown = Major Roads | Red = Historical Landslides",
  ) +
  theme_void()
