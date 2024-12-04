# Load the necessary libraries

library(sf)
library(leaflet)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(leaflet.extras)

# Task 1 - Create map in New Zealand
# Create a basic map centered on New Zealand
nz_map <- leaflet() %>%
  addTiles() %>%
  setView(lng = 174.886, lat = -40.9006, zoom = 5)  # Coordinates for New Zealand

# Display the map
nz_map

# Task 2 - Adding Markers to Auckland, Wellington and Christchurch 


# Create a dataset for New Zealand cities
nz_pop <- data.frame(
  city = c("Auckland", "Wellington", "Christchurch"),
  lat = c(-36.8485, -41.2924, -43.5320),
  lng = c(174.7645, 174.7787, 172.6366),
  population = c(1657000, 212700, 381500)
)

 # Create New Zealand map with markers and provider tiles
nz_map_pop <- leaflet(nz_pop) %>%
  addTiles() %>%  # Using a clear, simple tile for readability
  setView(lng = 174.886, lat = -40.9006, zoom = 5) %>%
  addMarkers(~lng, ~lat, popup = ~paste(city, "<br>Population:", population)) %>% 
  addProviderTiles(providers$OpenTopoMap) 
  
# Display the map
nz_map_pop


# Task 3 - Heat map


# Create the New Zealand heatmap based on population with adjusted intensity
nz_heatmap <- leaflet(nz_pop) %>%
  addTiles() %>%  # Using a clear, simple tile for readability
  setView(lng = 174.886, lat = -40.9006, zoom = 5) %>%
  addMarkers(~lng, ~lat, popup = ~paste(city, "<br>Population:", population)) %>% 
  addProviderTiles(providers$OpenTopoMap) %>% 
  addHeatmap(
    lng = ~lng, 
    lat = ~lat, 
    intensity = ~population, 
    blur = 25,        # Increase blur to soften the heatmap points
    max = 1,  # Ensure scaling based on the highest population
    radius = 20 
)
# Display the heatmap
nz_heatmap


# Choropleth Map

# Task 4 - Read a .geoJSON file

# Read the geoJSON file into an sf object
nz_geojson <- st_read("nz_ta.geojson")

# View the structure of the loaded geoJSON data
print(nz_geojson)

# Task 5 - Create a black and white plot with your regions

# Generate a black and white plot of the NZ territories with a smaller title size
ggplot() +
  geom_sf(data = nz_geojson) +
  theme_void() +
  ggtitle("New Zealand Territorial Boundaries (Black and White)") +
  theme(
    plot.title = element_text(size = 12)  # Adjust the size value as needed
  )


# Task 6: Load the population data. Clean the data

# Load the CSV file containing population data
nz_population_data <- read.csv("nz_territory_2016_population.csv")
head(nz_population_data)

# Clean the data: rename columns
nz_population_cleaned <- nz_population_data[, c("nz_territory", "X2016_population")]
colnames(nz_population_cleaned) <- c("territory", "population")

# Check for missing or unnecessary data (unnecessary rows can be removed if identified)
nz_population_cleaned <- na.omit(nz_population_cleaned)  # Removes any rows with missing data

# Check for outliers by inspecting the summary of the population column
summary(nz_population_cleaned$population)

head(nz_population_cleaned)

# Check the distribution of the population data by plotting a histogram
ggplot(nz_population_cleaned, aes(x = population)) +
  geom_histogram(binwidth = 20000, fill = "gray", color = "black") +
  labs(title = "Distribution of Population Across NZ Territories (2016)", x = "Population", y = "Count of Territories") +
  theme_minimal()

#The histogram shows the distribution of population across New Zealand territories in 2016. 
#We can see that most territories have smaller populations, with a few having significantly higher populations 
#(e.g., Auckland, as mentioned earlier).
#This distribution confirms that there's a long tail with a few territories having much higher
#population counts compared to the majority. To address this, a log scale can be applied when 
#visualizing the data in a choropleth map, which will prevent the high population values from overpowering the rest of the data.

# Task 7 - Merge geospatial and numeric data

# Merge the geospatial data with the population data using a left join
# Ensure the column names used for the merge are correct
nz_merged <- nz_geojson %>%
  left_join(nz_population_cleaned, by = c("TA2016_NAM" = "territory"))

# Inspect the merged data
head(nz_merged)

# Task 8 - Make basic choropleth

# Generate the choropleth map
ggplot(data = nz_merged) +
  geom_sf(aes(fill = population)) +
  theme_void() +
  scale_fill_continuous(type = "viridis", name = "Population") +
  labs(title = "Choropleth Map of NZ Population by Territory")

# Task 9 - Modified choropleth map

# Create the choropleth map with all the enhancements
nz_plot <- ggplot(data = nz_merged) +
  geom_sf(aes(fill = log(population))) +  # Apply log scale transformation
  theme_void() +  # Clean background
  scale_fill_viridis_c(option = "plasma", name = "Log(Population)") +  # Custom color scale
  labs(
    title = "New Zealand Population by Territory (2016)",
    subtitle = "Population distribution visualized on a log scale",
    caption = "Source: StatsNZ, 2016"
  ) +
  theme(
    text = element_text(family = "Arial", size = 12),  # Text font and size
    plot.background = element_rect(fill = "lightblue"),  # Set plot background
    panel.background = element_rect(fill = "white"),  # Set panel background
    legend.background = element_rect(fill = "lightgray"),  # Set legend background
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),  # Title formatting
    plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5),  # Subtitle formatting
    plot.caption = element_text(size = 8, hjust = 0),  # Caption formatting
    legend.position = "right"  # Position the legend on the right
  )

# View plot
print(nz_plot)

# Save plot as PNG file
ggsave("nz_plot.png", plot = nz_plot, width = 10, height = 6, dpi = 300)


# Extra for experts

# Choose a vibrant color palette from RColorBrewer
palette <- brewer.pal(9, "YlGnBu")

# Create the static ggplot choropleth map with custom color scale
static_map <- ggplot(data = nz_merged) +
  geom_sf(aes(fill = log(population), text = paste("Territory:", TA2016_NAM, "<br>Population:", population))) +
  scale_fill_gradientn(colors = palette, name = "Log(Population)") +
  labs(
    title = "Interactive New Zealand Population Map (2016)",
    subtitle = "Log-transformed population distribution",
    caption = "Source: StatsNZ"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    plot.subtitle = element_text(size = 110, face = "italic"),
    plot.caption = element_text(size = 8, hjust = 0)
  )

# Convert the static map into an interactive plotly map
interactive_map <- ggplotly(static_map, tooltip = "text")

# View the interactive map
interactive_map



