library(sf)
library(ggplot2)
library(dplyr)
library(sf)
library(ggplot2)
library(dplyr)
library(readxl)
library(ggtext)
library(tidyverse)
########################
setwd("D:/AQLI Test Files")

# Load files 

gadm2_shp <- read_sf("./gadm2_aqli_shapefile/gadm2_aqli_shapefile/aqli_gadm2_final_june302023.shp")

gadm2_aqli <- read_csv("./gadm2_aqli_1998_2021.csv")

# Plot a bar graph for the life years lost relative to the WHO guideline in the 10 most
# polluted countries in the world and also plot them on a global country level map. For the
# map, the 10 most polluted country boundaries should be filled in with “dark red” and the
# rest of the map should be grayed out. Save both the bar graph and the map as high
# quality PNG files.



columns_llpp_who <- grep("^llpp_who", colnames(gadm2_aqli), value = T)

country_pollution <- gadm2_aqli %>%
  group_by(country) %>%
  summarise(across(all_of(columns_llpp_who), ~ sum(. * population, na.rm = TRUE) / sum(population, na.rm = TRUE)),
            country_population = sum(population, na.rm = TRUE)) %>%
  arrange(desc(llpp_who_2021)) %>%  
  head(10)


# Defining theme for bar chart
theme_aqli <- function() {
  theme_minimal(base_size = 14) +
    theme(
      panel.grid.major = element_blank(),  # Remove major gridlines
      panel.grid.minor = element_blank(),  # Remove minor gridlines
      axis.line = element_line(color = "black"),  # Add axis lines
      axis.text = element_text(face = "bold"),  # Bold axis text
      axis.title = element_text(face = "bold"),  # Bold axis titles
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Centered title
      plot.caption = element_text(size = 10, hjust = 1, color = "gray50"),
      legend.position = "none"
    )
}

# Bar Chart for Top 10 Most Polluted Countries

top_10_polluted_0level <- ggplot(country_pollution, aes(x = reorder(country, llpp_who_2021), y = llpp_who_2021, fill = llpp_who_2021)) +
  geom_bar(stat = "identity", width = 0.6, color = "grey50") +  # Add border for clarity
  scale_fill_gradient(high = "#8B0000", low = "#FF4500") +  # Dark Red to Bright Red
  coord_flip() +  # Horizontal bar chart
  labs(title = "Life Years Lost Relative to WHO Guideline (2021)",
       x = "",
       y = "Life Years Lost (Years)",
       fill = "Years Lost",  # Legend title
       caption = "Source: Air Quality Life Index (AQLI)") +
  theme_aqli()


ggsave("top_10_polluted_0level.png",plot = top_10_polluted_0level, width = 10, height = 6, dpi = 300, bg = "white")



##############################################
# Dissolve by country and "name1

# countries_sf <- gadm2_shp %>%
#   group_by(country) %>%
#   summarise(geometry = st_union(geometry), .groups = "drop")

# Load high-resolution country shapefile
country_level_shp <- st_read("./AQLI_country_shp/countries_sf.shp") %>%
  st_transform(crs = "+proj=robin")  

# Ensure proper column names for join
country_pollution <- country_pollution %>% 
  select(country, llpp_who_2021)

gadm_sf_final <- left_join(country_level_shp, country_pollution, by = c("name0" = "country"))

# Define theme for GIS
theme_aqli_map <- function() {
  theme_void(base_size = 16) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5, family = "sans"),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40", family = "sans"),
      plot.caption = element_text(size = 10, hjust = 0.95,vjust = -0.3 ,color = "gray50", family = "sans"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10, family = "sans", vjust = 0.8),
      legend.text = element_text(face = "bold",size = 8, family = "sans", vjust = NULL),
      plot.background = element_rect(fill = "white", color = NA),
      legend.key.width = unit(2.5, "lines"),
      legend.key.height = unit(0.75, "lines"),
      legend.spacing = unit(-5, "pt"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}


# Plot the GIS map
plot<- ggplot(data = gadm_sf_final) +
  geom_sf(aes(fill = llpp_who_2021), color = "gray72", size = 0.3) +  # Thin country borders
  scale_fill_gradientn(
    colors = c("#8B0000"), # Yellow to deep red
    na.value = "gray85",
    name = "Life Years Lost (Years)"
  )+
  labs(
    title = "Air Pollution Impact: Life Years Lost Relative to WHO Guideline",
    subtitle = "Estimated loss in life expectancy due to PM2.5 pollution",
    caption = "Source: Air Quality Life Index (AQLI)"
  ) +
  theme_aqli_map()

ggsave("2.1_name0_10_most_polluted_countries.png", plot = plot,width = 14, height = 8, dpi = 300, type = "cairo-png")



# Define Eastern and Western Europe based on EuroVoc classification
# Source: http://op.europa.eu/en/web/eu-vocabularies/concept-scheme/-/resource?uri=http://eurovoc.europa.eu/100277

eastern_europe <- c("Albania", "Belarus", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Czech Republic", "Estonia", "Hungary", "Latvia", "Lithuania", "Moldova", "Montenegro", "North Macedonia", "Poland", "Romania", "Russia", "Serbia", "Slovakia", "Slovenia", "Ukraine")

western_europe <- c("Austria", "Belgium", "Denmark", "Finland", "France", "Germany", "Iceland", "Ireland", "Italy", "Luxembourg", "Netherlands", "Norway", "Portugal", "Spain", "Sweden", "Switzerland", "United Kingdom")


# Filter for Europe only
gadm2_europe <- gadm2_aqli %>% 
  filter(country %in% c(eastern_europe, western_europe))

gadm2_europe <- gadm2_europe %>% mutate(region = ifelse(country %in% eastern_europe, "Eastern Europe", "Western Europe"))


gadm2_final <- left_join(gadm2_shp, gadm2_europe, by = c("obidgadm2" = "objectid_gadm2"))


# Create the plot
europe_le <- ggplot(data = gadm2_final) +
  geom_sf(aes(fill = llpp_who_2021), color = NA, size = 0) +
  
  scale_fill_stepsn(
    colours = c("#ffffff", "#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#e04130", "#d7301f", "#990000", "#4d004b"),  
    breaks = c(0, 0.1, 0.5, 1, 2, 3, 4, 5, 6, 7),  # 10 breaks
    values = scales::rescale(c(0, 0.1, 0.5, 1, 2, 3, 4, 5, 6, 7)),  
    limits = c(0, 7),
    na.value = "gray85",
    name = "Potential Gain in Life Expectancy (Years)",
    labels = c("0", "0.1", "0.5", "1.0", "2.0", "3.0", "4.0", "5.0", ">=6.0", "")  # 10 labels
  ) +
  
  labs(
    title = "Potential Gain in Life Expectancy",
    subtitle = "Relative to WHO PM2.5 Guideline",
    caption = "Source: Air Quality Life Index (AQLI)"
  ) +
  
  theme_aqli_map()

# Save the plot

ggsave("Europe_Potential_Gain_Life_Expectancy1.png", plot = europe_le, width = 10, height = 6, dpi = 300, bg = "white")

# Save the plot as a high-quality PDF
ggsave("Europe_Potential_Grain_Life_Expectancy.pdf", plot = europe_le, width = 12, height = 8, dpi = 150, device = cairo_pdf)
gc()





###################################################################################################

# Aggregate data to country (Level 0) level
gadm0_data <- gadm2_aqli %>% select("country", "objectid_gadm2", "iso_alpha3", "country", "name_1", "population", "pm2021") %>%
  group_by(country, name_1 ) %>%
  summarise(PM2_5_Avg = mean(pm2021, na.rm = TRUE))

# countries_sf <- gadm2_shp %>%
#   group_by(country, name1) %>%
#   summarise(geometry = st_union(geometry), .groups = "drop")

gadm2_shp <- read_sf("./State_level_shp/state_sf.shp")


# Merge country-level data
gadm0_sf_final <- merge(gadm2_shp, gadm0_data, by.x = c("name0", "name1"), by.y = c("country","name_1"), all.x =T)

 For country label
country__select_level_shp <- country_level_shp %>% filter(name0 %in% c("Australia","Russia","Afghanistan","Bhutan", "China", "Canada",
                                                                       "Cambodia", "Egypt", "France","Iran","Philippines"
))

theme_aqli_map <- function() {
  theme_void(base_size = 16) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5, family = "sans"),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40", family = "sans"),
      plot.caption = element_text(size = 10, hjust = 0.95,vjust = -0.3 ,color = "gray50", family = "sans"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10, family = "sans", vjust = 0.8),
      legend.text = element_text(face = "bold",size = 9, family = "sans", vjust = NULL),
      plot.background = element_rect(fill = "white", color = NA),
      legend.key.width = unit(2.5, "lines"),
      legend.key.height = unit(0.75, "lines"),
      legend.spacing = unit(-5, "pt"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# AQLI Air Pollution color scale
color_scale <- scale_fill_gradientn(
  colors = c("#e5f4fb", "#c1e3f9", "#7dc4f5", "#2b7ac0", "#3a5073", "#2f2f4f"),  # Light blue to dark purple
  values = scales::rescale(c(0, 5, 15, 25, 40, 80, 100, 130)),
  na.value = "gray85",
  name = "PM2.5 (µg/m³)",
)


# Create the plot
plot <- ggplot(data = gadm0_sf_final) +
  geom_sf(aes(fill = PM2_5_Avg), color = "NA", size =0) +
  color_scale +
  theme_void() + 
  labs(
    title = "Global Air Pollution Levels",
    subtitle = "PM2.5 Concentration (µg/m³)",
    caption = "Source: Air Quality Life Index (AQLI)"
  ) +
  geom_text(
    data = country__select_level_shp, 
    aes(label = name0, geometry = geometry), 
    stat = "sf_coordinates", 
    size = 3, 
    color = "grey40", 
    family = "sans"  # Change this to your desired font
  )+
  theme_aqli_map()

ggsave("Country_Level_Air_Pollution.png", plot = plot,width = 14, height = 8, dpi = 300, type = "cairo-png")

# Save the plot as a high-quality SVG
ggsave("Country_Level_Air_Pollution.svg", plot = plot, width = 14, height = 8, dpi = 320, device = "svg")

