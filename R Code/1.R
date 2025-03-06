# Set Working Directory

setwd("D:/AQLI Test Files")

# Load library

library(tidyverse)
library(data.table)
library(sf)
library(ggplot2)
library(sp)
library(highcharter)
library(rmapshaper)

# Reading files

gadm2_aqli <- read_csv("./gadm2_aqli_1998_2021.csv")

# Reading Shape File


# Reading Aqli data dictionary

# How many GADM2 regions are present in India?

num_regions <- gadm2_aqli %>%
  filter(iso_alpha3 == "IND") %>%
  summarise(numbers = n_distinct(objectid_gadm2))


# How many GADM2 regions are present in India

# Total GADM2 regions in India result
sprintf("Total GADM2 regions in India: %d", num_regions$numbers)



# Calculate population weighted pollution average of all years at country (GADM0) level


# Find pollution columns  
pollution_years <- grep("^pm", colnames(gadm2_aqli), value = TRUE)


df_filtered <- gadm2_aqli %>%
  select(country, population, all_of(pollution_years)) %>%
  drop_na()

# Calculate population weighted pollution at country level

country_level_pop_weighted_pollution <- gadm2_aqli %>%
                                        group_by(country) %>%
                                        summarise(across(all_of(pollution_years), ~ sum(. * population, na.rm = TRUE) / sum(population, na.rm = TRUE)))


write_csv(country_level_pop_weighted_pollution, "./population_weighted_pollution.csv")


# Find the 10 most polluted countries in 2021
top_10_polluted <- country_level_pop_weighted_pollution %>%
                   arrange(desc(pm2021)) %>%
                   head(10) %>% select(country)

sprintf("10 most polluted countries in 2021: %d", top_10_polluted$country)


# What was the most polluted GADM2 region in the world in 1998, 2005 and 2021?

# Select relevant columns
selected_years <- c("pm1998", "pm2005", "pm2021", "country", "name_1", "name_2")

# Find the most polluted GADM2 region for selected year

most_polluted <-  gadm2_aqli %>%
                  select(selected_years) %>%
                  pivot_longer(cols = !c("country", "name_1", "name_2"), names_to = "year", values_to = "pollution") %>%
                  group_by(year) %>% 
                  slice_max(order_by = pollution ,n = 1)


sprintf("Most polluted GADM2 region 1998, 2005, 2021: %d", most_polluted)

# Plot a population weighted pollution average trendline plot for Uttar Pradesh from
# 1998 to 2021. Save this plot as a high quality PNG file.

pop_weighted_pollutions_up <- gadm2_aqli %>% filter(name_1 %in% "Uttar Pradesh") %>% 
  group_by(name_1) %>%
  summarise(across(all_of(pollution_years), ~ sum(. * population, na.rm = TRUE) / sum(population, na.rm = TRUE))) %>% 
  pivot_longer(cols = !name_1, names_to = "year", values_to = "pollution")

pop_weighted_pollutions_up$year <- as.integer(gsub("pm", "", pop_weighted_pollutions_up$year))



# Filter data for Uttar Pradesh and the required years


pop_weighted_pol_up <- ggplot(pop_weighted_pollutions_up, aes(x = year, y = pollution)) +
  geom_line(color = "#990000", size = 1.2) +  
  geom_point(color = "#990000", size = 3) + 
  scale_x_continuous(breaks = seq(1998, 2021, by = 2)) +
  labs(
    title = "Population-Weighted PM2.5 Levels in Uttar Pradesh (1998–2021)",
    subtitle = "Annual Trend Line Plot",
    x = "Year",
    y = "PM2.5 Concentration (µg/m³)",
    caption = "Source: Air Quality Life Index (AQLI)"
  ) +
  theme_minimal(base_size = 14) +   
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5,),
    #plot.caption = element_text(size = 5, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  )


# Save high-quality PNG
ggsave("UP_PM2.5_Trend.png",plot = pop_weighted_pol_up, width = 10, height = 6, dpi = 300, bg = "white")


