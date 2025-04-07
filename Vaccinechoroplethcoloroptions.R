library(readr)
library(tidyr)
library(dplyr)
library(countrycode)
library(rnaturalearth)
library(ggplot2)
library(viridisLite)
library(scales)

#Reading in the data and cleaning it up
diseases.raw <- read.csv("MandatedDiseasesFinal.csv")

diseases.df <- subset(diseases.raw, !is.na(Disease) & Disease != "")

diseases.sep <- diseases.df %>%
  separate_rows(Disease, sep = ",")

# Count vaccines in each country 
country.counts <- diseases.sep %>%
  count(Country)
# Rename columns for clarity
colnames(country.counts) <- c("Country", "Count")

#Cool, so now I want to merge with iso codes so I can map this
country.counts$ISO <- countrycode(sourcevar = country.counts$Country, origin = "country.name", destination = "iso3c")

#map geometry from rnaturalearth
map <- ne_countries(type = 'countries')

#join
vaxx.data <- full_join(country.counts, map, by = c("ISO" = "iso_a3_eh"))

choropleth <- ggplot() +
  geom_sf(data = vaxx.data, aes(fill = Count, geometry = geometry), color = "black", size = 0.2) +
  scale_fill_gradientn(colors = c("#D3F9FE", "#A9F4FF", "#33838F", "#01333B"),
                      na.value = "white", 
                      breaks = seq(0, max(vaxx.data$Count, na.rm = TRUE), by = 2), 
                      name = "Number of Mandated Vaccines") +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10),
    legend.key.size = unit(4, "lines"),
    legend.key.height = unit(1.0, "cm"),
    legend.key.width = unit(0.7, "cm"), 
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  labs(fill = "Frequency") +  
  coord_sf(expand = FALSE) + 
  guides(fill = guide_colorbar(ticks.colour = NA))

print(choropleth)

choropleth <- ggplot() +
  geom_sf(data = vaxx.data, aes(fill = Count, geometry = geometry), color = "black", size = 0.2) +
  scale_fill_gradientn(colors = c("#DDC4F6", "#B066F9", "#440187"),
                       na.value = "white", 
                       breaks = seq(0, max(vaxx.data$Count, na.rm = TRUE), by = 2), 
                       name = "Number of Mandated Vaccines") +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10),
    legend.key.size = unit(4, "lines"),
    legend.key.height = unit(1.0, "cm"),
    legend.key.width = unit(0.7, "cm"), 
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  labs(fill = "Frequency") +  
  coord_sf(expand = FALSE) + 
  guides(fill = guide_colorbar(ticks.colour = NA))

print(choropleth)

choropleth <- ggplot() +
  geom_sf(data = vaxx.data, aes(fill = Count, geometry = geometry), color = "black", size = 0.2) +
  scale_fill_gradientn(colors = c("#BED5F9", "#70A6FB", "#013FA1"),
                       na.value = "white", 
                       breaks = seq(0, max(vaxx.data$Count, na.rm = TRUE), by = 2), 
                       name = "Number of Mandated Vaccines") +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10),
    legend.key.size = unit(4, "lines"),
    legend.key.height = unit(1.0, "cm"),
    legend.key.width = unit(0.7, "cm"), 
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  labs(fill = "Frequency") +  
  coord_sf(expand = FALSE) + 
  guides(fill = guide_colorbar(ticks.colour = NA))

print(choropleth)


  
choropleth <- ggplot() +
    geom_sf(data = vaxx.data, aes(fill = Count, geometry = geometry), color = "black", size = 0.2) +
    scale_fill_gradientn(
      colours = rev(viridis(45, option = "G")),
                         na.value = "white", 
                         breaks = seq(0, max(vaxx.data$Count, na.rm = TRUE), by = 2), 
                         name = "Number of Mandated Vaccines") +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 10),
      legend.key.size = unit(4, "lines"),
      legend.key.height = unit(1.0, "cm"),
      legend.key.width = unit(0.7, "cm"), 
      plot.title = element_text(hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    ) +
    labs(fill = "Frequency") +  
    coord_sf(expand = FALSE) + 
    guides(fill = guide_colorbar(ticks.colour = NA))
  
  print(choropleth)
  
  choropleth <- ggplot() +
    geom_sf(data = vaxx.data, aes(fill = Count, geometry = geometry), color = "black", size = 0.2) +
    scale_fill_gradientn(colors = c("#E7D3FE", "#A9F4FF", "#33838F", "#01333B"),
                         na.value = "white", 
                         breaks = seq(0, max(vaxx.data$Count, na.rm = TRUE), by = 2), 
                         name = "Number of \nMandated Vaccines") +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 10),
      legend.key.size = unit(4, "lines"),
      legend.key.height = unit(1.0, "cm"),
      legend.key.width = unit(0.7, "cm"), 
      plot.title = element_text(hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),  
      axis.text = element_blank(),  
      axis.ticks = element_blank()  
    ) +
    labs(fill = "Frequency") +  
    coord_sf(expand = FALSE) + 
    guides(fill = guide_colorbar(ticks.colour = NA))
  
  print(choropleth)

  
  
  choropleth <- ggplot() +
    geom_sf(data = vaxx.data, aes(fill = Count, geometry = geometry), color = "black", size = 0.2) +
    scale_fill_gradientn(colors = c("#E7D3FE", "#A9F4FF", "#33838F", "#01333B"),
                         na.value = "white", 
                         breaks = seq(0, max(vaxx.data$Count, na.rm = TRUE), by = 2), 
                         name = "Number of \nMandated Vaccines") +
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 10),
      legend.key.size = unit(4, "lines"),
      legend.key.height = unit(1.0, "cm"),
      legend.key.width = unit(0.7, "cm"), 
      plot.title = element_text(hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),  
      axis.text = element_blank(),  
      axis.ticks = element_blank()  
    ) +
    labs(fill = "Frequency") +  
    coord_sf(expand = FALSE) + 
    guides(fill = guide_colorbar(ticks.colour = NA))
  
  print(choropleth)
 