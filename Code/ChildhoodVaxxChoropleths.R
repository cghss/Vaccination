##libraries##
library(tidyverse)
library(tidyr)
library(readr)
library(magrittr)
library(dplyr)
library(rnaturalearth)
library(ggplot2)
library(patchwork)


##Load raw data##
vax.raw <- read.csv("Data/ChildhoodVaxxWithIsos1.csv")

##Make Measles only df##

measles_dataframe <- vax.raw %>%
  filter(grepl("Measles", Subtopic_link, ignore.case = TRUE)) %>%
  data.frame()

##Make Diphtheria only df##

dip_dataframe <- vax.raw %>%
  filter(grepl("Diphtheria", Subtopic_link, ignore.case = TRUE)) %>%
  data.frame()

##Make Polio only df##

Polio_dataframe <- vax.raw %>%
  filter(grepl("Polio", Subtopic_link, ignore.case = TRUE)) %>%
  data.frame()

##Make Chickenpox only df##

CPox_dataframe <- vax.raw %>%
  filter(grepl("Varicella", Subtopic_link, ignore.case = TRUE)) %>%
  data.frame()

##Make TB only df##

TB_dataframe <- vax.raw %>%
  filter(grepl("Tuberculosis", Subtopic_link, ignore.case = TRUE)) %>%
  data.frame()

#Call world dataset from rnaturalearth library #
map <- ne_countries(type = 'countries')

# Merge world map data with datasets based on ISO codes
world_Mea_data <- inner_join(measles_dataframe, map, by = "iso_a3_eh")
world_Dip_data <- inner_join(dip_dataframe, map, by = "iso_a3_eh")
world_Pol_data <- inner_join(Polio_dataframe, map, by = "iso_a3_eh")
world_Cpx_data <- inner_join(CPox_dataframe, map, by = "iso_a3_eh")
world_TB_data <- inner_join(TB_dataframe, map, by = "iso_a3_eh")


##Map time
#Measles
world_Mea_map <- ggplot() +
  geom_sf(data = world_Mea_data, aes(fill = Map_color, geometry = geometry),color = "black", size = 0.2) +
  #scale_fill_gradientn(colours = rev(viridis(45, option = "G")), na.value = "white", breaks = seq(0, max(df$n), by = 5))+
  theme_minimal() +
  scale_fill_manual(values =c("white","#33838F","#FFE3F1", "#DEDBDB"), 
                    labels = c("No Data", "Vaccine Mandated", "Vaccine Not Included \nin Mandatory Vaccine Policy", "No Mandated Vaccines")) +
  ggtitle("Measles") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank())
#print(world_Mea_map)

##Diphtheria
world_Dip_map <- ggplot() +
  geom_sf(data = world_Dip_data, aes(fill = Map_color, geometry = geometry),color = "black", size = 0.2) +
  #scale_fill_gradientn(colours = rev(viridis(45, option = "G")), na.value = "white", breaks = seq(0, max(df$n), by = 5))+
  theme_minimal() +
  scale_fill_manual(values =c("white","#33838F","#FFE3F1", "#DEDBDB"), 
                    labels = c("No Data", "Vaccine Mandated", "Vaccine Not Included \nin Mandatory Vaccine Policy", "No Mandated Vaccines")) +
  ggtitle("Diphtheria", ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank())
#print(world_Dip_map)

#Polio
world_Pol_map <- ggplot() +
  geom_sf(data = world_Pol_data, aes(fill = Map_color, geometry = geometry),color = "black", size = 0.2) +
  #scale_fill_gradientn(colours = rev(viridis(45, option = "G")), na.value = "white", breaks = seq(0, max(df$n), by = 5))+
  theme_minimal() +
  scale_fill_manual(values =c("white","#33838F","#FFE3F1", "#DEDBDB"), 
                    labels = c("No Data", "Vaccine Mandated", "Vaccine Not Included \nin Mandatory Vaccine Policy", "No Mandated Vaccines")) +
  ggtitle("Polio") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank())
#print(world_Pol_map)

#Varicella
world_Cpx_map <- ggplot() +
  geom_sf(data = world_Cpx_data, aes(fill = Map_color, geometry = geometry, color = mycolors),color = "black", size = 0.2) +
  #scale_fill_gradientn(colours = rev(viridis(45, option = "G")), na.value = "white", breaks = seq(0, max(df$n), by = 5))+
  theme_minimal() +
  scale_fill_manual(values =c("white","#33838F","#FFE3F1", "#DEDBDB"), 
                    labels = c("No Data", "Vaccine Mandated", "Vaccine Not Included \nin Mandatory Vaccine Policy", "No Mandated Vaccines")) +
  ggtitle("Varicella") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank())
print(world_Cpx_map)

#TB
world_TB_map <- ggplot() +
  geom_sf(data = world_TB_data, aes(fill = Map_color, geometry = geometry),color = "black", size = 0.2) +
  #scale_fill_gradientn(colours = rev(viridis(45, option = "G")), na.value = "white", breaks = seq(0, max(df$n), by = 5))+
  theme_minimal() +
  scale_fill_manual(values =c("white","#33838F","#FFE3F1", "#DEDBDB"), 
                    labels = c("No Data", "Vaccine Mandated", "Vaccine Not Included \nin Mandatory Vaccine Policy", "No Mandated Vaccines")) +
  ggtitle("Tuberculosis") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank())
#print(world_TB_map)

(world_Mea_map + world_Dip_map) /
  (world_Pol_map + world_TB_map + world_Cpx_map) + plot_layout(guides = "collect")





#####This is the end of the code that is in the paper####
#make a map of the vaccine preventable disease outbreaks in 2019

dis.raw <- read.csv("VPD2019WHO.csv")
countries <- read.csv("countries.csv")

dis.raw %>%
  select(- Country...Region) %>%
  mutate(across(where(is.numeric), as.character)) %>%
  pivot_longer(!iso_a3_eh, names_to = "Disease", values_to = "Cases") %>%
  filter(!is.na(Cases) & Cases != "") -> disdf

dis_data <- inner_join(disdf, countries, by = "iso_a3_eh")

# Assign coordinates
dis_data$Latitude <- as.numeric(dis_data$Latitude)
dis_data$Longitude <- as.numeric(dis_data$Longitude)

dis_data$Cases <- as.numeric(gsub(",", "", dis_data$Cases))
dis_data$Disease <- factor(dis_data$Disease)
disease_colors <- c("Measles" = "#33838F", "Polio" = "#FF0000", "Diptheria" = "#00FF00", "TB" = "#FFFF00")

# Plotting
outbreak_map <- ggplot() +
  geom_sf(data = map, aes(geometry = geometry)) +
  geom_point(data = dis_data, aes(x = Longitude, y = Latitude, color = Disease, size = Cases), alpha = 0.3, 
             position = position_jitter(width = 1.2, height = 1.2))+
  scale_color_manual(values = disease_colors) +
  scale_size_continuous(range = c(1, 40), guide = "none") + 
  theme_minimal() + 
  ggtitle("Outbreaks of Vaccine Preventable Diseases, 2019") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank())

print(outbreak_map)



((outbreak_map)+ plot_layout(guides = 'keep')) /
  ((world_Mea_map | world_Dip_map ) / (world_Pol_map | world_TB_map) + plot_layout(guides = "collect"))
