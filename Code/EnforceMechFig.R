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
genvax.raw <- read.csv("Data/GenVaxxWIsos.csv")

gv_df <- genvax.raw %>%
  subset(Subtopic.link == "Enforcement of childhood vaccination requirement")
##Make 2+ only df##

two_plus <- gv_df %>%
  filter(grepl("2 or more enforcement mechanisms", Status.link, ignore.case = TRUE)) %>%
  data.frame()

##Make legal sanctions only df##

legal <- gv_df %>%
  filter(grepl("Legal sanctions", Status.link, ignore.case = TRUE)) %>%
  data.frame()

##Make social exclusion only df##

social <- gv_df %>%
  filter(grepl("Social exclusion", Status.link, ignore.case = TRUE)) %>%
  data.frame()

##Make Financial levers only df##

financial <- gv_df %>%
  filter(grepl("Financial levers", Status.link, ignore.case = TRUE)) %>%
  data.frame()


##Make none only df##

none <- gv_df %>%
  filter(grepl("No enforcement mechanisms", Status.link, ignore.case = TRUE)) %>%
  data.frame()

#Call world dataset from rnaturalearth library #
map <- ne_countries(type = 'countries')

# Merge world map data with datasets based on ISO codes
world_2_data <- right_join(two_plus, map, by = "iso_a3_eh")
world_leg_data <- right_join(legal, map, by = "iso_a3_eh")
world_soc_data <- right_join(social, map, by = "iso_a3_eh")
world_fin_data <- right_join(financial, map, by = "iso_a3_eh")
world_none_data <- right_join(none, map, by = "iso_a3_eh")

#Get rid of NA, which is messing up the mapping aesetics below
world_2_data$Status.link[is.na(world_2_data$Status.link)] <- "No data"
world_leg_data$Status.link[is.na(world_leg_data$Status.link)] <- "No data"
world_soc_data$Status.link[is.na(world_soc_data$Status.link)] <- "No data"
world_fin_data$Status.link[is.na(world_fin_data$Status.link)] <- "No data"
world_none_data$Status.link[is.na(world_none_data$Status.link)] <- "No data"
##Map time
#2 or more

world_two_map <- ggplot() +
  geom_sf(data = world_2_data, aes(fill = Status.link, geometry = geometry), color = "black", size = 0.2) +
  theme_minimal() +
  scale_fill_manual(
    values = c("#33838F", "#F0EDEC"),
    labels = c("Enforcement Mechanism In Place", "Enforcement Mechanism Not In Place")
  ) +
  ggtitle("2 or More Enforcement Mechanisms") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank()
  )
print(world_two_map)

##Legal Sanctions
world_leg_map <- ggplot() +
  geom_sf(data = world_leg_data, aes(fill = Status.link, geometry = geometry), color = "black", size = 0.2) +
  theme_minimal() +
  scale_fill_manual(
    values = c("#33838F", "#F0EDEC"),
    labels = c("Enforcement Mechanism In Place", "Enforcement Mechanism Not In Place")
  ) +
  ggtitle("Legal Sanctions") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank()
  )
print(world_leg_map)


#Social Exclusion
world_soc_map <- ggplot() +
  geom_sf(data = world_soc_data, aes(fill = Status.link, geometry = geometry), color = "black", size = 0.2) +
  theme_minimal() +
  scale_fill_manual(
    values = c("#33838F", "#F0EDEC"),  # Replicating the color values from world_fin_map
    labels = c("Enforcement Mechanism In Place", "Enforcement Mechanism Not In Place"), 
    breaks = c("Social exclusion", "No data")  # Specifying breaks explicitly
  ) +
  ggtitle("Social Exclusion") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank()
  )
print(world_soc_map)


world_fin_map <- ggplot() +
  geom_sf(data = world_fin_data, aes(fill = Status.link, geometry = geometry), color = "black", size = 0.2) +
  theme_minimal() +
  scale_fill_manual(
    values = c("#33838F", "#F0EDEC"),
    labels = c("Enforcement Mechanism In Place", "Enforcement Mechanism Not In Place")
  ) +
  ggtitle("Financial Levers") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank()
  )
print(world_fin_map)

#None 

world_none_map <- ggplot() +
  geom_sf(data = world_none_data, aes(fill = Status.link, geometry = geometry), color = "black", size = 0.2) +
  theme_minimal() +
  scale_fill_manual(
    values = c("#33838F", "#F0EDEC"),  # Replicating the color values from world_fin_map
    labels = c("Enforcement Mechanism In Place", "Enforcement Mechanism Not In Place"), 
    breaks = c("No enforcement mechanisms", "No data")  # Specifying breaks explicitly
  ) +
  ggtitle("No Enforcement Mechanism Identified") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    legend.position = "right",  
    legend.title = element_blank()
  )
print(world_none_map)


(world_two_map + world_leg_map + world_soc_map) / 
  (world_fin_map + world_none_map) + plot_layout(guides = "collect")



