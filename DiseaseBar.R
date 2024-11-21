library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

#Reading in the data and cleaning it up
diseases.raw <- read.csv("MandatedDiseasesFinal.csv")

diseases.df <- subset(diseases.raw, !is.na(Disease) & Disease != "")

diseases.sep <- diseases.df %>%
  separate_rows(Disease, sep = ",")

#Now I want to know how many times each of the diseases shows up in policy
disease_counts <- diseases.sep %>%
  count(Disease)

colnames(disease_counts) <- c("Disease", "Count")

#A few diseases have better names for display
disease_counts <- disease_counts %>%
  mutate(Disease = case_when(
    Disease == "meningococcal disease" ~ "Meningococcal Disease",
    Disease == "Influenza" ~ "Annual Influenza",
    Disease == "Streptococcus pneumoniae" ~ "Pneumococcal Disease",
    Disease == "Human Papillomavirus" ~ "Human Papillomavirus (HPV)",
    Disease == "Haemophilus influenzae type B" ~ "Haemophilus Influenzae Type B(HiB)",
    Disease == "Tuberculosis (all forms)" ~ "Tuberculosis",
    TRUE ~ Disease
  ))
#Arrange them in descending order so it looks nicer
disease_counts <- disease_counts %>% arrange(desc(Count))

#Plot!
ggplot(disease_counts, aes(x = reorder(Disease, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "#1202A8") +
  theme_minimal() +
  labs(x = "Disease", y = "Number of Countries Mandating Routine Vaccination") +
  theme(
    axis.text.y =element_text(angle = 0, vjust =0.5, hjust =1),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
    ) +
  coord_flip()



