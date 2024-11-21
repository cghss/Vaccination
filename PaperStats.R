library(readr)
library(tidyverse)
library(dplyr)


vaxx.master <- read.csv("~/Dropbox/R Projects/AMPEIDData/Vaxx-Master.csv")

#Answer: How many countries have at least one disease for which a child must be vaccinated
vaxx.df <- vaxx.master %>%
  filter(Subtopic == "Requirement for childhood vaccination")

status_counts <- vaxx.df %>%
  count(Status, name = "Count")

#Answer: How do countries enforce mandates?
force.df <- vaxx.master %>%
  filter(Subtopic == "Enforcement of childhood vaccine mandates")

force_status_counts <- force.df %>%
  count(Status, name = "Count")

#Need the legal sanctions pulled out
enforce.df <- vaxx.master %>%
  filter(Subtopic == "Enforcement of childhood vaccine mandates")

legal.df <- enforce.df %>%
  filter(Status %in% c("Criminalization is the only enforcement mechanism specified in legislation", "2 or more enforcement mechanisms specified in legislation"))
