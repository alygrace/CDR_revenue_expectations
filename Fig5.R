rm(list = ls())

# ------------------------------------------------------
# 1) Load Required Libraries
# ------------------------------------------------------

library(tidyverse)   
library(dplyr)    
library(stringr)    
library(patchwork)   
library(ggtext)      
library(cowplot) 
library(ggrepel)
library(knitr)

# ------------------------------------------------------
# 2) Load and Recode Dataset
# ------------------------------------------------------

path <- ""

data <- read_csv(file.path(path,"processed/full_data.csv"), show_col_types = FALSE)

year_map <- c("1" = 2024, "2" = 2027, "3" = 2030, "4" = 2040, "5" = 2050)

data_subset <- data %>% filter(!is.na(Byproducts))

data_subset <- data_subset %>% 
  separate_rows(`Byproducts`, sep = ",\\s*")

data_subset <- data_subset %>%
  select(interview_ID, CDR_solution_recode,Byproducts, matches("^Q10_\\d+$")) %>%
  pivot_longer(
    cols = matches("^Q10_\\d+$"),
    names_to = "Code",
    names_pattern = "Q10_(\\d+)",
    values_to = "Value"
  ) %>%
  mutate(
    Value = as.numeric(Value),
    YEAR = as.integer(year_map[Code])
  )

# ------------------------------------------------------
# Data input
# ------------------------------------------------------

# Share per solution
df <- data %>% 
  separate_rows(`Byproducts`, sep = ",\\s*")

summary_share <- df %>%
  group_by(CDR_solution_recode) %>%
  mutate(total_n = n()) %>%  
  ungroup() %>%
  filter(!is.na(Byproducts) & Byproducts != "") %>%
  count(CDR_solution_recode, Byproducts) %>%
  left_join(
    data %>%
      count(CDR_solution_recode, name = "total_n"),
    by = "CDR_solution_recode"
  ) %>%
  mutate(share = n / total_n)

print(summary_share)


#Sample size
data_subset %>%
  distinct(CDR_solution_recode,Byproducts, interview_ID) %>%
  count(CDR_solution_recode,Byproducts)

summary_table <- data_subset %>%
  filter(YEAR %in% c(2024, 2030, 2050)) %>%
  group_by(CDR_solution_recode, Byproducts, YEAR) %>%
  summarise(
    mean = mean(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = YEAR,
    values_from = c(mean),
    names_glue = "{.value}_{YEAR}"
  )

print(summary_table)

summary_table_2 <- data_subset %>%
  filter(YEAR %in% c(2024, 2030, 2050)) %>%
  group_by(CDR_solution_recode, YEAR) %>%
  summarise(
    mean = mean(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = YEAR,
    values_from = c(mean),
    names_glue = "{.value}_{YEAR}"
  )

print(summary_table_2)

summary <- full_join(summary_share, summary_table, by = c( "CDR_solution_recode","Byproducts"))


kable(summary, caption = "Additional revenue source summary statistics", digits = 2)

