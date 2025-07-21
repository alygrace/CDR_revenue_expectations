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
library(rnaturalearthdata)
library(rnaturalearthhires)
library(rnaturalearth)
library(sf)
library(scales)
library(svglite)

# ------------------------------------------------------
# 2) Load and Recode Dataset
# ------------------------------------------------------

path <- ""

all <- read_csv(file.path(path,"RAW/interview_population.csv"),show_col_types = FALSE)
data <- read_csv(file.path(path,"processed/full_data.csv"),show_col_types = FALSE)

# Group mentioned HQ and project location countries into regions (Canada, United States, Europe, other OECD, non OECD)
europe <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
  "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
  "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
  "Spain", "Sweden", "Northern Ireland", "Switzerland", "Iceland", "Liechtenstein", "Norway",
  "UK", "Scotland"
)

Canada <- c(
  "Canada"
)

USA <- c(
  "USA", "United States"
)

oecd_countries <- c(
  "Chile", "New Zealand", "Mexico", "Israel", "Australia","Japan", "South Korea"
)

non_oecd_countries <- c( "Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Dominica", "Grenada", "Guyana", "Monserrat", "Saint Kitts and Nevis","Saint Vincent and the Grenadines", "Saint Lucia", "Suriname", "Trinidad and Tobago", "Namibia", "India", "Indonesia",
  "Kenya", "China", "Philippines", "UAE", "Kingdom of Saudi Arabia", "Oman", "Egypt", "Morocco",  "Jamaica", "Ukraine", "Brazil"
)

info_df <- data %>% 
  separate_rows(`Project_location`, sep = ",\\s*")

length(unique(info_df$HQ))
length(unique(info_df$Project_location))

info_df <- info_df %>%
  mutate(HQ_region = case_when(
    HQ %in% USA ~ "United States",
    HQ %in% Canada ~ "Canada",
    HQ %in% europe ~ "Europe",
    HQ %in% oecd_countries ~ "Other OECD",
    HQ %in% non_oecd_countries ~ "Non-OECD"
  ))

info_df <- info_df %>%
  mutate(project_region = case_when(
    Project_location %in% USA ~ "United States",
    Project_location %in% Canada ~ "Canada",
    Project_location %in% europe ~ "Europe",
    Project_location %in% oecd_countries ~ "Other OECD",
    Project_location %in% non_oecd_countries ~ "Non-OECD"
  ))


# Only keep one observation per project region
df <- info_df %>%
  group_by(interview_ID) %>%
  distinct(project_region, .keep_all = TRUE) %>%
  ungroup()


# ------------------------------------------------------
# 3) Panel a and c: World Map
# ------------------------------------------------------

# Fix known mismatches between country names and the world map
name_corrections <- tibble(
  original = c("UK", "USA", "Scotland", "UAE", "Kingdom of Saudi Arabia", "Monserrat"),
  corrected = c("United Kingdom", "United States of America", "United Kingdom", "United Arab Emirates", "Saudi Arabia", "Montserrat")
)

recode_names <- function(x, lookup) {
  tibble(original = x) %>%
    left_join(lookup, by = "original") %>%
    mutate(final = coalesce(corrected, original)) %>%
    pull(final)
}

# Load country boundaries as 'sf' object
world <- ne_countries(scale = "large", returnclass = "sf") %>%
  filter(name != "Antarctica") 

# ------------------------------------------------------
# Panel a: Population
# ------------------------------------------------------

HQ <- unique(all$`Country (HQ)`)
HQ <- recode_names(HQ, name_corrections)

print(HQ)

world <- world %>%
  mutate(category = ifelse(admin %in% HQ, "HQ", "Other"))

color_map <- c("HQ" = "#916953", "Other" = "gray95")

population_map <- ggplot(data = world) +
  geom_sf(aes(fill = category), color = "white", size = 0.2) +
  scale_fill_manual(values = color_map, name = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank()
  )

print(population_map)

ggsave(
  filename = paste0(file.path(path,"Visualizations/Population/map"), ".svg"),
  plot = population_map,
  width = 8, height = 2.5,
  device = svglite
)

# ------------------------------------------------------
# Panel c: Sample
# ------------------------------------------------------

project_locations <- unique(info_df$Project_location)
HQ_locations <- unique(info_df$HQ)

project_locations <- recode_names(project_locations, name_corrections)
HQ_locations      <- recode_names(HQ_locations, name_corrections)

print(project_locations)
print(HQ_locations)


world <- world %>%
  mutate(
    category = case_when(
      name %in% project_locations & name %in% HQ_locations ~ "HQ and Project location",
      name %in% project_locations & !(name %in% HQ_locations) ~ "Project location Only",
      TRUE ~ "Other"
    )
  )

color_map <- c(
  "HQ and Project location" = "#916953",  
  "Project location Only"   = "#F6DC87",  
  "Other"                   = "gray95"
)

sample_map <- ggplot(data = world) +
  geom_sf(aes(fill = category), color = "white", size = 0.2) +
  scale_fill_manual(values = color_map, name = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank()
  )

print(sample_map)

ggsave(
  filename = paste0(file.path(path,"Visualizations/Sample/map"), ".svg"),
  plot = sample_map,
  width = 8, height = 2.5,
  device = svglite
)

# ------------------------------------------------------
# 4) Panel b and d: 100% Stacked bar charts
# ------------------------------------------------------

color_palette <- c(
  "DACCS & DOCCS" = "#E96900",
  "BECCS" = "#CA9B23",
  "Enhanced weathering" = "#C5373D",
  "Land-based biomass" = "#647314",
  "Marine biomass & Blue carbon" = "#006479"
)

# ------------------------------------------------------
# Panel b: Sample vs Population
# ------------------------------------------------------

all$dummy <- ifelse(!is.na(all$interview_ID), 1, 0)
all$group <- ifelse(all$dummy == 1, "Sample", "Population")

all_data <- all %>%
  mutate(group = "Population")

interview_data <- all %>%
  filter(dummy == 1) %>%
  mutate(group = "Sample")

plot_data <- bind_rows(all_data, interview_data)

bars <- ggplot(plot_data, aes(x = group, fill = `CDR Solution`)) +
  geom_bar(position = "fill", width = 0.5) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = color_palette) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

print(bars)

ggsave(
  filename = paste0(file.path(path,"Visualizations/Population/bar"), ".svg"),
  plot = bars,
  width = 2.5, height = 2,
  device = svglite
)

# ------------------------------------------------------
# Panel d: By project region and CDR solution (Sample)
# ------------------------------------------------------

df$project_region <- factor(df$project_region, levels = c("Canada", "United States", "Europe", "Other OECD", "Non-OECD"))

bar <- ggplot(df, aes(x = project_region, fill = CDR_solution_recode)) +
  geom_bar(position = "fill", width = 0.5) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = color_palette) +
  labs(
    x = "Project Region",
    y = "Percentage",
    fill = "CDR Method"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10,  color = "black"),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 10, color = "black")
  )

print(bar)

ggsave(
  filename = paste0(file.path(path,"Visualizations/Sample/bar"), ".svg"),
  plot = bar,
  width = 5, height = 2,
  device = svglite
)
