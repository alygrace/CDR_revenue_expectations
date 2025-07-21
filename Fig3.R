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
library(svglite)

# ------------------------------------------------------
# 2) Load and Recode Dataset
# ------------------------------------------------------

path <- ""

data <- read_csv(file.path(path, "processed/full_data.csv"), show_col_types = FALSE)

ets <- read_delim(file.path(path, "RAW/ETS_data.csv"),
                delim = ",", show_col_types = FALSE)

eu <- read_csv(file.path(path,"RAW/EUA prices 2024.csv"), show_col_types = FALSE)

# ------------------------------------------------------
# 3) Panel a: Historic ETS prices
# ------------------------------------------------------

scope <- c('EU ETS', 'UK ETS', 'New Zealand ETS', 'China national ETS', 'California CaT', 'Tokyo CaT')

etsf <- ets %>% filter(`Name of the initiative` %in% scope)

etsf <- etsf %>%
  mutate(
    `2022` = `2022` * 1.072,
    `2023` = `2023` * 1.029
  )

ets_long <- etsf %>%
  pivot_longer(cols = c('2022', '2023', '2024'), names_to = "year", values_to = "value") %>%
  mutate(
    year = as.numeric(year),
    x_scaled = case_when(
      year == 2022 ~ 0.6,
      year == 2023 ~ 0.8,
      year == 2024 ~ 0.95
    ),
    color_group = ifelse(`Name of the initiative` == "EU ETS", "EU ETS", "Other"),
    color_group = factor(color_group, levels = c("EU ETS", "Other"))
  )

ets_plot <- ggplot() +
  geom_line(
    data = subset(ets_long, `Name of the initiative` != "EU ETS"),
    aes(
      x = x_scaled,
      y = value,
      group = `Name of the initiative`
    ),
    color = "black",
    linewidth = 0.4
  ) +
  geom_line(
    data = subset(ets_long, `Name of the initiative` == "EU ETS"),
    aes(
      x = x_scaled,
      y = value,
      group = `Name of the initiative`
    ),
    color = "black",
    linewidth = 0.8
  ) +
  geom_text_repel(
    data = subset(etsf, `Name of the initiative` != "EU ETS"),
    aes(x = 0.6, y = `2022`, label = `Name of the initiative`),
    direction = "y",
    hjust = 1,
    nudge_x = -0.05,
    segment.size = 0.3,
    segment.color = "gray40",
    box.padding = 0.2,
    point.padding = 0.1,
    force = 1,
    max.overlaps = Inf,
    size = 10 / .pt
  ) +
  geom_text_repel(
    data = subset(etsf, `Name of the initiative` == "EU ETS"),
    aes(x = 0.6, y = `2022`, label = `Name of the initiative`),
    direction = "y",
    hjust = 1,
    nudge_x = -0.05,
    segment.size = 0.3,
    segment.color = "gray40",
    box.padding = 0.2,
    point.padding = 0.1,
    force = 1,
    max.overlaps = Inf,
    size = 10 / .pt,
    fontface = "bold"
  ) +
  scale_y_continuous(limits = c(0, 400)) +
  coord_cartesian(xlim = c(-0.5, 1)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(y = NULL)

print(ets_plot)

ggsave(
  filename = paste0(file.path(path, "Visualizations/ETS/ets"), ".svg"),
  plot = ets_plot,
  width = 2, height = 5,
  device = svglite
)

# ------------------------------------------------------
# 4) Panel b: EU ETS price forecast
# ------------------------------------------------------

eu <- eu %>%
  mutate(
    x_center = 0.5,
    x_left = 0.1,
    x_right = 0.75,
    y_mid = `2030`,
    y_min = 135,
    y_max = 226,
    source_type = if_else(
      str_detect(Source, regex("academic", ignore_case = TRUE)),
      "academic",
      "other"
    )
  )

eu_plot <- ggplot(eu) +
  geom_rect(aes(xmin = x_left, xmax = x_right, ymin = y_min, ymax = y_max),
            fill = "gray80") +
  geom_segment(aes(
    x = x_left, xend = x_right,
    y = y_mid, yend = y_mid,
    linetype = source_type  # mapped to a variable
  ),
  color = "black", 
  linewidth = 1,
  show.legend = FALSE
  ) +
  geom_text_repel(
    aes(x = x_right, y = y_mid, label = `Name of the initiative`),
    direction = "y",
    hjust = 0,
    nudge_x = 0.05,
    segment.size = 0.3,
    segment.color = "black",
    box.padding = 0.1,
    point.padding = 0.05,
    force = 1,
    max.overlaps = Inf,
    size = 10 / .pt 
  ) +
  scale_linetype_manual(values = c(
    "academic" = "dotted",  
    "other" = "dashed"    
  )) +
  coord_cartesian(xlim = c(0, 2)) +
  scale_y_continuous(limits = c(0, 400)) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(eu_plot)

ggsave(
  filename = paste0(file.path(path,"Visualizations/ETS/eu"), ".svg"),
  plot = eu_plot,
  width = 2, height = 5,
  device = svglite
)

# ------------------------------------------------------
# 5) Panel c: Price expectations
# ------------------------------------------------------

data_subset <- data %>% filter(!is.na(ETS_Price_2030_dollar))

data_subset <- data_subset %>% 
  separate_rows(`ETS_Price_2030_dollar`, sep = ",\\s*")

data_subset$ETS_Price_2030_dollar <- as.numeric(data_subset$ETS_Price_2030_dollar)

#drop expectations if unclear with ETS 

data_subset <- data_subset %>%
  filter(!interview_ID %in% c("J05", "J13", "J17", "J20", "J25"))

cdr_colors <- c(
  "Land-based biomass" = "#647314",
  "DACCS & DOCCS" = "#E96900",
  "Enhanced weathering" = "#C5373D",
  "BECCS" = "#CA9B23",
  "Marine biomass & Blue carbon" = "#006479"
)

exp <- ggplot(data_subset, aes(x = "", y = ETS_Price_2030_dollar)) +
  geom_violin(fill = "gray70", color = "white", width = 0.3, trim = TRUE) +
  geom_boxplot(width = 0.1, fill = "black", outlier.shape = NA, color = "white") +
  geom_jitter(aes(color = CDR_solution_recode), 
              width = 0.15, height = 0, size = 3.5, alpha = 0.8) +
  scale_color_manual(values = cdr_colors) +
  coord_cartesian(ylim = c(0, 400)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),           
    axis.text.x = element_blank(),             
    axis.ticks.x = element_blank(),  
    axis.text.y = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) + 
  labs(y = NULL)

print(exp)

ggsave(
  filename = paste0(file.path(path,"Visualizations/ETS/exp"), ".svg"),
  plot = exp,
  width = 2, height = 5,
  device = svglite
)
