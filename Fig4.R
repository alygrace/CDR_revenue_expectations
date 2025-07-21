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
library(maps)
library(svglite)

# ------------------------------------------------------
# 2) Load and Recode Dataset
# ------------------------------------------------------

path <- ""

data <- read_csv(file.path(path,"processed/full_data.csv"), show_col_types = FALSE)

data_long <- data %>%
  select(!starts_with("Q") | matches("^Q2-\\d{4}\\s*_\\d+$")) %>%
  pivot_longer(
    cols = matches("^Q2-\\d{4}\\s*_\\d+$"),
    names_to = c("YEAR", "Market"),
    names_pattern = "Q2-(\\d{4})\\s*_(\\d+)",
    values_to = "Response"
  ) %>%
  mutate(
    YEAR = as.integer(YEAR),
    Measurement_Type = recode(Market,
                              "1" = "Government support",
                              "2" = "Compliance markets",
                              "3" = "Voluntary carbon markets"),
    Response = as.numeric(Response)
  ) %>%
  filter(!is.na(Measurement_Type)) %>%
  select(-Market) %>%
  pivot_wider(names_from = Measurement_Type, values_from = Response)

year_map <- c("1" = 2024, "2" = 2027, "3" = 2030, "4" = 2040, "5" = 2050)

data_long_comp <- data %>%
  select(!starts_with("Q") | matches("^Q3#\\d_\\d$")) %>%
  pivot_longer(
    cols = matches("^Q3#\\d_\\d$"),
    names_to = c("Code", "Z"),
    names_pattern = "Q3#(\\d)_(\\d)",
    values_to = "Response"
  ) %>%
  mutate(
    YEAR = as.integer(year_map[Code]),
    Measurement_Type = recode(Z,
                              "1" = "Plant level",
                              "2" = "Per ton"
    ),
    Response = as.numeric(Response)
  ) %>%
  filter(!is.na(Measurement_Type)) %>%
  select(-Code, -Z) %>%
  pivot_wider(names_from = Measurement_Type, values_from = Response)%>%
  select(YEAR, interview_ID, `Plant level`, `Per ton`)

merged_data <- left_join(data_long, data_long_comp, by = c("interview_ID", "YEAR"))

merged_data %>%
  distinct(interview_ID, TRL_upper_recode) %>%
  count(TRL_upper_recode)

# Overview Table (supplementary table)
merged_data %>%
  distinct(interview_ID, CDR_solution_recode, TRL_upper_recode) %>%  
  count(CDR_solution_recode, TRL_upper_recode) %>%                  
  arrange(CDR_solution_recode, TRL_upper_recode)

# ------------------------------------------------------
# 3) Panel a-c: Line charts
# ------------------------------------------------------

color_palette <- c("Per ton" = "#430B4E","Plant level" = "#B778B3")

classification <- "TRL_upper_recode"
class_label <- "Upper TRL Level"
government_support <- c("Plant level", "Per ton")
custom_order <- c("Laboratory scale", "Pilot plant scale", "Full scale & Commercial")


plot_line_by_group <- function(data, output_dir) {
  group_levels <- intersect(custom_order, unique(data[[classification]]))
  
  for (group in group_levels) {
    plot_data <- data %>%
      filter(.data[[classification]] == group) %>%
      select(YEAR, all_of(government_support)) %>%
      pivot_longer(cols = all_of(government_support),
                   names_to = "GovernmentSupport",
                   values_to = "Value") %>%
      mutate(YEAR = as.numeric(YEAR)) %>%
      group_by(YEAR, GovernmentSupport) %>%
      summarise(
        Mean = mean(Value, na.rm = TRUE),
        SEM = sd(Value, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      ) %>%
      mutate(
        CI = SEM * 1.645,
        Mean = pmin(pmax(Mean, 1), 4),
        ymin_clipped = pmax(Mean - CI, 1),
        ymax_clipped = pmin(Mean + CI, 4)
      )
    
    p <- ggplot(plot_data, aes(x = YEAR, y = Mean, color = GovernmentSupport, group = GovernmentSupport)) +
      geom_line(size = 0.5) +
      geom_point(size = 0.5) +
      geom_ribbon(aes(ymin = ymin_clipped, ymax = ymax_clipped, fill = GovernmentSupport),
                  alpha = 0.2, color = NA) +
      scale_color_manual(values = color_palette, name = NULL) +
      scale_fill_manual(values = color_palette, name = NULL) +
      guides(color = guide_legend(), fill = guide_legend()) +
      scale_x_continuous(
        breaks = c(2024, 2027, 2030, 2040, 2050),
        labels = c("2024", "'27", "'30", "2040", "2050"),
        limits = c(2023, 2051)
      ) +
      scale_y_continuous(breaks = c(1, 2, 3, 4), limits = c(0.99, 4.01)) +
      labs(title = NULL, x = NULL, y = NULL) +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    ggsave(
      filename = file.path(path, paste0("Visualizations/Revenue/TRL/Line_", group, ".svg")),
      plot = p,
      width = 2.8,
      height = 2,
      device = svglite
    )
  }
}

plot_line_by_group(merged_data, path)
