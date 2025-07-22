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

# ------------------------------------------------------
# 2) Load and Recode Dataset
# ------------------------------------------------------

path <- ""

data <- read_csv(file.path(path,"data.csv"), show_col_types = FALSE)

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
  select(!starts_with("Q") | matches("^Q5#\\d_\\d$")) %>%
  pivot_longer(
    cols = matches("^Q5#\\d_\\d$"),
    names_to = c("Code", "Z"),
    names_pattern = "Q5#(\\d)_(\\d)",
    values_to = "Response"
  ) %>%
  mutate(
    YEAR = as.integer(year_map[Code]),
    Measurement_Type = recode(Z,
                              "1" = "Cap and Trade",
                              "2" = "Portfolio Standards",
                              "3" = "International Climate Schemes"
    ),
    Response = as.numeric(Response)
  ) %>%
  filter(!is.na(Measurement_Type)) %>%
  select(-Code, -Z) %>%
  pivot_wider(names_from = Measurement_Type, values_from = Response)%>%
  select(YEAR, interview_ID, `Cap and Trade`, `Portfolio Standards`, `International Climate Schemes`)

merged_data <- left_join(data_long, data_long_comp, by = c("interview_ID", "YEAR"))


# ------------------------------------------------------
# 3) Supplementary Figure 1
# ------------------------------------------------------

color_palette_1 <- c(
  "Compliance markets" = "#429130",
  "Government support" = "#A54891",
  "Voluntary carbon markets" = "#006EAE")

color_palette_2 <- c(
  "4" = "#0E3716",
  "3" = "#346D34",
  "2" = "#5EB342",
  "1" = "#A0CA78")

classification <- "CDR_solution_recode"
class_label <- "CDR Solution"
revenue_streams <- c("Compliance markets", "Government support", "Voluntary carbon markets")
cap <- c("Cap and Trade")
scheme <- c("International Climate Schemes")
portfolio <- c("Portfolio Standards")
custom_order <- c(
  "DACCS & DOCCS",
  "BECCS",
  "Enhanced weathering",
  "Land-based biomass",
  "Marine biomass & Blue carbon"
)

# --- PLOT FUNCTION 1: REVENUE STREAM BAR CHART ---
make_bar_plot <- function(data, group_name, show_y = FALSE) {
  plot_data <- data %>%
    filter(.data[[classification]] == group_name) %>%
    select(YEAR, all_of(revenue_streams)) %>%
    pivot_longer(cols = all_of(revenue_streams),
                 names_to = "RevenueStream",
                 values_to = "Value") %>%
    filter(Value == 1) %>%
    mutate(
      YEAR = as.numeric(YEAR),
      RevenueStream = factor(RevenueStream, levels = names(color_palette_1))
    ) %>%
    group_by(YEAR, RevenueStream) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(YEAR) %>%
    mutate(Proportion = Count / sum(Count)) %>%
    ungroup()
  
  ggplot(plot_data, aes(x = YEAR, y = Proportion, fill = RevenueStream)) +
    geom_bar(stat = "identity", position = "fill", width = 2) +
    scale_fill_manual(values = color_palette_1, name = NULL) +
    scale_y_continuous(breaks = c(0, 0.5, 1), labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(breaks = c(2024, 2027, 2030, 2040, 2050),
                       labels = c("'24", "'27", "'30", "2040", "2050"),
                       limits = c(2023, 2051)) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_blank(),
      legend.position = "bottom",
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 10, color = "black"),
      axis.ticks.x = element_line(color = "black"),
      axis.line.x = element_line(color = "black"),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 10, color = "black")
    )
}

# --- PLOT FUNCTION 2: VALUE COUNT BY YEAR & STREAM TYPE ---
make_value_count_bar_plot <- function(data, group_name, stream_group, show_y = FALSE) {
  plot_data <- data %>%
    filter(.data[[classification]] == group_name) %>%
    select(YEAR, all_of(stream_group)) %>%
    pivot_longer(cols = all_of(stream_group),
                 names_to = "RevenueStream",
                 values_to = "Value") %>%
    filter(Value %in% c(1, 2, 3, 4)) %>%
    mutate(
      YEAR = as.numeric(YEAR),
      Value = factor(Value, levels = c(1, 2, 3, 4))
    ) %>%
    group_by(YEAR, Value) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(YEAR) %>%
    mutate(Proportion = Count / sum(Count)) %>%
    ungroup()
  
  ggplot(plot_data, aes(x = YEAR, y = Proportion, fill = Value)) +
    geom_bar(stat = "identity", position = "fill", width = 2) +
    scale_fill_manual(values = color_palette_2, name = NULL) +
    scale_y_continuous(breaks = c(0, 0.5, 1), labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(breaks = c(2024, 2027, 2030, 2040, 2050),
                       labels = c("'24", "'27", "'30", "2040", "2050"),
                       limits = c(2023, 2051)) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_blank(),
      legend.position = "bottom",
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 10, color = "black"),
      axis.ticks.x = element_line(color = "black"),
      axis.line.x = element_line(color = "black"),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 10, color = "black")
    )
}

# --- COMBINE ALL PLOTS PER CDR GROUP ---
plot_combined_trend <- function(data, path) {
  group_levels <- intersect(custom_order, unique(data[[classification]]))
  
  for (group in group_levels) {
    p1 <- make_bar_plot(data, group)
    
    p2 <- make_value_count_bar_plot(data, group, cap) 
    
    p3 <- make_value_count_bar_plot(data, group, scheme) 
    
    p4 <- make_value_count_bar_plot(data, group, portfolio) 
    
    spacer <- patchwork::plot_spacer() + plot_layout(heights = 1)
    
    combined_panel <- (p1 / spacer / p2 / spacer / p3 / spacer / p4) +
      patchwork::plot_layout(heights = c(5, 1, 5, 1, 5, 1, 5)) &
      theme(
        plot.title       = element_blank(),
        panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.2),
        panel.spacing    = unit(10, "pt"),
        plot.background  = element_rect(fill = "transparent", color = NA),
        legend.position  = "none"
      )
    
    ggsave(
      filename = file.path(path, paste0("Visualizations/SI/SI_bar_", group, ".svg")),
      plot = combined_panel,
      width = 2.5, height = 4,
      device = svglite
    )
  }
}

plot_combined_trend(merged_data, path)

# ------------------------------------------------------
# 4) Supplementary Table 2
# ------------------------------------------------------

# Revenue Streams
data_long <- merged_data %>%
  pivot_longer(
    cols = c("Government support", "Compliance markets", "Voluntary carbon markets"),
    names_to = "item",
    values_to = "rank"
  )

data_long %>%
  group_by(YEAR) %>%
  group_modify(~ {
    test_result <- friedman.test(rank ~ item | interview_ID, data = .x)
    tibble(
      statistic = test_result$statistic,
      df = test_result$parameter,
      p_value = round(test_result$p.value, 4)
    )
  })

