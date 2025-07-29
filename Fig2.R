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
library(PMCMRplus)
library(svglite)

# ------------------------------------------------------
# 2) Load and Recode Dataset
# ------------------------------------------------------

path <- ""

data <- read_csv(file.path(path, "data.csv"), show_col_types = FALSE)

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

merged_data %>%
  distinct(interview_ID, CDR_solution_recode) %>%
  count(CDR_solution_recode)

# ------------------------------------------------------
# 3) Panel a-j: Line Charts
# ------------------------------------------------------

color_palette_1 <- c(
  "Compliance markets" = "#429130",
  "Government support" = "#A54891",
  "Voluntary carbon markets" = "#006EAE"
)

color_palette_2 <- c(
  "Cap and Trade" = "#0E3716",
  "International Climate Schemes" = "#5EB342",
  "Portfolio Standards" = "#70DE64"
)

classification <- "CDR_solution_recode"
class_label <- "CDR Solution"
revenue_streams <- c("Compliance markets","Government support", "Voluntary carbon markets")
compliance_markets <- c("Cap and Trade", "International Climate Schemes", "Portfolio Standards")
custom_order <- c("DACCS & DOCCS", "BECCS", "Enhanced weathering", "Land-based biomass", "Marine biomass & Blue carbon")

# Line plot for a single group
make_line_plot <- function(data, group_name, show_y = FALSE) {
  plot_data <- data %>%
    filter(.data[[classification]] == group_name) %>%
    select(YEAR, all_of(revenue_streams)) %>%
    pivot_longer(cols = all_of(revenue_streams),
                 names_to = "RevenueStream",
                 values_to = "Value") %>%
    mutate(YEAR = as.numeric(YEAR)) %>%
    group_by(YEAR, RevenueStream) %>%
    summarise(
      Mean = mean(Value, na.rm = TRUE),
      SEM = sd(Value, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(CI = SEM * 1.645) %>%
    mutate(
      Mean = pmin(pmax(Mean, 1), 3),
      ymin_clipped = pmax(Mean - CI, 1),
      ymax_clipped = pmin(Mean + CI, 3)
    )
  
  ggplot(plot_data, aes(x = YEAR, y = Mean, color = RevenueStream, group = RevenueStream)) +
    geom_line(size =0.5) +
    geom_point(size = 0.5) +
    geom_ribbon(aes(ymin = ymin_clipped, ymax = ymax_clipped, fill = RevenueStream), alpha = 0.2, color = NA) +
    scale_color_manual(values = color_palette_1, name =  NULL) +
    scale_fill_manual(values = color_palette_1, name = NULL) +
    guides(color = guide_legend(), fill = guide_legend()) +
    scale_x_continuous(
      breaks = c(2024, 2027, 2030, 2040, 2050),
      labels = c("2024", "'27", "'30", "2040", "2050"),
      limits = c(2023, 2051)
    ) +
    scale_y_continuous(
      breaks = c(3, 2, 1),
      limits = c(3, 1),
      trans = "reverse"
    )+
    labs(title = NULL,
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 10,  color = "black"),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 10, color = "black")
    )
}

# Line plot for a single group focus compliance markets
make_line_plot_2 <- function(data, group_name, show_y = FALSE) {
  plot_data <- data %>%
    filter(.data[[classification]] == group_name) %>%
    select(YEAR, all_of(compliance_markets)) %>%
    pivot_longer(cols = all_of(compliance_markets),
                 names_to = "ComplianceMarkets",
                 values_to = "Value") %>%
    mutate(YEAR = as.numeric(YEAR)) %>%
    group_by(YEAR, ComplianceMarkets) %>%
    summarise(
      Mean = mean(Value, na.rm = TRUE),
      SEM = sd(Value, na.rm = TRUE) / sqrt(n()),
      .groups = "drop",
    ) %>%
    mutate(CI = SEM * 1.645) %>%
    mutate(
      Mean = pmin(pmax(Mean, 1), 4),
      ymin_clipped = pmax(Mean - CI, 1),
      ymax_clipped = pmin(Mean + CI, 4)
    )
  
  ggplot(plot_data, aes(x = YEAR, y = Mean, color = ComplianceMarkets, group = ComplianceMarkets)) +
    geom_line(size =0.5) +
    geom_point(size = 0.5) +
    geom_ribbon(aes(ymin = ymin_clipped, ymax = ymax_clipped, fill = ComplianceMarkets), alpha = 0.2, color = NA) +
    scale_color_manual(values = color_palette_2, name = NULL) +
    scale_fill_manual(values = color_palette_2, name = NULL) +
    guides(color = guide_legend(), fill = guide_legend()) + 
    scale_x_continuous(
      breaks = c(2024, 2027, 2030, 2040, 2050),
      labels = c("2024", "'27", "'30", "2040", "2050"),
      limits = c(2023, 2051)
    ) +
    scale_y_continuous(breaks = c(1,2,3,4),
                       limits = c(0.99, 4.01))+
    labs(title = NULL,
         x = NULL,
         y = NULL) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 10, color = "black"),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 10, color = "black")
    )
}

# Combine plots per CDR solution
plot_combined_trend <- function(data, path) {
  group_levels <- intersect(custom_order, unique(data[[classification]]))
  
  for (i in seq_along(group_levels)) {
    group <- group_levels[i]
    
    # Create individual plots
    p1 <- make_line_plot(data, group) 
    
    p2 <- make_line_plot_2(data, group) 
    
    spacer <- patchwork::plot_spacer() + plot_layout(heights = 1)
    
    # Combine plots
    combined_panel <- (p1 + spacer + p2) +
      plot_layout(widths = c(4.5,0.25,4.5)) &
      theme(
        plot.title       = element_blank(),
        panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.2),
        panel.spacing    = unit(10, "pt"),
        plot.background  = element_rect(fill = "transparent", color = NA)  # transparent outer background
      )
    
  
    ggsave(
      filename = file.path(path, paste0("Visualizations/Revenue/Solution/No_Bar_", group, ".svg")),
      plot = combined_panel,
      width = 6,
      height = 2,
      device = svglite
    )
  }
}

plot_combined_trend(merged_data, path)


