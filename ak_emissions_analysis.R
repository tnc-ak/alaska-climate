
# =========================================================
# Alaska GHG Emissions Analysis: Summary Stats + Stacked Plots
# Author: [Your Name]
# Date: [Today's Date]
# =========================================================

# --- Load Libraries ---
library(readxl)   # for reading Excel files
library(dplyr)    # for data wrangling
library(tidyr)    # for data reshaping
library(ggplot2)  # for plotting

# --- User Inputs ---
file_path <- "C:/path/to/AllStateGHGData.xlsx"   # Update this path
sheet_name <- "Data by Economic Sectors"        # Update if sheet name differs
state_code <- "AK"                               # Two-letter state code for Alaska

# === Load Data ===
data <- read_excel(file_path, sheet = sheet_name)

# === Filter for Alaska ===
ak_data <- data %>%
  filter(State == state_code)   # Replace 'State' with exact column name in your file

# === Function: Summarize & Plot ===
summarize_and_plot <- function(df, group_var, year_start, year_end) {
  
  df_filtered <- df %>%
    filter(Year >= year_start & Year <= year_end)
  
  # Summary statistics
  summary_stats <- df_filtered %>%
    group_by(across(all_of(group_var)), Year) %>%
    summarise(
      Mean_Emissions   = mean(Value, na.rm = TRUE),
      SD_Emissions     = sd(Value, na.rm = TRUE),
      Total_Emissions  = sum(Value, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Save summary table
  write.csv(
    summary_stats,
    paste0("AK_GHG_Summary_", gsub(" ", "_", group_var), "_", year_start, "_", year_end, ".csv"),
    row.names = FALSE
  )
  
  # Stacked bar plot
  p <- ggplot(df_filtered, aes(x = Year, y = Value, fill = .data[[group_var]])) +
    geom_bar(stat = "identity") +
    labs(
      title = paste("Alaska GHG Emissions by", group_var, "(", year_start, "-", year_end, ")"),
      x = "Year",
      y = "Emissions (MMTCO2e)",
      fill = group_var
    ) +
    theme_minimal()
  
  # Save plot
  ggsave(
    filename = paste0("AK_GHG_Stacked_", gsub(" ", "_", group_var), "_", year_start, "_", year_end, ".png"),
    plot = p, width = 10, height = 6
  )
  
  return(list(summary = summary_stats, plot = p))
}

# === Define periods ===
periods <- list(
  c(1990, 1999),
  c(2000, 2004),
  c(2005, 2010),
  c(2011, 2015),
  c(2016, 2022)
)

# === Run for Economic Sector ===
for (period in periods) {
  summarize_and_plot(ak_data, "Economic Sector", period[1], period[2])
}

# === Run for Sub-Sector (if column exists) ===
if ("Sub-Sector" %in% names(ak_data)) {
  for (period in periods) {
    summarize_and_plot(ak_data, "Sub-Sector", period[1], period[2])
  }
}

# =========================================================
# End of Script
# =========================================================
