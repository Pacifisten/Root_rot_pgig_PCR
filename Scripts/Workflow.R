library(openxlsx)
library(tidyverse)
library(lubridate)
# Sets the color scale for the project
Color_scale <- c("#ff7f00", "#377eb8", "#4daf4a")

# Reads in the data using the excel file
source("Scripts/Read_in_v1.R")

# Visualisation and graphs
source("Scripts/Visualisation.R")

ggsave(plot = Barplot_coverage_pos_PCR, "Output/Figure_1.jpg", units = "mm", height = 120, width = 240, dpi = 600, bg = "white")
ggsave(plot = Barplot_coverage_pos_PCR, "Output/Figure_1.pdf", units = "mm", height = 120, width = 214, bg = "white")

ggsave(plot = Spread_of_coverage, filename = "Output/Figure_2.jpg", height = 100, width = 180, units = "mm", dpi = 600, bg = "white")
ggsave(plot = Spread_of_coverage, filename = "Output/Figure_2.pdf", height = 100, width = 180, units = "mm", bg = "white")

ggsave(plot = Both_graphs, filename = "Output/Figure_3.jpg", height = 100, width = 290, units = "mm", dpi = 600, bg = "white")
ggsave(plot = Both_graphs, filename = "Output/Figure_3.pdf", height = 100, width = 240, units = "mm", bg = "white")

# Produces table
source("Scripts/Table_calc.R")

# Saves table
write.xlsx(x = Table, file = "Output/table.xlsx")

# Weather data
source("Scripts/Weather_data.R")
ggsave(plot = Plot_both_years, filename = "Output/Figure_S1.jpg", units = "mm", width = 210, height = 160)
ggsave(plot = Plot_both_years, filename = "Output/Figure_S1.pdf", units = "mm", width = 210, height = 160)
