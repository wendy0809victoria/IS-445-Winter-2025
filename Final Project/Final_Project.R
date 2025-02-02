library(readr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(VIM)
library(GGally)
library(dplyr)
library(tidyverse)
library(superheat)
library(scales)
library(ggradar)
library(ggrepel)
library(waterfalls)

Attrition <- read_csv("/Users/wendiwang/Downloads/Traffic_Violations-2.csv")
# Attrition <- sample_frac(Attrition, 0.05, replace = FALSE)
df <- sample_frac(Attrition, 0.005, replace = FALSE)
print(df)
print(colnames(Attrition))

plot <- scatter_plot <- ggplot(df, aes(x = Latitude, y = Longitude)) +  
  geom_point(color = "red", size = 3, alpha = 0.7) +  
  scale_x_continuous(
    breaks = seq(38.7, 39.9, by = 0.05)
  ) +
  scale_y_continuous(
    breaks = seq(-77.4, -68.8, by = 0.05)
  ) +
  labs(title = "Distribution of Violation Locations", x = "Latitude", y = "Longitude")
# ggsave("dv-0113-2.png", plot=plot)

plot <- scatter_plot <- ggplot(df, aes(x = Latitude, y = Longitude, color = `Violation Type`)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("Citation" = "red", "Warning" = "blue", "ESERO" = "green", "SERO" = "orange")) +  
  scale_x_continuous(
    breaks = seq(38.7, 39.9, by = 0.05)
  ) +
  scale_y_continuous(
    breaks = seq(-77.4, -68.8, by = 0.05)
  ) +
  labs(title = "Relationship between Violation Locations and Violation Types", x = "Latitude", y = "Longitude", color = "Violation Type") +
  theme_minimal()
# ggsave("dv-0113-3.png", plot=plot)

cat_cat <- ggplot(Attrition, aes(x = `Violation Type`, fill = `Contributed To Accident`)) + 
  geom_bar(position = "stack", width = 0.3) +
  theme(
    plot.title = element_text(size = 24),       # Title font size
    axis.title.x = element_text(size = 20),    # X-axis title font size
    axis.title.y = element_text(size = 20),    # Y-axis title font size
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),  # X-axis text size and angle
    axis.text.y = element_text(size = 16),     # Y-axis text size
    legend.title = element_text(size = 18),    # Legend title font size
    legend.text = element_text(size = 16)      # Legend text font size
  ) +
  scale_y_continuous(
    breaks = seq(0, 2500, by = 200)  # Set Y-axis breaks
  ) +
  labs(
    title = "Relationship between Violation Types and Accidents", 
    x = "Violation Type", 
    y = "Count"
  ) +
  theme_minimal()

cat_cat <- ggplot(Attrition, aes(x = `Violation Type`, fill = `Contributed To Accident`)) + 
  geom_bar(position = "dodge", width = 0.7) +  
  theme(
    plot.title = element_text(size = 24),       
    axis.title.x = element_text(size = 20),    
    axis.title.y = element_text(size = 20),    
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 16),     
    legend.title = element_text(size = 18),    
    legend.text = element_text(size = 16)     
  ) +
  scale_y_continuous(
    breaks = seq(0, 2500, by = 200)  
  ) +
  labs(
    title = "Relationship between Violation Types and Accidents", 
    x = "Violation Type", 
    y = "Count"
  ) +
  theme_minimal()
ggsave("2-0120.png", plot = cat_cat, width = 10, height = 8)
# Save the plot
# ggsave("dv-0113-4.png", plot = cat_cat, width = 10, height = 8)