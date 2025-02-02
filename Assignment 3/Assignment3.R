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
library(waterfalls)
library(ggcorrplot)
library(visreg)
library(patchwork)
library(cowplot)
library(gridExtra)

# Read the CSV file
Attrition <- read_csv("/Users/wendiwang/Desktop/university/is 445/books2.csv")
par(mfrow=c(2,2))
layout(matrix(1:4, ncol=2, byrow = TRUE))

os <- Attrition %>% 
  filter(one_star_ratings < 1000) %>% 
  mutate(one_star = floor(one_star_ratings / 100) * 100)

ts <- Attrition %>% 
  filter(two_star_ratings < 1000) %>% 
  mutate(two_star = floor(two_star_ratings / 100) * 100)

ths <- Attrition %>% 
  filter(three_star_ratings < 1000) %>% 
  mutate(three_star = floor(three_star_ratings / 100) * 100)

fs <- Attrition %>% 
  filter(four_star_ratings < 1000) %>% 
  mutate(four_star = floor(four_star_ratings / 100) * 100)

fis <- Attrition %>% 
  filter(five_star_ratings < 1000) %>% 
  mutate(five_star = floor(five_star_ratings / 100) * 100)

bar_chart_1 <- ggplot(os, aes(x = as.factor(one_star))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of One Star Ratings",
    x = "One Star Ratings",
    y = "Number of Books"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

bar_chart_2 <- ggplot(ts, aes(x = as.factor(two_star))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Two Star Ratings",
    x = "Two Star Ratings",
    y = "Number of Books"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

bar_chart_3 <- ggplot(ths, aes(x = as.factor(three_star))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Three Star Ratings",
    x = "Three Star Ratings",
    y = "Number of Books"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

bar_chart_4 <- ggplot(fs, aes(x = as.factor(four_star))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Four Star Ratings",
    x = "Four Star Ratings",
    y = "Number of Books"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

bar_chart_5 <- ggplot(fis, aes(x = as.factor(five_star))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Five Star Ratings",
    x = "Five Star Ratings",
    y = "Number of Books"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

ggsave("Multi_Panel-0119.png", 
       plot=(bar_chart_1|bar_chart_2)/(bar_chart_3|bar_chart_4)/bar_chart_5)
