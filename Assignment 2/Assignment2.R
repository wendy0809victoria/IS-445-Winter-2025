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

# Read the CSV file
Attrition <- read_csv("/Users/wendiwang/Desktop/university/is 445/books2.csv")
Attrition_subset <- read_excel("/Users/wendiwang/Desktop/university/is 445/books2-subset.xlsx", sheet = "books2-subset")
df <- Attrition %>% 
  select(rating_count, review_count, average_rating, number_of_pages) 

print(Attrition_subset)

fig_num <- 1

title <- Attrition %>%
  filter(title %in% c("Inner Circle", "A Time to Embrace",
                      "Reliquary",
                      "Sylvester",
                      "Seven Years to Sin",
"Black Sheep")) %>%
  select(title, average_rating)

wf <- waterfall(title,  
          calc_total=TRUE, 
          total_axis_text = "Net",
          total_rect_text_color="black",
          total_rect_color="goldenrod1",
          rect_text_size = 6) +
  labs(title = "Average Ratings of 6 Publications and Books", 
       subtitle = "From Goodreads API",
       y="Average Rating", 
       x="Book/Publication Title") +
  theme_minimal() +
  theme(
    text = element_text(size = 30),  
    axis.text = element_text(size = 26),  
    axis.title = element_text(size = 28),  
    plot.title = element_text(size = 34, face = "bold"),  
    plot.subtitle = element_text(size = 30),  
    plot.caption = element_text(size = 26),
    axis.text.x = element_text(angle = 45, hjust = 1),
  )
ggsave("example-0113-waterfall.png", plot=wf)

plotdata <- Attrition %>%
  filter(title %in% c("Inner Circle", "A Time to Embrace", "Take Two")) %>%
  select(title, five_star_ratings, four_star_ratings, 
         three_star_ratings, two_star_ratings, one_star_ratings) %>%
  rename(group = title) %>%
  mutate_at(vars(-group),
            funs(rescale))
radar <- ggradar(plotdata, 
                 grid.label.size = 9,
                 axis.label.size = 9, 
                 group.point.size = 14,
                 group.line.width = 3,
                 legend.text.size = 30) +
  labs(title = "Inner Circle, A Time to Embrace and Take Two") +
  theme(plot.title = element_text(size = 40))
ggsave("example-0113-radar.png", plot=radar)

png("example-0113-heatmap.png", width = 2000, height = 1000)
superheat(sample_frac(Attrition, 0.0001, replace = FALSE), 
          scale = TRUE,
          title = "Heatmap of Selected Data",
          title.size = 20,
          bottom.label.text.angle = 45,
          bottom.label.text.size = 8)
dev.off()

# create a scatterplot matrix
plot <- ggpairs(df, upper = list(continuous = wrap("cor", size = 4, digits = 3))) +
  labs(title = "Scatterplot Matrix of Ratings, Reviews, and Pages",
       subtitle = "Various publication and book information from Goodreads API") + 
  ggtitle(paste("Figure", fig_num, ": Scatterplot Matrix of Ratings, Reviews, and Pages")) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("example-0113-scatterplot.png", plot=plot)

fig_num <- 2

bubble <- ggplot(Attrition, 
                 aes(x = rating_count, y = review_count, size = one_star_ratings)) +
  geom_point(alpha = .5, 
             fill="cornflowerblue", 
             color="black", 
             shape=21) +
  scale_size_continuous(range = c(1, 14)) +
  labs(title = "Bubble Chart of Ratings and Reviews",
       subtitle = "Various publication and book information from Goodreads API",
       x = "Rating Count",
       y = "Review Count",
       size = "One Star Rating Count") + 
  ggtitle(paste("Figure", fig_num, ": Bubble Chart of Ratings and Reviews")) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("example-0113-bubble.png", plot=bubble)
