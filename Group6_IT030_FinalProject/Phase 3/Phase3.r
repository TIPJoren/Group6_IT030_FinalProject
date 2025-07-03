# Load them every time
library(tidyverse)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)

# Replace this with your actual CSV file name
cyber_data <- read.csv("C:/Users/Joren Hanz/Documents/cleaned_DA_FinalProj.csv")

# View the first few rows
head(cyber_data)

ggplot(cyber_data, aes(x = Attack.Type)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Most Common Cyberattack Types",
       x = "Attack Type",
       y = "Count")

cyber_data %>%
  group_by(Target.Industry) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(Target.Industry, Total), y = Total)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Targeted Industries",
       x = "Industry",
       y = "Number of Attacks")

ggplot(cyber_data, aes(x = Year)) +
  geom_bar(fill = "darkgreen") +
  theme_minimal() +
  labs(title = "Cyber Attacks Over the Years",
       x = "Year",
       y = "Number of Attacks")
