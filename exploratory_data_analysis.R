# Load packages
library(tidyverse)

# Set up the file we'll be using

ds <- read_csv("data-raw/raw_data.csv")
demo <- readxl::read_excel("data-raw/participant_demo.xlsx", sheet = "Data") %>% rename_all(janitor::make_clean_names)
ds <- ds %>% pivot_longer(-id, names_to = "variable", values_to = "value") %>% 
  mutate(condition = ifelse(str_detect(variable, "walk"), "walk", "search"),
         variable = str_remove_all(variable, "walk_"),
         variable = str_remove_all(variable, "search_"))
ds <- ds %>% pivot_wider(id_cols = c("id", "condition"), names_from = "variable", values_from = "value")
ds <- left_join(ds, demo, by = c("id" = "subject_number"))
ds <- ds %>% relocate(test_date:humidity, .after = condition)

# Example data for today

glimpse(ds)

# What type of aesthetic mapping depends on what type of geom

ggplot(ds, aes(x = posy_std)) + 
  geom_histogram() + 
  xlab("Average vertical head position")

ggplot(ds, aes(x = posy_std)) + 
  geom_density() + 
  xlab("Average vertical head position")

ggplot(ds, aes(x = posy_std)) + 
  geom_boxplot() + 
  xlab("Average vertical head position")

# Add a category

ggplot(ds) + geom_boxplot(aes(y = posy_std)) 
ggplot(ds) + geom_boxplot(aes(x = condition, y = posy_std))
ggplot(ds) + geom_boxplot(aes(x = condition, y = posy_std, fill = condition))
ggplot(ds) + geom_boxplot(aes(x = condition, y = posy_std, fill = target)) 

# Simple scatterplot

ggplot(ds, aes(x = posx_mean, y = posy_mean)) + 
  geom_point() + 
  xlim(-20, 20) + 
  ylim(-20, 20) + 
  xlab("Mean horizontal head position") + 
  ylab("Mean vertical head position") +
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0)

# Add category to scatterplot

ggplot(ds, aes(x = posx_mean, y = posy_mean, color = condition)) + 
  geom_point()

# Facetting a scatterplot

ggplot(ds, aes(x = posx_mean, y = posy_mean)) + 
  geom_point() + 
  facet_wrap("condition", ncol = 1)

# Saving plots

dir.create("eda") 
ggplot(ds, aes(x = posx_mean, y = posy_mean)) + geom_point()
#ggsave will save the last plot to file
ggsave("eda/head-position-scatter.jpg")

#You can also save plots to objects (they are lists)
p1 <- ggplot(ds, aes(x = posx_mean, y = posy_mean)) + geom_point()
ggsave("eda/head-position-scatter.jpg", plot = p1)

