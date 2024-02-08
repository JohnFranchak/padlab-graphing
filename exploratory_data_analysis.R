# Load packages
library(tidyverse)
library(ggforce)

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

ggplot(ds) + 
  geom_point(aes(x = posx_mean, y = posy_mean, color = condition)) + 
  geom_smooth(aes(x = posx_mean, y = posy_mean, color = condition), method = "lm", se = F) + 
  geom_smooth(aes(x = posx_mean, y = posy_mean), method = "lm", se = F, color = "black")

# Facetting a scatterplot

ggplot(ds, aes(x = posx_mean, y = posy_mean)) + 
  geom_point() + 
  facet_wrap("condition", nrow = 1)

# Saving plots

dir.create("eda") 
ggplot(ds, aes(x = posx_mean, y = posy_mean)) + geom_point()
#ggsave will save the last plot to file
ggsave("eda/head-position-scatter.jpg", width = 5, height = 5, units = "in")

#You can also save plots to objects (they are lists)
(p1 <- ggplot(ds, aes(x = posx_mean, y = posy_mean)) + geom_point())
ggsave("eda/head-position-scatter.jpg", plot = p1)

# Graphing raw data and summaries

ggplot(ds, aes(x = condition, y = gazey_std)) +
  geom_sina(scale = "width", maxwidth = .25, size = 3, alpha = .75, color = "gray") + 
  stat_summary(color = "black", geom = "point", linewidth = .75, shape = "—", size = 8)  +
  ylim = c(10,30)

ggplot(ds, aes(x = condition, y = gazey_std)) +
  stat_summary(color = "black", geom = "point", linewidth = .75, size = 3)  +
  stat_summary(color = "black", geom = "errorbar", linewidth = .75, width = .2)  +
  coord_cartesian(ylim = c(0,30))

# Fix the theme and rerun the graph

theme_update(text = element_text(size = 12),
   axis.text.x = element_text(size = 12, color = "black"), 
   axis.title.x = element_text(size = 14),
   axis.text.y = element_text(size = 12,  color = "black"), 
   axis.title.y = element_text(size = 14), 
   panel.background = element_blank(),panel.border = element_blank(), 
   panel.grid.major = element_blank(),
   panel.grid.minor = element_blank(), axis.line = element_blank(), 
   axis.ticks.length=unit(.25, "cm"), 
   legend.key = element_rect(fill = "white")) 

ggplot(ds, aes(x = condition, y = gazey_std)) +
  geom_sina(scale = "width", maxwidth = .25, size = 3, alpha = .75, color = "gray") + 
  stat_summary(color = "black", geom = "errorbar", linewidth = .75, width = .2)  +
  coord_cartesian(ylim = c(0,30))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(ds, aes(x = condition, y = gazey_std, color = condition)) +
  geom_sina(scale = "width", maxwidth = .25, size = 3, alpha = .25) + 
  scale_color_manual(values = c("#E69F00", "#56B4E9")) + 
  stat_summary(color = "black", geom = "errorbar", linewidth = .75, width = .2)  +
  coord_cartesian(ylim = c(0,30))
