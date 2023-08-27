library(tidyverse)
library(ggplot2)

#Load biomass_cox.xlsx then do this
total_weights <- sum(biomass$initial_weight)
View(total_weights)


decomp_notfull <- biomass %>% 
  filter(biomass_loss != 7)

decomp_full <- biomass %>% 
  filter(biomass_loss == 7)

decomp_wk4 <- barplot_data2 %>% filter(sample_time == "Week 4")

#load scav_data from the permanova file

n_fly <- scav_data %>% 
  group_by(type, sample_time) %>% 
  summarise(mean_n = round(mean(fly_n), digits = 0),
            se = round(sd(fly_n)/sqrt(length(fly_n)), digits = 0)) %>% 
  ungroup()

barplot_fly <- ggplot(n_fly,
                      aes(type, mean_n,
                          fill = sample_time)) +
  geom_col(position = position_dodge(), col = "black")+
  geom_errorbar(aes(ymin = mean_n - se, ymax = mean_n + se), position = position_dodge2(width = 0.2, padding = 0.8)) + 
  labs(title = "",
       x = "",
       y = "Mean fly number per carcass (n)",
       fill = "Sampling period")+
  # coord_flip()+
  theme_classic()
barplot_fly

n_beetle <- scav_data %>% 
  group_by(type, sample_time) %>% 
  summarise(mean_n = round(mean(beetle_n), digits = 0), se = round(sd(beetle_n)/sqrt(length(beetle_n)), digits = 0)) %>% 
  ungroup()

barplot_beetle <- ggplot(n_beetle,
                      aes(type, mean_n,
                          fill = sample_time)) +
  geom_col(position = position_dodge(), col = "black")+
  geom_errorbar(aes(ymin = mean_n - se, ymax = mean_n + se), position = position_dodge2(width = 0.2, padding = 0.8)) + 
    labs(title = "",
       x = "",
       y = "Mean beetle number per carcass (n)",
       fill = "Sampling period")+
  # coord_flip()+
  theme_classic()
barplot_beetle

ggsave("figures/barplot_fly_mean.png", barplot_fly,
       width = 8,
       height = 5)
ggsave("figures/barplot_beetle_mean.png", barplot_beetle,
       width = 8,
       height = 5)

n_beetle <- n_beetle %>% 
  mutate(species = paste("Beetle"))

n_fly <- n_fly %>% 
  mutate(species = paste("Fly"))

n_invert <- rbind(n_beetle, n_fly) %>% 
  mutate(sp_type = paste(species, type, sep = ", "))


barplot_inv <- ggplot(n_invert,
                         aes(sp_type, mean_n,
                             fill = sample_time)) +
  geom_col(position = position_dodge(), col = "black")+
  geom_errorbar(aes(ymin = mean_n - se, ymax = mean_n + se), position = position_dodge2(width = 0.2, padding = 0.8)) + 
  labs(title = "",
       x = "",
       y = "Mean number per carcass (n)",
       fill = "Sampling period")+
  # coord_flip()+
  theme_classic()
barplot_inv

ggsave("figures/barplot_inv.png", barplot_inv,
       width = 9,
       height = 5)