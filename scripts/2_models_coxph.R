#Packages####
#Data manipulation
library(readxl)
library(tidyverse)
library(lubridate)

#Plotting
library(ggplot2)
library(ggpubr)

#Statistics
library(survival)
library(coxme)
library(MuMIn)

#Model validation
library(predictmeans)

#load data####
biomass_data0 <- read_excel("data/biomass_cox.xlsx") 

biomass <- biomass_data0 %>% 
  mutate(status = case_when(biomass_loss == 7 ~ 1, #when biomass_loss = 7, the carcass has reached full decomp
                            TRUE ~ 0), #with all the other values, there is still carcass mass remaining
          .after = biomass_loss,
         type = case_when(str_detect(type, "MM") ~ "Mass mortality",
                          str_detect(type, "SC") ~ "Single carcass",
                          str_detect(type, "C") ~ "Control"),
         site = case_when(str_detect(site, "CR") ~ "Site 1",
                          str_detect(site, "WP") ~ "Site 2",
                          str_detect(site, "MS") ~ "Site 3"),
         treatment = case_when(str_detect(treatment, "HE") ~ "Open", #pool together HE and Nil as they are effectively the same
                               str_detect(treatment, "Nil") ~ "Open", 
                               str_detect(treatment, "VE") ~ "Vertebrate exclusion",
                               str_detect(treatment, "IE") ~ "Invertebrate exclusion"), 
         )

#Model formulas####

f1a   <- Surv(time, status) ~ site + type + treatment + initial_weight + treatment*type + treatment*initial_weight + type*initial_weight
f1b   <- Surv(time, status) ~ type + treatment + initial_weight + treatment*type + treatment*initial_weight + type*initial_weight + (1|site)

#Random model structure####

#Cox's PH models: check whether adding a random structure might help

m1 <- coxph(f1a,
            data = biomass)

#Below does not run
m2 <- coxme(f1b,
            data = biomass)

AICc(m1, m2) #coxme is better

rm(f1a, f1b, m1, m2)

#Candidate model set for model selection
f1   <- Surv(time, status) ~ type + treatment + initial_weight + treatment*type + treatment*initial_weight + type*initial_weight + (1|site)
f2	 <- Surv(time, status) ~ type + (1|site)
f3	 <- Surv(time, status) ~ treatment + (1|site)
f4	 <- Surv(time, status) ~ type + treatment + (1|site)
f5	 <- Surv(time, status) ~ type + initial_weight + (1|site)
f6   <- Surv(time, status) ~ treatment + initial_weight + (1|site)
f7   <- Surv(time, status) ~ type + treatment + initial_weight + (1|site)
f8   <- Surv(time, status) ~ type + treatment + initial_weight + treatment*type + (1|site)
f9   <- Surv(time, status) ~ type + treatment + initial_weight + treatment*initial_weight + (1|site)
f10  <- Surv(time, status) ~ type + treatment + initial_weight + type*initial_weight + (1|site)
f11  <- Surv(time, status) ~ type + treatment + initial_weight + treatment*type + treatment*initial_weight + (1|site)
f12  <- Surv(time, status) ~ type + treatment + initial_weight + treatment*type + type*initial_weight + (1|site)
f13  <- Surv(time, status) ~ type + treatment + initial_weight + treatment*initial_weight + type*initial_weight + (1|site)

f_list <-  c(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)
models_biomass <- map(f_list, ~coxme(.x,
                                     data = biomass))

sel_biomass <- model.sel(models_biomass)

#Average all the models with delta <2
avg_biomass <- model.avg(sel_biomass, subset = delta <2)
summary(avg_biomass)

#Re-run resulting model
m_biomass <- coxme(Surv(time, status) ~ initial_weight + treatment + type + 
                     (1|site),
                   data = biomass)
summary(m_biomass)

#Model plot####
#put together the data needed for a plot
plot_data <- as.data.frame(coefTable(m_biomass))%>% 
  rownames_to_column() %>% 
  rename("coefficients" = "rowname") 

plot_data1 <- plot_data %>% 
  mutate(labels = case_when(coefficients == "treatmentVertebrate exclusion"~ "Vertebrate exclusion",
                             coefficients == "treatmentOpen" ~ "Open",
                             coefficients == "initial_weight" ~ "Initial weight",
                             coefficients == "typeSingle carcass" ~ "Single carcass", 
                             TRUE ~ coefficients),
                 df = NULL,
         pos_neg = case_when(Estimate > 0 ~ 1,
                             Estimate < 0 ~ -1,
                             Estimate == 0 ~ 0))


confint_m <- as.data.frame(confint(m_biomass)) %>% 
  rownames_to_column() %>% 
  rename("coefficients" = "rowname",
         "lowerCI" = "2.5 %",
         "upperCI" = "97.5 %")

plot_data <- merge(plot_data1, confint_m, by = "coefficients")

#Add reference values for the plot
plot_data[nrow(plot_data) + 1,] <- c("Mass mortality", 0, 0, "Mass mortality", 0, "NA", "NA") #Reference value 1
plot_data[nrow(plot_data) + 1,] <- c("Invertebrate exclusion", 0, 0, "Invertebrate exclusion", 0, "NA", "NA") #Reference value 2

#Need to adjust the data type before plotting
plot_data$Estimate <- as.numeric(plot_data$Estimate)
plot_data$'Std. Error' <- as.numeric(plot_data$'Std. Error')
plot_data$lowerCI <- as.numeric(plot_data$lowerCI)
plot_data$upperCI <- as.numeric(plot_data$upperCI)
plot_data$pos_neg <- as.numeric(plot_data$pos_neg)

plot_data <- plot_data %>% 
  mutate(across(c(2,3,6,7), ~round(.x, digits = 2))) %>% 
  rename("SE" = "Std. Error")

rm(plot_data1, confint_m)

plot_biomass <- plot_data %>% 
          ggplot(mapping = aes(reorder(labels, Estimate), Estimate,
                               col = pos_neg))+
          geom_point()+theme_classic()+
  theme(plot.background = element_rect(colour = "black", fill = "white"),
        legend.position="none",
        axis.text = element_text(size = 10),
        title = element_text(size = 12),
        axis.title.y = element_text(size = 10),
        )+
  coord_flip()+
  geom_abline(intercept = 0,
              slope = 0,
             linetype = 2,
             colour = "black")+
  labs(x = "",
       y = "Odds ratio",
       title = "")+
  geom_errorbar(aes(ymin = Estimate - SE, ymax = Estimate + SE), width = 0.2)+
  
  geom_text(label = round(plot_data$Estimate, digits = 3),
            vjust = -0.8,
            hjust = 1.15)

plot_biomass



ggsave("figures/plot_biomass_general.png", plot_biomass,
       height = 4,
       width = 8)

#Model results####

model_est <- plot_data %>% 
  dplyr::select(labels, -pos_neg, Estimate, "Std. Error", lowerCI, upperCI) %>% 
  rename(
    "Variable" = "labels",
    "SE" = "Std. Error",
    "Lower CI (95%)" = "lowerCI",
    "Upper CI (95%)" = "upperCI"
  ) %>% 
  mutate(SE = as.numeric(SE),
    across(.cols = c(2:5), ~round(.x, digits = 2)))%>% 
  mutate(lower.SE = Estimate - SE,
         upper.SE = Estimate + SE)

writexl::write_xlsx(model_est, "results/m_biomass.xlsx")
