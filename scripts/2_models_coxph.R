#Packages####
#Data manipulation
library(readxl)
library(tidyverse)
library(lubridate)
library(tools)

#Plotting
library(ggplot2)
library(ggpubr)
library(survminer)

#Statistics
library(survival)
library(coxme)
library(MuMIn)
library(powerSurvEpi)#power analysis
# library(simstudy)

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

AICc(m1, m2) #coxme is better, but cannot use it cause DFs are negative

rm(f1a, f1b, m1, m2)

#Candidate model set for model selection
f1   <- Surv(time, status) ~ type + treatment + initial_weight + treatment*type + treatment*initial_weight + type*initial_weight + site
f2	 <- Surv(time, status) ~ type + site
f3	 <- Surv(time, status) ~ treatment + site
f4	 <- Surv(time, status) ~ type + treatment + site
f5	 <- Surv(time, status) ~ type + initial_weight + site
f6   <- Surv(time, status) ~ treatment + initial_weight + site
f7   <- Surv(time, status) ~ type + treatment + initial_weight + site
f8   <- Surv(time, status) ~ type + treatment + initial_weight + treatment*type + site
f9   <- Surv(time, status) ~ type + treatment + initial_weight + treatment*initial_weight + site
f10  <- Surv(time, status) ~ type + treatment + initial_weight + type*initial_weight + site
f11  <- Surv(time, status) ~ type + treatment + initial_weight + treatment*type + treatment*initial_weight + site
f12  <- Surv(time, status) ~ type + treatment + initial_weight + treatment*type + type*initial_weight + site
f13  <- Surv(time, status) ~ type + treatment + initial_weight + treatment*initial_weight + type*initial_weight + site

f_list <-  c(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)
models_biomass <- map(f_list, ~coxph(.x,
                                     data = biomass))

sel_biomass <- model.sel(models_biomass)

#Average all the models with delta <2
avg_biomass <- model.avg(sel_biomass, subset = delta <2)
summary(avg_biomass)

#Final model####
#Re-run resulting model
m_biomass <- coxph(Surv(time, status) ~ site + treatment + type,
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
                             coefficients == "siteSite 2" ~ "Site 2", 
                             coefficients == "siteSite 3" ~ "Site 3",
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
plot_data[nrow(plot_data) + 1,] <- c("Invertebrate suppression", 0, 0, "Invertebrate suppression", 0, "NA", "NA") #Reference value 2
plot_data[nrow(plot_data) + 1,] <- c("Site 1", 0, 0, "Site 1", 0, "NA", "NA") #Reference value 3

#Need to adjust the data type before plotting
plot_data$Estimate <- as.numeric(plot_data$Estimate)
plot_data$'Std. Error' <- as.numeric(plot_data$'Std. Error')
plot_data$lowerCI <- as.numeric(plot_data$lowerCI)
plot_data$upperCI <- as.numeric(plot_data$upperCI)
plot_data$pos_neg <- as.numeric(plot_data$pos_neg)

plot_data <- plot_data %>% 
  mutate(across(c(2,3,6,7), ~round(.x, digits = 2))) %>% 
  rename("SE" = "Std. Error") %>% 
  mutate(SELower = Estimate - SE,
         SEUpper = Estimate + SE)

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

plot_biomass2 <- plot_data %>% 
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
  geom_errorbar(aes(ymin = lowerCI, ymax = upperCI), width = 0.2)+
  
  geom_text(label = round(plot_data$Estimate, digits = 3),
            vjust = -0.8,
            hjust = 1.15)

plot_biomass2

ggsave("figures/plot_biomass_general.png", plot_biomass,
       height = 4,
       width = 8)

ggsave("figures/plot_biomass_CI.png", plot_biomass2,
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


#Survival curves####
# Create separate ggsurvplot for each variable with different levels
custom_var_names <- c(
  "initial_weight" = "Initial carcass weight",
  "treatment" = "Exclusion treatment",
  "type" = "Carcass treatment",
  "site" = "Site"
)
var_names <- names(custom_var_names)
plot_list <- list()

for (i in seq_along(var_names)) {
  var_name <- var_names[i]
  title <- custom_var_names[var_name] 
  p <- ggsurvplot(
    survfit(surv_obj ~ get(var_name), data = biomass),
    data = biomass,
    conf.int = T,
    title = paste(title),
    palette = "Set1",
    xlab = "Time (days)",
    ylab = "Carcass persistence probability",
    legend.title = "",
    legend.labs = levels(factor(biomass[[var_name]]))
  )
  
  plot_list[[i]] <- p
}

# Combine the plots you need
combined_plot <- ggarrange(plot_list[[2]]$plot, plot_list[[3]]$plot, plot_list[[4]]$plot,
                           labels = c("A", "B", "C")
                           )

# Save the combined plot
ggsave("figures/surv_plots.png", combined_plot,
       width = 10,
       height = 10)

#Power analysis Carcass treatment####
power_data <- biomass %>% 
  select(time, status, type) %>% 
  mutate(type = gsub("Mass mortality", "E", type),
         type = gsub("Single carcass", "C", type))

power.analysis1 <-  ssizeCT(formula = Surv(time, status) ~ type, #Mass mortality vs single carcass
                            dat = power_data,
                            power = 0.8,
                            k = 10, #Technically it's 1 MM plot and 1 SC plot, although MM has 10 carcasses
                            RR = 0.5, #postulated HR of 0.5
                            alpha = 0.05
                            )


power.analysis1$ssize 
#313 carcasses in MM sites and 32 carcasses in single carcass plots, i.e. 8 site replicates
df1 <- data.frame(nE = power.analysis1$ssize[1], nC = power.analysis1$ssize[2])
df1$Var <- "Vertebrate exclusion and invertebrate exclusion"

#Power analysis Exclusion treatment ####
power_data2a <- biomass %>% 
  select(time, status, treatment) %>% 
  filter(treatment != "Vertebrate exclusion") %>% 
  mutate(treatment = gsub("Open", "E", treatment),
         treatment = gsub("Invertebrate exclusion", "C", treatment))

power.analysis2a <-  ssizeCT(formula = Surv(time, status) ~ treatment, #Mass mortality vs single carcass
                           dat = power_data2a,
                           power = 0.8,
                           k = 2, #Two open plots vs 1 inv exclusion
                           RR = 0.5, #postulated HR of 0.5
                           alpha = 0.05
)


power.analysis2a$ssize 
#241 carcasses in MME but 121 in SC. Total of 362 carcasses.
#241 carcasses /40 at each site, would mean 6 sites
#121 single carcasses needed, divide these by 6 sites, meaning 20 single carcasses needed at each site. 
#It would mean 10 carcasses for Open treatment, 5 carcasses for vertebrate excl, 5 carcasses for inv exclusion at each site 
df2a <- data.frame(nE = power.analysis2a$ssize[1], nC = power.analysis2a$ssize[2])
df2a$Var <- "Open and invertebrate exclusion"

power_data2b <- biomass %>% 
  select(time, status, treatment) %>% 
  filter(treatment != "Open") %>% 
  mutate(treatment = gsub("Vertebrate exclusion", "E", treatment),
         treatment = gsub("Invertebrate exclusion", "C", treatment))

power.analysis2b <-  ssizeCT(formula = Surv(time, status) ~ treatment, #Mass mortality vs single carcass
                            dat = power_data2b,
                            power = 0.8,
                            k = 1, #Same n of vert exclusion and inv exclusion
                            RR = 0.5, #postulated HR of 0.5
                            alpha = 0.05
)


power.analysis2b$ssize

# You would need a total of 182 carcasses for MME plots. Divide total by 40, That would mean 4.5 sites (5 sites)
# 182 single carcasses needed too. However, you would need 36 single carcasses per site. 
# 18 with vertebrate exclusion, 18 with invertebrate exclusion
df2b <- data.frame(nE = power.analysis2b$ssize[1], nC = power.analysis2b$ssize[2])
df2b$Var <- "Vertebrate exclusion and invertebrate exclusion"

power_data2c <- biomass %>% 
  select(time, status, treatment) %>% 
  filter(treatment != "Invertebrate exclusion") %>% 
  mutate(treatment = gsub("Open", "E", treatment),
         treatment = gsub("Vertebrate exclusion", "C", treatment))

power.analysis2c <-  ssizeCT(formula = Surv(time, status) ~ treatment, #Mass mortality vs single carcass
                             dat = power_data2c,
                             power = 0.8,
                             k = 2, #Open plots are 2x vert exclusion plots
                             RR = 0.5, #postulated HR of 0.5
                             alpha = 0.05
)


power.analysis2c$ssize
#89 carcasses required for Open, 45 required for vertebrate exclusion
#89 divided by 40 carcasses (mass mortality) equals 2.2 sites
#45 divided by two sites equals 22.5 carcasses for vertebrate exclusion at each site.
df2c <- data.frame(nE = power.analysis2c$ssize[1], nC = power.analysis2c$ssize[2])
df2c$Var <- "Open and Vertebrate exclusion"

#Site power analysis####
power_data3a <- biomass %>% 
  select(time, status, site) %>% 
  filter(site != "Site 2") %>% 
  mutate(site = gsub("Site 3", "E", site),
         site = gsub("Site 1", "C", site))


power.analysis3a <-  ssizeCT(formula = Surv(time, status) ~ site, #Mass mortality vs single carcass
                            dat = power_data3a,
                            power = 0.8,
                            k = 1, #Same n of vert exclusion and inv exclusion
                            RR = 0.5, #postulated HR of 0.5
                            alpha = 0.05
)


power.analysis3a$ssize
#A total of 177 carcasses needed for each site to detect site differences?
df3a <- data.frame(nE = power.analysis3a$ssize[1], nC = power.analysis3a$ssize[2])
df3a$Var <- "Site 1 and Site 3"


power_data3b <- biomass %>% 
  select(time, status, site) %>% 
  filter(site != "Site 3") %>% 
  mutate(site = gsub("Site 2", "E", site),
         site = gsub("Site 1", "C", site))

power.analysis3b <-  ssizeCT(formula = Surv(time, status) ~ site, #Mass mortality vs single carcass
                            dat = power_data3b,
                            power = 0.8,
                            k = 1, #Same n of vert exclusion and inv exclusion
                            RR = 0.5, #postulated HR of 0.5
                            alpha = 0.05
)


power.analysis3b$ssize
#Same as before: 177 carcasses required for each site.
df3b <- data.frame(nE = power.analysis3b$ssize[1], nC = power.analysis3b$ssize[2])
df3b$Var <- "Site 1 and Site 2"

power_data3c <- biomass %>% 
  select(time, status, site) %>% 
  filter(site != "Site 1") %>% 
  mutate(site = gsub("Site 3", "E", site),
         site = gsub("Site 2", "C", site))

power.analysis3c <-  ssizeCT(formula = Surv(time, status) ~ site, #Mass mortality vs single carcass
                            dat = power_data3c,
                            power = 0.8,
                            k = 1, #Same n of vert exclusion and inv exclusion
                            RR = 0.5, #postulated HR of 0.5
                            alpha = 0.05
)


power.analysis3c$ssize
#80 carcasses required for each site to detect differences
df3c <- data.frame(nE = power.analysis3c$ssize[1], nC = power.analysis3c$ssize[2])
df3c$Var <- "Site 2 and Site 3"

power_results <- rbind(df1, df2a, df2b, df2c, df3a, df3b, df3c) %>% 
  select(Var, everything()) %>% 
  rename(Category1_category2= Var,
          Number_category1= nE,
          Number_category2= nC )

# library(pwr)
# 
# # Specify parameters
# effect_size <- 0.5  # Example effect size (Cohen's d)
# alpha <- 0.05
# power <- 0.80
# # block_size <- 3
# num_blocks <- 2
# num_treatments <- 3
# 
# # Perform power analysis
# required_sample_size <- pwr.anova.test(
#   k = ,
#   n = 3 * 2,
#   f = 0.5,
#   sig.level = 0.05,
#   power = 0.8
#   )
