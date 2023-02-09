#Libraries###
#Data manipulation
library(readxl)
library(tidyverse)
library(lubridate)

#load data####
biomass <- read_excel("data/biomass_clean.xlsx", sheet = "biomass_clean") 
data_camtrap <- read_excel("data/camtrapdata_clean.xlsx")
data_f <- read_excel("data/fly_n_analysis.xlsx") %>% 
  rename("fly_n" = "n")
data_b <- read_excel("data/beetle_n_analysis.xlsx") %>% 
  rename("beetle_n" = "n")


#Biomass loss data####
biomass1 <- biomass %>% 
  group_by(site, type, treatment, carcass_n) %>% 
  # nest() %>% 
  mutate(days = as.numeric(difftime(date_measured, date, units = "days"))) %>% 
  #select when the biomass was measured in week1, week2, week4, corresponding to invert sampling.
  #There are some differences in days due to how we sampled according to weather etc.
  mutate(sample_time = case_when(days >= 0 & days <=3 ~ "T1",
                                 days >= 7 & days <= 14 ~ "T2",
                                 days >= 23 & days <= 29 ~ "T3",
                                 TRUE ~ "NA")
  ) %>% 
  ungroup() %>% 
  filter(sample_time != "NA") %>% 
  group_by(site, type, treatment, sample_time) %>% 
  mutate(mass = round(mean(biomass_loss))) %>%  #Take the average from the measurements taken and round to the nearest integer
  filter(row_number()==1) %>%  #keep first row of each group
  select(site, type, treatment, sample_time, decomposition_stage, mass) %>% 
  ungroup() %>% 
  mutate(mass = case_when(mass == 1 ~ 1,
                   mass == 2 ~ 0.9,
                   mass == 3 ~ 0.7,
                   mass == 4 ~ 0.5,
                   mass == 5 ~ 0.3,
                   mass == 6 ~ 0.2,
                   mass == 7 ~ 0.1),
         type = case_when(str_detect(type, "MM") ~ "Mass mortality",
                          str_detect(type, "SC") ~ "Single carcass",
                          str_detect(type, "C") ~ "Control"),
         site = case_when(str_detect(site, "CR") ~ "Site 1",
                          str_detect(site, "WP") ~ "Site 2",
                          str_detect(site, "MS") ~ "Site 3"),
         treatment = case_when(str_detect(treatment, "HE") ~ "Open 2",
                               str_detect(treatment, "Nil") ~ "Open 1",
                               str_detect(treatment, "VE") ~ "Vertebrate exclusion",
                               str_detect(treatment, "IE") ~ "Invertebrate exclusion",)
         )%>% 
  mutate(across(.cols = 6, ~replace_na(., 1)))

  
#Vertebrate data####  
verts <- data_camtrap %>% 
  rename("plot_id" = "Station",
         "species" = "Species",
         "indep_event" = "group",
         "behaviour" = "Behaviour",
         "f_time" = "visit_time") %>% 
  mutate(behaviour = case_when(
    #Tidy up the non-scavenger species
    species ==  "Bird"| species == "Common wombat"|species == "Emu"|species == "Fallow deer"|
      species =="Red-necked wallaby"|species == "Horse"|species == "Rabbit"|species == "Sambar"|
      species =="Kookaburra"|species == "Macropod"|species == "Unknown deer"|species == "Crimson rosella"|
      species =="Eastern grey kangaroo"|species == "White-winged chough"|species == "Galah"|species == "Red deer"|species == "Willie wagtail"|
      species =="Pied currawong"|species == "Australian magpie"|species == "Macropod"|species == "Swamp wallaby"
    ~ "Not feeding",
    TRUE ~ behaviour)) %>% 
  
  filter(behaviour == "Feeding") %>% 
  select(plot_id, indep_event, date_set, DateTimeOriginal, species, N, f_time) %>% 
  group_by(plot_id) %>%
  nest() %>% 
  mutate(data = map(data, ~mutate(.x,
                                  #calculate days since carcass placement
                                  days = as.numeric(round(difftime(DateTimeOriginal, date_set, 
                                                                   units = "days"))))
  ),
  #this is the fourth week after carcass placement, filtering by days > 0 cause there's two mistakes
  data = map(data, ~filter(.x, days < 35 & days >0)) 
  ) %>% 
  unnest(cols = c(data)) %>% 
  ungroup() %>% 
  mutate(sample_time = case_when(days >= 0 & days <=3 ~ "T1",
                                 days >= 7 & days <= 14 ~ "T2",
                                 days >= 23 & days <= 29 ~ "T3",
                                 TRUE ~ "NA")
  ) %>% 
  filter(sample_time != "NA") %>%  #Exclude all the values that are not in those sampling periods
  group_by(plot_id, species, sample_time) %>% 
  mutate(event_n = length(sample_time) #get the number of events for each week (= to the number of rows)
  ) %>% #get the presence/absence data
  filter(row_number()==1) %>%  #keep first row for each group
  select(plot_id, species, sample_time, days, event_n) %>%   #these columns are  necessary
  #add info about site, type, treatment  
  mutate(type = case_when(str_detect(plot_id, "MM") ~ "Mass mortality",
                          str_detect(plot_id, "SC") ~ "Single carcass"),
         site = case_when(str_detect(plot_id, "CR") ~ "Site 1",
                          str_detect(plot_id, "WP") ~ "Site 2",
                          str_detect(plot_id, "MS") ~ "Site 3"),
         treatment = case_when(str_detect(plot_id, "HE") ~ "Open 2",
                               str_detect(plot_id, "NIL") ~ "Open 1"), .after = plot_id,
  ) %>%  
  ungroup()

#pivot_wider by species

vert1 <- verts %>% 
  mutate(species = gsub("Brushtail possum", "Possum", species),
         species = gsub("Red fox", "Fox", species),
         species = gsub("Wedge-tailed eagle", "Eagle", species)
  ) %>% 
  pivot_wider(names_from = species,
              values_from = c(event_n)) %>% 
  mutate(across(.cols = 6:11, ~replace_na(., 0))) %>%  #add zeroes for all missing values
  mutate(across(.cols = 6:11, ~replace(., .x != 0, 1))) #subsitute non-zero values with 1, to get presence/absence data

#merge invert and biomass data

fly_biomass <- merge(data_f, biomass1, by = c("site", "type", "treatment", "sample_time"))

invert_biomass <- merge(data_b, fly_biomass, by = c("site", "type", "treatment", "sample_time")) 

#Add the scavenger presence data to the dataset####
scav_data <- merge(invert_biomass, vert1, 
                   by = c("site", "type", "treatment", "sample_time"), 
                   #This is important, it means that the only rows kept are 
                   #the ones in common between the two datasets, 
                   #i.e. just open treatment
                   all.x = F 
    )%>% 
  select(-plot_id, -treatment, -Eagle, -days) %>% #maybe just use event_n to compare with fly and beetle n
  mutate(site = as.factor(site),
         type = as.factor(type),
         sample_time = as.factor(sample_time),
         Corvid = Corvid + 2,
         Dingo = Dingo + 4,
         Fox = Fox + 6,
         Possum = factor(Possum,
                         levels = c(0, 1),
                         labels = c("Possum Absent", "Possum Present") #0 means absent, 1 means present
         ),
         Corvid = factor(Corvid,
                         levels = c(2, 3),
                         labels = c("Corvid Absent", "Corvid Present") #2 means absent, 3 means present
         ),
         Dingo = factor(Dingo,
                        levels = c(4, 5),
                        labels = c("Dingo Absent", "Dingo Present") #4 means absent, 5 means present
         ),
         Fox = factor(Fox,
                      levels = c(6, 7),
                      labels = c("Fox Absent", "Fox Present") #6 means absent, 7 means present
         ),
         beetle_n_cat = cut_number(beetle_n, 6),#approx same number of observations per category, similar to quantiles
         fly_n_cat = cut_number(fly_n, 6),#approx same number of observations per category, similar to quantiles
         beetle_n_cat = factor(beetle_n_cat,
                               levels = c("[1,9]", "(9,18]", "(18,45]", "(45,78]", "(78,182]", "(182,264]"),
                               labels = c("Beetle n < 9", "10 < Beetle n < 18", "19 < Beetle n < 45",
                                          "46 < Beetle n < 78", "79 < Beetle n < 182", "Beetle n > 183")
         ),
         fly_n_cat = factor(fly_n_cat,
                            levels = c("[3,9]", "(9,20]", "(20,39]", "(39,74]", "(74,127]", "(127,449]"),
                            labels = c("Fly n < 9", "10 < Fly n < 20", "21 < Fly n < 39", "40 < Fly n < 74",
                                       "75 < Fly n < 127", "Fly n > 128"))
        ) 

rm(vert1, verts, invert_biomass, data_camtrap, data_f, data_b, biomass, biomass1, fly_biomass)

writexl::write_xlsx(scav_data, "data/scav_data_analysis.xlsx")

#Get data for barplots
biomass_plots <- biomass %>% 
  group_by(site, type, treatment, carcass_n) %>% 
  # nest() %>% 
  mutate(days = as.numeric(difftime(date_measured, date, units = "days"))) %>% 
  #select when the biomass was measured in week1, week2, week4, corresponding to invert sampling.
  #There are some differences in days due to how we sampled according to weather etc.
  mutate(sample_time = case_when(days >= 0 & days <=3 ~ "T1",
                                 days >= 7 & days <= 14 ~ "T2",
                                 days >= 23 & days <= 29 ~ "T3",
                                 TRUE ~ "NA")
  ) %>% 
  ungroup() %>% 
  filter(sample_time != "NA") %>% 
  group_by(site, type, treatment, carcass_n, sample_time) %>% 
  mutate(mass = round(mean(biomass_loss))) %>%  #Take the average from the measurements taken and round to the nearest integer
  filter(row_number()==1) %>%  #keep first row of each group
  select(site, type, treatment, carcass_n, sample_time, decomposition_stage, mass) %>% 
  ungroup() %>% 
  mutate(mass = case_when(mass == 1 ~ 1,
                          mass == 2 ~ 0.9,
                          mass == 3 ~ 0.7,
                          mass == 4 ~ 0.5,
                          mass == 5 ~ 0.3,
                          mass == 6 ~ 0.2,
                          mass == 7 ~ 0.1),
         type = case_when(str_detect(type, "MM") ~ "Mass mortality",
                          str_detect(type, "SC") ~ "Single carcass",
                          str_detect(type, "C") ~ "Control"),
         site = case_when(str_detect(site, "CR") ~ "Site 1",
                          str_detect(site, "WP") ~ "Site 2",
                          str_detect(site, "MS") ~ "Site 3"),
         treatment = case_when(str_detect(treatment, "HE") ~ "Open 2",
                               str_detect(treatment, "Nil") ~ "Open 1",
                               str_detect(treatment, "VE") ~ "Vertebrate exclusion",
                               str_detect(treatment, "IE") ~ "Invertebrate exclusion",)
  )%>%
  mutate(across(.cols = 7, ~replace_na(., 1)))


writexl::write_xlsx(biomass_plots, "data/biomass_plots_data.xlsx")
