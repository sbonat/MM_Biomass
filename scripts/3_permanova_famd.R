#Libraries####
#Data manipulation
library(readxl)
library(tidyverse)
library(lubridate)

#Plotting
library(ggplot2)
library(ggpubr)
library(factoextra) #plotting ordinations

#Statistics
library(FactoMineR) #factor analysis of mixed type data
library(vegan) #permanova


#Load data####
scav_data <- read_excel("data/scav_data_analysis.xlsx") %>% 
  mutate(
    sample_time = gsub("T1", "Week 0", sample_time),
    sample_time = gsub("T2", "Week 2", sample_time),
    sample_time = gsub("T3", "Week 4", sample_time)
    )


# Factor analysis (Ordination) ####
# For factor analysis, use the beetle n and fly categories calculated above
scav_data1 <- scav_data %>% 
  select(-fly_n, -beetle_n)
famd1 <- FAMD(scav_data1, graph = F)
summary(famd1)

# plot1 <-  fviz_famd(famd1,
#                     axes = c(1,2), 
#                     geom= "text", 
#                     repel = T,
#                     habillage = c("type"),
#                     # col.quali.var = "darkblue",
#                     labelsize = 5,
#                     legend.title = "Carcass treatment",
#                     addEllipses = T, ellipse.type = "confidence",
#                     #alpha.ind = 0.5, #transparency
#                     )+
#           labs(x = "Axis 1 (14.1%)",
#                y = "Axis 2 (12.2%)",
#                title = "")+
#           theme(
#                legend.position = c(.95, .95),
#                legend.justification = c("right", "top"),
#                legend.box.just = "right",
#                legend.margin = margin(6, 6, 6, 6),
#                legend.box.background = element_rect(fill = "white", colour = "black"),
#                legend.title =element_text(size=18),
#                legend.text=element_text(size=16),
#                axis.title = element_text(size = 18),
#                plot.title = element_text(size = 20)
#              )
# # plot1
# 
# ggsave("figures/b_plot_famd1.png",
#        plot = plot1,
#        width = 10,
#        height = 10,
#        bg = "white")


plot2 <-  fviz_famd(famd1,
                    axes = c(1,2), 
                    geom= "text", 
                    repel = T,
                    habillage = c("sample_time"),
                    labelsize = 5,
                    legend.title = "Sampling period",
                    addEllipses = T, ellipse.type = "confidence",
                    #alpha.ind = 0.7, #transparency,
                    ggtheme = theme_minimal())+
  labs(x = "Axis 1 (14.1%)",
       y = "Axis 2 (12.2%)",
       title = "")+ 
  theme(
         legend.position = c(.95, .95),
         legend.justification = c("right", "top"),
         legend.box.just = "right",
         legend.margin = margin(6, 6, 6, 6),
         legend.box.background = element_rect(fill = "white", colour = "black"),
         legend.title =element_text(size=18),
         legend.text=element_text(size=16),
         axis.title = element_text(size = 18),
         plot.title = element_text(size = 20)
       )

plot2

ggsave("figures/b_plot_famd2.png", 
       plot = plot2,
       width = 11,
       height = 11,
       bg = "white")

plot_mass_scav <- ggarrange(plot1, plot2,
                            labels = "AUTO",
                            nrow = 2, 
                            ncol = 1
                            )

ggsave("figures/plot_famd_mass_scav.png", 
       plot = plot_mass_scav,
       width = 10,
       height = 20,
       bg = "white")

percent_explained <- as.data.frame(famd1$eig) %>% 
  dplyr::select(2,3) %>% 
  mutate_all(~round(.x, digits = 1)) %>% 
  rownames_to_column() %>% 
  rename("Component" = "rowname",
         "percent_explained" = "percentage of variance",
         "percent_cumulative" = "cumulative percentage of variance") %>% 
  mutate(Component = case_when(str_detect(Component, "comp") ~ paste("Axis", 1:n())))

famd_res_n <- get_famd_var(famd1, element = "quanti.var")
quanti.vars <- as.data.frame(famd_res_n$contrib)
famd_res_other <- get_famd_var(famd1, element = "quali.var")
quali.vars <-  as.data.frame(famd_res_other$contrib)

#Coordinates on PC axes (i.e. correlation values)
quanti.coord <- as.data.frame(famd_res_n$coord)
quali.coord <-  as.data.frame(famd_res_other$coord)
#Put all the values in one df
coord_data <- rbind(quanti.coord, quali.coord) %>% 
  mutate_all(~round(.x, digits = 3)) %>% 
  rownames_to_column()%>% 
  rename("Variable" = "rowname") 

colnames(coord_data)[2:6] <- paste(percent_explained$Component, " (", percent_explained$percent_explained, "%)", sep = "")

writexl::write_xlsx(coord_data, "results/famd_coord.xlsx")


#PERMANOVA ####

permanova_biomass <- with(scav_data, adonis2(mass ~ type + sample_time + fly_n + 
                                              beetle_n + Possum + Corvid +
                                              Dingo + Fox + sample_time*type,
                                              permutations = 9999,
                                              sqrt.dist = T,
                                              method = "gower",
                                              strata = site))

permanova_biomass <- permanova_biomass %>% 
                     rownames_to_column() %>% 
                     rename("Variable" = "rowname")


writexl::write_xlsx(permanova_biomass, "results/permanova_biomass.xlsx")

#plot time vs decomposition stage and biomass loss
biomass_plots <- read_excel("data/biomass_plots_data.xlsx") %>% 
  mutate(
    sample_time = gsub("T1", "Week 0", sample_time),
    sample_time = gsub("T2", "Week 2", sample_time),
    sample_time = gsub("T3", "Week 4", sample_time))


barplot_data1 <- biomass_plots %>% 
  group_by(decomposition_stage, sample_time) %>% 
  tally() %>% 
  ungroup()


decomp_plot1 <- ggplot(barplot_data1,
                      mapping = aes(sample_time,
                                    n,
                                    fill = factor(decomposition_stage,
                                                  levels = c("Fresh", "Bloat", "Active",
                                                             "Advanced", "Dry"))
                                    ))+
                geom_col(position = position_dodge2(preserve = "single"), col = "black")+
                labs(x = "Sampling period",
                     y = "Number of carcasses",
                     title = "",
                     fill = "Decomposition stage"
                     )+
                theme_classic()+
                scale_fill_brewer(palette = "RdYlBu")
#decomp_plot1

barplot_data2 <- biomass_plots %>% 
  group_by(mass, sample_time) %>% 
  tally() %>% 
  mutate(mass = mass*100)


decomp_plot2 <- ggplot(barplot_data2,
                       mapping = aes(sample_time,
                                     n,
                                     fill = factor(paste(mass, "%", sep = ""), 
                                                   levels = c("100%","90%","70%", "50%",
                                                              "40%", "30%", "20%", "10%" 
                                                               ),
                                                   labels = c("100%", "Between 90 and 99%", 
                                                              "Between 70 and 89%", "Between 50 and 69%", 
                                                              "Between 30 and 49%", "Between 20 and 29%",
                                                              "Between 10 and 19%", "< 10%" )
                                                   )
                                     ))+
  geom_col(position = position_dodge2(preserve = "single"), col = "black")+
  labs(x = "Sampling period",
       y = "Number of carcasses",
       title = "",
       fill = "Percentage carcass remaining") +
  theme_classic()+
  scale_fill_brewer(palette = "RdYlBu")
#decomp_plot2

barplots <- ggarrange(decomp_plot1, decomp_plot2,
                      labels = c("A", "B"),
                      ncol = 2,
                      nrow = 1,
                      align = "h")


ggsave("figures/barplots.png",
       
       width = 10,
       height = 5,
       bg = "white")
