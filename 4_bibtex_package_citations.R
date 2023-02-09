library(knitr)

knitr::write_bib(c("readxl", "tidyverse", "lubridate", "ggplot2", 
                        "ggpubr", "factoextra", "FactoMineR", "vegan", 
                        "performance", "coxme", "MuMIn"), file = "paper/Rpackages.bib")


#All packages for thesis
knitr::write_bib(c("readxl", "tidyverse", "lubridate", "ggplot2", 
                   "ggpubr", "factoextra", "FactoMineR", "vegan", 
                   "performance", "coxme", "MuMIn","broom", "broom.mixed", "broom.helpers", "lares",
                   "lme4", "nlme", "optimx", "nloptr", "fitdistrplus",
                   "camtrapR", "predictmeans", "emmeans", "lmPerm"), file = "paper/Rpackages_all.bib")