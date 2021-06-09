# HF NDL Satellite Analysis Rmd


# setup -------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(lubridate)
library(plotly)
library(skimr)
library(DBI)
library(odbc)
library(stringr)
library(forcats)
library(plotly)
library(readr)
library(openxlsx)

rm(list = ls())

# load --------------------------------------------------------------------
# load from file
openxlsx::getSheetNames("Output_SatelliteC_Tables_Leeds5.xlsx")
ASC_Frailty_sex <- openxlsx::read.xlsx("Output_SatelliteC_Tables_Leeds5.xlsx", 
                                       sheet = "ASC_Frailty_sex")

# Tables
unique(ASC_Frailty_sex$sex)

ASC_Frailty_sex %>%
  group_by(ASC, Frailty_Level) %>% 
  summarise(number.patients = sum(number.patients)) %>% 
  pivot_wider(., names_from = "ASC", values_from = "number.patients")

ASC_Frailty_sex %>%
  pivot_wider(., names_from = "ASC", values_from = "number.patients")  

# ASC_Frailty_sex %>% 
#   mutate(across(where(is.character), as_factor),
#          sex = fct_collapse(sex, both = c("female", "male")))


# functions ---------------------------------------------------------------
# single case
ASC_Frailty_sex %>%
  group_by(ASC, Frailty_Level) %>% 
  summarise(number.patients = sum(number.patients)) %>%
  ggplot(aes(x = Frailty_Level, y = number.patients, fill = ASC)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12))

# group by / summarise
NDL_summ1 <- function(data, x, y){
  data %>% group_by({{x}}, {{y}}) %>% 
  summarise(number.patients = sum(number.patients)) 
#  pivot_wider(., names_from = {{x}}, values_from = "number.patients")
}

# ggplot func
NDL_col1 <- function(data, x, y) {
  data %>% 
    ggplot(data, mapping = aes(x = .data[[x]], y = number.patients, fill = .data[[y]])) +
    geom_col(position = "dodge") +
    scale_fill_viridis_d() +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12, face = "bold"),
          legend.position = "bottom",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12, face = "bold"),
          strip.text = element_text(size = 12))
}

# test
ASC_Frailty_sex %>% 
  NDL_summ1(., ASC, Frailty_Level) %>% 
  NDL_col1(., "Frailty_Level", "ASC")

NDL_col2 <- function(data, x, y, z) {
    p <- ggplot(data, mapping = aes(x = .data[[x]], y = number.patients, fill = .data[[y]])) +
    geom_col(position = "dodge") +
    scale_fill_viridis_d() +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12, face = "bold"),
          legend.position = "bottom",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12, face = "bold"),
          strip.text = element_text(size = 12))
    if (!is.null(z)){
      p + facet_wrap(~.data[[z]])
    }
}
  
ASC_Frailty_deprivation %>% 
  NDL_col2(., "deprivation", "Frailty_Level", "ASC")

ASC_Frailty_sex %>%
  NDL_summ1(., ASC, Frailty_Level) %>% 
  NDL_col1(., "Frailty_Level", "ASC")

  
ASC_Frailty_deprivation %>% 
  ggplot(aes(x = deprivation, y = number.patients, fill = Frailty_Level)) +
  geom_col(position = "fill") +
  geom_text(
    mapping = aes(label = number.patients, vjust = 2),
    stat = "identity",
    position = "fill",
#    nudge_y = 0.5,
    color = "white",
    check_overlap = T) +
  facet_wrap(~ASC) +
  scale_fill_viridis_d() +
  labs(y = "proportion of patients") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 12)
        )

# 33569
## Frailty
# Fit = 13040
# Mild = 9842
# Moderate = 5787
# Severe = 3778
# Unknown = 1122
13040+9842+5787+3778+1122 # 33569

## ASC
# Known = 3421
# Unknown = 30148
3421+30148


# find data ---------------------------------------------------------------
rm(list = ls())
load(file = "HF_NDL_Satellite1_6.RData")

# commit
# commit
# commit
# commit
# commit
# commit
# commit
# commit
# commit
# commit


