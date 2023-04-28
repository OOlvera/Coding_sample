## Data visualizations
## Author: Laura Olivia Olvera

library(tidyverse)
library(ggplot2)
library(readxl)
library(readr)
library(dplyr)
library(magrittr)
library(tidyr)

## Set paths
## Change this when needed
path_raw <- "C:/Users/olivi/Box/FALL 2022/Data and Programming/Coding_sample/raw_data"
path_clean <- "C:/Users/olivi/Box/FALL 2022/Data and Programming/Coding_sample/clean_data"
path_plot <- "C:/Users/olivi/Box/FALL 2022/Data and Programming/Coding_sample/plots"


plot_format <-  theme(panel.background = element_blank(), 
                      plot.title = element_text(size=15, vjust = 2.5, hjust = 0.5),
                      legend.title = element_blank(),
                      legend.position="none", 
                      axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10),
                      axis.title.x = element_text(size = 11, margin = margin(r = 15)),
                      axis.title.y = element_text(size = 11, margin = margin(r = 15)),
                      axis.line = element_line(colour = "black", size = .5), 
                      plot.caption=element_text(size=9, hjust=0)) 

## Out-of-pocket
df_final <- read.csv(file.path(path_clean,"df_final.csv"))

df_final <- transform(df_final, diabetes = as.numeric(diabetes))

out_of_pocket_plot <- df_final %>%
  filter(date==2019) %>%
  drop_na(out_of_pocket) %>%
  ggplot(aes(x = reorder(country, out_of_pocket), y = out_of_pocket, fill = ifelse(country == "Mexico", "Highlighted", "Normal"))) +
  geom_bar(position = "dodge", stat = "identity",  
           width = 0.7) +
  labs(title = "Out-of-pocket expenditure (2019), Latin America and the Caribbean", 
       y = "Percentage (%)", 
       x = "", 
       caption = "Source: World Bank")  +
  plot_format +
  coord_flip()

png(file.path(path_plot,"out_of_pocket_plot.png"))
out_of_pocket_plot
dev.off()


## Life expectancy
life_expectancy_plot <- df_final %>%
  filter(date==2019) %>%
  drop_na(out_of_pocket) %>%
  ggplot(aes(x = life_exp, y = out_of_pocket)) + 
  geom_point() +
  geom_text(hjust = 0, nudge_x = 0.1, size = 2, aes(label = iso3c)) +
  geom_smooth(method = lm, color = "darkred") + 
  #scale_fill_continuous(low='yellow', high='#de2d26') +
  labs(title = "Relation between life expectancy and out-of-pocket expenditure (2019) in Latin America and the Caribbean",
       y = "Percentage of out-of-pocket expenditure", 
       x = "Life expectancy", 
       caption = "Source: World Bank")  +
  plot_format


png(file.path(path_plot,"life_expectancy_plot.png"))
life_expectancy_plot
dev.off()



## Diabetes
diabetes_plot <- df_final %>%
  filter(date==2019) %>%
  drop_na(out_of_pocket) %>%
  drop_na(diabetes) %>%
  ggplot(aes(x = diabetes, y = out_of_pocket)) + 
  geom_point() +
  geom_text(hjust = 0, nudge_x = 0.1, size = 2, aes(label = iso3c)) +
  geom_smooth(method = lm, color = "darkred") +
  labs(title = "Diabetes and out-of-pocket expenditure (2019) in Latin America and the Caribbean",
       y = "Percentage of out-of-pocket expenditure", 
       x = "Percentage of population with diabetes", 
       caption = "Source: World Bank")  +
  plot_format

png(file.path(path_plot,"diabetes_plot.png"))
diabetes_plot
dev.off()


## Health expenditure in Mexico 
health_exp_mex <- read.csv(file.path(path_clean,"health_exp_mex.csv"))

health_exp_mex_plot <- ggplot(health_exp_mex, aes(y = expenditure, x = date)) +
  geom_col(aes(fill = exp_type1), width = 0.7)+
  geom_text(aes(label = expenditure, group = exp_type1), color = "white", position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "Out-of-pocket expenditure in Mexico by dimensions", 
       y = "Percentage", 
       x = "", 
       caption = "Source: National Institute of Statistics and Geography (INEGI)")  +
  scale_fill_manual(values = c("#003f5c", "#444e86","#955196","#dd5182","#ff6e54")) +
  plot_format +
  theme(legend.title = element_blank(),
        legend.position="right")


png(file.path(path_plot,"health_exp_mex_plot.png"))
health_exp_mex_plot
dev.off()
