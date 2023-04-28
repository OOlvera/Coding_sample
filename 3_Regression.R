## Basic regressions
## Author: Laura Olivia Olvera


install.packages("rstan")
install.packages("boot")
install.packages("finalfit")
install.packages("jtools")
library(jtools) 
library(tidyverse)
library(finalfit)
library(dplyr)
library(ggplot2)
library(stringr) 

## Set paths
## Change this when needed
path_clean <- "C:/Users/olivi/Box/FALL 2022/Data and Programming/Coding_sample/clean_data"

global_health_expenditure <- read.csv(file.path(path_clean,"df_final.csv"))

## Testing if out-of-pocket expenditure affects life expectancy outputs
model_life_expectancy <- lm(life_exp ~ out_of_pocket + gdp_percap +country, data = global_health_expenditure)
summ(model_life_expectancy)


## Testing if out-of-pocket expenditure affects outputs in chronic diseases as diabetes
data_diabetes <-
  global_health_expenditure %>%
  filter(date==2000 |date== 2011 |date== 2019)%>%
  na.omit() %>%
  filter(diabetes != "-")

model_diabetes <- lm(diabetes ~ out_of_pocket + gdp_percap + country, data = data_diabetes)
summ(model_diabetes)

