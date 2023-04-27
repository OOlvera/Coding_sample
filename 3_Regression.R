#install.packages("rstan")
#install.packages("boot")
#install.packages("finalfit")
#install.packages("jtools")
library(jtools) 
library(tidyverse)
library(finalfit)
library(dplyr)
library(ggplot2)
library(stringr) 


path_clean <- "C:/Users/stiar/OneDrive/Documents/data_and_programming_II/final-project-karla-olivia-astrid/clean_data/"

global_health_expenditure <- read.csv(file.path(path_clean,"df_final.csv"))

#Testing if out-of-pocket expenditure affects life expectancy outputs
model_life_expectancy <- lm(life_exp ~ out_of_pocket + gdp_percap +country, data = global_health_expenditure)
summ(model_life_expectancy)

#Testing if out-of-pocket expenditure affects outputs in chronic diseases as diabetes
data_diabetes <-
  global_health_expenditure %>%
  filter(date==2000 |date== 2011 |date== 2019)%>%
  na.omit() %>%
  filter(diabetes != "-")

model_diabetes <- lm(diabetes ~ out_of_pocket + gdp_percap + country, data = data_diabetes)
summ(model_diabetes)

#Visual comparison of both models
plot_summs(model_life_expectancy,model_diabetes,  model.names = c("Life expectancy", "Diabetes"))


