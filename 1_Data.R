# Get Data sources
# Author: Laura Olivia Olvera

library(tidyverse)
library(ggplot2)
library(readxl)
library(readr)
library(dplyr)
library(hrbrthemes)
library(ggsci)
library(ggpubr)
library(rstatix)
library(tidytext) 
library(stringr) 
library(knitr) 
library(DT) 
library(tm)
library(tidyr)
library(wbstats)
library(syuzhet)
library(rvest)
library (xml2)
library(rvest) 
library(tidytext)
library(textdata)
library(lubridate)
# install.packages("wbstats")

# Set paths
# Change this when needed
path_raw <- "C:/Users/olivi/Box/FALL 2022/Data and Programming/Coding_sample/raw_data"
path_clean <- "C:/Users/olivi/Box/FALL 2022/Data and Programming/Coding_sample/clean_data"


## Data source 1
## Source: the World Bank API
## Variables: Data of life expectancy, out-of-pocket expenditure, GDP and health expenditure (% of GDP) by country

start_date = 2000
end_date = 2019

latam <- c("ABW","ARG","ATG","BHS","BLZ","BOL","BRA","BRB","CHL","COL","CRI","CUB","CUW","CYM",
           "DMA","DOM","ECU","GRD","GTM","GUY","HND","HTI","JAM","KNA","LCA","MAF","MEX","NIC",
           "PAN","PER","PRI","PRY","SLV","SUR","SXM","TCA","TTO","URY","VCT","VEN","VGB","VIR")

indicators <- c("SH.XPD.OOPC.CH.ZS", "NY.GDP.PCAP.CD", "SP.DYN.LE00.IN", "SH.XPD.CHEX.PP.CD")

get_state <- function(ind) {
  s <- wb_data(ind, start_date = start_date, end_date = end_date) %>%
    select(iso3c, country, date, ind)
}

data <- list()
i <- 1
for (ind in indicators) {
  data[[i]] <- get_state(ind)
  i = i + 1
}

df <- reduce(data, left_join, by = c("iso3c", "date", "country")) %>%
  filter(iso3c  %in% latam) %>%
  rename(`out_of_pocket` = `SH.XPD.OOPC.CH.ZS`, 
         `gdp_percap` = `NY.GDP.PCAP.CD`, 
         `life_exp` = `SP.DYN.LE00.IN`, 
         `health_exp_percap` = `SH.XPD.CHEX.PP.CD`)


## Data source 2
## Source: https://diabetesatlas.org/data/en/indicators/2/
## Variables: CSV of diabetes prevalence

df_diabetes = read.csv(file.path(path_raw,"IDF (age-adjusted-comparative-prevalence-of-diabetes---).csv")) %>%
  select(Country.Territory, X2000, X2011, X2021) %>%
  rename(`X2019` = `X2021`, 
         `country` = `Country.Territory`) %>%
  pivot_longer(cols = -c(country), names_to = "date", values_to = "diabetes")

df_diabetes$date <- substring(df_diabetes$date, 2)

df_diabetes <- df_diabetes %>%
  mutate_at('date', as.numeric)

df_final <- df %>% 
  left_join(df_diabetes, by = c("country", "date"))

write.csv(df_final, file.path(path_clean, "df_final.csv"), row.names = FALSE)


## Data source 3
## Disaggreation out-of-pocket expenditure, just Mexico 
## Source: https://www.inegi.org.mx/app/tabulados/pxwebclient/default.html?pxq=BISE_BISE_Ac0CuayZ_220713150914_7e479a70-05a1-4689-95be-2441cc671ad7

headers = c("expenditure_type", "country", "date", "expenditure")
health_exp_mex = read.csv(file.path(path_raw, "Interactivos20221130232334.csv"), header = F)
colnames(health_exp_mex) = headers

health_exp_mex <- health_exp_mex %>%
  filter(country!="") %>% 
  separate(expenditure_type, into = paste0('type', 1:3), sep = '[.]') %>%
  separate(type3, into = paste0('exp_type', 1:2), sep = '[(]') %>%
  select(date, exp_type1, expenditure) %>%
  mutate(exp_type1 = str_replace(exp_type1, "Consultas m?dicas ", "Doctor visits"),
         exp_type1 = str_replace(exp_type1, "Bienes de apoyo ", "Support items"),
         exp_type1 = str_replace(exp_type1, "Servicios de apoyo ", "Support care"),
         exp_type1 = str_replace(exp_type1, "Servicios hospitalarios ", "Inpatient care"),
         exp_type1 = str_replace(exp_type1, "Medicamentos y otros bienes ", "Drugs"))

  

write.csv(health_exp_mex, file.path(path_clean, "health_exp_mex.csv"), row.names = FALSE)


## Data source 4
## Source: https://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data
## ZIP directly from the url 
## Variables: Name and location of all private primary care centers in Mexico
## Note that the file is too heavy (20MB) and might take time to load

temp <- tempfile()
filename = "https://www.inegi.org.mx/contenidos/masiva/denue/2022_05/denue_00_62_0522_csv.zip"
csvfile = "conjunto_de_datos/denue_inegi_62_.csv"

download.file(filename,temp, mode = "wb")
unzip(temp, csvfile)
nlp_data <- read.table(csvfile, sep = ",", header = T)
unlink(temp)

nlp_data <- nlp_data %>%
  filter(nombre_act == 'Consultorios de medicina general del sector privado') %>%
  select(nom_estab, nombre_act, entidad, latitud, longitud)

write.csv(nlp_data, file.path(path_clean, "nlp_data.csv"), row.names = FALSE)


## Data source 5
## Get news from Google Search 
## Extract news from Google - Search "Mexico out-of-pocket")
## Sources: https://www.listendata.com/2020/12/web-scrape-google-news-with-r.html
## https://allanvc.github.io/post/2018-08-21-google_news_scraping/

news_outofpocket <- read_html("https://news.google.com/search?q=mexico%20out%20of%20pocket%20health&hl=en-US&gl=US&ceid=US%3Aen")

# Get headlines
headlines <- data.frame(title = news_outofpocket %>% 
                          html_nodes('.DY5T1d') %>% 
                          html_text())
# Get time of each row
prod <- html_nodes(news_outofpocket, ".SVJrMe")

time <- lapply(prod, function(x) {
  norm <- tryCatch(html_node(x, "time") %>% html_text(),
                   error=function(err) {NA})
})

# Create news data frame
time <- data.frame(time = do.call(rbind, time), stringsAsFactors = F)
news_nlp <- cbind(headlines, time)

write.csv(news_nlp, file.path(path_clean, "news_nlp.csv"), row.names = FALSE)
