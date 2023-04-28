## Text Processing Analysis
## Author: Laura Olivia Olvera

library(tidyverse)
library(ggplot2)
library(tidytext)
library(stringr) 
#install.packages("wordcloud")
library(wordcloud)
#install.packages("sf")
library(sf)
library(rnaturalearth)
library(devtools)
#devtools::install_github("diegovalle/mxmaps")   
library(mxmaps)
library(syuzhet)
#install.packages("stopwords")
library(stopwords)
library(dplyr)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("textdata")
library(textdata)

## Set paths
## Change this when needed
path_raw <- "C:/Users/olivi/Box/FALL 2022/Data and Programming/Coding_sample/raw_data"
path_clean <- "C:/Users/olivi/Box/FALL 2022/Data and Programming/Coding_sample/clean_data"
path_plot <- "C:/Users/olivi/Box/FALL 2022/Data and Programming/Coding_sample/plots"


## From Data source 4
## Objective: Text processing of the primary care centers in Mexico
## Source: Mexican National Institute of Statistics

nlp_data <- read.csv(file.path(path_clean,"nlp_data.csv"))

## Delete stop words and additional common words not useful for the analysis. The words are in Spanish
custom_stop_words <- data.frame(word = stopwords("es"), lexicon = "custom")


uni_sw <- data.frame(word = c("consultorio","medico","nombre", "medicina", "médico", "consultorios", "medica", "consultorios"))

nlp_words <- nlp_data %>% 
  unnest_tokens(word, nom_estab) %>%
  anti_join(custom_stop_words) %>%
  anti_join(uni_sw, by = "word") %>%
  select(word) %>%
  count(word, sort = TRUE) %>% 
  ungroup()


## Visualizations of most common names/words for private primary care centers
## 1. WordCloud
## Source: https://richpauloo.github.io/2017-12-29-Using-tidytext-to-make-word-clouds/
## The WordCloud shows that most of the primary care centers are located in pharmacies ("farmacia")
## This is important for healthcare policies

pal <- brewer.pal(8,"Dark2")
png(file.path(path_plot,"wordcloud_plot.png"))
nlp_words %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 40, colors=pal))
dev.off()


## 2. Maps
## Data wrangling based on the NLP data
primary_care <- nlp_data %>%
  group_by(entidad) %>%
  summarise(total_pc = n())

primary_care$ADM1_ES <- str_to_title(primary_care$entidad) 
primary_care$ADM1_ES <-str_trim(primary_care$ADM1_ES, side = c("right"))


primary_care <- primary_care %>%
  mutate(ADM1_ES = str_replace(ADM1_ES, "Veracruz De Ignacio De La Llave", "Veracruz de Ignacio de la Llave"),
         ADM1_ES = str_replace(ADM1_ES, "Michoacán De Ocampo", "Michoacán de Ocampo"),
         ADM1_ES = str_replace(ADM1_ES, "Coahuila De Zaragoza", "Coahuila de Zaragoza"),
         ADM1_ES = str_replace(ADM1_ES, "Ciudad De México", "Distrito Federal"),
         ADM1_ES = str_replace(ADM1_ES, "Querétaro", "Querétaro de Arteaga"))

## Map 1: Total number of private primary care centers by state (absolute numbers) 

mexico_shape <- st_read(file.path(path_raw, "mexican_states_shapefile.shp"), options = "ENCODING=UTF-8")

primary_care_final<- left_join(mexico_shape, primary_care, by='ADM1_ES')

map_format <- scale_fill_gradientn(colours = hcl.colors(3, "GnBu", rev = TRUE),n.breaks = 4)

png(file.path(path_plot,"map_1.png"))
ggplot() +
  geom_sf(data = primary_care_final, aes(fill = total_pc)) +
  map_format +
  labs(title="Total private primary care centers", 
       fill = "Total") +
  theme_void()
dev.off()


## Map 2: Total number of private primary care centers by state (controlled by population) 
## population data from the library mxmaps
pop_data <- df_mxstate_2020 %>%
  mutate(state_name_official = str_replace(state_name_official, "Querétaro", "Querétaro de Arteaga"),
         state_name_official = str_replace(state_name_official, "Ciudad de México", "Distrito Federal"))

pop_data$ADM1_ES <- pop_data$state_name_official

primary_care_final<- primary_care_final %>%
  left_join(pop_data, by='ADM1_ES') %>%
  mutate(total_pc_r = (total_pc/pop)* 100)

png(file.path(path_plot,"map_2.png"))
ggplot() +
  geom_sf(data = primary_care_final, aes(fill = total_pc_r)) +
  map_format +
  labs(title="Private primary care centers (relative to population)",
       fill = "Percentage") +
  theme_void()
dev.off()


## From Data source 5
## Objective: NRC Sentiment Analysis of Mexican news about out-of-pocket
news_nlp <- read.csv(file.path(path_clean,"news_nlp.csv"))

# NRC as the chosen sentiment package
sentiment_nrc <- get_sentiments("nrc") %>%
  rename(nrc = sentiment)

text_df <- as.character(news_nlp$title)
text_df <- tibble(text = text_df)

text_df <- text_df %>%
  unnest_tokens(word_tokens, text, token="words") %>%
  anti_join(stop_words, by = c("word_tokens" = "word")) %>%
  left_join(sentiment_nrc, by = c("word_tokens" = "word")) %>%
  filter(nrc != "positive" & nrc != "negative")

## Plotting the NRC results
png(file.path(path_plot,"NRC_plot.png"))
ggplot(data = filter(text_df, !is.na(nrc))) +
  geom_histogram(aes(nrc), stat = "count", fill="Navy") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Overall sentiment analysis: Mexican news about out-of-pocket",
       subtitle = "NRC method") +
  theme_minimal()
dev.off()
