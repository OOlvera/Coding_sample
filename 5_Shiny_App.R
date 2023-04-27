library(tidyverse)
library(tidytext)
library(shiny)
library(plotly)
library(sf)
library(rsconnect)
library(stringr) 
library(ggplot2)
library(readxl)
library(readr)
library(dplyr)
library(magrittr)
#devtools::install_github("diegovalle/mxmaps")
library(mxmaps)
library(syuzhet)



path_clean <- "C:/Users/stiar/OneDrive/Documents/data_and_programming_II/final-project-karla-olivia-astrid/clean_data/"
path_raw <- "C:/Users/stiar/OneDrive/Documents/data_and_programming_II/final-project-karla-olivia-astrid/raw_data/"
path_plot <- "C:/Users/stiar/OneDrive/Documents/data_and_programming_II/final-project-karla-olivia-astrid/plots/"

ui <- fluidPage(
  fluidRow(
    column(width = 10,
           tags$h3("Out of pocket health distribution"),
           tags$h5("What is the effect of out-of-pocket expenditure in health outcomes in Latin America and the Caribbean?"),
           tags$hr()
    )
  ),
  fluidRow(
    column(width = 12,
           tags$h4("Out-of-pocket distribution in Latin America and the Caribbean"),
           plotlyOutput("bar_plot")
           )
  ),
  fluidRow(  
    column(width = 12,
           selectInput(inputId = "correlation",
                       label = "Correlation with other variable",
                       choices = c("Diabetes","Life expectancy"))
           )
    ),
    column(width = 12,
           plotlyOutput("correlations"),
           tags$hr()
  ),
  fluidRow(
    column(width = 12,
           tags$h4("Out-of-pocket expenditure in Mexico"),
           sliderInput("date",
                       label = "Select a year",
                       min = 2015, max = 2020, value = 100),
           plotlyOutput("hist_mexico"),
           tags$hr()
      )
  ),
  fluidRow(  
    column(width = 12,
           tags$h4("Distribution of private primary care clinics in Mexico"),
           selectInput(inputId = "map",
                       label = "Select a map",
                       choices = c("Absolute values","Controlled by population"))
    ),
    column(width = 12,
           plotlyOutput("maps"),
           tags$hr()
           )
    ),
    fluidRow(  
      column(width = 12,
             tags$h4("Sentiment analysis"),
             imageOutput(outputId = "wordclouds")
    )
  )
)

server <- function(input, output) {
  plot_format <-  theme(panel.background = element_blank(), 
                        plot.title = element_text(size=15, vjust = 2.5, hjust = 0.5),
                        legend.title = element_blank(),
                        legend.position="none", 
                        axis.text.x = element_text(size = 10),
                        axis.text.y = element_text(size = 7),
                        axis.title.x = element_text(size = 11, margin = margin(r = 15)),
                        axis.title.y = element_text(size = 11, margin = margin(r = 15)),
                        axis.line = element_line(colour = "black", size = .5), 
                        plot.caption=element_text(size=9, hjust=0)) 
  
  df_final <- read.csv(file.path(path_clean,"df_final.csv"))
  
  df_final <- transform(df_final, diabetes = as.numeric(diabetes))
  
  output$bar_plot <- renderPlotly({
    
    out_of_pocket_plot <- df_final %>%
      filter(date==2019) %>%
      drop_na(out_of_pocket) %>%
      ggplot(aes(x = reorder(country, out_of_pocket), y = out_of_pocket, fill = ifelse(country == "Mexico", "Highlighted", "Normal"))) +
      geom_bar(position = "dodge", stat = "identity",  
               width = 0.7) +
      labs(title = "Out-of-pocket expenditure (2019)", 
           y = "Percentage (%)", 
           x = "", 
           caption = "Source: World Bank")  +
      plot_format +
      coord_flip()
  })
  
  output$correlations <- renderPlotly({
    if (input$correlation == "Diabetes"){
      diabetes_plot <- df_final %>%
        filter(date==2019) %>%
        drop_na(out_of_pocket) %>%
        drop_na(diabetes) %>%
        ggplot(aes(x = diabetes, y = out_of_pocket)) + 
        geom_point() +
        geom_text(hjust = 0, nudge_x = 0.1, size = 2, aes(label = iso3c)) +
        geom_smooth(method = lm, color = "darkred") +
        labs(title = "Diabetes and out-of-pocket expenditure (2019)",
             y = "Percentage of out-of-pocket expenditure", 
             x = "Percentage of population with diabetes", 
             caption = "Source: World Bank")  +
        plot_format
      
    }
    else if (input$correlation == "Life expectancy"){
      life_expectancy_plot <- df_final %>%
        filter(date==2019) %>%
        drop_na(out_of_pocket) %>%
        ggplot(aes(x = life_exp, y = out_of_pocket)) + 
        geom_point() +
        geom_text(hjust = 0, nudge_x = 0.1, size = 2, aes(label = iso3c)) +
        geom_smooth(method = lm, color = "darkred") + 
        #scale_fill_continuous(low='yellow', high='#de2d26') +
        labs(title = "Relation between life expectancy and out-of-pocket expenditure (2019)",
             y = "Percentage of out-of-pocket expenditure", 
             x = "Life expectancy", 
             caption = "Source: World Bank")  +
        plot_format 
    }
  })
  output$hist_mexico <- renderPlotly({
    health_exp_mex <- read.csv(file.path(path_clean,"health_exp_mex.csv"))
    #years <- c(2015:2020)
    for (i in input$date ) {
      gap_to_plot <- 
        health_exp_mex %>%
        filter(date == i)
      
      my_plot <-
        ggplot(data = gap_to_plot, aes(y = expenditure, x = date)) +
        geom_col(aes(fill = exp_type1), width = 0.7)+
        geom_text(aes(label = expenditure, group = exp_type1), color = "white", position = position_stack(vjust = 0.5), size = 4) +
        guides(fill = guide_legend(title = "Distribution"))+
        labs(title = "How is out-of-pocket expenditure distributed in Mexico?", 
             y = "Percentage", 
             x = "Year", 
             caption = "Source: National Institute of Statistics and Geography (INEGI)")  +
        scale_fill_manual(values = c("#003f5c", "#444e86","#955196","#dd5182","#ff6e54")) +
        scale_x_continuous(breaks= input$date)+
        theme(legend.title = element_blank(),
              legend.position="right",
              panel.background = element_blank(),
              axis.line = element_line(),
        )
    } 
    return(my_plot)
  })
  
  output$maps <- renderPlotly({
    nlp_data <- readr::read_csv(file.path(path_clean, "nlp_data.csv"), locale = readr::locale(encoding = "latin1"))
    primary_care <- nlp_data %>%
      group_by(entidad) %>%
      summarise(total_pc = n())
    
    primary_care$ADM1_ES <- str_to_title(primary_care$entidad)


    primary_care <- primary_care %>%
      mutate(ADM1_ES = str_replace(ADM1_ES, "Veracruz De Ignacio De La Llave", "Veracruz de Ignacio de la Llave"),
             ADM1_ES = str_replace(ADM1_ES, "Michoacán De Ocampo", "Michoacán de Ocampo"),
             ADM1_ES = str_replace(ADM1_ES, "Coahuila De Zaragoza", "Coahuila de Zaragoza"),
             ADM1_ES = str_replace(ADM1_ES, "Ciudad De México", "Distrito Federal"),
             ADM1_ES = str_replace(ADM1_ES, "Querétaro", "Querétaro de Arteaga"))

    if (input$map == "Absolute values"){  
      mexico_shape <- st_read(file.path(path_raw, "mexican_states_shapefile.shp"))
      primary_care_final<- left_join(mexico_shape, primary_care, by='ADM1_ES')
    
      map_format <- scale_fill_gradientn(colours = hcl.colors(3, "GnBu", rev = TRUE),n.breaks = 4)
      
      ggplot() +
        geom_sf(data = primary_care_final, aes(fill = total_pc)) +
        map_format +
        labs(title="Total private primary care centers", 
             fill = "Total") +
        theme_void()
      
    }
   
    else if (input$map == "Controlled by population"){
      mexico_shape <- st_read(file.path(path_raw, "mexican_states_shapefile.shp"))
      primary_care_final<- left_join(mexico_shape, primary_care, by='ADM1_ES')
      map_format <- scale_fill_gradientn(colours = hcl.colors(3, "GnBu", rev = TRUE),n.breaks = 4)
      
      pop_data <- df_mxstate_2020 %>%
        mutate(state_name_official = str_replace(state_name_official, "Querétaro", "Querétaro de Arteaga"),
               state_name_official = str_replace(state_name_official, "Ciudad de México", "Distrito Federal"))
      
      pop_data$ADM1_ES <- pop_data$state_name_official
      
      primary_care_final<- primary_care_final %>%
        left_join(pop_data, by='ADM1_ES') %>%
        mutate(total_pc_r = (total_pc/pop)* 100)
      
      ggplot() +
        geom_sf(data = primary_care_final, aes(fill = total_pc_r)) +
        map_format +
        labs(title="Private primary care centers (relative to population)",
             fill = "Percentage") +
        theme_void()
      
    }
  })
  
  output$wordclouds <- renderImage({ 
    ofile <- (file.path(path_plot,"NRC_plot.png"))
    list(src = ofile,
         contentType = "image/png")
  }, deleteFile = FALSE)
    
}

shinyApp(ui = ui, server = server)
