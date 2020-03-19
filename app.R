#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(httr)

GET("https://www.mscbs.gob.es/ciudadanos/prestaciones/centrosServiciosSNS/hospitales/docs/CNH_2019.xls", write_disk(tf <- tempfile(fileext = ".xls")))

selected_date =  as.character(lubridate::today() - 1, format = "%d/%m/%Y")

CNH_2019 <- read_excel(tf,  col_types = c("text", "text", "text", 
                                                    "text", "text", "text", "text", "text", 
                                                    "text", "text", "text", "text", "text", 
                                                    "numeric", "text", "text", "text", 
                                                    "text", "text", "text", "text", "text", 
                                                    "text", "text", "text", "text", "text", 
                                                    "text", "text", "text", "text", "text", 
                                                    "text", "text", "text", "text", "text", 
                                                    "text", "text", "text", "text", "text", 
                                                    "text"))

X2915 <- read_excel("C:/Users/jose/Downloads/2915.xls")


casos_covid_CCAA <- read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_casos.csv") %>%
    select(cod_ine,CCAA,selected_date)  %>% 
    rename(casos = selected_date)

fallecidos_covid_CCAA <- read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos.csv") %>%
    select(cod_ine,CCAA,selected_date) %>% 
    rename(fallecidos = selected_date)

casos_fallecidos_covid_CCAA <- casos_covid_CCAA %>%
    left_join(fallecidos_covid_CCAA) %>% 
    mutate(fatality_rate = fallecidos/casos * 100) 

habitante_camas_df <- CNH_2019 %>% 
    filter(`FINALIDAD ASISITENCIAL` != "PSIQUIÁTRICO") %>%
    group_by(COMUNIDADES,CODAUTO) %>%
    summarise(total_camas = sum(NCAMAS)) %>%
    left_join(X2915) %>%
    select(CODAUTO,total_camas,Habitantes)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Como de capacitados estaban nuestros hospitales"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("mortalityrate", "Ratio de Fallecidos", 0, 0.5, 0.03),
            sliderInput("severityyrate", "Ratio de Hospitalizados", 0, 1, 0.23),
            sliderInput("originalocc", "% de Camas libres en Hospital", 0, 1, 0.65),
            selectInput("countryInput", "Country",
                        choices = c("SPAIN"))
        ),
        
        mainPanel(tableOutput("results"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$results <- renderTable({
        filtered <-
            habitante_camas_df %>%
            left_join(casos_fallecidos_covid_CCAA, by = c("CODAUTO" = "cod_ine")) %>% 
            mutate(estimacion_casos = (fallecidos/input$mortalityrate[1])*2^4,
                   camas_libres = total_camas*input$originalocc[1]) %>%
            mutate(ratio_casos_habitantes = (estimacion_casos/Habitantes) * 100,
                   ocupacion_camas = (estimacion_casos * input$severityyrate[1])/camas_libres * 100)  %>%
            arrange(desc(ratio_casos_habitantes)) %>%
            select(COMUNIDADES,Habitantes,total_camas, camas_libres,casos,estimacion_casos, ratio_casos_habitantes, ocupacion_camas)
        
        filtered
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
