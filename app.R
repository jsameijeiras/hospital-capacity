library(shiny)
library(readr)
library(magrittr)
library(dplyr)


print("libraries lodaded")

#GET("https://www.mscbs.gob.es/ciudadanos/prestaciones/centrosServiciosSNS/hospitales/docs/CNH_2019.xls", write_disk(tf <- tempfile(fileext = ".xls")))

#GET("https://github.com/jsameijeiras/hospital-capacity/blob/master/ine_poblacion_CCAA.xls?raw=true", write_disk(poblacion_ine <- tempfile(fileext = ".xls")))

print("get executed")

selected_date =  as.character(lubridate::today() - 1, format = "%d/%m/%Y")

print("read file cnh")

CNH_2019 <- read_csv("https://raw.githubusercontent.com/jsameijeiras/hospital-capacity/master/CNH_2019.csv")

X2915 <- read_csv("https://raw.githubusercontent.com/jsameijeiras/hospital-capacity/master/ine_poblacion_CCAA.csv")


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
    group_by(COMUNIDADES,CODAUTO) %>%
    summarise(total_camas = sum(NCAMAS)) %>%
    left_join(X2915) %>%
    select(CODAUTO,total_camas,Habitantes)

# User Interface
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

# Server logic
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
            select(COMUNIDADES,Habitantes,total_camas, camas_libres,casos,estimacion_casos, ratio_casos_habitantes, ocupacion_camas) %>%
            rename(CCAA = COMUNIDADES,
                     "Población" = Habitantes,
                     "Camas Totales" = total_camas,
                     "Camas Libres Estimadas" = camas_libres,
                     "Casos Detectados" = casos,
                     "Casos Reales Estimados" = estimacion_casos,
                     "Casos por cada 100 habitantes" = ratio_casos_habitantes,
                     "Porcentaje de Ocupacion de Camas Libres" = ocupacion_camas)
        
        filtered
    })
}

# Run the application
shinyApp(ui = ui, server = server)
