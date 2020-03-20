library(shiny)
library(readr)
library(magrittr)
library(dplyr)
library(kableExtra)
library(knitr)
library(ggplot2)

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
    titlePanel("Capacidad de Respuesta COVID-19 en España"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("mortalityrate", "Ratio de Fallecidos", 0, 0.5, 0.03),
            sliderInput("severityyrate", "Ratio de Hospitalizados", 0, 1, 0.23),
            sliderInput("originalocc", "% de Camas libres en Hospital", 0, 1, 0.65)
        ),
        
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("Gráfico", plotOutput("plot")),
                              tabPanel("Tabla", tableOutput("results")
                                       )
                              )
                  )
    )
)

# Server logic
server <- function(input, output) {
    
    
    
    output$results <- function(){
        
        analisis <- habitante_camas_df %>%
            left_join(casos_fallecidos_covid_CCAA, by = c("CODAUTO" = "cod_ine")) %>% 
            mutate(estimacion_casos = ifelse(fallecidos > 0, 
                                             (fallecidos/input$mortalityrate[1])*2^4,
                                             casos),
                   camas_libres = total_camas*input$originalocc[1]) %>%
            mutate(ratio_casos_habitantes = (estimacion_casos/Habitantes) * 100,
                   ocupacion_camas = (estimacion_casos * input$severityyrate[1])/camas_libres * 100) %>%
            arrange(desc(ratio_casos_habitantes)) %>%
            select(COMUNIDADES,Habitantes,total_camas, camas_libres,casos,estimacion_casos, ratio_casos_habitantes, ocupacion_camas)
            
        analisis%>%
            rename("Comunidad Autónoma" = COMUNIDADES,
                     "Población" = Habitantes,
                     "Camas Totales" = total_camas,
                     "Camas Libres Estimadas" = camas_libres,
                     "Casos Detectados" = casos,
                     "Casos Reales Estimados" = estimacion_casos,
                     "Casos por cada 100 habitantes" = ratio_casos_habitantes,
                     "Porcentaje de Ocupacion de Camas Libres" = ocupacion_camas) %>%
            knitr::kable("html", digits = 1, format.args = list(big.mark = ".", 
                                                                decimal.mark = ",",
                                                                scientific = FALSE)) %>%
            kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
            footnote(general = "Codigo original de @jsmeijeiras. Datos del Ministerio de Sanidad y Datadista")
        
    }
    
    output$plot <- renderPlot({
        
        analisis <- habitante_camas_df %>%
            left_join(casos_fallecidos_covid_CCAA, by = c("CODAUTO" = "cod_ine")) %>% 
            mutate(estimacion_casos = ifelse(fallecidos > 0, 
                                             (fallecidos/input$mortalityrate[1])*2^4,
                                             casos),
                   camas_libres = total_camas*input$originalocc[1]) %>%
            mutate(ratio_casos_habitantes = (estimacion_casos/Habitantes) * 100,
                   ocupacion_camas = (estimacion_casos * input$severityyrate[1])/camas_libres * 100) %>%
            arrange(desc(ocupacion_camas)) %>%
            select(COMUNIDADES,Habitantes,total_camas, camas_libres,casos,estimacion_casos, ratio_casos_habitantes, ocupacion_camas)
        
        ggplot(data=analisis, aes(x=reorder(COMUNIDADES, ocupacion_camas), y = ocupacion_camas)) +
            geom_bar(stat="identity", fill = "#7ebec0") +
            geom_hline(yintercept=100, linetype="dashed", color = "red") +
            coord_flip() +
            theme_minimal() +
            labs(x = "", y = "% Ocupacion Camas disponibles")
            
            
        
    })
}

# Run the application
shinyApp(ui = ui, server = server)
