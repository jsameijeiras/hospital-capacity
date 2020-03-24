library(shiny)
library(readr)
library(magrittr)
library(dplyr)
library(kableExtra)
library(knitr)
library(ggplot2)
library(deSolve)


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
            sliderInput("mortalityrate", "Ratio de Fallecidos", 0.01, 0.5, 0.03),
            sliderInput("severityyrate", "Ratio de Hospitalizados", 0, 1, 0.23),
            sliderInput("originalocc", "% de Camas libres en Hospital", 0, 1, 0.65),
            sliderInput("efe", "% of recovered that can get sick again", 0, 1, 0.96),
            sliderInput("r0", "R0 of the COVID-19", 2.5, 4.5, 3.5)
        ),
        
        mainPanel(tabsetPanel(type = "tabs",
                              tabPanel("Gráfico", plotOutput("plot")),
                              tabPanel("Tabla", tableOutput("results")),
                              tabPanel("Model SIRS", plotOutput("sirs"))
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
            footnote(general = "Codigo original de @jsameijeiras. Datos del Ministerio de Sanidad y Datadista")
        
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
            labs(x = "", y = "% Ocupacion Camas Disponibles")
        
    })
    
    output$sirs <- renderPlot({
        
        #The population is divided into compartments, with the assumption that every individual in the same compartment has the same characteristics
        
        
        #S = suceptible
        #I = Infectious 
        #R = Recovered
        #\xi is the rate which recovered individuals return to the susceptible statue due to loss of immunity.
        
        
        sirs <- function(time, state, parameters) {
            
            with(as.list(c(state, parameters)), {
                
                n = S + I + R
                
                dS <- -(beta * S * I)/n + efe*R
                dI <-  (beta * S * I)/n - gamma * I 
                dR <-                     gamma * I - efe*R
                
                return(list(c(dS, dI, dR)))
            })
        }
        
        derivative_calc_func=function(t, x, vparameters){
            S = x[1]  
            I = x[2]  
            R = x[3]  
            
            with(as.list(vparameters),{
                
                npop = S+I+R   
                
                
                dS = -beta*S*I/npop  + efe*R          
                dI = +beta*S*I/npop - gamma*I  
                dR = +gamma*I - efe*R                  
                
                vout = c(dS,dI,dR)
                list(vout)
            })
        }
        
        npop = 2700000
        I_0 = 100
        R_0 = 0
        S_0 = npop-I_0-R_0
        
        ##################################################################################
        # now the parameters of the model.  Note that in some posts on sherrytowers.com
        # I refer to the recovery rate as k (here it is gamma), and the transmission
        # rate as b (here is is beta).
        #
        # tbegin  is the begin time for the simulation (here we assume units of days)
        # tend    is the time we want the simulation to end
        #
        # gamma=1/3   The recovery rate in units 1/days.  Note that 1/gamma is the
        #             average recovery period.  For influenza,  this is quite short, but
        #             for other diseases it can be quite long
        # R0          This is the reproduction number of the disease.  For pandemic influenza
        #             this has been found to be around 1.5
        # beta        The transmission rate.  For the SIR model, mathematical analysis of
        #             the model yields the relationship R0=beta/gamma
        #             Thus if we know R0 and we know gamma, we can calculate beta
        #
        # vt          is the vector of time steps at which we want model estimates
        ##################################################################################
        tbegin = 0
        tend   = 300
        vt = seq(tbegin,tend,1)  
        
        #beta=1.99459e-06   
        
        
        # fill named vectors with our parameter values and initial conditions
        vparameters = c(gamma=1.82E-02,beta=(input$r0[1]*gamma)/npop,efe = input$efe[1])
        inits = c(S=S_0,I=I_0,R=R_0)
        
        # uses the 4th order Runge-Kutta method to solve the system of ODE's described
        
        solved_model <-  as.data.frame(ode(inits, vt, derivative_calc_func, vparameters)) %>%
            mutate(date =  as.Date("05-03-2020", format = "%d-%m-%Y" )+ time) 
        
        
        ggplot(solved_model, aes(x=date, y=S)) +
            geom_line() + 
            xlab("Infected at Spain") +
            scale_x_date(date_labels = "%b %d", date_breaks = "20 day") +
            theme_minimal()
        
    })
    
    
    
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
