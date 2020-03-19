# Capacidad de Respuesta COVID-19 en España

Esta aplicación pretende ser una forma interactiva para estudiar la capacidad de los hospitales españoles de reaccionar ante la crisis de coronavirus en España. Hay dos cuestiones a priori que es importante tener en cuenta: 

​	(i) Se estudian el número de camas disponibles a fecha diciembre 2018 y  por lo tanto todas las medidas extraordinarias, tras el Real Decreto 463/2020, llevadas a cabo por las distintas administraciones no se contemplan. 

​	Además, (ii) los datos de ocupación de hospitales no se exponen desde 2005 por el INE y por lo tanto la estimación de camas reales es complicado. 

Teniendo en cuenta estas limitaciones y aceptando que son varias las desconocidas a la hora de ejecutar este análisis la intención de esta herramienta es la de proveer al usuario con la capacidad de interpretar los datos de la forma más rápida y explorar distintos escenarios. 

La idea surge con motivo de este [tweet]( https://twitter.com/victorianoi/status/1240363156154671104?s=20  ) y este [análisis]( https://docs.google.com/spreadsheets/u/1/d/1DlC5kh9ve-Giv96XTnhCiB6vQAkQCjl5bDSjT68Q0FY/htmlview#) . Además especial agradecimiento a  [@martasvm](https://twitter.com/Martasvm),  [@victorianoi ](https://twitter.com/victorianoi) y  [@jlantunez](https://twitter.com/jlantunez)  por la información almacenada en este [directorio](https://www.notion.so/Fighting-Coronavirus-with-Tech-Data-ce8f69bdbba44924829c2968a2ffae73). 

A continuación se describen las variables que se observan en la aplicación:

**CCAA** = Nombre de la Comunidad Autónoma.
**Población** = Numero de habitantes en dicha Comunidad Autónoma.
**Camas Totales** = Numero de camas totales reportadas por el Ministerio de Sanidad, excluyendo camas dedicadas a Psiquiatría	
**Camas Libres Estimadas** = Numero de camas que quedan disponibles tras aplicar el porcentaje de camas libres.
**Casos Detectados** = Casos reportados por el Ministerio de Sanidad el día anterior. Fuente: DATADISTA
**Casos Reales Estimados** = Casos estimados en base al número de muertes asumiendo que de media esas personas tardan 17 días en morirse y los casos en España se doblan cada 2.6 días.
**Casos por cada 100 habitantes** = Porcentaje de casos en estimado en base a la población de esa Comunidad Autónoma.
**Porcentaje de Ocupación de Camas Libres** = Porcentaje de camas ocupadas que se encontraban disponibles para pacientes de COVID-19. Vienen determinadas por cuantas camas suelen estar ocupadas por otras enfermedades. Ese dato se puede modelar en la aplicación.