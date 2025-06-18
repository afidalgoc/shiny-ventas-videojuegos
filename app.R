#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Cargar paquetes
library(shiny)
library(bslib) #para el tema estético
library(dplyr)
library(ggplot2)
library(plotly)
library(magrittr) #layout de plotly

# Cargar datos
vj <- read.csv("VideoGame_Sales.csv")


# Define UI para la aplicación Shiny
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "pulse"), #tema con letras violetas
  
  #Título de la app
  titlePanel("Análisis de ventas de videojuegos"),
  # Configuración de las pestañas
  tabsetPanel(
    
  #Pestaña General: Presentación
    tabPanel("Presentación",
             img(src = "video-games-header.jpg", width = 600, heigth = 300,
                 style = "text-align: center; margin-top: 20px;"),
             h2("Presentación"),
             p("La producción de videojuegos es una de las industrias más
               fructíferas en la actualidad, con un rápido crecimiento a partir 
               de finales de los años 80 del siglo pasado, debido a la inversión 
               en el desarrollo de productos tecnológicos por su 
               rentabilidad en ascenso. En los últimos años, la popularidad de 
               los mismos como forma de entretenimiento se ha visto en alza 
               gracias al surgimento de la figura de los streamers y jugadores 
               de videojuegos profesionales, así como la creación de comunidades
               de jugadores a través de internet."),
             p("Con el fin de obtener más información sobre las ventas de
               videojuegos, se ha decidido trabajar con la base de datos
               VideoGame_Sales.csv, obtenida a través de ",
               a("Kaggle", href = "https://www.kaggle.com/", target = "_blank"),
               "."),
             #Botón de contacto
             p("Para más información, se proporcionan la siguiente forma de
               contacto:"),
             tags$div(
               tags$a(
                 tags$button(
                   tags$span("", style = "font-size: 20px;"),
                   " Contacto",
                   class = "glyphicon glyphicon-envelope"),
                 href = "mailto:afidalgoclase@gmail.com"),
               style = "text-align: center; margin-top: 20px;")
             
             ),
    # Pestaña 1: Resúmenes Numéricos
    tabPanel("Resúmenes Numéricos",
             h2("Resúmenes Numéricos"),
             p("A continuación, se muestra una porción de la base de datos con 
             la que se ha trabajado, seguida del resumen de los mismos."),
             
             #Tabla head(datos) y summary de la base de datos
             tableOutput("head_vj"),
             verbatimTextOutput("resumen_texto"),
             
             p("Como se puede observar, se trata de una base de datos con 5 
             variables caracter y 5 variables numéricas. Se incluyen los datos 
             de las ventas de videojuegos globales, en Europa, Japón, Norteamérica,
             y otros territorios no especificados teniendo en cuenta la plataforma 
             o consola, el año de publicación, la empresa y género temático de 
             los mismos."),
             
             p("En las pestañas inferiores se muestrán algunas de las 
             características de los datos por territorio."),
             
             # Pestañas para separar por territorio
             tabsetPanel(
               tabPanel("Ventas globales",
                        h3("Ventas globales"),
                        p("A continuación, se muestran una serie de resúmenes 
                          numéricos para las ventas globales"),
                        
                        # Botones de radio (global)
                        radioButtons("resumen_opcion", "Seleccionar resumen numérico:",
                                     choices = c("Los 10 años con más ventas globales",
                                                 "Los 10 Años con menos ventas globales",
                                                 "Ventas por género",
                                                 "Ventas por empresa (top 10)",
                                                 "Videojuegos más populares"),
                                     selected = "Los 10 años con más ventas globales"),
                        helpText("Selecciona los datos que deseas visualizar."),
                        tableOutput("resumen_table"),
                        
                        p("Tal y como se puede observar, los años con un mayor 
                        número de ventas totales fueron 2008 y 2009 y los años 
                        con un menor número de ventas fueron 2017 y 2020. 
                        Los géneros más populares son la Acción, los Deportes 
                        y los Shooter. En lo que a marcas respecta, el mayor 
                        número de ventas se concentra en Nintendo, Electronic 
                        Arts y Activision. Destacan títulos como Wii Sports, 
                        Grand Theft Auto V y Super Mario Bros.")
               ),
               
               tabPanel("Ventas en Japón",
                        h3("Ventas en Japón"),
                        p("Se han considerado los mismos resúmenes para Japón."),
                        
                        # Botones de radio (JP)
                        radioButtons("resumen_opcion2", "Seleccionar resumen numérico:",
                                     choices = c("Los 10 años con más ventas en Japón",
                                                 "Ventas por género",
                                                 "Ventas por empresa (top 10)",
                                                 "Videojuegos más populares"),
                                     selected = "Los 10 años con más ventas en Japón"),
                        helpText("Selecciona los datos que deseas visualizar."),
                        tableOutput("resumen_table2"),
                        
                        p("En el caso japonés, el mayor número de ventas se 
                        alcanzó en 2006 y 2009 y los géneros más populares son 
                        el Role-Playing, la Acción y los Deportes. Destacan como 
                        títulos de videojuegos Pokemon Red/Pokemon Blue, Pokemon 
                        Gold/Pokemon Silver y Super Mario Bros. Por otro lado, 
                        en lo que a las empresas con mayor número de ventas se 
                        refiere, el top 3 está conformado por empresas
                        nacionales: Nintendo, Namco Bandai Games y Konami 
                        Digital Entertainment.")
               ),
               
               tabPanel("Ventas en Europa",
                        h3("Ventas en Europa"),
                        p("Los siguientes datos muestran las particularidades 
                          en la Unión Europea."),
                        
                        # Botones de radio (UE)
                        radioButtons("resumen_opcion3", "Seleccionar resumen numérico:",
                                     choices = c("Los 10 años con más ventas en la UE",
                                                 "Ventas por género",
                                                 "Ventas por empresa (top 10)",
                                                 "Videojuegos más populares"),
                                     selected = "Los 10 años con más ventas en la UE"),
                        helpText("Selecciona los datos que deseas visualizar."),
                        tableOutput("resumen_table3"),
                        
                        p("En este caso, se observa una mayor consonancia entre
                        los datos europeos y los datos globales, siendo los dos
                        años con un mayor número de ventas 2008 y 2009, los 
                        géneros más consumidos la Acción, los Deportes y los
                        Shooters y los títulos más populares Wii Sports, 
                        Grand Theft Auto V y Mario Kart Wii. De nuevo, las 
                        empresas dominantes son Nintendo, Electronic Arts y 
                          Activision.")
               ),
               
               tabPanel("Ventas en Norteamérica",
                        h3("Ventas en Norteamérica"),
                        p("Los siguientes datos muestran las particularidades
                          de las ventas en Norteamérica."),
                        
                        # Botones de radio (NA)
                        radioButtons("resumen_opcion4", "Seleccionar resumen numérico:",
                                     choices = c("Los 10 años con más ventas en Norteamérica",
                                                 "Ventas por género",
                                                 "Ventas por empresa (top 10)",
                                                 "Videojuegos más populares"),
                                     selected = "Los 10 años con más ventas en Norteamérica"),
                        helpText("Selecciona los datos que deseas visualizar."),
                        tableOutput("resumen_table4"),
                        
                        p("Hay una gran consonancia con los datos globales,
                          siendo de nuevo 2008 y 2009 los años con más ventas y 
                          los géneros más populares la Acción, los Deportes y
                          los Shooter. Particularmente, los tres juegos con
                          mayor número de ventas son  Wii Sports, Super Mario
                          Bros y Duck Hunt."),
               ),
               
               tabPanel("Ventas en otros territorios",
                        h3("Ventas en otros territorios"),
                        p("Esta sección corresponde a los datos de otros
                          territorios no especificados."),
                        
                        # Botones de radio (otros)
                        radioButtons("resumen_opcion5", "Seleccionar resumen numérico:",
                                     choices = c("Los 10 años con más ventas en otros territorios",
                                                 "Ventas por género",
                                                 "Ventas por empresa (top 10)",
                                                 "Videojuegos más populares"),
                                     selected = "Los 10 años con más ventas en otros territorios"),
                        helpText("Selecciona los datos que deseas visualizar."),
                        tableOutput("resumen_table5"),
                        
                        p("Se puede observar que los años con mayores ventas son
                          2008 y 2007, siendo de nuevo los géneros más populares
                          la Acción, los Deportes y los Shooter. Cabe destacar
                          que se trata de las únicas localizaciones en las que 
                          Nintendo no ocupa el primer lugar como empresa líder
                          en ventas, sino Electronic Arts, y los videojuegos
                          más vendidos son Grand Theft Auto: San Andreas, 
                          Wii Sports y Grand Theft Auto V.")
               )
             )
    ),
    
    # Pestaña 2: Análisis de datos
    tabPanel("Análisis de datos",
             h2("Análisis de datos"),
             p("En este apartado de Análisis de datos, se realizará una 
               regresión lineal para analizar la relación entre las ventas en 
               los distintos territorios y las ventas globales en el sector 
               de los videojuegos. Cabe destacar que la variable Global_Sales
               surge de la suma de las demás variables de ventas."),
             h3("Análisis de regresión"),
             # Checkbox group
             checkboxGroupInput("variables_checkbox",
                                "Seleccionar variables:",
                                choices = c("Ventas Europa" = "EU_Sales",
                                            "Ventas Japón" = "JP_Sales",
                                            "Ventas Norteamérica" = "NA_Sales",
                                            "Ventas otros territorios" = "Other_Sales"),
                                selected = c("EU_Sales", "JP_Sales", "NA_Sales", "Other_Sales")),
             helpText("Selecciona las variable que deseas que se añadan al
                      modelo de regresión"),
             
             # Análisis de regresión
             verbatimTextOutput("analisis_reg"),
             
             p("Como se puede apreciar, al ir añadiendo variables al 
             análisis de regresión, los coeficientes de determinación R cuadrado y
             R cuadrado ajustado mejoran hasta llegar a ser prácticamente 1 si 
             se tienen en cuenta las ventas en todos los territorios. Esto tiene 
             sentido, ya que recordemos que las ventas globales son el resultado
             de sumar las ventas de todos los lugares."),
             p("Dado el resumen del modelo, es observable que todas las variables
               tienen un coeficiente significativo y por lo tanto, aportan
               información relevante a la regresión. Al utilizar las opciones de
               selección de variables se puede ver cómo las ventas 
               en Norteamérica aportan una mayor mejora al valor de los
               coeficientes de determinación."),
             

    ),
   
    # Pestaña 3: Gráficos
    tabPanel("Gráficos",
            
             #Barra lateral con la caja de selección
             sidebarLayout(
               sidebarPanel(
                 h3("Configuración de Gráficos"),
                 selectInput("seleccionar_genero", "Seleccionar Género", 
                             choices = unique(vj$Genre)),
                 selectInput("seleccionar_territorio", "Seleccionar Territorio", 
                             choices = c("Global", "Japón", "Europa", "Norteamérica",
                                         "Otros territorios")),
                 helpText("Selecciona el género de los videojuegos y la
                          localización geográfica a tener en cuenta"),   
                              
                 
                 
               ),
               
               mainPanel(
                 h3("Gráficos de ventas según el género y el territorio"),
                 p("En esta pestaña se muestran tres gráficos reactivos
                   relacionados con los años y el número de ventas según el 
                   género de los videojuegos. Para facilitar su visualización,
                   se permite seleccionar el género de preferencia, así como
                   el territorio para el que se muestran los gráficos."),
                 p("En primer lugar, se muestra un gráfico de barras en el
                   que se pueden visualizar las ventas de cada género por año.
                   A continuación, un pie chart para visualizar los porcentajes
                   de ventas que se corresponden con cada género de videojuegos.
                   En último lugar, se muestran los 5 videojuegos más vendidos
                   en los 5 años con mayores ventas según la selección en la 
                   barra lateral."),
                 
                 #Los tres gráficos van juntos porque son reactivos a la barra
                 #lateral
                 uiOutput("grafico_output"),
                 helpText("Posiciona el puntero de ratón sobre las barras para
                          ver los datos de cada videojuego representado"),
                
               
               )
             )
    )
  )
)


# Define server logic para la aplicación Shiny
server <- function(input, output) {
  
  ## PESTAÑA RESÚMENES NUMÉRICOS
  
  #Head datos
  output$head_vj <- renderTable({
    head_vj <- head(vj)
    head(vj)
  })
  
  # Resumen numérico
  output$resumen_texto <- renderPrint({
    summary(vj)
  })
  
  # Botón de radio para diferentes opciones de resumen
  output$resumen_table <- renderTable({
    resumen_table <- switch(input$resumen_opcion,
                            "Videojuegos más populares" = {
                              vj %>%
                                group_by(Videojuego = Name) %>%
                                summarise(VentasGlobales = sum(Global_Sales)) %>%
                                arrange(desc(VentasGlobales)) %>%
                                head(10)
                            }, 
                            "Los 10 años con más ventas globales" = {
                              vj %>%
                                group_by(Año = Year) %>%
                                summarise(VentasGlobales = sum(Global_Sales),
                                          MediaVentasGlobales = mean(Global_Sales)) %>%
                                arrange(desc(VentasGlobales)) %>%
                                head(10)
                            },
                            "Los 10 Años con menos ventas globales" = {
                              vj %>%
                                group_by(Año = Year) %>%
                                summarise(VentasGlobales = sum(Global_Sales),
                                          MediaVentasGlobales = mean(Global_Sales)) %>%
                                arrange(VentasGlobales) %>%
                                head(10)
                            },
                            "Ventas por género" = {
                              vj %>%
                                group_by(Género = Genre) %>%
                                summarise(VentasGlobales = sum(Global_Sales),
                                          MediaVentasGlobales = mean(Global_Sales),
                                          sd = sd(Global_Sales)) %>%
                                arrange(desc(VentasGlobales))
                            },
                            "Ventas por empresa (top 10)" = {
                              vj %>%
                                group_by(Empresa = Publisher) %>%
                                summarise(VentasGlobales = sum(Global_Sales),
                                          MediaVentasGlobales = mean(Global_Sales),
                                          sd = sd(Global_Sales)) %>%
                                arrange(desc(VentasGlobales)) %>%
                                head(10)
                              
                              })
    resumen_table
  })
  
  # Botón de radio para diferentes opciones de resumen en Japón
  output$resumen_table2 <- renderTable({
    resumen_table2 <- switch(input$resumen_opcion2,
                             "Videojuegos más populares" = {
                               vj %>%
                                 group_by(Videojuego = Name) %>%
                                 summarise(VentasJP = sum(JP_Sales)) %>%
                                 arrange(desc(VentasJP)) %>%
                                 head(10)
                             },
                            "Los 10 años con más ventas en Japón" = {
                              vj %>%
                                group_by(Año = Year) %>%
                                summarise(VentasJP = sum(JP_Sales),
                                          MediaVentasJP = mean(JP_Sales)) %>%
                                arrange(desc(VentasJP)) %>%
                                head(10)
                            },
                          
                            "Ventas por género" = {
                              vj %>%
                                group_by(Género = Genre) %>%
                                summarise(VentasJP = sum(JP_Sales),
                                         MediaVentasJP = mean(JP_Sales),
                                         sd = sd(JP_Sales)) %>%
                                arrange(desc(VentasJP))
                                
                            },
                            "Ventas por empresa (top 10)" = {
                              vj %>%
                                group_by(Empresa = Publisher) %>%
                                summarise(VentasJP = sum(JP_Sales),
                                         MediaVentasJP = mean(JP_Sales),
                                         sd = sd(JP_Sales)) %>%
                                arrange(desc(VentasJP)) %>%
                                head(10)
                              
                            })
    resumen_table2
  })
  
  # Botón de radio para diferentes opciones de resumen en la UE
  output$resumen_table3 <- renderTable({
    resumen_table3 <- switch(input$resumen_opcion3,
                             "Videojuegos más populares" = {
                               vj %>%
                                 group_by(Videojuego = Name) %>%
                                 summarise(VentasUE = sum(EU_Sales)) %>%
                                 arrange(desc(VentasUE)) %>%
                                 head(10)
                             },
                             "Los 10 años con más ventas en la UE" = {
                               vj %>%
                                 group_by(Año = Year) %>%
                                 summarise(VentasUE = sum(EU_Sales),
                                           MediaVentasUE = mean(EU_Sales)) %>%
                                 arrange(desc(VentasUE)) %>%
                                 head(10)
                             },
                             
                             "Ventas por género" = {
                               vj %>%
                                 group_by(Género = Genre) %>%
                                 summarise(VentasUE = sum(EU_Sales),
                                           MediaVentasUE = mean(EU_Sales),
                                           sd = sd(EU_Sales)) %>%
                                 arrange(desc(VentasUE))
                               
                             },
                             "Ventas por empresa (top 10)" = {
                               vj %>%
                                 group_by(Empresa = Publisher) %>%
                                 summarise(VentasUE = sum(EU_Sales),
                                           MediaVentasUE = mean(EU_Sales),
                                           sd = sd(EU_Sales)) %>%
                                 arrange(desc(VentasUE)) %>%
                                 head(10)
                               
                             })
    resumen_table3
  })
  
  # Botón de radio para diferentes opciones de resumen en Norteamérica
  output$resumen_table4 <- renderTable({
    resumen_table4 <- switch(input$resumen_opcion4,
                             "Videojuegos más populares" = {
                               vj %>%
                                 group_by(Videojuego = Name) %>%
                                 summarise(VentasNA = sum(NA_Sales)) %>%
                                 arrange(desc(VentasNA)) %>%
                                 head(10)
                             },
                             "Los 10 años con más ventas en Norteamérica" = {
                               vj %>%
                                 group_by(Año = Year) %>%
                                 summarise(VentasNA = sum(NA_Sales),
                                           MediaVentasNA = mean(NA_Sales)) %>%
                                 arrange(desc(VentasNA)) %>%
                                 head(10)
                             },
                             
                             "Ventas por género" = {
                               vj %>%
                                 group_by(Género = Genre) %>%
                                 summarise(VentasNA = sum(NA_Sales),
                                           MediaVentasNA = mean(NA_Sales),
                                           sd = sd(NA_Sales)) %>%
                                 arrange(desc(VentasNA))
                               
                             },
                             "Ventas por empresa (top 10)" = {
                               vj %>%
                                 group_by(Empresa = Publisher) %>%
                                 summarise(VentasNA = sum(NA_Sales),
                                           MediaVentasNA = mean(NA_Sales),
                                           sd = sd(NA_Sales)) %>%
                                 arrange(desc(VentasNA)) %>%
                                 head(10)
                               
                             })
    resumen_table4
  })
  
  # Botón de radio para diferentes opciones de resumen en otros territorios)
  output$resumen_table5 <- renderTable({
    resumen_table5 <- switch(input$resumen_opcion5,
                             "Videojuegos más populares" = {
                               vj %>%
                                 group_by(Videojuego = Name) %>%
                                 summarise(VentasOT = sum(Other_Sales)) %>%
                                 arrange(desc(VentasOT)) %>%
                                 head(10)
                             },
                             "Los 10 años con más ventas en otros territorios" = {
                               vj %>%
                                 group_by(Año = Year) %>%
                                 summarise(VentasOT = sum(Other_Sales),
                                           MediaVentasOT = mean(Other_Sales)) %>%
                                 arrange(desc(VentasOT)) %>%
                                 head(10)
                             },
                             
                             "Ventas por género" = {
                               vj %>%
                                 group_by(Género = Genre) %>%
                                 summarise(VentasOT = sum(Other_Sales),
                                           MediaVentasOT = mean(Other_Sales),
                                           sd = sd(Other_Sales)) %>%
                                 arrange(desc(VentasOT))
                               
                             },
                             "Ventas por empresa (top 10)" = {
                               vj %>%
                                 group_by(Empresa = Publisher) %>%
                                 summarise(VentasOT = sum(Other_Sales),
                                           MediaVentasOT = mean(Other_Sales),
                                           sd = sd(Other_Sales)) %>%
                                 arrange(desc(VentasOT)) %>%
                                 head(10)
                               
                             })
    resumen_table5
  })
  ##PESTAÑA ANÁLISIS DE DATOS
  
  # Almacena las variables seleccionadas de forma reactiva
  selec_vars <- reactiveVal(c("Global_Sales"))
  
  observeEvent(input$variables_checkbox, {
  selec_vars(c("Global_Sales", input$variables_checkbox))
  })
  
  # Análisis de regresión lineal dinámico
  output$analisis_reg <- renderPrint({
  mod_datos <- vj %>%
  select(all_of(selec_vars()))
  mod <- lm(Global_Sales ~ ., data = mod_datos)
  r2 <- summary(mod)$r.squared
  r2_ajustado <- summary(mod)$adj.r.squared
  cat("R cuadrado:", r2, "\n") #dice cuál es el valor de R^2
  cat("R cuadrado ajustado:", r2_ajustado, "\n")
  cat("Resumen del modelo:\n")
  print(summary(mod))

  })
  
  
  
  ## PESTAÑA GRÁFICOS

  # Gráfico interactivo ventas globales por género
  output$ventasg_plot_interactivo <- renderPlot({
    filtered_data <- vj %>%
      filter(Genre == input$seleccionar_genero)
    
    ggplot(filtered_data, aes(x = Year, y = Global_Sales, 
                              fill = as.factor(Genre))) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      labs(title = paste("Ventas de cada género a nivel global por año"),
           x = "Año",
           y = "Ventas Globales") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_fill_manual(values = "mediumpurple2")
  })
  
  # Gráfico interactivo ventas Japón
  output$ventasg_plot_jp <- renderPlot({
    filtered_data <- vj %>%
      filter(Genre == input$seleccionar_genero)
    
    ggplot(filtered_data, aes(x = Year, y = JP_Sales, 
                              fill = as.factor(Genre))) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      labs(title = paste("Ventas de cada género en Japón por año"),
           x = "Año",
           y = "Ventas en Japón") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_fill_manual(values = "mediumpurple2")
  })  
  
  # Gráfico interactivo ventas UE
  output$ventasg_plot_ue <- renderPlot({
    filtered_data <- vj %>%
      filter(Genre == input$seleccionar_genero)
    
    ggplot(filtered_data, aes(x = Year, y = EU_Sales, 
                              fill = as.factor(Genre))) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      labs(title = paste("Ventas de cada género en la Unión Europea por año"),
           x = "Año",
           y = "Ventas en la Unión Europea") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_fill_manual(values = "mediumpurple2")
  }) 
  
  # Gráfico interactivo ventas NA
  output$ventasg_plot_na <- renderPlot({
    filtered_data <- vj %>%
      filter(Genre == input$seleccionar_genero)
    
    ggplot(filtered_data, aes(x = Year, y = NA_Sales, 
                              fill = as.factor(Genre))) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      labs(title = paste("Ventas de cada género en Norteamérica por año"),
           x = "Año",
           y = "Ventas en Norteamérica") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_fill_manual(values = "mediumpurple2")
  })  
  
  # Gráfico interactivo ventas otros
  output$ventasg_plot_ot <- renderPlot({
    filtered_data <- vj %>%
      filter(Genre == input$seleccionar_genero)
    
    ggplot(filtered_data, aes(x = Year, y = Other_Sales, 
                              fill = as.factor(Genre))) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      labs(title = paste("Ventas de cada género en otros territorios por año"),
           x = "Año",
           y = "Ventas en otros territorios") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_fill_manual(values = "mediumpurple2")
  }) 
  
  
  # Gráfico pie chart interactivo global
  output$pie_chart_global <- renderPlotly({
    pie_chart_global <- vj %>%
      group_by(Genre) %>%
      summarise(Sales = sum(Global_Sales))
    
    pie_chart_global %>%
      plot_ly(labels = ~Genre, values = ~Sales, type = 'pie') %>%
      layout(title = paste("Distribución de ventas globales por género"))
  })
  
  # Gráfico pie chart interactivo Japón
  output$pie_chart_jp <- renderPlotly({
    pie_chart_jp <- vj %>%
      group_by(Genre) %>%
      summarise(Sales = sum(JP_Sales))
    
    pie_chart_jp %>%
      plot_ly(labels = ~Genre, values = ~Sales, type = 'pie') %>%
      layout(title = paste("Distribución de ventas en Japón por género"))
  })
  
  # Gráfico pie chart interactivo Unión Europea
  output$pie_chart_ue <- renderPlotly({
    pie_chart_ue <- vj %>%
      group_by(Genre) %>%
      summarise(Sales = sum(EU_Sales))
    
    pie_chart_ue %>%
      plot_ly(labels = ~Genre, values = ~Sales, type = 'pie') %>%
      layout(title = paste("Distribución de ventas en Europa por género"))
  })
  
  # Gráfico pie chart interactivo Norteamérica
  output$pie_chart_na <- renderPlotly({
    pie_chart_na <- vj %>%
      group_by(Genre) %>%
      summarise(Sales = sum(NA_Sales))
    
    pie_chart_na %>%
      plot_ly(labels = ~Genre, values = ~Sales, type = 'pie') %>%
      layout(title = paste("Distribución de ventas en Norteamérica por género"))
  })
  
  # Gráfico pie chart interactivo otros territorios
  output$pie_chart_ot <- renderPlotly({
    pie_chart_ot <- vj %>%
      group_by(Genre) %>%
      summarise(Sales = sum(Other_Sales))
    
    pie_chart_ot %>%
      plot_ly(labels = ~Genre, values = ~Sales, type = 'pie') %>%
      layout(title = paste("Distribución de ventas en otros territorios por género"))
  })
  
  # Gráfico juegos por género y año (Global)
  #filtrando los 5 mejores años y los 5 mejores juegos
 output$juegos_plot_g <- renderPlotly({
filtered_data <- vj %>%
  filter(Genre == input$seleccionar_genero)
    top_años <- filtered_data %>%  
      group_by(Year) %>%
      summarize(Ventas_Total = sum(Global_Sales)) %>%
      top_n(5, Ventas_Total) %>%
      arrange(desc(Ventas_Total)) %>%
      pull(Year)
      top_data <- filtered_data %>%
      filter(Year %in% top_años)
      top_juegos <- top_data %>%
      group_by(Year, Name) %>%
      summarize(Ventas_Total = sum(Global_Sales)) %>%
      top_n(5, Ventas_Total) %>%
      arrange(Year, desc(Ventas_Total))

    plot_ly(top_juegos, x = ~Year, y = ~Ventas_Total, color = ~Name, type = "bar",
            text = ~paste("Juego: ", Name, "<br>Año: ", Year, 
                          "<br>Ventas: ", Ventas_Total)) %>% #<br> html para salto de línea
      layout(title = "Top 5 juegos en los años con más ventas",
             xaxis = list(title = "Año"),
             yaxis = list(title = "Ventas Globales"),
             showlegend = FALSE)
  })
 
 # Gráfico juegos por género y año (UE)
 #filtrando los 5 mejores años y los 5 mejores juegos
 output$juegos_plot_ue <- renderPlotly({
   filtered_data <- vj %>%
     filter(Genre == input$seleccionar_genero)
   top_años <- filtered_data %>%  
     group_by(Year) %>%
     summarize(Ventas_TotalUE = sum(EU_Sales)) %>%
     top_n(5, Ventas_TotalUE) %>%
     arrange(desc(Ventas_TotalUE)) %>%
     pull(Year)
   top_data <- filtered_data %>%
     filter(Year %in% top_años)
   top_juegos <- top_data %>%
     group_by(Year, Name) %>%
     summarize(Ventas_TotalUE = sum(EU_Sales)) %>%
     top_n(5, Ventas_TotalUE) %>%
     arrange(Year, desc(Ventas_TotalUE))
   

   plot_ly(top_juegos, x = ~Year, y = ~Ventas_TotalUE, color = ~Name, type = "bar",
           text = ~paste("Juego: ", Name, "<br>Año: ", Year,
                         "<br>Ventas: ", Ventas_TotalUE)) %>%
     layout(title = "Top 5 juegos en los años con más ventas",
            xaxis = list(title = "Año"),
            yaxis = list(title = "Ventas en Europa"),
            showlegend = FALSE)
 })
 
 # Código gráfico juegos por género y año (NA)
 output$juegos_plot_na <- renderPlotly({
   filtered_data <- vj %>%
     filter(Genre == input$seleccionar_genero)
   top_años <- filtered_data %>% 
     group_by(Year) %>%
     summarize(Ventas_TotalNA = sum(NA_Sales)) %>%
     top_n(5, Ventas_TotalNA) %>%
     arrange(desc(Ventas_TotalNA)) %>%
     pull(Year)
   top_data <- filtered_data %>%
     filter(Year %in% top_años)
   top_juegos <- top_data %>%
     group_by(Year, Name) %>%
     summarize(Ventas_TotalNA = sum(NA_Sales)) %>%
     top_n(5, Ventas_TotalNA) %>%
     arrange(Year, desc(Ventas_TotalNA))
   

   plot_ly(top_juegos, x = ~Year, y = ~Ventas_TotalNA, color = ~Name, type = "bar",
           text = ~paste("Juego: ", Name, "<br>Año: ", Year, 
                         "<br>Ventas: ", Ventas_TotalNA)) %>%
     layout(title = "Top 5 juegos en los años con más ventas",
            xaxis = list(title = "Año"),
            yaxis = list(title = "Ventas en Norteamérica"),
            showlegend = FALSE)
 })
 
 # Gráfico juegos por género y año (JP)
 output$juegos_plot_jp <- renderPlotly({
   filtered_data <- vj %>%
     filter(Genre == input$seleccionar_genero)
   top_años <- filtered_data %>%
     group_by(Year) %>%
     summarize(Ventas_TotalJP = sum(JP_Sales)) %>%
     top_n(5, Ventas_TotalJP) %>%
     arrange(desc(Ventas_TotalJP)) %>%
     pull(Year)
   top_data <- filtered_data %>%
     filter(Year %in% top_años)
   top_juegos <- top_data %>%
     group_by(Year, Name) %>%
     summarize(Ventas_TotalJP = sum(JP_Sales)) %>%
     top_n(5, Ventas_TotalJP) %>%
     arrange(Year, desc(Ventas_TotalJP))

   plot_ly(top_juegos, x = ~Year, y = ~Ventas_TotalJP, color = ~Name, type = "bar",
           text = ~paste("Juego: ", Name, "<br>Año: ", Year, 
                         "<br>Ventas: ", Ventas_TotalJP)) %>%
     layout(title = "Top 5 juegos en los años con más ventas",
            xaxis = list(title = "Año"),
            yaxis = list(title = "Ventas en Japón"),
            showlegend = FALSE)
 })
  
 # Gráfico juegos por género y año (otros)
 output$juegos_plot_ot <- renderPlotly({
   filtered_data <- vj %>%
     filter(Genre == input$seleccionar_genero)
   top_años <- filtered_data %>% 
     group_by(Year) %>%
     summarize(Ventas_TotalOT = sum(Other_Sales)) %>%
     top_n(5, Ventas_TotalOT) %>%
     arrange(desc(Ventas_TotalOT)) %>%
     pull(Year)
   top_data <- filtered_data %>%
     filter(Year %in% top_años)
   top_juegos <- top_data %>%
     group_by(Year, Name) %>%
     summarize(Ventas_TotalOT = sum(Other_Sales)) %>%
     top_n(5, Ventas_TotalOT) %>%
     arrange(Year, desc(Ventas_TotalOT))
   

   plot_ly(top_juegos, x = ~Year, y = ~Ventas_TotalOT, color = ~Name, type = "bar",
           text = ~paste("Juego: ", Name, "<br>Año: ", Year, 
                         "<br>Ventas: ", Ventas_TotalOT)) %>%
         layout(title = "Top 5 juegos en los años con más ventas",
            xaxis = list(title = "Año"),
            yaxis = list(title = "Ventas en otros territorios"),
            showlegend = FALSE)
 })
 
 # Gráficos según selección en la caja de la barra lateral
 output$grafico_output <- renderUI({
   if (input$seleccionar_territorio == "Global") {
     list(
       plotOutput("ventasg_plot_interactivo"),
       plotlyOutput("pie_chart_global"),
       plotlyOutput("juegos_plot_g")
     )
   } else if (input$seleccionar_territorio == "Japón") {
     list(
       plotOutput("ventasg_plot_jp"),
       plotlyOutput("pie_chart_jp"),
       plotlyOutput("juegos_plot_jp")
     )
   } else if (input$seleccionar_territorio == "Europa") {
     list(
       plotOutput("ventasg_plot_ue"),
       plotlyOutput("pie_chart_ue"),
       plotlyOutput("juegos_plot_ue")
     )
   } else if (input$seleccionar_territorio == "Norteamérica") {
     list(
       plotOutput("ventasg_plot_na"),
       plotlyOutput("pie_chart_na"),
       plotlyOutput("juegos_plot_na")
     ) 
   } else if (input$seleccionar_territorio == "Otros territorios") {
     list(
       plotOutput("ventasg_plot_ot"),
       plotlyOutput("pie_chart_ot"),
       plotlyOutput("juegos_plot_ot")
     ) 
   }
 })
 
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)