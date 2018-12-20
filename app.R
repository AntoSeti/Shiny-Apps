library(leaflet)
library(shiny)
library(plotly)
library(shinythemes)
library(shinydashboard)
library(dplyr)

FFT<-read.csv2(file="FFTennis.csv", header = TRUE, sep = ";", dec = ".")
Effectif <- read.csv2(file="Effectif.csv", header = TRUE, sep = ";", dec = ".")

couleurs <- colorNumeric("YlOrRd", FFT$Eff2019Tot, n = 5)

ui <- dashboardPage(skin = "purple",
                    
                    dashboardHeader(title="#TennisNormandie Twitter"),
                    
                    dashboardSidebar(
                      
                      sidebarMenu(
                        
                        menuItem("Cartographie", tabName = "Carto",  icon = icon("dashboard")), 
                        menuItem("Evolution effectifs", tabName = "Effectif", icon = icon("thumbs-up")), 
                        menuItem("A propos", tabName = "infos", icon = icon("question-circle"))
                      )
                      
                    ),
                    dashboardBody(
                                  
                                  tabItems(
                                    
                                    tabItem(tabName = "Carto",
                                            box(
                                              title = "Filtres personnalisés", status = "warning", solidHeader = TRUE,
                                              collapsible = TRUE, width=14,
                                              fluidRow(
                                                column(6,
                                                       sliderInput("nbrelicencies", strong("Veuillez choisir le nombre de licenciés"), step=10,
                                                                      min = 1, max = 1000,
                                                                      value = c(1,1000))
                                                ),
                                                column(6,
                                                       sliderInput("nbreterrains", strong("Veuillez choisir le nombre de terrains"), step=1,
                                                                   min = 1, max = 20,
                                                                   value = c(1,20))
                                                )
                                              )
                                              ),
                                    box(
                                              title = "Carte Seine-Maritime", status = "primary", solidHeader = FALSE,
                                              collapsible = TRUE, width=14,
                                    leafletOutput("mymap")
                                    )
                                
                                    ),
                                    tabItem(tabName = "Effectif",
                                          
                                            box(title = "Evolution Effectifs", status = "primary", solidHeader = TRUE,
                                                collapsible = TRUE, width=12,
                                                fluidRow(
                                                  column(4,
                                                         selectInput(inputId = "club1", label = strong("Choix club"),
                                                                     choices = unique(Effectif$Club),
                                                                     selected = "MAROMME AL")
                                                  ),
                                                  column(4,
                                                         selectInput(inputId = "club2",label = strong(""),
                                                                     choices = unique(Effectif$Club),
                                                                     selected = "NOTRE DAME DE BONDEVILLE BTC")
                                                  ),
                                                  column(4,
                                                         selectInput(inputId = "club3", label = strong(""),
                                                                     choices = unique(Effectif$Club),
                                                                     selected = "DEVILLE TC DEVILLOIS")
                                                  )
                                                  ),
                                                tabBox(width=12,
                                                       tabPanel("Total",
                                                                plotlyOutput(outputId = "efftot")),
                                                       tabPanel("Adultes",
                                                                plotlyOutput(outputId = "effadu")),
                                                       tabPanel("Jeunes",
                                                                plotlyOutput(outputId = "effjeu"))
                                                )
                                            )
                                    ),
                                    
                                    tabItem(tabName = "infos",
                                          
                                            fluidRow(
                                              
                                              column(width = 12,
                                                     
                                                     br(),     
                                                     
                                                     h4("Les résultats fournis concernent les club de tennis de l'agglomération rouennaise. On pourra localiser les clubs sur une carte, accompagné des caractéristiques du club (membres, surface, nombre de terrains...). On analysera également l'évolution des effectifs sur les 10 dernières années . Les données proviennent du site officiel de la  ",
                                                        a("FFT", href = "http://www.fft.fr/", target="_blank"), "."),
                                                     
                                                     br(),
                                                     
                                                     h4("Ce dashboard a été réalisé grâce au logiciel ", a("R", href = "https://www.r-project.org/", target="_blank"), " et aux packages suivants : ", strong("leaflet"), ",", strong("plotly"), ",", strong("shinydashboard"),".")
                                                     
                                                     
                                              )
                                              
                                            )
                                            
                                    )
                                    
                                  )
                                  
                    )
)

server <- function(input, output) {
  # Subset data
  selected_trends <- reactive({
   FFT %>%
      filter(
        NbreCourts >= input$nbreterrains[1] & NbreCourts <= input$nbreterrains[2],
        Eff2019Tot >= input$nbrelicencies[1] & Eff2019Tot <= input$nbrelicencies[2]
      )
  })

  selected_trendseff <- reactive({
    Effectif %>%
      filter(
        Club %in% c(input$club1,input$club2,input$club3)
      )
  })
  
  output$mymap <- renderLeaflet({
    leaflet(selected_trends()) %>% addTiles() %>%
      setView(lng = 1.0993, lat = 49.4431, zoom = 12) %>%
      addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1,
                 radius = 400,
                 popup = ~paste("<b>",Club,"</b>","<br>",
                                Eff2019Tot, "membres :",Eff2019A,"adultes,",Eff2019J,"jeunes","<br>",
                                "Surface :", Surface, "<br>",
                                NbreCourts, "terrains :",NbreCourtsCouverts,"couverts,",NbreCourtsExterieurs,"exterieurs", "<br>",
                               "<a href=",Internet,">Voir le site </a>"),
                 color = ~couleurs(Eff2019Tot), fillOpacity = 0.8) %>%
      addLegend(pal = couleurs, values = ~Eff2019Tot, opacity = 0.6, title="Effectif")
      # addMarkers(lng = ~Longitude, lat = ~Latitude)
  })

  output$efftot <- renderPlotly({
    plot_ly(source = "source") %>% 
      add_lines(data = data.frame(selected_trendseff()), x = ~Annee, y = ~EffectifT, color = ~Club, mode = 'lines+markers', line = list(width = 3)) %>% 
      layout(title = "Comparatif évolution effectif total",
             xaxis = list(title = "Année", domain = c(0, 0.98)),
             yaxis = list(title = "Effectif"))
  })
  
  output$effadu <- renderPlotly({
    plot_ly(source = "source") %>% 
      add_lines(data = data.frame(selected_trendseff()), x = ~Annee, y = ~EffectifA, color = ~Club, mode = 'lines+markers', line = list(width = 3)) %>% 
      layout(title = "Comparatif évolution effectif adultes",
             xaxis = list(title = "Année", gridcolor = "#bfbfbf", domain = c(0, 0.98)),
             yaxis = list(title = "Effectif", gridcolor = "#bfbfbf"))
  })
  
  output$effjeu <- renderPlotly({
    plot_ly(source = "source") %>% 
      add_lines(data = data.frame(selected_trendseff()), x = ~Annee, y = ~EffectifJ, color = ~Club, mode = 'lines+markers', line = list(width = 3)) %>% 
      layout(title = "Comparatif évolution effectif jeunes",
             xaxis = list(title = "Année", gridcolor = "#bfbfbf", domain = c(0, 0.98)),
             yaxis = list(title = "Effectif", gridcolor = "#bfbfbf"))
  })
}

shinyApp(ui, server)