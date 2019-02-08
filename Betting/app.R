## app.R ##
library(shiny)
library(shinythemes)
library(rAmCharts)
library(shinydashboard)
library(dplyr)
library(pipeR)
library(readr)
library(ggplot2)

# setwd("~/Desktop/ProjetProno/BettingProject")

Mars2018<-read.csv2(file="Mars2018.csv", header = TRUE, sep = ";", dec = ".")
Mars2018$Date <- as.POSIXct(as.Date(Mars2018$Date, "%d/%m/%Y"))

Avril2018<-read.csv2(file="Avril2018.csv", header = TRUE, sep = ";", dec = ".")
Avril2018$Date <- as.POSIXct(as.Date(Avril2018$Date, "%d/%m/%Y")) ## attention format

Mai2018<-read.csv2(file="Mai2018.csv", header = TRUE, sep = ";", dec = ".")
Mai2018$Date <- as.POSIXct(as.Date(Mai2018$Date, "%m/%d/%Y")) 

Juin2018<-read.csv2(file="Juin2018.csv", header = TRUE, sep = ";", dec = ".")
Juin2018$Date <- as.POSIXct(as.Date(Juin2018$Date, "%m/%d/%Y"))

Juillet2018<-read.csv2(file="Juillet2018.csv", header = TRUE, sep = ";", dec = ".")
Juillet2018$Date <- as.POSIXct(as.Date(Juillet2018$Date, "%m/%d/%Y"))

Aout2018<-read.csv2(file="Aout2018.csv", header = TRUE, sep = ";", dec = ".")
Aout2018$Date <- as.POSIXct(as.Date(Aout2018$Date, "%m/%d/%Y"))

Sept2018<-read.csv2(file="Sept2018.csv", header = TRUE, sep = ";", dec = ".")
Sept2018$Date <- as.POSIXct(as.Date(Sept2018$Date, "%d/%m/%Y"))

An2018 <- rbind(Sept2018,Aout2018,Juillet2018,Juin2018,Mai2018,Avril2018,Mars2018)

#An2018<-read.csv2(file="Antoine.csv", header = TRUE, sep = ";", dec = ".")
An2018$Date <- as.POSIXct(as.Date(An2018$Date, "%d/%m/%Y"))

ui <- dashboardPage(
  
  dashboardHeader(title = "Analyse paris 2018"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Vue générale", tabName = "tabgeneral", icon = icon("dashboard")),
      menuItem("Par Sport", tabName = "tabsport", icon = icon("calendar")),
      menuItem("Par Tipster", tabName = "tabtipster", icon = icon("credit-card")),
      menuItem("Par Bookmaker", tabName = "tabbook", icon = icon("thumbs-up")),
      menuItem("Visualisation données 1", tabName = "tabvue", icon = icon("table")),
      menuItem("Visualisation données 2", tabName = "tabvue2", icon = icon("app-store")),
      menuItem("A propos", tabName = "infos", icon = icon("question-circle"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "tabgeneral",
                box(
                  title = "Filtres personnalisés", status = "warning", solidHeader = TRUE,
                  collapsible = TRUE, width=14,
                  fluidRow(
                  column(6,
                  dateRangeInput("rangedate", strong("Veuillez choisir un intevalle de temps"), format = "dd/mm/yy",
                                 start = "2018-02-28", end = "2018-11-30",
                                 min = "2007-01-01", max = "2020-07-31", separator = "-")
                  ),
                  column(6,
                  sliderInput("rangecote", strong("Veuillez choisir un intervalle de côte"), step=1,
                              min = 1, max = 200,
                              value = c(1,200))
                  )
                  )
              ),
              fluidRow(
                box(title = "Bénéfice", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, width=8,
                    tabBox(width=12,
                      tabPanel("JavaScript",
                               amChartsOutput(outputId = "amplotgen1")),
                      tabPanel("R-Shiny",
                               checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
                               conditionalPanel(condition = "input.smoother == true",
                                                sliderInput(inputId = "f", label = "Smoother span:",
                                                            min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                            animate = animationOptions(interval = 100)),
                                                HTML("Higher values give more smoothness.")),
                               plotOutput(outputId = "amplotgen"))
                    )
                ),
                valueBoxOutput("NbParisGenBox"),
                valueBoxOutput("BeneficeGenBox"),
                valueBoxOutput("RoiGenBox"),
                valueBoxOutput("CoteMoyBox")
              ),
              fluidRow(
                box(title = "Répartition des paris", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    tabBox(width=12,
                      tabPanel("Par Sport", "Camembert/Donuts 3D",
                               amChartsOutput(outputId = "amdonuts3dsport")),
                      tabPanel("Par Tipster", "Barplot",
                               amChartsOutput(outputId = "ambarplottipster")),
                      tabPanel("Par Bookmaker", "Pyramide inversée",
                               amChartsOutput(outputId = "ampyramidebook"))
                    )
                ),
                box(title = "Répartition bénéfice", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    tabBox(width=12,
                      tabPanel("Par Sport",
                               amChartsOutput(outputId = "amhistsport")),
                      tabPanel("Par Tipster",
                               amChartsOutput(outputId = "amhisttipster")),
                      tabPanel("Par Bookmaker",
                               amChartsOutput(outputId = "amhistbook"))
                    )
                )
                )   
           ),
      tabItem(tabName = "tabsport",
              fluidRow(
                box(
                  title = "Choix dates, côtes", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  dateRangeInput("rangedatesport", strong("Veuillez choisir un intevalle de temps"), format = "dd/mm/yy",
                                 start = "2018-02-28", end = "2018-11-30",
                                 min = "2007-01-01", max = "2020-07-31", separator = "-"),
                  sliderInput("rangecotesport", strong("Veuillez choisir un intervalle de cote"), step=1,
                              min = 1, max = 200,
                              value = c(1,200))
                ),
                box(
                  title = "Sport", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  selectInput(inputId = "sport", label = strong("Choix sport"),
                              choices = unique(An2018$Sport),
                              selected = "Football")
                )
              ),
              fluidRow(
                box(title = "Bénéfice par sport", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, width=8,
                    tabBox(width=12,
                      tabPanel("JavaScript",
                               amChartsOutput(outputId = "amplotgen1sport")),
                      tabPanel("R-Shiny",
                               checkboxInput(inputId = "smoothersport", label = strong("Overlay smooth trend line"), value = FALSE),
                               conditionalPanel(condition = "input.smoothersport == true",
                                                sliderInput(inputId = "fsport", label = "Smoother span:",
                                                            min = 0.01, max = 1, value = 0.67, step = 1,
                                                            animate = animationOptions(interval = 100)),
                                                HTML("Higher values give more smoothness.")),
                               plotOutput(outputId = "amplotgensport"))
                    )
                ),
                valueBoxOutput("NbParisSportBox"),
                valueBoxOutput("BeneficeSportBox"),
                valueBoxOutput("RoiSportBox"),
                valueBoxOutput("CoteMoySportBox")
              )
      ),
      tabItem(tabName = "tabtipster",
              fluidRow(
                box(
                  title = "Choix dates, côtes", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  dateRangeInput("rangedatetipster", strong("Veuillez choisir un intevalle de temps"), format = "dd/mm/yy",
                                 start = "2018-02-28", end = "2018-11-30",
                                 min = "2007-01-01", max = "2020-07-31", separator = "-"),
                  sliderInput("rangecotetipster", strong("Veuillez choisir un intervalle de cote"), step=0.1,
                              min = 1, max = 200,
                              value = c(1,200))
                ),
                box(
                  title = "Tipster", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  selectInput(inputId = "tipster", label = strong("Choix tipster"),
                              choices = unique(An2018$Tipster),
                              selected = "Zorro")
                )
              ),
              fluidRow(
                box(title = "Bénéfice par tipster", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, width=8,
                    tabBox(width=12,
                      tabPanel("JavaScript",
                               amChartsOutput(outputId = "amplotgen1tipster")),
                      tabPanel("R-Shiny",
                               checkboxInput(inputId = "smoothertipster", label = strong("Overlay smooth trend line"), value = FALSE),
                               conditionalPanel(condition = "input.smoothersport == true",
                                                sliderInput(inputId = "ftipster", label = "Smoother span:",
                                                            min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                            animate = animationOptions(interval = 100)),
                                                HTML("Higher values give more smoothness.")),
                               plotOutput(outputId = "amplotgentipster"))
                    )
                ),
                valueBoxOutput("NbParisTipsterBox"),
                valueBoxOutput("BeneficeTipsterBox"),
                valueBoxOutput("RoiTipsterBox"),
                valueBoxOutput("CoteMoyTipsterBox")
              )
      ),
      tabItem(tabName = "tabbook",
              fluidRow(
                box(
                  title = "Choix dates, côtes", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  dateRangeInput("rangedatebook", strong("Veuillez choisir un intevalle de temps"), format = "dd/mm/yy",
                                 start = "2018-02-28", end = "2018-11-30",
                                 min = "2007-01-01", max = "2020-07-31", separator = "-"),
                  sliderInput("rangecotebook", strong("Veuillez choisir un intervalle de cote"), step=1,
                              min = 1, max = 200,
                              value = c(1,200))
                ),
                box(
                  title = "Bookmaker", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  selectInput(inputId = "bookmaker", label = strong("Choix bookmaker"),
                              choices = unique(An2018$Bookmaker),
                              selected = "Asianodds")
                )
              ),
              fluidRow(
                box(title = "Répartition par bookmaker", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, width=8,
                    tabBox(width=12,
                      tabPanel("JavaScript",
                               amChartsOutput(outputId = "amplotgen1book")),
                      tabPanel("R-Shiny",
                               checkboxInput(inputId = "smootherbook", label = strong("Overlay smooth trend line"), value = FALSE),
                               conditionalPanel(condition = "input.smootherbook == true",
                                                sliderInput(inputId = "fbook", label = "Smoother span:",
                                                            min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                            animate = animationOptions(interval = 100)),
                                                HTML("Higher values give more smoothness.")),
                               plotOutput(outputId = "amplotgenbook"))
                    )
                ),
                valueBoxOutput("NbParisBookBox"),
                valueBoxOutput("BeneficeBookBox"),
                valueBoxOutput("RoiBookBox"),
                valueBoxOutput("CoteMoyBookBox")
              )
      ),
      tabItem(tabName = "tabvue",
              titlePanel("Visualisation données (par filtres)"),
              fluidRow(
                column(4,
                       selectInput("sport11",
                                   "Sport :",
                                   c("All",
                                     unique(as.character(An2018$Sport))))
                ),
                column(4,
                       selectInput("book11",
                                   "Bookmaker :",
                                   c("All",
                                     unique(as.character(An2018$Bookmaker))))
                ),
                column(4,
                       selectInput("tipster11",
                                   "Tipster :",
                                   c("All",
                                     unique(as.character(An2018$Tipster))))
                )
              ),
              DT::dataTableOutput("table11")
      ),
      tabItem(tabName = "tabvue2",
              titlePanel("Visualisation données (Choix colonnes)"),
              sidebarLayout(
                sidebarPanel(
                    checkboxGroupInput("show_vars", "Columns to show:",
                                       names(An2018), selected = names(An2018))
                ),
                mainPanel(
                 DT::dataTableOutput("mytable11")
                )
              )
      ),
      tabItem(tabName = "infos",
              
              fluidRow(
                
                column(width = 12,
                       
                       br(),     
                       
                       h4("L'idée de ce projet est de synthétiser un ensemble important de données .csv liés aux résultats de paris sportifs (issu de divers sites internets) afin d'avoir le meilleur rendu visuel d'un suivi sur la période Mars 2018--Septembre 2018. Les caractéristiques et indicateurs associés se mettent à jour en fonction des filtres sélectionnés (par côte, tipster, sport, bookmaker...).",

                       br(),
                       
                       h4("Ce dashboard a été réalisé grâce au logiciel ", a("R", href = "https://www.r-project.org/", target="_blank"), " et aux packages suivants : ", strong("shiny"), ",", strong("rAmCharts"), ",", strong("dplyr"),",", strong("pipeR"),",", strong("readr"),",", strong("shinydashboard"),".")
                       
                       
                )
                
              )
              
      )
    )
  )
)
)


server <- function(input, output) {
  # Subset data
  selected_trends <- reactive({
    req(input$rangedate)
    validate(need(!is.na(input$rangedate[1]) & !is.na(input$rangedate[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$rangedate[1] < input$rangedate[2], "Error: Start date should be earlier than end date."))
    An2018 %>%
      filter(
        Cote >= input$rangecote[1] & Cote <= input$rangecote[2],
        Date >= as.POSIXct(input$rangedate[1]) & Date <= as.POSIXct(input$rangedate[2])
      )
  })
  
  # Subset data sport
  selected_trendssport <- reactive({
    req(input$rangedatesport)
    validate(need(!is.na(input$rangedatesport[1]) & !is.na(input$rangedatesport[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$rangedatesport[1] < input$rangedatesport[2], "Error: Start date should be earlier than end date."))
    An2018 %>%
      filter(
        Cote >= input$rangecotesport[1] & Cote <= input$rangecotesport[2],
        Date >= as.POSIXct(input$rangedatesport[1]) & Date <= as.POSIXct(input$rangedatesport[2]),
        Sport == input$sport
      )
  })
  
  # Subset data tipster
  selected_trendstipster <- reactive({
    req(input$rangedatetipster)
    validate(need(!is.na(input$rangedatetipster[1]) & !is.na(input$rangedatetipster[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$rangedatetipster[1] < input$rangedatetipster[2], "Error: Start date should be earlier than end date."))
    An2018 %>%
      filter(
        Cote >= input$rangecotetipster[1] & Cote <= input$rangecotetipster[2],
        Date >= as.POSIXct(input$rangedatetipster[1]) & Date <= as.POSIXct(input$rangedatetipster[2]),
        Tipster == input$tipster
      )
  })
  
  # Subset data bookmaker
  selected_trendsbook <- reactive({
    req(input$rangedatebook)
    validate(need(!is.na(input$rangedatebook[1]) & !is.na(input$rangedatebook[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$rangedatebook[1] < input$rangedatebook[2], "Error: Start date should be earlier than end date."))
    An2018 %>%
      filter(
        Cote >= input$rangecotebook[1] & Cote <= input$rangecotebook[2],
        Date >= as.POSIXct(input$rangedatebook[1]) & Date <= as.POSIXct(input$rangedatebook[2]),
        Bookmaker == input$bookmaker
      )
  })
  
  output$amplotgen <- renderPlot({
    color = "#434343"
    plot(x = 1:length(selected_trends()$Etat), y = cumsum(rev(selected_trends()$Benefice)), type = "l", main="Evolution Bénéfice",
         xlab = "Nombre de paris", ylab = "Bénéfice", col = color, fg = color, col.lab = color, col.axis = color)
    # Display only if smoother is checked
    if(input$smoother){
      smooth_curve <- lowess(x = 1:length(selected_trends()$Etat), y = cumsum(rev(selected_trends()$Benefice)), f = input$f)
      lines(smooth_curve, col = "#E6553A", lwd = 3)
    }
  })
  
  output$amplotgen1 <- renderAmCharts({
    # An2018Cote <- subset(An2018, Cote>= input$rangecote[1] & Cote <= input$rangecote[2])
    # xlab <- 1:length(An2018Cote$Etat)
    # y <- cumsum(rev(An2018Cote$Benefice))
    dataProvider <- data.frame(x=1:length(selected_trends()$Etat),y=cumsum(rev(selected_trends()$Benefice)))
    # build the chart
    amSerialChart(dataProvider = dataProvider, categoryField = "x") %>%
      setExport(enabled = TRUE) %>%
      addGraph(id="g1",valueField = "y", lineColor = "green", fillAlphas=0.3,
               bullet= "none",
               bulletBorderAlpha= 0,
               bulletColor = "white",
               bulletSize = 0,
               balloonText = "[[value]]") %>%
      addTitle(text = "Evolution gain cumulé") %>%
      addValueAxis(title = "Gains") %>%
      setCategoryAxis(dashLength= 1, minorGridEnabled=T) %>%
      addGuide(inside=T) %>%
      setBalloon(adjustBorderColor=F, fillColor = "white", borderThickness=1, shadowAlpha=0,
                 color = "white", cornerRadius = 25, fillAlpha=0.45, pointerOrientation="up") %>%
      setChartCursor(valueLineEnabled=T,
                     valueLineBalloonEnabled=F,
                     cursorAlpha=10,
                     cursorColor="green",
                     bulletsEnabled=F,
                     limitToGraph="g1",
                     valueLineAlpha=0)
  })
  
  output$ampie3dsport <- renderAmCharts({
    dataProvider <- data.frame(x=sort(table(selected_trends()$Sport),decreasing=TRUE)[1:length(names(sort(table(selected_trends()$Sport))))])
    colnames(dataProvider) <- c("label","value")
    # build the chart
    amPie(data = dataProvider, depth = 10, show_values = FALSE, legend = TRUE) %>%
      addTitle(text = "Nombre de paris par sport")
  })
  
  output$amdonuts3dsport <- renderAmCharts({
    dataProvider <- data.frame(x=sort(table(selected_trends()$Sport),decreasing=TRUE)[1:length(names(sort(table(selected_trends()$Sport))))])
    colnames(dataProvider) <- c("label","value")
    # build the chart
    amPie(data = dataProvider, inner_radius = 70, depth = 10, show_values = FALSE, legend = TRUE) %>%
      addTitle(text = "Nombre de paris par sport")
  })
  
  output$ampyramidesport <- renderAmCharts({
    dataProvider <- data.frame(x=sort(table(selected_trends()$Sport),decreasing=TRUE)[1:length(names(sort(table(selected_trends()$Sport))))])
    colnames(dataProvider) <- c("description","value")
    # build the chart
    amFunnel(data = dataProvider, inverse = FALSE, legend=TRUE) %>%
      addTitle(text = "Nombre de paris par sport")
  })
  
  output$ambarplotsport <- renderAmCharts({
    dataProvider <- data.frame(x=sort(table(selected_trends()$Sport), decreasing=TRUE)[1:length(names(sort(table(selected_trends()$Sport))))])
    colnames(dataProvider) <- c("label","value")
    # build the chart
    amBarplot(x = "label", y = "value", data = dataProvider, labelRotation = -45, show_values = TRUE)  %>%
      addTitle(text = "Nombre de paris par sport")
  })
  
  output$ampie3dbook <- renderAmCharts({
    dataProvider <- data.frame(x=sort(table(selected_trends()$Bookmaker),decreasing=TRUE)[1:length(names(sort(table(selected_trends()$Bookmaker))))])
    colnames(dataProvider) <- c("label","value")
    # build the chart
    amPie(data = dataProvider, depth = 10, show_values = FALSE, legend = TRUE) %>%
      addTitle(text = "Nombre de paris par bookmaker")
  })
  
  output$amdonuts3dbook <- renderAmCharts({
    dataProvider <- data.frame(x=sort(table(selected_trends()$Bookmaker),decreasing=TRUE)[1:length(names(sort(table(selected_trends()$Bookmaker))))])
    colnames(dataProvider) <- c("label","value")
    # build the chart
    amPie(data = dataProvider, inner_radius = 50, depth = 10, show_values = FALSE, legend = TRUE) %>%
      addTitle(text = "Nombre de paris par bookmaker")
  })
  
  output$ampyramidebook <- renderAmCharts({
    dataProvider <- data.frame(x=sort(table(selected_trends()$Bookmaker),decreasing=TRUE)[1:length(names(sort(table(selected_trends()$Bookmaker))))])
    colnames(dataProvider) <- c("description","value")
    # build the chart
    amFunnel(data = dataProvider, inverse = FALSE, legend=TRUE) %>%
      addTitle(text = "")
  })
  
  output$ambarplotbook <- renderAmCharts({
    dataProvider <- data.frame(x=sort(table(selected_trends()$Bookmaker), decreasing=TRUE)[1:length(names(sort(table(selected_trends()$Bookmaker))))])
    colnames(dataProvider) <- c("label","value")
    # build the chart
    amBarplot(x = "label", y = "value", data = dataProvider, labelRotation = -45, show_values = TRUE)  %>%
      addTitle(text = "Nombre de paris par bookmaker")
  })
  
  output$amdonuts3dtipster <- renderAmCharts({
    dataProvider <- data.frame(x=sort(table(selected_trends()$Tipster),decreasing=TRUE)[1:length(names(sort(table(selected_trends()$Tipster))))])
    colnames(dataProvider) <- c("label","value")
    # build the chart
    amPie(data = dataProvider, inner_radius = 50, depth = 10, show_values = FALSE, legend = TRUE) %>%
      addTitle(text = "Nombre de paris par tipster")
  })
  
  output$ampyramidetipster <- renderAmCharts({
    dataProvider <- data.frame(x=sort(table(selected_trends()$Tipster),decreasing=TRUE)[1:length(names(sort(table(selected_trends()$Tipster))))])
    colnames(dataProvider) <- c("description","value")
    # build the chart
    amFunnel(data = dataProvider, inverse = FALSE, legend=FALSE) %>%
      addTitle(text = "Nombre de paris par tipster")
  })
  
  output$ambarplottipster <- renderAmCharts({
    dataProvider <- data.frame(x=sort(table(selected_trends()$Tipster), decreasing=TRUE)[1:length(names(sort(table(selected_trends()$Tipster))))])
    colnames(dataProvider) <- c("label","value")
    # build the chart
    amBarplot(x = "label", y = "value", data = dataProvider, labelRotation = -45, show_values = FALSE)  %>%
      addTitle(text = "Nombre de paris par tipster") %>%
      # addGraph(valueField = "value", balloonText = "[[value]] picks") %>%
      setChartCursor(categoryBalloonEnabled="true")
  })
  
  #output$ammekko <- renderAmCharts({
  #  dataProvider <- data.frame(x=selected_trends()$Sport, y=selected_trends()$Bookmaker)
  #  amMekko(x = "y", y = "x", data = dataProvider)
  #})
  
  output$amplotgensport <- renderPlot({
    color = "#434343"
    plot(x = 1:length(selected_trendssport()$Etat), y = cumsum(rev(selected_trendssport()$Benefice)), type = "l", main="Evolution Bénéfice",
         xlab = "Nombre de paris", ylab = "Bénéfice", col = color, fg = color, col.lab = color, col.axis = color)
    # Display only if smoother is checked
    if(input$smoothersport){
      smooth_curve <- lowess(x = 1:length(selected_trendssport()$Etat), y = cumsum(rev(selected_trendssport()$Benefice)), f = input$fsport)
      lines(smooth_curve, col = "#E6553A", lwd = 3)
    }
  })
  
  output$amplotgen1sport <- renderAmCharts({
    # An2018Cote <- subset(An2018, Cote>= input$rangecote[1] & Cote <= input$rangecote[2])
    # xlab <- 1:length(An2018$Etat)
    # y <- cumsum(rev(An2018$Benefice))
    dataProvider <- data.frame(x=1:length(selected_trendssport()$Etat),y=cumsum(rev(selected_trendssport()$Benefice)))
    # build the chart
    amSerialChart(dataProvider = dataProvider, categoryField = "x") %>%
      setExport(enabled = TRUE) %>%
      addGraph(id="g1",valueField = "y", lineColor = "green", fillAlphas=0.3,
               bullet= "none",
               bulletBorderAlpha= 0,
               bulletColor = "white",
               bulletSize = 0,
               balloonText = "[[value]]") %>%
      addTitle(text = "Evolution gain cumulé") %>%
      addValueAxis(title = "Gains") %>%
      setCategoryAxis(dashLength= 1, minorGridEnabled=T) %>%
      addGuide(inside=T) %>%
      setBalloon(adjustBorderColor=F, fillColor = "white", borderThickness=1, shadowAlpha=0,
                 color = "white", cornerRadius = 25, fillAlpha=0.45, pointerOrientation="up") %>%
      setChartCursor(valueLineEnabled=T,
                     valueLineBalloonEnabled=F,
                     cursorAlpha=10,
                     cursorColor="green",
                     bulletsEnabled=F,
                     limitToGraph="g1",
                     valueLineAlpha=0)
  })
  
  output$amplotgentipster <- renderPlot({
    color = "#434343"
    plot(x = 1:length(selected_trendstipster()$Etat), y = cumsum(rev(selected_trendstipster()$Benefice)), type = "l", main="Evolution Bénéfice",
         xlab = "Nombre de paris", ylab = "Bénéfice", col = color, fg = color, col.lab = color, col.axis = color)
    # Display only if smoother is checked
    if(input$smoothertipster){
      smooth_curve <- lowess(x = 1:length(selected_trendstipster()$Etat), y = cumsum(rev(selected_trendstipster()$Benefice)), f = input$ftipster)
      lines(smooth_curve, col = "#E6553A", lwd = 3)
    }
  })
  
  output$amplotgen1tipster <- renderAmCharts({
    # An2018Cote <- subset(An2018, Cote>= input$rangecote[1] & Cote <= input$rangecote[2])
    # xlab <- 1:length(An2018Cote$Etat)
    # y <- cumsum(rev(An2018Cote$Benefice))
    dataProvider <- data.frame(x=1:length(selected_trendstipster()$Etat),y=cumsum(rev(selected_trendstipster()$Benefice)))
    # build the chart
    amSerialChart(dataProvider = dataProvider, categoryField = "x") %>%
      setExport(enabled = TRUE) %>%
      addGraph(id="g1",valueField = "y", lineColor = "green", fillAlphas=0.3,
               bullet= "none",
               bulletBorderAlpha= 0,
               bulletColor = "white",
               bulletSize = 0,
               balloonText = "[[value]]") %>%
      addTitle(text = "Evolution gain cumulé") %>%
      addValueAxis(title = "Gains") %>%
      setCategoryAxis(dashLength= 1, minorGridEnabled=T) %>%
      addGuide(inside=T) %>%
      setBalloon(adjustBorderColor=F, fillColor = "white", borderThickness=1, shadowAlpha=0,
                 color = "white", cornerRadius = 25, fillAlpha=0.45, pointerOrientation="up") %>%
      setChartCursor(valueLineEnabled=T,
                     valueLineBalloonEnabled=F,
                     cursorAlpha=10,
                     cursorColor="green",
                     bulletsEnabled=F,
                     limitToGraph="g1",
                     valueLineAlpha=0)
  })
  
  output$amplotgenbook <- renderPlot({
    color = "#434343"
    plot(x = 1:length(selected_trendsbook()$Etat), y = cumsum(rev(selected_trendsbook()$Benefice)), type = "l", main="Evolution Bénéfice",
         xlab = "Nombre de paris", ylab = "Bénéfice", col = color, fg = color, col.lab = color, col.axis = color)
    # Display only if smoother is checked
    if(input$smootherbook){
      smooth_curve <- lowess(x = 1:length(selected_trendsbook()$Etat), y = cumsum(rev(selected_trendsbook()$Benefice)), f = input$fbook)
      lines(smooth_curve, col = "#E6553A", lwd = 3)
    }
  })
  
  output$amplotgen1book <- renderAmCharts({
    # An2018Cote <- subset(An2018, Cote>= input$rangecote[1] & Cote <= input$rangecote[2])
    # xlab <- 1:length(An2018Cote$Etat)
    # y <- cumsum(rev(An2018Cote$Benefice))
    dataProvider <- data.frame(x=1:length(selected_trendsbook()$Etat),y=cumsum(rev(selected_trendsbook()$Benefice)))
    # build the chart
    amSerialChart(dataProvider = dataProvider, categoryField = "x") %>%
      setExport(enabled = TRUE) %>%
      addGraph(id="g1",valueField = "y", lineColor = "green", fillAlphas=0.3,
               bullet= "none",
               bulletBorderAlpha= 0,
               bulletColor = "white",
               bulletSize = 0,
               balloonText = "[[value]]") %>%
      addTitle(text = "Evolution gain cumulé") %>%
      addValueAxis(title = "Gains") %>%
      setCategoryAxis(dashLength= 1, minorGridEnabled=T) %>%
      addGuide(inside=T) %>%
      setBalloon(adjustBorderColor=F, fillColor = "white", borderThickness=1, shadowAlpha=0,
                 color = "white", cornerRadius = 25, fillAlpha=0.45, pointerOrientation="up") %>%
      setChartCursor(valueLineEnabled=T,
                     valueLineBalloonEnabled=F,
                     cursorAlpha=10,
                     cursorColor="green",
                     bulletsEnabled=F,
                     limitToGraph="g1",
                     valueLineAlpha=0)
  })
  
  output$amhistsport <- renderAmCharts({
    bysport1 <- group_by(selected_trends(), Sport)
    bysport <- summarise(bysport1, BenCum=sum(Benefice))
    dataProvider <- data.frame(x=bysport$Sport,y=bysport$BenCum)
    colnames(dataProvider) = c("Sport","Benefice")
    dataProvider <- dataProvider[order(dataProvider$Benefice, decreasing = T),]
    # build the chart
    amBarplot(x="Sport", y="Benefice", data = dataProvider, labelRotation = -90, show_values = FALSE)  %>%
      addTitle(text = "Bénéfice par sport") %>%
      setChartCursor(categoryBalloonEnabled="true")
  })
  
  output$amhisttipster <- renderAmCharts({
    bytipster1 <- group_by(selected_trends(), Tipster)
    bytipster <- summarise(bytipster1, BenCum=sum(Benefice))
    dataProvider <- data.frame(x=bytipster$Tipster,y=bytipster$BenCum)
    colnames(dataProvider) = c("Tipster","Benefice")
    dataProvider <- dataProvider[order(dataProvider$Benefice, decreasing = T),]
    # build the chart
    amBarplot(x="Tipster", y="Benefice", data = dataProvider, labelRotation = -90, show_values = FALSE)  %>%
      addTitle(text = "Bénéfice par tipster") %>%
      setChartCursor(categoryBalloonEnabled="true")
  })
  
  output$amhistbook <- renderAmCharts({
    bybook1 <- group_by(selected_trends(), Bookmaker)
    bybook <- summarise(bybook1, BenCum=sum(Benefice))
    dataProvider <- data.frame(x=bybook$Bookmaker,y=bybook$BenCum)
    colnames(dataProvider) = c("Bookmaker","Benefice")
    dataProvider <- dataProvider[order(dataProvider$Benefice, decreasing = T),]
    # build the chart
    amBarplot(x="Bookmaker", y="Benefice", data = dataProvider, labelRotation = -90, show_values = FALSE)  %>%
      addTitle(text = "Bénéfice par bookmaker") %>%
      setChartCursor(categoryBalloonEnabled="true")
  })
  
  output$NbParisGenBox <- renderValueBox({
    valueBox(
      paste0(dim(selected_trends())[1]), "Nombre de Paris", icon = icon("list"),
      color = "yellow"
    )
  })
  
  output$BeneficeGenBox <- renderValueBox({
    valueBox(
      paste0(sum(rev(selected_trends()$Benefice))), "Bénéfices", icon = icon("credit-card"),
      color = "green"
    )
  }) 
  
  output$RoiGenBox <- renderValueBox({
    valueBox(
      paste0(100*(round(sum(rev(selected_trends()$Benefice))/sum(selected_trends()$Mise),4)), "%"), "ROI", icon = icon("thumbs-up", lib = "glyphicon"), 
      color = "purple"
    )
  })
  
  output$CoteMoyBox <- renderValueBox({
    valueBox(
      paste0(round(mean(selected_trends()$Cote),2)), "Cote Moyenne", icon = icon("th"), 
      color = "red"
    )
  })
  
  output$NbParisSportBox <- renderValueBox({
    valueBox(
      paste0(dim(selected_trendssport())[1]), "Nombre de Paris", icon = icon("list"),
      color = "yellow"
    )
  })
  
  output$BeneficeSportBox <- renderValueBox({
    valueBox(
      paste0(sum(rev(selected_trendssport()$Benefice))), "Bénéfices", "ROI", icon = icon("credit-card"),
      color = "green"
    )
  }) 
  
  output$RoiSportBox <- renderValueBox({
    valueBox(
      paste0(100*(round(sum(rev(selected_trendssport()$Benefice))/sum(selected_trendssport()$Mise),4)), "%"), "ROI", icon = icon("thumbs-up", lib = "glyphicon"), 
      color = "purple"
    )
  })
  
  output$CoteMoySportBox <- renderValueBox({
    valueBox(
      paste0(round(mean(selected_trendssport()$Cote),2)), "Cote Moyenne", icon = icon("th"), 
      color = "red"
    )
  })
  
  output$NbParisTipsterBox <- renderValueBox({
    valueBox(
      paste0(dim(selected_trendstipster())[1]), "Nombre de Paris", icon = icon("list"),
      color = "yellow"
    )
  })
  
  output$BeneficeTipsterBox <- renderValueBox({
    valueBox(
      paste0(sum(rev(selected_trendstipster()$Benefice))), "Bénéfices", "ROI", icon = icon("credit-card"),
      color = "green"
    )
  }) 
  
  output$RoiTipsterBox <- renderValueBox({
    valueBox(
      paste0(100*(round(sum(rev(selected_trendstipster()$Benefice))/sum(selected_trendstipster()$Mise),4)), "%"), "ROI", icon = icon("thumbs-up", lib = "glyphicon"), 
      color = "purple"
    )
  })
  
  output$CoteMoyTipsterBox <- renderValueBox({
    valueBox(
      paste0(round(mean(selected_trendstipster()$Cote),2)), "Cote Moyenne", icon = icon("th"), 
      color = "red"
    )
  })
  
  output$NbParisBookBox <- renderValueBox({
    valueBox(
      paste0(dim(selected_trendsbook())[1]), "Nombre de Paris", icon = icon("list"),
      color = "yellow"
    )
  })
  
  output$BeneficeBookBox <- renderValueBox({
    valueBox(
      paste0(sum(rev(selected_trendsbook()$Benefice))), "Bénéfices", "ROI", icon = icon("credit-card"),
      color = "green"
    )
  }) 
  
  output$RoiBookBox <- renderValueBox({
    valueBox(
      paste0(100*(round(sum(rev(selected_trendsbook()$Benefice))/sum(selected_trendsbook()$Mise),4)), "%"), "ROI", icon = icon("thumbs-up", lib = "glyphicon"), 
      color = "purple"
    )
  })
  
  output$CoteMoyBookBox <- renderValueBox({
    valueBox(
      paste0(round(mean(selected_trendsbook()$Cote),2)), "Cote Moyenne", icon = icon("th"), 
      color = "red"
    )
  })
  
  output$table11 <- DT::renderDataTable(DT::datatable({
    data <- An2018
    if (input$sport11 != "All") {
      data <- data[data$Sport == input$sport11,]
    }
    if (input$book11 != "All") {
      data <- data[data$Bookmaker == input$book11,]
    }
    if (input$tipster11 != "All") {
      data <- data[data$Tipster == input$tipster11,]
    }
    data
  }))
  
  output$mytable11 <- DT::renderDataTable({
    DT::datatable(An2018[, input$show_vars, drop = FALSE], options = list(orderClasses = TRUE, searching = FALSE, lengthMenu = c(10, 20, 30), lengthMenu=15))
  })
}


shinyApp(ui, server)

# runGitHub("Shiny-Apps", "Twan76", subdir = "Betting")
