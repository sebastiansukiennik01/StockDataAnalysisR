library(shiny)
ui <- fluidPage(
  titlePanel("Dane giełdowe"),
  navbarPage("",
             tabPanel('Notowania',uiOutput('page1'),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("bins",
                                      "Number of bins:",
                                      min = 1,
                                      max = 50,
                                      value = 30),
                          selectizeInput(
                            'spolka', label = 'Spółka',
                            choices = nzwSpolek$nazwy,
                            multiple=FALSE, selected='Elzab',
                            options = list(create = TRUE))
                        ), 
                        ### Pokaż wykres histogramu
                        mainPanel(
                          verbatimTextOutput("name1"),
                          verbatimTextOutput('print1'),
                          plotOutput("distPlot")
                        )
                      )
             ),
             tabPanel('Wyrównanie', uiOutput('page2'),
                      sidebarLayout(
                        sidebarPanel(
                          selectizeInput(
                            'spolka2', label = 'Spółka',
                            choices = nzwSpolek$nazwy,
                            multiple=FALSE, selected='Elzab',
                            options = list(create = TRUE)
                          ),
                          checkboxInput('plot',"Wykres",FALSE),
                          checkboxInput('smooth',"Wyrównanie",FALSE)
                        ), 
                        #Pokaż wykres szeregu czasowego
                        mainPanel(
                          h3(verbatimTextOutput("name2")),
                          plotOutput("zPlot")
                        ) 
                      )
             ),
             tabPanel("Analiza 1",
                        sidebarLayout(
                          sidebarPanel(
                            selectizeInput( # wybór nazwy spółki po raz trzeci
                              'spolka3', label = 'Spółka',
                              choices = nzwSpolek$nazwy,
                              multiple=FALSE, selected='Elzab',
                              options = list(create = TRUE)),
                            # tworzenie przycisków jednokrotnego wyboru
                            radioButtons("radio", label = h3("Wybór okresu wyrównania"),
                                         choices = list("miesięczny" = 23,
                                                        "kwartalny" = 69),
                                         selected = 23)
                          ), 
                          mainPanel(
                            h3(verbatimTextOutput('name4')), # wypisanie nazwy
                            # podział na kolumny
                            #column(3,img(src='shiny-logo.png', height = 50, width = 50)), # wstawienie rysunku
                            column(5,h2(verbatimTextOutput('analiza1'))), # wypisanie tekstu cat
                            column(4,h2(verbatimTextOutput('value'))), # liczba dni
                            plotOutput("z2Plot") # rysowanie wykresu
                          ) 
                        )
             )
  )
)
