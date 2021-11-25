library(shiny)

getwd()
nzwSpolek<-data.frame(nazwy=c("WIG20","Comarch","POK Bank Polski","Jastrzębska Spółka Węglowa"),
                      skroty=c("WIG20","CMR","PKO","JSW"))

getName <- function(nazwa) {
  return(nzwSpolek$skroty[nzwSpolek$nazwy == nazwa])
}
nzwPlikow <- data.frame(nazwy=c("WIG20","Comarch","POK Bank Polski","Jastrzębska Spółka Węglowa"),
                        pliki=c("wig_d.csv","cmr_d.csv","pko_d.csv","jsw_d.csv"))

findDataSet <- function(nazwa){
  return(nzwPlikow$pliki[nzwPlikow$nazwy==nazwa])
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # 1 strona
  splk <- reactive({getName(input$spolka)}) # wczytanie nazw
  output$name1 = renderText({ paste("Skrót : ", nzwSpolek[nzwSpolek$nazwy==input$spolka,][,2] ) })
  # wypisanie nazwy / ścieżki dostępu
  d<-read.csv("Dane_spolek.csv") # wczytanie danych
  output$print1 <- renderPrint({
    sknazwy <- nzwSpolek[nzwSpolek$nazwy==input$spolka,][,2]
    summary(d[,sknazwy])
  })
  
  
  output$distPlot <- renderPlot({
    # generowanie liczby słupków histogramu według input$bins z ui.R
    sknazwy <- nzwSpolek[nzwSpolek$nazwy==input $spolka,][,2]
    x<- d[,sknazwy]
    bins <- seq(min(x,na.rm = T), max(x,na.rm = T), length.out = input$bins + 1) # zeby dzielilo histogram na bins slupków trzeba dokladnie tak zapisac
    
    # rysowanie histogramu
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  
  # 2 strona
  splk2 <- reactive({getName(input$spolka2)}) # wczytanie nazw
  output$name2 = renderText({ input$spolka2 }) # wypisanie nazwy / ścieżki dostępu
  d <- read.csv("Dane_spolek.csv") # wczytanie spółek - lokalnie z dysku
  
  
  output$zPlot <- renderPlot({
    # generowanie wykresu szeregu
    sknazwy <- nzwSpolek[nzwSpolek$nazwy==input$spolka2,][,2]
    x<-d[,sknazwy]
    data = as.Date(d$Data, "%Y-%m-%d")
    x.m <- filter(x, sides=2, rep(1,69)/69) # wyrównanie szeregu
    # wykres na podstawie wyboru checkbox
    if(input$plot == T){ 
      plot(data,x,type="l", main=c('Wykres szeregu'),
           ylab="Prices", xlab="Time")
      if(input$smooth == T){ 
        lines(data,x.m, col="red",lty="dashed")} } # wykres wyrównany
    if(input$plot == F){ # jeśli nie rysujemy szeregu 
      if (input$smooth == T){ 
        plot(data,x.m, type="l",main='Wykres wyrównany', col='red',
             ylab="Prices", xlab="Time")}
      else
        plot.new()} 
  })
  
  
  #3 strona 1 podstrona
  splk3 <- reactive({getName(input$spolka3)}) # wczytanie nazw
  d <- read.csv("Dane_spolek.csv") # wczytanie spółek - lokalnie z dysku
  output$name4 = renderText({ input$spolka3 }) # nazwy spółek
  output$value <- renderText({ as.numeric(input$radio) }) # wybrana liczba dni do wyrównania
  output$analiza1 <- renderPrint({
    cat('Wyrównano liczbą dni ') # zwykłe wypisanie tekstu
  })
  
  output$z2Plot <- renderPlot({ # wykres
    sknazwy <- nzwSpolek[nzwSpolek$nazwy==input$spolka3,][,2]
    x<-d[,sknazwy]
    data = as.Date(d$Data, "%Y-%m-%d")
    # wyrównanie średnią ruchomą
    dl<-as.numeric(input$radio)
    x.m <- filter(x, sides=2, rep(1,dl)/dl)
    # rysowanie wykresu
    plot(data,x,type="l",col="darkblue", main=c('Wykres szeregu'),
         ylab="Zamknięcie", xlab="Dni",lwd =2)
    lines(data,x.m, col="red",lty="dashed",lwd =3)
  })

})

