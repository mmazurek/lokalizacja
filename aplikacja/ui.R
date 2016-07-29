
library(shiny)


shinyUI(fluidPage(
  

  titlePanel("Wykres błędów estymacji"),
  h5("Mazurek Magdalena"),

   sidebarLayout(
    sidebarPanel(
      h6("Aplikacja umożliwia wczytanie własnego pliku. Aby wybrać plik z katalogu, należy
          wybrać poniżej opcję 'z własnego katalogu', a następnie wybrać plik."),
      radioButtons("in_plik", "Wybór pliku", c("testowy", "z własnego katalogu"), selected = "testowy"),
       
      conditionalPanel("input.in_plik=='z własnego katalogu'",
                        fileInput('file1', 'Wybierz plik `.csv` (read.csv)',
                                  accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))   ),
       
      radioButtons("in_pietro", "Piętro", 
                    0:5)
    ),
    

    mainPanel(
       h5("Poniższy wykres przedstawia położenie punktów pomiarowych. 
          Po zaznaczeniu pewnego obszaru i kliknięciu na niego dwukrotnie otrzymamy 
          zbliżenie zaznaczonego obszaru wykresu. Kolorystyka punktów odpowiada średniej
          błędów dla punktów pomiarowych.
          Dodatkowo po kliknięciu na dany punkt
          otrzymujemy wykres pudełkowy błędów dla tego punktu wraz ze statystykami.
          Jeśli były błędy w estymacji piętra, to otrzymujemy również tabele z informacją o 
          liczbie błędów."),
       plotOutput("wykr_punkt", 
                  dblclick = "wykr_punkt_dblclick",
                  brush = brushOpts( id = "wykr_punkt_brush", resetOnNew = TRUE),
                  click = "plot_click"
       ),
       
       plotOutput("wykr_boxplot",  height = "200px"),
       #verbatimTextOutput("click_info"),
       verbatimTextOutput("boxplot_info"),
       tableOutput("boxplot_table")    
    )
  )
))
