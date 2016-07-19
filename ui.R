
library(shiny)


shinyUI(fluidPage(
  

  titlePanel("Wykres błędów estymacji"),
  h5("Mazurek Magdalena"),

   sidebarLayout(
    sidebarPanel(
       radioButtons("in_pietro", "Piętro", 
                    0:5)
    ),
    

    mainPanel(
       h5("Poniższy wykres przedstawia położenie punktów pomiarowych. 
          Po zaznaczeniu pewnego obszaru i kliknięciu na niego dwukrotnie otrzymamy 
          zbliżenie zaznaczonego obszaru wykresu. Dodatkowo po kliknięciu na dany punkt
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
