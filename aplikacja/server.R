
library(shiny)
require(reshape2)
require(ggplot2)
require(dplyr)
require(curl)
#require(repmis)

load( curl("https://raw.githubusercontent.com/mmazurek/lokalizacja/master/punkty.csv") )

#source_data("https://github.com/mmazurek/lokalizacja/blob/master/punkty.rda?raw=true")

odlegl <- sqrt((punkty$x_real-punkty$x_est)^2 + (punkty$y_real-punkty$y_est)^2)
blad_piet <- punkty$pietro!=punkty$pietro_est

punkty <- cbind(punkty, "blad"=odlegl, "blad_pietro"=blad_piet)

shinyServer(function(input, output, session) {
   
  ranges <- reactiveValues(x = NULL, y = NULL)

  observeEvent(input$wykr_punkt_dblclick, {
     brush <- input$wykr_punkt_brush
     if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
        
     } else {
        ranges$x <- NULL
        ranges$y <- NULL
     }
  })
  
  observeEvent(input$in_pietro, {
       ranges$x <- NULL
       ranges$y <- NULL
  })

  floorData <- eventReactive(input$in_pietro,{
     dane <- punkty[punkty$pietro==input$in_pietro,]
  })
   
  output$wykr_punkt <- renderPlot({
   srednie <- floorData() %>% group_by(x_real, y_real) %>% mutate(srednia=mean(blad))
      
     ggplot(srednie, mapping = aes(x_real,y_real)) +
        geom_point(aes(colour = srednia)) +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
        labs(title='Wykres punktów pomiarowych') +
        scale_color_gradient2(high = "tomato3", low= "slateblue3", mid="white", 
                              midpoint=min(srednie$srednia)+(max(srednie$srednia)-min(srednie$srednia))/2 )+
        theme( panel.background=element_rect(fill="gray65"),
               panel.grid.major = element_line(colour = "grey50"),
               panel.grid.minor = element_line(colour = "grey50"))
  })
  

  output$wykr_boxplot <- renderPlot({
     
     np <- nearPoints(floorData(), input$plot_click, xvar="x_real", yvar="y_real", threshold = 10, maxpoints = 1)
     
     if( nrow(np)==0 ){ frame() } else {
     
     dane2 <- floorData()[floorData()$x_real==np$x_real & floorData()$y_real==np$y_real,]
     
     dane2_wykres <- melt(dane2[,c("blad", "pietro")], id="pietro")
     
     
     ggplot(dane2_wykres, mapping = aes(y=value, x=pietro)) +
         geom_boxplot(fill="seagreen3") +
         stat_summary(fun.y=mean, geom="point", shape=19, size=2, col = "firebrick3")+
         labs(y="Błąd (odległość)", x="", title='Wykres błędów dla danego punktu pomiarowego')+
         scale_x_continuous(labels= rep("",3), breaks = min(dane2_wykres$pietro)+c(-1,0,1) )+
         scale_y_continuous(breaks = seq(floor(min(dane2_wykres$value))-0.5, floor(max(dane2_wykres$value))+1.5, by=1))+
         coord_flip()
   
     }
   })
  

  output$click_info <- renderPrint({
     nearPoints(floorData(), input$plot_click, xvar="x_real", yvar="y_real", threshold = 10, maxpoints = 1)
  })
  
  output$boxplot_info <- renderPrint({
     
     np <- nearPoints(floorData(), input$plot_click, xvar="x_real", yvar="y_real", threshold = 10, maxpoints = 1)
     
     if( nrow(np)==0 ){ "" } else {
        dane2 <- floorData()[floorData()$x_real==np$x_real & floorData()$y_real==np$y_real,]
        summary(dane2$blad)
     }
  })
  
  output$boxplot_table <- renderTable({
     
     np <- nearPoints(floorData(), input$plot_click, xvar="x_real", yvar="y_real", threshold = 10, maxpoints = 1)
     
     if( nrow(np)==0 ){ data.frame() } else {
        dane2 <- floorData()[floorData()$x_real==np$x_real & floorData()$y_real==np$y_real,]
        dane3 <- dane2[dane2$blad_pietro==TRUE,]
        if (nrow(dane3)==0){ data.frame() } else {
           tabela <- as.data.frame(table(dane3$pietro_est))
           colnames(tabela) <- c("pietro_est", "n")
           tabela
        }
     }
  })

  
})


# est_floor <- dane$floor
# est_floor[sample(1:114460,5723)] <- sample(0:5,5723,replace = TRUE)
# 
# punkty <- data.frame("x_real"=dane$x,
#                       "x_est"=dane$x+rnorm(length(dane$x), 0,3),
#                       "y_real"=dane$y,
#                       "y_est"=dane$y+rnorm(length(dane$y), 0,3),
#                      "pietro"=dane$floor,
#                      "pietro_est"=est_floor)
