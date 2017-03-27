#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(png)
library(RCurl)
library(imager)

color_extractor = function(rgb_data, colors = 3){
  kmeans_result = kmeans(x = rgb_data, centers = colors)
  df = data.frame(x = 1,
                  y = 1:colors)
  df = cbind(df, kmeans_result$centers)
  df[, "Hex"] = rgb(red   = df[, 3],
                    green = df[, 4],
                    blue  = df[, 5])
  
  library("ggplot2")
  gg = ggplot(data = df,
              aes(x = x,
                  y = y)) + 
    geom_tile(aes(fill = Hex)) + 
    geom_text(aes(label = Hex), size = 10) + 
    scale_fill_identity() + 
    theme(legend.position = "none")
  return(gg)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("주요 색상 추출기"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        tabsetPanel(type = "tabs", 
                    tabPanel("Url 입력", 
                             includeText("최적화가 되지 않아 아직 많이 느립니다. 인내심을 가지고 기다리시면 처리 됩니다. png, jpg 등 이미지라면 동작하는 것을 확인했습니다만, 문제가 있으면 [여기](https://github.com/mrchypark/shiny_extract_major_colors_form_png/issues)에 남겨주세요. 감사합니다."),
                             br(),textInput("url",
                                       "색상을 추출하고자 하는 사진의 url을 입력해주세요. :",
                                       value = "https://gephi.org/images/screenshots/layout2.png"
                             ),
                             actionButton("do", "처리하기"))
                    #tabPanel("Summary", fileInput('file', '색상을 추출하고자 하는 사진을 업로드해주세요.'))
        )

      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("pic"),
         plotOutput("Plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$url, {
    v$data <- input$url
  })
  
  # observeEvent(input$file, {
  #   save.image(input$file,"./tmp/aoweihf.im")
  #   v$data <- system.file("./tmp/aoweihf.im")
  # }) 
  
   output$Plot <- renderPlot({
     if (is.null(v$data)) return()
     png = imager::load.image(v$data)

     for(col in 1:3){
       col_sub = png[, , ,col]
       col_sub = cbind(line = nrow(col_sub):1,
                       as.data.frame(col_sub))
       if(col == 1){
         col_melt = reshape2::melt(data = col_sub,
                                   id.vars = "line")  
       } else {
         col_melt[, ncol(col_melt) + 1] = reshape2::melt(data = col_sub,
                                                         id.vars = "line")[, "value"]
       }
     }
     col_melt[, "variable"] = as.numeric(substr(col_melt[, "variable"], 2, 4))
     colnames(col_melt)[3:5] = c("Red", "Green", "Blue")
     col_melt[, "obs"] = 1:nrow(col_melt)

     color_extractor(rgb_data = col_melt[3:5], colors = 5)
   })
   output$pic <- renderPlot({
     if (is.null(v$data)) return()
     png = imager::load.image(v$data)
     plot(png)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

