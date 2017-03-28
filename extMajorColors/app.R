#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("ggmap")
library(shiny)
library(shinythemes)
library(png)
library(RCurl)
library(imager)
library(markdown)
library(ggmap)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

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
ui <- fluidPage(theme = shinytheme("slate"),
  # Application title
  titlePanel("주요 색상 추출기"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      includeMarkdown("info.md"),
      sliderInput("N","추출할 색상 수를 선택해주세요.",min=1,max=10,value="5"),
      tabsetPanel(type = "tabs", 
                  
                  tabPanel("Url 입력", 
                           br(),textInput("url",
                                          "색상을 추출하고자 하는 사진의 url을 입력해주세요. :",
                                          placeholder = "https://gephi.org/images/screenshots/layout2.png",
                                          value = ""
                           )),
                           #sliderInput("N","추출할 색상 수를 선택해주세요요.",min=1,max=10,value="5")),
                  tabPanel("파일 업로드", 
                           br(),
                           fileInput('file', '색상을 추출하고자 하는 사진을 업로드해주세요.'))
                           #sliderInput("N","추출할 색상 수를 선택해주세요요.",min=1,max=10,value="5"))
      )
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output ) {
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$url, {
    v$data <- input$url
  })
  
  observeEvent(input$file, {
    v$data <- input$file$datapath
  })
  
  observeEvent(input$N, {
    NULL
  })
  
  output$Plot <- renderPlot({
    if (v$data=="") return()
    png = imager::load.image(v$data)
    p1 <- ggimage(png[,,,])
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
    
    p2<-color_extractor(rgb_data = col_melt[3:5], colors = input$N)
    multiplot(p1, p2, cols=2)
  })
  
  # output$pic <- renderPlot({
  #   if (v$data=="") return()
  #   #png = imager::load.image(v$data)
  #   plot(png)
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)

