library(shiny)
library(shinythemes)
library(png)
library(RCurl)
library(imager)
library(markdown)
library(ggmap)
library(gridExtra)

color_extractor = function(png, colors = 3){
  
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
  
  kmeans_result = kmeans(x = col_melt[, 3:5], centers = colors)
  
  df = data.frame(x = 1,
                  Index = 1:colors)
  df = cbind(df, kmeans_result$centers)
  df[, "Hex"] = rgb(red   = df[, 3],
                    green = df[, 4],
                    blue  = df[, 5])
  
  gg = ggplot(data = df,
              aes(x = x,
                  y = Index)) + 
    geom_tile(aes(fill = Hex)) + 
    geom_text(aes(label = Hex), size = 10) + 
    scale_fill_identity() + 
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_blank())
  return(list(plot = gg,
              data = col_melt,
              colors = df))
}
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("paper"),
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
      ),
      dataTableOutput("result")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output ) {
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$url, {
    v$data <- input$url
    if(!grepl("^htt",input$url)){v$data <- paste0("http://",v$data)}
  })
  
  observeEvent(input$file, {
    v$data <- input$file$datapath
  })
  
  observeEvent(input$N, {
    NULL
  })
  
  output$result <- renderDataTable({
    if (v$data=="") return()
    png = imager::load.image(v$data)
    color_extractor(png=png, colors = input$N)[["colors"]][-1]
    })
  
  output$plot <- renderPlot({
    if (v$data=="") return()
    png = imager::load.image(v$data)
    pngf = imrotate(png,270)
    
    p1 <- ggimage(pngf[,,,])
    p2 <- color_extractor(png=png, colors = input$N)[["plot"]]
    grid.arrange(p1, p2, ncol=2)
  })
  
  # output$pic <- renderPlot({
  #   if (v$data=="") return()
  #   #png = imager::load.image(v$data)
  #   plot(png)
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)

