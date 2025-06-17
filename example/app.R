library(shiny)
library(tidyverse)
library(bslib)

makey<-function(x,w){w*x+rnorm(length(x),10,2)*(1-w)}

ui <- page_fillable(
   theme = bs_theme(
      bootswatch = "darkly",
      primary = "#007777",
      success = "#4CF4F4",
      secondary = "#4C7F7F",
      "table-color" = "#4CF4F4",
      base_font = font_google("Encode Sans Condensed"),
      font_weight_base = "font-weight-light",
      headings_font_weight = 300,
      font_scale = .7
   ),
   titlePanel("My random scatterplot"),
   sidebarLayout(
      sidebarPanel(
         fileInput("file","Data:"),
         radioButtons("colour","Point colour:",c("red","blue","green","yellow")),
         selectInput("shape","Point Shape:",choices = c(0:8)),
         numericInput("w","Weight for X:", min=0, max=1, step=.05, value=.5)
      ),
      # mainPanel(
      #    tabsetPanel(type="pills",
      #      tabPanel("Descriptives",textOutput("descriptives")),
      #      tabPanel("Graph",plotOutput("distPlot"))
      #    )
      mainPanel(
         
         layout_columns(
            card( card_header("Card1"), textOutput("descriptives") ),
            card( card_header("Card2"), ),
            card( card_header("Card3"),  ),
            card( card_header("Card4"),  ),
            card( card_header("Card5"), ) ,
            col_widths = c(2,5,5, 6,6),
            row_heights = c(2,5)
         )
            )
         
         
   )
)

server <- function(input, output) {
   
   data<-reactive({
      if(!is.null(input$file)){
         file<-input$file
         data<-read_csv(file$datapath)
         data<-data|>mutate(y=makey(x,input$w))
      }
   })
   
   output$distPlot <- renderPlot({
      if(!is.null(input$file)){
         data()|>ggplot((aes(x=x, y=y)))+
            geom_point(colour=input$colour,shape=as.numeric(input$shape))+
            theme_minimal()
      }
   })
   
   output$descriptives <- renderText({
      if(!is.null(input$file)){ 
         data<-data()
         paste("There are",nrow(data),
               "observations with a mean of",round(mean(data$x),1),
               "and a standard deviation of",round(sd(data$x),2)
         )
      }
   })
}

shinyApp(ui = ui, server = server)