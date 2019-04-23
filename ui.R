library(shiny)
library(plotly)

ui <- fluidPage(
  
  headerPanel("Instagram Explorer"),
  sidebarPanel(width="2",
               selectInput('y', 'Type', choices = c("Followers","Following","Likes Given"), selected = "Followers",width = "140px"),
               radioButtons("r1", "View Type:",c("Yearly","Monthly"), selected = "Yearly",inline=F),
               br(),
               br(),
               selectInput('z', 'Media Based Proportion', choices = c("Uploads"), selected = "Uploads",width = "200px"),
               radioButtons("r2", "View Type:", c("Yearly","Monthly"), selected = "Yearly",inline=F)
  ),
  mainPanel(
    plotlyOutput('trendPlot',width="980px",height="330px"),
    br(),
    br(),
    plotlyOutput('propPlot',width="980px",height="330px")
  )
)