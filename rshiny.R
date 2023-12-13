library(shiny)
library(ggplot2)

ui <- navbarPage(
  title = "Grup C",
  tabPanel(
    title = "Data",
    fileInput("file1", "Choose CSV file",
              multiple = TRUE,
              accept = c(".csv")),
    tags$hr(),
    checkboxInput("header", "Header", TRUE),
    radioButtons("sep", "Separator",
                 c(Comma = ",",
                   Semicolon = ";",
                   Tab = "\t"),
                 "Comma"),
    radioButtons("dec", "Decimal separator",
                 c('Comma' = ",",
                   "Punktum"="."),
                 "komma"),
    tableOutput(outputId = "table")),
  tabPanel(
    title = "Summary",
    verbatimTextOutput("summary")),
  tabPanel(
    title = "Visualization",
    navlistPanel(
      tabPanel(title = "Cleveland Plot",
               selectInput("var", "Choose an Atribute",
                           c("Total Cases" = 1,
                             "Total Deaths" = 2,
                             "Total Recovered" = 3,
                             "Active Cases" = 4,
                             "Total Cases per Million" = 5,
                             "Total Deaths per Million" = 6,
                             "Total Tests" = 7,
                             "Total Tests per Million" = 8,
                             "Population" = 9),
                           "var"),
               textOutput("inter"),
               plotOutput("cleveland", height = 2000)),
      tabPanel(title = "Scatter Plot",
               selectInput("x", "X Variable",
                           c("Total Cases" = 1,
                             "Total Deaths" = 2,
                             "Total Recovered" = 3,
                             "Active Cases" = 4,
                             "Total Cases per Million" = 5,
                             "Total Deaths per Million" = 6,
                             "Total Tests" = 7,
                             "Total Tests per Million" = 8,
                             "Population" = 9),
                           "x"),
               selectInput("y", "Y Variable",
                           c("Total Cases" = 1,
                             "Total Deaths" = 2,
                             "Total Recovered" = 3,
                             "Active Cases" = 4,
                             "Total Cases per Million" = 5,
                             "Total Deaths per Million" = 6,
                             "Total Tests" = 7,
                             "Total Tests per Million" = 8,
                             "Population" = 9),
                           "y"),
               selectInput("type", "Plot Type",
                           c("Points" = "p",
                             "Lines" = "l",
                             "Both" = "b",
                             "Overplotted" = "o"),
                           "type"),
               textOutput("interpret"),
               plotOutput("scatter", height = 750)),
      tabPanel(title = "Correlation Heatmap",
               "Korelasi positif (berbanding lurus) ditunjukkan dengan nilai korelasi mendekati 1 sedangkan nilai korelasi negatif (berbanding terbalik) mendekati angka -1.",
               plotOutput("corr", height = 750))
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    req(input$file1)
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    
    df <- read.csv(inFile$datapath, header = input$header,
                   sep = input$sep, dec = input$dec)
    
    return(df)
  })
  
  output$summary <- renderPrint({
    df <- data()
    summary(df)
  })
  
  output$inter <- renderPrint({
    var <- as.numeric(input$var)
    
    if (var == 1)
    {"Negara dengan jumlah kasus COVID-19 terbanyak adalah negara USA, India, Brazil, diikuti negara lainnya."}
    else if (var == 2)
    {"Negara dengan jumlah kematian terbanyak adalah USA, Brazil, dan India, diikuti negara lainnya."}
    else if (var == 3)
    {"Negara dengan jumlah pasien sembuh dari COVID-19 terbanyak adalah negara USA, India, Brazil, diikuti negara lainnya."}
    else if (var == 4)
    {"Negara dengan jumlah pasien positif (per 16 Oktober 2021) terbanyak adalah USA, UK, Rusia, diikuti negara lainnya."}
    else if (var == 5)
    {"Jumlah kasus per juta penduduk tertinggi dipegang oleh Seychelles, Montenegro, dan Andorra."}
    else if (var == 6)
    {"Negara dengan jumlah kematian (per juta penduduk) akibat COVID-19 terbanyak adalah negara Bosnia Herzegovina, NorthMacedonia, Bulgaria, dan diikuti negara lainnya."}
    else if (var == 7)
    {"Jumlah tes COVID-19 terbanyak dilakukan di USA, India, dan UK."}
    else if (var == 8)
    {"Jumlah testing tiap sejuta penduduk terbanyak dilakukan oleh Denmark, Gibraltar, dan Austria."}
    else
    {"Negara dengan populasi terbanyak adalah China, India, USA, Indonesia, diikuti negara lainnya."}
  })
  
  output$cleveland <- renderPlot({
    df <- data()
    column <- as.numeric(input$var)
    
    ggplot(df, aes(x = df[,column],
                   y = reorder(Country, df[,column]))) +
      geom_point()
  })
  
  output$interpret <- renderPrint({
    x <- as.numeric(input$x)
    y <- as.numeric(input$y)

    df <- data()
    corr <- cor(df[,x], df[,y])
    
    if (x == y) {
      print("Variabel x dan y sama, silakan mengganti dengan variabel lain")
    } else if (corr > 0) {
      paste("Variabel", dimnames(df)[[2]][x], "dan", dimnames(df)[[2]][y],
            "memiliki korelasi positif (berbanding lurus)")
    } else {
      paste("Variabel", dimnames(df)[[2]][x], "dan", dimnames(df)[[2]][y],
            "memiliki korelasi negatif (berbanding terbalik)")
    }
  })
  
  output$scatter <- renderPlot({
    df <- data()
    x <- as.numeric(input$x)
    y <- as.numeric(input$y)
    
    plot(x = df[,x], y = df[,y], type = input$type,
         xlab = "X Variable", ylab = "Y Variable")
    abline(lm(df[,y]~df[,x]), col = "red", lwd = 3)
  })
  
  output$corr <- renderPlot({
    df <- data()
    data <- df[, -10]
    corrplot::corrplot(cor(data), method = 'number',
                       type = 'lower', order = 'AOE')
  })
}

shinyApp(ui, server)