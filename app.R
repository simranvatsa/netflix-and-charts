library(shiny)
library(tidyverse)
library(ggvis)
library(zoo)
library(randomcoloR)

ui <- fluidPage(
  
  h2("Netflix and Charts"),
  p("Take off your blindfold for a look at your Netflix viewing activity that's interactive, like Black Mirror: Bandersnatch.
    I've tidied up this data so much that even Marie Kondo would approve!"),
  tags$a(href = "https://www.netflix.com/ViewingActivity", "You can download your Netflix viewing history here."),
  p("A caveat: While the dataset theoretically contains all titles you've watched on Netflix, it does not keep records of repeat viewings. So, for example, when I rewatched \"To All The Boys I've Loved Before\" in December 2018, my original August viewing was erased from the dataset."),
  p("Also, the chart filters out any TV show of which only one episode has been watched."),
  p("The chart you see prior to uploading your own data displays my Netflix data for a year, based on a file containing my viewing history until Feb 10, 2019. You can play around with it if you don't want to upload your data/don't use Netflix!"),
  br(),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("netflix_history", "Upload your viewing history:",
                multiple = FALSE,
                accept = c("text/csv")),
      
      tags$hr(),
      
      checkboxGroupInput(inputId = "media_type", "Media types to display:", 
                         choices = list("TV", "movies and specials"), 
                         selected = list("TV", "movies and specials")),
      
      tags$hr(),
      
      dateInput(inputId = "time_begin", "Start date:", 
                  min = "2007-01-01", 
                  max = Sys.Date(), 
                  value = "2018-01-01"),
      dateInput(inputId = "time_end", "End date:", 
                min = "2007-01-01", 
                max = Sys.Date(), 
                value = "2018-12-31"),
      width = 3
    ),
    
    mainPanel(
      ggvisOutput("dotplot")
    )
    
  )
)


server <- function(input, output) {
  
  netflix <- reactive({
    infile <- input$netflix_history       
    if(is.null(infile))
      data <- read.csv("Data/NetflixViewingHistory.csv")
    else
      data <- read.csv(input$netflix_history$datapath, encoding = "UTF-8")
    
    data
  })
  
  modified_data <- reactive({
    netflix <- netflix()
    netflix$Media_Type <- ifelse(str_count(netflix$Title, ":") >= 2, "TV", "movies and specials")
    netflix$Date <- as.Date(netflix$Date, "%m/%d/%y")
    netflix$Month_Year <- factor(as.yearmon(netflix$Date))
    netflix$Year <- str_sub(as.character(netflix$Month_Year), 5, 8) %>% as.numeric()
    netflix$`Show Title` <- ifelse(netflix$Media_Type == "TV", 
                              word(netflix$Title, sep = ":"), "Movie/Special")
    
    to_include <- netflix %>% count(`Show Title`) %>% filter(`Show Title` != "SHOW", n > 2)
    per_month <- netflix %>% filter(`Show Title` %in% to_include$`Show Title`) %>% count(Month_Year)
    netflix <- netflix %>% 
      filter(`Show Title` != "SHOW")
    
    Position <- c()
    for(i in 1:nrow(per_month)) {
      Position <- c(Position, 1:per_month$n[i])
    }
    
    netflix <- netflix %>%
      filter(`Show Title` %in% to_include$`Show Title`) %>% 
      mutate(Position = rev(Position)) 
    
    netflix <- netflix %>% 
      filter(Media_Type %in% input$media_type) %>% 
      filter(Date >= input$time_begin, Date <= input$time_end)
    
    netflix$Month_Year <- droplevels(netflix$Month_Year)
    
    netflix
  })
  
  tooltip_values <- function(df) {
    
    modified_data <- isolate(modified_data())
    modified_data <- modified_data[modified_data$Title == df$Title, ]
    paste0(modified_data$Title, "<br>", 
           "Date watched: ", modified_data$Date)
    
  }
  
  vis1 <- reactive({
    
    plot_data <- modified_data() 

    plot_data %>% 
      ggvis(x = ~Month_Year, y = ~Position, fill = ~`Show Title`, key := ~Title) %>% 
      layer_points(size := 40) %>% 
      add_tooltip(tooltip_values, "hover") %>% 
      add_axis("x", title = "", properties = 
                 axis_props(labels = list(angle = -45, fontSize = 10)), 
               tick_padding = 15) %>% 
      add_axis("y", title = "", offset = 5) %>% 
      set_options(height = 500, width = 850) %>% 
      scale_ordinal("fill", range = distinctColorPalette(k = 70, altCol = FALSE, runTsne = FALSE)) %>% 
      add_legend("fill", title = "Program title",
                 properties = legend_props(title = list(fontSize = 14),
                   labels = list(fontSize = 12)))
    
  })
  vis1 %>% bind_shiny("dotplot")

}

shinyApp(ui, server)

