library(tidyverse)
library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)


ui <- dashboardPage(
    dashboardHeader(title = "BPD Arrest"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Plotly", tabName = "page1", icon = icon("line-chart")),
            menuItem("Density", tabName = "page2", icon = icon("area-chart")),
            menuItem("Map", tabName = "page3", icon = icon("map-o")),
            menuItem("Data", tabName = "page4", icon = icon("database"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "page1",
                    checkboxInput("holiday", label = "Show holidays", value = FALSE),
                    plotlyOutput("plot2", height = 500)
                    ),
            tabItem(tabName = "page2",
                    sliderInput("year", "Year:", min = 2014, max = 2020, value = 1, 
                                step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
                    plotOutput("plot1")
                    ),
            tabItem(tabName = "page3",
                    leafletOutput("myMap", width="100%")),
            tabItem(tabName = "page4",
                    dataTableOutput("myTable")        
                    )
        )
    )
)


server <- function(input, output, session) {
  
  data <- read_csv("data.csv")
  View(data)
  
  data$ArrestDate = as.Date(data$ArrestDate,  format = "%m/%d/%Y")
  str(data)
  
  round(data$Longitude, digits = 5)
  round(data$Latitude, digits = 5)
  
  us <- read_csv("usholidays.csv")
  View(us)
  holidays <- subset(us, select = -1)
  View(holidays)
  
  holidays$Date = as.Date(holidays$Date,  format = "%Y-%m-%d")
  colnames(holidays)[1] = "ArrestDate"
  str(holidays)
  
  words <- unique(holidays$Holiday)
  Abb <- c("NYD","MLKB","WaB", "MeD", "InD", "LaD", "CoD", "VeD", "ThD", "ChD", "NYD","MLKB","WaB")
  holidays$Abb <- holidays$Holiday
  for (i in 1:length(words)) {
    holidays$Abb=str_replace(holidays$Abb,words[i],Abb[i])
  }
  
  crimes <- data %>%
    group_by(ArrestDate) %>%
    summarise(N=n())

    
    
    
    output$plot1 = renderPlot({
      
     p =  data %>%
        filter(as.numeric(format(ArrestDate,'%Y')) == input$year) %>%
        ggplot(aes(Age, color = Sex)) + 
        geom_density(size = 1.5) + 
        xlim(14, 70) +
        ylim(0, .05) +
        annotate("text", label = input$year, x = 60, y= 0.025, alpha = .2, size = 16) +
        labs(
          subtitle = "Distribution of crimes reported over four main gender",
          x = "Age",
          y = "Density") +
        scale_color_discrete(name = "Gender", label = c("Female", "Male")) +
        theme_classic() +
        theme(axis.text.y=element_blank(), axis.ticks.y = element_blank(),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_blank())
     
     return(p)
        
    })
    
    output$plot2 = renderPlotly({
      
      data_hol <- left_join(crimes, holidays, by = "ArrestDate")
      View(data_hol)
      
      #geom_line & geom_smooth
      f = ggplot(data = data_hol, mapping = aes(x = ArrestDate, y = N)) +
        geom_line() +
        geom_smooth() +
        labs(
          title = "Arrests in Baltimore",
          x = "Date",
          y = "Number of Arrests"
        ) 
      
      
      #plotly
      if(input$holiday==TRUE){
        data_subset = data_hol %>% filter(!is.na(Holiday))
        f=f+geom_point(data = data_subset, color="purple")+
        geom_text(data = data_subset, aes(x=ArrestDate, y=N, label=Abb))
     }
        return(f)
    })
    
    output$myMap = renderLeaflet({
      
      loc_data= data %>%
        group_by(lng=round(Longitude,3),lat=round(Latitude,3)) %>%
        summarise(N=n()) %>%
        mutate(latL = lat-.0005, latH = lat+.0005, lngL = lng-.0005, lngH = lng+.0005)
      
      m=loc_data %>% 
        leaflet() %>% 
        addTiles() %>%
        addRectangles(
          lng1=~lngL, lat1=~latL,
          lng2=~lngH, lat2=~latH,
          fillOpacity = ~N/150, opacity = 0, fillColor = "red", label = ~N
        )%>%
        setView(-76.6,39.31, zoom=12) %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addLayersControl(baseGroups = c("Toner", "OSM"),
                         options = layersControlOptions(collapsed = FALSE))
        
    })
    
    output$myTable = renderDataTable({
      datatable(data, 
                options = list(scrollX = TRUE))
      return(datatable(data, rownames= FALSE))
      })
    

}

shinyApp(ui = ui, server = server)
