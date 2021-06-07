#installing packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(viridis)) install.packages("viridis", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(ozmaps)) install.packages("ozmaps", repos = "http://cran.us.r-project.org")
if(!require(leaflet.minicharts)) install.packages("leaflet.minicharts", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(leaflet.extras)) install.packages("leaflet.extras", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(tm)) install.packages("tm", repos = "http://cran.us.r-project.org")
if(!require(htmltools)) install.packages("htmltools", repos = "http://cran.us.r-project.org")
#devtools::install_github("lchiffon/wordcloud2")



#loading libraries
library(tidyverse)
library(shiny)
library(shinydashboard)
library(here)
library(shinythemes)
library(leaflet)
library(viridis)
library(plotly)
library(tm)
library(wordcloud2)
library(ozmaps)
library(leaflet.minicharts)
library(ggthemes)
library(leaflet.extras)
library(sf)
library("htmltools")

#data wrangling

# casualty <- read.csv(here::here("data","Casualties.csv"))
# crash <- read.csv(here::here("data","Crash.csv"))
# date <- read.csv(here::here("data","DateTime.csv"))
# descp <- read.csv(here::here("data","Description.csv"))
# loc <- read.csv(here::here("data","Location.csv"))
# veh <- read.csv(here::here("data","Vehicles.csv"))
# crash <- read.csv(here::here("data","Crash.csv"))


# casualty <- read.csv(here::here("data","Casualties.csv")) %>% 
#   select(-X) %>% 
#   na.omit()
# date <- read.csv(here::here("data","DateTime.csv")) %>% 
#   select(-c(X, day_of_month))
# descp <- read.csv(here::here("data","Description.csv"))%>% 
#   select(-X) 
# loc <- read.csv(here::here("data","Location.csv")) %>% filter(country == "AU") %>% 
#   na.omit() %>%
#   select(-X)
# veh <- read.csv(here::here("data","Vehicles.csv"))
# accidents <- crash %>%  
#   left_join(casualty, by = "casualties_id") %>%
#   left_join(date, by = "date_time_id") %>% 
#   left_join(descp,by = "description_id") %>% 
#   left_join(loc, by = "lat_long") %>% 
#   left_join(veh, by = "vehicles_id") %>% 
#   select(-c(X.x,X.y))
# write.csv(here::here("data","accidents.csv"))

#reading data

Accident <- read.csv(here::here("data","Accident.csv")) %>% select(-X)
sa_data <- Accident %>% filter(state == "SA") 
vic_data <- Accident %>% filter(state == "VIC")
qld_data <- Accident %>% filter(state == "QLD")
mapplot <- read.csv(here::here("data","mapaccidents.csv"))
vehicles_sum <- read.csv(here::here("data","vehicle-sum.csv"))

#data wrangling for page 1
typesplot <- Accident %>% 
  select(state,casualties,fatalities,serious_injuries,minor_injuries) %>% 
  group_by(state) %>% 
  summarise(casualties = sum(casualties),fatalities = sum(fatalities),serious_injuries = sum(serious_injuries),minor_injuries = sum(minor_injuries)) %>% 
  pivot_longer(cols = c(casualties,fatalities,serious_injuries,minor_injuries),names_to = "type",values_to = "values")

mapplot <- mapplot %>% mutate(state_name = c("Queensland","South Australia","Victoria"))

# data wrangling for the leaflet map page 1
oz_accidents <- typesplot %>% mutate(NAME = case_when(state == "QLD" ~ "Queensland",
                                                         state == "VIC" ~ "Victoria",
                                                         state == "SA" ~ "South Australia")) 
oz_accidents <- oz_accidents %>% mutate(longitude = case_when(state == "QLD" ~ 142.702789,
                                                              state == "VIC" ~ 144.964600,
                                                              state == "SA" ~ 136.209152),
                                        latitude = case_when(state == "QLD" ~ -20.917574,
                                                             state == "VIC" ~ 	-37.020100,
                                                             state == "SA" ~ 	-30.000233))
dataset <- oz_accidents %>% pivot_wider(names_from = "type", values_from = "values")

# data wrangling for line plots in page 1 and page 2
allplot <- Accident %>% select(casualties_id,casualties,fatalities,serious_injuries,minor_injuries,month,state) %>%
  group_by(month,state) %>%
  summarise(casualties = sum(casualties),fatalities = sum(fatalities),serious_injury = sum(serious_injuries),minor_injury = sum(minor_injuries)) 

# data wrangling for vehicles choropleth map
states_sf <- st_transform(ozmap_states, 3395)
veh_sum <- vehicles_sum %>% mutate(state_name = case_when(state == "QLD" ~ "Queensland",
                                                          state == "VIC" ~ "Victoria",
                                                          state == "SA" ~ "South Australia")) %>% pivot_longer(cols = animals:scooter, names_to = "veh_type", values_to = "times")

states_sf <- states_sf %>% left_join(veh_sum, by = c("NAME" = "state_name")) %>% drop_na(times)

# data wrangling for factors page, page 5
speed <- Accident %>% 
  select(casualties,state,speed_limit) %>%  
  group_by(state,speed_limit) %>% 
  summarise(total = sum(casualties)) 

road_cond <- Accident %>% select(state,casualties,road_sealed,road_wet) %>% group_by(state,road_sealed,road_wet) %>% 
  summarise(tot = sum(casualties)) %>% pivot_longer(cols = c("road_sealed","road_wet"),names_to = "road_type", values_to = "binary")


weather <- Accident %>% select(state,casualties,weather) %>% group_by(state,weather) %>% summarise(tot = sum(casualties))

traff <- Accident %>% select(state,casualties,traffic_controls) %>% group_by(state,traffic_controls) %>% 
  summarise(tot = sum(casualties)) 

# data wrangling for word cloud
text <- c(Accident$crash_type,Accident$severity,Accident$lighting,Accident$weather,Accident$traffic_controls)
docs <- Corpus(VectorSource(text))
docs <- docs %>% tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix),decreasing=TRUE)
df <- data.frame(word = names(words),freq=words)


# adding extensive html tools onto the application
addDeps <- function(x) {
  if (getOption("shiny.minified", TRUE)) {
    adminLTE_js <- "app.min.js"
    adminLTE_css <- c("AdminLTE.min.css", "_all-skins.min.css")
  } else {
    adminLTE_js <- "app.js"
    adminLTE_css <- c("AdminLTE.css", "_all-skins.css")
  }
  
  dashboardDeps <- list(
    htmlDependency("AdminLTE", "2.0.6",
                   c(file = system.file("AdminLTE", package = "shinydashboard")),
                   script = adminLTE_js,
                   stylesheet = adminLTE_css
    ),
    htmlDependency("shinydashboard",
                   as.character(utils::packageVersion("shinydashboard")),
                   c(file = system.file(package = "shinydashboard")),
                   script = "shinydashboard.js",
                   stylesheet = "shinydashboard.css"
    )
  )
  
  shinydashboard:::appendDependencies(x, dashboardDeps)
}


# start of the user interface code
ui <- addDeps(fluidPage(
  fluidRow(
    navbarPage(theme = shinytheme("cyborg"),collapsible = TRUE,
               "Road Accidents in Australia",id = "nav",tabPanel("Introduction",
                                                                 sidebarPanel(
                                                                   tags$img(src = 'drive.gif',width = 500,height = 800,width = 4)),
                                                                   mainPanel(
                                                                     fluidRow(
                                                                       column(12,
                                                                              tags$div(
                                                                                tags$h2("Road accident is a global tragedy with the ever-rising trend"),
                                                                                tags$h4(tags$strong("Road Accidents"),"kill thousands of people every week in the world. 
                                                                                       Road accidents are pretty commonly occurring mishaps throughout the world and it is no stranger in Australia as well.
                                                                                       As the population is increasing, the demand for automobiles is getting higher as well and in that the need for every household to have an automobile for each person is increasing as well. 
                                                                                       With this more than ever increase in the number of incidents throughout.", tags$br(),tags$br(),
                                                                                        tags$p("There are many factors which cause accidents everyday and you may think that usage of illegal drugs/drinking would cause the major number of accidents but that indeed is not the case,
                                                                                              where it is believed that ~90% of crashes occur due to minor mistakes like distraction, fatigue or even road rage"),
                                                                                        tags$br(),tags$br(),
                                                                                        tags$p("All countries throughout the world keep a record of all thr road accidents that occured in each of it's states and the overall count for the country at the end of the year,
                                                                                              to reflect on that year statistics and bring up more rules and regulations or plan on how to avoid these incidents from happening/ reducing the count of accidents in the following year.")
                                                                                )
                                                                              ),
                                                                              fluidRow(
                                                                                column(12,style='padding:2px;',
                                                                              infoBoxOutput("SA"),
                                                                              infoBoxOutput("VIC"),
                                                                              infoBoxOutput("QLD"),
                                                                              includeMarkdown("how_to.md")))
                                                                     )
                                                                   )
                                                                 )), 
               tabPanel("State Comparision",
                          sidebarPanel(
                            selectInput("states","Choose a state name:",choices = allplot$state,selected = "QLD"),
                            plotlyOutput("allplot", width = "100%", height = "100%"),
                            includeMarkdown("state_comp.md"),width = 4
                          ),
                          
                          mainPanel(
                              leafletOutput("leaflet",height = "830")
                        )),
               tabPanel("Time Visualization",
                        tabsetPanel(
                          tabPanel("South Australia",
                                   fluidRow(
                                   column(6,tags$div(tags$h5("Yearly counts for different types of injuries")),
                                                     plotlyOutput("yearsa")),
                                   column(6,tags$div(tags$h5("Description")),includeMarkdown("SA_sum.md"))),
                                   fluidRow(
                                   column(6,tags$div(tags$h5("Day of week counts for different types of injuries")),
                                                     plotlyOutput("daysa")),
                                   column(6,tags$div(tags$h5("Hourly counts for different types of injuries")),
                                                     plotlyOutput("hoursa")))),
                          tabPanel("Queensland",
                                   fluidRow(
                                   column(6,tags$div(tags$h5("Yearly counts for different types of injuries")),
                                                     plotlyOutput("yearqld")),
                                   column(6,tags$div(tags$h5("Description")),includeMarkdown("QLD_sum.md"))),
                                   fluidRow(
                                   column(6,tags$div(tags$h5("Day of week counts for different types of injuries")),
                                                     plotlyOutput("dayqld")),
                                   column(6,tags$div(tags$h5("Hourly counts for different types of injuries")),
                                                     plotlyOutput("hourqld")))),
                          tabPanel("Victoria",
                                   fluidRow(
                                   column(6,tags$div(tags$h5("Yearly counts for different types of injuries")),
                                                     plotlyOutput("yearvic")),
                                   column(6,"Description",includeMarkdown("VIC_sum.md"))),
                                   fluidRow(
                                   column(6,tags$div(tags$h5("Day of week counts for different types of injuries")),
                                                     plotlyOutput("dayvic")),
                                   column(6,tags$div(tags$h5("Hourly counts for different types of injuries")),
                                                     plotlyOutput("hourvic")))
                          )
                        )),
               tabPanel("Vehicles that are involved in the incidents",
                        sidebarPanel(
                          radioButtons("veh", "Choose vehicle type:", choices = unique(states_sf$veh_type), selected = "cars"),
                          includeMarkdown("veh.md"),width = 3
                        ),
                        mainPanel(
                       plotlyOutput("vehicles", height = "800"))),
               tabPanel("Factors causing accidents",
                        sidebarPanel(
                          radioButtons("s","Choose a state:", choices = unique(speed$state), selected = "SA"),
                        includeMarkdown("instr.md"),width = 2),
                        mainPanel(
                            column(6,
                                   tabsetPanel(
                                     tabPanel(
                                   "Speed Limits",
                                   plotlyOutput("speed")),
                                   tabPanel("Summary",includeMarkdown("speed.md")))),
                            column(6,
                                   tabsetPanel(
                                     tabPanel(
                                   "Road as a factor",
                                   plotlyOutput("con")),
                            tabPanel("Summary",includeMarkdown("roads.md")))),
                            column(6,
                                   tabsetPanel(
                                     tabPanel(
                                   "Weather as a factor",
                                   plotlyOutput("box")),
                                   tabPanel("Summary",includeMarkdown("weather.md")))),
                            column(6,
                                   tabsetPanel(
                                     tabPanel(
                                   "Traffic controls a factor?",
                                   plotlyOutput("control")),
                                   tabPanel("Summary",includeMarkdown("traff.md"))))
                        ),width = 10),
               tabPanel("Wordcloud",
                        sidebarPanel("Summary",includeMarkdown("word.md"), width = 3),
                        mainPanel(
                        wordcloud2Output("word",height = 700)
                        )),
               tabPanel("References",
                        includeMarkdown("references.md"))
               ))
                       
    )
               )



# start of server side of the application
server <- function(input,output,session){

  total_sa <- sa_data %>% select(casualties)%>%  summarise(total = sum(casualties)) 
  total_qld <- qld_data %>% select(casualties)%>%  summarise(total = sum(casualties)) 
  total_vic <- vic_data %>% select(casualties)%>%  summarise(total = sum(casualties)) 
  
  output$SA <- renderInfoBox({
     infoBox(
      "Casualties in South Australia", total_sa$total,color = "purple", fill = TRUE
    )
  })
  
  output$VIC <- renderInfoBox({
    infoBox(
      "Casualties in Victoria", total_vic$total, color = "yellow",fill = TRUE
    )
  })
  
  output$QLD <- renderInfoBox({
    infoBox(
      "Casualties in Queensland", total_qld$total, color = "red",fill=TRUE
    )
  })
  

  
  colors <- c("#e53078", "#0d3d1f", "#a88725", "#7a1c15")
  
  output$leaflet <- renderLeaflet({
    leaflet(dataset) %>% 
      addTiles() %>% 
      addProviderTiles("Esri.WorldImagery") %>%
      addFullscreenControl() %>% 
      leaflet.minicharts::addMinicharts(
        dataset$longitude, dataset$latitude,
        type = "bar",
        chartdata = dataset[,c("casualties","serious_injuries","fatalities","minor_injuries")],
        colorPalette = colors,
        width = 70, height = 70,
        layerId = dataset$state
      ) 
      
  })
  
    output$allplot <- renderPlotly({
     
      if (nchar(input$states) == 0) {
       selected <- "QLD"
     }
     else {
       selected <- input$states
     }
      
      p1 <- allplot %>% 
        filter(state == selected) %>%  
        ggplot(aes(x = month)) +
        geom_line(aes(y = casualties,color = "casualties"))+
        geom_line(aes(y = minor_injury,color = "minor_injury"))+
        geom_line(aes(y = serious_injury,color = "serious_injury"))+
        geom_line(aes(y = fatalities,color = "fatalities"))+
        geom_point(aes(y = casualties, color = "casualties"))+
        geom_point(aes(y = minor_injury,color = "minor_injury"))+
        geom_point(aes(y = serious_injury,color = "serious_injury"))+
        geom_point(aes(y = fatalities,color = "fatalities"))+
        theme(legend.position = "right")+
        labs(x = "Month", y= "Injury type",color = "Types")+
        theme_solarized()+
        ggtitle(paste("State:", selected))
        ggplotly(p1)  
      
       })

  
  
  #south australia
  output$yearsa <- renderPlotly({
    sa_plot1 <- sa_data %>% select(casualties_id,casualties,fatalities,serious_injuries,minor_injuries,year) %>%
      group_by(year) %>%
      summarise(casualties = sum(casualties),fatalities = sum(fatalities),serious_injury = sum(serious_injuries),minor_injury = sum(minor_injuries)) %>%
      ggplot(aes(x = year)) +
      geom_line(aes(y = casualties,color = "casualties"))+
      geom_line(aes(y = fatalities,color = "fatalities")) +
      geom_line(aes(y = serious_injury,color = "serious_injury")) +
      geom_line(aes(y = minor_injury,color = "minor_injury")) +
      geom_point(aes(y = casualties, color = "casualties"))+
      geom_point(aes(y = minor_injury,color = "minor_injury"))+
      geom_point(aes(y = serious_injury,color = "serious_injury"))+
      geom_point(aes(y = fatalities,color = "fatalities"))+
      labs(color = "Types",y = "Accidents")+
      theme_solarized()
    ggplotly(sa_plot1, dynamicTicks = TRUE) %>%
      rangeslider() %>%
      layout(hovermode = "x")
  })

  output$daysa <- renderPlotly({
    sa_plot3 <- sa_data %>% select(casualties_id,casualties,fatalities,serious_injuries,minor_injuries,day_of_week) %>%
      group_by(day_of_week) %>%
      summarise(casualties = sum(casualties),fatalities = sum(fatalities),serious_injury = sum(serious_injuries),minor_injury = sum(minor_injuries)) %>%
      ggplot(aes(x = day_of_week)) +
      geom_line(aes(y = casualties,color = "casualties"))+
      geom_line(aes(y = fatalities,color = "fatalities")) +
      geom_line(aes(y = serious_injury,color = "serious_injury")) +
      geom_line(aes(y = minor_injury,color = "minor_injury")) +
      geom_point(aes(y = casualties, color = "casualties"))+
      geom_point(aes(y = minor_injury,color = "minor_injury"))+
      geom_point(aes(y = serious_injury,color = "serious_injury"))+
      geom_point(aes(y = fatalities,color = "fatalities"))+
      labs(color = "Types",y = "Accidents")+
      theme_solarized()
    ggplotly(sa_plot3, dynamicTicks = TRUE) %>%
      rangeslider() %>%
      layout(hovermode = "x")
  })

  output$hoursa <- renderPlotly({
    sa_plot4 <- sa_data %>% select(casualties_id,casualties,fatalities,serious_injuries,minor_injuries,hour) %>%
      group_by(hour) %>%
      summarise(casualties = sum(casualties),fatalities = sum(fatalities),serious_injury = sum(serious_injuries),minor_injury = sum(minor_injuries)) %>%
      ggplot(aes(x = hour)) +
      geom_line(aes(y = casualties,color = "casualties"))+
      geom_line(aes(y = fatalities,color = "fatalities")) +
      geom_line(aes(y = serious_injury,color = "serious_injury")) +
      geom_line(aes(y = minor_injury,color = "minor_injury")) +
      geom_point(aes(y = casualties, color = "casualties"))+
      geom_point(aes(y = minor_injury,color = "minor_injury"))+
      geom_point(aes(y = serious_injury,color = "serious_injury"))+
      geom_point(aes(y = fatalities,color = "fatalities"))+
      labs(color = "Types",y = "Accidents")+
      theme_solarized()
    ggplotly(sa_plot4, dynamicTicks = TRUE) %>%
      rangeslider() %>%
      layout(hovermode = "x")
  })
  #queensland
  output$yearqld <- renderPlotly({
    qld_year <- qld_data %>% select(casualties_id,casualties,fatalities,serious_injuries,minor_injuries,year) %>%
      group_by(year) %>%
      summarise(casualties = sum(casualties),fatalities = sum(fatalities),serious_injury = sum(serious_injuries),minor_injury = sum(minor_injuries)) %>%
      ggplot(aes(x = year)) +
      geom_line(aes(y = casualties,color = "casualties"))+
      geom_line(aes(y = fatalities,color = "fatalities")) +
      geom_line(aes(y = serious_injury,color = "serious_injury")) +
      geom_line(aes(y = minor_injury,color = "minor_injury")) +
      geom_point(aes(y = casualties, color = "casualties"))+
      geom_point(aes(y = minor_injury,color = "minor_injury"))+
      geom_point(aes(y = serious_injury,color = "serious_injury"))+
      geom_point(aes(y = fatalities,color = "fatalities"))+
      labs(color = "Types",y = "Accidents")+
      theme_solarized()
    ggplotly(qld_year, dynamicTicks = TRUE) %>%
      rangeslider() %>%
      layout(hovermode = "x")
  })



  output$dayqld <- renderPlotly({
    qld_day <- qld_data %>% select(casualties_id,casualties,fatalities,serious_injuries,minor_injuries,day_of_week) %>%
      group_by(day_of_week) %>%
      summarise(casualties = sum(casualties),fatalities = sum(fatalities),serious_injury = sum(serious_injuries),minor_injury = sum(minor_injuries)) %>%
      ggplot(aes(x = day_of_week)) +
      geom_line(aes(y = casualties,color = "casualties"))+
      geom_line(aes(y = fatalities,color = "fatalities")) +
      geom_line(aes(y = serious_injury,color = "serious_injury")) +
      geom_line(aes(y = minor_injury,color = "minor_injury")) +
      geom_point(aes(y = casualties, color = "casualties"))+
      geom_point(aes(y = minor_injury,color = "minor_injury"))+
      geom_point(aes(y = serious_injury,color = "serious_injury"))+
      geom_point(aes(y = fatalities,color = "fatalities"))+
      labs(color = "Types",y = "Accidents")+
      theme_solarized()
    ggplotly(qld_day, dynamicTicks = TRUE) %>%
      rangeslider() %>%
      layout(hovermode = "x")
  })

  output$hourqld <- renderPlotly({
    qld_hour <- qld_data %>% select(casualties_id,casualties,fatalities,serious_injuries,minor_injuries,hour) %>%
      group_by(hour) %>%
      summarise(casualties = sum(casualties),fatalities = sum(fatalities),serious_injury = sum(serious_injuries),minor_injury = sum(minor_injuries)) %>%
      ggplot(aes(x = hour)) +
      geom_line(aes(y = casualties,color = "casualties"))+
      geom_line(aes(y = fatalities,color = "fatalities")) +
      geom_line(aes(y = serious_injury,color = "serious_injury")) +
      geom_line(aes(y = minor_injury,color = "minor_injury")) +
      geom_point(aes(y = casualties, color = "casualties"))+
      geom_point(aes(y = minor_injury,color = "minor_injury"))+
      geom_point(aes(y = serious_injury,color = "serious_injury"))+
      geom_point(aes(y = fatalities,color = "fatalities"))+
      labs(color = "Types",y = "Accidents")+
      theme_solarized()
    ggplotly(qld_hour, dynamicTicks = TRUE) %>%
      rangeslider() %>%
      layout(hovermode = "x")
  })

  #victoria
  output$yearvic <- renderPlotly({
    vic_year <- vic_data %>% select(casualties_id,casualties,fatalities,serious_injuries,minor_injuries,year) %>%
      group_by(year) %>%
      summarise(casualties = sum(casualties),fatalities = sum(fatalities),serious_injury = sum(serious_injuries),minor_injury = sum(minor_injuries)) %>%
      ggplot(aes(x = year)) +
      geom_line(aes(y = casualties,color = "casualties"))+
      geom_line(aes(y = fatalities,color = "fatalities")) +
      geom_line(aes(y = serious_injury,color = "serious_injury")) +
      geom_line(aes(y = minor_injury,color = "minor_injury")) +
      geom_point(aes(y = casualties, color = "casualties"))+
      geom_point(aes(y = minor_injury,color = "minor_injury"))+
      geom_point(aes(y = serious_injury,color = "serious_injury"))+
      geom_point(aes(y = fatalities,color = "fatalities"))+
      labs(color = "Types",y = "Accidents")+
      theme_solarized()
    ggplotly(vic_year, dynamicTicks = TRUE) %>%
      rangeslider() %>%
      layout(hovermode = "x")
  })

  

  output$dayvic <- renderPlotly({
    vic_day <- vic_data %>% select(casualties_id,casualties,fatalities,serious_injuries,minor_injuries,day_of_week) %>%
      group_by(day_of_week) %>%
      summarise(casualties = sum(casualties),fatalities = sum(fatalities),serious_injury = sum(serious_injuries),minor_injury = sum(minor_injuries)) %>%
      ggplot(aes(x = day_of_week)) +
      geom_line(aes(y = casualties,color = "casualties"))+
      geom_line(aes(y = fatalities,color = "fatalities")) +
      geom_line(aes(y = serious_injury,color = "serious_injury")) +
      geom_line(aes(y = minor_injury,color = "minor_injury")) +
      geom_point(aes(y = casualties, color = "casualties"))+
      geom_point(aes(y = minor_injury,color = "minor_injury"))+
      geom_point(aes(y = serious_injury,color = "serious_injury"))+
      geom_point(aes(y = fatalities,color = "fatalities"))+
      labs(color = "Types",y = "Accidents")+
      theme_solarized()
    ggplotly(vic_day, dynamicTicks = TRUE) %>%
      rangeslider() %>%
      layout(hovermode = "x")
  })

  output$hourvic <- renderPlotly({
    vic_hour <- vic_data %>% select(casualties_id,casualties,fatalities,serious_injuries,minor_injuries,hour) %>%
      group_by(hour) %>%
      summarise(casualties = sum(casualties),fatalities = sum(fatalities),serious_injury = sum(serious_injuries),minor_injury = sum(minor_injuries)) %>%
      ggplot(aes(x = hour)) +
      geom_line(aes(y = casualties,color = "casualties"))+
      geom_line(aes(y = fatalities,color = "fatalities")) +
      geom_line(aes(y = serious_injury,color = "serious_injury")) +
      geom_line(aes(y = minor_injury,color = "minor_injury")) +
      geom_point(aes(y = casualties, color = "casualties"))+
      geom_point(aes(y = minor_injury,color = "minor_injury"))+
      geom_point(aes(y = serious_injury,color = "serious_injury"))+
      geom_point(aes(y = fatalities,color = "fatalities"))+
      labs(color = "Types",y = "Accidents")+
      theme_solarized()
      ggplotly(vic_hour, dynamicTicks = TRUE) %>%
      rangeslider() %>%
      layout(hovermode = "x")
  })
  
  output$vehicles <- renderPlotly({
      
      if (nchar(input$veh) == 0) {
        veh <- "cars"
      }
      else {
        veh <- input$veh
      }
     
    veh <- ggplot(states_sf %>% filter(veh_type == input$veh))+
      geom_sf(aes(geometry = geometry,fill = times, text = paste("State:",state, "<br>", "Vehicle count:", times)),color = "white") +
      #geom_sf_(label = unique(states_sf$state))+
      scale_fill_viridis_c(breaks = scales::pretty_breaks(n=8))+
      coord_sf()+
      theme_solarized()+
      labs(fill = "Accidents")+
      theme(legend.position = "right") + 
      ggtitle(label = paste("Accidents caused by", input$veh))
    ggplotly(veh, tooltip = c("text"))
      
  })
  
  output$speed <- renderPlotly({
    speed %>% filter(state == input$s) %>% 
    ggplot(aes(x = reorder(speed_limit, total), 
               y = total,
               fill = speed_limit,
               color = speed_limit))+
      geom_col() +
      scale_x_discrete(breaks = waiver())+
      coord_flip()+
      theme_solarized()+
      theme(legend.position = "none",axis.text.y = element_text(angle = 45))+
      labs(x = "Speed ranges km/h", 
           y = "Number of Accidents",
           title = "Speed as a factor",
           subtitle = paste("State:" , input$s))
  })
  
  output$con <- renderPlotly({
    road_cond %>% filter(state == input$s) %>%  
      ggplot(aes(x = road_type))+
      geom_col(aes(y = tot,fill = road_type))+
      labs(x = "Road conditions", y = "Accidents", title = "Roads as a factor", subtitle = paste("State:" ,input$s))+
      theme_solarized()+
      theme(legend.position = "right",axis.text.x = element_blank())+
      facet_wrap(~ binary)
  })
  
  output$box <- renderPlotly({
    weather %>% filter(state == input$s) %>% 
    ggplot(aes(x = weather, y = tot, fill = weather))+
      geom_boxplot( )+
      theme(legend.position = "none",axis.text.x = element_text(angle = 90))+
      geom_point()+
      labs(x = "Weather",y = "Accidents", title = "Weather as a factor", subtitle = paste("State:", input$s))+
      theme_solarized()
  })
  
  output$control <- renderPlotly({
    traff %>% filter(state == input$s) %>% 
    ggplot(aes(x = reorder(traffic_controls,-tot), y = tot,fill = traffic_controls)) +
      geom_col()+
      theme(legend.position = "right",axis.text.x = element_blank())+
      labs(x = "Traffic controls",y = "Accidents", title = "Traffic controls as a factor", subtitle = paste("State:", input$s))+
      theme_solarized()
  })
  
  output$word <- renderWordcloud2({

    wordcloud2(df,size = 1.7,minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)
  })
}
shinyApp(ui = ui,server = server)
