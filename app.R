library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(plotly)
library(DT)
library(lubridate)
library(scales)
library(shinycssloaders)

df <- read_csv("US_Accidents_March23.csv",
               n_max = 500000) %>%
  mutate(
    Start_Time   = parse_date_time(Start_Time, orders = c("ymd HMS", "ymd")),
    End_Time     = parse_date_time(End_Time,   orders = c("ymd HMS", "ymd")),
    Year         = year(Start_Time),
    Month        = month(Start_Time, label = TRUE),
    Hour         = hour(Start_Time),
    Weekday      = wday(Start_Time, label = TRUE, week_start = 1),
    Duration_min = as.numeric(difftime(End_Time, Start_Time, units = "mins")),
    Duration_min = ifelse(Duration_min < 0 | Duration_min > 1440, NA, Duration_min),
    Severity     = as.integer(Severity)
  )

severity_colors <- c("1" = "#2ECC71", "2" = "#F39C12",
                     "3" = "#E67E22", "4" = "#E74C3C")
severity_labels <- c("1" = "Minor", "2" = "Moderate",
                     "3" = "Serious", "4" = "Severe")

road_features <- c("Amenity","Bump","Crossing","Give_Way","Junction",
                   "No_Exit","Railway","Roundabout","Station","Stop",
                   "Traffic_Calming","Traffic_Signal","Turning_Loop")

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Traffic Accident Dashboard", titleWidth = 300),
  dashboardSidebar(
    width = 240,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview",        tabName = "overview",  icon = icon("tachometer-alt")),
      menuItem("Map",             tabName = "map",       icon = icon("map-marked-alt")),
      menuItem("Time Analysis",   tabName = "time",      icon = icon("clock")),
      menuItem("Severity",        tabName = "severity",  icon = icon("exclamation-triangle")),
      menuItem("Road Features",   tabName = "road",      icon = icon("road")),
      menuItem("Weather",         tabName = "weather",   icon = icon("cloud-rain")),
      menuItem("Duration",        tabName = "duration",  icon = icon("hourglass-half")),
      menuItem("Region",          tabName = "region",    icon = icon("map")),
      menuItem("Data Table",      tabName = "table",     icon = icon("table"))
    ),
    hr(),
    tags$div(
      style = "padding: 10px 15px;",
      tags$h5("Filters", style = "color:#ccc; margin-bottom:8px;"),
      selectInput("filter_severity", "Severity",
                  choices = c("All"="all","1-Minor"="1","2-Moderate"="2",
                              "3-Serious"="3","4-Severe"="4"),
                  selected = "all"),
      selectInput("filter_daynight", "Day/Night",
                  choices = c("All"="all","Day"="Day","Night"="Night"),
                  selected = "all"),
      selectInput("filter_twilight", "Civil Twilight",
                  choices = c("All"="all","Day"="Day","Night"="Night"),
                  selected = "all"),
      sliderInput("filter_year", "Year Range",
                  min = 2016, max = 2023,
                  value = c(2016, 2023), sep = "")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f4f6f9; }
      .box { border-radius: 8px; }
    "))),
    tabItems(
      
      # Overview
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("vbox_total",    width = 3),
                valueBoxOutput("vbox_sev3",     width = 3),
                valueBoxOutput("vbox_night",    width = 3),
                valueBoxOutput("vbox_avgdist",  width = 3)
              ),
              fluidRow(
                valueBoxOutput("vbox_avgdur",   width = 3),
                valueBoxOutput("vbox_rain",     width = 3),
                valueBoxOutput("vbox_junction", width = 3),
                valueBoxOutput("vbox_signal",   width = 3)
              ),
              fluidRow(
                box(title="Top 10 Cities by Accident Count", status="primary",
                    solidHeader=TRUE, width=6,
                    withSpinner(plotlyOutput("plot_city", height="320px"))),
                box(title="Severity Distribution", status="warning",
                    solidHeader=TRUE, width=6,
                    withSpinner(plotlyOutput("plot_sev_pie", height="320px")))
              ),
              fluidRow(
                box(title="Monthly Accident Trend", status="info",
                    solidHeader=TRUE, width=12,
                    withSpinner(plotlyOutput("plot_monthly", height="280px")))
              )
      ),
      
      # Map
      tabItem(tabName = "map",
              fluidRow(
                box(title="Accident Location Map", status="primary",
                    solidHeader=TRUE, width=12,
                    tags$p("Color: Green=Minor, Yellow=Moderate, Orange=Serious, Red=Severe. Click markers for details.",
                           style="color:#666;font-size:12px;"),
                    withSpinner(leafletOutput("map_accidents", height="560px")))
              )
      ),
      
      # Time Analysis
      tabItem(tabName = "time",
              fluidRow(
                box(title="Accident Heatmap (Hour x Weekday)", status="info",
                    solidHeader=TRUE, width=12,
                    withSpinner(plotlyOutput("plot_heatmap", height="360px")))
              ),
              fluidRow(
                box(title="Accidents by Weekday", status="primary",
                    solidHeader=TRUE, width=4,
                    withSpinner(plotlyOutput("plot_weekday", height="300px"))),
                box(title="Civil Twilight vs Severity", status="warning",
                    solidHeader=TRUE, width=4,
                    withSpinner(plotlyOutput("plot_twilight", height="300px"))),
                box(title="Astronomical Twilight vs Severity", status="info",
                    solidHeader=TRUE, width=4,
                    withSpinner(plotlyOutput("plot_astro", height="300px")))
              )
      ),
      
      # Severity
      tabItem(tabName = "severity",
              fluidRow(
                box(title="Visibility vs Severity", status="warning",
                    solidHeader=TRUE, width=6,
                    withSpinner(plotlyOutput("plot_visibility", height="340px"))),
                box(title="Pressure vs Severity", status="info",
                    solidHeader=TRUE, width=6,
                    withSpinner(plotlyOutput("plot_pressure", height="340px")))
              ),
              fluidRow(
                box(title="Accident Distance by Severity", status="primary",
                    solidHeader=TRUE, width=6,
                    withSpinner(plotlyOutput("plot_distance_box", height="300px"))),
                box(title="Wind Chill vs Severity", status="danger",
                    solidHeader=TRUE, width=6,
                    withSpinner(plotlyOutput("plot_windchill", height="300px")))
              )
      ),
      
      # Road Features
      tabItem(tabName = "road",
              fluidRow(
                box(title="Road Feature Presence Rate", status="danger",
                    solidHeader=TRUE, width=6,
                    withSpinner(plotlyOutput("plot_road_presence", height="400px"))),
                box(title="Road Features vs Severity", status="warning",
                    solidHeader=TRUE, width=6,
                    withSpinner(plotlyOutput("plot_road_severity", height="400px")))
              ),
              fluidRow(
                box(title="Road Feature Heatmap", status="primary",
                    solidHeader=TRUE, width=12,
                    withSpinner(plotlyOutput("plot_road_heatmap", height="320px")))
              )
      ),
      
      # Weather
      tabItem(tabName = "weather",
              fluidRow(
                box(title="Top 10 Weather Conditions", status="info",
                    solidHeader=TRUE, width=7,
                    withSpinner(plotlyOutput("plot_weather_bar", height="360px"))),
                box(title="Temperature Distribution by Severity", status="primary",
                    solidHeader=TRUE, width=5,
                    withSpinner(plotlyOutput("plot_temp_violin", height="360px")))
              ),
              fluidRow(
                box(title="Humidity Distribution by Severity", status="warning",
                    solidHeader=TRUE, width=6,
                    withSpinner(plotlyOutput("plot_humidity", height="300px"))),
                box(title="Wind Speed vs Precipitation", status="info",
                    solidHeader=TRUE, width=6,
                    withSpinner(plotlyOutput("plot_wind_precip", height="300px")))
              ),
              fluidRow(
                box(title="Wind Direction Distribution", status="primary",
                    solidHeader=TRUE, width=6,
                    withSpinner(plotlyOutput("plot_wind_dir", height="340px"))),
                box(title="Avg Precipitation by Weather Condition", status="danger",
                    solidHeader=TRUE, width=6,
                    withSpinner(plotlyOutput("plot_precip_weather", height="340px")))
              )
      ),
      
      # Duration
      tabItem(tabName = "duration",
              fluidRow(
                box(title="Accident Duration by Severity", status="primary",
                    solidHeader=TRUE, width=12,
                    withSpinner(plotlyOutput("plot_duration_violin", height="360px")))
              ),
              fluidRow(
                box(title="Duration vs Distance", status="warning",
                    solidHeader=TRUE, width=6,
                    withSpinner(plotlyOutput("plot_dur_dist", height="320px"))),
                box(title="Avg Duration by Hour", status="info",
                    solidHeader=TRUE, width=6,
                    withSpinner(plotlyOutput("plot_dur_hour", height="320px")))
              )
      ),
      
      # Region
      tabItem(tabName = "region",
              fluidRow(
                box(title="Accident Count by State", status="primary",
                    solidHeader=TRUE, width=6,
                    withSpinner(plotlyOutput("plot_state", height="380px"))),
                box(title="Avg Severity by State", status="danger",
                    solidHeader=TRUE, width=6,
                    withSpinner(plotlyOutput("plot_state_sev", height="380px")))
              ),
              fluidRow(
                box(title="Timezone Distribution", status="info",
                    solidHeader=TRUE, width=6,
                    withSpinner(plotlyOutput("plot_timezone", height="300px"))),
                box(title="Avg Duration by State", status="warning",
                    solidHeader=TRUE, width=6,
                    withSpinner(plotlyOutput("plot_state_dur", height="300px")))
              )
      ),
      
      # Data Table
      tabItem(tabName = "table",
              fluidRow(
                box(title="Raw Data Table", status="primary",
                    solidHeader=TRUE, width=12,
                    DTOutput("data_table"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  df_filtered <- reactive({
    d <- df
    if (input$filter_severity != "all")
      d <- d %>% filter(Severity == as.integer(input$filter_severity))
    if (input$filter_daynight != "all")
      d <- d %>% filter(Sunrise_Sunset == input$filter_daynight)
    if (input$filter_twilight != "all")
      d <- d %>% filter(Civil_Twilight == input$filter_twilight)
    d <- d %>% filter(Year >= input$filter_year[1], Year <= input$filter_year[2])
    d
  })
  
  # Value Boxes
  output$vbox_total <- renderValueBox({
    valueBox(format(nrow(df_filtered()), big.mark=","),
             "Total Accidents", icon=icon("car-crash"), color="blue")
  })
  output$vbox_sev3 <- renderValueBox({
    n3  <- df_filtered() %>% filter(Severity %in% c(3,4)) %>% nrow()
    pct <- round(n3 / max(nrow(df_filtered()), 1) * 100, 1)
    valueBox(paste0(pct, "%"), "Serious/Severe Accidents",
             icon=icon("exclamation-triangle"), color="red")
  })
  output$vbox_night <- renderValueBox({
    nn  <- df_filtered() %>% filter(Sunrise_Sunset=="Night") %>% nrow()
    pct <- round(nn / max(nrow(df_filtered()), 1) * 100, 1)
    valueBox(paste0(pct, "%"), "Night Accidents",
             icon=icon("moon"), color="navy")
  })
  output$vbox_avgdist <- renderValueBox({
    avg <- round(mean(df_filtered()$`Distance(mi)`, na.rm=TRUE), 2)
    valueBox(paste0(avg, " mi"), "Avg Impact Distance",
             icon=icon("road"), color="green")
  })
  output$vbox_avgdur <- renderValueBox({
    avg <- round(mean(df_filtered()$Duration_min, na.rm=TRUE), 1)
    valueBox(paste0(avg, " min"), "Avg Accident Duration",
             icon=icon("hourglass-half"), color="orange")
  })
  output$vbox_rain <- renderValueBox({
    nr  <- df_filtered() %>%
      filter(grepl("Rain|Snow|Drizzle", Weather_Condition, ignore.case=TRUE)) %>% nrow()
    pct <- round(nr / max(nrow(df_filtered()), 1) * 100, 1)
    valueBox(paste0(pct, "%"), "Rain/Snow Weather",
             icon=icon("cloud-rain"), color="light-blue")
  })
  output$vbox_junction <- renderValueBox({
    nj  <- df_filtered() %>% filter(Junction==TRUE) %>% nrow()
    pct <- round(nj / max(nrow(df_filtered()), 1) * 100, 1)
    valueBox(paste0(pct, "%"), "At Junction",
             icon=icon("arrows-alt"), color="yellow")
  })
  output$vbox_signal <- renderValueBox({
    ns  <- df_filtered() %>% filter(Traffic_Signal==TRUE) %>% nrow()
    pct <- round(ns / max(nrow(df_filtered()), 1) * 100, 1)
    valueBox(paste0(pct, "%"), "At Traffic Signal",
             icon=icon("traffic-light"), color="purple")
  })
  
  # City
  output$plot_city <- renderPlotly({
    d <- df_filtered() %>% count(City, sort=TRUE) %>% head(10)
    plot_ly(d, x=~reorder(City,n), y=~n, type="bar",
            marker=list(color="#3498DB")) %>%
      layout(xaxis=list(title="City"), yaxis=list(title="Accident Count"),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Severity Pie
  output$plot_sev_pie <- renderPlotly({
    d <- df_filtered() %>% count(Severity) %>%
      mutate(label=paste0("Severity ", Severity, " - ",
                          severity_labels[as.character(Severity)]))
    plot_ly(d, labels=~label, values=~n, type="pie",
            marker=list(colors=unname(severity_colors[as.character(d$Severity)])),
            textinfo="label+percent") %>%
      layout(paper_bgcolor="transparent", showlegend=FALSE)
  })
  
  # Monthly Trend
  output$plot_monthly <- renderPlotly({
    d <- df_filtered() %>% count(Year, Month)
    plot_ly(d, x=~Month, y=~n, color=~as.factor(Year),
            type="scatter", mode="lines+markers") %>%
      layout(xaxis=list(title="Month"), yaxis=list(title="Accident Count"),
             legend=list(title=list(text="Year")),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Map
  output$map_accidents <- renderLeaflet({
    d   <- df_filtered() %>% sample_n(min(1000, nrow(.)))
    pal <- colorFactor(palette=unname(severity_colors), levels=c("1","2","3","4"))
    leaflet(d) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng=~Start_Lng, lat=~Start_Lat,
        color=~pal(as.character(Severity)),
        radius=~Severity*2.5, opacity=0.8, fillOpacity=0.6,
        popup=~paste0(
          "<b>ID:</b> ", ID, "<br>",
          "<b>Severity:</b> ", Severity, " (", severity_labels[as.character(Severity)], ")<br>",
          "<b>Time:</b> ", format(Start_Time, "%Y-%m-%d %H:%M"), "<br>",
          "<b>Location:</b> ", Street, ", ", City, ", ", State, "<br>",
          "<b>Weather:</b> ", Weather_Condition, "<br>",
          "<b>Temperature:</b> ", `Temperature(F)`, " F<br>",
          "<b>Visibility:</b> ", `Visibility(mi)`, " mi<br>",
          "<b>Description:</b> ", substr(Description, 1, 100)
        )
      ) %>%
      addLegend(pal=pal, values=~as.character(Severity),
                title="Severity", position="bottomright")
  })
  
  # Heatmap
  output$plot_heatmap <- renderPlotly({
    d <- df_filtered() %>% count(Weekday, Hour) %>%
      complete(Weekday, Hour=0:23, fill=list(n=0))
    plot_ly(d, x=~Hour, y=~Weekday, z=~n, type="heatmap",
            colorscale=list(c(0,"#EBF5FB"),c(0.5,"#3498DB"),c(1,"#1A5276"))) %>%
      layout(xaxis=list(title="Hour", dtick=2), yaxis=list(title=""),
             paper_bgcolor="transparent")
  })
  
  # Weekday
  output$plot_weekday <- renderPlotly({
    d <- df_filtered() %>% count(Weekday)
    plot_ly(d, x=~Weekday, y=~n, type="bar",
            marker=list(color="#8E44AD")) %>%
      layout(xaxis=list(title=""), yaxis=list(title="Accident Count"),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Civil Twilight
  output$plot_twilight <- renderPlotly({
    d <- df_filtered() %>%
      count(Civil_Twilight, Severity) %>%
      mutate(Sev_label=paste0("Severity ", Severity))
    plot_ly(d, x=~Civil_Twilight, y=~n, color=~Sev_label,
            type="bar", colors=unname(severity_colors)) %>%
      layout(barmode="stack",
             xaxis=list(title="Civil Twilight"), yaxis=list(title="Accident Count"),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Astronomical Twilight
  output$plot_astro <- renderPlotly({
    d <- df_filtered() %>% count(Astronomical_Twilight, Severity) %>%
      mutate(Sev_label=paste0("Severity ", Severity))
    plot_ly(d, x=~Astronomical_Twilight, y=~n, color=~Sev_label,
            type="bar", colors=unname(severity_colors)) %>%
      layout(barmode="stack",
             xaxis=list(title="Astronomical Twilight"), yaxis=list(title="Accident Count"),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Visibility
  output$plot_visibility <- renderPlotly({
    plot_ly(df_filtered(), x=~as.factor(Severity), y=~`Visibility(mi)`,
            color=~as.factor(Severity), type="box",
            colors=unname(severity_colors), showlegend=FALSE) %>%
      layout(xaxis=list(title="Severity"), yaxis=list(title="Visibility (mi)"),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Pressure
  output$plot_pressure <- renderPlotly({
    plot_ly(df_filtered(), x=~as.factor(Severity), y=~`Pressure(in)`,
            color=~as.factor(Severity), type="box",
            colors=unname(severity_colors), showlegend=FALSE) %>%
      layout(xaxis=list(title="Severity"), yaxis=list(title="Pressure (in)"),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Distance
  output$plot_distance_box <- renderPlotly({
    plot_ly(df_filtered(), x=~as.factor(Severity), y=~`Distance(mi)`,
            color=~as.factor(Severity), type="violin",
            colors=unname(severity_colors), showlegend=FALSE,
            box=list(visible=TRUE), meanline=list(visible=TRUE)) %>%
      layout(xaxis=list(title="Severity"), yaxis=list(title="Impact Distance (mi)"),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Wind Chill
  output$plot_windchill <- renderPlotly({
    plot_ly(df_filtered(), x=~as.factor(Severity), y=~`Wind_Chill(F)`,
            color=~as.factor(Severity), type="box",
            colors=unname(severity_colors), showlegend=FALSE) %>%
      layout(xaxis=list(title="Severity"), yaxis=list(title="Wind Chill (F)"),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Road Feature Presence
  output$plot_road_presence <- renderPlotly({
    total <- nrow(df_filtered())
    d <- df_filtered() %>%
      select(all_of(road_features)) %>%
      summarise(across(everything(), ~sum(.x == TRUE, na.rm=TRUE))) %>%
      pivot_longer(everything(), names_to="Feature", values_to="Count") %>%
      mutate(Pct = round(Count / total * 100, 1)) %>%
      arrange(desc(Pct))
    plot_ly(d, x=~Pct, y=~reorder(Feature, Pct),
            type="bar", orientation="h",
            marker=list(color="#E74C3C")) %>%
      layout(xaxis=list(title="% of Total Accidents"), yaxis=list(title=""),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Road Feature vs Severity
  output$plot_road_severity <- renderPlotly({
    d <- df_filtered() %>%
      select(Severity, all_of(road_features)) %>%
      pivot_longer(-Severity, names_to="Feature", values_to="Present") %>%
      filter(Present==TRUE) %>%
      count(Feature, Severity)
    plot_ly(d, x=~Feature, y=~n, color=~as.factor(Severity),
            type="bar", colors=unname(severity_colors)) %>%
      layout(barmode="stack",
             xaxis=list(title="", tickangle=-45),
             yaxis=list(title="Accident Count"),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Road Feature Heatmap
  output$plot_road_heatmap <- renderPlotly({
    d <- df_filtered() %>%
      select(all_of(road_features)) %>%
      summarise(across(everything(), ~mean(.x == TRUE, na.rm=TRUE))) %>%
      pivot_longer(everything(), names_to="Feature", values_to="Rate") %>%
      mutate(x = 1)
    plot_ly(d, x=~Feature, y=~x, z=~Rate, type="heatmap",
            colorscale=list(c(0,"#EBF5FB"),c(1,"#E74C3C")),
            showscale=TRUE) %>%
      layout(xaxis=list(title="", tickangle=-45),
             yaxis=list(showticklabels=FALSE, title=""),
             paper_bgcolor="transparent")
  })
  
  # Weather Bar
  output$plot_weather_bar <- renderPlotly({
    d <- df_filtered() %>% count(Weather_Condition, sort=TRUE) %>% head(10)
    plot_ly(d, x=~n, y=~reorder(Weather_Condition,n),
            type="bar", orientation="h",
            marker=list(color="#1ABC9C")) %>%
      layout(xaxis=list(title="Accident Count"), yaxis=list(title=""),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Temperature
  output$plot_temp_violin <- renderPlotly({
    plot_ly(df_filtered(), x=~as.factor(Severity), y=~`Temperature(F)`,
            color=~as.factor(Severity), type="violin",
            colors=unname(severity_colors), showlegend=FALSE,
            box=list(visible=TRUE)) %>%
      layout(xaxis=list(title="Severity"), yaxis=list(title="Temperature (F)"),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Humidity
  output$plot_humidity <- renderPlotly({
    plot_ly(df_filtered(), x=~as.factor(Severity), y=~`Humidity(%)`,
            color=~as.factor(Severity), type="box",
            colors=unname(severity_colors), showlegend=FALSE) %>%
      layout(xaxis=list(title="Severity"), yaxis=list(title="Humidity (%)"),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Wind vs Precip
  output$plot_wind_precip <- renderPlotly({
    d <- df_filtered() %>% sample_n(min(500, nrow(.)))
    plot_ly(d, x=~`Wind_Speed(mph)`, y=~`Precipitation(in)`,
            color=~as.factor(Severity), type="scatter", mode="markers",
            colors=unname(severity_colors),
            marker=list(size=5, opacity=0.6)) %>%
      layout(xaxis=list(title="Wind Speed (mph)"), yaxis=list(title="Precipitation (in)"),
             legend=list(title=list(text="Severity")),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Wind Direction
  output$plot_wind_dir <- renderPlotly({
    d <- df_filtered() %>%
      filter(!is.na(Wind_Direction), Wind_Direction != "Calm",
             Wind_Direction != "Variable") %>%
      count(Wind_Direction, sort=TRUE) %>% head(12)
    plot_ly(d, x=~Wind_Direction, y=~n, type="bar",
            marker=list(color="#2980B9")) %>%
      layout(xaxis=list(title="Wind Direction"), yaxis=list(title="Accident Count"),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Precip by Weather
  output$plot_precip_weather <- renderPlotly({
    d <- df_filtered() %>%
      filter(!is.na(`Precipitation(in)`), `Precipitation(in)` > 0) %>%
      group_by(Weather_Condition) %>%
      summarise(avg_precip = mean(`Precipitation(in)`, na.rm=TRUE), n=n()) %>%
      filter(n >= 10) %>%
      arrange(desc(avg_precip)) %>% head(10)
    plot_ly(d, x=~avg_precip, y=~reorder(Weather_Condition, avg_precip),
            type="bar", orientation="h",
            marker=list(color="#3498DB")) %>%
      layout(xaxis=list(title="Avg Precipitation (in)"), yaxis=list(title=""),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Duration Violin
  output$plot_duration_violin <- renderPlotly({
    d <- df_filtered() %>% filter(!is.na(Duration_min), Duration_min <= 480)
    plot_ly(d, x=~as.factor(Severity), y=~Duration_min,
            color=~as.factor(Severity), type="violin",
            colors=unname(severity_colors), showlegend=FALSE,
            box=list(visible=TRUE), meanline=list(visible=TRUE)) %>%
      layout(xaxis=list(title="Severity"),
             yaxis=list(title="Duration (minutes)"),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Duration vs Distance
  output$plot_dur_dist <- renderPlotly({
    d <- df_filtered() %>%
      filter(!is.na(Duration_min), Duration_min <= 480) %>%
      sample_n(min(500, nrow(.)))
    plot_ly(d, x=~Duration_min, y=~`Distance(mi)`,
            color=~as.factor(Severity), type="scatter", mode="markers",
            colors=unname(severity_colors),
            marker=list(size=5, opacity=0.6)) %>%
      layout(xaxis=list(title="Duration (min)"),
             yaxis=list(title="Impact Distance (mi)"),
             legend=list(title=list(text="Severity")),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Duration by Hour
  output$plot_dur_hour <- renderPlotly({
    d <- df_filtered() %>%
      filter(!is.na(Duration_min), Duration_min <= 480) %>%
      group_by(Hour) %>%
      summarise(avg_dur = mean(Duration_min, na.rm=TRUE))
    plot_ly(d, x=~Hour, y=~avg_dur, type="scatter", mode="lines+markers",
            line=list(color="#E67E22"), marker=list(color="#E67E22")) %>%
      layout(xaxis=list(title="Hour", dtick=2),
             yaxis=list(title="Avg Duration (min)"),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # State Count
  output$plot_state <- renderPlotly({
    d <- df_filtered() %>% count(State, sort=TRUE) %>% head(20)
    plot_ly(d, x=~reorder(State,n), y=~n, type="bar",
            marker=list(color="#1A5276")) %>%
      layout(xaxis=list(title="State"), yaxis=list(title="Accident Count"),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # State Avg Severity
  output$plot_state_sev <- renderPlotly({
    d <- df_filtered() %>%
      group_by(State) %>%
      summarise(avg_sev = mean(Severity, na.rm=TRUE), n=n()) %>%
      filter(n >= 50) %>%
      arrange(desc(avg_sev)) %>% head(20)
    plot_ly(d, x=~reorder(State, avg_sev), y=~avg_sev, type="bar",
            marker=list(color="#C0392B")) %>%
      layout(xaxis=list(title="State"), yaxis=list(title="Avg Severity"),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Timezone
  output$plot_timezone <- renderPlotly({
    d <- df_filtered() %>% count(Timezone, sort=TRUE)
    plot_ly(d, labels=~Timezone, values=~n, type="pie",
            textinfo="label+percent") %>%
      layout(paper_bgcolor="transparent", showlegend=TRUE)
  })
  
  # State Duration
  output$plot_state_dur <- renderPlotly({
    d <- df_filtered() %>%
      filter(!is.na(Duration_min), Duration_min <= 480) %>%
      group_by(State) %>%
      summarise(avg_dur = mean(Duration_min, na.rm=TRUE), n=n()) %>%
      filter(n >= 50) %>%
      arrange(desc(avg_dur)) %>% head(20)
    plot_ly(d, x=~reorder(State, avg_dur), y=~avg_dur, type="bar",
            marker=list(color="#8E44AD")) %>%
      layout(xaxis=list(title="State"), yaxis=list(title="Avg Duration (min)"),
             paper_bgcolor="transparent", plot_bgcolor="transparent")
  })
  
  # Data Table
  output$data_table <- renderDT({
    df_filtered() %>%
      select(ID, Severity, Start_Time, City, State,
             Weather_Condition, `Temperature(F)`, `Humidity(%)`,
             `Wind_Speed(mph)`, `Visibility(mi)`,
             Sunrise_Sunset, Civil_Twilight, `Distance(mi)`,
             Duration_min, Junction, Traffic_Signal) %>%
      datatable(
        options=list(pageLength=15, scrollX=TRUE),
        rownames=FALSE, filter="top"
      ) %>%
      formatStyle("Severity",
                  backgroundColor=styleEqual(
                    c(1,2,3,4),
                    c("#D5F5E3","#FDEBD0","#FAD7A0","#FADBD8")))
  })
}

shinyApp(ui = ui, server = server)