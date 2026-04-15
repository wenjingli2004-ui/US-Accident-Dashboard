# US-Accident-Dashboard
This dashboard was built to explore and analyze accident records across the United States from 2016 to 2023, covering 46 fields including accident severity, location, time, weather conditions, road features, and more. Due to hardware limitations, the dashboard loads the first 500,000 rows by default. This can be changed by modifying the `n_max` parameter on line 11 of `app.R`.

All charts are interactive and fully responsive to the global filter panel, allowing users to explore patterns across different severity levels, time periods, and lighting conditions.

# Modules
- Overview: A summary including total accident count, serious/severe accident rate, night accident rate, average impact distance including and duration, rain/snow weather rate, and the proportion of accidents occurring at junctions and traffic signals. Also includes a top 10 cities bar chart, severity pie chart and monthly trend line.
- Map: A map displaying up to 1000 accident locations with color coded severity markers. Click on any marker to view details including location, time, weather condition, temperature, visibility and accident description.
- Time Analysis: Heatmap showing accident frequency by hour and weekday, weekday distribution bar chart and stacked bar charts for Civil Twilight and Astronomical Twillght conditions broken down by severity.
- Severity: Box and violin plots exploring the relationship between accident severity and visibility, atmospheric presure, wind chill and impact distance.
- Road Features: Analysis of 13 road features, covering their frequency of occurrence and distribution by severity
- Weather: Top 10 weather conditions by accident count, temperature and humidity distributions by severity, wind speed vs precipitation scatter plot, wind direction bar chart and average precipitation by weather condition.
- Duration: Violin plots of accident duration by severity, duration vs impact distance scatter plot and average duration by hour of day.
- Region: Top 20 states by accident count, average severity by state, timezone distribution pie chart and average accident duration by state.
- Data Table: Filterable and searchable table displaying key fields including severity, weather condition, temperature, humidity, wind speed, visibility, twilight condition, distance, duration and road features.

# Filters
A global filter panel on the sidebar applies to all modules simultaneously:
- Severity: Filter by accident severity level: All, Minor, Moderate, Serious, Severe
- Day/Night: Filter by time of day based on Sunrise and Sunset data: All, Day, Night
- Civil Twilight: Filter by Civil Twilight condition: All, Day, Night
- Year Range: Slider to select a specific range of years

# Packages Used
-shiny, shinydashboard, tidyverse, leaflet, plotly, DT, lubridate, scales, shinycssloaders

# Data Source
UA Accidents (March 2023)
