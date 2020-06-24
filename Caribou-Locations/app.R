
# load packages
library(shiny)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(ggspatial)
library(lubridate)

# read in data
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

# isolate year from date
locations <- locations %>%
    mutate(year = year(timestamp))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Caribou Locations in British Columbia Over Time"),

    "The purpose of this interactive map is to track the location of caribou
    populations in British Columbia between 1988 and 2016. Over the past few
    years, the caribou population in this area has been declining rapidly, and
    the species is now classified as 'Vulnerable' on the International Union for
    the Conservation of Nature's (IUCN) Red List. According to the BC Ministry of
    Environment (2014) Science update for the South Peace Northern Caribou
    (link here: https://www2.gov.bc.ca/assets/gov/environment/plants-animals-and-ecosystems/wildlife-wildlife-habitat/caribou/science_update_final_from_web_jan_2014.pdf),
    the increase in the rate of death for caribou populations in recent years
    can mainly be attributed to increased predation, mainly by grey wolves.
    The report identifies several human activities including energy production
    and mining that contribute to this increase in predation by disrupting the
    natural habitats of the caribou. The data used to create this map
    is from a study by the B.C. Ministry of Environment & Climate Change that had
    the goal of recovering the caribou population. The source of the data is here:
    Seip DR, Price E (2019) Data from: Science update for the South Peace Northern
    Caribou (Rangifer tarandus caribou pop. 15) in British Columbia. Movebank Data Repository.
    https://doi.org/10.5441/001/1.p5bn656k.",

    "Note: Not every single year is represented in the data set, so if the map
    is completely blank, that likely indicates missing data rather than the caribou
    population being completely absent.",

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Select a Year",
                        min = 1988,
                        max = 2016,
                        value = 1988),

            checkboxGroupInput("study_site",
                          "Filter by Regional Group Name",
                          choices = c("Graham", "Burnt Pine", "Hart Ranges", "Kennedy", "Moberly", "Narraway",    "Quintette", "Scott"),
                          selected = c("Graham", "Burnt Pine", "Hart Ranges", "Kennedy", "Moberly", "Narraway",    "Quintette", "Scott")
                          ),
            checkboxGroupInput("season",
                               "Filter by Season",
                               choices = c("Winter", "Summer"),
                               selected = c("Winter", "Summer"),
                               )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("map")
        )
    )
)

# code below to create map from  https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
# get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# assign study sites to levels
# from stack overflow: https://stackoverflow.com/questions/30789837/assign-specific-colors-to-specific-values
locations$study_site = factor(locations$study_site, levels = c("Graham", "Burnt Pine", "Hart Ranges", "Kennedy", "Moberly", "Narraway",    "Quintette", "Scott"))

# assign colors to study sites
col <- setNames(c( "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00"), levels(locations$study_site))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderPlot({
        # convert locations to sf object and filter according to inputs
        sites <- st_as_sf(locations, coords = c("longitude", "latitude"),
                          crs = 4326, agr = "constant") %>%
            filter(year == input$year) %>%
            filter(study_site %in% input$study_site) %>%
            filter(season %in% input$season)

        # create map
        ggplot(data = world) +
            geom_sf() +
            annotation_scale(location = "bl", width_hint = 0.5) +
            annotation_north_arrow(location = "bl", which_north = "true",
                                   pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                                   style = north_arrow_fancy_orienteering) +
            geom_sf(data = sites, size = 4, alpha = 0.5, aes(color = study_site)) +
            coord_sf(xlim = c(-130, -118), ylim = c(52, 58)) +
            scale_color_manual(values = col) +
            labs(title = "Caribou Locations in British Columbia 1988-2016",
                 x = "Longitude",
                 y = "Latitude",
                 color = "Regional Group Name") +
            theme_bw()

    })
}

# Run the application
shinyApp(ui = ui, server = server)
