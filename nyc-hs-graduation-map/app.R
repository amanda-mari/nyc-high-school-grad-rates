# Load packages ---
library("shiny")
library("leaflet")
library("here")
library("tidyverse")
library("readxl")

data <- readxl::read_excel(here::here("nyc-hs-graduation-map/data/nyc_hs_grad_data.xlsx"))

# jittering latitude and longitude so that multiple schools within a location
# can be viewed when zooming in
data$lat2 <- jitter(data$lat, factor = 32)
data$lon2 <- jitter(data$lon, factor = 32)


# Creating UI

ui <- fluidPage(
  titlePanel(strong("NYC High School Graduation Results")),

  sidebarLayout(
    position = "left",
    sidebarPanel(
      selectizeInput("cohort_group", "Student Demographic Group", choices = NULL),
      numericInput("cohort_start", "Student Start Year", value = NULL),
      selectizeInput("cohort_graduation_outcome", "Student Graduation Outcome",
        choices = c(
          "Graduation Rate" = "group_grad_rate",
          "Dropout Rate" = "group_dropout_rate",
          "Still Enrolled Rate" = "group_still_enrolled_rate"
        )
      ),
      selectizeInput("cohort_type",
        label = "Student Trajectory",
        choices = NULL
      )
    ),
    mainPanel(
      leafletOutput("my_map"),
      br(),
      br(),
      dataTableOutput("filtered_data")
    )
  )
)

# Server

server <- function(input, output, session) {
  df <- reactiveVal(data)

  observe({
    # Get all cohort groups
    updateSelectizeInput(session, "cohort_group", choices = unique(df()$cohort_group))
    # Get cohort start years
    updateNumericInput(session, "cohort_start",
      min = min(df()$cohort_start),
      max = max(df()$cohort_start),
      value = min(df()$cohort_start),
      step = 1
    )
    # Get cohort types
    updateSelectizeInput(session, "cohort_type", choices = unique(df()$cohort_type))
  })

  output$my_map <- renderLeaflet({
    req(input$cohort_group, input$cohort_type, input$cohort_start, input$cohort_graduation_outcome)

    df() %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -73.977655, lat = 40.769361, zoom = 10.2)
  })



  output$filtered_data <- renderDataTable({
    filtered_data <- df()[df()$cohort_group == input$cohort_group &
      df()$cohort_type == input$cohort_type &
      df()$cohort_start == input$cohort_start, ] %>%
      select(
        lat, lon, campus_number, total_schools_on_campus, school_name,
        cohort_group, cohort_start, cohort_type, group_size,
        input$cohort_graduation_outcome
      )
  })

  filtered_data <- reactive(df()[df()$cohort_group == input$cohort_group &
    df()$cohort_type == input$cohort_type &
    df()$cohort_start == input$cohort_start, ] %>%
    select(
      lat2, lon2, campus_number, borough,
      school_name, cohort_group, cohort_start,
      cohort_type, group_size, input$cohort_graduation_outcome
    ))

  observe({
    outcome_variable <- switch(input$cohort_graduation_outcome,
      "group_grad_rate" = filtered_data()$group_grad_rate,
      "group_dropout_rate" = filtered_data()$group_dropout_rate,
      "group_still_enrolled_rate" = filtered_data()$group_still_enrolled_rate
    )

    full_name_outcome_variable <- switch(input$cohort_graduation_outcome,
      "group_grad_rate" = "Graduation Rate",
      "group_dropout_rate" = "Dropout Rate",
      "group_still_enrolled_rate" = "Still Enrolled Rate"
    )

    color_palette <- colorNumeric(
      palette = "viridis",
      c(0, 100), reverse = TRUE
    )

    leafletProxy("my_map", data = filtered_data()) %>%
      addCircleMarkers(
        lng = filtered_data()$lon2,
        lat = filtered_data()$lat2,
        stroke = TRUE,
        opacity = .7,
        fillOpacity = .6,
        radius = 6,
        label = paste0(
          filtered_data()$school_name, " ", round(outcome_variable),
          "%", " (campus # ", filtered_data()$campus_number, ")"
        ),
        color = ~ color_palette(outcome_variable)
      ) %>%
      addLegend("bottomleft",
        pal = color_palette, bins = c(0, 20, 40, 60, 80, 100),
        opacity = 1, values = c(0, 100),
        title = if (input$cohort_group == "All Students") {
          paste(
            input$cohort_type, "<br>", full_name_outcome_variable,
            "<br> for", input$cohort_group, "<br> starting in", input$cohort_start
          )
        }
        else {
          paste(
            input$cohort_type, "<br>", full_name_outcome_variable,
            "<br> for", input$cohort_group, "Students", "<br> starting in", input$cohort_start
          )
        },
        labFormat = labelFormat(suffix = "%")
      )
  })
}

# Run the application

shinyApp(ui = ui, server = server)
