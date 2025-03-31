library(shiny)
library(shinyjs)
library(dplyr)
library(plotly)
library(ggplot2)
library(ComplexUpset)
library(viridis)
library(usmap)
library(sf)
library(collegeScorecard)

### Viridis palette names for selection ###
viridis_palettes <- c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")

### Define Metric Levels and Names ###
metric_levels <- list(
  deg_predominant = c("Certificate", "Associate", "Bachelor", "Graduate"),
  deg_highest   = c("Certificate", "Associate", "Bachelor", "Graduate"),
  control       = c("For-profit", "Nonprofit", "Public"),
  locale_type   = c("Rural", "Town", "Suburb", "City"),
  locale_size   = c("Remote", "Small", "Fringe", "Distant", "Midsize", "Large"),
  adm_req_test  = c("Not recommended", "Considered", "Recommended", "Required")
)

metric_names <- c(
  "deg_predominant" = "Predominant Degree Awarded",
  "deg_highest" = "Highest Degree Offered",
  "control" = "Institution Type",
  "locale_type" = "Institution Location Type",
  "locale_size" = "Institution Location Size",
  "adm_req_test" = "Admission Test Requirement",
  "is_hbcu" = "Historically Black College",
  "is_pbi" = "Predominantly Black Institution",
  "is_annhi" = "Alaska Native / Native Hawaiian Institution",
  "is_tribal" = "Tribal College or University",
  "is_aanapii" = "Asian American / Pacific Islander Institution",
  "is_hsi" = "Hispanic-Serving Institution",
  "is_nanti" = "Native American Non-Tribal Institution",
  "is_only_men" = "Men-Only Institution",
  "is_only_women" = "Women-Only Institution",
  "is_only_distance" = "Distance-Only Education Institution",
  "religious_affiliation" = "Religious Affiliation"
)

# Reverse lookup: display names as choices
metric_choices <- setNames(names(metric_names), metric_names)

# Boolean metrics
boolean_metrics <- c("is_hbcu", "is_pbi", "is_annhi", "is_tribal", 
                     "is_aanapii", "is_hsi", "is_nanti", 
                     "is_only_men", "is_only_women", "is_only_distance")

# Define available metrics (categorical, Boolean, and religious affiliation)
available_metrics <- c(names(metric_levels), boolean_metrics, "religious_affiliation")

### Prepare Lookup Tables ###
state_lookup <- data.frame(
  state_abbr = state.abb,
  state_name = state.name,
  stringsAsFactors = FALSE
) %>% arrange(state_name)

### Prepare School Data ###
# Transform coordinates using usmap and sf.
# This makes Alaska plot underneath the USA map similar to the Canary Islands under Spain.
school_data <- school %>% 
  rows_update(
    school %>% 
      rename(lat = latitude, lon = longitude) %>%
      filter(!is.na(lat) & !is.na(lon)) %>%
      usmap_transform() %>%
      st_transform(crs = 4326) %>%
      mutate(
        longitude = st_coordinates(geometry)[, 1],
        latitude  = st_coordinates(geometry)[, 2]
      ) %>%
      st_drop_geometry() %>%
      as_tibble(),
    by = "id"
  )

### Generate USA map in dataset coordinates ###
states_map <- us_map("states") %>% st_transform(crs = 4326)

### Helper Functions ###
computeNumericMetricValue <- function(data, metric) {
  if (metric %in% names(metric_levels)) {
    # Evenly map categorical levels between 0 and 1.
    levels_order <- metric_levels[[metric]]
    mapping <- setNames(seq(0, 1, length.out = length(levels_order)), levels_order)
    data <- data %>% mutate(metric_numeric = mapping[as.character(.data[[metric]])])
  } else if (metric %in% boolean_metrics) {
    # TRUE becomes 1; FALSE becomes 0.
    data <- data %>% mutate(metric_numeric = ifelse(.data[[metric]] == TRUE, 1,
                                                    ifelse(.data[[metric]] == FALSE, 0, NA)))
  } else if (metric == "religious_affiliation") {
    # Any non-NA becomes 1; NA becomes 0.
    data <- data %>% mutate(metric_numeric = ifelse(is.na(.data[[metric]]), 0, 1))
  }
  return(data)
}

computeMetricValue <- function(data, metric) {
  if (metric %in% names(metric_levels)) {
    data <- data %>% mutate(metric_val = factor(.data[[metric]], levels = metric_levels[[metric]]))
  } else if (metric %in% boolean_metrics) {
    data <- data %>% mutate(metric_val = factor(ifelse(.data[[metric]] == TRUE, "Yes", "No")))
  } else if (metric == "religious_affiliation") {
    data <- data %>% mutate(metric_val = factor(ifelse(is.na(.data[[metric]]), "No/NA", "Yes")))
  }
  return(data)
}

### Define UI ###
ui <- fluidPage(
  useShinyjs(),  # Include shinyjs for custom messages
  tags$script("
    Shiny.addCustomMessageHandler('open-url', function(url) {
      window.open(url, '_blank');
    });
  "),
  titlePanel("College Scorecard Visualizer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("state", "Select State", 
                  choices = c("All", setNames(state_lookup$state_abbr, state_lookup$state_name)), 
                  selected = "All"),
      selectInput("metric", "Select Metric", 
                  choices = metric_choices, 
                  selected = "deg_predominant"),
      selectInput("second_metric", "Compare metric",
                  choices = c("None", metric_choices), 
                  selected = "None"),
      selectInput("palette", "Palette",
                  choices = viridis_palettes,
                  selected = "viridis")
    ),
    mainPanel(
      width = 9,
      plotlyOutput("mapPlot", width = "100%", height = "50%"),
      plotlyOutput("comparisonPlot", width = "100%", height = "50%")
    )
  )
)

### Define Server ###
server <- function(input, output, session) {
  
  # Reactive data filtered by state selection
  filteredData <- reactive({
    if (input$state == "All") {
      school_data
    } else {
      school_data %>% filter(state == input$state)
    }
  })
  
  # Render interactive map plot
  output$mapPlot <- renderPlotly({
    req(filteredData())
    data <- filteredData()
    
    if (input$state == "All") {
      # National view: compute numeric metric values for color mapping.
      data_numeric <- computeNumericMetricValue(data, input$metric)
      
      # Aggregate metric values by state.
      aggregated <- data_numeric %>% 
        group_by(state) %>% 
        summarize(mean_metric = mean(metric_numeric, na.rm = TRUE), .groups = "drop")
      
      # Create tooltip text with category breakdown.
      tooltip_data <- data %>% 
        computeMetricValue(input$metric) %>%
        filter(!is.na(metric_val)) %>%
        group_by(state, metric_val) %>%
        summarize(count = n(), .groups = "drop") %>%
        group_by(state) %>%
        summarize(
          perc_text = paste0(metric_val, ": ", round(100 * count / sum(count), 1), "%", collapse = "<br>"),
          .groups = "drop"
        )
      
      # Merge aggregated values and tooltip data with state polygons.
      states_map_merged <- states_map %>%
        left_join(aggregated, by = c("abbr" = "state")) %>%
        left_join(tooltip_data, by = c("abbr" = "state")) %>%
        mutate(tooltip_text = paste0("State: ", full, "<br>", perc_text))
      
      # Build the ggplot for national view.
      if (input$metric %in% boolean_metrics) {
        p <- ggplot() +
          geom_sf(data = states_map_merged, 
                  aes(fill = mean_metric, text = tooltip_text, customdata = abbr),
                  color = "black") +
          scale_fill_viridis_c(
            name = metric_names[[input$metric]],
            na.value = "gray",
            limits = c(0, 1),
            breaks = c(0, 1),
            labels = c("0%", "100%"),
            option = input$palette
          ) +
          theme_minimal() +
          labs(title = paste("Mean", metric_names[[input$metric]], "by State"))
      } else if (input$metric == "religious_affiliation") {
        p <- ggplot() +
          geom_sf(data = states_map_merged, 
                  aes(fill = mean_metric, text = tooltip_text, customdata = abbr),
                  color = "black") +
          scale_fill_viridis_c(
            name = metric_names[[input$metric]],
            na.value = "gray",
            limits = c(0, 1),
            breaks = c(0, 1),
            labels = c("0%\n(includes NA)", "100%"),
            option = input$palette
          ) +
          theme_minimal() +
          labs(title = paste("Mean", metric_names[[input$metric]], "by State"))
      } else {
        p <- ggplot() +
          geom_sf(data = states_map_merged, 
                  aes(fill = mean_metric, text = tooltip_text, customdata = abbr),
                  color = "black") +
          scale_fill_viridis_c(
            name = metric_names[[input$metric]],
            na.value = "gray",
            limits = c(0, 1),
            breaks = 0:(length(metric_levels[[input$metric]]) - 1) / (length(metric_levels[[input$metric]]) - 1),
            labels = metric_levels[[input$metric]],
            option = input$palette
          ) +
          theme_minimal() +
          labs(title = paste("Mean", metric_names[[input$metric]], "by State"))
      }
      
    } else {
      # State view: compute categorical metric values.
      data <- computeMetricValue(data, input$metric)
      states_map_state <- states_map %>% filter(abbr == input$state)
      
      p <- ggplot() +
        geom_sf(data = states_map_state, fill = "white", color = "black") +
        coord_sf() +
        theme_minimal() +
        geom_point(
          data = data, 
          aes(
            x = longitude, 
            y = latitude, 
            text = paste("School:", name,
                         "<br>City:", city,
                         "<br>Category:", metric_val),
            color = metric_val,
            customdata = url
          ),
          size = 1, alpha = 0.8
        ) +
        scale_color_viridis_d(
          na.value = "gray",
          option = input$palette
        ) +
        labs(
          color = metric_names[[input$metric]],
          title = paste(
            metric_names[[input$metric]],
            "in",
            state_lookup$state_name[match(input$state, state_lookup$state_abbr)]
          )
        )
    }
    
    # Convert ggplot to an interactive Plotly object.
    ggplotly(p, tooltip = "text", source = "main")
  })
  
  output$comparisonPlot <- renderPlotly({
    req(filteredData())
    # Only proceed if a second metric is selected.
    if (input$second_metric != "None") {
      data <- filteredData()
      cont_table <- table(
        as.character(data[[input$metric]]), 
        as.character(data[[input$second_metric]])
      )
      # Convert the table to a data frame.
      cont_df <- as.data.frame(cont_table)
      names(cont_df) <- c("X", "Y", "Count")
      
      # Create the heatmap with ggplot2.
      p <- ggplot(cont_df, aes(x = X, y = Y, fill = Count)) +
        geom_tile(color = "white") +
        scale_fill_viridis_c(option = input$palette) +
        geom_text(aes(label = Count), color = "white") +
        labs(
          title = paste(
            "Comparison of",
            metric_names[[input$metric]],
            "and",
            metric_names[[input$second_metric]],
            "in",
            ifelse(input$state == "All", "the US", input$state)
          ),
          x = metric_names[[input$metric]],
          y = metric_names[[input$second_metric]]
        ) +
        theme_minimal()
      
      # Convert to interactive Plotly plot.
      ggplotly(p, tooltip = "text")
    }
  })
  
  # Observe Plotly click events.
  observeEvent(event_data("plotly_click", source = "main"), {
    click <- event_data("plotly_click", source = "main")
    if (!is.null(click)) {
      if (input$state == "All") {
        # National view: clicking a state updates the "state" selectInput.
        # Create a point geometry from the click coordinates.
        click_point <- st_as_sf(data.frame(x = click$x, y = click$y), coords = c("x", "y"), crs = 4326)
        # Identify which state polygon was clicked.
        clicked_state <- states_map$abbr[as.integer(st_intersects(click_point, states_map))]
        if (!is.na(clicked_state)) {
          updateSelectInput(session, "state", selected = clicked_state)
        }
      } else {
        # State view: clicking a school point should open the website.
        # Assumes the school's URL starts with "http".
        if (!is.null(click$customdata) && grepl("^http", click$customdata)) {
          session$sendCustomMessage("open-url", click$customdata)
        }
      }
    }
  })
}

shinyApp(ui, server)
