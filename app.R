#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# app.R
library(shiny)
library(tidyverse)
library(plotly)


# Read in diel oxygen data
diel_oxygen <- readRDS("data/diel_oxygen_30min.RDS")
# Define consistent site order
site_order2 <- c("Sage Lot", "Childs River")

# Define UI
ui <- navbarPage(
  
  # Title
  title = "Semester in Environmental Science",
  
  theme = shinythemes::shinytheme("flatly"),
  
  # Dissolved oxygen 
  tabPanel("Dissolved Oxygen",
           fluidRow(
             column(
               width = 12,
               h3("Welcome to the Dissolved Oxygen Data Explorer"),
               p("Use the controls on the left to explore and compare time series of dissolved oxygen across the Sage Lot and Childs River sub-estuaries."),
               p("Start by adding DO data for one or both sites, then layer on environmental drivers like light or temperature, and highlight hypoxia thresholds. You can also adjust the date range to focus on specific periods before exporting your plot."),
               p("Tip: This plot is interactive — zoom in on specific days and hover to view exact DO levels."),
               tags$hr()
             )
           ),
           sidebarLayout(
             sidebarPanel(
               
               # Add oxygen
               checkboxGroupInput(
                 "add_oxygen",
                 "Add dissolved oxygen:",
                 choices = site_order2,
                 selected = NULL
               ),
               
               # Add hypoxia thresholds
               checkboxGroupInput(
                 "add_thresholds",
                 "Add dissolved oxygen threshold:",
                 choices = c("Stressful (< 4 mg/L)", "Hypoxic (< 2 mg/L)"),
                 selected = NULL
               ),
               
               # Add drivers
               selectInput(
                 "which_driver",
                 "Add light or temperature:",
                 choices = c(
                   "None" = "none",
                   "Temperature" = "Temp",
                   "Light" = "TotPar"
                 ),
                 selected = "none"
               ),
               
               dateRangeInput(
                 inputId = "clip_dates",
                 label = "Custom date range:",
                 start = min(diel_oxygen$DateTimeStamp2, na.rm = TRUE),
                 end = max(diel_oxygen$DateTimeStamp2, na.rm = TRUE),
                 min = min(diel_oxygen$DateTimeStamp2, na.rm = TRUE),
                 max = max(diel_oxygen$DateTimeStamp2, na.rm = TRUE)
               ),
               actionButton("reset_dates", "Reset Dates"),
               
               width = 3  # narrower sidebar
               
             ),
             mainPanel(
               plotlyOutput("oxygenPlotOutput", height = "800px"),  # bigger plot
               width = 9
             )
           )
  )
)

################################################################################
######### === Server logic ===
################################################################################

server <- function(input, output, session) {
  
  ### === Diel Oxygen plot for third tab === ###
  # --- Reset date range to full extent ---
  observeEvent(input$reset_dates, {
    updateDateRangeInput(
      session,
      inputId = "clip_dates",
      start = min(diel_oxygen$DateTimeStamp2, na.rm = TRUE),
      end = max(diel_oxygen$DateTimeStamp2, na.rm = TRUE)
    )
  })
  
  output$oxygenPlotOutput <- renderPlotly({
    
    # Use full range for y-axis
    DO_range <- range(diel_oxygen$DO_mgl, na.rm = TRUE)
    DO_margin <- 0
    y_limits <- c(max(0, DO_range[1] - DO_margin), DO_range[2] + DO_margin)
    
    # Default to full date range if no user input (helps avoid NULL error)
    if (is.null(input$clip_dates)) {
      clip_range <- range(diel_oxygen$DateTimeStamp2, na.rm = TRUE)
    } else {
      clip_range <- as.POSIXct(input$clip_dates)
    }
    
    if (is.null(input$add_oxygen) || length(input$add_oxygen) == 0) {
      # Placeholder when no site is selected
      placeholder <- expand.grid(
        station_name = factor(site_order2, levels = site_order2),
        DateTimeStamp2 = clip_range[1],
        DO_mgl = y_limits
      )
      
      p <- ggplot() +
        geom_blank(data = placeholder, aes(x = DateTimeStamp2, y = DO_mgl)) +
        facet_wrap(~station_name, ncol = 1, drop = FALSE) +
        xlim(clip_range) +
        ylim(y_limits) +
        scale_y_continuous(name = "DO (mg/L)") +
        xlab("") +
        theme_classic() +
        theme(
          strip.text = element_text(size = 20),
          axis.text = element_text(size = 16, colour = "black"),
          axis.title = element_text(size = 18, colour = "black")
        )
      
    } else {
      # Filter data by selected sites and date range
      plot_data <- diel_oxygen %>% 
        filter(
          station_name %in% input$add_oxygen,
          DateTimeStamp2 >= clip_range[1],
          DateTimeStamp2 <= clip_range[2]
        ) %>%
        mutate(station_name = factor(station_name, levels = site_order2))
      
      # Placeholder ensures all facets show up
      placeholder <- expand.grid(
        station_name = factor(site_order2, levels = site_order2),
        DateTimeStamp2 = clip_range[1],
        DO_mgl = y_limits
      )
      
      p <- ggplot() +
        geom_blank(data = placeholder, aes(x = DateTimeStamp2, y = DO_mgl)) +
        facet_wrap(~station_name, ncol = 1, drop = FALSE) +
        xlim(clip_range) +
        ylim(y_limits) +
        scale_y_continuous(name = "DO (mg/L)") +
        xlab("") +
        theme_classic() +
        theme(
          strip.text = element_text(size = 20),
          axis.text = element_text(size = 16, colour = "black"),
          axis.title = element_text(size = 18, colour = "black")
        )
      
      # === Add driver layer (light or temperature) BEFORE DO line ===
      if (input$which_driver == "TotPar") {
        scaling_factor <- max(plot_data$DO_mgl, na.rm = TRUE) / max(plot_data$TotPAR, na.rm = TRUE)
        
        p <- p + geom_ribbon(
          data = plot_data,
          aes(
            x = DateTimeStamp2,
            ymin = 0,
            ymax = TotPAR * scaling_factor,
            group = TotPAR_group
          ),
          fill = "#00A2A2",
          alpha = 0.3,
          na.rm = TRUE,
          inherit.aes = FALSE
        )
      } else if (input$which_driver == "Temp") {
        scaling_factor <- max(plot_data$DO_mgl, na.rm = TRUE) / max(plot_data$Temp, na.rm = TRUE)
        p <- p + geom_line(
          data = plot_data,
          aes(x = DateTimeStamp2, y = Temp * scaling_factor),
          color = "#00A2A2", alpha = 0.5, na.rm = TRUE
        )
      }
      
      # === Add DO line AFTER driver layer ===
      p <- p + geom_line(
        data = plot_data,
        aes(x = DateTimeStamp2, y = DO_mgl, text = tooltip_text, group = station_name),
        color = "black", na.rm = TRUE
      )
      
      # === Add thresholds AFTER everything else ===
      if ("Stressful (< 4 mg/L)" %in% input$add_thresholds) {
        p <- p + geom_hline(yintercept = 4, color = "orange", linetype = "dashed", linewidth = 1)
      }
      if ("Hypoxic (< 2 mg/L)" %in% input$add_thresholds) {
        p <- p + geom_hline(yintercept = 2, color = "red", linetype = "dashed", linewidth = 1)
      }
    }
    
    ggplotly(p, tooltip = "text")
  })
  
  
}


# Run app
shinyApp(ui = ui, server = server)

##rsconnect::deployApp(appDir = here("ses-diel-oxygen"))


