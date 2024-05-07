library(shiny)
library(shinythemes)
library(readr)
library(caret)
library(DT)  # For displaying data tables

# Load and prepare the data
data <-Data  # Update the path as necessary
names(data)[names(data) == "Dry Bulb Temperature [°C]"] <- "Dry_Bulb_Temperature_C"
names(data) <- gsub("^in\\.", "", names(data))
cols_to_convert <- 1:79
data[cols_to_convert] <- lapply(data[cols_to_convert], as.factor)
train_index <- createDataPartition(data$energy_sum, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
lm_model <- lm(energy_sum ~ sqft + cooling_setpoint + cooling_setpoint_offset_period +
                 window_areas + Dry_Bulb_Temperature_C + ducts +
                 occupants + heating_setpoint + windows + 
                 orientation + geometry_garage + heating_setpoint_offset_period + weather_file_city,
               data = train_data)

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Energy Consumption Prediction"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sqft", "Square Footage", choices = unique(train_data$sqft)),
      selectInput("cooling_setpoint", "Cooling Setpoint", choices = unique(train_data$cooling_setpoint)),
      selectInput("cooling_setpoint_offset_period", "Cooling Setpoint Offset Period", choices = unique(train_data$cooling_setpoint_offset_period)),
      selectInput("window_areas", "Window Areas", choices = unique(train_data$window_areas)),
      selectInput("dry_bulb_temperature", "Dry Bulb Temperature (C)", choices = unique(train_data$Dry_Bulb_Temperature_C)),
      selectInput("ducts", "Ducts", choices = unique(train_data$ducts)),
      selectInput("occupants", "Occupants", choices = unique(train_data$occupants)),
      selectInput("heating_setpoint", "Heating Setpoint", choices = unique(train_data$heating_setpoint)),
      selectInput("windows", "Windows", choices = unique(train_data$windows)),
      selectInput("orientation", "Orientation", choices = unique(train_data$orientation)),
      selectInput("geometry_garage", "Geometry Garage", choices = unique(train_data$geometry_garage)),
      selectInput("heating_setpoint_offset_period", "Heating Setpoint Offset Period", choices = unique(train_data$heating_setpoint_offset_period)),
      selectInput("weather_file_city", "Weather File City", choices = unique(train_data$weather_file_city)),
      actionButton("predict", "Generate Prediction", class = "btn-primary")
    ),
    mainPanel(
      h3("Entered Values"),
      verbatimTextOutput("inputDisplay"),
      h3("Prediction Result"),
      verbatimTextOutput("prediction")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$predict, {
    # Prepare testDF using the same column names as the model expects
    testDF <- data.frame(
      sqft = factor(input$sqft, levels = levels(train_data$sqft)),
      cooling_setpoint = factor(input$cooling_setpoint, levels = levels(train_data$cooling_setpoint)),
      cooling_setpoint_offset_period = factor(input$cooling_setpoint_offset_period, levels = levels(train_data$cooling_setpoint_offset_period)),
      window_areas = factor(input$window_areas, levels = levels(train_data$window_areas)),
      Dry_Bulb_Temperature_C = as.numeric(input$dry_bulb_temperature),
      ducts = factor(input$ducts, levels = levels(train_data$ducts)),
      occupants = factor(input$occupants, levels = levels(train_data$occupants)),
      heating_setpoint = factor(input$heating_setpoint, levels = levels(train_data$heating_setpoint)),
      windows = factor(input$windows, levels = levels(train_data$windows)),
      orientation = factor(input$orientation, levels = levels(train_data$orientation)),
      geometry_garage = factor(input$geometry_garage, levels = levels(train_data$geometry_garage)),
      heating_setpoint_offset_period = factor(input$heating_setpoint_offset_period, levels = levels(train_data$heating_setpoint_offset_period)),
      weather_file_city = factor(input$weather_file_city, levels = levels(train_data$weather_file_city)),
      stringsAsFactors = TRUE
    )
    
    # Display input values
    output$inputDisplay <- renderText({
      input_labels <- c("Square Footage", "Cooling Setpoint", "Cooling Setpoint Offset Period", "Window Areas", "Dry Bulb Temperature (C)", "Ducts", "Occupants", "Heating Setpoint", "Windows", "Orientation", "Geometry Garage", "Heating Setpoint Offset Period", "Weather File City")
      input_values <- sapply(testDF, as.character)
      paste(input_labels, input_values, sep=": ", collapse="\n")
    })
    
    # Generate prediction and display it
    tryCatch({
      prediction <- predict(lm_model, newdata = testDF, type = "response")
      output$prediction <- renderText({
        sprintf("Prediction: %.6f kWh", prediction)
      })
    }, error = function(e) {
      output$prediction <- renderText({
        paste("Error in generating prediction:", e$message)
      })
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
