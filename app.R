# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(plotly)

# Load the data
load("C:/Users/patri/OneDrive/Documents/R/AirBnB.RData")

# Join datasets and create df2
df1 <- L %>%
  full_join(R, by = c("id" = "listing_id"))
df2 <- df1[c("id", "host_response_time", "state", "beds", "amenities", "bathrooms", 
             "room_type", "price", "monthly_price", "cleaning_fee", "review_scores_rating", 
             "require_guest_profile_picture", "instant_bookable", "security_deposit", 
             "weekly_price", "square_feet", "accommodates", "latitude", "longitude", 
             "zipcode", "neighbourhood_cleansed", "host_has_profile_pic", "city", 
             "minimum_nights", "maximum_nights", "host_id", "host_name", "reviews_per_month", 
             "first_review", "last_review")]

# Define UI for the application
ui <- fluidPage(
  titlePanel("Airbnb Data Analysis - Paris"),
  sidebarLayout(
    sidebarPanel(
      selectInput("feature", "Select Apartment Feature:", # Select the different apartment/listing features to view
                  choices = c("room_type" = "room_type", "beds" = "beds", "bathrooms" = "bathrooms", 
                               "accommodates" = "accommodates")),
      
      selectInput("priceRange", "Select Price Range:", #Select different price ranges to view the features above
                  choices = c("0 to 50" = "0-50", 
                              "50 to 100" = "50-100", 
                              "100 to 150" = "100-150", 
                              "150 to 200" = "150-200", 
                              "200 to 250" = "200-250", 
                              "250 to 300" = "250-300", 
                              "300 to 350" = "300-350", 
                              "350 to 400" = "350-400", 
                              "400 to 450" = "400-450", 
                              "450 to 500" = "450-500")),
      selectInput("arrondissement", "Select City Quarter:",
                  choices = sort(unique(df2$neighbourhood_cleansed))),
      dateRangeInput("dateRange", "Select Date Range:", # the different dates to enable selection of visit frequency along timeline 
                     start = min(as.Date(df2$first_review), na.rm = TRUE), 
                     end = max(as.Date(df2$last_review), na.rm = TRUE))
    ),
    mainPanel(
      plotOutput("featurePlot"), # Plot of Price against different features
      plotOutput("pricePlot"), # Plot of listings at different neighborhoods 
      plotOutput("visitPlot"), # Plot of visit frequency at different times. Select dates
      plotOutput("ownersPlot") # Plot of top owners of apartments 
    )
  )
)

# Define server logic required for the app
server <- function(input, output, session) {
  filtered_data <- reactive({
    price_bounds <- strsplit(input$priceRange, "-")[[1]]
    lower_bound <- as.numeric(price_bounds[1])
    upper_bound <- as.numeric(price_bounds[2])
    
    df2 %>%
      filter(between(as.numeric(price), lower_bound, upper_bound))
  })
  
  output$featurePlot <- renderPlot({
    req(input$feature)  # Ensure input$feature is selected
    data <- filtered_data()
    
    # Check if the feature is numeric or categorical
    is_numeric <- is.numeric(data[[input$feature]])
    
    if (!is_numeric) {
      # Categorical feature plotting
      top_categories <- data %>%
        group_by(.data[[input$feature]]) %>%
        summarise(count = n(), .groups = 'drop') %>%
        top_n(3, count) %>%
        pull(.data[[input$feature]])
      
      plot_data <- data %>%
        filter(.data[[input$feature]] %in% top_categories) %>%
        select(.data[[input$feature]], price) %>%
        na.omit()
      
      ggplot(plot_data, aes(x = factor(.data[[input$feature]]), y = as.numeric(price))) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = paste("Price vs Top 3", input$feature, "within selected price range"),
             x = input$feature, y = "Price") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      # Numeric feature plotting with jittering to reduce overplotting
      ggplot(data, aes(x = .data[[input$feature]], y = as.numeric(price))) +
        geom_point(position = position_jitter(width = 0.2, height = 0), alpha = 0.5) +
        geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
        theme_minimal() +
        labs(title = paste("Price vs", input$feature, "within selected price range"),
             x = input$feature, y = "Price")
    }
  })
  
  output$pricePlot <- renderPlot({
    top_neighbourhoods <- df2 %>%
      group_by(neighbourhood_cleansed) %>%
      summarise(median_price = median(as.numeric(price), na.rm = TRUE)) %>%
      top_n(10, median_price) %>%
      pull(neighbourhood_cleansed)
    
    filtered_data <- df2 %>%
      filter(neighbourhood_cleansed %in% top_neighbourhoods)
    
    ggplot(filtered_data, aes(x = neighbourhood_cleansed, y = as.numeric(price), fill = neighbourhood_cleansed)) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Paired") +  # Using a ColorBrewer palette
      theme_minimal() +
      labs(title = "Top 10 Neighbourhoods by Median Renting Price", x = "City Quarter", y = "Price") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")  # Hides the legend if it's redundant
  })
  
  output$visitPlot <- renderPlot({
    req(input$dateRange)  # Ensure date range is selected
    filtered_data <- df2 %>%
      filter(as.Date(first_review) >= input$dateRange[1] & as.Date(last_review) <= input$dateRange[2])
    summarised_data <- filtered_data %>%
      group_by(neighbourhood_cleansed) %>%
      summarize(average_visits = mean(reviews_per_month, na.rm = TRUE))
    ggplot(summarised_data, aes(x = reorder(neighbourhood_cleansed, -average_visits), y = average_visits)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Visit Frequency at Different Quarters by selected dates", x = "City Quarter", y = "Average Visits per Month") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$ownersPlot <- renderPlot({
    owners_data <- df2 %>%
      group_by(host_id, host_name) %>%
      summarise(n = n(), .groups = 'drop') %>%
      top_n(n = 10, wt = n) %>%
      arrange(desc(n))
    ggplot(owners_data, aes(x = reorder(host_name, n), y = n)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Top 10 Owners by Number of Apartments", x = "Owner", y = "Number of Apartments") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
