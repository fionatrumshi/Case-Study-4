# =============================================================================
# AKSTA Statistical Computing
# Case Study 4: Shiny apps 
# CIA World Factbook 2020
# =============================================================================
# To run:  shiny::runApp()  (with data_cia2.json in the same folder)
# =============================================================================


# -----------------------------------------------------------------------------
# 0. SETUP
# -----------------------------------------------------------------------------
library(shiny)
library(jsonlite)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(plotly)
library(countrycode)


# -----------------------------------------------------------------------------
# 1. DATA LOADING & PREPARATION
# -----------------------------------------------------------------------------

# 1a. Load the data set
cia2020_table <- jsonlite::fromJSON("data_cia2.json")
cia2020_table <- as.data.frame(cia2020_table)
cia2020_table <- cia2020_table %>% filter(!is.na(continent))
# 1b. Lookup: friendly label -> actual column name
column_lookup <- c("Education Expenditure" = "expenditure",
                   "Youth Unemployment Rate" = "youth_unempl_rate",
                   "Net Migration Rate" = "net_migr_rate",
                   "Population Growth Rate" = "pop_growth_rate",
                   "Electricity Fossil Fuel" = "electricity_fossil_fuel",
                   "Life Expectancy" = "life_expectancy")

## MAP Data
# 1c. World map + ISO3 codes
world_map <- map_data("world")
world_map$ISO3 <- countrycode::countrycode(sourcevar = world_map$region,
                                           origin = "country.name",
                                           destination = "iso3c", 
                                           nomatch = NA, warn = FALSE)

# Ensure CIA dataset has an ISO3 column for a safe join
if(!"iso3" %in% colnames(cia2020_table)){
  cia2020_table$iso3 <- countrycode::countrycode(sourcevar = cia2020_table$country, 
                                                 origin = "country.name", 
                                                 destination = "iso3c", 
                                                 nomatch = NA,
                                                 warn = FALSE)
}

# 1d. Left-join map polygons with the CIA data
map_data_joined <- left_join(world_map, cia2020_table, by = c("ISO3" = "iso3"),
                             relationship = "many-to-many")


# -----------------------------------------------------------------------------
# 2. USER INTERFACE (ui)
# -----------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("CIA World Factbook 2020"),
  p("Welcome to my Shiny App, which allows you to visualize variables from the CIA 
    factbook on the worldmap, generate descriptive statistic and statistical graphics!"),
  tabsetPanel(
    # --- TAB 1: UNIVARIATE ANALYSIS ------------------------------------------
    tabPanel("Univariate Analysis",
             sidebarPanel(
               selectInput("variable_univariate", "Select a variable", 
                           selected = "Education Expenditure", 
                           choices = names(column_lookup)),
               actionButton("displaytable", "View Raw Data"),
               br(), br(),
               dataTableOutput("Table_Univariate")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Map", 
                          p("The map contains values of the selected variable. The countries with gray areas have a missing value for the visualized variable."),
                          plotlyOutput("Plot_Map", height = "500px")),
                 
                 tabPanel("Global Analysis", 
                          fluidRow(
                            column(6, plotlyOutput("Plot_Global_HistDens")),
                            column(6, plotlyOutput("Plot_Global_Box"))
                          )),
                 
                 tabPanel("Analysis per Continent", 
                          fluidRow(
                            column(6, plotlyOutput("Plot_Continent_Dens")),
                            column(6, plotlyOutput("Plot_Continent_Box"))
                          ))
               )
             )
    ),
    # --- TAB 2: MULTIVARIATE ANALYSIS ----------------------------------------
    tabPanel("Multivariate Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable_multivariate1", "Select variable 1", 
                             selected = "Education Expenditure", 
                             choices = names(column_lookup)),
                 selectInput("variable_multivariate2", "Select variable 2", 
                             selected = "Youth Unemployment Rate", 
                             choices = names(column_lookup)),
                 selectInput("scale", "Scale points by", 
                             selected = "area", 
                             choices = c("Area" = "area", "Population" = "population"))
               ),
               mainPanel(
                 plotlyOutput("Plot_Multivariate", height = "600px")
               )
             )    
    )
  )
)


# -----------------------------------------------------------------------------
# 3. SERVER (server)
# -----------------------------------------------------------------------------

server <- function(input, output, session){
  
  # Map friendly UI labels back to actual dataframe column names
  var_uni    <- reactive({ column_lookup[[input$variable_univariate]] })
  var_multi1 <- reactive({ column_lookup[[input$variable_multivariate1]] })
  var_multi2 <- reactive({ column_lookup[[input$variable_multivariate2]] })
  var_size   <- reactive({ input$scale })   # "area" or "population" (already a column name)
  
  selected_data <- eventReactive(input$displaytable, {
    col_name <- var_uni()
    df_sub <- cia2020_table[, c("country", "continent", col_name), drop = FALSE]
    colnames(df_sub) <- c("Country", "Continent", input$variable_univariate)
    df_sub
  })
  
  # --- UNIVARIATE OUTPUTS ---------------------------------------------------
  
  output$Table_Univariate <- renderDataTable({
    datatable(
      selected_data(),
      options = list(pageLength = 15, lengthMenu = c(15, 25, 50, 100))
    )
  })
  
  output$Plot_Map <- renderPlotly({
    col_name <- var_uni()
    p <- ggplot(map_data_joined, aes(x = long, y = lat, group = group, 
                                     text = paste("Country:", region, "<br>Value:", .data[[col_name]]))) +
      geom_polygon(aes(fill = .data[[col_name]]), colour = "white", size = 0.1) +
      scale_fill_viridis_c(na.value = "gray90", name = input$variable_univariate) +
      theme_minimal() +
      theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank())
    ggplotly(p, tooltip = "text") %>% style(hoveron = "fills")
  })
  
  output$Plot_Global_HistDens <- renderPlotly({
    col_name <- var_uni()
    p <- ggplot(cia2020_table, aes(x = .data[[col_name]])) +
      geom_histogram(aes(y = ..density..), fill = "#8b9dc3", color = "white", alpha = 0.7, bins = 30) +
      geom_density(color = "darkblue", size = 1) +
      labs(x = input$variable_univariate, y = "Density") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$Plot_Global_Box <- renderPlotly({
    col_name <- var_uni()
    p <- ggplot(cia2020_table, aes(y = .data[[col_name]])) +
      geom_boxplot(fill = "white", color = "black") +
      labs(y = input$variable_univariate, x = "") +
      theme_minimal() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    ggplotly(p)
  })
  
  output$Plot_Continent_Dens <- renderPlotly({
    col_name <- var_uni()
    p <- ggplot(cia2020_table, aes(x = .data[[col_name]], fill = continent, color = continent)) +
      geom_density(alpha = 0.4) +
      labs(x = input$variable_univariate, y = "Density") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$Plot_Continent_Box <- renderPlotly({
    col_name <- var_uni()
    p <- ggplot(cia2020_table, aes(x = continent, y = .data[[col_name]])) +
      geom_boxplot() +
      labs(x = "Continent", y = input$variable_univariate) +
      theme_minimal()
    ggplotly(p)
  })
  
  # --- MULTIVARIATE OUTPUTS -------------------------------------------------
  
  output$Plot_Multivariate <- renderPlotly({
    col_name1 <- var_multi1()
    col_name2 <- var_multi2()
  
    plot_df <- cia2020_table
    plot_df$size_var <- if (var_size() == "population") plot_df$population else plot_df$area
    
    p <- ggplot(plot_df,
                aes(x = .data[[col_name1]], y = .data[[col_name2]], color = continent)) +
      geom_point(aes(size = size_var), alpha = 0.6) +   
      geom_smooth(method = "loess", se = FALSE) +
      labs(x = input$variable_multivariate1,
           y = input$variable_multivariate2,
           color = "Continent",
           size  = input$scale) +
      theme_minimal()
    ggplotly(p)
  })
}


# -----------------------------------------------------------------------------
# 4. RUN THE APP
# -----------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
