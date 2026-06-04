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
# Load required libraries:
#   - shiny           (app framework, tabsetPanel, reactive, action button)
#   - jsonlite        (read data_cia2.json)
#   - ggplot2         (plots + map_data("world") + scale_fill_viridis_c)
#   - plotly          (ggplotly for all interactive plots)
#   - dplyr / tidyr   (data wrangling, left_join)
#   - countrycode     (convert country names -> ISO3 codes)
#   - DT              (DTOutput / renderDT for the raw-data table)

#install.packages("shiny")
#install.packages("jsonlite")
#install.packages("plotly")
#install.packages("DT")

library(shiny)
library(jsonlite)
library(dplyr)
library(tidyr)
library(DT)

#??jsonlite


# -----------------------------------------------------------------------------
# 1. DATA LOADING & PREPARATION
# -----------------------------------------------------------------------------

# 1a. Load the data set
#     - Read data_cia2.json into a data frame (placed in the app folder).

cia2020_table <- jsonlite::fromJSON("data_cia2.json")
cia2020_table <- as.data.frame(cia2020_table)

cia2020_table


# 1b. Define a "lookup" between user-friendly variable labels and the actual
#     column names in the data, e.g.:
#       "Expenditure on education"  -> education column
#       "Youth unemployment rate"   -> youth_unempl column
#       "Net migration rate"        -> migration column
#       "Population growth rate"    -> pop_growth column
#       "Electricity from fossil fuels" -> electricity column
#       "Life expectancy"           -> life_exp column
#     (Used so the UI never shows raw/code column names.)

column_lookup <- c("Education Expenditure" = "expenditure",
                   "Youth Unemployment Rate" = "youth_unempl_rate",
                   "Net Migration Rate" = "net_migr_rate",
                   "Population Growth Rate" = "pop_growth_rate",
                   "Electricity Fossil Fuel" = "electricity_fossil_fuel",
                   "Life Expectancy" = "life_expectancy")


## MAP Datas
# 1c. Prepare the world map data for the map tab:
#       world_map <- map_data("world")
#     Add ISO3 codes via countrycode::countrycode(..., destination = "iso3c").

# 1d. Left-join world_map with data_cia on the ISO3 codes so each map polygon
#     carries the variable values (countries with no match -> NA -> gray).




# -----------------------------------------------------------------------------
# 2. USER INTERFACE (ui)
# -----------------------------------------------------------------------------

# 2a. Title of the app ("CIA World Factbook 2020").
ui <- fluidPage(
  titlePanel("CIA World Factbook 2020"),
# 2b. A short "welcome" message describing what the app does.
  "Welcome to my Shiny App, which allows you to visualize variables from the CIA 
    factbook on the worldmap, generate descriptive statistic and statistical graphics!",
# 2c. tabsetPanel() with TWO tabs: "Univariate analysis" and
#     "Multivariate analysis".
  tabsetPanel(
    tabPanel("Univariate Analysis",
             # --- TAB 1: UNIVARIATE ANALYSIS (sidebarLayout) ---------------------------
             # 2c-i. SIDEBAR:
             #   - selectInput: choose ONE variable (education expenditure,
             #     youth unemployment, net migration, population growth,
             #     electricity fossil fuel, life expectancy) using friendly labels.
             #   - actionButton "View raw data": when pressed, show a table in the
             #     sidebar with Country, Continent and the selected variable's value.
             #     Use DTOutput()/renderDT() (or dataTableOutput/renderDataTable).
             #     Max 15 rows shown; use "nice" column names.
             sidebarPanel(
               selectInput("variable_univariate", "Select a variable", 
                           selected = "Education Expenditure", 
                           choices = column_lookup),
               actionButton("displaytable", "View Raw Data"),
               dataTableOutput("Table_Univariate")
             ),
             # 2c-ii. MAIN PANEL: a nested tabsetPanel with THREE tabs:
             #   - "Map":             plotlyOutput  (interactive world map)
             #   - "Global analysis": plotlyOutput x2 (boxplot + histogram/density)
             #   - "Analysis per continent": plotlyOutput x2 (grouped boxplot +
             #                               grouped density, grouped by continent)   
             mainPanel(
               tabsetPanel(
                 tabPanel("Map"),
                 tabPanel("Global Analysis"),
                 tabPanel("Analysis per Continent")
               )
             )
    ), 
    # --- TAB 2: MULTIVARIATE ANALYSIS (sidebarLayout) -------------------------
    tabPanel("Multivariate Analysis",
             # 2c-iii. SIDEBAR: THREE selectInputs:
             #   - selectInput variable 1 (same choices as univariate tab)
             #   - selectInput variable 2 (same choices as univariate tab)
             #   - selectInput "Scale points by": Population or Area
             sidebarPanel(
               selectInput("variable_multivariate_1", 
                           "Select variable 1", 
                           selected = "Education Expenditure",
                           choices = column_lookup),
               selectInput("variable_multivariate_2", 
                           "Select variable 2",
                           selected = "Education Expenditure",
                           choices = column_lookup),
               selectInput("variable_sized", 
                           "Scale points by:", 
                           selected = "Area",
                           choices = c("Area", "Population"))
             ),
             # 2c-iv. MAIN PANEL: plotlyOutput
             #   - interactive scatterplot of variable 1 vs variable 2
             #   - points colored by continent
             #   - point size scaled by chosen variable (population or area)
             #   - per-continent LOESS smooth: geom_smooth(method = "loess")
             #   - Hint: use different aesthetics for geom_point vs geom_smooth so the
             #     smooth lines are NOT sized.
             mainPanel()
             )
  )
)

# -----------------------------------------------------------------------------
# 3. SERVER (server)
# -----------------------------------------------------------------------------

server <- function(input, output, session){
  selected_data <- eventReactive(input$displaytable, {
    # Get the selected column from the input
    # Return a subset of iris with just that column
    cia2020_table[, c("country","continent",input$variable_univariate), drop = FALSE]
  })
  
  # Render the table using the reactive data
  output$Table_Univariate <- renderDataTable({
  datatable(
    selected_data(),
    options = list(
      pageLength = 15,
      lengthMenu = c(15, 25, 50, 100)
    )
  )
    })
}

# 3a. REACTIVES
#   - reactive() for the variable selected in the univariate tab.
#   - reactive()s for variable 1, variable 2, and the size variable in the
#     multivariate tab.
#   (Map friendly labels back to the real column names here.)

  # --- UNIVARIATE OUTPUTS ---------------------------------------------------

    # 3b. Raw-data table (renderDT) triggered by the "View raw data" button
    #     (observeEvent / eventReactive). Country, Continent, value; 15 rows;
    #     nice column names.

    # 3c. Map (renderPlotly):
    #     ggplot(...) + geom_polygon + scale_fill_viridis_c() ... |> ggplotly()
    #     Tooltip shows country name and the selected variable's value.

    # 3d. Global analysis (renderPlotly x2):
    #     - boxplot over the whole data set
    #     - histogram + density plot over the whole data set

    # 3e. Analysis per continent (renderPlotly x2):
    #     - grouped boxplot (by continent)
    #     - grouped density plot (by continent)

  # --- MULTIVARIATE OUTPUTS -------------------------------------------------

    # 3f. Scatterplot (renderPlotly):
    #     geom_point (colored by continent, sized by pop/area)
    #     + geom_smooth(method = "loess") per continent
    #     |> ggplotly()




# -----------------------------------------------------------------------------
# 4. RUN THE APP
# -----------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
