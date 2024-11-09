# Load required packages and data
pacman::p_load(shiny, sf, tmap, bslib, tidyverse, sfdep, shinydashboard, shinythemes,
               spdep, ClustGeo, ggpubr, cluster, factoextra, NbClust, heatmaply, 
               corrplot, psych, GGally)

# Load data
vietnam_geo <- readRDS("data/rds/vietnam_geo.rds")
farms <- readRDS("data/rds/farms.rds")
enterprise <- readRDS("data/rds/enterprise.rds")
vietnam_farm <- readRDS("data/rds/vietnam_farm.rds")
vietnam_farm <- st_as_sf(vietnam_farm)
# Reference geospatial data (shapefile)
vietnam_provinces <- st_read(dsn = "data/geospatial/DiaphanTinh", layer = "Dia_phan_Tinh")


farms <- farms %>%
  mutate(across(starts_with("20"), as.numeric)) %>%
  mutate(across(starts_with("20"), ~ replace_na(., 0)))

farms_long <- farms %>%
  pivot_longer(cols = starts_with("20"), names_to = "year_type", values_to = "count")

geospatial_provinces <- unique(vietnam_provinces$Name)

farms <- farms %>%
  filter(`Cities, provincies` %in% geospatial_provinces)

vietnam_provinces <- vietnam_provinces %>%
  rename(province_name = Name)

farms_long <- farms_long %>%
  rename(province_name = `Cities, provincies`) %>%
  filter(!province_name %in% c("WHOLE COUNTRY", "Northern Central area and Central coastal area", 
                               "Northern midlands and mountain areas"))

vietnam_farms <- vietnam_provinces %>%
  left_join(farms_long, by = "province_name")

#========================#
###### Shiny UI ######
#========================#  

ui <- navbarPage(
  title = "VietEcoMap",
  fluid = TRUE,
  theme = shinytheme("flatly"),
  id = "navbarID",
  
  tags$img(
    src = "background.jpg",  # Ensure "background.jpg" is in the www folder
    style = '
      position: fixed;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      z-index: -1;
      opacity: 0.5;
    '
  ),
  # UI EDA ---------------------------------------------------------------------
  tabPanel("EDA",
           fluidRow(
             sidebarLayout(
               sidebarPanel(
                 style = "position: fixed; width: 25%; left: 2%; top: 50%; transform: translateY(-50%);",
                 
                 # # Conditional panel for Type of Farm selection
                 # conditionalPanel(
                 #   condition = "input.eda_tab == 'Farm' || input.eda_tab == 'Enterprise'",
                 #   selectInput("type_of_farm_eda", "Type of Farm", choices = c("Cultivation", "Livestock", "Fishing", "Others"))
                 # ),
                 selectInput("type_of_farm", "Type of Farm", choices = c("Cultivation farm", "Livestock farm", "Fishing farm", "Others(*)")),
                 
                 
                 sliderInput("year", "Year", min = 2012, max = 2023, value = 2012, step = 1),
                 
                 # Conditional input for graph format based on context in EDA tab
                 conditionalPanel(
                   condition = "input.eda_tab == 'Farm'",
                   selectInput("graph_format_eda", "Format of Graph", choices = c("Boxplot", "Line Plot", "Bar Chart"))
                 )
               ),
               
               mainPanel(
                 style = "margin-left:30%;",  # Adjust main panel position relative to the fixed sidebar width
                 
                 # TabsetPanel for Farm and Enterprise views
                 tabsetPanel(
                   id = "eda_tab",  # ID to track the active sub-tab
                   
                   # Farm Tab
                   tabPanel("Farm",
                            h4("EDA Analysis for Farm Data"),
                            p("Explore various economic indicators related to farm types across different years. Adjust the parameters to analyze trends and distributions."),
                            plotOutput("total_farms_map"),  # Total farms map for selected year
                            
                            h4("Boxplot of Selected Farm Type (2012-2023)"),
                            plotOutput("farm_type_map"),  # Map for specific farm type
                            
                            # Additional Outputs for Spatial Analysis
                            h4("Global Moran's I Analysis"),
                            textOutput("global_morans_i"),  # Global Moran's I output as text
                            
                            h4("Local Moran's I (LISA) Map"),
                            plotOutput("lisa_map"),  # LISA map for spatial clusters
                            
                            h4("Cluster Type Map"),
                            plotOutput("cluster_type_map")  # Cluster type classification based on Local Moran's I
                   ),
                   
                   # Enterprise Tab
                   tabPanel("Enterprise",
                            h4("EDA Analysis for Enterprise Data"),
                            p("Analyze enterprise-related indicators and trends over time. Adjust the settings to customize the visualization."),
                            plotOutput("enterprise_plot"),  # Placeholder for enterprise data plot
                            
                            h4("Time Series Analysis of Enterprise Growth (2012-2023)"),
                            plotOutput("enterprise_timeseries"),  # Placeholder for time series plot
                            
                            # Additional Output for Temporal Trends
                            h4("Temporal Trend Map for Farm Counts"),
                            plotOutput("temporal_trend_map")  # Temporal trend map for year-by-year visualization
                   )
                 )
               )
             )
           )
  ),
           
  # UI Segmentation -----------------------------------------------------------
  tabPanel("Segmentation",
           fluidRow(
             sidebarLayout(
               sidebarPanel(
                 style = "position: fixed; width: 25%; left: 2%; top: 50%; transform: translateY(-50%);", 
                 
                 conditionalPanel(
                   condition = "input.segmentation_tab == 'Correlation' || input.segmentation_tab == 'Visuals'",
                   selectInput("type_of_farm", "Type of Farm", choices = c("Cultivation", "Livestock", "Fishing", "Others"))
                 ),
                 
                 # Common Inputs for both Segmentation and SKATER
                 sliderInput("year", "Year", min = 2012, max = 2023, value = 2012, step = 1),
                 
                 conditionalPanel(
                   condition = "input.segmentation_tab == 'Correlation'",
                   selectInput("graph_format", "Format of Graph", choices = c("Histogram", "Density"))
                 ),
                 
                 sliderInput("k_value", "K Value", min = 2, max = 10, value = 6, step = 1),
                 
                 conditionalPanel(
                   condition = "input.segmentation_tab == 'ClustGeo'",
                   sliderInput("alpha", "Alpha (Spatial Constraint)", min = 0.0, max = 1.0, value = 0.2, step = 0.1)
                 )
               ),
               
               mainPanel(
                 style = "margin-left:30%;",  # Adjust main panel position relative to the fixed sidebar width
                 
                 # Tabset with an ID to track the active sub-tab within Segmentation
                 tabsetPanel(
                   id = "segmentation_tab",  # ID to track the sub-tab
                   
                   # Correlation Tab
                   tabPanel("Correlation",
                            h4("Correlation Analysis of Farm Types for the Selected Year"),
                            p("This correlation analysis provides insights into the relationship between different types of farm output in the selected year. Adjust the variable 'Year' to observe how the correlations change over time. The correlation matrix displays the correlation coefficients, where values close to 1 or -1 indicate strong relationships."),
                            plotOutput("correlation_plot"),
                            
                            h4("Standardized Clustering Analysis for the Selected Year and Farm Type"),
                            p("This section shows the distributions of the selected farm type data across different standardization methods (raw values, Min-Max, and Z-score) for the chosen year. Adjust 'Year' and 'Type of Farm' to explore how the data varies under each standardization method."),
                            plotOutput("standardized_clustering"),
                            
                            h4("Agglomerative Coefficient Values"),
                            fluidRow(
                              valueBoxOutput("ac_average", width = 3),
                              valueBoxOutput("ac_single", width = 3),
                              valueBoxOutput("ac_complete", width = 3),
                              valueBoxOutput("ac_ward", width = 3)
                            ),
                            
                            h4("Agglomerative Coefficient and Gap Statistic for Clustering"),
                            p("The agglomerative coefficient and gap statistic help evaluate clustering solutions. Adjust the 'Year' to update the clustering analysis."),
                            plotOutput("agglo_gap_plot"),
                            
                            h4("Cluster Dendrogram with User-Defined Number of Clusters"),
                            p("The dendrogram shows the hierarchical clustering of the data based on the chosen 'Year'. Use the 'K Value' to select the number of clusters to display in the dendrogram."),
                            plotOutput("dendrogram_plot"),
                            
                            h4("Clustered Map of Regions Based on Selected Year and Cluster Count"),
                            p("This map displays the spatial distribution of regions grouped into clusters based on hierarchical clustering. Adjust the 'K Value' slider to change the number of clusters displayed on the map, which dynamically updates according to the clustering analysis."),
                            plotOutput("clustered_map")
                   ),
                   
                   # SKATER Tab
                   tabPanel("SKATER",
                            h4("Choropleth Map of SKATER Spatial Clusters"),
                            p("This choropleth map displays spatial clusters using the SKATER method based on the selected year and the number of clusters derived from the 'K Value m' slider. Adjusting 'Year' and 'K Value' will dynamically update the spatial clusters."),
                            plotOutput("skater_choropleth_map"),
                            
                            h4("Comparison of Hierarchical and SKATER Cluster Maps"),
                            p("The comparison below shows the spatial distribution of clusters generated by two methods: hierarchical clustering and the SKATER method. Use this visualization to assess differences in clustering patterns across methods for the selected year and cluster count."),
                            plotOutput("comparison_map")
                   ),
                   
                   # ClustGeo Tab
                   tabPanel("ClustGeo",
                            h4("Ward-like Hierarchical Clustering with ClustGeo"),
                            p("This clustering analysis uses the Ward-like hierarchical clustering method from the ClustGeo package. Adjust the 'Year' and 'K Value ' to explore different cluster groupings."),
                            plotOutput("clustgeo_dendrogram"),
                            
                            h4("Mapping the Clusters Formed by ClustGeo"),
                            p("This map visualizes the spatial distribution of clusters formed by the ClustGeo hierarchical clustering method. The 'K Value ' slider determines the number of clusters displayed."),
                            plotOutput("clustgeo_map"),
                            
                            h4("Spatially Constrained Hierarchical Clustering"),
                            p("This plot shows the impact of different alpha values on clustering with spatial constraints."),
                            plotOutput("choicealpha_plot"),
                            
                            h4("Mapping Clusters Formed by Spatially Constrained ClustGeo"),
                            p("This map shows the clusters formed with spatial constraints. Use the 'Alpha' slider to adjust the spatial constraint level."),
                            plotOutput("spatially_constrained_map")
                   ),
                   
                   # Visuals Tab
                   tabPanel("Visuals",
                            h4("Visual Interpretation of Clusters"),
                            p("This boxplot shows the distribution of the selected farm type within each cluster. Adjust 'Year', 'K Value', and 'Type of Farm' to observe changes in clustering and distribution."),
                            plotOutput("cluster_boxplot"),
                            
                            h4("Parallel Coordinates Plot for ICT Variables by Cluster"),
                            p("The parallel coordinates plot displays multiple ICT variables across clusters for the selected year. Adjust 'Year' and 'K Value' to examine changes in clustering patterns."),
                            plotOutput("parallel_coordinates_plot")
                   )
                 )
               )
             )
           )
  ),
  
  # Moran tab with multiple sub-tabs ------------------------------------------
  tabPanel("Moran",
           fluidRow(
             sidebarLayout(
               sidebarPanel(
                 style = "position: fixed; width: 25%; left: 2%; top: 50%; transform: translateY(-50%);", 
                 
                 # Common Inputs for Moran
                 selectInput("type_of_farm", "Type of Farm", choices = c("Cultivation farm", "Livestock farm", "Fishing farm", "Others(*)")),
                 sliderInput("year", "Year", min = 2012, max = 2023, value = 2012, step = 1),
                 
                 radioButtons("contiguity_method", "Contiguity Method",
                              c("Queen" = TRUE,
                                "Rook" = FALSE)),
                 
                 # conditionalPanel(
                   # condition = "input.segmentation_tab == 'ClustGeo'",
                   sliderInput("simulations", "No. of Simulations", min = 99, max = 999, value = 99, step = 100)
                 # )
               ),
               
               mainPanel(
                 style = "margin-left:30%;",  # Adjust main panel position relative to the fixed sidebar width
                 
                 # Tabset with an ID to track the active sub-tab within Segmentation
                 tabsetPanel(
                   id = "moran_tab",  # ID to track the sub-tab
                   
                   # Global Moran Tab
                   tabPanel("Global Moran",
                            # h4("Monte Carlo's I of Farm Types for the Selected Year"),
                            # p("This correlation analysis provides insights into the relationship between different types of farm output in the selected year. Adjust the variable 'Year' to observe how the correlations change over time. The correlation matrix displays the correlation coefficients, where values close to 1 or -1 indicate strong relationships."),
                            plotOutput("global_moran_plot"),
                            
                            h4("Monte Carlo's I Stat Window"),
                            # p("This section shows the distributions of the selected farm type data across different standardization methods (raw values, Min-Max, and Z-score) for the chosen year. Adjust 'Year' and 'Type of Farm' to explore how the data varies under each standardization method."),
                            tableOutput("global_moran_stat"),
                   ),
                   
                   # Local Moran Tab
                   tabPanel("Local Moran",
                            # h4("Spatial Autocorrelation for the selected Year"),
                            # p("This choropleth map displays spatial clusters using the SKATER method based on the selected year and the number of clusters derived from the 'K Value m' slider. Adjusting 'Year' and 'K Value' will dynamically update the spatial clusters."),
                            plotOutput("local_moran_map", height = "100vh"),
                   ),
                   
                   # LISA Tab
                   tabPanel("LISA",
                            # h4("Ward-like Hierarchical Clustering with ClustGeo"),
                            # p("This clustering analysis uses the Ward-like hierarchical clustering method from the ClustGeo package. Adjust the 'Year' and 'K Value ' to explore different cluster groupings."),
                            plotOutput("local_moran_lisa", height = "100vh"),
                   ),
                   
                   # Hot/Cold Spot Tab
                   tabPanel("Hot/Cold Spot",
                            # h4("Visual Interpretation of Clusters"),
                            # p("This boxplot shows the distribution of the selected farm type within each cluster. Adjust 'Year', 'K Value', and 'Type of Farm' to observe changes in clustering and distribution."),
                            plotOutput("hotcoldspot_map", height = "100vh")
                   )
                 )
               )
             )
           )
  ),
)



#========================#
###### Shiny Server ######
#========================#  

server <- function(input, output) {
  
  # EDA server functions ------------------------------------------------------
  
  
  output$total_farms_map <- renderPlot({
    selected_year <- as.character(input$year)
    map_data <- vietnam_farms %>%
      filter(year_type == paste(selected_year, "Total")) %>%
      select(province_name, count, geometry)
    
    tm_shape(map_data) +
      tm_polygons("count", title = paste(selected_year, "Total Farms"), palette = "Greens") +
      tm_layout(legend.position = c("right", "bottom"))
  })
  
  ## Plot of Specific Farm Type for a Selected Year ----------------------------
  output$farm_type_map <- renderPlot({
    # Dynamically select farm type based on input
    selected_farm_type <- input$type_of_farm
    col_pattern <- paste0(".*", selected_farm_type, "$")  # Regex pattern to match columns ending with the selected farm type
    
    # Select columns for all years with the specified farm type
    map_data <- vietnam_farm %>%
      pivot_longer(cols = matches(col_pattern), names_to = "year", values_to = "value") %>%
      mutate(year = sub(paste0(" ", selected_farm_type, "$"), "", year))  # Remove farm type suffix to get the year
    
    # Check if the data frame is empty
    if (nrow(map_data) == 0) return(NULL)
    
    # Create boxplot with year as x-axis and values as y-axis
    ggplot(data = map_data, aes(x = as.factor(year), y = value)) +
      geom_boxplot() +
      labs(title = paste("Boxplot of", selected_farm_type, "across Years"),
           x = "Year",
           y = paste("Total number of", selected_farm_type)) +
      theme_minimal()
  })
  
  ## Compare Farms Between Two Years -------------------------------------------
  output$compare_farms_map <- renderPlot({
    year_1 <- "2012"
    year_2 <- "2019"
    
    map_year1 <- vietnam_farms %>%
      left_join(farms_long %>% filter(year_type == paste(year_1, "Total")), by = "province_name")
    map_year2 <- vietnam_farms %>%
      left_join(farms_long %>% filter(year_type == paste(year_2, "Total")), by = "province_name")
    
    # Display maps side by side
    tm1 <- tm_shape(map_year1) +
      tm_polygons("count", title = paste(year_1, "Total Farms"), palette = "Greens") +
      tm_layout(main.title = paste("Total Farms in", year_1), legend.position = c("right", "bottom"))
    
    tm2 <- tm_shape(map_year2) +
      tm_polygons("count", title = paste(year_2, "Total Farms"), palette = "Blues") +
      tm_layout(main.title = paste("Total Farms in", year_2), legend.position = c("right", "bottom"))
    
    tmap_arrange(tm1, tm2, ncol = 2)
  })
  
  ## Global Moran's I ----------------------------------------------------------
  output$global_morans_i <- renderText({
    selected_year <- as.character(input$year)
    map_data <- vietnam_farms %>%
      filter(year_type == paste(selected_year, "Total")) %>%
      select(province_name, count, geometry) %>%
      distinct()
    
    neighbors <- poly2nb(map_data, queen = TRUE)
    weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)
    morans_i <- moran.test(map_data$count, weights, zero.policy = TRUE)
    
    paste("Moran's I:", round(morans_i$estimate[1], 4), 
          "p-value:", round(morans_i$p.value, 4))
  })
  
  ## Local Moran's I (LISA) Map ------------------------------------------------
  output$lisa_map <- renderPlot({
    selected_year <- as.character(input$year)
    map_data <- vietnam_farms %>%
      filter(year_type == paste(selected_year, "Total")) %>%
      select(province_name, count, geometry) %>%
      distinct()
    
    neighbors <- poly2nb(map_data, queen = TRUE)
    weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)
    local_morans <- localmoran(map_data$count, weights, zero.policy = TRUE)
    map_data$local_I <- local_morans[, 1]
    map_data$p_value <- local_morans[, 5]
    
    tm_shape(map_data) +
      tm_polygons("local_I", style = "quantile", title = "Local Moran's I") +
      tm_layout(main.title = "LISA (Local Moran's I) for Farm Counts")
  })
  
  ## Cluster Types Map (High-High, Low-Low, etc.) ------------------------------
  output$cluster_type_map <- renderPlot({
    selected_year <- as.character(input$year)
    map_data <- vietnam_farms %>%
      filter(year_type == paste(selected_year, "Total")) %>%
      select(province_name, count, geometry) %>%
      distinct()
    
    neighbors <- poly2nb(map_data, queen = TRUE)
    weights <- nb2listw(neighbors, style = "W", zero.policy = TRUE)
    local_morans <- localmoran(map_data$count, weights, zero.policy = TRUE)
    map_data$local_I <- local_morans[, 1]
    map_data$p_value <- local_morans[, 5]
    
    map_data <- map_data %>%
      mutate(cluster_type = case_when(
        local_I > 0 & p_value < 0.05 & count > mean(count, na.rm = TRUE) ~ "High-High",
        local_I > 0 & p_value < 0.05 & count < mean(count, na.rm = TRUE) ~ "Low-Low",
        local_I < 0 & p_value < 0.05 & count > mean(count, na.rm = TRUE) ~ "High-Low",
        local_I < 0 & p_value < 0.05 & count < mean(count, na.rm = TRUE) ~ "Low-High",
        TRUE ~ "Not Significant"
      ))
    
    tm_shape(map_data) +
      tm_polygons("cluster_type", palette = c("red", "blue", "orange", "green", "grey"),
                  title = "Cluster Type") +
      tm_layout(main.title = "Cluster Types Based on Local Moran's I", 
                legend.position = c("right", "bottom"))
  })
  
  ## Temporal Trend Map for Farm Counts ----------------------------------------
  output$temporal_trend_map <- renderPlot({
    selected_year <- as.character(input$year)
    map_year <- vietnam_farms %>%
      left_join(farms_long %>% filter(year_type == paste(selected_year, "Total")), 
                by = "province_name")
    
    tm_shape(map_year) +
      tm_polygons("count", title = paste(selected_year, "Total Farms"), palette = "Blues") +
      tm_layout(main.title = paste("Total Farms in", selected_year), legend.position = c("right", "bottom"))
  })
  
  # Segmentation server functions  --------------------------------------------
  
  ## Correlation Analysis -----------------------------------------------------
  output$correlation_plot <- renderPlot({
    # Dynamically select columns based on the chosen year
    selected_year <- input$year
    farm_data <- vietnam_farm %>%
      st_drop_geometry() %>%  
      select_at(vars(matches(paste0(selected_year, " (Cultivation|Livestock|Fishing|Others) PR")))) %>%
      mutate(across(everything(), as.numeric))  
    
    # Rename columns by removing the year prefix
    colnames(farm_data) <- gsub(paste0(selected_year, " "), "", colnames(farm_data))  
    
    # Calculate correlation matrix
    cluster_vars_cor_pr <- cor(farm_data, use = "complete.obs")
    
    # Plot correlation matrix
    corrplot.mixed(cluster_vars_cor_pr,
                   lower = "ellipse", 
                   upper = "number",
                   tl.pos = "lt",
                   diag = "l",
                   tl.col = "black")
    
  })
  
  ## Standardized Clustering Analysis ------------------------------------------
  output$standardized_clustering <- renderPlot({
    selected_year <- input$year
    selected_farm_type <- input$type_of_farm
    selected_format <- input$graph_format  # Get the selected format
    
    # Construct the column name based on the selected year and farm type
    column_name <- paste0(selected_year, " ", selected_farm_type, " PR")
    
    # Prepare data for the selected column
    farm_data <- vietnam_farm %>%
      st_drop_geometry() %>%
      select(all_of(column_name)) %>%
      mutate(across(everything(), as.numeric))
    
    # Conditionally generate plots based on selected format
    if (selected_format == "Histogram") {
      # Raw values plot
      r <- ggplot(farm_data, aes(x = .data[[column_name]])) +
        geom_histogram(bins = 20, color = "black", fill = "light blue") +
        ggtitle("Raw Values (without Standardization)")
      
      # Min-Max Standardization
      vietnam_farm_std_df <- as.data.frame(scale(farm_data, center = FALSE, 
                                                 scale = apply(farm_data, 2, max) - apply(farm_data, 2, min)))
      s <- ggplot(vietnam_farm_std_df, aes(x = .data[[column_name]])) +
        geom_histogram(bins = 20, color = "black", fill = "light blue") +
        ggtitle("Min-Max Standardization")
      
      # Z-score Standardization
      vietnam_farm_z_df <- as.data.frame(scale(farm_data))
      z <- ggplot(vietnam_farm_z_df, aes(x = .data[[column_name]])) +
        geom_histogram(bins = 20, color = "black", fill = "light blue") +
        ggtitle("Z-score Standardization")
      
    } else {
      # Density plots
      r <- ggplot(farm_data, aes(x = .data[[column_name]])) +
        geom_density(color = "black", fill = "light blue") +
        ggtitle("Raw Values (without Standardization)")
      
      # Min-Max Standardization
      vietnam_farm_std_df <- as.data.frame(scale(farm_data, center = FALSE, 
                                                 scale = apply(farm_data, 2, max) - apply(farm_data, 2, min)))
      s <- ggplot(vietnam_farm_std_df, aes(x = .data[[column_name]])) +
        geom_density(color = "black", fill = "light blue") +
        ggtitle("Min-Max Standardization")
      
      # Z-score Standardization
      vietnam_farm_z_df <- as.data.frame(scale(farm_data))
      z <- ggplot(vietnam_farm_z_df, aes(x = .data[[column_name]])) +
        geom_density(color = "black", fill = "light blue") +
        ggtitle("Z-score Standardization")
    }
    
    # Arrange plots side by side
    ggarrange(r, s, z, ncol = 3, nrow = 1)
  })
  
  # Agglomerative Coefficient Values
  ac_values <- reactive({
    selected_year <- input$year
    vietnam_farm_ict <- vietnam_farm %>%
      st_drop_geometry() %>%
      select(matches(paste0("^", selected_year, ".*(PR|farm|Others\\(\\*\\))")))
    
    m <- c("average", "single", "complete", "ward")
    names(m) <- c("average", "single", "complete", "ward")
    
    ac <- function(x) {
      agnes(vietnam_farm_ict, method = x)$ac 
    }
    
    map_dbl(m, ac)  # Returns named vector with values for average, single, complete, ward
  })
  
  ## Display each agglomerative coefficient in a separate valueBox --------------
  output$ac_average <- renderValueBox({
    valueBox(
      format(ac_values()["average"], digits = 4),
      subtitle = "Average",
      color = "blue"
    )
  })
  
  output$ac_single <- renderValueBox({
    valueBox(
      format(ac_values()["single"], digits = 4),
      subtitle = "Single",
      color = "blue"
    )
  })
  
  output$ac_complete <- renderValueBox({
    valueBox(
      format(ac_values()["complete"], digits = 4),
      subtitle = "Complete",
      color = "blue"
    )
  })
  
  output$ac_ward <- renderValueBox({
    valueBox(
      format(ac_values()["ward"], digits = 4),
      subtitle = "Ward",
      color = "blue"
    )
  })
  
  # Agglomerative Coefficient and Gap Statistic Plot
  output$agglo_gap_plot <- renderPlot({
    selected_year <- input$year
    
    # Select columns based on the selected year
    vietnam_farm_ict <- vietnam_farm %>%
      st_drop_geometry() %>%
      select(matches(paste0("^", selected_year, ".*(PR|farm|Others\\(\\*\\))")))
    
    set.seed(12345)
    gap_stat <- clusGap(vietnam_farm_ict, 
                        FUN = hcut,      
                        nstart = 25,     
                        K.max = 10, 
                        B = 50)          
    
    # Plot gap statistic
    fviz_gap_stat(gap_stat)
  })
  
  #Dendrogram
  output$dendrogram_plot <- renderPlot({
    selected_year <- input$year
    k <- input$k_value  # Use selected k value
    
    # Select columns based on the selected year and keep only numeric columns
    vietnam_farm_ict <- vietnam_farm %>%
      st_drop_geometry() %>%
      select(matches(paste0("^", selected_year, ".*(PR|farm|Others\\(\\*\\))"))) %>%
      select_if(is.numeric)
    
    # Calculate distance matrix and hierarchical clustering
    proxmat <- dist(vietnam_farm_ict, method = 'euclidean')
    hclust_ward <- hclust(proxmat, method = 'ward.D')
    
    # Plot dendrogram with k clusters
    plot(hclust_ward, labels = vietnam_farm$`Cities, provincies`, cex = 0.9)
    rect.hclust(hclust_ward, k = k, border = 2:5)
  })
  
  output$clustered_map <- renderPlot({
    selected_year <- input$year
    k <- input$k_value  # Use selected k value
    
    # Select columns based on the selected year and keep only numeric columns
    vietnam_farm_ict <- vietnam_farm %>%
      st_drop_geometry() %>%
      select(matches(paste0("^", selected_year, ".*(PR|farm|Others\\(\\*\\))"))) %>%
      select_if(is.numeric) %>%
      drop_na()  # Remove rows with any missing values
    
    # Check if there are any numeric columns remaining
    if (ncol(vietnam_farm_ict) == 0 || nrow(vietnam_farm_ict) == 0) {
      return(NULL)  # Exit if there are no numeric columns or no rows left to cluster
    }
    
    # Calculate distance matrix and hierarchical clustering
    proxmat <- dist(vietnam_farm_ict, method = 'euclidean')
    hclust_ward <- hclust(proxmat, method = 'ward.D')
    
    # Create clusters and bind them to the spatial data
    groups <- as.factor(cutree(hclust_ward, k = k))
    vietnam_farm_cluster <- cbind(vietnam_farm, as.matrix(groups)) %>%
      rename(`CLUSTER` = `as.matrix.groups.`)
    
    # Plot the clustered map
    qtm(vietnam_farm_cluster, "CLUSTER")
  })
  
  # SKATER Choropleth Map
  output$skater_choropleth_map <- renderPlot({
    selected_year <- input$year
    k <- input$k_value  # Use selected k value
    ncuts <- k - 1  # Calculate ncuts as k - 1
    
    # Prepare spatial and numerical data for SKATER
    vietnam_farm_sp <- as_Spatial(vietnam_farm)
    
    # Neighborhood and cost list
    vietnam_farm.nb <- poly2nb(vietnam_farm_sp)
    coords <- st_coordinates(st_centroid(st_geometry(vietnam_farm)))
    
    # Select columns for the selected year and ensure only numeric values
    vietnam_farm_ict <- vietnam_farm %>%
      st_drop_geometry() %>%
      select(matches(paste0("^", selected_year, ".*(PR|farm|Others\\(\\*\\))"))) %>%
      select_if(is.numeric) %>%
      drop_na()
    
    # Ensure data consistency
    if (ncol(vietnam_farm_ict) == 0 || nrow(vietnam_farm_ict) == 0) {
      return(NULL)
    }
    
    # Create cost list and minimum spanning tree
    lcosts <- nbcosts(vietnam_farm.nb, vietnam_farm_ict)
    vietnam_farm.w <- nb2listw(vietnam_farm.nb, lcosts, style = "B")
    vietnam_farm.mst <- mstree(vietnam_farm.w)
    
    # Apply SKATER clustering
    clust <- spdep::skater(edges = vietnam_farm.mst[, 1:2], 
                           data = vietnam_farm_ict, 
                           method = "euclidean", 
                           ncuts = ncuts)
    
    # Bind SKATER groups to spatial data and rename column
    groups_mat <- as.matrix(clust$groups)
    vietnam_farm_spatialcluster <- cbind(vietnam_farm, as.factor(groups_mat)) %>%
      rename(`SP_CLUSTER` = `as.factor.groups_mat.`)
    
    # Plot SKATER clustered map
    qtm(vietnam_farm_spatialcluster, "SP_CLUSTER")
  })
  
  # Comparison Map: Hierarchical vs SKATER Clustering
  output$comparison_map <- renderPlot({
    selected_year <- input$year
    k <- input$k_value  # Use selected k value
    
    # Select columns based on the selected year and keep only numeric columns
    vietnam_farm_ict <- vietnam_farm %>%
      st_drop_geometry() %>%
      select(matches(paste0("^", selected_year, ".*(PR|farm|Others\\(\\*\\))"))) %>%
      select_if(is.numeric) %>%
      drop_na()  # Remove rows with any missing values
    
    # Check if there are any numeric columns remaining
    if (ncol(vietnam_farm_ict) == 0 || nrow(vietnam_farm_ict) == 0) {
      return(NULL)  # Exit if there are no numeric columns or no rows left to cluster
    }
    
    # Calculate distance matrix and hierarchical clustering
    proxmat <- dist(vietnam_farm_ict, method = 'euclidean')
    hclust_ward <- hclust(proxmat, method = 'ward.D')
    
    # Create hierarchical clusters and bind them to the spatial data
    groups <- as.factor(cutree(hclust_ward, k = k))
    vietnam_farm_cluster <- cbind(vietnam_farm, as.matrix(groups)) %>%
      rename(`CLUSTER` = `as.matrix.groups.`)
    
    # SKATER Clustering
    vietnam_farm_sp <- as_Spatial(vietnam_farm)
    vietnam_farm.nb <- poly2nb(vietnam_farm_sp)
    lcosts <- nbcosts(vietnam_farm.nb, vietnam_farm_ict)
    vietnam_farm.w <- nb2listw(vietnam_farm.nb, lcosts, style = "B")
    vietnam_farm.mst <- mstree(vietnam_farm.w)
    
    clust <- spdep::skater(edges = vietnam_farm.mst[, 1:2], 
                           data = vietnam_farm_ict, 
                           method = "euclidean", 
                           ncuts = k - 1)
    
    # Bind SKATER groups to spatial data and rename column
    groups_mat <- as.matrix(clust$groups)
    vietnam_farm_spatialcluster <- cbind(vietnam_farm, as.factor(groups_mat)) %>%
      rename(`SP_CLUSTER` = `as.factor.groups_mat.`)
    
    # Maps for comparison
    hclust.map <- qtm(vietnam_farm_cluster, "CLUSTER") + tm_borders(alpha = 0.5)
    skater.map <- qtm(vietnam_farm_spatialcluster, "SP_CLUSTER") + tm_borders(alpha = 0.5)
    
    # Arrange maps side-by-side
    tmap_arrange(hclust.map, skater.map, asp = NA, ncol = 2)
  })
  
  output$clustgeo_dendrogram <- renderPlot({
    selected_year <- input$year
    k <- input$k_value  # Use selected k value
    
    # Select columns based on the selected year and keep only numeric columns
    vietnam_farm_ict <- vietnam_farm %>%
      st_drop_geometry() %>%
      select(matches(paste0("^", selected_year, ".*(PR|farm|Others\\(\\*\\))"))) %>%
      select_if(is.numeric) %>%
      drop_na()  # Remove rows with any missing values
    
    # Check if there are any numeric columns remaining
    if (ncol(vietnam_farm_ict) == 0 || nrow(vietnam_farm_ict) == 0) {
      return(NULL)  # Exit if there are no numeric columns or no rows left to cluster
    }
    
    # Calculate distance matrix for clustering
    proxmat <- dist(vietnam_farm_ict, method = 'euclidean')
    
    # Perform Ward-like hierarchical clustering with ClustGeo
    nongeo_cluster <- hclustgeo(proxmat)
    
    # Plot dendrogram with selected number of clusters
    plot(nongeo_cluster, labels = vietnam_farm$`Cities, provincies`, cex = 0.9)
    rect.hclust(nongeo_cluster, k = k, border = 2:5)
  })
  
  output$clustgeo_map <- renderPlot({
    selected_year <- input$year
    k <- input$k_value  # Use selected k value
    
    # Select columns based on the selected year and keep only numeric columns
    vietnam_farm_ict <- vietnam_farm %>%
      st_drop_geometry() %>%
      select(matches(paste0("^", selected_year, ".*(PR|farm|Others\\(\\*\\))"))) %>%
      select_if(is.numeric) %>%
      drop_na()  # Remove rows with any missing values
    
    # Check if there are any numeric columns remaining
    if (ncol(vietnam_farm_ict) == 0 || nrow(vietnam_farm_ict) == 0) {
      return(NULL)  # Exit if there are no numeric columns or no rows left to cluster
    }
    
    # Calculate distance matrix for clustering
    proxmat <- dist(vietnam_farm_ict, method = 'euclidean')
    
    # Perform Ward-like hierarchical clustering with ClustGeo
    nongeo_cluster <- hclustgeo(proxmat)
    
    # Create clusters and bind them to the spatial data
    groups <- as.factor(cutree(nongeo_cluster, k = k))
    vietnam_farm_ngeo_cluster <- cbind(vietnam_farm, as.matrix(groups)) %>%
      rename(`CLUSTER` = `as.matrix.groups.`)
    
    # Plot the clustered map
    qtm(vietnam_farm_ngeo_cluster, "CLUSTER")
  })
  
  output$choicealpha_plot <- renderPlot({
    selected_year <- input$year
    k <- input$k_value
    
    # Select columns based on the selected year and keep only numeric columns
    vietnam_farm_ict <- vietnam_farm %>%
      st_drop_geometry() %>%
      select(matches(paste0("^", selected_year, ".*(PR|farm|Others\\(\\*\\))"))) %>%
      select_if(is.numeric) %>%
      drop_na()  # Remove rows with any missing values
    
    # Check if there are any numeric columns remaining
    if (ncol(vietnam_farm_ict) == 0 || nrow(vietnam_farm_ict) == 0) {
      return(NULL)  # Exit if there are no numeric columns or no rows left to cluster
    }
    
    # Calculate distance matrices
    proxmat <- dist(vietnam_farm_ict, method = 'euclidean')
    dist <- st_distance(vietnam_farm, vietnam_farm)
    distmat <- as.dist(dist)
    
    # Perform choicealpha analysis to explore spatial constraints
    cr <- choicealpha(proxmat, distmat, range.alpha = seq(0, 1, 0.1), K = k, graph = TRUE)
  })
  
  output$spatially_constrained_map <- renderPlot({
    selected_year <- input$year
    k <- input$k_value
    alpha <- input$alpha  # Use the selected alpha value
    
    # Select columns based on the selected year and keep only numeric columns
    vietnam_farm_ict <- vietnam_farm %>%
      st_drop_geometry() %>%
      select(matches(paste0("^", selected_year, ".*(PR|farm|Others\\(\\*\\))"))) %>%
      select_if(is.numeric) %>%
      drop_na()  # Remove rows with any missing values
    
    # Check if there are any numeric columns remaining
    if (ncol(vietnam_farm_ict) == 0 || nrow(vietnam_farm_ict) == 0) {
      return(NULL)  # Exit if there are no numeric columns or no rows left to cluster
    }
    
    # Calculate distance matrices
    proxmat <- dist(vietnam_farm_ict, method = 'euclidean')
    dist <- st_distance(vietnam_farm, vietnam_farm)
    distmat <- as.dist(dist)
    
    # Perform spatially constrained hierarchical clustering with the selected alpha value
    clustG <- hclustgeo(proxmat, distmat, alpha = alpha)
    groups <- as.factor(cutree(clustG, k = k))
    
    # Bind clusters to spatial data and map
    vietnam_farm_Gcluster <- cbind(vietnam_farm, as.matrix(groups)) %>%
      rename(`CLUSTER` = `as.matrix.groups.`)
    
    qtm(vietnam_farm_Gcluster, "CLUSTER")
  })
  
  output$cluster_boxplot <- renderPlot({
    selected_year <- input$year
    selected_farm_type <- input$type_of_farm
    k <- input$k_value
    
    # Select columns based on the selected year and keep only relevant columns
    vietnam_farm_ict <- vietnam_farm %>%
      st_drop_geometry() %>%
      select(matches(paste0("^", selected_year, ".*(PR|farm|Others\\(\\*\\))"))) %>%
      select_if(is.numeric) %>%
      drop_na()  # Remove rows with any missing values
    
    # Check if there are any numeric columns remaining
    if (ncol(vietnam_farm_ict) == 0 || nrow(vietnam_farm_ict) == 0) {
      showNotification("No numeric columns available for clustering", type = "error")
      return(NULL)
    }
    
    # Calculate distance matrix and perform hierarchical clustering
    proxmat <- dist(vietnam_farm_ict, method = 'euclidean')
    nongeo_cluster <- hclustgeo(proxmat)
    groups <- as.factor(cutree(nongeo_cluster, k = k))
    
    # Add cluster information to the dataset
    vietnam_farm_ngeo_cluster <- cbind(vietnam_farm, as.matrix(groups)) %>%
      rename(`CLUSTER` = `as.matrix.groups.`)
    
    # Construct the exact column name for the selected type and year
    y_column <- paste0("X", selected_year, ".", selected_farm_type, ".PR")
    
    # Check if the column exists in the dataset
    if (!y_column %in% colnames(vietnam_farm_ngeo_cluster)) {
      showNotification("Selected column does not exist in the dataset", type = "error")
      return(NULL)
    }
    
    # Create boxplot using the dynamically selected column
    ggplot(data = vietnam_farm_ngeo_cluster, aes(x = CLUSTER, y = .data[[y_column]])) +
      geom_boxplot() +
      labs(title = paste("Boxplot of", selected_farm_type, "by Cluster"),
           x = "Cluster", 
           y = paste(selected_farm_type, "for", selected_year))
  })
  
  output$parallel_coordinates_plot <- renderPlot({
    selected_year <- input$year
    k <- input$k_value
    
    # Calculate the columns based on the selected year
    start_column <- 62 + (selected_year - 2012) * 4
    columns_to_plot <- start_column:(start_column + 3)
    
    # Select columns based on the selected year and keep only numeric columns
    vietnam_farm_ict <- vietnam_farm %>%
      st_drop_geometry() %>%
      select(matches(paste0("^", selected_year, ".*(PR|farm|Others\\(\\*\\))"))) %>%
      select_if(is.numeric) %>%
      drop_na()  # Remove rows with any missing values
    
    # Check if there are any numeric columns remaining
    if (ncol(vietnam_farm_ict) == 0 || nrow(vietnam_farm_ict) == 0) {
      return(NULL)  # Exit if there are no numeric columns or no rows left to cluster
    }
    
    # Calculate distance matrices and perform clustering
    proxmat <- dist(vietnam_farm_ict, method = 'euclidean')
    clustG <- hclustgeo(proxmat)
    groups <- as.factor(cutree(clustG, k = k))
    
    # Add cluster information to the dataset
    vietnam_farm_ngeo_cluster <- cbind(vietnam_farm, as.matrix(groups)) %>%
      rename(`CLUSTER` = `as.matrix.groups.`)
    
    # Construct labels for scale_x_discrete outside of ggplot
    x_labels <- setNames(
      c("Cultivation PR", "Livestock PR", "Fishing PR", "Others PR"),
      paste0("X", selected_year, ".", c("Cultivation.PR", "Livestock.PR", "Fishing.PR", "Others.PR"))
    )
    
    # Parallel coordinates plot using the selected columns for the given year
    ggparcoord(data = vietnam_farm_ngeo_cluster, 
               columns = columns_to_plot, 
               scale = "globalminmax",
               alphaLines = 0.2,
               boxplot = TRUE, 
               title = paste("Multiple Parallel Coordinates Plots of ICT Variables by Cluster (", selected_year, ")", sep = "")) +
      facet_grid(~ CLUSTER) + 
      theme(axis.text.x = element_text(angle = 30)) +
      scale_x_discrete(labels = x_labels)
  })
  
  # Global Moran
  output$global_moran_plot <- renderPlot({
    # Dynamically select columns based on the chosen year
    selected_year <- input$year
    selected_farm_type <- input$type_of_farm
    selected_num_simulation <- input$simulations
    selected_contiguity_method <- input$contiguity_method
    col_name <- paste0(selected_year, " ", selected_farm_type)
    
    farm_data <- vietnam_farm %>%
      select(col_name, geometry)
    
    farm_data_q <- farm_data %>%
      mutate(nb = st_contiguity(geometry, queen = selected_contiguity_method),
             wt = st_weights(nb,
                             style = "W"),
             .before = 1)
    
    farm_data_moran_i_test_res <- global_moran_perm(farm_data_q[[col_name]],
                                                    farm_data_q$nb,
                                                    farm_data_q$wt,
                                                    nsim = selected_num_simulation,
                                                    zero.policy = TRUE, 
                                                    na.action=na.omit)
    
    hist(farm_data_moran_i_test_res$res, 
         freq=TRUE, 
         breaks=20, 
         xlab="Simulated Moran's I")
    
    abline(v=0, 
           col="red") 
  })
    
    
  output$global_moran_stat <- renderTable({
    # Dynamically select columns based on the chosen year
    selected_year <- input$year
    selected_farm_type <- input$type_of_farm
    selected_num_simulation <- input$simulations
    selected_contiguity_method <- input$contiguity_method
    col_name <- paste0(selected_year, " ", selected_farm_type)
    
    farm_data <- vietnam_farm %>%
      select(col_name, geometry)
    
    farm_data_q <- farm_data %>%
      mutate(nb = st_contiguity(geometry, queen = selected_contiguity_method),
             wt = st_weights(nb,
                             style = "W"),
             .before = 1)
    
    farm_data_moran_i_test_res <- global_moran_perm(farm_data_q[[col_name]],
                                                    farm_data_q$nb,
                                                    farm_data_q$wt,
                                                    nsim = selected_num_simulation,
                                                    zero.policy = TRUE, 
                                                    na.action=na.omit)
    
    # Display the full test result as a table
    
    # Extract the required components
    test_method <- farm_data_moran_i_test_res$method
    statistic <- as.numeric(farm_data_moran_i_test_res[['statistic']][['statistic']])
    observed_rank <- as.numeric(farm_data_moran_i_test_res[['parameter']][['observed rank']])
    p_value <- format(as.numeric(farm_data_moran_i_test_res$p.value), scientific = TRUE)
    alternative_hypothesis <- farm_data_moran_i_test_res$alternative
    
    # Extract sample estimates (Expectation, Variance)
    expectation <- as.numeric(farm_data_moran_i_test_res[['estimate']][['Expectation']])
    variance <- as.numeric(farm_data_moran_i_test_res[['estimate']][['Variance']])
    
    # Create the result as a data frame to display in a table
    data.frame(
      Test = c(test_method, 
               paste("Number of simulations = ", observed_rank),
               paste("Statistic = ", statistic, "Observed rank = ", observed_rank, "p-value = ", p_value),
               paste("alternative hypothesis:", alternative_hypothesis)
              ),
      stringsAsFactors = FALSE
    )
    
  })
  
  # Local Moran
  output$local_moran_map <- renderPlot({
    # Dynamically select columns based on the chosen year
    selected_year <- input$year
    selected_farm_type <- input$type_of_farm
    selected_num_simulation <- input$simulations
    selected_contiguity_method <- input$contiguity_method
    col_name <- paste0(selected_year, " ", selected_farm_type)
    
    farm_data <- vietnam_farm %>%
      select(col_name, geometry)
    
    farm_data_q <- farm_data %>%
      mutate(nb = st_contiguity(geometry, queen = selected_contiguity_method),
             wt = st_weights(nb,
                             style = "W"),
             .before = 1)
    
    vietnam_farm_lisa <- farm_data_q %>% 
      mutate(local_moran = local_moran(
        farm_data_q[[col_name]], 
        farm_data_q$nb, 
        farm_data_q$wt, nsim = selected_num_simulation),
        .before = 1) %>%
      unnest(local_moran)
    
    tm_shape(st_as_sf(vietnam_farm_lisa)) +
      tm_fill("ii",
              palette = c("#b7dce9","#e1ecbb","#f5f3a6",
                          "#ec9a64","#d21b1c")) +
      tm_layout(main.title = paste("Spatial Autocorrelation of Vietnam", col_name),
                main.title.position = "center",
                # main.title.size = 1.3,
                # main.title.fontface = "bold",
                legend.title.size = 1,
                legend.text.size = 1,
                frame = TRUE) +
      tm_borders(col = "black", alpha = 0.6) +
      tm_compass(type="8star", text.size = 1.5, size = 3, position=c("RIGHT", "TOP"))
    
  })
  
  # LISA
  output$local_moran_lisa <- renderPlot({
    # Dynamically select columns based on the chosen year
    selected_year <- input$year
    selected_farm_type <- input$type_of_farm
    selected_num_simulation <- input$simulations
    selected_contiguity_method <- input$contiguity_method
    col_name <- paste0(selected_year, " ", selected_farm_type)
    
    farm_data <- vietnam_farm %>%
      select(col_name, geometry)
    
    farm_data_q <- farm_data %>%
      mutate(nb = st_contiguity(geometry, queen = selected_contiguity_method),
             wt = st_weights(nb,
                             style = "W"),
             .before = 1)
    
    vietnam_farm_lisa <- farm_data_q %>% 
      mutate(local_moran = local_moran(
        farm_data_q[[col_name]], 
        farm_data_q$nb, 
        farm_data_q$wt, nsim = selected_num_simulation),
        .before = 1) %>%
      unnest(local_moran)
    
    vietnam_farm_lisa_sig <- vietnam_farm_lisa  %>%
      filter(p_ii_sim < 0.05)
    
    tm_shape(vietnam_farm_lisa) +
      tm_polygons() +
      tm_borders(col = "black", alpha = 0.6)+
      tm_shape(vietnam_farm_lisa_sig)+
      tm_fill("mean", 
              palette = c("#b7dce9","#ec9a64","#e1ecbb", "#d21b1c"),
              title = "LISA class",
              midpoint = NA,
              legend.hist = TRUE, 
              legend.is.portrait = TRUE,
              legend.hist.z = 0.1) +
      tm_borders(col = "black", alpha = 0.6)+
      tm_layout(main.title = paste("Province-Level LISA Map of\n ", col_name),
                main.title.position = "center",
                main.title.size = 1.7,
                main.title.fontface = "bold",
                legend.outside = TRUE,
                legend.outside.position = "right",
                legend.title.size = 1.8,
                legend.text.size = 1.3,
                frame = TRUE) +
      tm_borders(alpha = 0.5) +
      tm_compass(type="8star", text.size = 1.5, size = 2, position=c("RIGHT", "TOP")) +
      tm_scale_bar(position=c("LEFT", "BOTTOM"), text.size=1.2) +
      tm_grid(labels.size = 1,alpha =0.2)
    
  })
  
  # Hot/Cold Spot
  output$hotcoldspot_map <- renderPlot({
    # Dynamically select columns based on the chosen year
    selected_year <- input$year
    selected_farm_type <- input$type_of_farm
    selected_num_simulation <- input$simulations
    selected_contiguity_method <- input$contiguity_method
    col_name <- paste0(selected_year, " ", selected_farm_type)
    
    farm_data <- vietnam_farm %>%
      select(col_name, geometry)
    
    vietnam_farm_wm <- farm_data %>%
      mutate(nb = include_self(st_contiguity(geometry, queen = selected_contiguity_method)),
             wt = st_inverse_distance(nb, geometry,
                                      scale = 1,
                                      alpha = 1),
                                      .before = 1)
    
    vietnam_farm_HCSA <- vietnam_farm_wm %>% 
      mutate(local_Gi_star = local_gstar_perm(vietnam_farm_wm[[col_name]], 
                                              nb, 
                                              vietnam_farm_wm$wt, nsim = selected_num_simulation),
                                              .before = 1) %>%
                                              unnest(local_Gi_star)
    
    tm_shape(vietnam_farm_HCSA) +
      tm_fill("gi_star", 
              palette = c("#57bfc0", "#7977f3","#f8d673","#f8b675","#f67774"),
              title = "Gi*",
              midpoint = 0) +
      tm_borders(col = "black", alpha = 0.6) +
      tm_layout(main.title = paste("Hotspots & Coldspots of\n ", col_name),
                main.title.position = "center",
                main.title.size = 1.5,
                main.title.fontface = "bold",
                legend.title.size = 1,
                legend.text.size = 1,
                frame = TRUE) +
      tm_compass(type="8star", text.size = 1.5, size = 3, position=c("RIGHT", "TOP")) +
      tm_scale_bar(position=c("LEFT", "BOTTOM"), text.size=1.2) +
      tm_grid(labels.size = 1, alpha = 0.2)
    
  })
}

# Launch Shiny app
shinyApp(ui = ui, server = server)