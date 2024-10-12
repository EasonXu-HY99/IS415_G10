# Geospatial Analytics Project

## Project Title: 
**Vietnam Economic Development (2019-2023)**

## Project Description
This project aims to develop a web-based geospatial analytics tool to explore, analyze, and visualize spatial data related to Vietnam’s economic development from 2019 to 2023. We utilize open-source web mapping APIs, data visualization APIs, and geospatial analysis libraries to analyze how Vietnam’s economy has evolved, providing deeper insights into growth patterns and regional disparities.

### Project Motivation
The project explores the **economic growth and regional disparities** in Vietnam from 2019 to 2023, with the potential to understand future impacts of infrastructure developments like Cambodia's China-funded canal. By leveraging spatial data analysis techniques, the project seeks to provide insights into the country's economic development and identify trends that may shape its future.

## Objectives
- To apply geospatial analytics techniques such as **Spatial Point Patterns Analysis** and **Exploratory Spatial Data Analysis (ESDA)**.
- To develop an interactive web application for exploring and visualizing economic and spatial data.
- To interpret the spatial relationships and economic trends across Vietnam from 2019 to 2023.
- To provide a comprehensive tool for analyzing and interacting with geospatial economic data.

## Features
- **Interactive Map**: A web-based interactive map allowing users to visualize economic development (e.g., GDP, trade, FDI) across Vietnam’s provinces from 2019 to 2023.
- **Data Exploration**: Users can filter, search, and explore datasets based on parameters such as time, economic indicator, and location.
- **Statistical Analysis**: Includes spatial clustering, geographically weighted regression models, and exploratory spatial data analysis to support in-depth analysis.
- **Visualizations**: Heat maps, bar charts, and scatter plots provide easy-to-understand representations of economic trends.

## Tools and Technologies
- **Web Mapping API(s)**: Leaflet.js for interactive mapping.
- **Data Visualization API(s)**: Plotly.js and D3.js for dynamic data visualizations.
- **Geospatial Libraries**: `sf`, `tidyverse`, `spdep` for geospatial data wrangling and analysis in R.
- **Backend**: R and Shiny for building the interactive web application.
- **Frontend**: Quarto for creating and publishing the project website on Netlify.
- **Hosting**: Netlify for website hosting.
- **Version Control**: GitHub for version control and collaboration.

## Data Sources
- **Primary Dataset**: Vietnam’s economic indicators (GDP, trade, FDI) from the **World Bank** and **Asian Development Bank (ADB)**.
- **Additional Datasets**: Environmental data from the **Mekong River Commission (MRC)** and geopolitical data from **ACLED**, focusing on cross-border impacts and developments like the Cambodia-China canal.

## Setup Instructions
1. **Clone the repository**:
    ```bash
    git clone https://github.com/your-username/geospatial-analytics-project.git
    ```
2. **Install required R packages**:
    ```r
    install.packages(c("sf", "tidyverse", "shiny", "leaflet", "spdep", "quarto"))
    ```
3. **Run the Shiny Application**:
    ```r
    # Navigate to the shiny_app directory
    setwd("shiny_app")
    shiny::runApp()
    ```
4. **View the Quarto Project Website**:
    - Access the published website at **[Netlify link]**.

## Contributions
We welcome contributions from the community! Please follow these steps:
1. Fork the repository.
2. Create a new branch (`git checkout -b feature-branch`).
3. Commit your changes (`git commit -m 'Add new feature'`).
4. Push to the branch (`git push origin feature-branch`).
5. Open a pull request.

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.


## Team Members
- **Xu Haiyang** - Role: [Project Manager/Data Analyst/Developer]
- **Ming Yan** - Role: [Frontend Developer/Geospatial Analyst/Visualization Specialist]
- **Javier** - Role: [Backend Developer/ShinyApp Developer]
