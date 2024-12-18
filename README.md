# **Geospatial Analytics Project**

Welcome to our geospatial analytics project website. This project focuses on Vietnam and applies **Analytical Regionalisation** and **Geographical Segmentation** to investigate the country’s economic development from 2012 to 2023 using geospatial data and web-based tools.

### **Motivation**

Our project is motivated by Vietnam's remarkable farm economic growth within the ASEAN region, particularly its strong ties with countries like Singapore, which serve as major trading partners. Understanding the spatial dynamics of Vietnam’s farm economic development over the past decade is crucial for assessing regional disparities, planning future investments, and promoting balanced growth. Our analysis will provide valuable insights into how provinces differ in their agricultural and enterprise performance, contributing to a deeper understanding of Vietnam's regional economic structure.

### **Objectives**

-   To apply **Analytical Regionalisation** and **Geographical Segmentation** to explore economic development trends across Vietnam’s provinces.

-   To build an interactive web application that facilitates data exploration and visualisation of provincial farm economic trends.

-   To provide actionable insights for policymakers, economists, and investors on regional economic growth and disparities in Vietnam.

### **Approach**

Our approach involves leveraging open-source geospatial tools and datasets to analyse Vietnam's economic development. Here's an overview of the steps we are taking:

#### **Data Sources**

-   **Primary Dataset**: Economic data from the **General Statistics Office** ([GSO](https://www.gso.gov.vn/en/homepage/)), including the "Number of farms by kinds of economic activity and by province," "Number of newly established enterprises by province," and "Index of Industrial production by province."

-   **Geospatial Dataset**: Vietnam’s provincial boundaries sourced from **Open Development Mekong** ([dataset link](https://data.opendevelopmentmekong.net/dataset/a-phn-tnh)).

#### **Tools and Technologies**

-   **Web Mapping API**: Leaflet.js for interactive geospatial mapping.

-   **Data Visualization**: Plotly.js for dynamic data charts and graphs.

-   **Geospatial Analysis Libraries**: `sf` and `spdep` for spatial data wrangling and analysis in R.

-   **Web Framework**: Quarto for generating the project website and Shiny for the interactive web application.

### **Expected Outcomes**

-   **Regional Economic Segmentation**: We will generate maps and analyses that classify Vietnam’s provinces into distinct economic regions based on their performance in agriculture, industry, and enterprise development.

-   **Interactive Data Exploration**: Users can filter by year, region, and economic indicators, allowing for dynamic exploration of Vietnam's farm economic growth from 2012 to 2023.

### **Challenges and Future Work**

While we expect to achieve meaningful results, several challenges include:

-   **Data Consistency**: Variability in data quality and coverage across provinces may affect the robustness of our segmentation and regression models.

-   **Scalability**: Processing and displaying large geospatial datasets in an interactive web application could present performance challenges that require optimization.

In the future, we aim to expand the analysis by incorporating additional economic and environmental datasets and exploring more advanced geospatial methods such as flow and movement data analytics.
