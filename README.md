# Income Mapping application

The idea of this project is to test the capabilities of Shiny by creating an application with a real-word data set of our choosing.

# Dependencies

The app make use of the following packages: 
* Shiny
* Shinyjs
* Leaflet
* RColorBrewer
* tidyverse
* plotly
* DT
* magrittr
* rmarkdown
* kaggler for R

# Introduction

The chosen data can be found [here](https://www.kaggle.com/goldenoakresearch/us-household-income-stats-geo-locations) and its retrieval was possible thanks to the adaptation of the Kaggler library for R performed [here](https://github.com/bernardo-dauria/kaggler). 

Data is composed by numerous attributes related to the income of U.S cities. Theres stpatial information such as location, mean income, standard deviations, water and land proportions, etc. Every time the app is executed the data is directly loaded from Kaggle leaving no trace in the computer. As a result of this, if the data in its original Kaggle repository is updated, so will the application.

The application is meant to be a tool for exploring the different incomes throughout the United States and in no way the attempt is to perform a thorough statistical analysis of the different possible locations.

# Functionalities

The layour of the app is simple, with a navigation bar with only two options: map and data.

Map is the principal page, where a map of the US is displayed along with bubbles indicating the areas with data and a color based on an income gradient. Using the options found on the right side of the screen, it is possible to filter by state, income or county. Also, it is possible to download the raw selected data as a csv file or an statistical analysis in pdf of the selection.

On the other hand, Data shows different plots with information about the data selected in Map can be seen as well as a table displaying it.

