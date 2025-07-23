# Country Health Analysis Dashboard

An interactive Shiny dashboard for analyzing and visualizing country-level indicators across economic, social, environmental, health, education, development, and energy domains.

## Project Structure

```
Assignment 3/
├── global.R          
├── ui.R              # User interface definition with conditional panels
├── server.R          # Server-side logic and plot generation
├── data/             # Sample data directory
│   ├── indicators_afg.csv  # Afghanistan indicators
│   ├── indicators_alb.csv  # Albania indicators
│   ├── indicators_arm.csv  # Armenia indicators
│   ├── indicators_irl.csv  # Ireland indicators
│   └── indicators_mlt.csv  # Malta indicators
└── README.md         # Project documentation

```

## Requirements

- R 4.0+
- Required R packages:
  - shiny: Web application framework
  - DT: Interactive data tables
  - ggplot2: Data visualization
  - data.table: Fast data manipulation

## Installation

1. Install required packages:
```R
install.packages(c("shiny", "DT", "ggplot2", "data.table"))
``` 

## Running the App

From R or RStudio:
```R
library(shiny)
runApp()
```


