# Country Health Analysis Dashboard

An interactive Shiny dashboard for analyzing and visualizing country-level indicators across economic, social, environmental, health, education, development, and energy domains.

## Project Structure

```
Assignment 3/
├── global.R           # Global settings, libraries, and data loading functions
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

1. Clone or download the repository
2. Install required packages:
```R
install.packages(c("shiny", "DT", "ggplot2", "data.table"))
```

## Running the App

From R or RStudio:
```R
library(shiny)
runApp()
```

## Features

### Data Management
- Built-in data for 5 countries (Afghanistan, Albania, Armenia, Ireland, Malta)
- Upload additional country data files
- Interactive data table with customizable columns
- Flexible row display options

### Visualizations

1. Time Series Analysis
   - Track indicators over time
   - Multiple aggregation options (mean, median, sum)
   - Customizable year ranges
   - Interactive plot

2. Top/Bottom Values Analysis
   - View highest/lowest values
   - Top 10 rankings
   - Bar chart visualization
   - Value labels

3. Rolling Average Analysis
   - Compare raw data with trends
   - Adjustable window size
   - Clear trend visualization
   - Dual-line display

### Data Requirements

Sample data files should follow this format:
- CSV format
- Required columns: Country Name, Country ISO3, Year, Indicator Name, Indicator Code, Value
- First row: Column headers
- Second row: Metadata (skipped during import)
- Subsequent rows: Actual data

Example data structure:
```csv
Country Name,Country ISO3,Year,Indicator Name,Indicator Code,Value
#country+name,#country+code,#date+year,#indicator+name,#indicator+code,#indicator+value+num
Afghanistan,AFG,2022,GDP per capita,NY.GDP.PCAP.CD,516.854
```

## Usage Tips

1. Data View:
   - Use the "Rows to display" option to manage large datasets
   - Select specific columns for focused analysis
   - Use the search function for quick data location

2. Plots:
   - Each plot tab shows only relevant controls
   - Adjust year ranges for detailed or broad analysis
   - Use the rolling average for trend analysis
   - Compare extremes with top/bottom values

## Contributing

Feel free to submit issues and enhancement requests!
