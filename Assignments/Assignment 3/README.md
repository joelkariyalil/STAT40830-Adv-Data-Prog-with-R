# Country Health Analysis Dashboard

A Shiny dashboard for analyzing and visualizing country health indicators and other metrics across multiple countries.

## Features

### Data Handling
- Built-in data for multiple countries (Afghanistan, Albania, Armenia, Ireland, Malta)
- Support for uploading additional country data files
- Handles World Bank indicator format CSV files
- Automatic data validation and processing

### Interactive Data Table
- View raw data in tabular format
- Customizable number of rows to display
- Column selection functionality
- Sortable and searchable interface

### Visualization Types

#### 1. Time Series Analysis
Multiple views available for comparing countries:
- **Individual Countries**: Raw values for direct comparison
- **Running Mean (5-year)**: Smoothed trends using 5-year moving average
- **Cumulative Sum**: Running total over time
- **Year-over-Year Change %**: Annual percentage changes

Features:
- Customizable year range
- Multiple color palette options (Default, Set3, Blues)
- Interactive legend
- Responsive to multiple country selection

#### 2. Top/Bottom Values Analysis
- Display top or bottom 10 values
- Compare extremes across selected countries
- Customizable year range
- Multiple color palette options

#### 3. Rolling Average Analysis
- Dynamic window size selection (2-10 years)
- Shows both actual values and rolling averages
- Multiple color palette options
- Dashed lines for rolling averages

## Usage

1. **Country Selection**:
   - Select one or multiple countries from the dropdown
   - Click "Update Plots" to refresh visualizations

2. **Data Upload**:
   - Use the file upload section to add new country data
   - Files must be in World Bank indicator CSV format
   - Filename format: `indicators_COUNTRYCODE.csv`

3. **Visualization Settings**:
   - Choose an indicator from the dropdown
   - Select year range using the slider
   - Pick a color palette
   - Adjust specific settings for each plot type

4. **Data Table View**:
   - Select columns to display
   - Adjust number of rows
   - Use search functionality to filter data

## Data Format Requirements

Input CSV files should follow the World Bank indicator format:
- Required columns: Country Name, Country ISO3, Year, Indicator Name, Indicator Code, Value
- First row should contain metadata (will be skipped during import)
- Values should be numeric (non-numeric values will be cleaned)

## Technical Details

Built using:
- R Shiny for web interface
- data.table for fast data manipulation
- ggplot2 for visualizations
- DT for interactive tables
- RColorBrewer for color palettes

## Error Handling

The application includes robust error handling for:
- File uploads
- Data processing
- Missing values
- Invalid data formats
- Non-numeric values
