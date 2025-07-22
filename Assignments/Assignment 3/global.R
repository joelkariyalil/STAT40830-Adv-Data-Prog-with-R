# Load required packages
library(shiny)
library(DT)
library(ggplot2)
library(data.table)
library(RColorBrewer)

# Set global options
options(shiny.maxRequestSize = 30 * 1024^2)  # 30MB file upload limit

#' Read and process World Bank indicator data files
#' 
#' @param file_path Path to the CSV file
#' @return data.table with processed indicators
readIndicators <- function(file_path) {
    # Read data, skipping metadata row
    dt <- fread(file_path, skip = 1, header = TRUE)
    
    # Set standardized column names
    setnames(dt, c("Country Name", "Country ISO3", "Year", 
                   "Indicator Name", "Indicator Code", "Value"))
    
    # Convert values to numeric, removing any non-numeric characters
    dt[, Value := as.numeric(gsub("[^0-9.-]", "", Value))]
    
    return(dt)
}

# Load initial datasets
builtInData <- list(
    Afghanistan = readIndicators("data/indicators_afg.csv"),
    Albania = readIndicators("data/indicators_alb.csv"),
    Armenia = readIndicators("data/indicators_arm.csv"),
    Ireland = readIndicators("data/indicators_irl.csv"),
    Malta = readIndicators("data/indicators_mlt.csv")
) 