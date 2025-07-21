# Required Libraries
library(shiny)
library(DT)
library(ggplot2)
library(data.table)

# Global Settings
options(shiny.maxRequestSize = 30 * 1024^2)  # Allow up to 30MB file uploads

#' Read and process indicator files
#' @param file_path Path to the CSV file
#' @return data.table with processed indicators
readIndicators <- function(file_path) {
    # Skip metadata row and read data
    dt <- fread(file_path, skip = 1, header = TRUE)
    
    # Set proper column names
    setnames(dt, c("Country Name", "Country ISO3", "Year", 
                   "Indicator Name", "Indicator Code", "Value"))
    
    # Convert values to numeric, handling non-numeric entries
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