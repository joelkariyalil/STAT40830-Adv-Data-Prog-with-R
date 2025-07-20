#' Read Indicator CSV File
#'
#' Loads a country indicator CSV file, handles formatting quirks, assigns classes,
#' and returns an object of class 'indicatorData'.
#'
#' @param filename Path to the 'indicators_COUNTRYCODE.csv' file.
#' @param verbose Logical; if TRUE (default), prints summary and load time.
#' @return An S3 object of class 'indicatorData'.
#' @export
readIndicators <- function(filename, verbose = TRUE) {
    startTime <- Sys.time()
    
    if (!file.exists(filename)) stop("File not found: ", filename)
    
    dt <- data.table::fread(filename, stringsAsFactors = TRUE)
    
    expectedCols <- c("Country Name", "Country ISO3", "Year",
                      "Indicator Name", "Indicator Code", "Value")
    
    # Only drop row 2 if it contains metadata, not valid numeric Value
    row2Value <- suppressWarnings(as.numeric(dt$Value[2]))
    if (is.na(row2Value)) {
        dt <- dt[-2]
    }
    
    # Validate structure
    if (!all(expectedCols %in% colnames(dt))) {
        stop("Missing columns: ", paste(setdiff(expectedCols, colnames(dt)), collapse = ", "))
    }
    
    # Type casting
    dt$Year <- as.integer(dt$Year)
    dt$Value <- as.numeric(dt$Value)
    dt$'Indicator Name' <- as.factor(dt$'Indicator Name')
    dt$'Indicator Code' <- as.factor(dt$'Indicator Code')
    
    # Assign S3 class
    class(dt) <- c("indicatorData", class(dt))
    
    if (verbose) {
        naCount <- sum(is.na(dt))
        duration <- round(difftime(Sys.time(), startTime, units = "secs"), 3)
        
        stats <- data.frame(
            File = basename(filename),
            Rows = nrow(dt),
            Indicators = length(unique(dt$'Indicator Name')),
            Years = paste(range(dt$Year, na.rm = TRUE), collapse = " - "),
            NAs = naCount,
            Time_sec = as.numeric(duration)
        )
        
        if ("knitr" %in% loadedNamespaces()) {
            suppressWarnings(
                print(knitr::kable(stats, caption = "Indicator Dataset Summary"))
            )
        } else {
            print(stats)
        }
    }
    
    invisible(dt)
}
