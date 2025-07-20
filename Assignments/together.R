#' Read Indicator CSV File
#'
#' Loads a country indicator CSV file, handles formatting quirks, assigns classes,
#' and returns an object of class `indicatorData`.
#'
#' @param filename Path to the `indicators_COUNTRYCODE.csv` file.
#' @param verbose Logical; if TRUE (default), prints summary and load time.
#' @return An S3 object of class `indicatorData`.
#' @export
readIndicators <- function(filename, verbose = TRUE) {
    startTime <- Sys.time()
    
    if (!file.exists(filename)) stop("File not found: ", filename)
    
    dt <- data.table::fread(filename, stringsAsFactors = TRUE)
    
    expectedCols <- c("Country Name", "Country ISO3", "Year",
                      "Indicator Name", "Indicator Code", "Value")
    
    # Drop second row (metadata row)
    dt <- dt[-2]
    
    # Validate structure
    if (!all(expectedCols %in% colnames(dt))) {
        stop("Missing columns: ", paste(setdiff(expectedCols, colnames(dt)), collapse = ", "))
    }
    
    # Type casting
    dt$Year <- as.integer(dt$Year)
    dt$Value <- as.numeric(dt$Value)
    dt$`Indicator Name` <- as.factor(dt$`Indicator Name`)
    dt$`Indicator Code` <- as.factor(dt$`Indicator Code`)
    
    # Assign S3 class
    class(dt) <- c("indicatorData", class(dt))
    
    if (verbose) {
        naCount <- sum(is.na(dt))
        duration <- round(difftime(Sys.time(), startTime, units = "secs"), 3)
        
        stats <- data.frame(
            File = basename(filename),
            Rows = nrow(dt),
            Indicators = length(unique(dt$`Indicator Name`)),
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





################################################################################






#' Print Method for indicatorData Objects (With Indicator Filter)
#'
#' Prints a summary line and a preview of selected or top indicators.
#'
#' @param x An object of class `indicatorData`.
#' @param indicatorList Optional character vector of indicator names to preview.
#'                      If NULL or empty, will display top 3 indicators.
#' @param ... Ignored.
#' @export
print.indicatorData <- function(x, indicatorList = NULL, ...) {
    country <- unique(x$`Country Name`)
    years <- paste(range(x$Year, na.rm = TRUE), collapse = " - ")
    nIndicators <- length(unique(x$`Indicator Name`))
    
    cat("Indicator Data for:", country, "\n")
    cat("Years:", years, "\n")
    cat("Total Indicators:", nIndicators, "\n\n")
    
    if (is.null(indicatorList) || length(indicatorList) == 0) {
        # Compute top 3 indicators by frequency
        topIndicators <- sort(table(x$`Indicator Name`), decreasing = TRUE)[1:3]
        selectedIndicators <- names(topIndicators)
        cat("Top 3 Indicators by Frequency:\n")
        if ("knitr" %in% loadedNamespaces()) {
            suppressWarnings(
                print(knitr::kable(as.data.frame(topIndicators), col.names = c("Indicator", "Count")))
            )
        } else {
            print(topIndicators)
        }
    } else {
        # Validate provided indicators
        validIndicators <- unique(x$`Indicator Name`)
        invalid <- setdiff(indicatorList, validIndicators)
        if (length(invalid) > 0) {
            warning("Some provided indicators not found: ", paste(invalid, collapse = ", "))
        }
        selectedIndicators <- intersect(indicatorList, validIndicators)
        if (length(selectedIndicators) == 0) {
            stop("No valid indicators provided.")
        }
        cat("Preview for selected indicators:\n")
    }
    
    for (ind in selectedIndicators) {
        sub <- subset(x, `Indicator Name` == ind)
        preview <- utils::head(sub[order(sub$Year), ], 5)
        cat("\nIndicator:", ind, "\n")
        if ("knitr" %in% loadedNamespaces()) {
            suppressWarnings(
                print(knitr::kable(preview, caption = paste("Preview for", ind)))
            )
        } else {
            print(preview)
        }
    }
    
    invisible(x)
}








############################################################################






#' Summary Method for indicatorData Objects (with Indicator List and topN)
#'
#' Provides descriptive statistics (min, max, mean, median, sd, NA count)
#' for selected indicators or top N most frequent indicators.
#'
#' @param object An object of class `indicatorData`.
#' @param indicatorList Optional character vector of indicator names to summarize.
#'                      If NULL (default), summarizes the top `topN` indicators by frequency.
#' @param topN Integer; how many top indicators to show if `indicatorList` is NULL. Default is 7.
#' @param ... Ignored.
#' @export
summary.indicatorData <- function(object, indicatorList = NULL, topN = 7, ...) {
    country <- unique(object$`Country Name`)
    indicators <- unique(object$`Indicator Name`)
    yearRange <- range(object$Year, na.rm = TRUE)
    
    cat("Summary for", country, "\n")
    cat("Number of indicators:", length(indicators), "\n")
    cat("Year range:", paste(yearRange, collapse = " - "), "\n\n")
    
    if (is.null(indicatorList) || length(indicatorList) == 0) {
        topCounts <- sort(table(object$`Indicator Name`), decreasing = TRUE)[1:topN]
        selectedIndicators <- names(topCounts)
        cat(paste0("Top ", topN, " most frequent indicators:\n"))
        if ("knitr" %in% loadedNamespaces()) {
            suppressWarnings(
                print(knitr::kable(as.data.frame(topCounts), col.names = c("Indicator", "Count")))
            )
        } else {
            print(topCounts)
        }
    } else {
        invalid <- setdiff(indicatorList, indicators)
        if (length(invalid) > 0) {
            warning("Some provided indicators not found: ", paste(invalid, collapse = ", "))
        }
        selectedIndicators <- intersect(indicatorList, indicators)
        if (length(selectedIndicators) == 0) {
            stop("No valid indicators provided.")
        }
        cat("Summary for selected indicators:\n")
    }
    
    # Compute descriptive statistics
    statTable <- lapply(selectedIndicators, function(ind) {
        vals <- object$Value[object$`Indicator Name` == ind]
        data.frame(
            Indicator = ind,
            Min = min(vals, na.rm = TRUE),
            Max = max(vals, na.rm = TRUE),
            Mean = mean(vals, na.rm = TRUE),
            Median = median(vals, na.rm = TRUE),
            SD = sd(vals, na.rm = TRUE),
            NAs = sum(is.na(vals)),
            stringsAsFactors = FALSE
        )
    })
    statTable <- do.call(rbind, statTable)
    
    cat("\nDescriptive statistics:\n")
    if ("knitr" %in% loadedNamespaces()) {
        suppressWarnings(
            print(knitr::kable(statTable, digits = 2, caption = "Descriptive Statistics for Indicators"))
        )
    } else {
        print(statTable)
    }
    
    invisible(object)
}





#' Plot Method for indicatorData Objects
#'
#' Plots average annual values for top N or specified indicators.
#'
#' @param x An object of class `indicatorData`.
#' @param indicatorList Optional vector of indicator names to plot.
#' @param topN Number of top indicators to plot if no list is provided. Default is 3.
#' @param facet Logical; if TRUE, use facetting instead of overlaying lines. Default is FALSE.
#' @param ... Additional graphical parameters (currently ignored).
#' @export
plot.indicatorData <- function(x, indicatorList = NULL, topN = 3, facet = FALSE, ...) {
    library(ggplot2)
    library(dplyr)
    
    # Pick indicators
    if (is.null(indicatorList)) {
        topIndicators <- x %>%
            count(`Indicator Name`, sort = TRUE) %>%
            slice_head(n = topN) %>%
            pull(`Indicator Name`)
    } else {
        available <- unique(x$`Indicator Name`)
        missing <- setdiff(indicatorList, available)
        if (length(missing) > 0) {
            warning("Some indicators not found: ", paste(missing, collapse = ", "))
        }
        topIndicators <- intersect(indicatorList, available)
    }
    
    filtered <- x %>%
        filter(`Indicator Name` %in% topIndicators)
    
    # Clean country name for title
    country <- unique(filtered$`Country Name`)
    if (length(country) > 1) {
        warning("Multiple countries found in data; using the first for title.")
        country <- country[1]
    }
    
    # Summarize for plotting
    summaryData <- filtered %>%
        group_by(Year, `Indicator Name`) %>%
        summarise(
            mean = mean(Value, na.rm = TRUE),
            ymin = min(Value, na.rm = TRUE),
            ymax = max(Value, na.rm = TRUE),
            .groups = "drop"
        )
    
    # Plot
    p <- ggplot(summaryData, aes(x = Year, y = mean, color = `Indicator Name`)) +
        geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = `Indicator Name`), alpha = 0.2, color = NA) +
        geom_line(linewidth = 1.2) +
        labs(
            title = paste("Indicator Trends for", country),
            y = "Average Value",
            x = "Year"
        ) +
        theme_minimal()
    
    if (facet) {
        p <- p + facet_wrap(~ `Indicator Name`, scales = "free_y") +
            theme(legend.position = "none")
    }
    
    print(p)
    invisible(x)
}





sample = readIndicators("indicators_alb.csv")
# 
# print(sample)
# print(sample, "Net migration")

summary(sample)

summary(sample, "Net migration")


# Plot top 3 indicators (default)
plot(sample)

# Plot custom indicators
plot(sample, indicatorList = c("Net migration", "Population, total"))

# Plot top 5 with one panel per indicator
plot(sample, topN = 5, facet = TRUE)

