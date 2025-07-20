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
