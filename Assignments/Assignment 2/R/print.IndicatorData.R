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
