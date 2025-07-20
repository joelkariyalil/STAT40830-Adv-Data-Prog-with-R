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
