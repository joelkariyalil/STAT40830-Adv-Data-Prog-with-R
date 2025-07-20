test_that("plot.indicatorData executes without error", {
    path <- system.file("extdata", "indicators_alb.csv", package = "CountryHealthAnalysis")
    obj <- readIndicators(path, verbose = FALSE)
    
    expect_silent(plot(obj))
    expect_silent(plot(obj, indicatorList = c("Net migration")))
    expect_silent(plot(obj, topN = 5, facet = TRUE))
})
