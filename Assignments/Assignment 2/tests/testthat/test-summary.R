test_that("summary.indicatorData runs correctly with defaults and filters", {
    path <- system.file("extdata", "indicators_alb.csv", package = "CountryHealthAnalysis")
    obj <- readIndicators(path, verbose = FALSE)
    
    expect_silent(summary(obj))
    expect_silent(summary(obj, indicatorList = c("Net migration")))
    expect_silent(summary(obj, topN = 5))
})
