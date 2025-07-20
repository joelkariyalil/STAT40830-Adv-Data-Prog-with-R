test_that("print.indicatorData runs silently with default and custom indicators", {
    path <- system.file("extdata", "indicators_alb.csv", package = "CountryHealthAnalysis")
    obj <- readIndicators(path, verbose = FALSE)
    
    expect_silent(print(obj))
    expect_silent(print(obj, indicatorList = c("Net migration")))
})
