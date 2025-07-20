test_that("readIndicators loads file and assigns correct class", {
    path <- system.file("extdata", "indicators_alb.csv", package = "CountryHealthAnalysis")
    obj <- readIndicators(path, verbose = T)
    
    expect_s3_class(obj, "indicatorData")
    expect_true("Country Name" %in% colnames(obj))
    expect_true("Year" %in% colnames(obj))
    expect_true("Value" %in% colnames(obj))
    expect_type(obj$Year, "integer")
    expect_type(obj$Value, "double")
})
