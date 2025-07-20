install.packages("CountryHealthAnalysis_1.0.0.tar.gz", repos = NULL, type = "source")
library(CountryHealthAnalysis)


sample = readIndicators("indicators_alb.csv",F)
# 
print(sample)
print(sample, "Net migration")

summary(sample)

summary(sample, "Net migration")


# Plot top 3 indicators (default)
plot(sample)

# Plot custom indicators
plot(sample, indicatorList = c("Net migration", "Population, total"))

# Plot top 5 with one panel per indicator
plot(sample, topN = 5, facet = TRUE)

