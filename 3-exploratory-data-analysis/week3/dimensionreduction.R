# Dimension Reduction -----------------------------------------------------

# Matrix data
set.seed(12345)
par(mar = rep(0.2,4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])

heatmap(dataMatrix)
