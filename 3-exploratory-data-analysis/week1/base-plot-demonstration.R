set.seed(1)

## Histogram
x <- rnorm(100)
hist(x)

## Scatterplot
y <- rnorm(100)
z <- rnorm(100)
plot(x, y)
plot(x, z)

## Margin parameters (mar)
par("mar") # defaults 5.1 4.1 4.1 2.1
par(mar = c(2, 2, 2, 2))
plot(x, y)
par(mar = c(4, 4, 2, 2))
plot(x, y)


## Plot characters (pch)
par("pch") # default 1
plot(x, y)
plot(x, y, pch = 20)
plot(x, y, pch = 19)
plot(x, y, pch = 2)
plot(x, y, pch = 3)
plot(x, y, pch = 4)

example(points) # runs through point examples

## Specify some options when making plots
plot(x, y, pch = 20)
title("Scatterplot")
text(-2, -2, "label")
legend("topleft", legend = "Data", pch = 20)
fit <- lm(y ~ x)
abline(fit, lwd = 3)
