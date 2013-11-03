library(ggplot2)

heights.weights <- read.delim('data/01_heights_weights_genders.csv',
                              header = TRUE, sep=",")
heights <- heights.weights$Height

summary(heights)
mean(heights)
range(heights)
quantile(heights, probs=seq(0, 1, by = 0.20))

c(quantile(heights, probs=0.25), quantile(heights, probs=0.75))
c(quantile(heights, probs=0.025), quantile(heights, probs=0.975))

var(heights)
c(mean(heights) - var(heights), mean(heights) + var(heights))

sd(heights)
c(mean(heights) - sd(heights), mean(heights) + sd(heights))
range(heights)
c(quantile(heights, probs = 0.25), quantile(heights, probs = 0.75))

ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth = 1)
ggplot(heights.weights, aes(x = Height)) + geom_density()
ggplot(heights.weights, aes(x = Height, fill = Gender)) + geom_density()

ggplot(heights.weights, aes(x = Weight, fill = Gender)) +
  geom_density() + facet_grid(Gender ~ .)

ggplot(heights.weights, aes(x = Height, y = Weight, color = Gender)) + geom_point() + geom_smooth()

m <- 0
s <- 1
norm.values <- rnorm(100000, m, s)
cauchy.values <- rcauchy(100000, m, s)
gamma.values <- rgamma(100000, 1, 0.001)
ggplot(data.frame(X = norm.values), aes(x = X)) + geom_density()
ggplot(data.frame(X = cauchy.values), aes(x = X)) + geom_density() + scale_x_continuous(limits=c(-30, 30))
ggplot(data.frame(X = gamma.values), aes(x = X)) + geom_density()