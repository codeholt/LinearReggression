library(tidyverse)

library(broom)
y <-rnorm(10)
x <-1:10
mod <- lm(mpg ~ cyl,data=mtcars)
df <- augment(mod)
ggplot(df, aes(x = .fitted, y = .resid)) + geom_point()

(15.38^2)*48



