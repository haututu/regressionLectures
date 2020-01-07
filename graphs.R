library(tidyverse)
library(ggpmisc)

set.seed(1234)

n <- 200

regression_data <- data.frame(
  weight = round(rnorm(n, 70, 14)),
  sex = rbinom(n, 1, 0.5),
  noise = rnorm(n, 0, 5)
  ) %>%
  mutate(height = round(175 + 0.2 * weight + -11 * sex + noise),
         sex = as.factor(sex)
         ) %>%
  select(-noise)

# One continuous predictor
formula <- y ~ x
ggplot(data = regression_data, aes(x = weight, y = height)) +
  geom_smooth(method = "lm", se=FALSE, color="black") +
  stat_poly_eq(formula = formula,
               eq.with.lhs = "italic(y)~`=`~",
               aes(label = paste(..eq.label.., sep = "~~~")), 
               parse = TRUE,
               label.x = "right",
               size = 10) +         
  geom_point() +
  theme_classic() +
  labs(
    x="Weight (x)",
    y="Height (y)"
  ) +
  theme(
    text = element_text(size = 20)
  )