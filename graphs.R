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

levels(regression_data$sex) <- c("Male", "Female")

# One continuous predictor
formula <- y ~ x
regression_data %>%
ggplot(aes(x = weight, y = height)) +
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

# Two continuous predictors
nzhs <- readRDS("nzhs2017.RDS")

summary(lm("k10 ~ age + nzdep", nzhs))$coefficients %>%
  round(., 4)

# One continuous and one categorical
regression_data %>%
  ggplot(aes(x = weight, y = height, group=sex, color=sex)) +
  geom_smooth(method = "lm", se=FALSE, aes(linetype=sex)) +
  geom_point(aes(shape=sex)) +
  theme_classic() +
  labs(
    x="Weight (x)",
    y="Height (y)",
    color="Sex",
    linetype="Sex",
    shape="Sex"
  ) +
  theme(
    text = element_text(size = 20)
  )

summary(lm("height ~ sex + weight", regression_data))$coefficients %>%
  round(., 4)
