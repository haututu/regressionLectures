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

########## One continuous predictor
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
nzhs <- readRDS("data/nzhs2017.RDS")

summary(lm("k10 ~ age + nzdep", nzhs))$coefficients %>%
  round(., 4)

summary(lm("k10 ~ age + nzdep", nzhs))

########## One continuous and one categorical
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

summary(lm("height ~ sex + weight", regression_data))

########## Interaction between continous and categorical variable
summary(lm("k10 ~ age * nzdep", nzhs))$coefficients %>%
  round(., 4)

nzhs %>%
  ggplot(aes(x = age, y = k10, group=sex, color=sex)) +
  geom_smooth(method = "lm", se=FALSE, aes(linetype=sex)) +
  theme_classic() +
  labs(
    x="Age (years)",
    y="Stress",
    color="Sex",
    linetype="Sex"
  ) +
  theme(
    text = element_text(size = 20)
  )

summary(lm("k10 ~ age * sex", nzhs))$coefficients %>%
  round(., 4)

########## Interaction between continuous variables
interaction_data <- readRDS("../../../socialMediaMood/dataForBen.RDS") %>%
  select(id, day, sn, fomo, negative) %>%
  group_by(id, day) %>%
  summarise_all(mean) %>%
  ungroup() %>%
  mutate(fomo_group = case_when(
    fomo < -1 ~ "Low (-1SD)",
    fomo > 1 ~ "High (+1SD)",
    TRUE ~ "Average (0SD)"
    ),
    fomo_group = ordered(fomo_group, levels=c("High (+1SD)", "Average (0SD)", "Low (-1SD)"))
    )

interaction_data %>%
  ggplot(aes(x=sn, y=negative, group=fomo_group, color=fomo_group)) +
  geom_smooth(method = "lm", se=FALSE, aes(linetype=fomo_group)) +
  theme_classic() +
  labs(
    x="Social Network Use",
    y="Negative mood",
    color="FoMO",
    linetype="FoMO"
  ) +
  theme(
    text = element_text(size = 20)
  ) +
  scale_color_discrete()

summary(lm("negative ~ sn * fomo", interaction_data))$coefficients %>%
  round(., 4)

########## Drug study
dat <- readxl::read_xlsx("data/drug_data.xlsx") %>%
  janitor::clean_names()

library(lavaan)

cfa.memory <- cfa(
  paste("memory =~", paste(select(dat, contains("cvlt")) %>% colnames(), collapse=" + ")),
  data = dat
  )

