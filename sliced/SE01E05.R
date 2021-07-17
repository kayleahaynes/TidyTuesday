
# set up ------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(INLA)
library(tidytext)
library(textrecipes)
library(stopwords)

theme_set(theme_light())

# load data  --------------------------------------------------------------

train <- read_csv("~/Documents/Learning and development/sliced-s01e05-WXx7h8/train.csv")
test <- read_csv("~/Documents/Learning and development/sliced-s01e05-WXx7h8/test.csv")

# some EDA plots

train %>%
  ggplot() +
  geom_histogram(aes(x = log(price)))

train %>%
  ggplot() +
  geom_bar(aes(x = neighbourhood_group))

train %>%
  ggplot(aes(x = log(price), y = fct_reorder(neighbourhood_group, price, median))) +
  geom_boxplot()

# split the data
split <- initial_split(train, 0.75, strata = neighbourhood_group)

holdout <- testing(split)
model_data <- training(split)

holdout %>%
  ggplot() +
  geom_bar(aes(x = neighbourhood_group))

model_data %>%
  ggplot() +
  geom_bar(aes(x = neighbourhood_group))

model_data %>%
  ggplot() +
  geom_point(aes(x = longitude, y = latitude, col = log(price)))

model_data$price <- log(model_data$price + 1)

# spatial interpolation model using INLA ----------------------------------

# 1. Create a mesh to approximate the spatial effect

coords <- data.frame(model_data)[,c('longitude', 'latitude')]
mesh <- inla.mesh.2d(loc = coords,
                     max.edge = c(0.5, 0.1),
                     cutoff = 0.01)
plot(mesh)
points(coords, col = "red")

# 2. Create a projection matrix to link the observations to the mesh

Amat <- inla.spde.make.A(mesh, loc = as.matrix(coords))

# 3. Set the stochastic partial differential equation

spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)
indexs <- inla.spde.make.index("s", spde$n.spde)

# stack for estimation stk.e
stk.e <- inla.stack(
  tag = "est",
  data = list(y = model_data$price),
  A = list(1, Amat),
  effects = list(data.frame(b0 = 1, room_type = model_data$room_type,
                            minimum_nights = model_data$minimum_nights,
                            number_of_reviews = model_data$number_of_reviews,
                            availability_365 = model_data$availability_365), s = indexs)
)

# stack for prediction stk.p
coords_test <- data.frame(holdout)[,c('longitude', 'latitude')]
Ap <- inla.spde.make.A(mesh = mesh, loc = as.matrix(coords_test))

stk.p <- inla.stack(
  tag = "pred",
  data = list(y = NA),
  A = list(1, Ap),
  effects = list(data.frame(b0 = 1, room_type = holdout$room_type,
                            minimum_nights = holdout$minimum_nights,
                            number_of_reviews = holdout$number_of_reviews,
                            availability_365 = holdout$availability_365),
                 s = indexs
  )
)

# stk.full has stk.e and stk.p
stk.full <- inla.stack(stk.e, stk.p)

# fit the model
# put all the stacks together
formula <- y ~ 0 + b0 + room_type + minimum_nights + number_of_reviews + availability_365 + f(s, model = spde)

m_inla <- inla(formula,
               data = inla.stack.data(stk.full),
               control.predictor = list(A = inla.stack.A(stk.full), compute = TRUE),
               quantiles = NULL)

index_est <- inla.stack.index(stack = stk.full, tag = "est")$data
model_data$inla_pred <- m_inla$summary.fitted.values[index_est, "mean"]

index_pred <- inla.stack.index(stack = stk.full, tag = "pred")$data
holdout$inla_pred <- m_inla$summary.fitted.values[index_pred, "mean"]

model_data %>%
  ggplot() +
  geom_point(aes(x = price, y = inla_pred)) +
  theme_minimal() +
  xlab("Final Price") +
  ylab("Predicted")

holdout$price <- log(holdout$price + 1)

holdout %>%
  ggplot() +
    geom_point(aes(x = price, y = inla_pred)) +
  theme_minimal() +
  xlab("Final Price") +
  ylab("Predicted")

holdout$price <- log(holdout$price + 1)

truth <- exp(holdout$price)
estimate <- exp(holdout$inla_pred)

sqrt(mean((log(truth + 1) - log(estimate + 1))^2))

# predict on the test set -------------------------------------------------

# stack for prediction stk.p
coords_test <- data.frame(test)[,c('longitude', 'latitude')]
Ap <- inla.spde.make.A(mesh = mesh, loc = as.matrix(coords_test))

stk.p <- inla.stack(
  tag = "pred",
  data = list(y = NA),
  A = list(1, Ap),
  effects = list(data.frame(b0 = 1, room_type = test$room_type,
                            minimum_nights = test$minimum_nights,
                            number_of_reviews = test$number_of_reviews,
                            availability_365 = test$availability_365),
                 s = indexs
  )
)

# stk.full has stk.e and stk.p
stk.full <- inla.stack(stk.e, stk.p)

# fit the model
# put all the stacks together
formula <- y ~ 0 + b0 + room_type + minimum_nights + number_of_reviews + availability_365 + f(s, model = spde)

m_inla <- inla(formula,
               data = inla.stack.data(stk.full),
               control.predictor = list(A = inla.stack.A(stk.full), compute = TRUE),
               quantiles = NULL)

index_est <- inla.stack.index(stack = stk.full, tag = "est")$data
model_data$inla_pred <- m_inla$summary.fitted.values[index_est, "mean"]

index_pred <- inla.stack.index(stack = stk.full, tag = "pred")$data
test$price <- exp(m_inla$summary.fitted.values[index_pred, "mean"])

test %>%
  select(id, price) %>%
  write_csv("inla_pred.csv")
