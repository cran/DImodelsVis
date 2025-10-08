## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  fig.width = 5,
  fig.height = 5,
  fig.align = "center"
)

## ----setup, message = FALSE---------------------------------------------------
library(DImodelsVis)
library(dplyr)
library(ggplot2)
library(nnet)
library(randomForest)

## ----simulate_data------------------------------------------------------------
# For reproducability
set.seed(123)

# Number of samples
n <- 200

# Simulate 3d compositional data
# Assume equal density for each variable
alpha <- c(1, 1, 1)
X <- matrix(rgamma(n * length(alpha), shape = alpha), ncol = length(alpha), byrow = TRUE)
X <- X / rowSums(X)  # normalize to sum to 1
colnames(X) <- c("Sand", "Silt", "Clay")

# Simulate Soil organic carbon (SOC) to be used as a response
# We assume, SOC is higher for higher clay content, lower for sand, moderate for silt
SOC <- 4 + 5*X[, "Clay"] - 2*X[, "Sand"] + 1*X[, "Silt"] + rnorm(n, 0, 0.5)

# Combine the compositional predictors and continuous response into data frame
soil_data <- data.frame(X, SOC)

# Snippet of data
head(soil_data)

## ----raw_data, fig.alt = "Plot of raw data in ternary"------------------------
ternary_plot(
  # Compositional variables
  prop = c("Sand", "Silt", "Clay"),
  # Labels for ternary axes
  tern_labels = c("Sand", "Silt", "Clay"),
  # Data with compositional variables 
  data = soil_data, 
  # Show raw data as points instead of a contour map
  show = "points",
  # Colour points by SOC variable
  col_var = "SOC")

## ----nn_model-----------------------------------------------------------------
# Seed to ensure weight initialisation is reproducible
set.seed(737)

nn_model <- nnet(SOC ~ Sand + Silt + Clay,
                 data = soil_data, 
                 size = 7,            # Seven nodes in hidden layer
                 decay = 0.01,        # Parameter for weight decay (helps stabilise predictions)
                 linout = TRUE,       # Boolean to indicate continuous predictions
                 maxit = 1000)        # Number of iterations
nn_model

## ----data_nn_model------------------------------------------------------------
# Prepare data
ternary_grid <- ternary_data(# Compositional predictors
                             prop = c("Sand", "Silt", "Clay"),   
                             # Don't make predictions now and only return template
                             prediction = FALSE) 

head(ternary_grid)

## ----predict_nn_model---------------------------------------------------------
# Add the predicted response to data
plot_data <- add_prediction(# Grid data from ternary_data() function
                            data = ternary_grid,
                            # Model to use for making predictions
                            model = nn_model,
                            # No uncertainty interval added to data
                            interval = "none")

# The predicted response is added as a column called .Pred
head(plot_data)

## ----visualise_nn_model, fig.alt = "Ternary diagram showing SOC"--------------
# Create ternary plot
ternary_plot(data = plot_data,      # Compositional data with predicted response
             lower_lim = 1.5,       # Lower limit of fill scale
             upper_lim = 9.5,       # Upper limit of fill scale
             nlevels = 8,           # Number of colours for fill scale
             # Labels for ternary axes
             tern_labels = c("Sand", "Silt", "Clay")) +
  # ggplot function to add labels on plot
  labs(fill = "Predicted\nSOC")

## ----nn_model_effects, fig.alt = "Effects plot for compositional predictors", fig.height = 5, fig.width=9----
# Compositions to be used for generating the effects plots.
# Not all are needed but having more initial compositions gives higher accuracy. 
eff_data <- tribble(~"Sand", ~"Silt", ~"Clay",
                          1,       0,       0, 
                          0,       1,       0, 
                          0,       0,       1, 
                        0.5,     0.5,       0, 
                        0.5,       0,     0.5, 
                          0,     0.5,     0.5, 
                        0.8,     0.2,       0,
                        0.2,     0.8,       0,
                        0.8,       0,     0.2, 
                        0.2,       0,     0.8,
                          0,     0.8,     0.2, 
                          0,     0.2,     0.8,
                        0.6,     0.2,     0.2,
                        0.2,     0.6,     0.2,
                        0.2,     0.2,     0.6,
                        1/3,     1/3,     1/3)

# The data-preparation, adding predictions and plotting are done in a single dplyr pipeline here
# Prepare data
visualise_effects_data(# Data with initial compostions
                       data = eff_data,
                       # Column names for compositional predictors in data
                       prop = c("Sand", "Silt", "Clay"),
                       # Show effects plot for all compostional variables
                       var_interest = c("Sand", "Silt", "Clay"),
                       # Don't make predictions now and only return template
                       prediction = FALSE) %>% 
  # Add predictions
  add_prediction(interval = "none",        # No uncertainty intervals
                 model = nn_model) %>%     # Model to use for making predictions 
  # Print first few rows of data
  as_tibble() %>% print(n = 6) %>%  
  # Create plot
  visualise_effects_plot() +
    # ggplot function to add labels on plot
    labs(y = "Predicted SOC")

## ----forest_model-------------------------------------------------------------

forest_model <- randomForest(# Predictor for random forest model
                             SOC ~ Sand + Silt + Clay + 
                               Sand:Silt + Silt:Clay + Sand:Clay,
                             # Data
                             data = soil_data, 
                             # 5000 trees to be used random forest
                             ntree = 5000)
forest_model

## ----visualise_forest_model, fig.alt = "Response surface from random forest"----
# Prepare data
ternary_data(# Names of compositional predictors
             prop = c("Sand", "Silt", "Clay"), 
             # Model to use for making predictions
             model = forest_model,
             # Add predictions directly
             prediction = TRUE) %>% 
  ternary_plot(lower_lim = 1.5,       # Lower limit of fill scale
             upper_lim = 9.5,       # Upper limit of fill scale
             nlevels = 8,           # Number of colours for fill scale
             # Labels for ternary axes
             tern_labels = c("Sand", "Silt", "Clay")) +
  # ggplot function to add labels on plot
  labs(fill = "Predicted\nSOC")

