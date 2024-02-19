## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 6,
  fig.aling = "center",
  warning = FALSE
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(DImodelsVis)
library(DImodels)
library(dplyr)

## ----load-data----------------------------------------------------------------
data("sim4")
head(sim4)

## ----fit-model----------------------------------------------------------------
species <- c("p1", "p2", "p3", "p4", "p5", "p6")
FG <- c("Gr", "Gr", "Le", "Le", "He", "He")

model_data <- sim4 %>% 
  mutate(AV = DI_data_E_AV(prop = species, data = .)$AV,
         treatment = as.factor(treatment))

mod <- lm(response ~  0 + p1 + p2 + p3 + p4 + p5 + p6 + (AV):treatment,
           data = model_data)

## -----------------------------------------------------------------------------
summary(mod)

## ----diagnostics-plot, fig.height=8, fig.width = 9----------------------------
# Create model diagnostics plot with pie-glyphs showing species proportions
model_diagnostics(model = mod, prop = species)

## -----------------------------------------------------------------------------
subset_data <- model_data[c(1, 4, 7, 10, 13, 16, 45), ]
print(subset_data)

## -----------------------------------------------------------------------------
plot_data <- prediction_contributions_data(data = subset_data,
                                           model = mod)
plot_data

## -----------------------------------------------------------------------------
coeffs <- c("p1" = 30.23, "p2" = 20.20, "p3" = 22.13,
            "p4" = 23.88, "p5" = 13.60, "p6" = 15.67,
            "AV:treatment50" = 11.46,
            "AV:treatment150" = 22.99,
            "AV:treatment250" = 30.37)
# One could also export the variance-covariance matrix from another 
# software as a .csv file and read it here using the read.csv() function
vcov_mat <- vcov(mod)

print(coeffs)

# Note when using model-coefficients, the data should be in the same order 
# as the model coefficients
# We can use the model.matrix function to prepare our subset data in the necessary format
coeff_data <- model.matrix(~ 0 + p1 + p2 + p3 + p4 + p5 + p6 + AV:treatment,
                           data = subset_data) %>% 
  as.data.frame()
print(coeff_data)

# Notice how the column names of the coefficients and coeff_data match exactly

# Use coefficients and vcov parameter to specify the coefficients and 
# variance-covariance matrix respectively.
# Note: specifing a vcov matrix is necessary only if the user want the 
# uncertainty associated with the prediction
plot_data <- prediction_contributions_data(data = coeff_data,
                                           coefficients = coeffs,
                                           vcov = vcov_mat)
head(plot_data)

## ----prediction-contributions-plot, fig.width=8-------------------------------
prediction_contributions_plot(data = plot_data)

## -----------------------------------------------------------------------------
plot_data <- gradient_change_data(data = model_data, prop = species,
                                  model = mod)
head(plot_data)

## ----gradient-change-plot, fig.width=8----------------------------------------
gradient_change_plot(data = plot_data, 
                     facet_var = "treatment")

## -----------------------------------------------------------------------------
plot_data <- visualise_effects_data(data = subset_data, prop = species,
                                    var_interest = c("p1", "p3"),
                                    effect = "increase",
                                    prediction = FALSE)
plot_data <- plot_data %>% mutate(AV = DI_data_E_AV(data = ., 
                                                    prop = species)$AV)
plot_data <- add_prediction(plot_data, model = mod, interval = "confidence")

## ----visualise-effects-plot, fig.width=8--------------------------------------
visualise_effects_plot(data = plot_data)

## -----------------------------------------------------------------------------
start_comm <- model_data %>% filter(richness == 6) %>% 
  distinct(treatment, .keep_all = TRUE) %>% 
  slice(rep(1:3, each = 6))

end_comm <- model_data %>% filter(richness == 1) %>% 
  distinct(p1, p2, p3, p4, p5, p6, treatment, .keep_all = TRUE)

plot_data <- simplex_path_data(starts = start_comm,
                               ends = end_comm,
                               prop = species,
                               prediction = FALSE)
plot_data <- plot_data %>% mutate(AV = DI_data_E_AV(data = ., 
                                                    prop = species)$AV)
plot_data <- add_prediction(plot_data, model = mod, interval = "confidence")
head(plot_data)

## ----simplex-path-plot, fig.width=8-------------------------------------------
simplex_path_plot(data = plot_data, se = TRUE,
                  facet_var = "treatment")

## -----------------------------------------------------------------------------
plot_data <- conditional_ternary_data(prop = species, 
                                      tern_vars = c("p1", "p5", "p6"),
                                      add_var = list("treatment" = c("50", "250")),
                                      conditional = data.frame("p2" = c(0, 0.3), 
                                                               "p3" = c(0.2, 0.3)),
                                      prediction = FALSE)

plot_data <- plot_data %>% mutate(AV = DI_data_E_AV(data = ., 
                                                    prop = species)$AV)
plot_data <- add_prediction(plot_data, model = mod)
head(plot_data)

## ----conditional-ternary-plot, fig.height=10, fig.width=8---------------------
conditional_ternary_plot(data = plot_data, nrow = 2)

## -----------------------------------------------------------------------------
plot_data <- grouped_ternary_data(prop = c("p1", "p2", "p3", "p4", "p5", "p6"), 
                                  FG = c("Gr", "Gr", "Le", "Le", "He", "He"),
                                  values = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
                                  add_var = list("treatment" = c("50","250")),
                                  prediction = FALSE)

plot_data <- plot_data %>% mutate(AV = DI_data_E_AV(data = ., 
                                                    prop = species)$AV)
plot_data <- add_prediction(plot_data, model = mod)

## ----grouped-ternary-plot, fig.height=8, fig.width=6--------------------------
grouped_ternary_plot(data = plot_data, nrow = 2)

