## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 6,
  fig.align = "center",
  warning = FALSE
)

library(knitr)

# Hook to automatically detect the function used in the chunk and set alt text
knit_hooks$set(plot = function(x, options) {
  if (is.null(options$fig.alt)) {
     # Combine the chunk code
    code <- paste(options$code, collapse = "\n")
    
    # List of known functions
    funs <- c("conditional_ternary_plot", "grouped_ternary_plot", "visualise_effects_plot", "simplex_path_plot", "gradient_change_plot", "prediction_contributions_plot", "model_diagnostics", "model_selection_plot")
    pattern <- paste(funs, collapse = "|")
    
    # Extract first matching function
    matches <- regmatches(code, gregexpr(pattern, code))[[1]]
    if (length(matches) > 0) {
      options$fig.alt <- paste0("Output from ", matches[1], "() function")
    } else {
      options$fig.alt <- "Plot output"
    }
  }
  knitr::hook_plot_md(x, options)
  hook_plot_md(x, options)
})


## ----libraries, message=FALSE, warning=FALSE----------------------------------
library(DImodels)     # For loading the dataset
library(DImodelsVis)  # For creating visualisations
library(dplyr)        # For data manipulation
library(ggplot2)      # For modifying visualisations
library(PieGlyph)     # Adding Pie-glyphs on figures

## ----theme-set, echo=FALSE----------------------------------------------------
theme_set(theme_DI(font_size = 10))

## ----raw_data-----------------------------------------------------------------
data("sim4")
head(sim4, 8)

## -----------------------------------------------------------------------------
model_data <- sim4 %>% 
  # Rename species columns
  rename(G1 = p1, G2 = p2, L1 = p3, 
         L2 = p4, H1 = p5, H2 = p6) %>% 
  # Total functional group proportion
  mutate(G = G1 + G2,
         L = L1 + L2,
         H = H1 + H2) %>% 
  # Add additional interaction columns to data
  mutate(wfg_G = G1*G2, wfg_L = L1*L2, wfg_H = H1*H2,
         bfg_GL = (G1 + G2) * (L1 + L2),
         bfg_GH = (G1 + G2) * (H1 + H2),
         bfg_LH = (L1 + L2) * (H1 + H2)) %>% 
  # Add dummy variables for treatment
  mutate(treatment50 = ifelse(treatment == 50, 1, 0),
         treatment150 = ifelse(treatment == 150, 1, 0),
         treatment250 = ifelse(treatment == 250, 1, 0)) %>% 
  # Rearrange columns
  select(-response, everything(), response)


head(model_data, 8)

## -----------------------------------------------------------------------------
cust_model <- glm(response ~ 0 + G1 + G2 + L1 + L2 + H1 + H2 + 
                          bfg_GL + bfg_GH + bfg_LH + 
                          wfg_G + wfg_L + wfg_H + 
                          treatment150 + treatment250, 
             data = model_data)

## -----------------------------------------------------------------------------
summary(cust_model)

## ----variables----------------------------------------------------------------
# Names of the compositional predictors (i.e., species) in the model
species <- c("G1", "G2", "L1", "L2", "H1", "H2")

# Functional groupings of the species
FG <- c("G", "G", "L", "L", "H", "H")

# Colours to be used for the six compostional variables (species) in plots
var_cols <- get_colours(vars = species, FG = FG)

## ----DiagData-----------------------------------------------------------------
diag_data <- model_diagnostics_data(
  model = cust_model,  # Regression model object
  prop = species)      # Names of the compositional predictors

## ----print_diag---------------------------------------------------------------
head(diag_data, 8)

## ----diag_attr----------------------------------------------------------------
attr(diag_data, "prop")

## ----diag_plot, fig.width=10, fig.height=6------------------------------------
model_diagnostics_plot(
    data = diag_data, # Output data-frame returned by model_diagnostics_data()
    which = c(1, 2),  # Only print first 2 plots to save space
    pie_colours = var_cols # Colours for the pie-slices
  )

## ----diag_plot2, fig.width=10, fig.height=6-----------------------------------
model_diagnostics_plot(
    data = diag_data, 
    which = c(1, 2),  
    pie_colours = var_cols,
    only_extremes = TRUE, # Show only extreme points as pie-glyphs
    npoints = 5) # Show the five most extreme points as pie-glyphs (default is 3)


## ----diag_plot3, fig.width=8, fig.height=6------------------------------------
# Extract data for containing observations recieving 250 N treatment
data_250 <- diag_data %>% 
  filter(treatment250 == 1)

# We first create the diagnostic plot
diag_plot <- model_diagnostics_plot(
    data = diag_data, 
    which = c(1),   # Only show residual vs fitted plot
    pie_colours = var_cols,
    only_extremes = TRUE, # Show only extreme points as pie-glyphs
    npoints = 0, # Show the no extreme points as pie-glyphs
  )

# Now we can use traditional ggplot machinery to modify the plot
diag_plot + 
  # Manually add pie-glyphs using the subsetted data from above
  geom_pie_glyph(data = data_250, slices = species, colour = "black")

## ----data_subset--------------------------------------------------------------
subset_data <- model_data %>% 
  # Filter only those observations recieving 250 N treatment
  filter(treatment250 == 1) %>% 
  distinct(G1, G2, L1, L2, H1, H2, .keep_all = TRUE) %>% 
  # Filter specific communities
  filter(
    # Six monocultures
    (G1 == 1 | G2 == 1 | L1 == 1 | L2 == 1 | H1 == 1 | H2 == 1) | 
    # Two species mixtures
    ((G1 == 0.5 & G2 == 0.5) | (L1 == 0.5 & L2 == 0.5) | (H1 == 0.5 & H2 == 0.5)) |
    # Six species centroid
    (near(G1, 0.16667, tol = 0.01)))
  
head(subset_data)

## -----------------------------------------------------------------------------
pred_cont_data <- prediction_contributions_data(
    model = cust_model, # Model used for making predicitons 
    data = subset_data,  # Data containing the observation for which to make prediction
    # X-axis labels to be used for the bars
    bar_labs = c("G1", "G2", "L1", "L2", "H1", "H2", "G-G", "L-L", "H-H", "Cent.")
  ) 
head(pred_cont_data)

## ----mod-coeffs---------------------------------------------------------------
(mod_coeffs <- coef(cust_model))

## ----pred-cont-coeffs---------------------------------------------------------
pred_cont_data <- prediction_contributions_data(
    coefficients = mod_coeffs, # Model coeffecients 
    coeff_cols = names(mod_coeffs), # Column names corresponding to the coefficients
    data = subset_data,  # Data containing the observation for which to make prediction
    # X-axis labels to be used for the bars
    bar_labs = c("G1", "G2", "L1", "L2", "H1", "H2", "G-G", "L-L", "H-H", "Cent.")
  ) 
head(pred_cont_data)

## ----pred_cont1, fig.width=8, fig.height=6------------------------------------
prediction_contributions_plot(
  data = pred_cont_data, # Output data from prediction_contributions_data()
  colours = c(# Colours for the six species contributions
              var_cols, 
              # Colours for between FG interactions
              get_shades("#DDCC77", shades = 3)[[1]], 
              # Colours for within FG interactions
              get_shades("#D55E00", shades = 3)[[1]],
              # Colours for nitrogen treatment
              get_shades("#505050", shades = 2)[[1]]))

## -----------------------------------------------------------------------------
pred_cont_data_grouped <- prediction_contributions_data(
    model = cust_model, 
    data = subset_data,
    # We group the three between and within FG interactions into single terms
    groups = list("Between FG ints." = c("bfg_GL", "bfg_GH", "bfg_LH"),
                  "Within FG ints." = c("wfg_G", "wfg_L", "wfg_H")),
    # X-axis labels to be used for the bars
    bar_labs = c("G1", "G2", "L1", "L2", "H1", "H2", "G-G", "L-L", "H-H", "Cent.")
  ) 
# Notice the .Contributions column
head(pred_cont_data_grouped, 10)

## ----pred_cont2, fig.width=8, fig.height=6------------------------------------
grouped_cont_plot <- prediction_contributions_plot(
  data = pred_cont_data_grouped, # Output data from prediction_contributions_data()
  colours = c(# Colours for the six species contributions
              var_cols, 
              # Colours for nitrogen treatment
              get_shades("#505050", shades = 2)[[1]],
              # Colours for between FG interactions
              "#DDCC77", 
              # Colours for within FG interactions
              "#0072B2"))
grouped_cont_plot

## ----pred_cont3, fig.width=8, fig.height=6------------------------------------
grouped_cont_plot +
  # Add dashed reference line
  geom_hline(yintercept = 25, linewidth = 1, 
             colour = "black", linetype = "dashed") + 
  facet_grid(~richness, labeller = label_both,
             space = "free_x", scale = "free_x")

## -----------------------------------------------------------------------------
cond_tern_template <- conditional_ternary_data(
  prop = species, # Names of the compositional predictors
  tern_vars = c("G1", "L1", "H1"), # Names for the three predictors to show in the ternary
  # The values at which to fix the remaining predictors
  # Any compositional predictors not specified here (e.g. H2) would be assumed to be 0 
  conditional = data.frame("G2" = c(0.2, 0.1), 
                           "L2" = c(0.1, 0.3)),
  # Do not add predictions
  prediction = FALSE)

head(cond_tern_template)

## -----------------------------------------------------------------------------
attributes(cond_tern_template)[-2]

## ----add_cols1----------------------------------------------------------------
cond_tern_template150 <- cond_tern_template %>% 
  # Add values for the interaction columns to data
  mutate(wfg_G = G1*G2, wfg_L = L1*L2, wfg_H = H1*H2,
         bfg_GL = (G1 + G2) * (L1 + L2),
         bfg_GH = (G1 + G2) * (H1 + H2),
         bfg_LH = (L1 + L2) * (H1 + H2)) %>%
  # We first want ternary for treatment150
  mutate(treatment150 = 1, treatment250 = 0)

head(cond_tern_template150)

## ----add_cols2----------------------------------------------------------------
cond_tern_template250 <- cond_tern_template %>% 
  # Add additional interaction columns to data
  mutate(wfg_G = G1*G2, wfg_L = L1*L2, wfg_H = H1*H2,
         bfg_GL = (G1 + G2) * (L1 + L2),
         bfg_GH = (G1 + G2) * (H1 + H2),
         bfg_LH = (L1 + L2) * (H1 + H2)) %>%
  # This will create ternary for treatment250
  mutate(treatment150 = 0, treatment250 = 1)

head(cond_tern_template250)

## -----------------------------------------------------------------------------
# Add predictions using helper function from DImodelsVis
cond_tern_template150 <- cond_tern_template150 %>% 
  add_prediction(model = cust_model)

# Add predictions using model coefficients
mod_coeffs <- coefficients(cust_model) # Coefficients
vcov_mat <- vcov(cust_model)           # vcov matrix of coefficients

cond_tern_template150 <- cond_tern_template150 %>% 
  add_prediction(coefficients = mod_coeffs,
                 vcov = vcov_mat)

# Add predictions using base R predict function
cond_tern_template250$.Pred <- predict(cust_model,
                                       newdata = cond_tern_template250)

head(cond_tern_template150)
head(cond_tern_template250)

## ----cond-limits--------------------------------------------------------------
lwr_lim <- floor(min(cond_tern_template150$.Pred, cond_tern_template250$.Pred))
upr_lim <- ceiling(max(cond_tern_template150$.Pred, cond_tern_template250$.Pred))

## ----cond_tern1, fig.width=7, fig.height=4------------------------------------
conditional_ternary_plot(cond_tern_template150,
                         contour_text = FALSE,
                         lower_lim = lwr_lim,
                         upper_lim = upr_lim)

## ----cond_tern2, fig.width=7, fig.height=4------------------------------------
conditional_ternary_plot(cond_tern_template250,
                         contour_text = FALSE,
                         lower_lim = lwr_lim,
                         upper_lim = upr_lim)

## -----------------------------------------------------------------------------
group_tern_data <- grouped_ternary_data(
  prediction = FALSE,                         # Do not make predictions now
  prop = species,                             # Compositional predictors
  FG = FG,                                    # Functional grouping
  # Relative split within each FG. Equal split in this example
  values = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5),    
  # Values for additional non-compsitional predictors
  add_var = list(treatment150 = c(0, 0),      
                 treatment250 = c(0, 1))
)
head(group_tern_data)

## -----------------------------------------------------------------------------
group_tern_data <- group_tern_data %>% 
  mutate(.add_str_ID = ifelse(.add_str_ID == "treatment150: 0; \ttreatment250: 0",
                              "Treatment: 50", "Treatment: 250"))

## ----group_tern, fig.width=8, fig.height=4------------------------------------
# Add interaction columns and the prediction
group_tern_data <- group_tern_data %>% 
  # Add additional interaction columns to data
  mutate(wfg_G = G1*G2, wfg_L = L1*L2, wfg_H = H1*H2,
         bfg_GL = (G1 + G2) * (L1 + L2),
         bfg_GH = (G1 + G2) * (H1 + H2),
         bfg_LH = (L1 + L2) * (H1 + H2)) %>%
  # Add predictions
  add_prediction(model = cust_model)

# Create visualisation
group_tern_plot <- grouped_ternary_plot(
  data = group_tern_data,
  lower_lim = 15,
  upper_lim = 40,
  nlevels = 5)

# Update plot to highlight the best-performing community in each ternary
group_tern_plot +
  # Add point highlighting the best performing community in each ternary
  geom_point(data = function(x) x %>% filter(.Pred == max(.Pred)),
             colour = "#505050", shape = 18, size = 4.5) +
  # Reduce size of legend so they fit
  theme(legend.key.width = unit(0.07, "npc"))

## ----simp-path-data-----------------------------------------------------------
starts <- tribble(~G1, ~G2, ~L1, ~L2, ~H1, ~H2,
                  1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
ends <- tribble(~G1, ~G2, ~L1, ~L2, ~H1, ~H2,
                1/2, 1/2,   0,   0,   0,  0,
                  0,   0, 1/2, 1/2,   0,  0,
                  0,   0,   0,   0, 1/2, 1/2)

path_data <- simplex_path_data(
  starts = starts,               # Starting compositions
  ends = ends,                   # Ending compositions
  prop = species,                # Names of compositional predictors
  prediction = FALSE,            # Don't make predictions now
  # We will visualise the paths at 150 and 250 level of treatment
  add_var = list(treatment = c("150", "250"))
)

## -----------------------------------------------------------------------------
path_data <- path_data %>% 
  # Add additional interaction columns to data
  mutate(wfg_G = G1*G2, wfg_L = L1*L2, wfg_H = H1*H2,
         bfg_GL = (G1 + G2) * (L1 + L2),
         bfg_GH = (G1 + G2) * (H1 + H2),
         bfg_LH = (L1 + L2) * (H1 + H2)) %>% 
  # Add dummy variables for treatment
  mutate(treatment50 = ifelse(treatment == 50, 1, 0),
         treatment150 = ifelse(treatment == 150, 1, 0),
         treatment250 = ifelse(treatment == 250, 1, 0)) %>% 
  # Add predictions and uncertainty around it
  add_prediction(model = cust_model, interval = "confidence")

head(path_data)

## ----path-plot, fig.width=10, fig.height=6------------------------------------
simplex_path_plot(
  data = path_data,       # Data
  pie_colours = var_cols, # Colours for the pie-glyphs
  pie_radius = 0.35,      # Radius of pie-glyphs
  se = TRUE,              # Show confidence band around predictions
  # Show pie-glyphs at 0%, 25%, 50%, 75% and 100% along each path
  pie_positions = c(0, 0.25, 0.5, 0.75, 1), 
  nrow = 1, ncol = 2 # Arrange all plots in a 1 row and 2 columns
) +
  labs(y = "Predicted yield", fill = "Species") +
  ylim(min(path_data$.Lower), max(path_data$.Upper))

## ----eff-template-------------------------------------------------------------
effects_data <- visualise_effects_data(
  data = subset_data,            # Data containing the communities to use for plot
  prop = species,                # Names of compositional predictors
  var_interest = species,        # Generate effects plot for all species
  prediction = FALSE             # Don't make predictions now
)

## ----eff-pred-----------------------------------------------------------------
effects_data <- effects_data %>% 
  # Add additional interaction columns to data
  mutate(wfg_G = G1*G2, wfg_L = L1*L2, wfg_H = H1*H2,
         bfg_GL = (G1 + G2) * (L1 + L2),
         bfg_GH = (G1 + G2) * (H1 + H2),
         bfg_LH = (L1 + L2) * (H1 + H2)) %>% 
  # Add predictions
  add_prediction(model = cust_model)
head(effects_data)

## ----effect-plot, fig.width=12, fig.height=9----------------------------------
visualise_effects_plot(
  data = effects_data,    # Data
  pie_colours = var_cols, # Colours for the pie-glyphs
  pie_radius = 0.35       # Radius of pie-glyphs
) +
  labs(y = "Predicted yield", fill = "Species")

