## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>",
  echo = TRUE, warning = FALSE, message = FALSE,
  fig.width = 8, fig.height = 5, fig.align = "center",
  class.source = "fold-show", eval = TRUE
)

library(knitr)

# Hook to automatically detect the function used in the chunk and set alt text
knit_hooks$set(plot = function(x, options) {
  if (is.null(options$fig.alt)) {
     # Combine the chunk code
    code <- paste(options$code, collapse = "\n")
    
    # List of known functions
    funs <- c("conditional_ternary", "grouped_ternary", "visualise_effects", "simplex_path", "gradient_change", "prediction_contributions_plot", "model_diagnostics", "model_selection")
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


## ----setup--------------------------------------------------------------------
library(DImodels)
library(DImodelsVis)
library(DImodelsMulti)
library(dplyr)
library(ggplot2)

## ----load-data----------------------------------------------------------------
head(dataBEL)

## ----models-------------------------------------------------------------------
# Name of compositional predictors (species)
species <- c("G1", "G2", "L1", "L2")
# Functional groupings of species
FG <- c("Gr", "Gr", "Le", "Le")
# Colours to be used for pie-glyphs for all figures
pie_cols <- get_colours(vars = species, FG = FG)

model <- DImulti(prop = species, FG = FG, y = "Y", 
                 eco_func = c("Var", "un"),
                 unit_IDs = "Plot", DImodel = "AV",
                 method = "REML", data = dataBEL)

## -----------------------------------------------------------------------------
model

## ----model-diagnostics1, fig.height=5, fig.width=8----------------------------
model_diagnostics(model = model, which = c(1, 2), nrow = 1)

## ----model-diagnostics2, fig.height=9, fig.width=12---------------------------
model_diagnostics(model = model, which = c(1, 2), nrow = 2) + 
  facet_wrap(~Var)

## ----gradient-change, fig.width=12, fig.height=5------------------------------
grad_data <- get_equi_comms(4, variables = c("G1", "G2", "L1", "L2")) %>%
  mutate("Rich." = Richness)

gradient_change(model = model, data = grad_data, nrow = 1)

## ----cond-tern, fig.height=10, fig.width=8------------------------------------
conditional_ternary(model = model, resolution = 1,
                    tern_vars = c("G1", "G2", "L2"),
                    conditional = data.frame(L1 = c(0, 0.25, 0.5)),
                    lower_lim = 30, upper_lim = 110, nlevels = 8,
                    nrow = 3)

## ----grouped-tern, fig.height=8, fig.width=4----------------------------------
grouped_ternary(model = model, resolution = 1,
                FG = c("G", "G", "L1", "L2"),
                # Split of species within each group
                values = c(0.5, 0.5, 1, 1),
                lower_lim = 30, upper_lim = 110, nlevels = 8,
                add_var = list("Var" = c("N", "Weed")),
                nrow = 2) # Arrange in two rows

## ----effects-plot, fig.height = 12, fig.width = 6-----------------------------
eff_data <- get_equi_comms(4, variables = c("G1", "G2", "L1", "L2")) 
visualise_effects(model = model, data = eff_data, 
                  var_interest = c("L1", "L2"), 
                  # arrange plot in three row (one for each function)
                  nrow = 3) 

## ----simplex-path, fig.width=10, fig.height=12--------------------------------
# The centroid community (starting point for the straight line)
starts <- tribble( ~G1,  ~G2,  ~L1,  ~L2,
                  0.25, 0.25, 0.25, 0.25)
# The six binary mixtures (ending points for the straight lines)
ends <- tribble(~G1,    ~G2,  ~L1,  ~L2,
                  0.5,  0.5,    0,    0,
                  0.5,    0,  0.5,    0,
                  0.5,    0,    0,  0.5,
                    0,  0.5,  0.5,    0,
                    0,  0.5,    0,  0.5,
                    0,    0,  0.5,  0.5)

# Create the visualisation
simplex_path(model = model,
             starts = starts, ends = ends,
             # Manually specify to create plot for Weed and N
             add_var = list("Density" = factor(c(- 1, 1))),                nrow = 3, ncol = 2)

## ----pred-cont, fig.width=8, fig.height=14------------------------------------
pred_data <- get_equi_comms(4, richness_lvl = c(1, 2, 4),
                            variables = c("G1", "G2", "L1", "L2")) %>%
  mutate(labels = c("G1_mono", "G1_mono", "G1_mono", "L2_mono",
                    "G1-G2", "G1-L1", "G1-L2", "G2-L1", "G2-L2", "L1-L2",
                    "Centroid")) %>% 
  # Repeat each community across the three ecosystem functions
  add_add_var(add_var = list("Var" = c("N", "Sown", "Weed"))) %>% 
  mutate(G1_ID = G1, G2_ID = G2, L1_ID = L1, L2_ID = L2,
         AV = DI_data_E_AV(prop = 1:4, data = .)$AV,
         "Rich." = Richness)

prediction_contributions_data(data = pred_data, model = model, 
                              bar_labs = "labels",
                              # Important to group the coefficients
                              groups = list("G1" = 1:3, "G2" = 4:6,
                                            "L1" = 7:9, "L2" = 10:12,
                                            "AV" = 13:15)) %>% 
  prediction_contributions_plot(colours = c(pie_cols, "steelblue3"),
                                nrow = 3) +
  facet_grid(~ Rich., scale = "free_x", space = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.1))

