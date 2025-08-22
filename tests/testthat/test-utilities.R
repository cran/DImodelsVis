test_that("get_colours works", {
  # Colour-blind friendly colours are returned
  expect_equal(colour_blind_friendly_cols(7),
               c("#009E73", "#AA4499", "#0072B2",
                 "#F0E442", "#D55E00", "#661100", "#332288"))

  # For n > 20, Spectral colour spectrum should be returned with a warning
  expect_message(colour_blind_friendly_cols(21),
                 "There are too many colours")
  expect_equal(colour_blind_friendly_cols(21) %>% suppressMessages(),
               grDevices::hcl.colors(palette = "Spectral", n = 21))

  # colours with shades and paired palettes
  expect_equal(get_colours(3),
               c("#009E73", "#AA4499", "#0072B2"))

  expect_equal(get_colours(paste0("p", 1:4)),
               c("#009E73", "#AA4499", "#0072B2", "#F0E442"))

  expect_equal(get_colours(paste0("p", 1:4), FG = c("G", "G", "L", "L")),
               c("#2D6852", "#5FCBA0", "#7A406F", "#B26CA5"))


  # Reasonable errors
  expect_error(get_colours(),
               "`vars` cannot be missing")

  expect_error(get_colours(4, c("1", "1")),
               "`FG` should be a character vector")
  expect_error(get_colours(4, c(1, 1, 1, 2)),
               "`FG` should be a character vector")

})

test_that("add_add_var works", {
  # add_add_var works as expected
  test_data <- data.frame("A" = 1, "B" = 2, "C" = 3)

  # Works with list
  expect_equal(add_add_var(test_data,
                           list("D" = c(1, 2),
                                "E" = c(1, 2, 3))),
               data.frame(A = 1, B = 2, C = 3,
                          D = rep(c(1,2), times = 3),
                          E = rep(c(1, 2, 3), each = 2)) %>%
                 mutate(.add_str_ID = paste0("D: ", D, "; \tE: ", E)))

  # Works with data.frame()
  expect_equal(add_add_var(test_data,
                           data.frame("D" = c(1, 2),
                                      "E" = c(3, 4))),
               data.frame(A = 1, B = 2, C = 3,
                          D = rep(c(1, 2)),
                          E = rep(c(3, 4))) %>%
                 mutate(.add_str_ID = paste0("D: ", D, "; \tE: ", E)))

  # If nothing specified return original data
  expect_equal(add_add_var(test_data),
               test_data)

  # Common errors
  expect_error(add_add_var(),
               "`data` cannot be empty")

  expect_error(add_add_var(test_data,
                           lm(A~B, data = test_data)),
               "The value specified in `add_var` should be a list or data-frame.")

  # Warning if same columns
  expect_warning(add_add_var(test_data, list("A" = c(1, 2))),
                 "Certain names specified")

})

test_that("get_shades works", {

  # 1 shade returns the same colour
  expect_equal(get_shades("#808080", shades = 1),
               list("#808080"),
               ignore_attr = T)

  # > 1 shades
  expect_equal(get_shades("#808080", shades = 8),
               list(c("#272727", "#404040", "#5A5A5A", "#737373",
                      "#8D8D8D", "#A6A6A6", "#C0C0C0", "#D9D9D9")),
               ignore_attr = T)

  # Multiple colours (same number of shades for each)
  expect_equal(get_shades(c("#808080", "black"), shades = 2),
               list(c("#666666", "#9A9A9A"),
                    c("#0D0D0D", "#404040")),
               ignore_attr = T)

  # Multiple colours (different number of shades for each)
  expect_equal(get_shades(c("#808080", "black"), shades = c(1, 2)),
               list(c("#808080"),
                    c("#0D0D0D", "#404040")),
               ignore_attr = T)

  # Error if length of shades don't match length of colours
  expect_error(get_shades(c("#808080", "black"), shades = c(1, 2, 3)),
               "`shades` should be of a vector of length 1 or same")
})

test_that("get_equi_comms works", {

  # Basic usage
  expect_equal(get_equi_comms(2),
               data.frame("Var1" = c(1, 0, 0.5),
                          "Var2" = c(0, 1, 0.5),
                          "Richness" = c(1, 1, 2)))

  # Choose specific richness level
  expect_equal(get_equi_comms(3, richness_lvl = c(1, 3)),
               data.frame("Var1" = c(1, 0, 0, 1/3),
                          "Var2" = c(0, 1, 0, 1/3),
                          "Var3" = c(0, 0, 1, 1/3),
                          "Richness" = c(1, 1, 1, 3)))

  # Random sampling
  set.seed(1)
  expect_equal(get_equi_comms(3, richness_lvl = c(1, 3),
                              threshold = 2,
                              variables = c("p1", "p2", "p3")),
               data.frame("p1" = c(1, 1, 1/3),
                          "p2" = c(0, 0, 1/3),
                          "p3" = c(0, 0, 1/3),
                          "Richness" = c(1, 1, 3)))

  # Reasonable errors
  expect_error(get_equi_comms(3, richness_lvl = c(1, 3),
                              variables = c("p1", "p2")),
               "The variable names specified in `variables`")

  expect_error(get_equi_comms(3, richness_lvl = c(1, 4),
                              threshold = 2,
                              variables = c("p1", "p2", "p3")),
               "`richness_lvl` can take values between 1 and 3")

})

test_that("DI utility functions works", {
  test_data <- get_equi_comms(4, variables = paste0("p", 1:4))

  # Richness
  expect_equal(test_data %>% slice(1, 5, 11, 15) %>%
                 mutate(rich = get_richness(.data, prop = paste0("p", 1:4))),
               test_data %>% slice(1, 5, 11, 15) %>% mutate(rich = 1:4))

  # Error if prop is null
  expect_error(test_data %>%
                 mutate(rich = get_richness(.data)),
               "In argument:")

  # Evenness
  expect_equal(test_data %>% slice(1, 5, 11, 15) %>%
                 mutate(even = get_evenness(.data, prop = paste0("p", 1:4))),
               test_data %>% slice(1, 5, 11, 15) %>% mutate(even = c(0, 6/9, 8/9, 9/9)),
               ignore_attr = T)

  # Error if prop is null
  expect_error(test_data %>%
                 mutate(even = get_evenness(.)),
               "In argument:")

  # Get comm sum
  # Evenness
  expect_equal(test_data  %>%
                 mutate(sum = get_comm_sum(.data, prop = paste0("p", 1:4))),
               test_data %>%  mutate(sum = 1),
               ignore_attr = T)

  # Error if prop is null
  expect_error(test_data %>%
                 mutate(sum = get_comm_sum(.data)),
               "In argument:")

  # Check equiproportional communities
  expect_true(check_equi(c(1/3, 1/3, 1/3)))
  expect_false(check_equi(c(1/3, 1/3, 2/3)))



  # Add ID, int and exp_str functions
  library(DImodels)

  data("Switzerland")

  species <- paste0("p", 1:4)
  mod_data <- Switzerland

  mod_STR <- DI(y = "yield", prop = species, data = mod_data,
               DImodel = "STR", estimate_theta = TRUE,
               treat = "nitrogen", density = "density") %>% suppressMessages()
  mod_ID <- DI(y = "yield", prop = species, data = mod_data,
               DImodel = "ID", estimate_theta = TRUE, extra_formula = ~ density + nitrogen:density,
               treat = "nitrogen") %>% suppressMessages()
  mod_E <- DI(y = "yield", prop = species,
              data = mod_data %>%
                mutate("nitrogen" = as.numeric(as.character(nitrogen))),
               DImodel = "E", estimate_theta = FALSE,
               treat = "nitrogen", density = "density") %>% suppressMessages()
  mod_AV <- DI(y = "yield", prop = species, data = mod_data,
               DImodel = "AV", estimate_theta = TRUE,
               treat = "nitrogen", density = "density") %>% suppressMessages()
  mod_FG <- DI(y = "yield", prop = species, data = mod_data,
               DImodel = "FG", estimate_theta = TRUE, FG = c("G", "G", "L", "L"),
               treat = "nitrogen", density = "density") %>% suppressMessages()
  mod_ADD <- DI(y = "yield", prop = species, data = mod_data, theta = 1,
                DImodel = "ADD", estimate_theta = FALSE, ID = c("G", "G", "L", "L"),
                treat = "nitrogen", density = "density") %>% suppressMessages()
  mod_FULL <- DI(y = "yield", prop = species, data = mod_data,
                 DImodel = "FULL", estimate_theta = FALSE, ID = c("p1", "p2", "p3", "p3"),
                 treat = "nitrogen", density = "density") %>% suppressMessages()

  mod_cust <- DI(custom_formula = yield ~ 0 + (p1 + p2 + p3 + p4)^2,
                 data = mod_data) %>% suppressMessages()

  pred_data <- mod_data %>%
    mutate(richness = get_richness(data = ., prop = species)) %>%
    filter(richness == 1 | p1 == 0.25) %>%
    distinct(p1, p2, p3, p4, nitrogen, density, richness) %>%
    arrange(richness)

  # Add ID works
  expect_equal(pred_data %>% slice(1:4) %>% add_ID_terms(mod_ID),
               pred_data %>% slice(1:4) %>%
                 mutate(p1_ID = c(1, 0, 0, 0),
                        p2_ID = c(0, 1, 0, 0),
                        p3_ID = c(0, 0, 1, 0),
                        p4_ID = c(0, 0, 0, 1)))

  expect_equal(pred_data %>% slice(20) %>% add_ID_terms(mod_AV),
               pred_data %>% slice(20) %>%
                 mutate(p1_ID = c(0.25),
                        p2_ID = c(0.25),
                        p3_ID = c(0.25),
                        p4_ID = c(0.25)))

  expect_equal(pred_data %>% slice(1:4) %>% add_ID_terms(mod_ADD),
               pred_data %>% slice(1:4) %>%
                 mutate(G = c(1, 1, 0, 0),
                        L = c(0, 0, 1, 1)))

  expect_equal(pred_data %>% slice(1:4) %>% add_ID_terms(mod_FULL),
               pred_data %>% slice(1:4) %>%
                 mutate(p1 = c(1, 0, 0, 0),
                        p2 = c(0, 1, 0, 0),
                        p3 = c(0, 0, 1, 1)))

  expect_equal(pred_data %>% add_ID_terms(mod_cust) %>% suppressWarnings(),
               pred_data)

  # add_ID errors
  expect_error(add_ID_terms(), "`data` cannot be empty")
  expect_error(add_ID_terms(pred_data), "`model` cannot be empty")

  expect_warning(pred_data %>% add_ID_terms(mod_cust),
                 "for a custom DI model")

  # Add Int works
  expect_equal(pred_data %>% add_interaction_terms(mod_STR),
               pred_data)

  expect_equal(pred_data %>% add_interaction_terms(mod_ID),
               pred_data)

  expect_equal(pred_data %>% slice(20) %>% add_interaction_terms(mod_E),
               pred_data %>% slice(20) %>% mutate(E = 1),
               ignore_attr = TRUE)

  expect_equal(pred_data %>% add_interaction_terms(mod_AV),
               pred_data %>% mutate(AV = DI_data(prop = species,
                                                 data = pred_data,
                                                 what = "AV",
                                                 theta = attr(mod_AV, "theta_val"))),
               ignore_attr = TRUE)

  expect_equal(pred_data %>% add_interaction_terms(mod_ADD),
               pred_data %>%
                 bind_cols(DI_data(prop = species,
                                   what = "ADD",
                                   data = pred_data,
                                   theta = 1)),
               ignore_attr = TRUE)

  expect_equal(pred_data %>% add_interaction_terms(mod_FG),
               pred_data %>%
                 bind_cols(DI_data_FG(prop = species,
                                      FG = c("G", "G", "L", "L"),
                                      data = pred_data,
                                      theta = attr(mod_FG, "theta_val"))),
               ignore_attr = TRUE)

  expect_equal(pred_data %>% add_interaction_terms(mod_FULL),
               pred_data %>%
                 bind_cols(DI_data(prop = species,
                                   what = "FULL",
                                   data = pred_data,
                                   theta = 1)),
               ignore_attr = TRUE)

  expect_equal(pred_data %>% add_interaction_terms(mod_cust) %>% suppressWarnings(),
               pred_data)

  # add_int errors
  expect_error(add_interaction_terms(), "`data` cannot be empty")
  expect_error(add_interaction_terms(pred_data), "`model` cannot be empty")

  expect_warning(pred_data %>% add_interaction_terms(mod_cust),
                 "for a custom DI model")

  # Add exp structures
  expect_equal(pred_data %>% slice(1:4) %>%
                 add_ID_terms(mod_AV) %>%
                 add_interaction_terms(mod_AV) %>%
                 add_exp_str(mod_AV),
               pred_data %>% slice(1:4) %>%
                 add_ID_terms(mod_AV) %>%
                 add_interaction_terms(mod_AV) %>%
                 mutate(nitrogen50 = 0,
                        nitrogen150 = 1,
                        densityhigh= 1))

  expect_equal(pred_data %>% slice(1:4) %>%
                 select(-nitrogen) %>%
                 add_ID_terms(mod_E) %>%
                 add_interaction_terms(mod_E) %>%
                 add_exp_str(mod_E),
               pred_data %>% slice(1:4) %>%
                 select(-nitrogen) %>%
                 add_ID_terms(mod_E) %>%
                 add_interaction_terms(mod_E) %>%
                 mutate(nitrogen = 150,
                        densitylow = 0,
                        densityhigh= 1))

  # With extra formula
  expect_equal(pred_data %>% slice(1:4) %>%
                 add_ID_terms(mod_ID) %>%
                 select(-nitrogen, -density) %>%
                 add_exp_str(mod_ID),
               pred_data %>% slice(1:4) %>%
                 add_ID_terms(mod_AV) %>%
                 select(-nitrogen, -density) %>%
                 mutate(nitrogen = factor("50"),
                        density = factor("low"),
                        nitrogen50 = 1,
                        nitrogen150 = 0,
                        densityhigh = 0,
                        "nitrogen150:densityhigh" = 0),
               ignore_attr = T)

  expect_equal(pred_data %>% slice(1:4) %>%
                 add_ID_terms(mod_ID) %>%
                 select(-density) %>%
                 mutate(nitrogen = as.character(nitrogen)) %>%
                 add_exp_str(mod_ID),
               pred_data %>% slice(1:4) %>%
                 add_ID_terms(mod_AV) %>%
                 select(-density) %>%
                 mutate(density = factor("low"),
                        nitrogen50 = 0,
                        nitrogen150 = 1,
                        densityhigh = 0,
                        "nitrogen150:densityhigh" = 0),
               ignore_attr = T)

  # Same data for custom model
  expect_equal(pred_data %>% add_exp_str(mod_cust) %>% suppressWarnings(),
               pred_data)

  # add_exp_str errors
  expect_error(add_exp_str(), "`data` cannot be empty")
  expect_error(add_exp_str(pred_data), "`model` cannot be empty")
  expect_error(pred_data %>%
                 add_ID_terms(mod_ID) %>%
                 mutate(density = "medium") %>%
                 add_exp_str(mod_ID),
               "Values given for")
  expect_error(pred_data %>%
                 add_ID_terms(mod_AV) %>%
                 mutate(density = "medium") %>%
                 add_exp_str(mod_AV),
               "Values given for")

  expect_warning(pred_data %>% add_exp_str(mod_cust),
                 "for a custom DI model")

  # Model not DI function
  expect_error(model_not_DI(call_fn = "prediction_contributions"),
               "`model` should be a regression model")

})

test_that("Basic data wrangling operations work", {

  # Rescale
  expect_equal(range(rescale(mtcars$mpg)), c(0, 1))
  expect_equal(range(rescale(mtcars$mpg), min = -1, max = 100),
               c(-1, 100))

  # Copy attributes
  source <- data.frame("A" = 1, "B" = 2) %>% `attr<-`("cust", "test")
  target <- data.frame("C" = 1, "D" = 2) %>% `attr<-`("cust2", "test2")
  expect_equal(attributes(copy_attributes(source, target)),
               list("names" = c("A", "B"),
                    "class" = "data.frame",
                    "row.names" = 1,
                    "cust" = "test",
                    "cust2" = "test2"))

  expect_error(copy_attributes(),
               "`target` cannot be empty")

  expect_error(copy_attributes(target = data.frame("A" = 1, "B" = 1)),
               "`source` cannot be empty")

  # group prop
  expect_error(group_prop(),
               "`data` cannot be empty")
  expect_error(group_prop(data.frame("A" = 1, "B" = 1)),
               "`prop` cannot be empty")
  expect_error(group_prop(data.frame("A" = 0, "B" = 1),
                          prop = 1:2, FG = 1:2),
               "`FG` was specified as ")
  expect_error(group_prop(data.frame("A" = 0, "B" = 1),
                          prop = 1:2, FG = c("1", "2", "3")),
               "`FG` has length ")

  # Custom filter
  expect_error(custom_filter(),
               "`data` cannot be empty")
  expect_error(custom_filter(test_data, special = 1),
               "`special` should be a character")
  expect_error(custom_filter(test_data, special = "1"),
               "`prop` cannot be empty")

  test_data <- get_equi_comms(3, variables = paste0("p", 1:3))
  expect_equal(test_data %>% custom_filter(p1 == 1),
               test_data[1, ])
  expect_equal(test_data %>% custom_filter(p1 == 1),
               test_data[1, ])
  expect_equal(test_data %>% custom_filter(p1 < 1, prop = paste0("p", 1:3),
                                           special = "richness == 2"),
               test_data[4:6, ],
               ignore_attr = TRUE)
  expect_equal(test_data %>% custom_filter(p1 > 1, prop = paste0("p", 1:3),
                                           special = "richness == 1"),
               test_data %>% filter(p1 > 1),
               ignore_attr = TRUE)


})

test_that("Check functions works", {
  # Check presence
  expect_true(check_presence(data.frame("A" = 1, "B" = 2), "A"))
  expect_true(check_presence(data.frame("A" = 5, "B" = 4), 2))

  expect_error(check_presence(data.frame("A" = 5, "B" = 4), 3),
               "is not present in the data.")
  expect_error(check_presence(data.frame("A" = 5, "B" = 4), "C"),
               "Cannot find column")

  # Check col exists
  expect_true(check_col_exists(data.frame("A" = 1, "B" = 2), "A"))
  expect_false(check_col_exists(data.frame("A" = 1, "B" = 2), "C"))

  # Check plot data
  expect_true(check_plot_data(data.frame("A" = 1, "B" = 2), "A"))
  expect_error(check_plot_data(data.frame("A" = 1, "B" = 2), c("C", "D", "E"),
                               calling_fun = "prediction_contributions"),
               "All variables necessary")

})

test_that("model_diagnostic helpers work", {
  expect_equal(dropInf(5, 0.5), 5)
  expect_equal(dropInf(c(5, 6, 7), c(0.5, 0.75, 1)), c(5, 6, NaN)) %>% suppressWarnings()

  expect_warning(dropInf(c(5, 6, 7), c(0.5, 0.75, 1)),
                 "not plotting observations with leverage one")

})

test_that("Basic plotting operations work", {
  pl_data <- data.frame(x = c(1, 2, 3), y = 1:3,
                        gr = c("A", "B", "C"))
  pl <- ggplot(data = pl_data) +
    geom_point(aes(x = x, y = y)) +
    theme_DI()

  expect_equal(add_facet(pl, data = pl_data, facet_var = "gr",
                         labeller = label_both),
               pl + facet_wrap(~gr, labeller = label_both),
               ignore_attr = TRUE)

  # Warning for more than one variable
  expect_warning(add_facet(pl, data = pl_data,
                           facet_var = c("x", "gr")),
                 "Currently facetting is supported only with a single variable")

  # Error if value doesn't exist
  expect_error(add_facet(pl, data = pl_data,
                         facet_var = c("x1")),
               "The value specified in `facet_var` is invalid")

  # Error for theme_DI
  expect_error(theme_DI(font_size = 13, legend = 2),
               "If specified as a numeric vector the values")
  expect_error(theme_DI(font_size = 13, legend = list(3)),
               "`legend` should be of type")

})

test_that("tern projection functions", {

  # Errors
  test_data <- get_equi_comms(3, variables = paste0("p", 1:3)) %>%
    mutate(y = 0.5)
  expect_error(tern_to_prop_proj(),
               "`data` cannot be empty")
  expect_error(tern_to_prop_proj(test_data),
               "`x` cannot be empty")
  expect_error(tern_to_prop_proj(test_data, x = "p1"),
               "`y` cannot be empty")
  expect_error(tern_to_prop_proj(test_data, x = "p1", y = "p2",
                                 prop = paste0("p",1:4)),
               "`prop` should be a character vector")
  expect_error(tern_to_prop_proj(test_data, x = "p1", y = "p2",
                                 prop = 1:3),
               "`prop` is specified as an")
  expect_error(tern_to_prop_proj(test_data, x = ".x", y = "p2",
                                 prop = paste0("p", 1:3)),
               "\"x\" was not present in data")
  expect_error(tern_to_prop_proj(test_data, x = "p1", y = ".y",
                                 prop = paste0("p", 1:3)),
               "\"y\" was not present in data")
  expect_warning(tern_to_prop_proj(test_data, x = "Richness", y = "y",
                                   prop = paste0("p", 1:3)),
                 "It is expected that the values in `x`")
  expect_warning(tern_to_prop_proj(test_data, x = "p2", y = "p1",
                                   prop = paste0("p", 1:3)),
                 "It is expected that the values in `y`")




  expect_error(prop_to_tern_proj(),
               "`data` cannot be empty")
  expect_error(prop_to_tern_proj(test_data, prop = paste0("p",1:4)),
               "Currently projections are supported for systems with three")

  expect_equal(prop_to_tern_proj(test_data, prop = paste0("p", 1:3))[1, c(".x", ".y")],
               data.frame(".x" = 0.5, ".y" = sqrt(3)/2))
})

test_that("Prediction functions work", {
  library(DImodels)
  library(DImodelsMulti) %>% suppressMessages()
  data("Switzerland")

  species <- paste0("p", 1:4)
  mod_data <- Switzerland

  mod <- DI(y = "yield", prop = species, data = mod_data,
            DImodel = "AV",
            treat = "nitrogen", density = "density") %>% suppressMessages()

  pred_data <- mod_data %>%
    mutate(richness = get_richness(data = ., prop = species)) %>%
    filter(richness == 1 | p1 == 0.25) %>%
    distinct(p1, p2, p3, p4, nitrogen, density, richness) %>%
    arrange(richness)

  all_data <- pred_data %>%
    add_ID_terms(mod) %>%
    add_interaction_terms(mod) %>%
    add_exp_str(mod)

  # With DI model
  expect_equal(pred_data %>%
                 add_prediction(model = mod),
               mutate(pred_data, ".Pred" = predict(mod, newdata = pred_data)))

  # Get CI
  expect_equal(pred_data %>% slice(1:2) %>%
                 add_prediction(model = mod, conf.level = 0.5,
                                interval = "prediction"),
               pred_data %>% slice(1:2) %>%
                 mutate(.Pred = c(8.63794122, 8.69461957),
                        .Lower = c(7.39963433, 7.45631268),
                        .Upper = c(9.87624811, 9.93292646)))
  expect_equal(pred_data %>% slice(1:2) %>%
                 add_prediction(model = mod, conf.level = 0.75,
                                interval = "confidence"),
               pred_data %>% slice(1:2) %>%
                 mutate(.Pred = c(8.63794122, 8.69461957),
                        .Lower = c(7.838957034, 7.895635378),
                        .Upper = c(9.43692541, 9.49360375)))

  # With non-DI models
  expect_equal(all_data %>%
                 add_prediction(model = mod %>% `class<-`("glm")),
               all_data %>% mutate(".Pred" = predict(mod, newdata = pred_data)),
               ignore_attr = TRUE)


  # With DImodelsMulti
  belModel <- DImulti(prop = c("G1", "G2", "L1", "L2"), y = "Y", eco_func = c("Var", "un"),
                      unit_IDs = "Plot", theta = c(0.5, 1, 1.2), DImodel = "FG",
                      FG = c("Grass", "Grass", "Legume", "Legume"), extra_fixed = ~ Density,
                      method = "REML", data = dataBEL)

  SWEmodel <- DImulti(y = "YIELD", time = c("YEARN", "CS"), unit_IDs = "PLOT",
                      prop = 5:8, data = dataSWE %>% mutate(YEARN = as.factor(YEARN)),
                      DImodel = "AV", method = "ML")

  expect_equal(dataBEL %>% slice(1:6) %>%
                 add_prediction(model = belModel),
               dataBEL %>% slice(1:6) %>%
                 mutate(".Pred" = c(163.08951430, 206.64080278, 75.02145021,
                                    151.97182599, 213.41018590, 65.45711904)),
               ignore_attr = TRUE)

  expect_equal(dataBEL %>% slice(1:6) %>%
                 mutate(AV = 0, E = 0, p1_add = 0 ,
                        "FG.f" = 0, `FULL.p1:p2` = 0) %>%
                 add_prediction(model = belModel),
               dataBEL %>% slice(1:6) %>%
                 mutate(AV = 0, E = 0, p1_add = 0 ,
                        "FG.f" = 0, `FULL.p1:p2` = 0) %>%
                 mutate(".Pred" = c(163.08951430, 206.64080278, 75.02145021,
                                    151.97182599, 213.41018590, 65.45711904)),
               ignore_attr = TRUE)

  expect_equal(dataSWE %>%
                 mutate(YEARN = as.factor(YEARN)) %>%
                 slice(1:4) %>%
                 add_prediction(model = SWEmodel),
               dataSWE %>%
                 mutate(YEARN = as.factor(YEARN)) %>%
                 slice(1:4) %>%
                 mutate(".Pred" = c(10.538905847, 11.182629628, 11.238914122, 10.388145143)),
               ignore_attr = TRUE)


  # With other models
  expect_equal(dataSWE %>%
                 mutate(YEARN = as.factor(YEARN)) %>%
                 slice(1:4) %>%
                 mutate("G1_ID" = G1, "G2_ID" = G2, L1_ID = L1, L2_ID = L2, "AV" = 0.24) %>%
                 add_prediction(model = SWEmodel %>% `class<-`("gls")),
               dataSWE %>%
                 mutate(YEARN = as.factor(YEARN)) %>%
                 slice(1:4) %>%
                 mutate("G1_ID" = G1, "G2_ID" = G2, L1_ID = L1, L2_ID = L2, "AV" = 0.24) %>%
                 mutate(".Pred" = c(10.538905847, 11.182629628, 11.238914122, 10.388145143)),
               ignore_attr = TRUE)

  expect_equal(dataSWE %>%
                 mutate(YEARN = as.factor(YEARN)) %>%
                 slice(1:4) %>%
                 mutate("G1_ID" = G1, "G2_ID" = G2, L1_ID = L1, L2_ID = L2, "AV" = 0.24) %>%
                 add_prediction(interval = "conf", model = SWEmodel %>% `class<-`("gls")),
               dataSWE %>%
                 mutate(YEARN = as.factor(YEARN)) %>%
                 slice(1:4) %>%
                 mutate("G1_ID" = G1, "G2_ID" = G2, L1_ID = L1, L2_ID = L2, "AV" = 0.24) %>%
                 mutate(".Pred" = c(10.538905847, 11.182629628, 11.238914122, 10.388145143),
                        ".Lower" = c(9.558136138, 10.201859920, 10.258144413, 9.407375434),
                        ".Upper" = c(11.519675556, 12.163399337, 12.219683831, 11.368914852)),
               ignore_attr = TRUE)

  # With coefficients
  expect_equal(all_data %>%
                 add_prediction(coefficients = mod$coefficients),
               all_data %>%
                 mutate(".Pred" = predict(mod, newdata = all_data)),
               ignore_attr = TRUE)

  # With coeff_cols
  expect_equal(all_data %>%
                 add_prediction(coefficients = unname(mod$coefficients),
                                coeff_cols = c(1:4, 12, 13, 15)),
               all_data %>%
                 mutate(".Pred" = predict(mod, newdata = all_data)),
               ignore_attr = TRUE)

  expect_equal(all_data %>%
                 add_prediction(coefficients = unname(mod$coefficients),
                                coeff_cols = c(paste0("p", 1:4), "AV", "nitrogen50", "densityhigh")),
               all_data %>%
                 mutate(".Pred" = predict(mod, newdata = all_data)),
               ignore_attr = TRUE)

  # No coefficient names or coeff_cols
  expect_equal(all_data %>%
                 select(1:4, 12, 13, 15) %>%
                 add_prediction(coefficients = unname(mod$coefficients)),
               all_data %>%
                 select(1:4, 12, 13, 15) %>%
                 mutate(".Pred" = predict(mod, newdata = all_data)),
               ignore_attr = TRUE)

  # Errors
  expect_error(add_prediction(), "`data` cannot be empty")
  expect_error(add_prediction(pred_data, model = mod,
                              conf.level = 2),
               "`conf.level` should be a value between 0 and 1")
  expect_error(add_prediction(pred_data, coefficients = "a"),
               "`coefficients` should be a numeric vector")
  expect_error(add_prediction(pred_data, coefficients = coef(mod)),
               "All coefficient names should be present in the data")
  expect_error(add_prediction(all_data, coefficients = coef(mod) %>% unname(),
                              coeff_cols = 1:4),
               "The number of values specified for selecting")
  expect_error(add_prediction(all_data, coefficients = coef(mod) %>% unname()),
               "If coeficients are not named, then the number of")

  expect_error(all_data %>%
                 select(1:4, 12, 13, 15) %>%
                 add_prediction(interval = "conf",
                                coefficients = unname(mod$coefficients),
                                vcov = list("A"= 1)),
               "`vcov` should be a numeric matrix")
  expect_error(all_data %>%
                 select(1:4, 12, 13, 15) %>%
                 add_prediction(interval = "conf",
                                coefficients = unname(mod$coefficients),
                                vcov = matrix(1, nrow = 5, ncol=6)),
               "`vcov` should be a symmetric square matrix")

  expect_error(all_data %>%
                 select(1:4, 12, 13, 15) %>%
                 add_prediction(interval = "conf",
                                coefficients = unname(mod$coefficients),
                                vcov = matrix(1, nrow = 5, ncol=5)),
               "The number of rows and columns in `vcov` should be")

  expect_error(dataSWE %>%
                 mutate(YEARN = as.factor(YEARN)) %>%
                 slice(1:4) %>%
                 mutate("G1_ID" = G1, "G2_ID" = G2, L1_ID = L1, L2_ID = L2, "AV" = 0.24) %>%
                 add_prediction(interval = "conf", model = SWEmodel %>% `class<-`("goog")),
               "It wasn't possible to automatically generate predictions")


  expect_error(dataSWE %>%
                  mutate(YEARN = as.factor(YEARN)) %>%
                  slice(1:4) %>%
                  mutate("G1_ID" = G1, "G2_ID" = G2, L1_ID = L1, L2_ID = L2) %>%
                  add_prediction(interval = "conf", model = SWEmodel %>% `class<-`("gls")) %>%
                 suppressWarnings() %>% suppressMessages(),
                 "It wasn't possible to automatically")

  expect_warning(all_data %>%
                 select(1:4, 12, 13, 15) %>%
                 add_prediction(interval = "conf",
                                coefficients = unname(mod$coefficients)),
               "`vcov` was not specified so uncertainty")

  # Linking DI models multi
  expect_error(link_DImodelsMulti(mod), "The model object")

  expect_equal(link_DImodelsMulti(SWEmodel),
               list("YEARN" = factor(c(1, 2, 3))))

  expect_equal(link_DImodelsMulti(SWEmodel, add_var = list("YEARN" = c(1, 3))),
               list("YEARN" = c(1, 3)))

  expect_equal(link_DImodelsMulti(belModel),
               list("Var" = factor(c("N", "Sown", "Weed"))))

  expect_equal(link_DImodelsMulti(belModel, add_var = list("Var" = factor(c("Sown")))),
               list("Var" = factor(c("Sown"))))

})
