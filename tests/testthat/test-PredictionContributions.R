
test_that("Prediction contributions works as expected", {
  library(DImodels)
  library(vdiffr)
  data("Switzerland")

  species <- paste0("p", 1:4)
  mod_data <- Switzerland

  mod <- DI(y = "yield", prop = species, data = mod_data,
            DImodel = "AV", estimate_theta = TRUE,
            treat = "nitrogen", density = "density") %>% suppressMessages()

  pred_data <- mod_data %>%
    mutate(richness = get_richness(data = ., prop = species)) %>%
    filter(richness == 1 | p1 == 0.25) %>%
    distinct(p1, p2, p3, p4, nitrogen, density, richness) %>%
    arrange(richness)

  expect_doppelganger(title = "PC with basic DImodels",
                      fig = prediction_contributions(model = mod) %>% suppressWarnings() %>% suppressMessages())

  expect_doppelganger(title = "PC basic DImodels and custom data",
                      fig = prediction_contributions(model = mod,
                                                     data = pred_data %>% filter(nitrogen == "150"),
                                                     facet_var = "density") %>% suppressWarnings() %>% suppressMessages())

  expect_doppelganger(title = "PC with add var in DImodels",
                      fig = prediction_contributions(model = mod,
                                                     data = pred_data %>% filter(nitrogen == "150"),
                                                     facet_var = "density",
                                                     se = TRUE,
                                                     FG = c("G", "G", "L", "L"),
                                                     conf.level = 0.99,
                                                     nrow = 2, ncol = 1,
                                                     add_var = list("nitrogen" = factor(c(50, 150), c("50", "150")))) %>%
                        suppressWarnings() %>% suppressMessages())

  expect_doppelganger(title = "PC with add var single and custom data",
                      fig = prediction_contributions(model = mod,
                                                     data = pred_data %>% filter(nitrogen == "50"),
                                                     groups = list("G" = c("p1_ID", "p2_ID"),
                                                                   "L" = c("p3_ID", "p4_ID")),
                                                     facet_var = "density",
                                                     bar_labs = "richness",
                                                     interval = "prediction",
                                                     se = TRUE,
                                                     bar_orientation = "horizontal",
                                                     FG = c("G", "G", "L", "L"),
                                                     add_var = list("nitrogen" = factor(c(150), c("50", "150")))) %>%
                        suppressMessages() %>% suppressWarnings())

  expect_doppelganger(title = "PC with pie_data",
                      fig = prediction_contributions(model = mod,
                                                     data = pred_data %>% filter(nitrogen == "50"),
                                                     groups = list("G" = c("p1_ID", "p2_ID")),
                                                     bar_labs = paste0("Comm", 1:10),
                                                     se = TRUE,
                                                     bar_orientation = "vertical") %>%
                        suppressMessages() %>% suppressWarnings())



  # Data is returned when plot = FALSE
  ret_data <- prediction_contributions(model = mod,
                                       data = pred_data %>% filter(nitrogen == "50") %>% select(-nitrogen),
                                       groups = list("G" = c("p1_ID", "p2_ID")),
                                       se = TRUE,
                                       bar_labs = NULL,
                                       add_var = list("nitrogen" = factor(c(150), c("50", "150"))),
                                       plot = FALSE) %>% suppressMessages()
  exp_data <- prediction_contributions_data(model = mod,
                                            data = pred_data %>% filter(nitrogen == "50") %>%
                                              select(-nitrogen) %>%
                                              add_ID_terms(mod) %>%
                                              add_interaction_terms(mod) %>%
                                              add_exp_str(mod),
                                            bar_labs = NULL,
                                            groups = list("G" = c("p1_ID", "p2_ID")),
                                            add_var = list("nitrogen" = factor(c(150), c("50", "150")))) %>%
    suppressMessages() %>% suppressWarnings()
  # expect_equal(ret_data, exp_data)

  # Also make sure function works with _data and _plot function
  expect_doppelganger(title = "PC with data manually prepared",
                      fig = prediction_contributions_plot(data = exp_data) %>% suppressMessages() %>% suppressWarnings())
  expect_doppelganger(title = "PC with data manually prepared",
                      fig = prediction_contributions_plot(data = ret_data) %>% suppressMessages() %>% suppressWarnings())
  # Given figures same title. So if they'd be different a warning would be thrown

  # Make sure data prep functions work with coefficients
  exp_data_mod <- prediction_contributions_data(model = mod,
                                            data = pred_data %>% filter(nitrogen == "50") %>%
                                              select(-nitrogen) %>%
                                              add_ID_terms(mod) %>%
                                              add_interaction_terms(mod) %>%
                                              add_exp_str(mod),
                                            add_var = list("nitrogen" = factor(c(50), c("50", "150"))),
                                            groups = list("G" = c("p1_ID", "p2_ID")),
                                            bar_labs = paste0("Comm", 1:10)) %>%
    suppressMessages() %>% suppressWarnings()

  exp_data_coef <- prediction_contributions_data(coefficients = coef(mod)[-8],
                                                 vcov = vcov(mod),
                                                 coeff_cols = c(7:11, 13, 15),
                                                 data = pred_data %>% filter(nitrogen == "50") %>%
                                                   select(-nitrogen) %>%
                                                   add_ID_terms(mod) %>%
                                                   add_interaction_terms(mod) %>%
                                                   add_exp_str(mod),
                                                 add_var = list("nitrogen" = factor(c(50), c("50", "150"))),
                                                 groups = list("G" = c("p1_ID", "p2_ID")),
                                                 bar_labs = paste0("Comm", 1:10)) %>%
    suppressMessages() %>% suppressWarnings()

  exp_data_coef2 <- prediction_contributions_data(coefficients = coef(mod)[-8],
                                                 vcov = vcov(mod),
                                                 data = pred_data %>% filter(nitrogen == "50") %>%
                                                   select(-nitrogen) %>%
                                                   add_ID_terms(mod) %>%
                                                   add_interaction_terms(mod) %>%
                                                   add_exp_str(mod),
                                                 groups = list("G" = c("p1_ID", "p2_ID")),
                                                 bar_labs = paste0("Comm", 1:10)) %>%
    suppressMessages() %>% suppressWarnings()

  exp_data_coef3 <- prediction_contributions_data(coefficients = unname(coef(mod)[-8]),
                                                  vcov = vcov(mod),
                                                  data = pred_data %>% filter(nitrogen == "50") %>%
                                                    select(-nitrogen) %>%
                                                    add_ID_terms(mod) %>%
                                                    add_interaction_terms(mod) %>%
                                                    add_exp_str(mod) %>%
                                                    select(7:11, 13, 15),
                                                  groups = list("G" = c(1, 2)),
                                                  bar_labs = paste0("Comm", 1:10)) %>%
    suppressMessages() %>% suppressWarnings()

  expect_equal(exp_data_mod %>% select(-.Lower, -.Upper),
               exp_data_coef %>% select(-.Lower, -.Upper),
               ignore_attr = TRUE)

  expect_equal(exp_data_coef %>% filter(nitrogen == "50") %>% select(-.add_str_ID, -nitrogen.data, -nitrogen),
               exp_data_coef2 %>% select(-nitrogen),
               ignore_attr = TRUE)

  expect_equal(exp_data_coef2 %>% select(.Pred, .Lower, .Upper, .Contributions, .Value),
               exp_data_coef3 %>% select(.Pred, .Lower, .Upper, .Contributions, .Value),
               ignore_attr = TRUE)
})


# Expected errors from function
test_that("Common errors are thrown", {
  library(DImodels)

  data("Switzerland")

  species <- paste0("p", 1:4)
  mod_data <- Switzerland

  mod <- DI(y = "yield", prop = species, data = mod_data,
            DImodel = "AV", estimate_theta = TRUE,
            treat = "nitrogen", density = "density") %>% suppressMessages()

  pred_data <- mod_data %>%
    mutate(richness = get_richness(data = ., prop = species)) %>%
    filter(richness == 1 | p1 == 0.25) %>%
    distinct(p1, p2, p3, p4, nitrogen, density, richness) %>%
    arrange(richness)

  ret_data <- prediction_contributions(model = mod,
                                       data = pred_data %>% filter(nitrogen == "50") %>% select(-nitrogen),
                                       groups = list("G" = c("p1_ID", "p2_ID")),
                                       bar_labs = paste0("Comm", 1:10),
                                       se = TRUE,
                                       add_var = list("nitrogen" = factor(c(150), c("50", "150"))),
                                       plot = FALSE) %>% suppressMessages()
  exp_data <- prediction_contributions_data(model = mod,
                                            data = pred_data %>% filter(nitrogen == "50") %>%
                                              select(-nitrogen) %>%
                                              add_ID_terms(mod) %>%
                                              add_interaction_terms(mod) %>%
                                              add_exp_str(mod),
                                            groups = list("G" = c("p1_ID", "p2_ID")),
                                            add_var = list("nitrogen" = factor(c(150), c("50", "150")))) %>%
    suppressMessages() %>% suppressWarnings()

  # Visualise_effects_data()
  expect_error(prediction_contributions_data(), "`data` cannot be empty")
  expect_error(prediction_contributions_data(data = mod_data),
               "Both `model` and `coefficients` cannot be empty")

  expect_error(prediction_contributions_data(model = mod,
                                             data = pred_data %>% filter(nitrogen == "50") %>%
                                               select(-nitrogen) %>%
                                               add_ID_terms(mod) %>%
                                               add_interaction_terms(mod) %>%
                                               add_exp_str(mod),
                                             bar_labs = c("p1", "p2")),
               "`bar_labs` should either be a character string/numeric index")

  expect_error(prediction_contributions_data(coefficients = coef(mod)[-8],
                                             data = pred_data %>% filter(nitrogen == "50") %>%
                                               select(-nitrogen) %>%
                                               add_ID_terms(mod) %>%
                                               add_interaction_terms(mod) %>%
                                               add_exp_str(mod) %>%
                                               select(-p1_ID),
                                             add_var = list("nitrogen" = factor(c(150), c("50", "150")))) %>%
                 suppressMessages() %>% suppressWarnings(),
               "All coefficient names should be present in the data as columns")

  expect_error(prediction_contributions_data(coefficients = coef(mod)[-8],
                                             vcov = vcov(mod),
                                             coeff_cols = 1:4,
                                             data = pred_data %>% filter(nitrogen == "50") %>%
                                               select(-nitrogen) %>%
                                               add_ID_terms(mod) %>%
                                               add_interaction_terms(mod) %>%
                                               add_exp_str(mod),
                                             groups = list("G" = c("p1_ID", "p2_ID")),
                                             bar_labs = paste0("Comm", 1:10)) %>%
                 suppressMessages() %>% suppressWarnings(),
               "The number of values specified for selecting and reordering")

  expect_error(prediction_contributions_data(coefficients = unname(coef(mod)[-8]),
                                             data = pred_data %>% filter(nitrogen == "50") %>%
                                               select(-nitrogen) %>%
                                               add_ID_terms(mod) %>%
                                               add_interaction_terms(mod) %>%
                                               add_exp_str(mod)) %>%
                 suppressMessages() %>% suppressWarnings(),
               "If coeficients are not named, then the number of columns")

  # prediction_contributions_plot()
  expect_error(prediction_contributions_plot(), "`data` cannot be empty")


  # prediction_contributions()
  expect_error(prediction_contributions(mod = mod_data), "`model` should be a regression model")
  expect_error(prediction_contributions(mod = mod, data = list(p1 = 1)), "`data` should be a data frame or tibble")

})

