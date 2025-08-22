
test_that("Visualise effects works as expected", {
  library(DImodels)
  library(vdiffr)
  data("sim2")

  mod_data <- sim2 %>% filter(block %in% c(1, 2))
  mod <- DI(y = "response", prop = 3:6, data = mod_data,
            DImodel = "AV", estimate_theta = TRUE,
            treat = "block") %>% suppressMessages()
  expect_doppelganger(title = "VE with basic DImodels",
                      fig = visualise_effects(model = mod,
                                              interval = "confidence",
                                              average = TRUE,
                                              se = TRUE,
                                              FG = c("G", "G", "L", "L")) %>% suppressWarnings() %>% suppressMessages())

  expect_doppelganger(title = "VE with additional variables in DImodels",
                      fig = visualise_effects(model = mod,
                                              data = mod_data %>% slice(24, 26, 28, 30) %>% select(-block),
                                              add_var = list("block" = factor(c(1, 2), 1:4)),
                                              var_interest = "p1",
                                              interval = "confidence",
                                              average = TRUE,
                                              se = TRUE,
                                              FG = c("G", "G", "L", "L")) %>% suppressMessages())

  expect_doppelganger(title = "VE with decrease",
                      fig = visualise_effects(model = mod,
                                              interval = "confidence",
                                              effect = "decrease",
                                              data = mod_data %>% slice(24, 26, 28, 30) %>% select(-block),
                                              add_var = list("block" = factor(c(1), 1:4)),
                                              var_interest = "p1",
                                              average = TRUE,
                                              se = TRUE,
                                              FG = c("G", "G", "L", "L")) %>% suppressWarnings() %>% suppressMessages())

  expect_doppelganger(title = "VE with both increase and decrease",
                      fig = visualise_effects(model = mod,
                                              interval = "confidence",
                                              effect = "both",
                                              data = mod_data %>% slice(24, 26, 28, 30) %>% select(-block),
                                              add_var = list("block" = factor(c(1), 1:4)),
                                              var_interest = "p1",
                                              average = TRUE,
                                              se = TRUE,
                                              FG = c("G", "G", "L", "L")) %>% suppressWarnings() %>% suppressMessages())

  # Function works if a specific species is missing
  expect_doppelganger(title = "Species missing to be assumed 0",
                      fig = visualise_effects(model = mod,
                                              effect = "decrease",
                                              data = mod_data %>% slice(24, 26, 28) %>% select(p1:p3),
                                              var_interest = "p1") %>% suppressWarnings() %>% suppressMessages())

  # Data is returned when plot = FALSE
  ret_data <- visualise_effects(model = mod,
                                data = mod_data %>% slice(26),
                                var_interest = "p1",
                                interval = "confidence",
                                average = TRUE,
                                se = TRUE,
                                FG = c("G", "G", "L", "L"),
                                plot = FALSE) %>% suppressMessages()
  exp_data <- visualise_effects_data(model = mod,
                                     prop = paste0("p", 1:4),
                                     data = mod_data %>% slice(26),
                                     var_interest = "p1",
                                     interval = "confidence") %>% suppressMessages()
  expect_equal(ret_data, exp_data)

  # Also make sure function works with _data and _plot function
  expect_doppelganger(title = "VE with DImodels data manually prepared",
                      fig = visualise_effects_plot(data = exp_data) %>% suppressMessages() %>% suppressWarnings())

  data(Bell)
  mod_bell <- DI(y = "response", prop = paste0("p", 1:72), data = Bell,
             DImodel = "ID") %>% suppressMessages() %>% suppressWarnings()

  # For too large datasets pick the first 100 unique communities
  expect_doppelganger(title = "VE for large data",
                      fig = visualise_effects(mod_bell, var_interest = "p1") %>% suppressMessages() %>% suppressWarnings())

})


# Expected errors from function
test_that("Common errors are thrown", {
  library(DImodels)
  data("sim2")

  mod_data <- sim2 %>% filter(block %in% c(1, 2))
  mod <- DI(y = "response", prop = 3:6, data = mod_data,
            DImodel = "AV") %>% suppressMessages()

  # Visualise_data()
  expect_error(visualise_effects_data(), "`data` cannot be empty")
  expect_error(visualise_effects_data(data = mod_data), "`prop` cannot be empty")
  expect_error(visualise_effects_data(data = mod_data, prop = paste0("p",1:4),
                                      var_interest = 1:4),
               "`var_interest` should be a character vector")
  expect_error(visualise_effects_data(data = mod_data, prop = paste0("p",1:4),
                                      var_interest = "p5"),
               "not present in `prop`")
  expect_error(visualise_effects_data(model = mod,
                                      effect = "decrease",
                                      data = mod_data %>% slice(30),
                                      prop = paste0("p",1:4),
                                      var_interest = "p1"),
               "Can't visualise effect of decrease")
  expect_error(visualise_effects_data(model = mod,
                                      effect = "increase",
                                      data = mod_data %>% slice(30),
                                      prop = paste0("p",1:4),
                                      var_interest = "p4"),
               "Can't visualise effect of increase of a variable if only observations with 100% of variable of interest are present")

  # Visualise_plot()
  expect_error(visualise_effects_plot(), "`data` cannot be empty")
  expect_error(visualise_effects_plot(data = mod_data), "`prop` is NULL and cannot be inferred from data")

  # Visualise_effect()
  expect_error(visualise_effects(mod = mod_data), "`model` should be a regression model")
  expect_error(visualise_effects(mod = mod, data = list(p1 = 1)), "`data` was not a data.frame")
  expect_error(visualise_effects(mod = mod, data = data.frame(p5 = 1)),
               "Update the column names in `data` to ensure they match")

})

