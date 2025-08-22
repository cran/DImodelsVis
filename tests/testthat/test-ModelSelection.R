
test_that("Model selection works as expected", {
  library(DImodels)
  library(vdiffr)
  data("sim2")

  mod_data <- sim2
  mod_ID <- DI(y = "response", prop = 3:6, data = mod_data,
               DImodel = "ID", estimate_theta = TRUE) %>% suppressMessages()
  mod_AV <- DI(y = "response", prop = 3:6, data = mod_data,
               DImodel = "AV", estimate_theta = TRUE) %>% suppressMessages()
  mod_FG <- DI(y = "response", prop = 3:6, data = mod_data, FG = c("G", "G", "L", "L"),
               DImodel = "FG", estimate_theta = TRUE) %>% suppressMessages()
  mod_ADD <- DI(y = "response", prop = 3:6, data = mod_data,
                DImodel = "ADD", estimate_theta = TRUE) %>% suppressMessages()
  mod_FULL <- DI(y = "response", prop = 3:6, data = mod_data,
                 DImodel = "FULL", estimate_theta = TRUE) %>% suppressMessages()

  mod_list <- list(mod_ID, mod_AV, mod_FG, mod_ADD, mod_FULL)

  expect_doppelganger(title = "MS with breakup",
                      fig = model_selection(models = mod_list,
                                            sort = TRUE,
                                            breakup = TRUE,
                                            metric = c("AICc")) %>% suppressMessages() %>% suppressWarnings())

  expect_doppelganger(title = "MS without breakup",
                      fig = model_selection(models = mod_list,
                                            sort = TRUE,
                                            breakup = FALSE) %>% suppressMessages() %>% suppressWarnings())

  expect_doppelganger(title = "MS multiple metrics",
                      fig = model_selection(models = list(mod_ID, mod_AV, mod_FG, mod_ADD, mod_FULL),
                                            sort = TRUE,
                                            metric = c("AIC", "AICc", "BIC"),
                                            breakup = FALSE) %>% suppressMessages() %>% suppressWarnings())

  # Data is returned when plot = FALSE
  ret_data <- model_selection(models = mod_list,
                              sort = TRUE,
                              metric = c("AIC", "AICc", "BIC"),
                              model_names = c("ID", "AV", "FG", "ADD", "FULL"),
                              breakup = FALSE,
                              plot = FALSE) %>% suppressMessages()
  exp_data <- model_selection_data(models = mod_list,
                                   sort = TRUE,
                                   model_names = c("ID", "AV", "FG", "ADD", "FULL"),
                                   metric = c("AIC", "AICc", "BIC"),
                                   breakup = FALSE) %>% suppressMessages()
  expect_equal(ret_data, exp_data)

  # Also make sure function works with _data and _plot function
  expect_doppelganger(title = "MS data manual prepared",
                      fig = model_selection_plot(data = exp_data) %>% suppressMessages() %>% suppressWarnings())

  # Single metric
  expect_doppelganger(title = "MS data manual single metric",
                      fig = model_selection_plot(data = exp_data %>%
                                                   filter(Component=="AIC") %>%
                                                   `attr<-`("Metric", "AIC")) %>%
                        suppressMessages() %>% suppressWarnings())

  # Single metric split into two
  expect_doppelganger(title = "MS data manual with breakup",
                      fig = model_selection_data(models = mod_list,
                                                 sort = TRUE,
                                                 model_names = c("ID", "AV", "FG", "ADD", "FULL"),
                                                 metric = c("AIC"),
                                                 breakup = TRUE) %>%
                        model_selection_plot())

})


# Expected errors from function
test_that("Common errors are thrown", {
  library(DImodels)
  data("sim2")

  mod_data <- sim2
  mod_ID <- DI(y = "response", prop = 3:6, data = mod_data,
               DImodel = "ID", estimate_theta = TRUE) %>% suppressMessages()
  mod_AV <- DI(y = "response", prop = 3:6, data = mod_data,
               DImodel = "AV", estimate_theta = TRUE) %>% suppressMessages()
  mod_FG <- DI(y = "response", prop = 3:6, data = mod_data, FG = c("G", "G", "L", "L"),
               DImodel = "FG", estimate_theta = TRUE) %>% suppressMessages()
  mod_ADD <- DI(y = "response", prop = 3:6, data = mod_data,
                DImodel = "ADD", estimate_theta = TRUE) %>% suppressMessages()
  mod_FULL <- DI(y = "response", prop = 3:6, data = mod_data,
                 DImodel = "FULL", estimate_theta = TRUE) %>% suppressMessages()

  mod_list <- list(mod_ID, mod_AV, mod_FG, mod_ADD, mod_FULL)


  # model_selection_data()
  expect_error(model_selection(), "`models` cannot be empty")
  expect_error(model_selection_data(), "`models` cannot be empty")
  expect_error(model_selection_data(list("A" = 1, "B" = 1)), "`models` should be a list of regression models")

  expect_error(model_selection(list(mod_ID, mod_AV),
                                    model_names = c("ID")),
               "`model_names` should be a character vector")

  expect_error(model_selection_data(list(mod_ID, mod_AV),
                                    metric = "IC",
                                    model_names = c("ID", "AV")),
               "`metric` should be one of")

  expect_warning(model_selection_data(list(mod_ID, mod_AV),
                                    metric = c("AIC", "BIC"),
                                    breakup = TRUE,
                                    model_names = c("ID", "AV")),
               "when multiple metrics are chosen")

  expect_warning(model_selection_data(list(mod_ID, mod_AV),
                                      metric = c("deviance"),
                                      breakup = TRUE,
                                      model_names = c("ID", "AV")),
                 "Showing deviance without the breakup")

  # model_selection_plot()
  expect_error(model_selection_plot(), "`data` cannot be empty")

})

