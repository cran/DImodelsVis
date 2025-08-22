
test_that("Model diagnostics works as expected", {
  library(DImodels)
  library(DImodelsMulti) %>% suppressMessages()
  library(vdiffr)
  data("sim2")

  mod_data <- sim2
  species <- paste0("p", 1:4)
  FG <- c("G", "G", "L", "L")
  mod <- DI(y = "response", prop = 3:6, data = mod_data, FG = FG,
             DImodel = "AV", estimate_theta = TRUE) %>% suppressMessages()


  expect_doppelganger(title = "MD basic plot",
                      fig = model_diagnostics(model = mod,
                                              which = 1:6,
                                              nrow = 1, ncol = 6) %>%
                        suppressMessages() %>% suppressWarnings())

  expect_doppelganger(title = "MD basic plot extreme pies",
                      fig = model_diagnostics(model = mod,
                                              which = 1:6,
                                              cook_levels = c(0, 0.1, 0.2, 0.5),
                                              only_extremes = TRUE,
                                              npoints = 5,
                                              nrow = 1, ncol = 6) %>%
                        suppressMessages() %>% suppressWarnings())


  # Also test with DImodelsMulti object
  belModel <- DImulti(prop = c("G1", "G2", "L1", "L2"), y = "Y", eco_func = c("Var", "un"),
                      unit_IDs = "Plot", theta = c(0.5, 1, 1.2), DImodel = "FG",
                      FG = c("Grass", "Grass", "Legume", "Legume"), extra_fixed = ~ Density,
                      method = "REML", data = dataBEL)

  expect_doppelganger(title = "MD with DImodelsMulti",
                      fig = model_diagnostics(model = belModel,
                                              which = 1:6,
                                              only_extremes = TRUE,
                                              npoints = 5) %>%
                        suppressMessages() %>% suppressWarnings())

  # Data is returned when plot = FALSE
  ret_data <-  model_diagnostics(model = mod,
                                 which = 1:2,
                                 only_extremes = TRUE,
                                 npoints = 5,
                                 nrow = 1, ncol = 2,
                                 plot = FALSE) %>% suppressMessages()
  exp_data <- model_diagnostics_data(model = mod) %>% suppressMessages()
  expect_equal(ret_data, exp_data)

  # Also make sure function works with _data and _plot function
  expect_doppelganger(title = "MD data manually prepared",
                      fig = model_diagnostics_plot(data = exp_data,
                                                   which = 1:2,
                                                   only_extremes = TRUE,
                                                   npoints = 5,
                                                   nrow = 1, ncol = 2) %>%
                        suppressMessages() %>% suppressWarnings())

  # Single panel
  expect_doppelganger(title = "MD data manual single metric",
                      fig = model_diagnostics_plot(data = exp_data,
                                                   which = 1,
                                                   pie_colours = get_colours(4),
                                                   only_extremes = TRUE,
                                                   npoints = 5,) %>%
                        suppressMessages() %>% suppressWarnings())

  # Also ensure it works for DImodelsMulti
  expect_doppelganger(title = "MD with DImodelsMulti separate calls",
                      fig = model_diagnostics_data(model = belModel) %>%
                        model_diagnostics_plot(which = 1:6,
                                               only_extremes = TRUE,
                                               npoints = 7) %>%
                        suppressMessages() %>% suppressWarnings())

  # Test with glm model
  mod_lm <- glm(response ~ 0 + p1 + p2 + p3 + p4,
                data = mod_data)
  class(mod_lm) <- c("glm")
  expect_doppelganger(title = "MD with non lm",
                      fig = model_diagnostics(mod_lm, prop = species,
                                              which = 1, pie_colours = get_colours(4)) %>%
                        suppressMessages() %>% suppressWarnings())
})


# Expected errors from function
test_that("Common errors are thrown", {
  library(DImodels)
  data("sim3")

  mod_data <- sim3
  prop <- paste0("p", 1:9)
  # FG <- c("G", "G", "L", "L")
  mod <- DI(y = "response", prop = 4:12, data = mod_data,
            DImodel = "AV", estimate_theta = TRUE) %>%
    suppressMessages() %>% suppressWarnings()

  exp_data <- model_diagnostics_data(model = mod) %>% suppressMessages()

  # Warnings with lm model
  mod_lm <- glm(response ~ 0 + p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9,
                data = mod_data)

  # model_diagnostics_data()
  expect_error(model_diagnostics(), "`model` cannot be empty")
  expect_error(model_diagnostics_data(), "`model` cannot be empty")

  expect_error(model_diagnostics(mod = mod, which = 7), "`which` must be a numeric vector")
  expect_error(model_diagnostics(mod = mod, npoints = -1), "`npoints` should be an integer between")
  expect_error(model_diagnostics(mod = mod, pie_colours = get_colours(3)),
               "The number of `colours` specified should")
  expect_warning(model_diagnostics(mod_lm, only_extremes = TRUE, which = 1),
                 "No compositional variables were specified in `prop`")

  # model_diagnostics_plot()
  expect_error(model_diagnostics_plot(), "`data` cannot be empty")
  expect_error(model_diagnostics_plot(exp_data, which = 7), "`which` must be a numeric vector")
  expect_error(model_diagnostics_plot(exp_data, npoints = -1), "`npoints` should be an integer between")
  expect_error(model_diagnostics_plot(exp_data, pie_colours = get_colours(3)),
               "The number of `colours` specified should")

  expect_warning(model_diagnostics_plot(exp_data %>% `attr<-`("prop", NULL), only_extremes = TRUE),
                 "No compositional variables were specified in `prop`")



})

