
test_that("Gradient change works as expected", {
  library(DImodels)
  library(vdiffr)
  data("sim2")

  species <- paste0("p", 1:4)
  mod_data <- sim2 %>% filter(block %in% c(1, 2))
  mod <- DI(y = "response", prop = 3:6, data = mod_data,
            DImodel = "ADD", estimate_theta = TRUE,
            treat = "block") %>% suppressMessages()


  expect_doppelganger(title = "GC basic DImodels no facet",
                      fig = gradient_change(model = mod,
                                            average = TRUE) %>% suppressWarnings() %>% suppressMessages())

  expect_doppelganger(title = "GC basic DImodels",
                      fig = gradient_change(model = mod,
                                            facet_var = "block",
                                            average = TRUE) %>% suppressWarnings() %>% suppressMessages())

  expect_doppelganger(title = "GC add var DImodels",
                      fig = gradient_change(model = mod,
                                            pie_colours = get_colours(4),
                                            gradient = "evenness",
                                            add_var = list("block" = factor(c(1:2), 1:4))) %>%
                        suppressWarnings %>% suppressMessages())

  expect_doppelganger(title = "GC add var, single, custom data",
                      fig = gradient_change(model = mod,
                                            data = get_equi_comms(4, variables = species),
                                            pie_colours = get_colours(4),
                                            gradient = "evenness",
                                            nrow = 2, ncol = 1,
                                            add_var = list("block" = factor(c(1:2), 1:4))) %>%
                        suppressMessages())

  expect_doppelganger(title = "GC with pie_data",
                      fig = gradient_change(model = mod,
                                            average = FALSE,
                                            data = get_equi_comms(4, variables = species),
                                            pie_data = mod_data %>% filter(community%in% c(1:5)) %>% select(-block),
                                            pie_radius = 0.3,
                                            points_size = 2,
                                            pie_colours = get_colours(4),
                                            gradient = "richness",
                                            add_var = list("block" = factor(c(1), 1:4))) %>%
                        suppressMessages())



  # Data is returned when plot = FALSE
  ret_data <- gradient_change(model = mod,
                              data = get_equi_comms(4, variables = species),
                              pie_colours = get_colours(4),
                              gradient = "evenness",
                              nrow = 2, ncol = 1,
                              add_var = list("block" = factor(c(1:2), 1:4)),
                              plot = FALSE) %>% suppressMessages()
  exp_data <- gradient_change_data(model = mod,
                                   prop = species,
                                   data = get_equi_comms(4, variables = species),
                                   gradient = "evenness",
                                   add_var = list("block" = factor(c(1:2), 1:4))) %>%
    suppressMessages()
  expect_equal(ret_data, exp_data)

  # Also make sure function works with _data and _plot function
  expect_doppelganger(title = "GC DImodels data manual",
                      fig = gradient_change_plot(data = exp_data) %>% suppressMessages() %>% suppressWarnings())

})


# Expected errors from function
test_that("Common errors are thrown", {
  library(DImodels)
  data("sim2")

  species <- paste0("p", 1:4)
  mod_data <- sim2 %>% filter(block %in% c(1, 2))
  mod <- DI(y = "response", prop = 3:6, data = mod_data,
            DImodel = "AV") %>% suppressMessages()

  exp_data <- gradient_change_data(model = mod,
                                   prop = species,
                                   data = get_equi_comms(4, variables = species),
                                   gradient = "evenness",
                                   add_var = list("block" = factor(c(1:2), 1:4))) %>%
    suppressMessages()

  # Visualise_effects_data()
  expect_error(gradient_change_data(), "`data` cannot be empty")
  expect_error(gradient_change_data(data = mod_data), "`prop` cannot be empty")

  # gradient_change_plot()
  expect_error(gradient_change_plot(), "`data` cannot be empty")
  expect_error(gradient_change_plot(data = mod_data, prop = species),
               "All variables necessary for creating the")
  expect_error(gradient_change_plot(data = exp_data, prop = species,
                                    pie_colours = get_colours(3)),
               "The number of `pie_colours` specified")
  expect_error(gradient_change_plot(data = exp_data %>% `attr<-`("prop", NULL),
                                    pie_data = exp_data %>% filter(block == 1)),
               "when `pie_data` is specified")


  # gradient_change()
  expect_error(gradient_change(mod = mod_data), "`model` should be a regression model")
  expect_error(gradient_change(mod = mod, data = list(p1 = 1)), "`data` should be a data frame or tibble")

})

