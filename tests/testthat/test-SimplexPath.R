
test_that("Simplex path works as expected", {
  library(DImodels)
  library(vdiffr)
  data("sim2")

  mod_data <- sim2 %>% filter(block %in% c(1, 2))
  mod <- DI(y = "response", prop = 3:6, data = mod_data,
            DImodel = "AV", estimate_theta = TRUE,
            treat = "block") %>% suppressMessages()
  starts <- mod_data %>% slice(24, 26, 28, 30)
  ends <- mod_data %>% filter(p1 == 0.25) %>% slice(1)
  expect_doppelganger(title = "SP basic DImodels",
                      fig = simplex_path(model = mod,
                                         starts = starts,
                                         ends = ends,
                                         interval = "confidence",
                                         se = TRUE,
                                         facet_var = "community",
                                         FG = c("G", "G", "L", "L")) %>% suppressWarnings() %>% suppressMessages())

  ends <- mod_data %>% slice(24, 26, 28, 30)
  starts <- mod_data %>% filter(p1 == 0.25) %>% slice(1)
  expect_doppelganger(title = "SP add var in DImodels",
                      fig = simplex_path(model = mod,
                                         starts = starts,
                                         ends = ends,
                                         add_var = list("block" = factor(c(1, 2), 1:4)),
                                         interval = "confidence") %>% suppressMessages() %>% suppressWarnings())

  expect_doppelganger(title = "SP single add var in DImodels",
                      fig = simplex_path(model = mod,
                                         starts = starts,
                                         ends = ends,
                                         add_var = list("block" = factor(c(1), 1:4)),
                                         interval = "confidence") %>% suppressMessages() %>% suppressWarnings())




  # Data is returned when plot = FALSE
  ret_data <- simplex_path(model = mod,
                           starts = starts,
                           ends = ends,
                           interval = "confidence",
                           se = TRUE,
                           facet_var = "community",
                           FG = c("G", "G", "L", "L"),
                           plot = FALSE) %>% suppressMessages()
  exp_data <- simplex_path_data(model = mod,
                                prop = paste0("p", 1:4),
                                starts = starts,
                                ends = ends,
                                interval = "confidence") %>% suppressMessages()
  expect_equal(ret_data, exp_data)

  # Also make sure function works with _data and _plot function
  expect_doppelganger(title = "SP with data manually prepared",
                      fig = simplex_path_plot(data = exp_data) %>% suppressMessages() %>% suppressWarnings())

})


# Expected errors from function
test_that("Common errors are thrown", {
  library(DImodels)
  data("sim2")

  mod_data <- sim2 %>% filter(block %in% c(1, 2))
  mod <- DI(y = "response", prop = 3:6, data = mod_data,
            DImodel = "AV") %>% suppressMessages()
  starts <- mod_data %>% slice(24, 26, 28, 30)
  ends <- mod_data %>% filter(p1 == 0.25)

  # simplex_path_data()
  expect_error(simplex_path_data(), "`starts` cannot be empty")
  expect_error(simplex_path_data(starts = mod_data), "`ends` cannot be empty")
  expect_error(simplex_path_data(starts = mod_data,
                                 ends = mod_data), "`prop` cannot be empty")
  expect_error(simplex_path_data(starts = starts,
                                 ends = ends,
                                 model = mod,
                                 prop = paste0("p",1:4)),
               "The number of rows of the data in `starts` and `ends`")
  expect_error(simplex_path_data(starts = starts,
                                 ends = ends %>% mutate(p5 = p4) %>% slice(1),
                                 model = mod,
                                 prop = paste0("p",1:4)),
               "The data in `starts` and `ends` should have same columns.")

  # simplex_path_plot()
  expect_error(simplex_path_plot(), "`data` cannot be empty")
  expect_error(simplex_path_plot(data = mod_data), "`prop` is NULL and cannot be inferred from data")

  # simplex_path()
  expect_error(simplex_path(mod = mod_data), "`model` should be a regression model")
  expect_error(simplex_path(model = mod), "`starts` cannot be empty")
  expect_error(simplex_path_data(model = mod, starts = mod_data), "`ends` cannot be empty")

  expect_error(simplex_path(mod = mod,
                            ends = data.frame(p5 = 1),
                            starts = data.frame(p5 = 1)),
               "are not present in the data.")

  expect_error(simplex_path(starts = starts,
                            ends = ends %>% slice(1),
                            model = mod,
                            pie_positions = c(1, 2)),
               "`pie_positions` should be a")

})

