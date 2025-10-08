
test_that("Conditional ternary works as expected", {
  library(DImodels)
  library(vdiffr)
  data("sim4")

  mod_data <- sim4 %>% filter(treatment %in% c(50, 150))
  species <- paste0("p",1:6)
  FG <- c("Gr", "Gr", "Le", "Le", "He", "He")

  mod <- DI(y = "response", prop = 3:8, data = mod_data,
            DImodel = "FG", FG = FG) %>% suppressMessages() %>% suppressWarnings()

  expect_doppelganger(title = "CT DImodels",
                      fig = conditional_ternary(model = mod,
                                                resolution = 0.5) %>% suppressWarnings() %>% suppressMessages())

  expect_doppelganger(title = "CT add var in DImodels",
                      fig = conditional_ternary(model = mod,
                                                resolution = 0.5,
                                                add_var = list("treatment" = c(50, 150)),
                                                nrow = 2) %>% suppressMessages() %>% suppressWarnings())

  expect_doppelganger(title = "CT single add var DImodels",
                      fig = conditional_ternary(model = mod,
                                                resolution = 0.5,
                                                add_var = list("treatment" = c(50)),
                                                nrow = 2) %>% suppressMessages() %>% suppressWarnings())

  # Function also works as grouped_ternary if FG is specified
  expect_doppelganger(title = "CT manual split, single FG species",
                      fig =   conditional_ternary(model = mod,
                                                  FG = c("G", "G", "G", "L", "L", "H"),
                                                  values = c(1, 0, 0, 0.5,0.5, 1),
                                                  add_var = list("treatment" = c(50)),
                                                  resolution = 0.5) %>% suppressMessages())

  # Choose species to show in ternary, also leave one empty so it's assumed 0
  expect_doppelganger(title = "CT over three FG with cust",
                      fig = conditional_ternary(model = mod,
                                                tern_vars = c("p1", "p2", "p5"),
                                                conditional = data.frame("p6" = c(0, 0.25)),
                                                add_var = list("treatment" = c(50)),
                                                resolution = 0.5) %>% suppressMessages())

  # Data is returned when plot = FALSE
  ret_data <- conditional_ternary(model = mod,
                                  tern_vars = c("p1", "p2", "p3"),
                                  conditional = data.frame("p5" = c(0, .5, 0.75),
                                                           "p6" = c(0.75, 0.25, 0)),
                                  add_var = list("treatment" = c(50)),
                                  resolution = 0.5,
                                  plot = FALSE) %>% suppressMessages()
  exp_data <- conditional_ternary_data(model = mod,
                                       tern_vars = c("p1", "p2", "p3"),
                                       conditional = data.frame("p5" = c(0, .5, 0.75),
                                                                "p6" = c(0.75, 0.25, 0)),
                                       add_var = list("treatment" = c(50)),
                                       resolution = 0.5,
                                       prop = species) %>% suppressMessages()
  expect_equal(ret_data, exp_data)

  # Also make sure function works with _data and _plot function
  expect_doppelganger(title = "CT DImodels data manual",
                      fig = conditional_ternary_plot(data = exp_data %>% rename("my_col" = .Pred),
                                                     col_var = "my_col",
                                                     show_axis_guides = TRUE,
                                                     show_axis_labels = TRUE,
                                                     tern_labels = c("G", "P", "P"),
                                                     axis_label_size = 8,
                                                     vertex_label_size = 6,
                                                     nlevels = 10) %>%
                        suppressMessages() %>% suppressWarnings())

  # Ensure conditional ternary works for systems with three species
  expect_equal(conditional_ternary_data(prop = species[1:3],
                                        tern_vars = species[1:3],
                                        resolution = 0.5,
                                        prediction = FALSE),
               ternary_data(prop = species[1:3],
                            resolution = 0.5,
                            prediction = FALSE))

})


# Expected errors from function
test_that("Common errors are thrown", {
  library(DImodels)
  data("sim4")

  mod_data <- sim4 %>% filter(treatment %in% c(50, 150))
  species <- paste0("p",1:6)
  FG <- c("Gr", "Gr", "Le", "Le", "He", "He")

  mod <- DI(y = "response", prop = 3:8, data = mod_data,
            DImodel = "FG", FG = FG) %>% suppressMessages() %>% suppressWarnings()


  # conditional_ternary_data()
  expect_error(conditional_ternary_data(), "`prop` cannot be empty")
  expect_error(conditional_ternary_data(prop = 1:6), "`prop` should be a character vector")
  expect_error(conditional_ternary_data(prop = species[1:2]), "Ternary diagrams can only be created for models with three or more")

  expect_error(conditional_ternary_data(prop = species,
                                        conditional = data.frame(p1 = 0, p2 = 0, p3 = 0, p4 = 0)) %>% suppressWarnings(),
               "only 2 variables are left")

  expect_error(conditional_ternary_data(prop = species,
                                        tern_vars = 1:3),
               "`tern_vars` should be a character vector of length 3")

  expect_error(conditional_ternary_data(prop = species,
                                        tern_vars = c("p1", "p3", "p7")),
               "All values specified in `tern_vars` should be present in `prop`.")

  expect_error(conditional_ternary_data(prop = species,
                                        prediction = FALSE,
                                        tern_vars = c("p1", "p3", "p6"),
                                        conditional = data.frame("p7" = 0)),
               "All variables specified in `conditional` should be present in `prop`.")

  expect_error(conditional_ternary_data(prop = species,
                                    FG = c("G1", "G2", "L1", "L2", "H1", "H2"),
                                    prediction = FALSE,
                                    values = c(1, 1, 1, 1,1,1),
                                    tern_vars = c("G1", "G2", "L1"),
                                    conditional = data.frame("p2" = 0)),
               "If specifying the `FG` parameter")

  expect_error(conditional_ternary_data(prop = species,
                                        tern_vars = c("p1", "p3", "p5", "p6")),
               "`tern_vars` should have exactly three elements.")

  expect_error(conditional_ternary_data(model = mod,
                                        prop = species,
                                        conditional = "list",
                                        add_var = list("treatment" = c(50)),
                                        resolution = 0.5) %>% suppressWarnings(),
               "`conditional` should be a data-frame")

  expect_error(conditional_ternary_data(prop = species,
                                        tern_vars = c("p1", "p2", "p3"),
                                        conditional = data.frame(p1 = 0, p2 = 0, p3 = 0, p4 = 0)),
               "The same variables can't be specified in both `conditional` and `tern_vars`.")

  expect_error(conditional_ternary_data(prop = species,
                                        tern_vars = c("p1", "p2", "p3"),
                                        conditional = data.frame(p4 = c(0, 0), p5 = c("0", 0))),
               "The values specified for conditioning in ")

  expect_error(conditional_ternary_data(prop = species,
                                        tern_vars = c("p1", "p2", "p3"),
                                        conditional = data.frame(p4 = c(2, 2), p5 = c(0, 0))),
               "The values specified for conditioning a particular slice should all be between 0 and 1")

  expect_error(conditional_ternary_data(prop = species,
                                        tern_vars = c("p1", "p2", "p3"),
                                        conditional = data.frame(p4 = c(0.5, 0.75), p5 = c(0.75, 0))),
               "The values specified for conditioning a particular slice should have a sum less than 1")

  # conditional_ternary_plot()
  expect_error(conditional_ternary_plot(), "`data` cannot be empty")
  expect_error(conditional_ternary_plot(data = mod_data),
               "All variables necessary for creating the `conditional_ternary_plot` are not present in")

  # conditional_ternary()
  expect_error(conditional_ternary(mod = mod_data), "`model` should be a regression model")

})

