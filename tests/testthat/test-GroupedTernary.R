
test_that("Grouped ternary works as expected", {
  library(DImodels)
  library(vdiffr)
  data("sim4")

  mod_data <- sim4 %>% filter(treatment %in% c(50, 150))
  species <- paste0("p",1:6)
  FG <- c("Gr", "Gr", "Le", "Le", "He", "He")

  mod <- DI(y = "response", prop = 3:8, data = mod_data,
            DImodel = "FG", FG = FG) %>% suppressMessages() %>% suppressWarnings()

  expect_doppelganger(title = "GT basic DImodels",
                      fig = grouped_ternary(model = mod,
                                            resolution = 0.5) %>% suppressWarnings() %>% suppressMessages())

  expect_doppelganger(title = "GT add var DImodels",
                      fig = grouped_ternary(model = mod,
                                            resolution = 0.5,
                                            add_var = list("treatment" = c(50, 150)),
                                            nrow = 2) %>% suppressMessages() %>% suppressWarnings())

  expect_doppelganger(title = "GT single add var DImodels",
                      fig = grouped_ternary(model = mod,
                                            resolution = 0.5,
                                            add_var = list("treatment" = c(50)),
                                            nrow = 2) %>% suppressMessages() %>% suppressWarnings())

  # Function also works with single FG in any group with manual splits
  expect_doppelganger(title = "GT manual split, single species in FG",
                      fig =   grouped_ternary(model = mod,
                                              FG = c("G", "G", "G", "L", "L", "H"),
                                              values = c(1, 0, 0, 0.5,0.5, 1),
                                              add_var = list("treatment" = c(50)),
                                              resolution = 0.5) %>% suppressMessages())

  # Conditional ternaries at the functional group level
  expect_doppelganger(title = "GT over three FG as default",
                      fig = grouped_ternary(model = mod,
                                            FG = c("G1", "G2", "L", "L", "H", "H"),
                                            values = c(1, 1, 0.5,0.5, 0.75, 0.25),
                                            add_var = list("treatment" = c(50)),
                                            resolution = 0.5) %>% suppressMessages() %>%
                        suppressWarnings())

  # Choose communities to show in ternary, also leave one empty so it's assumed 0
  expect_doppelganger(title = "GT over three FG with cust",
                      fig = grouped_ternary(model = mod,
                                            FG = c("G1", "G2", "L1", "L2", "H", "H"),
                                            values = c(1, 1, 1, 1, 0.75, 0.25),
                                            tern_vars = c("L1", "H", "G1"),
                                            conditional = data.frame("G2" = c(0, 0.25)),
                                            add_var = list("treatment" = c(50)),
                                            resolution = 0.5) %>% suppressMessages())

  # Data is returned when plot = FALSE
  ret_data <- grouped_ternary(model = mod,
                              values = rep(0.5, 6),
                              add_var = list("treatment" = c(50)),
                              resolution = 0.5,
                              plot = FALSE) %>% suppressMessages()
  exp_data <- grouped_ternary_data(model = mod,
                                   values = rep(0.5, 6),
                                   add_var = list("treatment" = c(50)),
                                   resolution = 0.5,
                                   FG = FG,
                                   prop = species) %>% suppressMessages()
  expect_equal(ret_data, exp_data)

  # Also make sure function works with _data and _plot function
  expect_doppelganger(title = "GT with DImodels data manual",
                      fig = grouped_ternary_plot(data = exp_data %>% rename("my_col" = .Pred),
                                                 col_var = "my_col",
                                                 show_axis_guides = TRUE,
                                                 show_axis_labels = TRUE,
                                                 tern_labels = c("G", "P", "P"),
                                                 axis_label_size = 8,
                                                 vertex_label_size = 6,
                                                 nlevels = 10,
                                                 contour_text = TRUE) %>%
                        suppressMessages() %>% suppressWarnings())

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


  # grouped_ternary_data()
  expect_error(grouped_ternary_data(), "`prop` cannot be empty")
  expect_error(grouped_ternary_data(prop = species), "`FG` argument cannot be empty")

  expect_error(grouped_ternary_data(model = mod,
                                    prop = species,
                                    tern_vars = c(1, 2, 3),
                                    FG = c("G", "G", "L1", "L2", "H", "H"),
                                    values = c(1, 0, 1, 1, 0.35, 0.65),
                                    add_var = list("treatment" = c(50)),
                                    resolution = 0.5) %>% suppressWarnings(),
               "`tern_vars` should be a character vector of length 3")

  expect_error(grouped_ternary_data(model = mod,
                                    prop = species,
                                    tern_vars = c("G", "L1", "L2", "H"),
                                    FG = c("G", "G", "L1", "L2", "H", "H"),
                                    values = c(1, 0, 1, 1, 0.35, 0.65),
                                    add_var = list("treatment" = c(50)),
                                    resolution = 0.5) %>% suppressWarnings(),
               "`tern_vars` cannot have more than three elements.")

  expect_error(grouped_ternary_data(model = mod,
                                    prop = species,
                                    tern_vars = c("G", "L", "L"),
                                    FG = c("G", "G", "L1", "L2", "H", "H"),
                                    values = c(1, 0, 1, 1, 0.35, 0.65),
                                    add_var = list("treatment" = c(50)),
                                    resolution = 0.5) %>% suppressWarnings(),
               "Choose three values from")

  expect_error(grouped_ternary_data(model = mod,
                                    prop = species,
                                    conditional = "list",
                                    FG = c("G", "G", "L1", "L2", "H", "H"),
                                    values = c(1, 0, 1, 1, 0.35, 0.65),
                                    add_var = list("treatment" = c(50)),
                                    resolution = 0.5) %>% suppressWarnings(),
               "`conditional` should be a data-frame")

  expect_warning(grouped_ternary_data(model = mod,
                                      prop = species,
                                      conditional = data.frame("H1" = 0),
                                      FG = c("G", "G", "L1", "L2", "H1", "H2"),
                                      values = c(1, 0, 1, 1, 1, 1),
                                      add_var = list("treatment" = c(50)),
                                      resolution = 0.5) %>% suppressMessages(),
               "After conditioning the simplex space at the specified values")

  # grouped_ternary_plot()
  expect_error(grouped_ternary_plot(), "`data` cannot be empty")
  expect_error(grouped_ternary_plot(data = mod_data),
               "All variables necessary for creating the `grouped_ternary_plot` are not present in")

  # grouped_ternary()
  expect_error(grouped_ternary(mod = mod_data), "`model` should be a regression model")
  expect_error(grouped_ternary(mod = mod %>% `attr<-`("FG", NULL)), "`FG` argument cannot be empty")
  expect_error(grouped_ternary(mod = mod, FG = 1:6),
               "`FG` was specified as a")
  expect_error(grouped_ternary(mod = mod, FG = c("G", "G", "L", "L")),
               "`FG` has length")
  expect_error(grouped_ternary(mod = mod, FG = FG, values = "g"),
               "`values` was specified as a")
  expect_error(grouped_ternary(mod = mod, FG = FG, values = 2),
               "`values` was specified with")
  expect_error(grouped_ternary(mod = mod, FG = FG, values = c(0, 1, 1)),
               "`values` has length")
  expect_error(grouped_ternary(mod = mod, FG = FG, values = c(0, 1, 1, 0, 1, 1)),
               "The species proportions within a functional group should sum to 1")

  expect_error(grouped_ternary(mod = mod, FG = c("G", "G", "G", "L", "L", "L")) %>% suppressWarnings(),
               "Ternary diagrams cannot be created for less than 3 unique groups")


})

