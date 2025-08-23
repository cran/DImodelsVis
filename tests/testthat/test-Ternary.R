
test_that("Basic ternary works as expected", {
  library(DImodels)
  library(vdiffr)
  data("sim0")

  mod_data <- sim0 %>% mutate(richness = as.factor(richness))
  species <- paste0("p",1:3)

  mod <- DI(y = "response", prop = 3:5, data = mod_data,
            DImodel = "FULL") %>% suppressMessages() %>% suppressWarnings()

  contour_data <- ternary_data(prop = species, resolution = 0.5, model = mod)
  contour_data_add <- ternary_data(prop = species,
                                   resolution = 0.5,
                                   add_var = (list("group" = 1:2)),
                                   prediction = FALSE) %>%
    add_prediction(model = mod)

  expect_doppelganger(title = "Ternary with points",
                      fig = ternary_plot(data = sim0, prop = species, show = "points") %>%
                        suppressWarnings() %>% suppressMessages())

  expect_doppelganger(title = "Ternary with points and colour",
                      fig = ternary_plot(data = sim0, col_var = "richness",
                                         prop = species, show = "points") %>%
                        suppressWarnings() %>% suppressMessages())

  expect_doppelganger(title = "Ternary with points aesthetics",
                      fig = ternary_plot(data = mod_data, prop = species,
                                         show = "points",
                                         points_size = 4,
                                         show_axis_guides = TRUE,
                                         show_axis_labels = FALSE,
                                         axis_label_size = 8,
                                         vertex_label_size = 6,
                                         lower_lim = 40,
                                         upper_lim = 60,
                                         col_var = "richness",
                                         contour_text = TRUE) %>%
                        suppressMessages())

  expect_doppelganger(title = "Ternary with contours",
                      fig = contour_data %>%
                        ternary_plot() %>%
                        suppressWarnings() %>%
                        suppressMessages())

  expect_doppelganger(title = "Ternary with additional variables",
                      fig = contour_data_add %>%
                        ternary_plot(nrow = 1, ncol = 2) %>%
                        suppressWarnings() %>%
                        suppressMessages())

  expect_doppelganger(title = "Ternary with a single add var",
                      fig = contour_data_add %>%
                        filter(group==1) %>%
                        ternary_plot() %>%
                        suppressWarnings() %>%
                        suppressMessages())

  expect_doppelganger(title = "Ternary with add var and aes",
                      fig = contour_data_add %>%
                        mutate("Pred" = .Pred*10) %>%
                        ternary_plot(show_axis_guides = TRUE,
                                     show_axis_labels = TRUE,
                                     tern_labels = c("G", "P", "P"),
                                     axis_label_size = 8,
                                     vertex_label_size = 6,
                                     points_size = 4,
                                     col_var = "Pred",
                                     nlevels = 10,
                                     lower_lim = 100,
                                     upper_lim = 350,
                                     contour_text = TRUE) %>%
                        suppressWarnings() %>%
                        suppressMessages())

})


# Expected errors from function
test_that("Common errors are thrown", {
  library(DImodels)
  data("sim0")

  mod_data <- sim0
  species <- paste0("p",1:3)

  mod <- DI(y = "response", prop = 3:5, data = mod_data,
            DImodel = "FULL") %>% suppressMessages() %>% suppressWarnings()

  contour_data <- ternary_data(prop = species, resolution = 0.5, model = mod)

  # ternary_data()
  expect_error(ternary_data(prop = 1:6, model = mod), "The value specified in `prop` is not a valid character")
  expect_error(ternary_data(prop = paste0("p", 1:6), model = mod),
               "works only for models with three compositional predictors")

  expect_warning(ternary_data(prop = species, prediction = FALSE,
                              resolution = 100),
                 "`resolution` should be a number with values between 0 and 10")


  # ternary_plot()
  expect_error(ternary_plot(), "`data` cannot be empty")
  expect_error(ternary_plot(data = sim0), "`prop` was not specified and can not be inferred from the `data` either.")


  expect_error(ternary_plot(data = sim0, show = "points",
                            prop = species, tern_labels = c("p1", "p2")),
               "Three labels are needed for the ternary, only 2 were specified")
  expect_warning(ternary_plot(data = sim0, show = "points",
                              prop = species,
                              tern_labels = c("p1", "p2", "p3", "P4")),
               "More than three labels were specified")

  expect_error(ternary_plot(contour_data %>% `attr<-`("x_proj", NULL) %>% select(-.x)),
               "Certain attributes of the data which are needed for plotting the response contours")

  expect_warning(ternary_plot(contour_data %>% `attr<-`("x_proj", NULL)),
                 "Certain attributes of the data which")


})

