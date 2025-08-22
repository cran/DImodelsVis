test_that("Sanity checks work", {

  # Return true by default
  expect_true(sanity_checks())

  # One of model or coefficients should be specified
  # data should be data.frame
  expect_error(sanity_checks(data = c(1, 2, 3)),
               "`data` should be a data frame or tibble")

  # DI model object
  expect_true(sanity_checks(DImodel = list("p" = 1) %>% `class<-`("DI")))
  expect_error(sanity_checks(DImodel = "p"),
               "or an object extending")

  # Regular model object
  expect_error(sanity_checks(model = "p"),
               "`model` should be a statistical model object")

  # Prop should be numeric or character
  expect_error(sanity_checks(prop = list(2, 2)),
               "`prop` should be a character vector")
  # and within bounds
  expect_error(sanity_checks(data = get_equi_comms(3),
                             prop = 3:5),
               "The indices specified in `prop` should be valid")
  expect_error(sanity_checks(data = get_equi_comms(3),
                             prop = c("p4")),
               "All values specified in `prop` should be present")
  expect_error(sanity_checks(data = iris,
                             prop = c(5)),
               "All columns specified in `prop` should be numeric")
  expect_error(sanity_checks(data = iris,
                             prop = c(1:4)),
               "between 0 and 1")
  expect_error(sanity_checks(data = get_equi_comms(3),
                             prop = c(1:2)),
               "Certain rows sum less than 1 currently.")
  expect_error(sanity_checks(data = get_equi_comms(3) %>% mutate(Richness = 0.5),
                             prop = c(1:4)),
               "Certain rows sum more than 1 currently.")

  # Checks for responses
  expect_error(sanity_checks(responses = list("p", TRUE)),
               "`responses` should be of type numeric or character")
  expect_error(sanity_checks(data = get_equi_comms(3),
                             responses = 3:5),
               "The indices specified in `responses` should be valid")
  expect_error(sanity_checks(data = get_equi_comms(3),
                             responses = c("p4")),
               "All values specified in `responses` should be present")
  expect_error(sanity_checks(data = iris,
                             responses = c(5)),
               "All columns specified in `responses` should be numeric")

  # Checks for treatment
  expect_true(sanity_checks(data = get_equi_comms(3),
                            treatments = 4))
  expect_true(sanity_checks(data = get_equi_comms(3),
                            treatments = "Richness"))

    expect_error(sanity_checks(treatments = list("p", TRUE)),
               "`treatments` should be of type numeric or character")
  expect_error(sanity_checks(data = get_equi_comms(3),
                             treatments = 3:5),
               "The indices specified in `treatments` should be valid")
  expect_error(sanity_checks(data = get_equi_comms(3),
                             treatments = c("p4")),
               "All values specified in `treatments` should be present")

  # Checks for facet
  expect_true(sanity_checks(data = get_equi_comms(3),
                            facet = 4))
  expect_true(sanity_checks(data = get_equi_comms(3),
                            facet = "Richness"))

  expect_error(sanity_checks(facet = list("p", TRUE)),
               "`facet` should be of type numeric or character")
  expect_error(sanity_checks(facet = c("1", "2", "3")),
               "`facet` cannot contain more than two")
  expect_error(sanity_checks(data = get_equi_comms(3),
                             facet = 5),
               "The indices specified in `facet` should be valid")
  expect_error(sanity_checks(data = get_equi_comms(3),
                             facet = c("p4")),
               "All values specified in `facet` should be present")


  # Other common checks
  expect_error(sanity_checks(colours = list("f" = "f5")),
               "not a valid colour")

  expect_error(sanity_checks(colours = list("f" = "f5")),
               "not a valid colour")

  expect_error(sanity_checks(booleans = list("f" = 3)),
               "not a boolean")

  expect_error(sanity_checks(characters = list("f" = 3)),
               "not a valid character")

  expect_error(sanity_checks(numerics = list("f" = "3")),
               "not numeric")

  expect_error(sanity_checks(unit_lengths = list("f" = c(1, 2, 3))),
               "be of length 1")

  # Warnings
  expect_warning(check_data_functions(model = "p", coefficients = 1:2),
                 "Both `model` and `coefficients` were specified")

})

test_that("check_data_function", {

  # One of model or coefficients should be specified
  expect_error(check_data_functions(model = NULL, coefficients = NULL),
               "Both `model` and `coefficients` cannot be empty.")

  # Warnings
  expect_warning(check_data_functions(model = "p", coefficients = 1:2),
                 "Both `model` and `coefficients` were specified")

})

test_that("check_add_var works", {

  library(DImodels)
  library(DImodelsMulti) %>% suppressMessages()

  data("Switzerland")

  species <- paste0("p", 1:4)
  mod_data <- Switzerland

  mod <- DI(y = "yield", prop = species, data = mod_data,
            DImodel = "AV",
            treat = "nitrogen", density = "density") %>% suppressMessages()
  # With DImodelsMulti
  belModel <- DImulti(prop = c("G1", "G2", "L1", "L2"), y = "Y", eco_func = c("Var", "un"),
                      unit_IDs = "Plot", theta = c(0.5, 1, 1.2), DImodel = "FG",
                      FG = c("Grass", "Grass", "Legume", "Legume"), extra_fixed = ~ Density,
                      method = "REML", data = dataBEL)

  # Return nothing is add_var is not specified
  expect_equal(check_add_var(model = mod), NULL)
  # Return same add_var if model is NULL
  expect_equal(check_add_var(add_var = list("nitrogen" = "high")),
               list("nitrogen" = "high"))
  expect_equal(check_add_var(add_var = data.frame("nitrogen" = c("high", "low"))),
               data.frame("nitrogen" = c("high", "low")))

  # Works for both list and data.frame
  expect_equal(check_add_var(model = mod,
                             add_var = data.frame("nitrogen" = c("50", "150"))) %>% suppressWarnings(),
               list("nitrogen" = factor(c("50", "150"), levels = c("50", "150"))))
  expect_equal(check_add_var(model = mod,
                             add_var = list("nitrogen" = c("50", "150"))) %>% suppressWarnings(),
               list("nitrogen" = factor(c("50", "150"), levels = c("50", "150"))))

  # Works with DImodelsMulti object
  expect_equal(check_add_var(model = belModel,
                             add_var = data.frame("Var" = c("N", "Weed"))) %>% suppressWarnings(),
               list("Var" = factor(c("N", "Weed"), levels = c("N", "Sown", "Weed"))))
  expect_equal(check_add_var(model = belModel,
                             add_var = list("Var" = c("N", "Weed"))) %>% suppressWarnings(),
               list("Var" = factor(c("N", "Weed"), levels = c("N", "Sown", "Weed"))))

  # Ensure conversion works
  expect_equal(check_add_var(model = mod,
                               add_var = list("nitrogen" = c(50, 150))) %>% suppressWarnings(),
               list("nitrogen" = factor(c("50", "150"), levels = c("50", "150"))))


  # Can only accept list or data.frame
  expect_error(check_add_var(model = mod, c("nitrogen" = "high")),
               "`add_var` should be a list or data.frame")
  # If non-numeric values specified for numeric values it should fail
  expect_error(check_add_var(model = mod, list("p1" = "2e")) %>% suppressWarnings(),
               "Specify a numeric vector for")

  # Warnings
  expect_warning(check_add_var(model = mod,
                               add_var = list("nitrogen" = c(50, 150))),
                 "Converting")

  expect_warning(check_add_var(model = mod,
                               add_var = data.frame("Var" = c("N", "Weed"))),
                 "The names for the additional")
  expect_warning(check_add_var(model = belModel,
                             add_var = data.frame("Var" = c("N", "Weed"))),
                 "Attempting to reconstruct")
  expect_warning(check_add_var(model = mod,
                               add_var = list("nitrogen" = factor(c("250", "150")))) %>% suppressMessages(),
                 "The values for categorical")

})

test_that("check_coeff_group works", {
  coeff_vec <- runif(7, 6, 12)
  coeff_named <- coeff_vec %>% `names<-`(paste0("p", 1:7))

  expect_true(check_coeff_groupings(coeff_vec, groups = list("g1" = 1:4,
                                                             "g2" = 5:6,
                                                             "g3" = 7)))
  expect_true(check_coeff_groupings(coeff_named, groups = list("g1" = "p1",
                                                               "g2" = c("p2", "p4"),
                                                               "g3" = c("p5", "p6"))))
  expect_true(check_coeff_groupings(coeff_named, groups = list("g1" = "p1",
                                                               "g2" = 5:6,
                                                               "g3" = 7)))

  expect_error(check_coeff_groupings(groups = c(1, 1, 2, 2)),
               "specified as a list")
  expect_error(check_coeff_groupings(coeff_vec, groups = list("p" = 1, "q" = 1, 2, 2)),
               "The same coefficient cannot be present in multiple")
  expect_error(check_coeff_groupings(coeff_vec, groups = list("p" = c(1, 7),
                                                              "q" = c(8))),
               "Can't group coefficients past the end.")
  expect_error(check_coeff_groupings(coeff_vec, groups = list("p" = c(1, 7),
                                                              "q" = c("p8"))),
               "Can't group coefficients that don't exist.")
  expect_warning(check_coeff_groupings(coeff_vec, groups = list(1, 3, 4, 2)),
                 "The coefficient groups should be given names.")
})

test_that("areColours works", {

  expect_true(all(areColours(c("blue", "green", "red"))))
  expect_true(all(areColours(c("blue", "green", "red", "#777", "#242424", "#58585885"))))
  expect_true(all(areColours(c("#58585885"))))

  expect_false(all(areColours(c("blue5", "green", "red"))))
  expect_false(all(areColours(c("blue5"))))

})
