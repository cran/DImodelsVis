#' @title Add additional variables to the data
#'
#' @description
#' Utility function for incorporating any additional variables
#' into the data. Each row in the data will be replicated and
#' new columns will be added for each variable specified in
#' `add_var` with values corresponding to their cartesian product.
#'
#' @param data A data frame containing the data in which to add the
#'             additional variables.
#' @param add_var A named list or data-frame specifying the names and
#'                corresponding values of each new variable to add to the data.
#'                If a list is specified, each row in the data would be replicated
#'                for each unique combination of values of the specified variables
#'                (i.e., their cartesian product) in `add_var`, while specifying
#'                a data-frame would replicate each row in the data for each row
#'                in add_var (i.e., merge the two data-frames).
#'
#' @return A data-frame with all additional columns specified in `add_var`
#'         and the following additional column.
#'  \describe{
#'    \item{.add_str_ID}{A unique identifier describing each element from the
#'                       cartesian product of all variables specified in `add_var`.}
#'    }
#'
#' @export
#'
#' @examples
#' test_data <- data.frame(diag(1, 3))
#' print(test_data)
#'
#' ## Adding a single variable
#' add_add_var(data = test_data,
#'             add_var = list("Var1" = c(10, 20)))
#'
#' ## Specifying multiple variables as a list will add values for
#' ##  each unique combination
#' add_add_var(data = test_data,
#'             add_var = list("Var1" = c(10, 20),
#'                            "Var2" = c(30, 40)))
#'
#' ## Specifying add_var as a data.frame would simply merge the two data-frames
#' add_add_var(data = test_data,
#'             add_var = data.frame("Var1" = c(10, 20),
#'                                  "Var2" = c(30, 40)))
#'
#' ## If the list specified in `add_var` is not named, then the additional
#' ## variables will be automatically named Var1, Var2, Var3, etc.
#' add_add_var(data = test_data,
#'             add_var = list(c(1, 2), c(3, 4)))
add_add_var <- function(data, add_var = NULL){
  if(missing(data)){
    cli::cli_abort(c("{.var data} cannot be empty.",
                     "i" = "Specify a data-frame or tibble."))
  }

  sanity_checks(data = data)

  if(!is.null(add_var)){
    if(!inherits(add_var, "list") && !inherits(add_var, "data.frame")){
      cli::cli_abort(c("The value specified in {.var add_var} should be a list
                       or data-frame.",
                       "i" = "Currently a value of class {.cls {class(add_var)}}
                              has been specified."))
    }

    # Add experimental structures to communities for predictions
    if(is.data.frame(add_var)){
      add_var_data <- add_var
    } else {
      add_var_data <- expand.grid(add_var)
    }

    # Add identifier for each unique combination of experimental structures
    add_var_data$.add_str_ID <- do.call(paste, c(Map(function(x, y) {paste(x, y, sep=": ")},
                                                     names(add_var_data), add_var_data),
                                                 sep="; \t"))

    # If any clashes with common columns in data and add_var
    # Rename columns in data
    common <- intersect(colnames(data), colnames(add_var_data))
    if(length(common)>0){
      cli::cli_warn(c("Certain names specified in {.var add_var} are already present
                      in the data.",
                      "i" = "The column{?s} {common} in the data will be given a
                      {.val .data} suffix to avoid conflicts."))
      idx <- which(colnames(data) == common)
      colnames(data)[idx] <- paste0(colnames(data)[idx], ".data")
    }
    # Merge experimental structures with species communities
    data <- merge(data, add_var_data, by = NULL) %>%
      copy_attributes(data)
    # data <- left_join(data, add_var_data, by = character())
  }
  return(data)
}

#' @keywords internal
#' Utility function to return colour-blind friendly colours/palette
#'
#' @usage NULL
NULL
colour_blind_friendly_cols <- function(n){
  # A safe colour-blind palette created by combining Okabe-Ito colours
  # and safe colour pallete from rcartocolor
  safe_colorblind_palette <- c("#009E73", "#AA4499", "#0072B2", "#F0E442", "#D55E00", "#661100",
                               "#332288", "#CC6677", "#E69F00", "#88CCEE", "#882255", "#DDCC77",
                               "#56B4E9", "#117733", "#CC79A7", "#44AA99", "#484848", "#999933",
                               "#6699CC", "#000000")
  if(n<=20){
    cols <- safe_colorblind_palette[1:n]
  } else {
    message("There are too many colours and they might not all be colour-blind
            friendly and distiguishable from one another")
    cols <- grDevices::hcl.colors(palette = "Spectral", n = n)
  }
}

#' @title Returns shades of colours
#'
#' @param colours A character vector of colours recognizable by R, to produces shades of
#' @param shades A numeric vector giving the number of shades for each colour
#'
#' @return A list consisting of hex codes describing the shades of each colour
#' @export
#'
#' @examples
#' ## Shades for a single colour
#' get_shades(c("red"))
#'
#' ## Shades for multiple colours
#' get_shades(c("red", "blue" ,"#A5F8E3", "#808080"), shades = c(2, 3, 4, 5))
#'
#' ## A single value for shade would imply all colours get the same number of shades
#' get_shades(c("red", "blue" ,"#A5F8E3", "#808080"), shades = 2)
get_shades <- function(colours = c("#808080"), shades = 3){
  if(length(shades) != 1 & length(shades) != length(colours)){
    cli::cli_abort(c("{.var shades} should be of a vector of length 1 or same as
                    the number of colours {.val {length(colours)}}"))
  }

  if(length(shades) == 1){
    shades <- rep(shades, each = length(colours))
  }

  # Get the L levels to adjust the shades of the colours in HSL format
  light_levels <- lapply(shades, function(x){
    light_level <- switch (x,
                           0,
                           c(-.1, .1),
                           c(-.15, 0, .15),
                           c(-.21, -.07, .07, .21),
                           c(-.2, -.1, 0, .1, .2),
                           c(-0.25, -.15, -0.05, 0.05, 0.15, 0.25),
                           c(-.3, -.2,-.1, 0, .1, .2, .3)
    )

    # If no of shades is greater than 7 then space the colours uniformly along the spectrum
    if(is.null(light_level)){
      light_level <- seq(-.35, .35, length.out = x)
    }
    return(light_level)
  })

  # Convert colours to HSL format
  hsl_cols <- t(plotwidgets::col2hsl(colours))

  # Shades of each colour will be creating by adjusting the
  # L parameter in the HSL respresentation of each colour
  new_cols <- lapply(1:length(colours), function(idx){
    col <- as.vector(hsl_cols[idx, ])
    light_level <- light_levels[[idx]]

    # Adjust for situations when L can become greater than 0 or 1
    threshold <- max(abs(light_level))
    if((col[3] + threshold) > 1){
      col[3] <- 1 - threshold - 0.05    # -0.05 for adding some wiggle room
    }

    if((col[3] - threshold) < 0){
      col[3] <- 0 + threshold + 0.05    # 0.05 for adding some wiggle room
    }

    shades_list <- sapply(light_level, function(x){
      new_col <- col + c(0, 0, x)
      plotwidgets::hsl2col(matrix(new_col, nrow = 3))
    })

    # # Return vector if only one colour is specified
    # if(shades == 1)
    #   shades_list <- shades_list[[1]]

    return(shades_list)
  })

  # Map the shades of colours to the original colour
  names(new_cols) <- colours

  # Desaturate colours
  new_cols <- lapply(new_cols, function(x) colorspace::desaturate(x, 0.25))
  return(new_cols)
}

#' @title Return colour-blind friendly colours
#'
#' @description
#' Utility function to return either a distinct colour-blind friendly colour
#' for each variable or if a functional grouping is specified, then shades of
#' the same colour for variables within a functional group
#'
#' @param vars Either a numeric value `n` to get n colours, or a character
#'             vector of values where each value will be mapped to a colour.
#' @param FG A character vector describing the functional grouping to which each
#'           variable belongs. Variables within the same group will have
#'           different shades of the same colour.
#'
#' @return A named vector containing the hex codes of colours
#'
#' @export
#'
#' @examples
#' ## Get n colours
#' get_colours(vars = 4)
#'
#' # Get a color-map for each value specified in vars
#' get_colours(vars = c("p1", "p2", "p3", "p4"))
#'
#' # Group values of vars using FG. Variables in the same group
#' # will have same shades of a colour
#' get_colours(vars = 4, FG = c("G1", "G1", "G2", "G2"))
get_colours <- function(vars, FG = NULL){
  if(missing(vars)){
    cli::cli_abort(c("{.var vars} cannot be missing.",
                    "i" = "Specify a number {.val n} to get {.var n} colours or
                           a {.cls character} vector where one colour would be mapped
                           each value."))
  }

  if(is.numeric(vars) && length(vars) == 1){
    vars <- as.character(1:vars)
  }

  nSpecies <- length(vars)
  if(!is.null(FG) & (!is.character(FG) | length(FG) != nSpecies)){
    cli::cli_abort(c("{.var FG} should be a character vector having the same length
                    as {.var vars} vector ({nSpecies}) giving the group
                    each variable belongs to.",
                     "i" = "Variables belonging to the same group would be assigned
                     shades of the same colour."))
  }

  if(is.null(FG)){
    cols <- colour_blind_friendly_cols(n = nSpecies)
  } else {
    FG_counts <- table(FG)
    base_cols <- colour_blind_friendly_cols(n = length(FG_counts))

    shades <- get_shades(colours = base_cols, shades = FG_counts)
    names(shades) <- unique(FG)

    cols <- stats::setNames(vector(mode = 'character', length = length(FG)), FG)

    for (sp in names(shades)){
      cols[which(names(cols) == sp)] <- shades[[sp]]
    }
    cols <- unname(cols)
  }
  return(cols)
}

#' @title Get all equi-proportional communities at specific levels of richness
#'
#' @importFrom dplyr %>% left_join
#'
#' @param nvars Number of variables in the design
#' @param richness_lvl The richness levels (number of non-zero compositional variables
#'                     in a community) at which to return the equi-proportional communities.
#'                     Defaults to each richness level from 1 up to `nvars` (both inclusive).
#' @param variables Names for the variables. Will be used as column names for the
#'                  final result. Default is "Var" followed by column number.
#' @param threshold The maximum number of communities to select for each level
#'                  of richness for situations when there are too many
#'                  equi-proportional communities. Default value is a million. \cr
#'                  Note: if threshold < `number of possible equi-proportional communities`
#'                  at a given level of richness, a random selection of communities
#'                  equal to the number specified in threshold would be returned.
#'
#' @return A dataframe consisting all or a random selection of equi-proportional
#'         communities at each level of richness
#' @export
#'
#' @examples
#' ## Get all equi-proportional communities for each level of richness upto 10
#' data10 <- get_equi_comms(10)
#' head(data10, 12)
#'
#' ## Change variable names
#' data4 <- get_equi_comms(4, variables = c("Lollium perenne", "Chichorum intybus",
#'                                          "Trifolium repens", "Trifolium pratense"))
#' head(data4)
#'
#' ## Get equi-proportional communities at specific levels of richness
#' ## Get all equi-proportional communities of four variables at richness
#' ## levels 1 and 3
#' data4_13 <- get_equi_comms(nvars = 4, richness = c(1, 3))
#' data4_13
#'
#' ## If threshold is specified and it is less than the number of possible
#' ## equi-proportional communites at a given level of richness, then a
#' ## random selection of communities from the total possible would be returned
#' ## Return only 2 random equi-proportional communities at the chosen richness
#' ## levels
#' data4_13_2 <- get_equi_comms(nvars = 4, richness = c(1, 3), threshold = 2)
#' data4_13_2
#'
#' ## Set threshold to a very high positive number to ensure
#' ## random selection is never performed
#' data_no_random <- get_equi_comms(nvars = 15,
#'                                  threshold = .Machine$integer.max)
#' head(data_no_random)
get_equi_comms <- function(nvars,
                           richness_lvl = 1:nvars,
                           variables = paste0('Var', 1:nvars),
                           threshold = 1000000){
  sanity_checks(numerics = list("nvars" = nvars,
                                "richness_lvl" = richness_lvl,
                                "threshold" = threshold),
                characters = list("variables" = variables))

  if(any(!between(richness_lvl, 1, nvars))){
    cli::cli_abort(c("{.var richness_lvl} can take values between 1 and {nvars}.",
                     "i" = "{.var richness_lvl} has value{?s}
                     {.val {as.character(richness_lvl[!between(richness_lvl, 1, nvars)])}}
                     outside this range."))
  }

  if(length(variables) != nvars){
    cli::cli_abort(c("The variable names specified in {.var variables} should have
                     the same length as the value specified in {.var nvars}.",
                     "i" = "{.var nvars} has a value {.val {nvars}} but {.var variables}
                     has {.val {length(variables)}} name{?s} specified."))
  }
  # Possible levels of richness
  choices <- richness_lvl
  # Communities to be used as seed for combinations
  seed_comms <- lapply(choices, function(rich_level){
    c(rep(1, rich_level), rep(0, nvars - rich_level))
  })

  chosen_comms <- lapply(seed_comms, function(comm){
    rich_level <- sum(comm)
    nrows <- choose(nvars, rich_level)
    # If nCr is less than threshold parameter then get
    # all combinations
    if(nrows <= threshold){
      combs <- matrix(0, nrow = nrows, ncol = nvars)
      idx <- utils::combn(1:nvars, sum(comm)) %>% t()
      for (i in 1:nrows){
        combs[i, idx[i, ]] <- 1
      }
      combs
      # Other wise choose random combinations of
      # communities
    } else {
      return(t(replicate(threshold, sample(comm))))
    }
  })

  # Bind everything together into a data.frame
  comms <- do.call(rbind, chosen_comms) %>%
    as.data.frame()
  # Add column names and richness variable
  names(comms) <- variables
  comms$Richness <- rowSums(comms)
  # Convert to proportions
  comms[, 1:nvars] <- comms[, 1:nvars]/comms$Richness
  return(comms)
}

#' @keywords internal
#' Utility function to return the richness of each community in the data.
#' Particularly useful in dplyr pipeline.
#'
#' @usage NULL
NULL
get_richness <- function(data, prop = NULL){
  # Special situation if funtion is called in dplyr pipeline as .data doesn't
  # allow to select over multiple columns
  if (inherits(data, "rlang_data_pronoun")) {
    # Add filler column to create data frame
    my_data <- data.frame('filler', data[[prop[1]]])
    for (var in prop){
      my_data[, var] <- data[[var]]
    }
    data <- my_data
  }
  return(rowSums(data[, prop] != 0))
}

#' @keywords internal
#' Utility function to return the evenness of each community in the data.
#' Particularly useful in dplyr pipeline.
#'
#' @usage NULL
NULL
get_evenness <- function(data, prop = NULL){
  # Special situation if funtion is called in dplyr pipeline as .data doesn't
  # allow to select over multiple columns
  if (inherits(data, "rlang_data_pronoun")) {
    # Add filler column to create data frame
    my_data <- data.frame('filler', data[[prop[1]]])
    for (var in prop){
      my_data[, var] <- data[[var]]
    }
    data <- my_data
  }
  return(DImodels::DI_data_E_AV(prop = prop, data = data)$E)
}

#' @keywords internal
#' Utility function to return the sum of species proportions in each community
#' in the data. Particularly useful in dplyr pipeline.
#'
#' @usage NULL
NULL
get_comm_sum <- function(data, prop = NULL){
  # Special situation if function is called in dplyr pipeline as .data doesn't
  # allow to select over multiple columns
  if (inherits(data, "rlang_data_pronoun")) {
    # Add filler column to create data frame
    my_data <- data.frame('filler', data[[prop[1]]])
    for (var in prop){
      my_data[, var] <- data[[var]]
    }
    data <- my_data
  }
  return(rowSums(data[, prop], na.rm = TRUE))
}

#' @keywords internal
#' Utility function to check if community is equi-proportional
#'
#' @usage NULL
NULL
check_equi <- function(comm){
  comm <- comm[which(comm != 0)]
  ret_value <- (abs(max(comm) - min(comm)) < 0.00001)
  return(ret_value)
}

#' @keywords internal
#' @importFrom stats AIC BIC logLik fitted residuals median model.frame terms
#' @importFrom insight n_obs n_parameters
#' @usage NULL
NULL
AICc <- function(model) {
  aic <- AIC(model)
  p <- insight::n_parameters(x = model, remove_nonestimable = TRUE) + 1
  n <- insight::n_obs(x = model)
  aicc <- aic + (2*p^2 + 2*p)/(n - p - 1)
  return(aicc)
}
#' @keywords internal
#' @usage NULL
NULL
BICc <- function(model) {
  bic <- BIC(model)
  p <- insight::n_parameters(x = model, remove_nonestimable = TRUE) + 1
  n <- insight::n_obs(x = model)
  bicc <- bic + (log(n)*(p+1)*p)/(n - p - 1)
  return(bicc)
}

# Create vectorized versions of the functions of information criteria
#' @keywords internal
#' @usage NULL
NULL
AIC_vec <- Vectorize(AIC)
#' @keywords internal
#' @usage NULL
NULL
BIC_vec <- Vectorize(BIC)
#' @keywords internal
#' @usage NULL
NULL
#' @keywords internal
#' @usage NULL
NULL
AICc_vec <- Vectorize(AICc)
#' @keywords internal
#' @usage NULL
NULL
BICc_vec <- Vectorize(BICc)
#' @keywords internal
#' @usage NULL
NULL
logLik_vec <- Vectorize(logLik)
#' @keywords internal
#' @usage NULL
NULL
deviance_vec <- Vectorize(deviance)

#' @keywords internal
#' @usage NULL
NULL
dropInf <- function(x, h) {
  if (any(isInf <- h >= 1)) {
    warning(gettextf("not plotting observations with leverage one:\n  %s",
                     paste(which(isInf), collapse = ", ")), call. = FALSE,
            domain = NA)
    x[isInf] <- NaN
  }
  x
}

#' @keywords internal
#' Function to return a smoothed curve over data points in a plot.
#' Useful for diagnostics plots
#'
#' @usage NULL
NULL
smoothing_fun <- function(x, y) {
  as.data.frame(stats::lowess(x, y, f = 2/3, iter = 3))
}

#' @keywords internal
#' Utility function for rescaling a vector between a specified min and max value
#'
#' @usage NULL
NULL
rescale <- function(x, min = 0, max = 1){
  ((x - min(x))/(max(x) - min(x)) * (max - min)) + min
}

#' @keywords internal
#' Function for checking for the presence of certain columns in the data
#' and print appropriate error message. Useful for plotting function to
#' ensure necessary columns are present in the data.
#'
#' @usage NULL
NULL
check_presence <- function(data, col, message = NULL){
  # Check if any columns are not present
  data_cols <- colnames(data)
  if(is.numeric(col)){
    not_present <- col[!col %in% 1:length(data_cols)]
    if(is.null(message)){
      message <- paste0("Location ", not_present, " is not present in the data.")
    }
  } else {
    not_present <- col[!col %in% data_cols]
    if(is.null(message)){
      message <- paste0("Cannot find column `", not_present, "` in the data.")
    }
  }

  # If column not present then print appropriate error message
  # specified in message
  if(length(not_present)>0){
    cli::cli_abort(message, call = caller_env())
  }
  return(TRUE)
}

#' @keywords internal
#' Wrapper function for adding a facet layer to a plot
#' @importFrom stats as.formula
#' @usage NULL
NULL
add_facet <- function(plot, data, facet_var, ...){
  facet_var <- rlang::try_fetch(data %>% dplyr::select(all_of(facet_var)),
                                error = function(cnd)
                                  rlang::abort(c("The value specified in `facet_var` is invalid."),
                                               parent = cnd,
                                               call = caller_env())) %>% colnames()
  if(length(facet_var) > 1){
    cli::cli_warn(c("Currently facetting is supported only with a single variable",
                  "i" = "Selecting to facet on the first specified value, i.e., {.val {facet_var[1]}}"))
    facet_var <- facet_var[1]
  }

  plot <- plot +
    facet_wrap(as.formula(paste("~", facet_var)),
               ...)
  return(plot)
}

#' Default theme for DImodelsVis
#'
#' @importFrom ggplot2 theme_bw theme element_text element_rect %+replace%
#' @importFrom dplyr between cross_join
#' @importFrom cli cli_abort
#'
#' @param font_size Base font size for text across the plot
#' @param font_family Font family for text across the plot
#' @param legend One of c("top", "bottom", "left", "right", "none") specifying
#'               the position of the legend. The legend position can also be
#'               specified as a numeric vector of form c(x, y) with x and y
#'               having values between 0 and 1. If specified as a numeric vector
#'               the legend within the plotting region where c(0,0) corresponds
#'               to the "bottom left" and c(1,1) corresponds to the "top right"
#'               position. The default position is "top".
#'
#' @return
#' A ggplot theme object
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' plot_data <- mtcars
#' plot_data$gear <- as.factor(plot_data$gear)
#' ggplot(data = plot_data,
#'        aes(x = mpg, y = disp, colour = gear))+
#'    geom_point(size = 3)+
#'    facet_wrap(~cyl) +
#'    theme_DI()
theme_DI <- function (font_size = 14, font_family = "",
                      legend = c("top", "bottom", "left", "right", "none")) {
  # Ensure input is appropriate
  sanity_checks(numerics = list("font_size" = font_size),
                characters = list("font_family" = font_family))

  # Legend position
  if(all(is.character(legend))){
    legend <- match.arg(legend)
  } else if(all(is.numeric(legend))){
    if(!all(between(legend, 0, 1))){
      cli_abort(c("If specified as a numeric vector the values for the legend
                  should be between 0 and 1."))
    }
  } else {
    cli_abort(c("{.var legend} should be of type {.cls character} or {.cls numeric}.",
                "i" = "You specified an object of type {.cls {class(legend)}}."))
  }

  # Scale font size of axis text relative to axis titles
  axis_text_size <- font_size * 0.85

  .theme <- theme_bw(base_size = font_size, base_family = font_family) %+replace%
    theme(axis.text = element_text(color = "black", size = axis_text_size),
          strip.background = element_rect(fill = "#93C572", colour = "black"),
          legend.position = legend,
          legend.text = element_text(size = axis_text_size),
          complete = TRUE)
  .theme
}


#' @title Add interaction terms used in a Diversity-Interactions (DI) model to new data
#'
#' @importFrom DImodels DI_data
#' @importFrom stats coef
#'
#' @param data A data-frame with species proportions that sum to 1 to create
#'             the appropriate interaction structures.
#' @param model A Diversity Interactions model object fit using the
#'              \code{\link[DImodels:DI]{DI()}} or \code{\link[DImodels:autoDI]{autoDI()}}
#'              functions from the \code{\link[DImodels:DImodels-package]{DImodels}}
#'              or \code{\link[DImodelsMulti:DImulti]{DImulti()}} from the
#'              \code{\link[DImodelsMulti:DImodelsMulti]{DImodelsMulti}} R packages.
#'
#' @description
#' Utility function that accepts a fitted Diversity-Interactions (DI) model
#' object along with a data frame and adds the necessary interaction structures to
#' the data for making predictions using the model object specified in `model`.
#'
#' @return
#' The original data-frame with additional columns appended at the end
#' describing the interactions terms present in the model object.
#'
#' @export
#'
#' @examples
#' library(DImodels)
#' data(sim1)
#'
#' # Fit different DI models
#' mod1 <- DI(y = "response", prop = 3:6, data = sim1, DImodel = "AV")
#' mod2 <- DI(y = "response", prop = 3:6, data = sim1, DImodel = "FULL")
#' mod3 <- DI(y = "response", prop = 3:6, data = sim1, DImodel = "ADD")
#' mod4 <- DI(y = "response", prop = 3:6, data = sim1,
#'            FG = c("G", "G", "H", "H"), DImodel = "FG")
#'
#' # Create new data for adding interaction terms
#' newdata <- sim1[sim1$block == 1, 3:6]
#' print(head(newdata))
#'
#' add_interaction_terms(data = newdata, model = mod1)
#' add_interaction_terms(data = newdata, model = mod2)
#' add_interaction_terms(data = newdata, model = mod3)
#' add_interaction_terms(data = newdata, model = mod4)
add_interaction_terms <- function(data, model){
  if(missing(data)){
    cli::cli_abort(c("{.var data} cannot be empty.",
                     "i" = "Specify a data-frame or tibble containing species
                     proportions that sum to 1 to create the appropriate
                     interaction structures."))
  }

  if(missing(model)){
    cli::cli_abort(c("{.var model} cannot be empty.",
                     "i" = "Specify a model object fit using the
                     {.help [{.pkg DImodels}](DImodels::DImodels)} or
                     {.help [{.pkg DImodelsMulti}](DImodelsMulti::DImodelsMulti)}
                     R packages."))
  }

  # Ensure model is a DImodels object
  sanity_checks(DImodel = model, data = data)
  data <- as.data.frame(data)
  DImodel_tag <- attr(model, "DImodel")

  if (DImodel_tag == "CUSTOM") {
    cli::cli_warn(c("!" = "Cannot add interaction terms for a custom DI model.",
                    "i"= "Returning original data-frame"))
    return(data)
  }
  original_data <- model$original_data
  model_data <- eval(model$model)

  # Get original species proportion columns
  prop <- attr(model, "prop")

  # Ensure proportions in data are appropriate and sum to 1
  sanity_checks(data = data, prop = prop)

  only_one_row <- nrow(data) == 1
  if (only_one_row) {
    data <- rbind(data, data)
  }
  theta_flag <- model$coefficients["theta"]
  betas <- coef(model)
  if (!DImodel_tag %in% c("ID", "STR")) {
    if (!is.na(theta_flag)) {
      theta_value <- coef(model)["theta"]
    }
    else {
      theta_value <- 1
    }
    extra_variables <- DI_data(prop = prop, FG = eval(model$DIcall$FG),
                               data = data, theta = theta_value, what = DImodel_tag)
    if (DImodel_tag == "E") {
      updated_data <- data.frame(data, E = extra_variables)
    }
    if (DImodel_tag == "AV") {
      updated_data <- data.frame(data, AV = extra_variables)
    }
    if (DImodel_tag == "ADD") {
      updated_data <- data.frame(data, extra_variables)
    }
    if (DImodel_tag == "FG") {
      data[, "FG_"] <- extra_variables
      updated_data <- data
    }
    if (DImodel_tag == "FULL") {
      updated_data <- data.frame(data, extra_variables,
                                 check.names = FALSE)
    }
  }
  else {
    updated_data <- data
  }
  if (only_one_row) {
    updated_data <- updated_data[1, ]
  }
  return(updated_data)
}

#' @title Add identity effect groups used in a Diversity-Interactions (DI) model to new data
#'
#' @param data A data-frame with species proportions that sum to 1 to
#'             create the identity effect groupings.
#' @param model A Diversity Interactions model object fit using the
#'              \code{\link[DImodels:DI]{DI()}} or \code{\link[DImodels:autoDI]{autoDI()}}
#'              functions from the \code{\link[DImodels:DImodels-package]{DImodels}}
#'              or \code{\link[DImodelsMulti:DImulti]{DImulti()}} from the
#'              \code{\link[DImodelsMulti:DImodelsMulti]{DImodelsMulti}} R packages.
#'
#' @description
#' Utility function that accepts a fitted Diversity-Interactions (DI) model
#' object along with a data frame and adds the appropriate species identity
#' effect groupings to the data for making predictions.
#'
#' @return
#' A data-frame with additional columns appended to the end that contain
#' the grouped species proportions.
#'
#' @export
#'
#' @examples
#' library(DImodels)
#' data(sim1)
#'
#' # Fit DI models with different ID effect groupings
#' mod1 <- DI(y = "response", prop = 3:6,
#'            data = sim1, DImodel = "AV") # No ID grouping
#' mod2 <- DI(y = "response", prop = 3:6,
#'            data = sim1, DImodel = "AV",
#'            ID = c("ID1", "ID1", "ID2", "ID2"))
#' mod3 <- DI(y = "response", prop = 3:6,
#'            data = sim1, DImodel = "AV",
#'            ID = c("ID1", "ID1", "ID1", "ID1"))
#'
#' # Create new data for adding interaction terms
#' newdata <- sim1[sim1$block == 1, 3:6]
#' print(head(newdata))
#'
#' add_ID_terms(data = newdata, model = mod1)
#' add_ID_terms(data = newdata, model = mod2)
#' add_ID_terms(data = newdata, model = mod3)
add_ID_terms <- function(data, model){
  if(missing(data)){
    cli::cli_abort(c("{.var data} cannot be empty.",
                     "i" = "Specify a data-frame or tibble containing species
                     proportions that sum to 1 to create the appropriate
                     interaction structures."))
  }

  if(missing(model)){
    cli::cli_abort(c("{.var model} cannot be empty.",
                     "i" = "Specify a model object fit using the
                     {.help [{.pkg DImodels}](DImodels::DImodels)} or
                     {.help [{.pkg DImodelsMulti}](DImodelsMulti::DImodelsMulti)}
                     R packages."))
  }

  # Ensure model is a DImodels object
  sanity_checks(DImodel = model, data = data)

  DImodel_tag <- attr(model, "DImodel")
  if (DImodel_tag == "CUSTOM") {
    cli::cli_warn(c("!" = "Cannot add identity terms for a custom DI model.",
                    "i"= "Returning original data-frame"))
    return(data)
  }

  # Get original species proportion columns
  prop <- attr(model, "prop")

  # Check species proportions in the data sum to 1
  sanity_checks(data = data, prop = prop)

  ID <- attr(model, "ID")

  data <-  group_prop(data = data, prop = prop, FG = ID)

  return(data)
}


#' @title Add predictions and confidence interval to data using either
#' a model object or model coefficients.
#'
#' @description
#' This function accepts a data.frame and either a model object or coefficients
#' and adds columns containing the predictions and associated uncertainty to the
#' data. When a model object is specified, the function uses
#' \code{\link[insight]{get_predicted}()} from the
#' \code{\link[insight]{insight}} package under the hood to
#' generate the predictions.
#'
#' @importFrom insight get_predicted
#'
#' @param data A data-frame containing appropriate values for all the terms
#'             in the model.
#' @param model A regression model object which will be used to make predictions
#'              for the observations in `data`. Will override `coefficients`
#'              if specified.
#' @param coefficients If a regression model is not available (or can't be fit in R),
#'                     the regression coefficients from a model fit in some other
#'                     language can be used to calculate predictions. However, the
#'                     user would have to ensure there's an appropriate one-to-one
#'                     positional mapping between the data columns and the
#'                     coefficient values. Further, they would also have to provide
#'                     a variance-covariance matrix of the coefficients in the `vcov`
#'                     parameter if they want the associated CI for the prediction or
#'                     it would not be possible to calculate confidence/prediction
#'                     intervals using this method.
#' @param vcov If regression coefficients are specified, then the variance-covariance
#'             matrix of the coefficients can be specified here to calculate the
#'             associated confidence interval around each prediction. Failure to do
#'             so would result in no confidence intervals being returned. Ensure
#'             `coefficients` and `vcov` have the same positional mapping with the data.
#' @param coeff_cols If `coefficients` are specified and a one-to-one positional
#'                   mapping between the data-columns and coefficient vector is
#'                   not present. A character string or numeric index can be specified
#'                   here to reorder the data columns and match the corresponding
#'                   coefficient value to the respective data column. See the
#'                   "Use model coefficients for prediction" section in examples.
#' @param conf.level The confidence level for calculating confidence/prediction
#'                   intervals. Default is 0.95.
#' @param interval Type of interval to calculate:
#'  \describe{
#'    \item{"none" (default)}{No interval to be calculated.}
#'    \item{"confidence"}{Calculate a confidence interval.}
#'    \item{"prediction"}{Calculate a prediction interval.}
#'  }
#'
#' @return
#' A data-frame with the following additional columns
#'  \describe{
#'    \item{.Pred}{The predicted response for each observation.}
#'    \item{.Lower}{The lower limit of the confidence/prediction interval
#'                  for each observation (will be same as ".Pred" if using
#'                  `coefficients` and `vcov` is not specified).}
#'    \item{.Upper}{The lower limit of the confidence/prediction interval
#'                  for each observation (will be same as ".Pred" if using
#'                  `coefficients` and `vcov` is not specified).}
#'  }
#'
#' @export
#'
#' @examples
#' library(DImodels)
#' data(sim1)
#'
#' # Fit a model
#' mod <- lm(response ~ 0 + p1 + p2 + p3 + p4 + p1:p2 + p3:p4, data = sim1)
#'
#' # Create new data for adding predictions
#' newdata <- head(sim1[sim1$block == 1,])
#' print(newdata)
#'
#' # Add predictions to data
#' add_prediction(data = newdata, model = mod)
#'
#' # Adding predictions to data with confidence interval
#' add_prediction(data = newdata, model = mod, interval = "confidence")
#'
#' # Calculate prediction intervals instead
#' add_prediction(data = newdata, model = mod, interval = "prediction")
#'
#' # Default is a 95% interval, change to 99%
#' add_prediction(data = newdata, model = mod, interval = "prediction",
#'                conf.level = 0.99)
#'
#' ####################################################################
#' ##### Use model coefficients for prediction
#' coeffs <- mod$coefficients
#'
#' # Would now have to add columns corresponding to each coefficient in the
#' # data and ensure there is an appropriate mapping between data columns and
#' # the coefficients.
#' newdata$`p1:p2` = newdata$p1 * newdata$p2
#' newdata$`p3:p4` = newdata$p3 * newdata$p4
#'
#' # If the coefficients are named then the function will try to
#' # perform matching between data columns and the coefficients
#' # Notice that confidence intervals are not produced if we don't
#' # specify a variance covariance matrix
#' add_prediction(data = newdata, coefficients = coeffs)
#'
#' # However, if the coefficients are not named
#' # The user would have to manually specify the subset
#' # of data columns arranged according to the coefficients
#' coeffs <- unname(coeffs)
#'
#' subset_data <- newdata[, c(3:6, 8,9)]
#' subset_data # Notice now we have the exact columns in data as in coefficients
#' add_prediction(data = subset_data, coefficients = coeffs)
#'
#' # Or specify a selection (either by name or index) in coeff_cols
#' add_prediction(data = newdata, coefficients = coeffs,
#'                coeff_cols = c("p1", "p2", "p3", "p4", "p1:p2", "p3:p4"))
#'
#' add_prediction(data = newdata, coefficients = coeffs,
#'                coeff_cols = c(3, 4, 5, 6, 8, 9))
#'
#' # Adding confidence intervals when using model coefficients
#' coeffs <- mod$coefficients
#' # We need to provide a variance-covariance matrix to calculate the CI
#' # when using `coefficients` argument. The following warning will be given
#' add_prediction(data = newdata, coefficients = coeffs,
#'                interval = "confidence")
#'
#' vcov_mat <- vcov(mod)
#' add_prediction(data = newdata, coefficients = coeffs,
#'                interval = "confidence", vcov = vcov_mat)
#'
#' # Currently both confidence and prediction intervals will be the same when
#' # using this method
#' add_prediction(data = newdata, coefficients = coeffs,
#'                interval = "prediction", vcov = vcov_mat)
add_prediction <- function(data, model = NULL,
                           coefficients = NULL, coeff_cols = NULL, vcov = NULL,
                           interval = c("none", "confidence", "prediction"),
                           conf.level = 0.95){

  if(missing(data)){
    cli::cli_abort(c("{.var data} cannot be empty.",
                     "i" = "Specify a data-frame or tibble containing the
                     respective columns necessary for making predictions."))
  }


  # Ensure model or coefficients are specified correctly
  check_data_functions(model = model, coefficients = coefficients)

  # Check other parametesr to ensure they are appropriate
  sanity_checks(data = data,
                numerics = list("conf.level" = conf.level),
                unit_lengths = list("conf.level" = conf.level))

  # Ensure conf.level is between 0 and 1
  if(!(conf.level < 1 && conf.level > 0)){
    cli::cli_abort(c("{.var conf.level} should be a value between 0 and 1
                     specifying the level at which to calculate the
                     confidence/prediction interval.",
                     "i" = "{.var conf.level} has a value of {conf.level}."))
  }

  # Choose appropriate interval for prediction CI
  interval <- match.arg(interval)

  # Branch here if model is specified
  if(!is.null(model)){
    # Prediction with SE
    if(inherits(model, "DI")){
      if(interval != "none"){
        preds <- predict(model, newdata = data,
                         interval = interval,
                         level = conf.level) %>%
          as.data.frame()
        data <- data %>%
          mutate(".Pred" := preds$fit,
                 ".Lower" = preds$lwr,
                 ".Upper" = preds$upr)
      } else{
        preds <- predict(model, newdata = data, se.fit = FALSE)
        data <- data %>%
          mutate(".Pred" := preds)
      }
    } else if (inherits(model, "DImulti")){
      preds <- predict_from_DImulti(model = model, newdata = data)
      data <- data %>%
        mutate(".Pred" := preds)
    } else {
      # These are custom model objects
      # First see if they are supported by insight and
      base_pred_flag <- FALSE
      if(isTRUE(insight::is_model_supported(model))){
        preds <- insight::get_predicted(x = model, data = data) %>%
          suppressMessages() %>% suppressWarnings()

        # Prediction from insight has failed give alert and move on
        if(length(preds) != nrow(data)){
          cli::cli_alert_warning("Unable to generate predictions using {.pkg insight}, will retry with base R {.fn {col_green(\"predict\")}} function")
          base_pred_flag <- TRUE
        } else {
        # Keep moving forward with generating CI using insight
          data <- data %>%
            mutate(".Pred" := as.numeric(preds))
          if(interval != "none"){
            CIs <- insight::get_predicted_ci(x = model,
                                             predictions = preds,
                                             data = data,
                                             ci = conf.level,
                                             ci_type = interval) %>%
              suppressMessages() %>% suppressWarnings()

            if(ncol(CIs) > 1){
              data <- data %>% mutate(".Lower" = CIs[, "CI_low"],
                                      ".Upper" = CIs[, "CI_high"])
            }
          }
        }
      } else {
        # Insight doesn't support model. So use base R prediction function
        base_pred_flag <- TRUE
      }

      if(isTRUE(base_pred_flag)){
        # Final hail mary for model objects not supported by insight or if prediction failed
        # there try if predictions can be made using the base predict function
        tryCatch(
          {
            preds <- predict(model, data)
            data <- data %>% mutate(".Pred" := as.numeric(preds))

            if(interval != "none"){
              cli::cli_warn(c("It wasn't possible to automatically generate uncertainty intervals for the specified model object.",
                              "i" = "Please generate them manually and add them to the data as columns named {.val .Lower} and {.val .Upper}, respectively."))
            }
          },
          error = function(e) {
            cli::cli_abort(c("It wasn't possible to automatically generate predictions for the specified model object.",
                             "i" = "The following error was encountered when running the {.fn {col_green(\"predict\")}} function:",
                             "x" = cli::col_red(conditionMessage(e)),
                             "i" = "Please generate the predictions manually and add them to the data as a column named {.val .Pred}.",
                             "i" = "If this error is encountered from any data-preparation function, i.e. {.fn {col_green(\"*_data\")}}, rerun the function with {.var prediction = FALSE} and then manually add predictions to the data as a column named {.val .Pred}."))
          }
        )
      }
      }
  }
  # Branch here if regression coefficients are specified
  else if(!is.null(coefficients)){
    # If matrix is specified convert to data for time being
    data <- as.data.frame(data)
    # Ensure coefficients are numeric
    if(!is.numeric(coefficients)){
      cli::cli_abort(c("{.var coefficients} should be a numeric vector.",
                       "i" = "{.var coefficients} was specified as an object
                       with class {.cls {class(coefficients)}}."))
    }
    # If coefficients are named then order data columns accordingly
    if(!is.null(names(coefficients)) && is.null(coeff_cols)){
      if(!all(names(coefficients) %in% colnames(data))){
        cli::cli_abort(c("All coefficient names should be present in the data as columns.",
                         "i" = "{.var {names(coefficients)[!names(coefficients) %in% colnames(data)]}}
                         {?was/were} not present in the data."))
      }
      # Create X matrix
      X_matrix <- as.matrix(sapply(data[, names(coefficients)], as.numeric))
    }
    # If a selection is specified from the data
    else if (!is.null(coeff_cols)){
      if(length(coefficients) != length(coeff_cols)){
        cli::cli_abort(c("The number of values specified for selecting and reordering
                         data columns in {.var coeff_cols} should be same as the
                         number of coefficients specified in the {.var coefficients}
                         vector.",
                         "i" = "The were {length(coefficients)} coefficients
                         while {.var coeff_cols} specified {length(coeff_cols)}
                         column{?s} to select."))
      }
      # select the specified columns
      X_cols <- data %>% select(all_of(coeff_cols))
      # Created X_matrix
      X_matrix <- as.matrix(sapply(X_cols, as.numeric))
    }
    # If neither coefficients are named nor a selection provided then
    # assume the user has specified everything correctly and calculate predictions
    else {
      if(ncol(data)!=length(coefficients)){
        cli::cli_abort(c("If coeficients are not named, then the number of columns in
                         {.var data} should be the same as the number of coefficients.",
                         "i" = "The were {length(coefficients)} coefficients
                         while data had {ncol(data)} columns (after adding anything which was specified in {.val add_var}).",
                         "i" = "Consider giving names to the coefficient vector
                         specified in {.var coefficients} corresponding to the
                         respective data columns or providing a selection of
                         columns in {.var coeff_cols} corresponding (in
                         sequential order) to each coefficient."))
      }
      # Create X_matrix
      X_matrix <- as.matrix(sapply(data, as.numeric))
    }
    # Calculate predictions
    preds <- as.vector(X_matrix %*% coefficients)

    # Calculate SE and uncertainty
    if(!is.null(vcov) && interval != "none"){
      if(!is.matrix(vcov) | !is.numeric(vcov)){
        cli::cli_abort(c("{.var vcov} should be a numeric matrix.",
                         "i" = "{.var vcov} was specified as an object
                               with class {.cls {class(vcov)}}."))
      }
      if(nrow(vcov) != ncol(vcov)){
        cli::cli_abort(c("{.var vcov} should be a symmetric square matrix.",
                         "i" = "Currently {.var vcov} has {.val {nrow(vcov)}} rows
                         and {.val {ncol(vcov)}} columns."))
      }
      if(length(coefficients) != ncol(vcov)){
        cli::cli_abort(c("The number of rows and columns in {.var vcov} should be the
                         same as the number of coefficients",
                         "i" = "The were {.val {ncol(vcov)}} columns in {.var vcov}
                         while there were {.val {length(coefficients)}} coefficients"))
      }

      # Calculate SE
      se <-  as.numeric(sqrt(diag(X_matrix %*% vcov %*%
                                    t(X_matrix))))
      # Calculate wald ci
      critval <- qnorm(conf.level + (1 - conf.level)/2)
      lwr <- preds - critval * se
      upr <- preds + critval * se
    } else {
      if(interval != "none"){
        cli::cli_warn(c("{.var vcov} was not specified so uncertainty
                        intervals cannot be calculated.",
                        "i" = "The {.val .Upper} and {.val .Lower} columns
                        will contain the same value as the {.val .Pred} column."))
        upr = preds
        lwr = preds
      }
    }

    if(interval != "none"){
      data <- data %>%
        mutate(".Pred" = preds,
               ".Lower" = lwr,
               ".Upper" = upr)
    } else {
      data <- data %>%
        mutate(".Pred" = preds)
    }

  }
  return(data)
}

#' @title Special custom filtering for compositional data
#'
#' @description
#' A handy wrapper around the dplyr \code{\link[dplyr:filter]{filter()}} function
#' enabling the user to filter rows which satisfy specific conditions
#' for compositional data like all equi-proportional communities, or communities
#' with a given value of richness without having to make any changes to the data
#' or adding any additional columns. All other functionalities are same as the
#' dplyr \code{\link[dplyr:filter]{filter()}} function.
#'
#'
#' @param data A data frame containing the compositional variables which should
#'             be used to perform the filtering.
#' @param prop A character/numeric vector indicating the columns containing the
#'             compositional variables in `data`.
#' @param special A character string specifying the filtering condition.
#'                Four special keywords can be specified here for filtering
#'                  1. richness: A positive integer value to filter communities with
#'                               a specific number of compositional variables (variables with non-zero values).
#'                  2. evenness: A numeric value between 0 and 1, to filter rows based on
#'                               the relative abundances of the compositional variables where a higher
#'                               value signifies a more even community with equal proportions
#'                               of all variables.
#'                  3. equi: A boolean variable indicating whether to filter rows containing
#'                           equi-proportional communities, i.e., communities where all variables
#'                           have the same non-zero proportion.
#'                  4. monos: A boolean value indicating whether to filter communities
#'                            containing a single compositional variable, i.e., richness == 1.
#'                These keywords can be combined using any logical operators and can even
#'                be combined with any other variables in the data. Please use the exact
#'                keywords (case-sensitive) in the query to get appropriate results. See
#'                examples for more details.
#' @param ... Any additional arguments specified to the dplyr \code{\link[dplyr:filter]{filter()}} function.
#'            Filtering conditions for any additional variables can also be specified here.
#'
#' @return A subset of the original data which matches the specified filtering conditions.
#'
#' @export
#'
#' @examples
#' library(DImodels)
#' library(dplyr)
#'
#' ## Load data
#' data(sim3)
#'
#' # The special filter keywords should be specified as a string
#' # Filter communities containing 3 species
#' head(custom_filter(data = sim3, prop = 4:12,
#'                    special = "richness == 3"))
#'
#' # Filter communities at richness 6 OR evenness 0
#' head(custom_filter(data = sim3, prop = 4:12,
#'                    special = "richness == 6 | evenness == 0"), 12)
#'
#' # Filter all monoculture AND treatment "A" (treatment is column present in data)
#' head(custom_filter(data = sim3, prop = 4:12,
#'                    special = "monos == TRUE & treatment == 'A'"), 10)
#'
#' # Filter all equi proportional communities but NOT monocultures
#' head(custom_filter(data = sim3, prop = 4:12,
#'                    special = "equi == TRUE & monos == FALSE"))
#'
#' # Can also use normal filter
#' sim3 %>% custom_filter(p1 == 1)
#'
#' # Both special filtering and normal filtering can be combined as well
#' sim3 %>% custom_filter(prop = paste0("p", 1:9),
#'                        special = "richness == 1",
#'                        community %in% c(7, 9))
custom_filter <- function(data, ..., prop = NULL, special = NULL){
  # Sanity Checks
  if(missing(data)){
    cli::cli_abort(c("{.var data} cannot be empty.",
                     "i" = "Specify a data frame or tibble in {.var data}."))
  }

  # If no special value specified then filter as normal
  if(is.null(special)){
    species <- FALSE
    prop <- FALSE
    return_data <- data %>% filter(...)
  } else {
  # special should be a character
    if(!is.character(special)){
      cli::cli_abort(c("{.var special} should be a character specifying the filtering condition.",
                       "i" = "Ensure {.var special} contains filtering conditions comprising
                       the special keywords {.val {c(\"richness\", \"evenness\", \"monos\", \"equi\")}}
                       or any other columns present in the data."))
    }
    # prop can't be null if special is specified
    if(missing(prop)){
      cli::cli_abort(c("{.var prop} cannot be empty if special filtering is used.",
                       "i" = "Specify a character/numeric vector indicating
                            the names/indicies corresponding to variable
                            proportions in {.var data}."))
    }

    sanity_checks(data = data, prop = prop)
    prop <- colnames(data[, prop])

    data <- data %>% mutate(.richness = get_richness(.data, prop),
                            .evenness = get_evenness(.data, prop),
                            .monos = ifelse(.data$.richness == 1, TRUE, FALSE),
                            .equi = apply(data[, prop], 1, check_equi))

    # Replace special term with "." variant
    special <- gsub("richness", ".richness", special, fixed = TRUE)
    special <- gsub("evenness", ".evenness", special, fixed = TRUE)
    special <- gsub("monos", ".monos", special, fixed = TRUE)
    special <- gsub("equi", ".equi", special, fixed = TRUE)
    drop_cols <- c(".richness", ".evenness", ".monos", ".equi")

    return_data <- data %>%
      filter(eval(rlang::parse_expr(special)), ...) %>%
      select(-all_of(drop_cols))
  }

  return(return_data)
}



#' @rdname Simplex_projection
#' @title Project 3-d compositional data onto x-y plane and vice versa
#'
#' @description
#' Points in the 3-d simplex space with coordinates (x, y ,z) such that
#' x + y + z = 1 are projected into the 2-d plane they reside in. This function
#' can be used to convert the 3-d compositional data into 2-d and then be overlayed
#' on the plots output by \code{\link{ternary_plot}},
#' \code{\link{conditional_ternary_plot}} and \code{\link{grouped_ternary_plot}}.
#'
#' @param data A data-frame containing 3-d compositional data.
#' @param prop A character vector specifying the columns names of columns
#'             containing variable proportions. Default is "p1", "p2",
#'             and "p3".
#' @param x A character string specifying the name for the column containing
#'          the x component of the x-y projection of the simplex. Default is ".x".
#' @param y A character string specifying the name for the column containing
#'          the y component of the x-y projection of the simplex. Default is ".y".
#'
#' @return A data-frame with the following two columns appended (when transforming to x-y projection)
#' \describe{
#'    \item{.x (or value specified in "x")}{The x component of the x-y projection of the simplex point.}
#'    \item{.y (or value specified in "y")}{The y component of the x-y projection of the simplex point.}
#'    }
#'
#' @export
#'
#' @examples
#' ## Convert proportions to x-y co-ordinates
#' library(DImodels)
#' data(sim0)
#' sim0 <- sim0[1:16, ]
#'
#' prop_to_tern_proj(data = sim0, prop = c("p1", "p2", "p3"))
#'
#' # Change names of the x and y projections
#' prop_to_tern_proj(data = sim0, prop = c("p1", "p2", "p3"),
#'                   x = "x-proj", y = "y-proj")
prop_to_tern_proj <- function(data, prop,
                              x = ".x", y = ".y"){
  # Sanity Checks
  if(missing(data)){
    cli::cli_abort(c("{.var data} cannot be empty.",
                     "i" = "Specify a data frame containing variable
                            proportions."))
  }
  if(length(prop) != 3){
    cli::cli_abort(c("Currently projections are supported for systems with
                     three compositional variables only. This lock will soon be
                     lifted to support systems with more variables."))
  }

  sanity_checks(data = data, prop = prop)

  prop <- colnames(data[, prop])

  data <- data %>%
    mutate(!! y := !!sym(prop[1]) * sqrt(3)/2,
           !! x := !!sym(prop[3]) + (!!sym(y))/sqrt(3)) %>%
    select(all_of(c(prop, x, y)), everything())
  return(data)
}

#' @rdname Simplex_projection
#' @title Project points from the x-y plane into the 3-d compositional data
#'
#' @param data A data-frame containing the x-y coordinates of the points.
#' @param x A character string specifying the name for the column containing
#'          the x component of the x-y projection of the simplex.
#' @param y A character string specifying the name for the column containing
#'          the y component of the x-y projection of the simplex.
#' @param prop A character vector specifying the columns names of variable
#'             containing the projected compositions. Default is "p1", "p2", and "p3".
#'
#' @return A data-frame with the following three columns appended (when transforming to compositional projection)
#' \describe{
#'    \item{p1 (or first value specified in "prop")}{The first component of the 3-d simplex point.}
#'    \item{p2 (or second value specified in "prop")}{The second component of the 3-d simplex point.}
#'    \item{p3 (or third value specified in "prop")}{The third component of the 3-d simplex point.}
#'    }
#' @export
#'
#' @examples
#' ## Convert x-y co-ordinates to proportions
#' library(DImodels)
#' data(sim0)
#' sim0 <- sim0[1:16, ]
#'
#' proj_data <- prop_to_tern_proj(data = sim0, prop = c("p1", "p2", "p3"))
#'
#' tern_to_prop_proj(data = proj_data, x = ".x", y = ".y")
#'
#' # Change prop names
#' tern_to_prop_proj(data = proj_data, x = ".x", y = ".y",
#'                   prop = c("prop1", "prop2", "prop3"))
tern_to_prop_proj <- function(data, x, y,
                              prop = c("p1", "p2", "p3")){
  # Sanity Checks
  if(missing(data)){
    cli::cli_abort(c("{.var data} cannot be empty.",
                     "i" = "Specify a data frame containing variable
                            proportions."))
  }

  if(missing(x)){
    cli::cli_abort(c("{.var x} cannot be empty.",
                     "i" = "Specify the name of the column in {.var data}
                            corresponding to the x-coordinate of ternary projection."))
  }

  if(missing(y)){
    cli::cli_abort(c("{.var y} cannot be empty.",
                     "i" = "Specify the name of the column in {.var data}
                            corresponding to the y-coordinate of ternary projection."))
  }

  if(length(prop)!=3){
    cli::cli_abort(c("{.var prop} should be a character vector of length three
                     containing the names of the transformed compositional variables."))
  }

  if(!is.character(prop)){
    cli::cli_abort(c("{.var prop} should be a character vector of length three
                     containing the names of the transformed compositional variables.",
                     "i" = "{.var prop} is specified as an object of {.cls {class(prop)}} type"))
  }

  if(!(x %in% colnames(data))){
    cli::cli_abort(c("The value specified in {.var x} should be present in data.",
                     "x" = "{.val x} was not present in data."))
  }

  if(!(y %in% colnames(data))){
    cli::cli_abort(c("The value specified in {.var y} should be present in data.",
                     "x" = "{.val y} was not present in data."))
  }

  if(!all(between(data[, x], 0, 1))){
    cli::cli_warn(c("It is expected that the values in {.var x}
                     would be between 0 and 1.",
                     "!" = "If there are values beyond this range then
                    the transformed proportions would not be between 0 and 1."))
  }

  if(!all(between(data[, y], 0, 0.87))){
    cli::cli_warn(c("It is expected that the values in {.var y}
                     would be between 0 and 0.87.",
                    "!" = "If there are values beyond this range then
                    the transformed proportions would not be between 0 and 1."))
  }
  sanity_checks(data = data)

  data <- data %>%
    mutate(!! prop[1] := !!sym(y) * 2/sqrt(3),
           !! prop[3] := !!sym(x) - !!sym(y)/sqrt(3),
           !! prop[2] := 1 - (!! sym(prop[1]) + !!sym(prop[3]))) %>%
    select(all_of(c(x, y, prop)), everything())
  return(data)
}

#' @title Combine variable proportions into groups
#'
#' @param data A data frame containing the compositional variables which need to
#'             be grouped.
#' @param prop A character/numeric vector indicating the columns containing the
#'             compositional variables in `data`.
#' @param FG A character vector of same length as `prop` specifying the group
#'           each variable belongs to.
#'
#' @return A data-frame with additional columns appended to the end that contain
#' the grouped variable proportions.
#'
#' @export
#'
#' @examples
#' library(DImodels)
#'
#' data(sim1)
#'
#' head(group_prop(data = sim1, prop = 3:6,
#'                 FG = c("Gr1", "Gr1", "Gr1", "Gr2")))
#'
#' head(group_prop(data = sim1, prop = 3:6,
#'                 FG = c("Group1", "Group2", "Group1", "Group3")))
#'
#' ## Data is returned as is, if no groups are specified in FG
#' head(group_prop(data = sim1, prop = 3:6))
group_prop <- function(data, prop, FG = NULL){

  # Sanity Checks
  if(missing(data)){
    cli::cli_abort(c("{.var data} cannot be empty.",
                     "i" = "Specify a data-frame containing variable
                            proportions."))
  }

  if(missing(prop)){
    cli::cli_abort(c("{.var prop} cannot be empty.",
                     "i" = "Specify a character/numeric vector indicating
                            the names/indicies corresponding to variable
                            proportions in {.var data}."))
  }

  sanity_checks(data = data, prop = prop)

  if(!is.null(FG)){
    if(!is.character(FG)){
      cli::cli_abort(c("The {.var FG} argument should be specified as a character
                       vector of same length as the {.var prop} argument, specifying
                       the group to which each variable in {.var prop} belongs.",
                       "i" = "{.var FG} was specified as a {.cls {class(FG)}} object."))
    }

    if(length(prop) != length(FG)){
      cli::cli_abort(c("The {.var FG} argument should be specified as a character
                       vector of same length as the {.var prop} argument, specifying
                       the group to which each variable in {.var prop} belongs.",
                       "i" = "{.var FG} has length {length(FG)} while {.var prop}
                       has length {length(prop)}."))
    }

    prop <- colnames(data[, prop])
    all_gr <- unique(FG)
    for (gr in all_gr){
      filter_prop <- prop[which(FG == gr)]
      data[, gr] <- data %>%
        select(all_of(filter_prop)) %>%
        rowSums()
    }
  }
  return(data)
}

#' Copy attributes from one object to another
#'
#' @description
#' This function copies over any additional attributes from `source`
#' into `target`. Any attributes already present in `target` would be
#' left untouched. This function is useful after manipulating the data
#' from the \code{*_data} preparation functions to ensure any attributes
#' necessary for creating the plot aren't lost.
#'
#' @param target The object to which attributes should be added.
#' @param source The object whose attributes to copy.
#'
#' @return
#' The object specified in `target` with all additional attributes in
#' `source` object.
#'
#' @export
#'
#' @examples
#'
#' ## Simple example
#' a <- data.frame(Var1 = runif(1:10), Var2 = runif(1:10))
#' b <- data.frame(Var3 = runif(1:10), Var4 = runif(1:10))
#' attr(b, "attr1") <- "Lorem"
#' attr(b, "attr2") <- "ipsum"
#'
#' print(attributes(a))
#' print(attributes(b))
#'
#' ## Copy over attributes of `b` into `a`
#' print(copy_attributes(target = a, source = b))
#' ## Note the attributes already present in `a` are left untouched
#'
#' ## Can also be used in the dplyr pipeline
#' library(dplyr)
#'
#' iris_sub <- iris[1:10, ]
#' attr(iris_sub, "attr1") <- "Lorem"
#' attr(iris_sub, "attr2") <- "ipsum"
#' attributes(iris_sub)
#'
#' ## Grouping can drop attributes we set
#' iris_sub %>%
#'    group_by(Species) %>%
#'    summarise(mean(Sepal.Length)) %>%
#'    attributes()
#'
#' ## Use copy_attributes with `iris_sub` object as source
#' ##  to add the attributes again
#' iris_sub %>%
#'    group_by(Species) %>%
#'    summarise(mean(Sepal.Length)) %>%
#'    copy_attributes(source = iris_sub) %>%
#'    attributes()
copy_attributes <- function(target, source){

  if(missing(target)){
    cli::cli_abort(c("{.var target} cannot be empty.",
                     "i" = "Specify an object to which the
                     attributes should be copied."))
  }

  if(missing(source)){
    cli::cli_abort(c("{.var source} cannot be empty.",
                     "i" = "Specify an object with attributes
                     to copy over."))
  }

  # Copy any non-existing attributes from source in target
  src_attr <- names(attributes(source))
  tar_attr <- names(attributes(target))
  for (attribute in src_attr){
    if(!attribute %in% tar_attr){
      attr(target, attribute) <- attr(source, attribute)
    }
  }
  return(target)
}


# Function to print generic message if model is not DI
#' @keywords internal
#' Utility function to check if model is a DImodel and print an appropriate
#' help message accordingly
#'
#' @usage NULL
NULL
model_not_DI <- function(call_fn){
  data_fn <- paste0(call_fn, "_data")
  plot_fn <- paste0(call_fn, "_plot")
  data_fn_link <- paste0("DImodelsVis::", data_fn)
  plot_fn_link <- paste0("DImodelsVis::", plot_fn)
  cli::cli_abort(c("{.var model} should be a regression model fit using the
                    {.help [{.fun DI}](DImodels::DI)} or
                    {.help [{.fun autoDI}](DImodels::autoDI)} functions from
                    the {.help [{.pkg DImodels}](DImodels::DImodels)}
                   package or the {.help [{.fun DImulti}](DImodelsMulti::DImulti)}
                   function from the {.help [{.pkg DImodelsMulti}](DImodelsMulti::DImodelsMulti)}
                   package.",
                   "i" = "If your model cannot be fit using the
                   {.help [{.fun autoDI}](DImodels::autoDI)},
                   {.help [{.fun DI}](DImodels::DI)}, or
                   {.help [{.fun DImulti}](DImodelsMulti::DImulti)} functions,
                     manually call the {.help [{.fn {col_green(data_fn)}}]({data_fn_link})}
                     function to prepare the data followed by the
                     {.help [{.fn {col_green(plot_fn)}}]({plot_fn_link})}) function to create the plot."))
}

#' @keywords internal
#' Utility function to check if a particular column exists in the data
#'
#' @usage NULL
NULL
check_col_exists <- function(data, col){
  col_names <- colnames(data)
  if(all(col %in% col_names)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @keywords internal
#' Utility function to check if all columns necessary for creating
#' a plot exist in the data
#'
#' @usage NULL
NULL
check_plot_data <- function(data, cols_to_check, calling_fun,
                            data_name = "data"){
  check_res <- check_col_exists(data, cols_to_check)

  if(check_res){
    return(TRUE)
  } else {
    plot_fn <- paste0(calling_fun, "_plot")
    data_fn <- paste0(calling_fun, "_data")
    data_fn_link <- paste0("DImodelsVis::", data_fn)

    missing_vars <- cols_to_check[!cols_to_check %in% names(data)]
    message = c("All variables necessary for creating the
                {.var {plot_fn}} are not present in {.var {data_name}}.",
                "!" = "The variable{?s} {.val {missing_vars}} {?is/are} not present in the data.",
                "i" = "Recreate the data using the {.help [{.fn {data_fn}}]({data_fn_link})}
                function or read the `{.field Value}` section on the help page of
                {.help [{.fn {data_fn}}]({data_fn_link})} if you wish to tweak the ouptut and
                manually add variables with these names to the data.")
    cli::cli_abort(message, call = caller_env())
  }
}

#' @keywords internal
#' Utility function to ensure plots work for a model of class DImulti
#'
#' @usage NULL
NULL
link_DImodelsMulti <- function(model, add_var = list()){
  if(!inherits(model, "DImulti")){
    cli::cli_abort(c("The model object provided should be of class
                     {.cls DImulti}"))
  }

  # Get transformed data used to fit the model
  model_data <- attr(model, "data")

  # If RM component used in model
  time_flag <- attr(model, "Timeflag")
  if(isTRUE(time_flag)){
    time_col <- attr(model, "time")
    # If variable is not specified in add_var then add it
    if(is.null(add_var[[time_col]])){
      time_vals <- unique(model_data[, time_col])
      add_var[[time_col]] <- time_vals
    }
  }

  # If MV component used in model
  MV_flag <- attr(model, "MVflag")
  if(isTRUE(MV_flag)){
    MV_col <- attr(model, "Yfunc")
    # If variable is not specified in add_var then add it
    if(is.null(add_var[[MV_col]])){
      MV_vals <- unique(model_data[, MV_col])
      add_var[[MV_col]] <- MV_vals
    }
  }
  return(add_var)
}

#' @keywords internal
#' Utility function to predict from a DImodelsMulti model object
#'
#' @usage NULL
NULL
predict_from_DImulti <- function(model, newdata = model$original_data, ...){
  # Get ID_col
  ID_col <- attr(model, "unitIDs")
  if(check_col_exists(newdata, ID_col)){
    newdata <- newdata %>% select(-all_of(ID_col))
  }
  cols <- colnames(newdata)
  if (any(c("AV", "E", "NULL", "NA") %in% cols)) {
    cols_to_remove <- cols[cols %in% c("AV", "E", "NULL", "NA")]
    newdata <- newdata %>% select(-all_of(cols_to_remove))
  }
  if (any(startsWith(colnames(newdata), c("FULL.", "FG.")))) {
    cols_to_remove <- cols[startsWith(cols, c("FULL.", "FG."))]
    newdata <- newdata %>% select(-all_of(cols_to_remove))
  }
  if (any(endsWith(colnames(newdata), c("_add")))) {
    cols_to_remove <- cols[endsWith(cols, c("_add"))]
    newdata <- newdata %>% select(-all_of(cols_to_remove))
  }
  preds <- suppressWarnings(predict(object = model, newdata = newdata,
                                    stacked = F, ...))
  preds <- preds %>% mutate(across(everything(), function(x) ifelse(is.nan(x), 0, x)))
  preds <- if (ncol(preds) > 2) rowSums(preds[, -1]) else preds[, -1]
  return(preds)
}

#' @keywords internal
#' Utility function to add experimental structures from a DImodels object to data
#'
#' @usage NULL
NULL
add_exp_str <- function(data, model){

  if(missing(data)){
    cli::cli_abort(c("{.var data} cannot be empty.",
                     "i" = "Specify a data-frame or tibble containing species
                     proportions that sum to 1 to create the appropriate
                     interaction structures."))
  }

  if(missing(model)){
    cli::cli_abort(c("{.var model} cannot be empty.",
                     "i" = "Specify a model object fit using the
                     {.help [{.pkg DImodels}](DImodels::DImodels)} or
                     {.help [{.pkg DImodelsMulti}](DImodelsMulti::DImodelsMulti)}
                     R packages."))
  }

  DImodel_tag <- attr(model, "DImodel")
  if (DImodel_tag == "CUSTOM") {
    cli::cli_warn(c("!" = "Cannot add experimental structures for a custom DI model.",
                    "i"= "Returning original data-frame"))
    return(data)
  }

  original_data <- model$original_data
  # Checking for experimental structures
  treat <- eval(model$DIcall$treat)
  density <- eval(model$DIcall$density)
  block <- eval(model$DIcall$block)

  structures <- list('treatment' = treat,
                     'density' = density,
                     'block' = block)

  updated_newdata <- as.data.frame(data)

  for (covariate in structures){
    if (!is.null(covariate)  && !is.na(covariate)){
      # If covariate was supplied as numeric in function call, getting its value
      if (is.numeric(covariate)){
        covariate <- colnames(original_data)[covariate]
      }

      if (is.numeric(original_data[, covariate])){
        if ( !(covariate %in% colnames(updated_newdata))){
          updated_newdata[, covariate] <- median(original_data[, covariate])
        }
      } else {
        # Levels of factor covariate in original data
        covariate_levels <- as.factor(unique(original_data[, covariate]))
        # If covariate isn't present in data, estimating for base level
        if ( !(covariate %in% colnames(updated_newdata))){
          updated_newdata[, covariate] <- levels(covariate_levels)[1]
        }

        # If levels of covariate in data not matching ones in original data, stop prediction
        if (! (all(unique(updated_newdata[, covariate]) %in% covariate_levels, na.rm = TRUE))){
          cli::cli_abort(c("Values given for {.val {covariate}} were not present in
                           training data used for fitting.",
                           "i" = "Predictions can not be made for these values."))
        }

        # If covariate is supplied as character or numeric, converting to factor
        if (!is.factor(updated_newdata[, covariate])){
          updated_newdata[, covariate] <- factor(updated_newdata[, covariate],
                                                 levels = levels(covariate_levels))
        }
      }
    }
  }


  # Handling extra formula
  extra_formula <- eval(model$DIcall$extra_formula)

  if (! is.null(extra_formula)){
    # If any column from extra_formula is missing in updated_newdata
    e <- try(model.frame(terms(extra_formula), updated_newdata), silent = TRUE)
    if(inherits(e, "try-error")){
      extra_vars <- model.frame(terms(extra_formula), original_data)
      for (covariate in colnames(extra_vars)){
        if(!covariate %in% colnames(updated_newdata)){
          if(is.numeric(extra_vars[, covariate])){
            updated_newdata[, covariate] <- median(extra_vars[, covariate])
          } else {
            # Levels of factor covariate in original data
            covariate_levels <- as.factor(unique(extra_vars[, covariate]))
            # If covariate isn't present in data, estimating for base level
            if ( !(covariate %in% colnames(updated_newdata))){
              updated_newdata[, covariate] <- levels(covariate_levels)[1]
            }

            # If covariate is supplied as character or numeric, converting to factor
            if (!is.factor(updated_newdata[, covariate])){
              updated_newdata[, covariate] <- factor(updated_newdata[, covariate],
                                                     levels = levels(covariate_levels))
            }
          }
        }
      }
    }

    extra_data <- model.frame(terms(extra_formula), updated_newdata)


    # Matching factors in extra_formula to ones in original_data
    og_factors <- original_data[, sapply(original_data, function(x){is.factor(x) | is.character(x)})]
    common_factors <- intersect(colnames(extra_data), colnames(og_factors))

    if (length(common_factors)!=0){

      # Levels of all factors in extra_formula
      xlevels <- lapply(common_factors, function(x){
        if(is.factor(original_data[, x])){
          return(levels(original_data[,x]))
        } else {
          return(levels(as.factor(original_data[,x])))
        }
      })
      names(xlevels) <- common_factors

      for (i in common_factors){

        # If levels of factors in extra_formula in data not matching ones in original data, stop prediction
        if (! (all(unique(updated_newdata[, i]) %in% xlevels[[i]], na.rm = TRUE))){
          cli::cli_abort(c("Values given for {.val {i}} were not present in
                           training data used for fitting.",
                           "i" = "Predictions can not be made for these values."))
        }

        # If factors in extra_formula is supplied as character or numeric, converting to factor
        if (!is.factor(updated_newdata[, i])){
          updated_newdata[, i] <- factor(updated_newdata[,i], levels = xlevels[[i]])
        }
      }
    }

    # Having certain functions in extra_formula like poly, ns, bs, etc.
    # cause problems in estimating model.matrix for data

    # So my solution is to simply refit the model with glm when
    # such functions are used and then make the prediction
  }

  fmla <-  eval(model$DIcheck_formula)

  mod_mat <- model.matrix(fmla[-2], data = updated_newdata)

  new_vars <- colnames(mod_mat)[!colnames(mod_mat) %in% colnames(updated_newdata)]
  updated_newdata <- cbind(updated_newdata, mod_mat[, new_vars])
  return(updated_newdata)
}


