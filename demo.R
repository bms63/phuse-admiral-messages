library(tibble)
library(dplyr)
library(cli)
library(glue)


hgt_good <- c(147, 163)
wgt_good <- c(54, 78.5)

hgt_bad <- c("a", "b")
wgt_bad <- c("c", "d")

bmi <- function(hgt, wgt) {
  wgt / (hgt / 100)^2
}

bmi(hgt = hgt_good, wgt = wgt_good)

bmi(hgt = hgt_bad, wgt = wgt_bad)

# Check inputs using base R stop()
bmi <- function(hgt, wgt) {
  if (!is.numeric(hgt)) {
    stop("The hgt argument must be numeric.")
  }
  if (!is.numeric(wgt)) {
    stop("The wgt argument must be numeric.")
  }

  wgt / (hgt / 100)^2
}

bmi(hgt = hgt_good, wgt = wgt_good)

bmi(hgt = hgt_bad, wgt = wgt_bad)

# check inputs using cli::cli_abort()
bmi <- function(hgt, wgt) {
  if (!is.numeric(hgt)) {
    cli::cli_abort("The hgt argument must be numeric.")
  }
  if (!is.numeric(wgt)) {
    cli::cli_abort("The wgt argument must be numeric.")
  }

  wgt / (hgt / 100)^2
}

bmi(hgt = hgt_good, wgt = wgt_good)

bmi(hgt = hgt_good, wgt = wgt_bad)

# Use glue syntax to help make stuff pop!
bmi <- function(hgt, wgt) {
  if (!is.numeric(hgt)) {
    cli::cli_abort("The {.arg hgt} argument must be numeric.")
  }
  if (!is.numeric(wgt)) {
    cli::cli_abort("The {.arg wgt} argument must be numeric.")
  }

  wgt / (hgt / 100)^2
}

bmi(hgt = hgt_good, wgt = wgt_good)

bmi(hgt = hgt_good, wgt = wgt_bad)

# Adding a `check_numeric()` function ------------------------------------------
check_numeric <- function(x, arg_name = rlang::caller_arg(x)) {
  if (!is.numeric(x)) {
    cli::cli_abort("The {.arg {arg_name}} argument must be numeric.")
  }
  invisible(x)
}

bmi <- function(hgt, wgt) {
  check_numeric(hgt)
  check_numeric(wgt)

  wgt / (hgt / 100)^2
}


bmi(hgt = hgt_good, wgt = wgt_good)

bmi(hgt = hgt_bad, wgt = wgt_bad)

# Add `check_numeric(call)` argument 
check_numeric <- function(x, 
                          arg_name = rlang::caller_arg(x), 
                          call = parent.frame()) {
  if (!is.numeric(x)) {
    cli::cli_abort("The {.arg {arg_name}} argument must be numeric.", call = call)
  }
  invisible(x)
}

bmi <- function(hgt, wgt) {
  check_numeric(hgt)
  check_numeric(wgt)

  wgt / (hgt / 100)^2
}

bmi(hgt = hgt_good, wgt = wgt_good)

bmi(hgt = hgt_bad, wgt = wgt_bad)

# Add `check_positive()` with `c("x" = , "i" = )`
check_positive <- function(x, arg_name = rlang::caller_arg(x), call = parent.frame()) {
  if (any(x <= 0)) {
    cli::cli_abort(
      c(
        "x" = "All values in the vector passed in the {.arg {arg_name}} argument must be postive.",
        "i" = "Consider {.emph checking your data} before attempting to calculate BMI."
      ),
      call = call
    )
  }
  invisible(x)
}

bmi <- function(hgt, wgt) {
  check_numeric(hgt)
  check_numeric(wgt)
  check_positive(hgt)
  check_positive(wgt)

  wgt / (hgt / 100)^2
}

bmi(hgt = hgt_good, wgt = wgt_good)

bmi(hgt = hgt_good * -1, wgt = wgt_good)

# admiral using cli and glue!!
assert_s3_class <- function(arg, 
                            cls,
                            optional = FALSE,
                            arg_name = rlang::caller_arg(arg),
                            message = NULL,
                            call = parent.frame()) {
  # if argument is optional and the value is NULL, exit function
  if (is.null(arg) && optional) {
    return(invisible(arg))
  }

  # use default message if one not provided in the `message` argument
  message <-
    ifelse(
      is.null(message),
      "Argument {.arg {arg_name}} must be class {.cls {cls}}, but is {.obj_type_friendly {arg}}.",
      message
    )

  # print error if input does not match specified class
  if (!inherits(arg, cls)) {
    cli::cli_abort(message = messagge, call = call)
  }

  invisible(arg)
}
