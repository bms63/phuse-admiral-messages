hgt <- c(147, 163)
wgt <- c(54, 78.5)

# First version ----------------------------------------------------------------
bmi <- function(hgt, wgt) {
  wgt / (hgt / 100) ^ 2
}
bmi(hgt = hgt, wgt = wgt)
bmi(hgt = letters, wgt = LETTERS)
# - error message references code the user has never seen
# - not entirely clear where the error is coming from

# check inputs using base R stop() ---------------------------------------------
bmi <- function(hgt, wgt) {
  # check inputs --------------------------------
  if (!is.numeric(hgt)) {
    stop("The hgt argument must be numeric.")
  }
  if (!is.numeric(wgt)) {
    stop("The wgt argument must be numeric.")
  }
  
  # perform calculation -------------------------
  wgt / (hgt / 100) ^ 2
}
bmi(hgt = hgt, wgt = wgt)
bmi(hgt = letters, wgt = LETTERS)
# - error message references the calling function. but also includes all arguments, which can be difficult to read for bigger functions
# - but I do not like the all red text: makes it difficult to see what is the important parts


# check inputs using cli::cli_abort() ------------------------------------------
bmi <- function(hgt, wgt) {
  # check inputs --------------------------------
  if (!is.numeric(hgt)) {
    cli::cli_abort("The {.arg hgt} argument must be numeric.")
  }
  if (!is.numeric(wgt)) {
    cli::cli_abort("The {.arg wgt} argument must be numeric.")
  }
  
  # perform calculation -------------------------
  wgt / (hgt / 100) ^ 2
}
bmi(hgt = hgt, wgt = wgt)
bmi(hgt = letters, wgt = LETTERS)
# - clear message that the error occurred in the `bmi()` function
# - the hgt argument is wrapped in backticks, making the full message easier to read.
# - includes a `rlang::last_trace()` link, so we can easily devle into the details if needed.

# Adding a `check_numeric()` function ------------------------------------------
check_numeric <- function(x, arg_name = rlang::caller_arg(x)) {
  if (!is.numeric(x)) {
    cli::cli_abort("The {.arg {arg_name}} argument must be numeric.")
  }
  invisible(x)
}
# - adding the `arg_name` argument, so we can clearly message to the user which argument is problematic
# - the {cli} tools use glue syntax! Cute!

bmi <- function(hgt, wgt) {
  # check inputs --------------------------------
  check_numeric(hgt)
  check_numeric(wgt)
  
  # perform calculation -------------------------
  wgt / (hgt / 100) ^ 2
}
bmi(hgt = hgt, wgt = wgt)
bmi(hgt = letters, wgt = LETTERS)
# - but this message is worse than before! `Error in `check_numeric()`` references a non-user facing function! :<

# Add `check_numeric(call)` argument -------------------------------------------
check_numeric <- function(x, arg_name = rlang::caller_arg(x), call = parent.frame()) {
  if (!is.numeric(x)) {
    cli::cli_abort("The {.arg {arg_name}} argument must be numeric.", call = call)
  }
  invisible(x)
}
# - show that the `cli::cli_abort()` function has a call argument to specify the env from which the USER called the function
# - discuss `arg_name = rlang::caller_arg(x)` and what it does

bmi <- function(hgt, wgt) {
  # check inputs --------------------------------
  check_numeric(hgt)
  check_numeric(wgt)
  
  # perform calculation -------------------------
  wgt / (hgt / 100) ^ 2
}
bmi(hgt = hgt, wgt = wgt)
bmi(hgt = letters, wgt = LETTERS)
# - We're back and the messaging is beautiful!

# Add `check_positive()` with `c("i"=)` -----------------------------------------
check_positive <- function(x, arg_name = rlang::caller_arg(x), call = parent.frame()) {
  if (any(x <= 0)) {
    cli::cli_abort(
      c("x" = "All values in the vector passed in the {.arg {arg_name}} argument must be postive.",
        "i" = "Consider {.emph checking your data} before attempting to calculate BMI."), 
      call = call
    )
  }
  invisible(x)
}

bmi <- function(hgt, wgt) {
  # check inputs --------------------------------
  check_numeric(hgt)
  check_numeric(wgt)
  check_positive(hgt)
  check_positive(wgt)
  
  # perform calculation -------------------------
  wgt / (hgt / 100) ^ 2
}
bmi(hgt = hgt, wgt = wgt)
bmi(hgt = hgt * -1, wgt = wgt)



# Other things we love about {cli} messaging -----------------------------------
# the cli package also has functions for warnings and messages, cli::cli_warn() and cli::cli_inform()
# We already highlighted that `{.arg xxx}` styles entries to distinguish arguments from regular prose
#    There are many more ways to highlight/style messaging
#    {.code} for general code chunks
#    {.val} for general value quoting/styling values
#    {.path} consistent styling for path AND creates a clickable hyperlink to the path!
#    {.emph} to italicize text
#    {.strong} to bold text
#    {.url} for a clickable hyperlink to any URL
#    {.help} for a clickable link to a a help page
#    {.var} for styling column names
#    Pluralization!
#    Progress bars for long computations.