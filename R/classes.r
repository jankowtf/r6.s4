# require(R6)
## --> only needed when manually sourcing

## LESSONS LEARNED AS OF 2015-03-19 23:50 //
## 1) AFAIU, the problem is caused by package`Rcpp` also defining a class with
##    name `Module`
## 2) The best place to call `setOldClass()` seems to be inside `.onAttach()`.
##    As the package is already fully loaded at this point, it has a namespace
##    environment that `where` can point to

# Object ------------------------------------------------------------------

#' @import R6
#' @export
Object <- R6Class(
  classname = "Object",
  portable = TRUE,
  public = list(
    foo = function() "foo"
  )
)

# Api ---------------------------------------------------------------------

#' @export
Api <- R6Class(
  classname = "Api",
  inherit = r6.s4::Object,
  portable = TRUE,
  public = list(
    bar = function() "bar"
  )
)

# Module ------------------------------------------------------------------

#' @export
Module <- R6Class(
  classname = "Module",
  inherit = r6.s4::Api,
  portable = TRUE,
  public = list(
    fooBar = function() "fooBar"
  )
)

#' @export
Module2 <- R6Class(
  classname = "Module2",
  inherit = r6.s4::Api,
  portable = TRUE,
  public = list(
    fooBar = function() "fooBar"
  )
)

# Conditional `where` -----------------------------------------------------

if (FALSE) {
  where <- if ("package:r6.s4" %in% search()) {
    as.environment("package:r6.s4")
  } else {
    .GlobalEnv
  }
  print(where)
  ## --> as even when using `devtools::load_all()` the package has not been
  ## loaded yet in the sense that a namespace environment has been created,
  ## the following `setOldClass()` statements will only create classes in
  ## `.GlobalEnv`:

  try(setOldClass(c("Object", "R6"), where = where))
  try(setOldClass(c("Api", "Object"), where = where))
  try(setOldClass(c("Module", "Api"), where = where))
  ## --> the last one fails with the following error:
  # Error in setOldClass(c("Module", "Api"), where = where) (from classes_2.r#49) :
  #   inconsistent old-style class information for "Module"; the class is
  # defined but does not extend "Api" and is not valid as the data part

  ## UPDATE 2015-03-19 23:00
  ## The error occurs due to a name clash with `Module` from package
  ## `Rcpp`. See `getClass("Module")` and unit test `test-S4.r` for details
}

