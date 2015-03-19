require(R6)

# Object ------------------------------------------------------------------

#' @import R6
Object <- R6Class(
  classname = "Object",
  portable = TRUE,
  public = list(
    foo = function() "foo"
  )
)

# Api ---------------------------------------------------------------------

Api <- R6Class(
  classname = "Api",
  inherit = Object,
  portable = TRUE,
  public = list(
    bar = function() "bar"
  )
)


# Module ------------------------------------------------------------------

Module <- R6Class(
  classname = "Module",
  inherit = Api,
  portable = TRUE,
  public = list(
    fooBar = function() "fooBar"
  )
)

# Conditional `where` -----------------------------------------------------

searchlist <- search()
where <- if ("package:r6.s4" %in% searchlist) {
  as.environment("package:r6.s4")
} else {
  .GlobalEnv
}
try(setOldClass(c("Object", "R6"), where = where))
try(setOldClass(c("Api", "Object"), where = where))
try(setOldClass(c("Module", "Api"), where = where))
## --> the last one fails with the following error:
# Error in setOldClass(c("Module", "Api"), where = where) (from classes_2.r#49) :
#   inconsistent old-style class information for "Module"; the class is
# defined but does not extend "Api" and is not valid as the data part
