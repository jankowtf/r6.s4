require(testthat)
test_that("S4", {

  ## Understanding the `where` argument in `getClasses()` //
  env <- environment()
  clss <- getClasses()
  clss_2 <- lapply(search(), function(ii) {
    getClasses(where= as.environment(ii))
  })
  names(clss_2) <- search()
  clss_2
  if (identical(env, .GlobalEnv)) {
    expect_true(identical(clss, clss_2$`package:methods`))
  } else {
    expect_true(!length(clss))
  }

  ## Conditional `where` //
  searchlist <- search()
  where <- if ("package:r6.s4" %in% searchlist) {
    where <- as.environment("package:r6.s4")
    expect_true(!length(getClasses(where = where)))
    where
  } else {
    .GlobalEnv
  }

  ## S4 method //
  setGeneric("foo", signature = "x",
    def = function(x) standardGeneric("foo")
  )
  setMethod("foo", c(x = "R6"),
    definition = function(x) {
      "I'm the method for `R6`"
    })

  ## Set old classes //
  if (!isClass("Object")) {
    expect_error(foo(x = Object$new()))
  }
  setOldClass(c("Object", "R6"), where = where)
  expect_true("Object" %in% getClasses(where = where))
  expect_identical(foo(x = Object$new()),
    "I'm the method for `R6`")

  if (!isClass("Api")) {
    expect_error(foo(x = Api$new()))
  }
  setOldClass(c("Api", "Object"), where = where)
  expect_true("Api" %in% getClasses(where = where))
  expect_identical(foo(x = Api$new()),
    "I'm the method for `R6`")

  if (!isClass("Module")) {
    expect_error(foo(x = Module$new()))
  }
  setOldClass(c("Module", "Api"), where = where)
  expect_true("Module" %in% getClasses(where = where))
  expect_identical(foo(x = Module$new()),
    "I'm the method for `R6`")

  removeClass("Object", where = where)
  removeClass("Api", where = where)
  removeClass("Module", where = where)

})
