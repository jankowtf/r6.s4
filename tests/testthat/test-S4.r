require(testthat)

test_that("initial investigation (2015-03-19 20:30)", {

  skip("Manually only and assumptions need to be fulfilled (OUTDATED)")

  ## ASSUMPTIONS //
  ## 1) In turn, sourcing via `source()` and then via `devtools::load_all()`
  ## 2) Interactively switching between calling `setOldClass()` inside the
  ##    respective block in `R/classes.r` and not calling it at all in order
  ##    to call it directly withing this test.

  ## OUTDATED //
  ## This test has been written at a time where it was unclear where and
  ## when exactly to call `setOldClass()`.
  ## As of 2015-03-19 23:50 I know that this has to happen inside either
  ## `.onLoad()` or `.onAttach()` with the latter making it possible to set
  ## the classes inside the package's namespace which is the best option.

  ## Understanding the `where` argument in `getClasses()` //
  env <- environment()
  clss <- getClasses()
  clss_2 <- lapply(search(), function(ii) {
    getClasses(where= as.environment(ii))
  })
  names(clss_2) <- search()

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

test_that("class name clashes (2015-03-19 23:30)", {

  skip("Manually only and assumptions need to be fulfilled (OUTDATED)")

  ## ASSUMPTIONS //
  ## 1) Sourced with `devtools::load_all()`
  ## 2) No call to `setOldClass()` yet

  ## OUTDATED //
  ## This test has been written at a time where it was unclear where and
  ## when exactly to call `setOldClass()`.
  ## As of 2015-03-19 23:50 I know that this has to happen inside either
  ## `.onLoad()` or `.onAttach()` with the latter making it possible to set
  ## the classes inside the package's namespace which is the best option.

  # showClass("Object")
  expect_error(info <- getClass("Object"))
  # showClass("Api")
  expect_error(info <- getClass("Api"))
  # showClass("Module")
  expect_is(
    getClass("Module"),
    "classRepresentation"
  )
  expect_is(
    getClass("Module", where = as.environment("package:r6.s4")),
    "classRepresentation"
  )
  ## --> argument `where` doesn't really seem to matter; R just looks
  ## "anywhere" for it
  expect_identical(attributes(getClass("Module"))$package, "Rcpp")

  expect_true(hasNameClash("Module"))
  expect_message(expect_true(hasNameClash("Module", strict = 1)))
  expect_warning(expect_true(hasNameClash("Module", strict = 2)))
  expect_error(hasNameClash("Module", strict = 3))
  expect_false(hasNameClash("NotYetExistingClass"))

  expect_false(isClass("UnlikelyClassName1"))
  trySetOldClass("UnlikelyClassName1")
  expect_true(isClass("UnlikelyClassName1"))

  expect_error(trySetOldClass("Module"))

})

test_that(".onAttach (2015-03-19 23:50)", {

  skip("Manually only and assumptions need to be fulfilled(LATEST)")

  ## ASSUMPTIONS //
  ## 1) Sourced with `devtools::load_all()`
  ## 2) Calls to `setOldClass()` happen inside `onAttach()`

  expect_is(r6.s4::Object, "R6ClassGenerator")

  expect_true(isClass("Object"))
  ## --> will only be true if `setOldClass()` is called from the R session
  ## that loads the package or inside of either `.onLoad()` or `onAttach()`
  expect_true(isClass("Api"))
  expect_true(isClass("Module"))
  expect_true(isClass("Module2"))

  # getClasses()
  where <- as.environment("package:r6.s4")
  expect_true(all(c("Object", "R6") %in% getClasses(where = where)))
  expect_true(all("Api" %in% getClasses(where = where)))
  expect_false(all("Module" %in% getClasses(where = where)))
  expect_true(all("Module2" %in% getClasses(where = where)))

  expect_true(attributes(getClass("Object"))$package == "r6.s4")
  expect_true(attributes(getClass("Api"))$package == "r6.s4")
  expect_true(attributes(getClass("Module"))$package == "Rcpp")
  expect_true(attributes(getClass("Module2"))$package == "r6.s4")

})

test_that("S4 methods (2015-03-19 23:50)", {

  setGeneric("foo", signature = "x",
    def = function(x) standardGeneric("foo")
  )
  setMethod("foo", c(x = "R6"),
    definition = function(x) {
      "I'm the method for `R6`"
    })

  expect_identical(foo(x = Object$new()), "I'm the method for `R6`")
  expect_identical(foo(x = Api$new()), "I'm the method for `R6`")
  expect_error(foo(x = Module$new()))
  expect_identical(foo(x = Module2$new()), "I'm the method for `R6`")

})

test_that("hasNameClash()", {

  expect_true(hasNameClash("Module"))
  expect_message(expect_true(hasNameClash("Module", strict = 1)))
  expect_warning(expect_true(hasNameClash("Module", strict = 2)))
  expect_error(hasNameClash("Module", strict = 3))
  expect_false(hasNameClash("NotYetExistingClass"))

})

test_that("trySetOldClass()", {

  where <- as.environment("package:r6.s4")
  expect_false(isClass("UnlikelyClassName1"))
  trySetOldClass("UnlikelyClassName1", where = where)
  expect_true(isClass("UnlikelyClassName1"))
  removeClass("UnlikelyClassName1", where = where)

  expect_error(trySetOldClass("Module"))

})
