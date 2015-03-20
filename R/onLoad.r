.onLoad <- function(libname, pkgname) {
  print(search())
}
.onAttach <- function(libname, pkgname) {
  print(search())
  where <- if ("package:r6.s4" %in% search()) {
    as.environment("package:r6.s4")
  } else {
    .GlobalEnv
  }
  print(where)
  clss <- list(
    c("Object", "R6"),
    c("Api", "Object"),
    c("Module2", "Api")
  )
  print("Before:")
  print(getClasses(where = where))
  sapply(clss, function(cls) {
    idx <- sapply(cls, isClass)
    try(sapply(cls[idx], removeClass, where = where))
    try(setOldClass(cls, where = where))
  })
  print("After:")
  print(getClasses(where = where))
}
