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
  try(setOldClass(c("Object", "R6"), where = where))
  try(setOldClass(c("Api", "Object"), where = where))
  try(setOldClass(c("Module", "Api"), where = where))
  try(setOldClass(c("Module2", "Api"), where = where))
}
