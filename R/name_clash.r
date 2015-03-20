hasNameClash <- function(name = "Module", strict = 0:3) {
  strict <- as.numeric(match.arg(as.character(strict), c("0", "1", "2", "3")))
  res <- try(info <- getClass(name), silent = TRUE)
  out <- if (inherits(res, "classRepresentation")) {
    out <- TRUE
    attributes(out)$class_info <- res
    out
  } else {
    FALSE
  }
  if (out) {
    msg <- sprintf("Class '%s' has name clash with package '%s'",
      name, attributes(res)$package)
    if (strict == 1) {
      message(msg)
    } else if (strict == 2) {
      warning(msg)
    } else if (strict == 3) {
      stop(msg)
    }
  }
  out
}

trySetOldClass <- function(Classes, ...) {
  sapply(Classes, hasNameClash, strict = 3)
  setOldClass(Classes = Classes, ...)
}
