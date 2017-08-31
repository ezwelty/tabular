#' tabular: Tabular Class Conversion
#'
#' tabular provides a simple programmatic interface for tabular object conversion. It is meant to simplify the task of writing functions that return tabular objects whose classes match the ones input by the user.
#'
#' The following tabular classes are supported:
#'
#' \itemize{
#'   \item data.frame (see \code{base::\link[base]{data.frame}}):
#'   \item data.table (see \code{data.table::\link[data.table]{data.table}})
#'   \item tbl_df (see \code{tibble::\link[tibble]{tibble}})
#'   \item tbl_dt (see \code{dtplyr::\link[dtplyr]{tbl_dt}})
#' }
#'
#' @docType package
#' @name tabular
NULL

#' Tabular class definitions
Classes <- list(
  data.frame = list(
    new = "base::data.frame",
    as = "base::as.data.frame"
  ),
  data.table = list(
    new = "data.table::data.table",
    as = "data.table::as.data.table"
  ),
  tbl_df = list(
    new = "tibble::tibble",
    as = "tibble::as_tibble"
  ),
  tbl_dt = list(
    new = c("data.table::data.table", "dtplyr::tbl_dt"),
    as = c("data.table::as.data.table", "dtplyr::tbl_dt")
  )
)

#' Test if tabular
#'
#' Tests whether an object is a supported tabular object.
#'
#' @param x An R object.
#' @param class Tabular class to test against (class name or object of that class) or \code{NULL} to test against all supported classes.
#' @export
#' @examples
#' is_tabular(data.frame()) # TRUE
#' is_tabular(data.frame(), "data.table") # FALSE
#' is_tabular(data.frame(), data.frame()) # TRUE
#' \dontrun{
#' is_tabular(data.table::data.table()) # TRUE
#' is_tabular(dtplyr::tbl_dt(data.table::data.table())) # TRUE
#' is_tabular(tibble::tibble()) # TRUE
#' }
#' is_tabular(list()) # FALSE
is_tabular <- function(x, class = NULL) {
  if (is.null(class)) {
    any(base::class(x) %in% names(Classes))
  } else {
    base::class(x) == parse_tabular_class(class)
  }
}

#' Coerce to tabular
#'
#' Coerces an object to a tabular object of the desired type.
#'
#' @param x An R object.
#' @param class Desired tabular class (class name or object of that class). If no coercion is necessary, \code{x} is return unchanged.
#' @param ... Arguments passed to the corresponding coercion function(s).
#' @export
#' @examples
#' x <- list(a = 1, b = 2)
#' as_tabular(x, "data.frame")
#' \dontrun{
#' as_tabular(x, "data.table")
#' as_tabular(x, "tbl_df")
#' as_tabular(x, "tbl_dt")
#' }
as_tabular <- function(x, class = "data.frame", ...) {
  class <- parse_tabular_class(class)
  if (is_tabular(x, class = class)) {
    return(x)
  }
  dots <- list(...)
  used <- rep(FALSE, length(dots))
  names(used) <- names(dots)
  functions <- Classes[[class]]$as
  for (fun in functions) {
    fun_args <- formals(eval(parse(text = fun)))
    if ("..." %in% names(fun_args)) {
      arg_names <- names(dots)
    } else {
      arg_names <- intersect(names(fun_args), names(dots))
    }
    used[arg_names] <- TRUE
    args <- c(list(x), dots[arg_names])
    # FIXME: Assumes first argument is always meant for x
    names(args)[1] <- names(fun_args)[1]
    txt <- sprintf("do.call(%s, args)", fun)
    expr <- parse(text = txt)
    x <- eval(expr)
  }
  # Since coercion may require chained function calls, argument use is only checked at the end.
  if (!all(used)) {
    stop(sprintf("unused arguments (%s) in function calls (%s)", toString(names(dots)[!used]), toString(functions)))
  }
  x
}

#' Create tabular object
#'
#' Creates a tabular object of the desired class.
#'
#' @param ... Arguments passed to the corresponding creation function.
#' @param class Desired tabular class (class name or object of that class).
#' @export
#' @examples
#' tabular(a = 1, b = 2)
#' tabular(a = 1, b = 2, class = "data.frame")
#' \dontrun{
#' tabular(a = 1, b = 2, class = "data.table")
#' tabular(a = 1, b = 2, class = "tbl_df")
#' tabular(a = 1, b = 2, class = "tbl_dt")
#' }
tabular <- function(..., class = "data.frame") {
  class <- parse_tabular_class(class)
  functions <- Classes[[class]]$new
  for (i in seq_along(functions)) {
    # FIXME: Assumes secondary functions have no special arguments.
    args <- ifelse(i == 1, "...", "x")
    txt <- sprintf("%s(%s)", functions[i], args)
    expr <- parse(text = txt)
    x <- eval(expr)
  }
  x
}

#' Parse tabular class
#'
#' @param x Character vector of class names or any (non-character) R object.
parse_tabular_class <- function(x) {
  if (!is.character(x)) {
    x <- class(x)
  }
  is_supported <- x %in% names(Classes)
  if (sum(is_supported) == 0) {
    stop(sprintf("Class not supported (%s)", toString(x)))
  } else {
    x[is_supported][1]
  }
}
