library(testthat)

is_tables <- list(
  data.frame = base::data.frame(),
  data.table = data.table::data.table(),
  tbl_df = tibble::tibble(),
  tbl_dt = dtplyr::tbl_dt(data.table::data.table())
)

test_that("is_tabular() accepts all supported tabular objects", {
  for (i in seq_along(is_tables)) {
    expect_true(is_tabular(is_tables[[i]]), label = sprintf("is_tabular(%s)", names(is_tables)[i]))
  }
})

is_not_tables <- list(
  list = list()
)

test_that("is_tabular() rejects non-tabular objects", {
  for (i in seq_along(is_not_tables)) {
    expect_false(is_tabular(is_not_tables[[i]]), label = sprintf("is_tabular(%s)", names(is_not_tables)[i]))
  }
})

x <- list(a = 1, b = 2)
as_tables <- list(
  data.frame = base::as.data.frame(x),
  data.table = data.table::as.data.table(x),
  tbl_df = tibble::as_tibble(x),
  tbl_dt = dtplyr::tbl_dt(data.table::as.data.table(x))
)

test_that("as_tabular() returns correct object", {
  for (i in seq_along(as_tables)) {
    expect_identical(
      as_tabular(x, names(as_tables)[i]),
      as_tables[[i]],
      label = sprintf("as_tabular(x, '%s')", names(as_tables)[i])
    )
  }
})

tables <- list(
  data.frame = base::data.frame(a = 1, b = 2),
  data.table = data.table::data.table(a = 1, b = 2),
  tbl_df = tibble::tibble(a = 1, b = 2),
  tbl_dt = dtplyr::tbl_dt(data.table::data.table(a = 1, b = 2))
)

test_that("tabular() returns correct object", {
  for (i in seq_along(tables)) {
    expect_identical(
      tabular(a = 1, b = 2, class = names(tables)[i]),
      tables[[i]],
      label = sprintf("tabular(x, class = '%s')", names(tables)[i])
    )
  }
})

