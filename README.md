# tabular: Tabular Class Conversion

tabular is an R package that provides a simple programmatic interface for tabular class conversion. It is meant to simplify the task of writing functions that return tabular objects whose classes match the ones input by the user.

The following tabular classes are supported:

- `data.frame` (base)
- `data.table` ([data.table](https://github.com/Rdatatable/data.table))
- `tbl_df` ([tibble](https://github.com/tidyverse/tibble))
- `tbl_dt` ([dtplyr](https://github.com/hadley/dtplyr))

## Installation

``` r
install.packages("devtools")
devtools::install_github("ezwelty/tabular")
```

## Example

The main purpose of `tabular` is to write functions that accept any class of tabular object and return the result in the same class as the input. For example, if you want to process the data using `data.table` syntax:

``` r
function(x) {
  stopifnot(tabular::is_tabular(x))
  y = tabular::as_tabular(x, class = "data.table")
  # ... data.table stuff ... #
  tabular::as_tabular(y, class = x)
}
```
