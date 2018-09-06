# r-data-recipes

A family cookbook of data `R`ecipes.

- [Row totals](#row-totals)
    - Calculate row totals using `rowwise()`, `do()`, and `case_when()`
- [Partial string matching](#partial-string-matching)
    - `%gin%`: A reimagination of `%in%` using `grepl()` for partial string matching
- [Keep distinct categories](#keep-distinct-categories)
    - Keep distinct instances of a category using `if_else()` and `distinct()`
- [Reinstall packages after a major R update](#reinstall-packages-after-a-major-r-update)
    - Reinstall packages from your previous R 3.x library path after a major R update.
- [Unnest all list-cols into columns](#unnest-all-list-cols-into-columns)
    - Unnest all list-cols in a data frame into columns for each unique element.

## Row totals

* Calculate row totals using `rowwise()`, `do()`, and `case_when()`
```r
# setup
library(tidyverse)
df <- data_frame(
  id = c(1,2,3,4),
  v1 = c(NA,1,1,NA),
  v2 = c(NA,1,NA,NA),
  v3 = c(NA,1,1,NA)
)
# mutate with if_else doesn't work
a <- df %>% mutate(v_sum =
    if_else(
        !(is.na(v1) & is.na(v2) & is.na(v3)),
        sum(v1, v2, v3, na.rm = TRUE), 
        if_else(
            is.na(v1) & is.na(v2) & is.na(v3),
            99,
            NA_real_)
    ))
# mutate with case_when doesn't work
b <- df %>% mutate(v_sum =
    case_when(
        !(is.na(v1) & is.na(v2) & is.na(v3)) ~ sum(v1, v2, v3, na.rm = TRUE),
        is.na(v1) & is.na(v2) & is.na(v3) ~ 99,
        TRUE ~ NA_real_)
)
# rowwise and do with case_when works, but requires binding the result back to the original data frame
c <- df %>% rowwise %>% do(v_sum =
case_when(
        !(is.na(.$v1) & is.na(.$v2) & is.na(.$v3)) ~ sum(.$v1, .$v2, .$v3, na.rm = TRUE),
        is.na(.$v1) & is.na(.$v2) & is.na(.$v3) ~ 99,
        TRUE ~ NA_real_)
) %>% unlist %>% as_tibble %>% bind_cols(df, .)
```

## Partial string matching

* `%gin%`: A reimagination of `%in%` using `grepl()` for partial string matching
```r
# define %gin%
"%gin%" <- function(pattern, x) grepl(pattern, x)
# %in% evaluates to FALSE because it looks for full string matches
"a" %in% "apple"
# %gin% evaluates to TRUE
"a" %gin% "apple"
```

## Keep distinct categories

* Keep distinct instances of a category using `if_else()` and `distinct()`
```r
# setup
library(tidyverse)
df <- data_frame(
  category = c("% proficient", "% proficient", "% proficient", "n proficient", "n proficient", "n proficient"),
  race = c("YES", "NO", "NO", "YES", "NO", "NO"),
  gender = c("NO", "NO", "YES", "NO", "NO", "NO"),
  frpl = c("NO", "NO", "NO", "NO", "YES", "NO"),
  race.x.gender = c("NO", "NO", "NO", "NO", "NO", "NO")
)
# answer 1
df %>%
  group_by(category) %>%
  mutate(
    race = TRUE & "YES" %in% race,
    gender = TRUE & "YES" %in% gender,
    frpl = TRUE & "YES" %in% frpl,
    race.x.gender = TRUE & "YES" %in% race.x.gender
  ) %>%
  distinct(.keep_all = TRUE)
# answer 2
df %>%
  group_by(category) %>%
  mutate(
    race = if_else("YES" %in% race, "YES", "NO"),
    gender = if_else("YES" %in% gender, "YES", "NO"),
    frpl = if_else("YES" %in% frpl, "YES", "NO"),
    race.x.gender = if_else("YES" %in% race.x.gender, "YES", "NO")
  ) %>%
  distinct(.keep_all = TRUE)
```

## Reinstall packages after a major R update

* Reinstall packages from your previous R 3.x library path after a major R update. Note that RStudio will prompt you to restart R repeatedly; to keep the script going keep pressing "No" when this happens.
```r
# setup
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
# get the new and old R versions as strings
new_r <- str_sub(.rs.rVersionString(), 1L, 3L)
old_r <- as.character(as.numeric(new_r) - 0.1)
# get your new and old R library paths
new_l <- .libPaths()
old_l <- str_replace(new_l, new_r, old_r)
# get the list of old installed packages
pkg_list <- as.list(list.files(old_l))
# install all packages listed in pkg_list
install_all <- function(x) {
  print(x)
  install.packages(x, quiet = TRUE)
}
quietly(lapply(pkg_list, install_all))
```

## Unnest all list-cols into columns

* Unnest all list-cols in a data frame into columns for each unique element.
``` r
# setup
suppressPackageStartupMessages(library(dplyr))
library(purrr)
library(stats)
library(tibble)
library(tidyr)
# create `a`, a tbl with 1 list-col, note "four" is not lined up with "five" in row 3
a <- tibble::tribble(
  ~v1,     ~v2,
  "one",   c("four", "five", "six"),
  "two",   NA_character_,
  "three", "five"
)
# create `b`, an even more complicated tbl with 2 list-cols, separated by an atomic v3
b <- tibble::tribble(
  ~v1,     ~v2,                      ~v3,    ~v4,
  "one",   c("four", "five", "six"), "four", c("four", "five", "six"),
  "two",   NA_character_,            "five", "four",
  "three", "five",                   "six",  "six"
)
# implement `unnest_wide()`
unnest_wide <- function(.data) {
  stopifnot(is.data.frame(.data))
  .data <- tibble::rowid_to_column(.data)
  lst_index <- purrr::map_int(.data, is.list)
  lst_cols <- names(lst_index)[lst_index == 1L]
  lst_vals <- paste0(lst_cols, ".")
  unique_vals <- vector("list", length(lst_cols))
  tmp <- vector("list", length(lst_cols))
  for (i in seq_along(lst_cols)) {
    unique_vals[[i]] <- stats::na.omit(unique(unlist(.data[[lst_cols[i]]])))
    tmp[[i]] <- dplyr::select(.data, rowid, lst_cols[i])
    tmp[[i]] <- dplyr::mutate(tmp[[i]], !!lst_vals[i] := .data[[lst_cols[i]]])
    tmp[[i]] <- tidyr::unnest(tmp[[i]])
    tmp[[i]] <- dplyr::mutate(tmp[[i]], !!lst_cols[i] := match(tmp[[i]][[lst_cols[i]]], unique_vals[[i]]))
    tmp[[i]] <- tidyr::spread(tmp[[i]], !!lst_cols[i], !!lst_vals[i], convert = TRUE, sep = "_")
    tmp[[i]] <- dplyr::select_if(tmp[[i]], !grepl(paste0(lst_cols[i], "_NA"), colnames(tmp[[i]])))
    .data <- dplyr::select(.data, -(!!lst_cols[i]))
    .data <- dplyr::left_join(.data, tmp[[i]], by = "rowid")
  }
  .data <- dplyr::select(.data, -rowid)
  return(.data)
}
# run on `a` and `b`
(a_wide <- unnest_wide(a))
#> # A tibble: 3 x 4
#>   v1    v2_1  v2_2  v2_3 
#>   <chr> <chr> <chr> <chr>
#> 1 one   four  five  six  
#> 2 two   <NA>  <NA>  <NA> 
#> 3 three <NA>  five  <NA>
(b_wide <- unnest_wide(b))
#> # A tibble: 3 x 8
#>   v1    v3    v2_1  v2_2  v2_3  v4_1  v4_2  v4_3 
#>   <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
#> 1 one   four  four  five  six   four  five  six  
#> 2 two   five  <NA>  <NA>  <NA>  four  <NA>  <NA> 
#> 3 three six   <NA>  five  <NA>  <NA>  <NA>  six
```

Created on 2018-07-25 by the [reprex package](http://reprex.tidyverse.org) (v0.2.0).
