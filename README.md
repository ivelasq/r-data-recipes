# r-data-recipes

A family cookbook of data `R`ecipes.

- [Row totals](#row)
    - Calculate row totals using `rowwise()`, `do()`, and `case_when()`
- [Partial string matching](#partial)
    - `%gin%`: A reimagination of `%in%` using `grepl()` for partial string matching
- [Keep distinct categories](#keep)
    - Keep distinct instances of a category using `if_else()` and `distinct()`
- [Reinstall packages after a major R update](#reinstall)
    - Reinstall packages from your previous R 3.x library path after a major R update.

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
require(tidyverse)
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