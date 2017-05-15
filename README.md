# r-data-recipes

A family cookbook of data `R`ecipes.

- [Rowwise](#rowwise)
    - Row totals using `rowwise()`, `do()`, and `case_when()`
- [Utility](#utility)
    - `%gin%`: A reimagination of `%in%` using `grepl()` for partial string matching
- [Distinct](#distinct)
    - Keep distinct instances of a category using `if_else()` and `distinct()`

## Rowwise

* Row totals using `rowwise()`, `do()`, and `case_when()`
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
        !(is.na(.$v1) & is.na(.$v2) & is.na(.$v3)),
        sum(.$v1, .$v2, .$v3, na.rm = TRUE), 
        if_else(
            is.na(.$v1) & is.na(.$v2) & is.na(.$v3),
            99,
            NA_real_)
    ))
# mutate with case_when doesn't work
b <- df %>% mutate(v_sum =
    case_when(
        !(is.na(.$v1) & is.na(.$v2) & is.na(.$v3)) ~ sum(.$v1, .$v2, .$v3, na.rm = TRUE),
        is.na(.$v1) & is.na(.$v2) & is.na(.$v3) ~ 99,
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

## Utility

* `%gin%`: A reimagination of `%in%` using `grepl()` for partial string matching
```r
# define %gin%
"%gin%" <- function(pattern, x) grepl(pattern, x)
# %in% evaluates to FALSE because it looks for full string matches
"a" %in% "apple"
# %gin% evaluates to TRUE
"a" %gin% "apple"
```

## Distinct

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