# setup
library(dplyr)
library(magrittr)
library(stringr)
library(palmerpenguins)

# mutate columns with random samples of island
penguins %<>%
  mutate(
    island_two = sample(island),
    island_three = sample(island)
  )

# mutate any across multiple columns looking for strings
penguins_same_string <-
  penguins %>% 
  rowwise() %>%
  mutate(
    like_islands = case_when(
      any(across(contains("island")) == "Dream") ~ 1L,
      TRUE ~ 0L
    ))

# mutate any across multiple columns looking for partial strings
penguins_same_partial_string <-
  penguins %>% 
  rowwise() %>%
  mutate(
    like_islands = case_when(
      any(str_detect(c_across(contains("island")), "Dre")) ~ 1L,
      TRUE ~ 0L
    ))

# mutate any specify individual columns
penguins_specify_cols <- 
  penguins %>%
  rowwise() %>%
  mutate(
    like_islands = case_when(
      any(across(c(island, island_two, island_three)) == "Dream") ~ 1L,
      TRUE ~ 0L
    ))
