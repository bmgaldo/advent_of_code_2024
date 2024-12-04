setwd("~/Documents/GitHub/advent_of_code_2024/day3")
library(stringr)
library(dplyr)
library(tidyr)
# PART 1 **BEGIN**
input <- 
  readr::read_lines("input.txt") |> 
  paste(collapse = "") |> 
  unlist() |> 
  as.character()

nchar <- nchar(input)

substrings <- 
  str_extract_all(input,regex("([m][u][l][(]\\d{1,3},\\d{1,3}[)])")) |> 
  unlist() 

# PART 1 ***solution***
sapply(substrings,\(x)
       str_extract_all(x,regex("\\d{1,3}")) |> 
         unlist() |>  
         as.numeric() |> 
         prod()) |> unlist() |> sum()

# PART 2 **BEGIN**
mul_idx = 
  gregexpr(pattern="([m][u][l][(]\\d{1,3},\\d{1,3}[)])",text=input) |> 
  unlist()
dos_idx = 
  gregexpr(pattern="([d][o][(][)])",text=input) |> 
  unlist()
dont_idx = 
  gregexpr(pattern="([d][o][n]['][t])",text=input) |> 
  unlist()

n_muls = length(mul_idx) 
add_mull = numeric(n_muls) 

# order do and don't idxs
do_df = rbind(data.frame(idx=dos_idx,do=1),data.frame(idx=dont_idx,do=0)) 
do_df = do_df[order(do_df$idx),] 
do_df = dplyr::left_join(data.frame(idx = 1:max(mul_idx)),do_df)

# fill in idices where the do state is valid
do_df$do[1] <- 1
do_df = do_df |> 
  tidyr::fill(do,
              .direction = "down") 

# find the subset of indices where do is valid
mul_df = data.frame(idx = mul_idx,
                    which_mul = 1:length(mul_idx)) |> 
  left_join(do_df) |> 
  filter(do==TRUE)

# now repeat part 1
substrings <- str_extract_all(input, 
  regex("([m][u][l][(]\\d{1,3},\\d{1,3}[)])")) |> 
  unlist() 

# PART 2 ***solution***
sapply(substrings[mul_df$which_mul],\(x)
       str_extract_all(x,regex("\\d{1,3}")) |> 
         unlist() |> 
         as.numeric() |> 
         prod()) |> 
  unlist() |> 
  sum()
