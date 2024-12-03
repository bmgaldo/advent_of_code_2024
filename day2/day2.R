setwd("~/Documents/GitHub/advent_of_code_2024/day2")

library(readr)
input <- read_delim("input.txt", delim = "\t",
                    escape_double = FALSE,
                    col_names = FALSE)

input_list = input$X1 |> 
  strsplit(split = " ") |>
  lapply(function(x)as.numeric(x))

diff_list = lapply(input_list,
                   function(x)diff(x))

# check for na's
unlist(diff_list) |> is.na() |> any() 

safe_or_not <- lapply(diff_list,function(x){
  (length(unique(sign(x)))==1) & 
    (max(abs(x)) < 4) & 
    (min(abs(x)) > 0)
})

# problem 1 solution  
part1 = safe_or_not |> unlist() |> sum()
print(part1)

# names(diff_list) <- as.character(1:length(diff_list))
# diff_list[which(unlist(safe_or_not))] 

# flag the number of elements the difference meets
# level_flagger <- lapply(diff_list,
#                         function(x){
#                           3 -
#                             ((sign(median(x))==sign(x)) +
#                                (abs(x)<4) +
#                                (abs(x)>0))
#                         }
# )
# 
# number_of_bad_diffs <- lapply(level_flagger,
#                               function(x)sum(x>0))  
# number_of_bad_diffs |> 
#   unlist() |> 
#   table()

# before Problem Dampener
unsafe_indices = which(!unlist(safe_or_not))

names(input_list) <- as.character(1:length(input_list))
unsafe_list <- input_list[unsafe_indices]

# vector of 0's for
safe_now <- numeric(length(unsafe_list))

# apply problem dampener 
# most naive way i know of solving this problem but i am stumped
for(i in 1:length(unsafe_list)){
  
  n_len = length(unsafe_list[[i]]) 
  
  for(j in 1:n_len){
    
    x <- unsafe_list[[i]] 
    dx <- diff(x[-j]) # remove j-th level and calculate differences
    
    # retest
    if((length(unique(sign(dx)))==1) & 
       (max(abs(dx)) < 4) & 
       (min(abs(dx)) > 0)
    ){
      safe_now[i] = 1
      # stop once you know it works for a repot
      break
    }
  }
  
}

part2 = part1 + sum(safe_now) 
part2

# diff could large, sign wrong
# diff could large, sign right
# diff could small, sign large
#
# any position, one zero diff and you're good
#
# large or small diff at the end and you're good
# large or small diff at the beginning and you're good
##############