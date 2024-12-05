setwd("~/Documents/GitHub/advent_of_code_2024/day4")
# PART 1 **BEGIN**
input <- 
  readr::read_lines("input.txt") 
n_xmas = 0 
len = length(input)
width = unique(lapply(input,nchar)) |> unlist()
# len x width = 140 x 140

###################
# find XMAS across
################### 
find_xmas_left_to_right = function(input){
  left_to_right = lapply(input,function(x){
    gregexpr(pattern=regex("([X][M][A][S])"),text=x)}) |> unlist() 
  left_to_right = left_to_right[ left_to_right > (-1) ]
  return(left_to_right)
}
find_xmas_right_to_left = function(input){
  right_to_left = lapply(input,function(x){
    gregexpr(pattern=regex("([S][A][M][X])"),text=x)}) |> unlist() 
  right_to_left = right_to_left[ right_to_left > (-1) ]
  return(right_to_left)
}

n_xmas = n_xmas + find_xmas_left_to_right(input) |> length()
n_xmas = n_xmas + find_xmas_right_to_left(input) |> length()
print(n_xmas) 
#######################
# find XMAS up and down
#######################
# create a matrix of individual chars 
char_matrix <- matrix(NA,len,width,byrow = TRUE)
for(i in 1:len){
  char_matrix[i,] <- sapply(1:len,\(y)substr(input[[i]],y,y))
}

rotate_matrix_by_90_degrees = function(x){
  t(apply(x, 2, rev))
} 

rotated_char_mat <- rotate_matrix_by_90_degrees(char_matrix) 
# collapse rows into strings 
input_rotated <- apply(rotated_char_mat,MARGIN = 1,\(x)paste0(x,collapse = ""))
# calculate
n_xmas = n_xmas + find_xmas_left_to_right(input_rotated) |> length()
n_xmas = n_xmas + find_xmas_right_to_left(input_rotated) |> length()
print(n_xmas) 
#######################
# find XMAS diagonal (top left to bottom right) 
#######################
for(i in 1:(len)){
  j = len - i + 1 
  # lower diagonal indices + main diagonal
  lower_diag_indices = seq(i,i+(j-1)*(len+1),length.out=j)
  diag_char = char_matrix[lower_diag_indices] |> paste0(collapse="")
  n_front = find_xmas_left_to_right(diag_char) |> length()
  n_backwards = find_xmas_right_to_left(diag_char) |> length()
  n_xmas = n_xmas + n_front + n_backwards
  # upper diagonal indices 
  if(j!=len){
    upper_diag_indices = seq((i-1)*len+1,
                             len^2-i+1,
                             length.out = j)
    diag_char = char_matrix[upper_diag_indices] |> paste0(collapse="")
    n_front = find_xmas_left_to_right(diag_char) |> length()
    n_backwards = find_xmas_right_to_left(diag_char) |> length()
    n_xmas = n_xmas + n_front + n_backwards
  }
}
print(n_xmas) 

#######################
# find XMAS diagonal (bottom left to top right) 
#######################
# same as above, just net to rotate algorithm 90 degrees 
for(i in 1:(len)){
  j = len - i + 1 
  # lower diagonal indices + main diagonal
  lower_diag_indices = seq(i,i+(j-1)*(len+1),length.out=j)
  
  diag_char = rotated_char_mat[lower_diag_indices] |> paste0(collapse="")
  n_front = find_xmas_left_to_right(diag_char) |> length()
  n_backwards = find_xmas_right_to_left(diag_char) |> length()
  n_xmas = n_xmas + n_front + n_backwards
  # upper diagonal indices 
  if(j!=len){
    upper_diag_indices = seq((i-1)*len+1,
                             len^2-i+1,
                             length.out = j)
    diag_char = rotated_char_mat[upper_diag_indices] |> paste0(collapse="")
    n_front = find_xmas_left_to_right(diag_char) |> length()
    n_backwards = find_xmas_right_to_left(diag_char) |> length()
    n_xmas = n_xmas + n_front + n_backwards
  }
}

print(n_xmas) 

find_mas_left_to_right = function(input){
  left_to_right = lapply(input,function(x){
    gregexpr(pattern=regex("([M][A][S])"),text=x)}) |> unlist() 
  left_to_right = left_to_right[ left_to_right > (-1) ]
  return(left_to_right)
}
find_mas_right_to_left = function(input){
  right_to_left = lapply(input,function(x){
    gregexpr(pattern=regex("([S][A][M])"),text=x)}) |> unlist() 
  right_to_left = right_to_left[ right_to_left > (-1) ]
  return(right_to_left)
}




n_crosses=0
for(i in 1:len){
  for(j in 1:len){
    if( i<=(len-2) & j<=(len-2) ){
      mat <- char_matrix[i:(i+2),j:(j+2)] 
      if(mat[5]=="A"){
        diag1 <- mat[c(1,5,9)]|> 
          paste0(collapse = "")
        diag2 <- mat[c(3,5,7)] |> 
          paste0(collapse = "")
        n_mas = length(find_mas_right_to_left(diag1)) + 
          length(find_mas_right_to_left(diag2)) + 
          length(find_mas_left_to_right(diag1)) + 
          length(find_mas_left_to_right(diag2))
        if(n_mas == 2){
          n_crosses = n_crosses + 1
        }
      }
    }
  }
}

print(n_crosses)