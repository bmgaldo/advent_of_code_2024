library(stringr)
setwd("~/Documents/GitHub/advent_of_code_2024/day3")
input <- readr::read_lines("input.txt") |> 
  paste(collapse = "") |> unlist() |> as.character()

nchar <- nchar(input)

substrings <- str_extract_all(input, 
                              regex("([m][u][l][(]\\d{1,3},\\d{1,3}[)])")) |> 
  unlist() 
# part 1
sapply(substrings,\(x)
       str_extract_all(x,regex("\\d{1,3}")) |> 
         unlist() |> 
         as.numeric() |> 
         prod()) |> unlist() |> sum()


# <- sapply(1:nchar,function(x)substr(x,x))
# input |> substr(,1:100,1:100)
# str_split

# substring1 <- str_extract_all(input, regex("([m][u][l][(]\\d,\\d[)])"))
# substring2 <- str_extract_all(input, regex("([m][u][l][(]\\d,\\d\\d[)])"))
# substring3 <- str_extract_all(input, regex("([m][u][l][(]\\d,\\d\\d\\d[)])"))
# 
# substring4 <- str_extract_all(input, regex("([m][u][l][(]\\d\\d,\\d[)])"))
# substring5 <- str_extract_all(input, regex("([m][u][l][(]\\d\\d,\\d\\d[)])"))
# substring6 <- str_extract_all(input, regex("([m][u][l][(]\\d\\d,\\d\\d\\d[)])"))
# 
# substring7 <- str_extract_all(input, regex("([m][u][l][(]\\d\\d\\d,\\d[)])"))
# substring8 <- str_extract_all(input, regex("([m][u][l][(]\\d\\d\\d,\\d\\d[)])"))
# 
# substring9 <- str_extract_all(input, regex("([m][u][l][(]\\d\\d\\d,\\d\\d\\d[)])"))
# 
# substring9 <- str_extract_all(input, regex("mul$"))
# 
# substring9[[1]] |> str_extract_all(regex('(\\d+)'))
# 












substring <- str_extract_all(input, regex("([m][u][l][(]\\d,\\d[)])"))
substring <- str_extract_all(input, regex("([m][u][l][(]\\d,\\d[)])"))
substring <- str_extract_all(input, regex("([m][u][l][(]\\d,\\d[)])"))
substring <- str_extract_all(input, regex("([m][u][l][(]\\d,\\d[)])"))
substring <- str_extract_all(input, regex("([m][u][l][(]\\d,\\d[)])"))

regexpr(pattern = "([m][u][l][(]\\d,\\d[)])",
        text = test,perl=TRUE)

# grep(pattern='([m][u][l][(]\\\d)',test)
# 
# grep(pattern='([m][u][l][(])',test)
regexpr(pattern = "([m][u][l][(]\\d,\\d[)])",text = input[1])
regexpr(pattern = "([m][u][l][(]\\d,\\d\\d[)])",text = input[1])
regexpr(pattern = "([m][u][l][(]\\d,\\d\\d\\d[)])",text = input[1])

grep(pattern = "([m][u][l][(]\\d\\d,\\d[)])",x = test)
grep(pattern = "([m][u][l][(]\\d\\d,\\d\\d[)])",x = test)
grep(pattern = "([m][u][l][(]\\d\\d,\\d\\d\\d[)])",x = test)

reg.finalizer(pattern = "([m][u][l][(]\\d\\d\\d,\\d[)])",x = test)
grep(pattern = "([m][u][l][(]\\d\\d\\d,\\d\\d[)])",x = test)
grep(pattern = "([m][u][l][(]\\d\\d\\d,\\d\\d\\d[)])",x = test)

