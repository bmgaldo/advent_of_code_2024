setwd("~/Documents/GitHub/advent_code_2024/day1")

library(readr)
input <- read_table("input.txt", col_names = FALSE)

input$X1_sorted <- sort(input$X1,decreasing=FALSE)
input$X2_sorted <- sort(input$X2,decreasing=FALSE)

# answer to puzzle one
abs(input$X1_sorted-input$X2_sorted) |> sum()

n_inputs <- length(input$X1)
sim_score = numeric(n_inputs)

i=1
x1 <- input$X1_sorted[i] 
sim_score[i] =  x1 * sum(abs(x1 - input$X2_sorted) == 0)

for(i in 2:n_inputs){ 
  x1 <- input$X1_sorted[i] 
  dx <-  abs(input$X1_sorted[i]-input$X1_sorted[i-1])
  if(dx == 0){
    sim_score[i] <- sim_score[i-1]
  } else {
    sim_score[i] <- x1 * sum(abs(x1 - input$X2_sorted) == 0)
  }
} 

# answer to puzzle two
sum(sim_score)