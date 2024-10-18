# Chpt 11, Loops. Calculate the expected pay-rate of the slot machine
# copied code to create the slot machine
get_symbols<- function(){
  wheel<- c("DD", '7', 'BBB',"BB", "B", "C", '0')
  sample(wheel, size =3, replace = TRUE,
         prob = c(0.03, 0.03,0.06, 0.1, 0.25, 0.01, 0.52))
}
symbols<- get_symbols()
score<- function(symbols){
  #identify case
  same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
  bars <- symbols %in% c("B", "BB", "BBB")
  
  #get prize
  if(same) { 
    payouts_same <- c("DD"= 100, '7'= 80, 'BBB' = 40,"BB" = 25, 
                      "B" = 10, "C" = 10, '0' = 0 )
    prize<- unname(payouts_same[symbols[1]])
  } else if (all(bars)) { 
    prize <- 5 
  } else {
    cherries <- sum(symbols == "C")
    prize<- c(0,2,5)[cherries+1]
  }
  #adjust for diamonds
  diamonds <- sum(symbols == "DD")
  prize*2 ^ diamonds
}
score(symbols)
#play slots
play<- function(){
  symbols <- get_symbols()
  print(symbols)
  score(symbols)
}
play()
--------------------------------------------------------------------------------
die <- c (1,2,3,4,5,6)
rolls <- expand.grid(die,die)
rolls
# n^r number of permutations, basic prob theory
 # calculate the value of each roll.
rolls$value <- rolls$Var1 + rolls$Var2
head (rolls, 3)
#calculate probability with the basic counting principal, p(a and b) = p(a)*p(b)
prob <- c("1" = 1/8, "2" = 1/8, "3" = 1/8, "4"=1/8, "5" = 1/8, "6"= 3/8)
prob
#subset by rolls$Var1 to see the vector of probs perfectly matched to the values of Var 1
prob[rolls$Var1]
rolls$prob1 <- prob[rolls$Var1]
head(rolls, 3)
# do the same for roll 2.
rolls$prob2 <- prob[rolls$Var2]
head(rolls, 3)
rolls$prob <- rolls$prob1*rolls$prob2
head(rolls, 3)
rolls$prob
# calculate the expected dice outcome now that we have both the value and the prob of the rolls
sum(rolls$value*rolls$prob)
#the expected value of rolling two loaded dice an infinite number of times is 8.25
#now do this for the slot machine
wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
slot_results <- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)
slot_results
pofslot<- c( "DD"= 0.03, "7"= 0.03,"BBB" = 0.06,"BB"= 0.1, "B"= 0.25, "C"= 0.01, "0"= 0.52)
slot_results$prob1 <- pofslot[slot_results$Var1]
slot_results$prob2 <- pofslot[slot_results$Var2]
slot_results$prob3 <- pofslot[slot_results$Var3]
head(slot_results)
slot_results$probability <- slot_results$prob1 * slot_results$prob2* slot_results$prob3
head(slot_results,25)
#check math
sum(slot_results$probability)
#create for loop to score the rolls. Use this to calculate the expected value E{x}= xp(x)
for (value in c("my", "first", "for", "loop")){
  print("one run")
}
#for (value in that){
#this
#}
#the that object is a set of objects. the code in between the braces  will run once for each member of that. 
#runs one print for each value in "my first for loop"
#can use any symbol you like.
#create an empty list to save output to.
chars <- vector(length= 4)
words <- c("my", "second", "for", "loop")
for (i in 1:4){
  chars[i] <- words[i]
}
chars
#create  a for loop for calculating the prize in each row of slot_results. 
slot_results$prize <- NA
head(slot_results, 3)
for (i in 1:nrow(slot_results)){
  symbols <- c(slot_results[i,1], slot_results[i,2], slot_results[i,3])
  slot_results$prize[i] <-score(symbols)
}
#expected payout from playing. sum of slot_results$prize * prob of winning that prize. 

sum(slot_results$prize * slot_results$probability)
slot_results[286,]
# try to handle write a version of score where DD is both wild and doubler.
#can't figure out on my own yet, learn from book code. 
symbols<- get_symbols()
score<- function(symbols){
  #identify case
  diamonds <- sum(symbols == "DD")
  cherries <- sum(symbols == "C")
  #sum number of diamonds in a row
  slots <- symbols[symbols != "DD"]
  same <- length(unique(slots)== 1)
  bars <- slots %in% c("B", "BB", "BBB")
  
  #get prize
  if(diamonds ==3){
    prize <- 100
  } else if(same) { 
    payouts_same <- c('7'= 80, 'BBB' = 40,"BB" = 25, 
                      "B" = 10, "C" = 10, '0' = 0 )
    prize<- unname(payouts_same[slots[1]])
  } else if (all(bars)) { 
    prize <- 5 
  } else if (cherries >0){
    prize<- c(0,2,5)[cherries + diamonds + 1]
  } else {
    prize <- 0 }
  

  prize*2 ^ diamonds
}
score(symbols)
# recalculate the expected values:
for (i in 1:nrow(slot_results)){
  symbols <- c(slot_results[i,1], slot_results[i,2], slot_results[i,3])
  slot_results$prize[i] <- score(symbols)
}
sum(slot_results$prize * slot_results$probability)
#wrong, try again. 