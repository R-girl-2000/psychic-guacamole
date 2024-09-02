# Chpt 11, Loops. Calculate the expected pay-rate of the slot machine
# copied code to create the slot machine
get_symbols<- function(){
  wheel<- c("DD", '7', 'BBB',"BB", "B", "c", '0')
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
                      "B" = 10, "c" = 10, '0' = 0 )
    prize<- unname(payouts_same[symbols[1]])
  } else if (all(bars)) { 
    prize <- 5 
  } else {
    cherries <- sum(symbols == "c")
    prize<- c(0,2,5)[cherries+1]
  }
  #adjust for diamonds
  diamonds <- sum(symbols == "DD")
  if (diamonds == 0) {diamonds <- diamonds +1}
  prize* diamonds^ 2
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
head(slot_results,25)\
#check math
sum(slot_results$probability)
