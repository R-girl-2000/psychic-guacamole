#Build slot machine
get_symbols<-function(){
  wheel<- c("DD", '7', 'BBB',"BB", "B", "c", '0')
  sample(wheel, size =3, replace = TRUE,
         prob = c(0.03, 0.03,0.06, 0.1, 0.25, 0.01, 0.52))
}
-------------------------------------------------------------------
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
    prize* diamonds ^ 2
  }
  ----------------------------------------------------------
  #play slots
  play<- function(){
    symbols <- get_symbols()
    print(symbols)
    score(symbols)
  }
  play()
-----------------------------------------------------------
  # Use the S3 system
    