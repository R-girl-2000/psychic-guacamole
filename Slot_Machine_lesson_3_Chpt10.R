#Build slot machine
get_symbols<- function(){
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
-----------------------------------------------------------
  # Use the S3 system
Num <- 1000000
print(Num)
class(Num) <- c("POSIXct", "POSIXt")
print(Num)
-----------------------------------------------------------
#create an array to test attributes commands
deck <- array(c("ace","king","queen","jack"), dim = c(2,2))
deck
names(deck) <- c("ace", "king", "queen", "jack")
#values is not an attribute, will return an error. names and dim are attrs. 
values(deck) <- c(13:10)

#create a database to assign value. 
deck_df<- data.frame(face= c("ace","king","queen","jack"), 
          suit = c("hearts", "hearts", "hearts", "spades"),
                  value = c(13:10))
deck <- deck_df
attributes(deck)
#class is also an attribute
#list of attrs
class(deck)
names(deck)
dim(deck)
row.names(deck)
levels(deck)

#can change attributes by using these functions
row.names(deck) <- 100:103
row.names(deck)
levels(deck) <- c("level 1", "level 2", "level 3")
attributes(deck)
----------------------------------------------------------------
# Add attributes, any kind with the attr function
one_play <- play()
one_play
attributes(one_play)

attr(one_play, "symbols") <- c("B","0", "B")
attributes(one_play)
# can also use attr to look up the value of a given attribute. 
attr(one_play,"symbols")
attr(one_play, "score")
# What happened above? Score does not exist as an attribute yet, we have to assign it.
attr(one_play, "score")<- score(symbols)
attr(one_play, "score")
#also doesn't work, symbols has no attributes. 

#exercise 10.1, add an attribute to play. Assigns symbols as attribute to prize.
play<- function(){
  symbols <- get_symbols()
  prize <-  score(symbols)
  attr(prize, "symbols")<- symbols
  prize
}
play()
#copy to new object to test if it sticks
two_play <- play()
two_play
#can generate a prize and set attributes in one step with the structure function
play<- function(){
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols)
}
# always structure(R object, attribute = desired value)
three_play <- play()
three_play
# lookup and use the attribute
slot_display <- function(prize){
  symbols <- attr(prize, "symbols")
  symbols <- paste(symbols, collapse = " ")
  string <- paste(symbols, prize, sep = '\n$')
  cat(string)
}
slot_display(one_play)
}

#step by step,save attr of prize called symbols to an object called symbols
symbols <- attr(prize, "symbols")
symbols
#then use paste and arg collapse to collapse the three strings in symbols into one.
symbols <- paste(symbols, collapse = '')
symbols
# use paste and sep to combine the score and symbols and seperate with a new line.
string <- paste(symbols, prize, sep = "\n$")
string
# use cat() to split lines and remove quotation marks.
cat(string)

#use full function to clean up the output of play. 
slot_display(play())